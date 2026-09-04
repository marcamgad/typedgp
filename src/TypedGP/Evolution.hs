{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | The top-level evolution loop, its per-generation statistics, and the
-- holdout discipline built around it.
--
-- Everything here is bookkeeping around 'nextGeneration': validate the
-- config once, stream generations, record statistics, decide when to stop.
-- There is deliberately no breeding logic in this module and no
-- hyperparameter literal — every number comes from the 'Config'.
--
-- The loop is expressed as a __lazy stream__ of generations that is then
-- cut to length, and consumed by a __strict fold__. Laziness means
-- generations past the cut are never computed; strictness means each
-- generation's population becomes garbage as soon as it has been
-- summarised. Both halves matter: without the first the early-stopping
-- budget would be wasted, and without the second a 60-generation run at a
-- population of 600 would hold every one of those populations alive at
-- once.
--
-- == Holdout
--
-- When 'cfgHoldout' is set, the data is split once, before the initial
-- population exists, and __selection only ever sees the training split__.
-- The holdout error is computed each generation for the current best
-- individual and reported alongside training error, never fed back. A
-- formula that fits training well and holdout badly is the single most
-- important thing this engine can tell a user, and it can only do so if
-- the two numbers are kept rigorously apart.
module TypedGP.Evolution
  ( -- * Statistics
    GenStats (..)
  , gsBestExpr
  , gsBestFitness
  , gsBestError
  , gsBestSize

    -- * Results
  , EvolutionResult (..)

    -- * Starting points
  , StartPoint (..)

    -- * Running
  , evolve
  , evolveIO
  , evolveOn
  , evolveIOOn

    -- * Diagnostics
  , runWarnings
  ) where

-- See the note in TypedGP.Fitness for why foldl' is conditional.
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl', intercalate, sortOn)
#else
import Data.List (intercalate, sortOn)
#endif

import TypedGP.Config (Config (..), SelectionStrategy (..), validateConfig)
import TypedGP.Data.Dataset (Dataset, datasetSize, splitDataset)
import TypedGP.Expr (Expr, countNodes)
import TypedGP.Fitness (Fitness (..), errorOf)
import TypedGP.Population
  ( Individual (..)
  , Population
  , bestIndividual
  , initPopulation
  , meanFitness
  , meanSize
  , nextGeneration
  , paretoFront
  )
import TypedGP.Random (Seed)
import TypedGP.Spectral (FrequencyTable, frequencyTable)
import TypedGP.Simplify (simplify)

-- | Where a run begins.
--
-- 'StartResumed' exists so that "TypedGP.Checkpoint" can continue an
-- interrupted run at the exact generation, population and PRNG state it
-- left off at, rather than restarting and pretending the earlier work
-- happened.
data StartPoint
  = StartFresh !Seed
  | StartResumed !Int !Population !Seed

-- | A snapshot of one generation.
data GenStats = GenStats
  { gsGeneration :: !Int
    -- ^ Generation number; @0@ is the randomly initialised population.
  , gsBest :: !Individual
    -- ^ Fittest individual in this generation, by training fitness.
  , gsBestSimplified :: !Expr
    -- ^ 'gsBest' after algebraic simplification. Identical value for every
    -- input, usually fewer nodes; this is what should be shown to a human.
  , gsMeanFitness :: !Double
    -- ^ Mean total fitness across the generation. Watch the gap between
    -- this and 'gsBestFitness': if it collapses, diversity is gone.
  , gsMeanSize :: !Double
    -- ^ Mean node count. Watch for bloat.
  , gsHoldoutError :: !(Maybe Double)
    -- ^ Error of the best individual on the holdout split, when one
    -- exists.
    --
    -- Purely observational. Nothing in the search reads this — the moment
    -- it influenced selection it would stop being a holdout.
  , gsSeed :: !Seed
    -- ^ PRNG state after producing this generation, so a run is resumable
    -- and any single generation can be reproduced in isolation.
  } deriving (Eq, Show)

gsBestExpr :: GenStats -> Expr
gsBestExpr = indExpr . gsBest

-- | Best total fitness (error plus size penalty) — the number selection
-- optimises and early stopping compares against.
gsBestFitness :: GenStats -> Double
gsBestFitness = fitTotal . indFitness . gsBest

-- | Best individual's raw training error, without the size penalty.
gsBestError :: GenStats -> Double
gsBestError = fitError . indFitness . gsBest

-- | Node count of the best individual after simplification.
gsBestSize :: GenStats -> Int
gsBestSize = countNodes . gsBestSimplified

-- | The outcome of a completed run.
data EvolutionResult = EvolutionResult
  { erBest :: !Individual
    -- ^ Best individual seen in /any/ generation, not merely the last.
  , erBestSimplified :: !Expr
    -- ^ 'erBest' algebraically simplified. The formula to report.
  , erHistory :: [GenStats]
    -- ^ Every generation, oldest first.
  , erGenerationsRun :: !Int
    -- ^ Number of the last generation reached.
  , erFinalSeed :: !Seed
  , erTargetReached :: !Bool
  , erTrainingSize :: !Int
  , erHoldoutSize :: !Int
    -- ^ Zero when no holdout was configured.
  , erHoldoutError :: !(Maybe Double)
    -- ^ Error of 'erBestSimplified' on the holdout split.
    --
    -- Compare against @fitError (indFitness erBest)@. A holdout error much
    -- larger than the training error means the formula described this
    -- sample rather than the process that generated it.
  , erWarnings :: ![String]
    -- ^ Non-fatal problems detected before the run. See 'runWarnings'.
  , erParetoFront :: ![Individual]
    -- ^ Non-dominated individuals of the final population, ascending by
    -- size. Populated in every mode, but only meaningful as a menu of
    -- alternatives under 'Pareto' selection.
  } deriving (Eq, Show)

-- | Non-fatal problems worth telling the user about before trusting a
-- result. The second argument is the total number of examples available,
-- counting both splits.
--
-- These are deliberately __not__ routed through 'validateConfig'. That
-- returns 'Left' and aborts, which is right for a configuration that
-- cannot produce a meaningful run at all; these conditions describe runs
-- that are legitimate but easy to over-trust. Aborting would be wrong — a
-- user may have good reason to fit six points — but so would staying
-- silent, so they travel with the result in 'erWarnings' and are printed
-- by the CLI and included in the JSON report.
runWarnings :: Config -> Int -> [String]
runWarnings cfg examples = concat
  [ warnIf (examples < 10 * inputs)
      ( "only " ++ show examples ++ " example(s) for " ++ show inputs
          ++ " input variable(s). Genetic programming searches an enormous "
          ++ "space of structures, so below roughly 10 examples per input "
          ++ "it can usually fit the sample exactly while learning nothing "
          ++ "that generalises. Treat any formula from this run as a "
          ++ "hypothesis, not a result."
      )
  , warnIf (examples < 2)
      "the dataset has fewer than 2 examples; no relationship can be estimated."
  , case cfgHoldout cfg of
      Nothing -> warnIf True
        ( "no holdout split configured (--holdout). The reported error is "
            ++ "training error only, which cannot distinguish a discovered "
            ++ "relationship from a memorised sample."
        )
      Just fraction -> warnIf (holdoutCount fraction < 5)
        ( "the holdout split holds fewer than 5 examples, so its error is "
            ++ "too noisy to be informative. Use more data or a larger "
            ++ "--holdout fraction."
        )
  -- Pareto mode ignores cfgParsimony by design, so a zero there is
  -- expected rather than an oversight.
  , warnIf (cfgParsimony cfg <= 0 && cfgSelection cfg /= Pareto)
      ( "parsimony pressure is disabled (--parsimony 0) and selection is "
          ++ "not in Pareto mode, so nothing penalises complexity. Expect "
          ++ "bloated formulas that fit noise."
      )
  ]
  where
    inputs :: Int
    inputs = length (cfgVariables cfg)

    holdoutCount :: Double -> Int
    holdoutCount fraction = round (fraction * fromIntegral examples)

    warnIf :: Bool -> String -> [String]
    warnIf True message = [message]
    warnIf False _      = []

-- | Run an evolution to completion, purely, splitting off a holdout set if
-- 'cfgHoldout' says so.
evolve :: Config -> Dataset -> Seed -> Either String EvolutionResult
evolve cfg ds s0 = case cfgHoldout cfg of
  Nothing -> evolveOn cfg ds Nothing (StartFresh s0)
  Just fraction ->
    let (training, holdout, s1) = splitDataset fraction s0 ds
    in evolveOn cfg training (Just holdout) (StartFresh s1)

-- | Run an evolution over an explicit training/holdout pair.
--
-- The entry point for cross-validation, which supplies its own folds and
-- must not have them re-split underneath it.
evolveOn
  :: Config
  -> Dataset
  -> Maybe Dataset
  -> StartPoint
  -> Either String EvolutionResult
evolveOn cfg training holdout start = do
  steps <- generationRun cfg training holdout start
  case steps of
    [] -> Left "evolution produced no generations"
    (first : rest) ->
      Right (finishRun cfg training holdout (foldl' stepRun (startRun first) rest))

-- | Like 'evolve', but invokes a callback as each generation completes.
--
-- The callback receives the population as well as the statistics, because
-- checkpointing needs the whole population while reporting needs only the
-- summary. Threading the population through 'GenStats' instead would mean
-- 'erHistory' retained every generation's population for the life of the
-- run, for the benefit of one consumer.
evolveIO
  :: Config
  -> Dataset
  -> Seed
  -> (GenStats -> Population -> IO ())
  -> IO (Either String EvolutionResult)
evolveIO cfg ds s0 report = case cfgHoldout cfg of
  Nothing -> evolveIOOn cfg ds Nothing (StartFresh s0) report
  Just fraction ->
    let (training, holdout, s1) = splitDataset fraction s0 ds
    in evolveIOOn cfg training (Just holdout) (StartFresh s1) report

-- | 'evolveOn' with a per-generation callback.
--
-- Reports and accumulates in a single pass, so that consumed generations
-- are collected as the run proceeds rather than being retained until the
-- end for a second traversal.
evolveIOOn
  :: Config
  -> Dataset
  -> Maybe Dataset
  -> StartPoint
  -> (GenStats -> Population -> IO ())
  -> IO (Either String EvolutionResult)
evolveIOOn cfg training holdout start report =
  case generationRun cfg training holdout start of
    Left problem -> return (Left problem)
    Right steps -> case steps of
      [] -> return (Left "evolution produced no generations")
      (first : rest) -> do
        reportStep first
        final <- loop (startRun first) rest
        return (Right (finishRun cfg training holdout final))
  where
    reportStep :: (GenStats, Population) -> IO ()
    reportStep (stats, pop) = report stats pop

    loop :: RunAccumulator -> [(GenStats, Population)] -> IO RunAccumulator
    loop !acc [] = return acc
    loop !acc (step : more) = do
      reportStep step
      loop (stepRun acc step) more

-- | What a completed run needs to remember, and nothing more.
--
-- Holding only the latest population — rather than the list of all of them
-- — is the whole reason this type exists.
data RunAccumulator = RunAccumulator
  { accHistoryReversed :: [GenStats]
  , accLatest :: !GenStats
  , accLatestPopulation :: !Population
  , accBest :: !GenStats
  }

startRun :: (GenStats, Population) -> RunAccumulator
startRun (stats, pop) = RunAccumulator
  { accHistoryReversed = [stats]
  , accLatest = stats
  , accLatestPopulation = pop
  , accBest = stats
  }

stepRun :: RunAccumulator -> (GenStats, Population) -> RunAccumulator
stepRun acc (stats, pop) = RunAccumulator
  { accHistoryReversed = stats : accHistoryReversed acc
  , accLatest = stats
  , accLatestPopulation = pop
  , accBest =
      if gsBestFitness stats < gsBestFitness (accBest acc)
        then stats
        else accBest acc
  }

finishRun :: Config -> Dataset -> Maybe Dataset -> RunAccumulator -> EvolutionResult
finishRun cfg training holdout acc = EvolutionResult
  { erBest = gsBest best
  , erBestSimplified = gsBestSimplified best
  , erHistory = reverse (accHistoryReversed acc)
  , erGenerationsRun = gsGeneration (accLatest acc)
  , erFinalSeed = gsSeed (accLatest acc)
  , erTargetReached = gsBestFitness best <= cfgTargetFitness cfg
  , erTrainingSize = trainingSize
  , erHoldoutSize = holdoutSize
  , erHoldoutError = fmap (holdoutErrorOf cfg (gsBestSimplified best)) holdout
  , erWarnings = runWarnings cfg (trainingSize + holdoutSize)
  , erParetoFront = distinctFront (paretoFront (accLatestPopulation acc))
  }
  where
    -- The raw front is riddled with exact ties: a population that has
    -- converged holds many copies of the same individual, and each copy is
    -- separately non-dominated. Listing them is noise — the front is only
    -- useful as a complexity ladder, one rung per (size, error) pair.
    --
    -- Deduplicating on that pair rather than on structure is the right
    -- key: within a front, two individuals of equal size cannot differ in
    -- error without one dominating the other, so equal (size, error) means
    -- they are genuinely interchangeable for the user's purposes even when
    -- the trees differ.
    distinctFront :: [Individual] -> [Individual]
    distinctFront = go [] . sortOn (countNodes . indExpr)
      where
        go :: [(Int, Double)] -> [Individual] -> [Individual]
        go _ [] = []
        go seen (ind : rest)
          | key `elem` seen = go seen rest
          | otherwise       = ind : go (key : seen) rest
          where
            key = (countNodes (indExpr ind), fitError (indFitness ind))

    best :: GenStats
    best = accBest acc

    trainingSize :: Int
    trainingSize = datasetSize training

    holdoutSize :: Int
    holdoutSize = maybe 0 datasetSize holdout

-- | Every generation the run should include, paired with its population.
--
-- Cutting happens in two stages, and the order matters: cap at
-- 'cfgGenerations' first, /then/ stop early on the target. Capping second
-- would let a run that never reaches its target produce one generation too
-- many.
generationRun
  :: Config
  -> Dataset
  -> Maybe Dataset
  -> StartPoint
  -> Either String [(GenStats, Population)]
generationRun rawCfg training holdout start = case validateConfig rawCfg of
  Left problems -> Left ("invalid configuration: " ++ intercalate "; " problems)
  Right cfg ->
    let startGeneration = case start of
          StartFresh _       -> 0
          StartResumed g _ _ -> g
        -- A resumed run continues towards the same absolute generation
        -- count, so its remaining budget is what is left of it. At least
        -- one generation always runs, so resuming past the end reports the
        -- restored population rather than failing.
        budget = max 1 (max 0 (cfgGenerations cfg) - startGeneration + 1)
        everyGeneration = map (statsOf cfg holdout) (generationStream cfg training start)
        -- A Nothing means an empty population, which a validated config
        -- makes impossible; stopping at it rather than skipping past it
        -- means a future bug surfaces as a clean error, not a hang.
        scored = takeWhileJust everyGeneration
        capped = take budget scored
        finished =
          takeThrough (\(st, _) -> gsBestFitness st <= cfgTargetFitness cfg) capped
    in if null finished
         then Left "population became empty before the first generation"
         else Right finished

-- | An unbounded stream of generations.
--
-- Unbounded on purpose: termination is a policy decision that belongs to
-- 'generationRun', not to the stepping logic.
generationStream :: Config -> Dataset -> StartPoint -> [(Int, Population, Seed)]
generationStream cfg training start = case start of
  StartFresh s0 ->
    let (initial, s1) = initPopulation cfg frequencies training s0
    in go 0 initial s1
  StartResumed generation pop s0 -> go generation pop s0
  where
    -- Computed once per run, here, and threaded into generation.
    --
    -- Deliberately not per generation. The cost would be tolerable, but
    -- the table is a property of the *dataset*, and recomputing it inside
    -- the loop would invite it to later depend on population state — which
    -- would couple initialisation to search and destroy the isolation that
    -- makes frequency seeding a risk-free change.
    frequencies :: FrequencyTable
    frequencies = frequencyTable cfg training

    go :: Int -> Population -> Seed -> [(Int, Population, Seed)]
    go !generation pop s =
      -- The generation number is threaded in so that periodic work inside
      -- the step -- currently constant refinement -- can be scheduled
      -- without the population having to track its own age.
      let (pop', s') = nextGeneration cfg frequencies training generation pop s
      in (generation, pop, s) : go (generation + 1) pop' s'

-- | Summarise one generation.
--
-- Simplification here is for reporting only — 'gsBest' keeps the
-- individual exactly as the population scored it. The population does its
-- own simplification of elites in 'nextGeneration', where a rescore
-- against the training data is available; duplicating that here would mean
-- either re-evaluating fitness or reporting a score that does not match
-- the tree it is attached to.
statsOf
  :: Config
  -> Maybe Dataset
  -> (Int, Population, Seed)
  -> Maybe (GenStats, Population)
statsOf cfg holdout (generation, pop, s) = fmap build (bestIndividual pop)
  where
    build :: Individual -> (GenStats, Population)
    build best =
      let simplified = simplify (indExpr best)
      in ( GenStats
             { gsGeneration = generation
             , gsBest = best
             , gsBestSimplified = simplified
             , gsMeanFitness = meanFitness pop
             , gsMeanSize = meanSize pop
             , gsHoldoutError = fmap (holdoutErrorOf cfg simplified) holdout
             , gsSeed = s
             }
         , pop
         )

holdoutErrorOf :: Config -> Expr -> Dataset -> Double
holdoutErrorOf cfg expr ds = errorOf (cfgErrorMetric cfg) ds expr

-- | Prefix of values before the first 'Nothing'.
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust (Just x : rest) = x : takeWhileJust rest
takeWhileJust _               = []

-- | Like 'takeWhile' on the negated predicate, but /inclusive/ of the
-- element that first satisfies it. Early stopping wants to keep the
-- generation that hit the target, not discard it.
takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough p (x : xs)
  | p x       = [x]
  | otherwise = x : takeThrough p xs
