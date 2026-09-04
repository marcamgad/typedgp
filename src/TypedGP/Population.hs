{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | The population type and one generational replacement step.
--
-- This is where fitness evaluation, selection and the genetic operators
-- meet. "TypedGP.Evolution" is then just a loop over 'nextGeneration' plus
-- bookkeeping — keeping the two apart means the breeding policy can change
-- without touching the loop, and the loop's termination logic can change
-- without touching breeding.
module TypedGP.Population
  ( -- * Types
    Individual (..)
  , Population
  , populationIndividuals
  , populationSize

    -- * Construction
  , initPopulation
  , populationFromExprs
  , populationFromAged
  , evaluateExprs
  , evaluateAged

    -- * Queries
  , scoredPopulation
  , bestIndividual
  , meanFitness
  , meanSize

    -- * Multi-objective views
  , objectivesOf
  , populationObjectives
  , rankingKeys
  , paretoFront

    -- * Generational step
  , nextGeneration
  , polishElite
  , simplifyIndividual
  , refineIndividual
  ) where

-- See the note in TypedGP.Fitness for why foldl' is conditional.
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl', sortOn)
#else
import Data.List (sortOn)
#endif
import GHC.Conc (par, pseq)

import TypedGP.Config (Config (..), SelectionStrategy (..))
import TypedGP.Data.Dataset (Dataset)
import TypedGP.Expr (Expr, countNodes)
import TypedGP.Fitness (Fitness (..), badFitness, caseErrors, evaluateFitness)
import TypedGP.Gen.Grow (Method (..), genExprWith, rampedHalfAndHalfWith)
import TypedGP.Spectral (FrequencyTable)
import TypedGP.Ops.Crossover (crossover)
import TypedGP.Ops.Mutation (mutate)
import TypedGP.Ops.Selection
  ( Objectives (..)
  , errorSizeObjectives
  , errorAgeObjectives
  , SelectionContext
  , Scored (..)
  , nsga2Keys
  , paretoFrontIndices
  , selectionContext
  , lexicaseElites
  , select
  )
import TypedGP.LocalSearch (optimiseConstants)
import TypedGP.Random (Seed, nextDouble)
import TypedGP.Simplify (simplify)

-- | A program together with its score.
--
-- Both fields are strict, and that is load-bearing for the parallel
-- fitness evaluation below: forcing an 'Individual' to weak head normal
-- form forces 'indFitness' to WHNF, which — because 'Fitness' also has
-- strict fields — forces all three of its 'Double's. So a spark that
-- evaluates the list spine to WHNF per element has genuinely done the
-- arithmetic, rather than sparking a thunk that gets handed back to the
-- main thread untouched.
data Individual = Individual
  { indExpr :: !Expr
  , indFitness :: !Fitness
  , indAge :: !Int
    -- ^ Generations since the __oldest__ genetic material in this
    -- individual entered the population. Read only by 'AgeFitness'
    -- selection; every other strategy ignores it, and it costs one Int
    -- per individual.
    --
    -- Not serialised by "TypedGP.Checkpoint", deliberately: a checkpoint
    -- stores no 'Config' and no strategy, so a resumed run cannot know it
    -- was an age-fitness run. Storing an age that a resume may silently
    -- ignore is worse than storing none, and resumed individuals get age
    -- 0, which is documented and testable.
  } deriving (Eq, Show)

-- | A scored generation. Never empty for a validated 'Config'.
newtype Population = Population
  { populationIndividuals :: [Individual]
  } deriving (Eq, Show)

populationSize :: Population -> Int
populationSize = length . populationIndividuals

-- | Build and score the generation-zero population using ramped
-- half-and-half.
initPopulation :: Config -> FrequencyTable -> Dataset -> Seed -> (Population, Seed)
initPopulation cfg table ds s0 =
  let (exprs, s1) = rampedHalfAndHalfWith cfg table (cfgPopulationSize cfg) s0
  in (Population (evaluateExprs cfg ds exprs), s1)

-- | Build a population from bare expressions, scoring them against a
-- dataset.
--
-- The route back in from "TypedGP.Checkpoint": a checkpoint stores
-- structures only, so resuming means rescoring. That is deliberate — it
-- keeps a restored population honest if the dataset or error metric
-- changed since the file was written.
populationFromExprs :: Config -> Dataset -> [Expr] -> Population
populationFromExprs cfg ds = Population . evaluateExprs cfg ds

-- | Build a population from expressions that already carry a lineage age.
--
-- The companion to 'populationFromExprs' for callers that have computed
-- ages from parentage. Exists so that 'Population' can stay an abstract
-- type: a test needing a specific age distribution would otherwise have to
-- reach for the constructor, and exporting that to make one test easier
-- would hand every caller the ability to build an unscored population.
populationFromAged :: Config -> Dataset -> [(Expr, Int)] -> Population
populationFromAged cfg ds = Population . evaluateAged cfg ds

-- | Score a batch of expressions, in parallel.
--
-- This is the single hottest path in the engine — every individual is
-- evaluated against every data point, every generation — and it is
-- embarrassingly parallel, since scores are independent.
--
-- To take this back to a sequential run (when bisecting a correctness
-- question, or profiling), replace 'parMapChunked' with @map@; nothing
-- else in the package depends on the parallelism.
evaluateExprs :: Config -> Dataset -> [Expr] -> [Individual]
evaluateExprs cfg ds exprs = evaluateAged cfg ds [(e, 0) | e <- exprs]

-- | Score expressions that already carry a lineage age.
--
-- Age is data the caller computed from parentage, not something this
-- function can derive, so it is threaded in rather than defaulted. Only
-- 'AgeFitness' reads it; every other strategy ignores the field.
evaluateAged :: Config -> Dataset -> [(Expr, Int)] -> [Individual]
evaluateAged cfg ds = parMapChunked (cfgParallelChunk cfg) score
  where
    score :: (Expr, Int) -> Individual
    score (expr, age) = Individual
      { indExpr = expr
      , indFitness = evaluateFitness cfg ds expr
      , indAge = age
      }

-- | The two objectives for one individual: raw error, and node count.
--
-- Note that this uses 'fitError', not 'fitTotal'. Feeding the
-- parsimony-penalised total into a multi-objective search would count size
-- twice — once inside the error objective and once as the size objective —
-- which is precisely the pre-committed trade-off Pareto mode exists to
-- avoid.
objectivesOf :: Individual -> Objectives
objectivesOf ind =
  errorSizeObjectives
    (fitError (indFitness ind))
    (fromIntegral (countNodes (indExpr ind)))

populationObjectives :: Population -> [Objectives]
populationObjectives = map objectivesOf . populationIndividuals

-- | (error, lineage age) for every individual, the 'AgeFitness' scheme.
--
-- Uses 'fitError' rather than 'fitTotal' for the same reason
-- 'objectivesOf' does: the parsimony penalty is a scalarised trade-off,
-- and feeding it into a multi-objective search mixes a pre-committed
-- exchange rate into an axis that exists to avoid one.
ageObjectivesOf :: Individual -> Objectives
ageObjectivesOf ind =
  errorAgeObjectives
    (fitError (indFitness ind))
    (fromIntegral (indAge ind))

populationAgeObjectives :: Population -> [Objectives]
populationAgeObjectives = map ageObjectivesOf . populationIndividuals

-- | One number per individual, lower being better, according to the
-- configured strategy.
--
-- Under 'Pareto' this is the NSGA-II crowded-comparison key; otherwise it
-- is scalarised total fitness. Everything downstream — selection, elitism,
-- survivor ranking — goes through this single function, so the two modes
-- cannot drift apart into using different orderings in different places.
rankingKeys :: Config -> Population -> [Double]
rankingKeys cfg pop = case cfgSelection cfg of
  Pareto     -> nsga2Keys (populationObjectives pop)
  AgeFitness -> nsga2Keys (populationAgeObjectives pop)
  -- Written out rather than left to a catch-all. A wildcard here is
  -- exactly the blind spot that lets a new strategy compile and silently
  -- select on scalar fitness — which for 'AgeFitness' would mean the
  -- mechanism does nothing at all while every test passes.
  Tournament          -> scalar
  FitnessProportional -> scalar
  Lexicase            -> scalar
  where
    scalar :: [Double]
    scalar = map (fitTotal . indFitness) (populationIndividuals pop)

-- | The non-dominated individuals: every formula not beaten on both
-- accuracy and size simultaneously.
--
-- Available regardless of the configured strategy, because the frontier is
-- an informative summary of any finished population, not only of one that
-- was selected for with it.
paretoFront :: Population -> [Individual]
paretoFront pop =
  [ ind
  | (i, ind) <- zip [0 :: Int ..] (populationIndividuals pop)
  , i `elem` frontIndices
  ]
  where
    frontIndices :: [Int]
    frontIndices = paretoFrontIndices (populationObjectives pop)

-- | Project a population into the generic form 'TypedGP.Ops.Selection'
-- consumes. Only the ranking key crosses the boundary — selection has no
-- business knowing about the error/penalty split or about Pareto ranks.
scoredPopulation :: Config -> Dataset -> Population -> [Scored Individual]
scoredPopulation cfg ds pop =
  zipWith toScored (populationIndividuals pop) (rankingKeys cfg pop)
  where
    toScored :: Individual -> Double -> Scored Individual
    toScored ind key = Scored
      { scoredValue = ind
      , scoredFitness = key
      -- Lazy field, built here once per generation and shared across every
      -- selection event in it. Strategies other than Lexicase never force
      -- it, so they pay nothing; see the note on 'Scored'.
      , scoredCaseErrors = caseErrors (cfgErrorMetric cfg) ds (indExpr ind)
      }

-- | The fittest individual, or 'Nothing' for an empty population.
bestIndividual :: Population -> Maybe Individual
bestIndividual pop = case populationIndividuals pop of
  []             -> Nothing
  (first : rest) -> Just (foldl' keepBetter first rest)
 where
  keepBetter :: Individual -> Individual -> Individual
  keepBetter champion challenger
    | fitTotal (indFitness challenger) < fitTotal (indFitness champion) = challenger
    | otherwise                                                        = champion

-- | Mean total fitness. An empty population scores 'badFitness', which
-- keeps the statistic finite and comparable instead of @NaN@.
meanFitness :: Population -> Double
meanFitness = meanOf badFitness (fitTotal . indFitness)

-- | Mean node count — the number to watch for bloat.
meanSize :: Population -> Double
meanSize = meanOf 0.0 (fromIntegral . countNodes . indExpr)

meanOf :: Double -> (Individual -> Double) -> Population -> Double
meanOf emptyValue f pop = case populationIndividuals pop of
  []   -> emptyValue
  inds -> foldl' (\ !acc ind -> acc + f ind) 0.0 inds / fromIntegral (length inds)

-- | Produce the next generation: elites carried over verbatim, the rest
-- bred from the current one.
--
-- Elites are copied /before/ breeding and are not re-evaluated, so their
-- scores are reused rather than recomputed. With @cfgElitism >= 1@ this
-- makes best-so-far fitness monotonically non-increasing, which is what
-- lets the convergence test assert improvement rather than merely hope
-- for it.
nextGeneration :: Config -> FrequencyTable -> Dataset -> Int -> Population -> Seed -> (Population, Seed)
nextGeneration cfg table ds generation pop s0 =
  let keys = rankingKeys cfg pop
      -- sortOn fst compares keys only, so no Ord instance is needed for
      -- Individual (and none should exist — there is no single natural
      -- ordering for one).
      ranked = map snd (sortOn fst (zip keys (populationIndividuals pop)))
      pool = scoredPopulation cfg ds pop
      -- Computed once per generation and shared by every selection event
      -- in it; see the note on SelectionContext.
      context = selectionContext pool
      (eliteBase, sAfterElites) =
        chooseElites cfg context pool ranked (max 0 (cfgElitism cfg)) s0
      elites = map (polishElite cfg ds generation) eliteBase
      wanted = max 0 (cfgPopulationSize cfg - length elites)
      (childExprs, s1) = breed cfg table context pool wanted sAfterElites
      children = evaluateAged cfg ds childExprs
      (injected, s2) = injectFresh cfg table ds s1
      -- Injection replaces the worst-ranked survivors rather than
      -- enlarging the population, so 'cfgPopulationSize' stays true and a
      -- run's cost profile does not change with the injection rate.
      survivors = take (max 0 (cfgPopulationSize cfg - length injected))
                       (elites ++ children)
  in (Population (map agedByOne survivors ++ injected), s2)

-- | One generation older.
--
-- Applied to survivors only. Injected individuals are created at age 0
-- /after/ this, so they enter the next generation genuinely new rather
-- than a generation old on arrival.
agedByOne :: Individual -> Individual
agedByOne ind = ind { indAge = indAge ind + 1 }

-- | The fresh random individuals 'AgeFitness' injects each generation.
--
-- Empty under every other strategy, and that matters for interpreting the
-- benchmark: injection without age protection is a /different/ mechanism —
-- it would simply replace the worst individuals with random ones — so
-- shipping both under one flag would make the result uninterpretable.
-- Whether injection alone explains any effect is a separate ablation, and
-- it is listed as an open question in the design note.
injectFresh :: Config -> FrequencyTable -> Dataset -> Seed -> ([Individual], Seed)
injectFresh cfg table ds s0
  | cfgSelection cfg /= AgeFitness = ([], s0)
  | count <= 0 = ([], s0)
  | otherwise =
      let (exprs, s1) = rampedHalfAndHalfWith cfg table count s0
      in (evaluateAged cfg ds [(e, 0) | e <- exprs], s1)
  where
    -- Never displace the entire population, however the knob is set.
    count :: Int
    count = min (cfgAgeInjection cfg) (max 0 (cfgPopulationSize cfg - 1))

-- | Pick the individuals carried over verbatim.
--
-- Ordinarily the top of the scalar ranking, which is deterministic and
-- consumes no randomness. Under 'Lexicase' with 'cfgLexicaseElites' set,
-- elites are instead drawn by the lexicase filter itself, so that elitism
-- and selection agree on what "best" means.
--
-- That distinction is not cosmetic. Under tournament, elitism preserves
-- the best by the same scalar the selector already orders by, so it
-- amplifies selection. Under lexicase the scalar is /not/ what selection
-- optimises, so scalar elitism pulls the population back towards the
-- generalists lexicase exists to stop privileging — measured on the
-- @nested@ benchmark as 30 points of recovery attributable to elitism
-- rather than to the selector.
chooseElites
  :: Config
  -> SelectionContext
  -> [Scored Individual]
  -> [Individual]
  -- ^ Individuals in scalar-ranked order, best first.
  -> Int
  -> Seed
  -> ([Individual], Seed)
chooseElites cfg context pool ranked wanted s0
  | wanted <= 0 = ([], s0)
  | not (useLexicaseElites) = (take wanted ranked, s0)
  | otherwise =
      -- lexicaseElites now yields whole individuals, so the carried-over
      -- fitness and age are the ones already computed rather than being
      -- recovered by matching on the expression.
      lexicaseElites cfg context pool wanted s0
  where
    useLexicaseElites :: Bool
    useLexicaseElites = cfgLexicaseElites cfg && cfgSelection cfg == Lexicase


-- | Everything applied to an elite before it is carried forward:
-- simplify, then optimise its constants.
--
-- Order matters. Simplification first means the optimiser sees the fewest,
-- most meaningful constants — refitting a coefficient inside a subtree
-- that is about to be deleted as @x - x@ is wasted work, and worse, it can
-- fit noise into a term that contributes nothing.
polishElite :: Config -> Dataset -> Int -> Individual -> Individual
polishElite cfg ds generation =
  refineIndividual cfg ds generation . simplifyIndividual cfg ds

-- | Hand an individual's constants to the simplex optimiser.
--
-- This is the bridge between the two halves of the problem: evolution
-- picks the structure, "TypedGP.LocalSearch" solves the continuous
-- parameters inside it. Without it, constants improve only by random
-- jitter in point mutation, which the benchmark suite shows failing on
-- exactly the problems whose difficulty is numerical rather than
-- structural.
--
-- Skipped entirely on generations that are not due, and skipped when the
-- optimiser changed nothing, so the common case costs one comparison
-- rather than a rescore.
refineIndividual :: Config -> Dataset -> Int -> Individual -> Individual
refineIndividual cfg ds generation ind
  | not (cfgRefineConstants cfg) = ind
  | generation `mod` max 1 (cfgRefineEvery cfg) /= 0 = ind
  | refined == indExpr ind = ind
  -- Age is carried over: refining constants edits an individual in
  -- place, it does not create a new lineage.
  | otherwise = ind
      { indExpr = refined
      , indFitness = evaluateFitness cfg ds refined
      }
  where
    refined :: Expr
    refined = optimiseConstants cfg ds (indExpr ind)

-- | Replace an individual with its algebraically simplified equivalent,
-- rescoring it.
--
-- Sound and free: "TypedGP.Simplify" is eval-preserving, so the error term
-- is unchanged by construction, and the simplified tree is never larger,
-- so the parsimony penalty can only fall. The individual therefore cannot
-- get worse — which is why this can be applied to elites without any risk
-- of undoing elitism's monotonicity guarantee.
--
-- Rescoring only happens when simplification actually changed something,
-- so the common no-op case costs one comparison rather than a full pass
-- over the dataset.
simplifyIndividual :: Config -> Dataset -> Individual -> Individual
simplifyIndividual cfg ds ind
  | not (cfgSimplifyBest cfg) = ind
  | simplified == indExpr ind = ind
  -- Likewise: simplifying is a rewrite of the same individual.
  | otherwise = ind
      { indExpr = simplified
      , indFitness = evaluateFitness cfg ds simplified
      }
  where
    simplified :: Expr
    simplified = simplify (indExpr ind)

-- | Breed exactly @wanted@ offspring expressions.
--
-- Crossover yields two children at a time, so the last batch may be
-- trimmed. Termination relies on 'makeOffspring' always returning at least
-- one expression.
breed :: Config -> FrequencyTable -> SelectionContext -> [Scored Individual] -> Int -> Seed -> ([(Expr, Int)], Seed)
breed cfg table context pool wanted s0 = go wanted s0 []
  where
    go :: Int -> Seed -> [(Expr, Int)] -> ([(Expr, Int)], Seed)
    go !remaining st acc
      | remaining <= 0 = (reverse acc, st)
      | otherwise =
          let (offspring, st') = makeOffspring cfg table context pool st
              taken = take remaining offspring
          in go (remaining - length taken) st' (reverse taken ++ acc)

-- | One breeding event: crossover, mutation, or plain reproduction.
--
-- A single uniform draw partitions @[0, 1)@ into the three cases, so the
-- rates cannot silently disagree with each other — whatever probability is
-- left after crossover and mutation is reproduction, by construction.
--
-- Always returns a non-empty list. When selection fails (only possible
-- with an empty pool) it falls back to a freshly generated individual,
-- which keeps 'breed' making progress instead of spinning.
makeOffspring :: Config -> FrequencyTable -> SelectionContext -> [Scored Individual] -> Seed -> ([(Expr, Int)], Seed)
makeOffspring cfg table context pool s0 =
  let (roll, s1) = nextDouble s0
      crossoverCut = cfgCrossoverRate cfg
      mutationCut = crossoverCut + cfgMutationRate cfg
  in if roll < crossoverCut
       then doCrossover s1
       else if roll < mutationCut
         then doMutation s1
         else doReproduction s1
  where
    -- A fresh individual starts a new lineage, so age 0.
    fresh :: Seed -> ([(Expr, Int)], Seed)
    fresh st =
      let (expr, st') = genExprWith cfg table Grow (cfgMaxInitialDepth cfg) st
      in ([(expr, 0)], st')

    doCrossover :: Seed -> ([(Expr, Int)], Seed)
    doCrossover st = case select cfg context pool st of
      Nothing -> fresh st
      Just (parentA, st1) -> case select cfg context pool st1 of
        Nothing -> ([(indExpr parentA, indAge parentA)], st1)
        Just (parentB, st2) ->
          let ((childA, childB), st3) =
                crossover cfg (indExpr parentA) (indExpr parentB) st2
              -- max, not min and not the mean. Age tracks the OLDEST
              -- genetic material present, so a child of an old lineage and
              -- a new one is only as protected as its oldest part.
              -- Taking the minimum would let crossing with a fresh
              -- individual launder an old lineage's age away, which
              -- defeats the entire mechanism. This is the easiest rule
              -- here to get wrong, because "how new is this" suggests min;
              -- age is really "how long has this been failing to be
              -- replaced".
              childAge = max (indAge parentA) (indAge parentB)
          in ([(childA, childAge), (childB, childAge)], st3)

    doMutation :: Seed -> ([(Expr, Int)], Seed)
    doMutation st = case select cfg context pool st of
      Nothing -> fresh st
      Just (parent, st1) ->
        let (child, st2) = mutate cfg (indExpr parent) st1
        -- Mutation edits a lineage rather than starting one.
        in ([(child, indAge parent)], st2)

    doReproduction :: Seed -> ([(Expr, Int)], Seed)
    doReproduction st = case select cfg context pool st of
      Nothing            -> fresh st
      Just (parent, st1) -> ([(indExpr parent, indAge parent)], st1)

-- | Parallel map over chunks, using only @par@\/@pseq@ from "GHC.Conc"
-- (which ships with @base@, so this costs no dependency).
--
-- Chunking matters: sparking one item per individual would create
-- thousands of sparks whose individual work is a few microseconds, and
-- spark bookkeeping would dominate. 'cfgParallelChunk' is the knob.
--
-- The idiom is @spark the chunk, then demand the rest before consing@:
-- @par@ registers the chunk as available for another capability, and the
-- @pseq@ stops GHC from reordering the demand so that the main thread
-- races ahead into work it just sparked.
--
-- Local bindings here are left unannotated on purpose: their types mention
-- the enclosing @a@ and @b@, which a local signature would shadow with
-- fresh variables.
parMapChunked :: Int -> (a -> b) -> [a] -> [b]
parMapChunked chunkSize f xs = concat (go (chunksOf (max 1 chunkSize) xs))
  where
    go [] = []
    go (chunk : rest) =
      let mapped = map f chunk
          mappedRest = go rest
      in forceSpine mapped `par` (mappedRest `pseq` (mapped : mappedRest))

-- | Force a list's spine and every element to weak head normal form.
--
-- WHNF per element is enough here because 'Individual' has strict fields;
-- see the note on that type. If a future element type is lazy in the work
-- you want done in parallel, this is the function that has to get deeper.
forceSpine :: [a] -> ()
forceSpine []       = ()
forceSpine (x : xs) = x `pseq` forceSpine xs

-- | Split into chunks of at most @n@. A non-positive @n@ yields a single
-- chunk rather than looping forever.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = [xs]
  | otherwise = case splitAt n xs of
      ([], _)       -> []
      (chunk, rest) -> chunk : chunksOf n rest
