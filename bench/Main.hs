-- | Benchmark runner.
--
-- Runs the family-spanning suite in "TypedGP.Benchmark" and reports the
-- metrics that distinguish a better search from a luckier one. As with the
-- main CLI, no search logic lives here: this drives 'evolve' and formats
-- what comes back.
--
-- Typical use is a before/after comparison across a change to the engine:
--
-- > typedgp-bench --seeds 5 --format json > before.json
-- > ...make a change...
-- > typedgp-bench --seeds 5 --format json > after.json
--
-- Everything is seeded explicitly, so two runs of the same command produce
-- byte-identical output and a diff means a real change.
module Main (main) where

import Control.Monad (forM, when)
import Data.List (intercalate, sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
  ( BufferMode (LineBuffering)
  , hSetBuffering
  , hSetEncoding
  , stdout
  , utf8
  )

import TypedGP.Benchmark
  ( Problem (..)
  , ProblemResult (..)
  , ProblemSummary (..)
  , Variant (..)
  , applyVariant
  , generateClean
  , generateNoisy
  , normalisedRmse
  , problemsNamed
  , exactThreshold
  , recoveryThreshold
  , summarise
  , variantName
  , variants
  )
import TypedGP.Config
  ( Config (..)
  , defaultConfig
  , parseErrorMetric
  , parseSelectionStrategy
  )
import TypedGP.Data.Dataset (Dataset, parseDouble)
import TypedGP.DomainStats
  ( DomainBreakdown
  , breakdownCount
  , breakdownInvalid
  , breakdownOver
  , breakdownTotal
  , legacyPowExposure
  )
import TypedGP.Eval (DomainError (..), eval)
import TypedGP.Evolution (EvolutionResult (..), evolve)
import TypedGP.Gen.Grow (rampedHalfAndHalfWith)
import TypedGP.Spectral (frequencyTable)
import TypedGP.Expr (Expr, countNodes, flatten, freeVariables, opName)
import TypedGP.Fitness (Fitness (..))
import TypedGP.Population (Individual (..))
import TypedGP.Pretty (pretty)
import TypedGP.Random (mkSeed)
import TypedGP.Report (JsonValue (..), encodeJson)

data Options = Options
  { optSeeds :: !Int
  , optProblems :: ![String]
  , optVariants :: ![Variant]
  , optTrainingSize :: !Int
  , optTestSize :: !Int
  , optJson :: !Bool
  , optVerbose :: !Bool
  , optConfig :: !Config
  , optShowHelp :: !Bool
  , optBreakdown :: !Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optSeeds = 3
  , optProblems = []
  , optVariants = [Clean]
  , optTrainingSize = 100
  , optTestSize = 400
  , optJson = False
  , optVerbose = False
  , optConfig = benchConfig
  , optShowHelp = False
  , optBreakdown = False
  }

-- | Search settings for benchmarking.
--
-- Deliberately more generous than 'defaultConfig': the point is to measure
-- what the engine can find, not how it behaves under a tight budget. A
-- holdout is always on, so every run reports generalisation alongside fit.
benchConfig :: Config
benchConfig = defaultConfig
  { cfgPopulationSize = 500
  , cfgGenerations = 80
  , cfgHoldout = Just 0.25
  , cfgTargetFitness = 0.0
    -- Never stop early: every run gets the same budget, otherwise the
    -- timing column would measure how quickly a problem got lucky rather
    -- than how expensive it is.
  }

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Left problem -> do
      putStrLn ("error: " ++ problem)
      putStrLn ""
      mapM_ putStrLn usage
      exitFailure
    Right opts
      | optShowHelp opts -> mapM_ putStrLn usage >> exitSuccess
      | optBreakdown opts -> runBreakdown opts
      | otherwise -> run opts

run :: Options -> IO ()
run opts = do
  let problems = problemsNamed (optProblems opts)
      combinations = [(p, v) | p <- problems, v <- optVariants opts]
  when (null combinations) $ do
    putStrLn "error: no problems matched"
    exitFailure
  when (not (optJson opts)) $ do
    putStrLn "TypedGP benchmark suite"
    putStrLn ""
    putStrLn ("problems : " ++ show (length problems))
    putStrLn ("variants : " ++ intercalate ", " (map variantName (optVariants opts)))
    putStrLn ("seeds    : " ++ show (optSeeds opts))
    putStrLn ("training : " ++ show (optTrainingSize opts) ++ " examples")
    putStrLn ""
  -- The header goes out before any work starts, so rows stream into a
  -- formed table as each combination finishes. A long run is then readable
  -- while it is still going, without printing every row twice.
  when (not (optJson opts)) (mapM_ putStrLn tableHeader)
  summaries <- forM combinations (uncurry (runCombination opts))
  if optJson opts
    then putStrLn (encodeJson (JArr (map summaryJson summaries)))
    else mapM_ putStrLn tableFooter
  -- Exit status reflects whether anything was recovered at all, so a
  -- totally broken engine fails a CI check rather than reporting a tidy
  -- table of zeros.
  if any ((> 0) . psRecoveryRate) summaries then exitSuccess else exitFailure

runCombination :: Options -> Problem -> Variant -> IO ProblemSummary
runCombination opts baseProblem variant = do
  let problem = applyVariant variant baseProblem
      seeds = [1 .. fromIntegral (max 1 (optSeeds opts))] :: [Word64]
  results <- forM seeds (runOne opts problem variant)
  let summary = summarise (probName problem) (variantName variant) (concat results)
  when (not (optJson opts)) $ putStrLn (renderRow summary)
  return summary

-- | Tabulate which domain errors actually occur, per problem.
--
-- Answers the Phase 3 open question: is 'Saturated' swamping the five
-- genuine domain errors? Two samples are reported per problem, because
-- they can differ and the difference is the informative part:
--
--   [@gen0@] The initial random population — exactly what the search
--     evaluates before selection has done anything.
--   [@front@] The final Pareto front of a real run — individuals that
--     survived selection.
--
-- If saturation dominates @gen0@ but not @front@, selection is already
-- removing saturating individuals and a penalty has little left to do.
-- If it dominates both, a single penalty weight is measuring overflow
-- rather than meaninglessness.
runBreakdown :: Options -> IO ()
runBreakdown opts = do
  putStrLn "Domain error breakdown"
  putStrLn ""
  putStrLn ("population : " ++ show (cfgPopulationSize (optConfig opts)))
  putStrLn ("training   : " ++ show (optTrainingSize opts) ++ " examples")
  putStrLn ""
  putStrLn (replicate 96 '-')
  putStrLn (concat
    [ padRight 16 "problem", padRight 7 "sample", padRight 10 "invalid%"
    , padRight 10 "div0", padRight 10 "log<=0", padRight 10 "pow<0"
    , padRight 10 "gamma", padRight 9 "zeta", padRight 10 "saturated"
    ])
  putStrLn (replicate 96 '-')
  mapM_ (breakdownFor opts) (problemsNamed (optProblems opts))
  putStrLn (replicate 96 '-')
  putStrLn ""
  putStrLn "Percentages are shares of INVALID evaluations, not of all evaluations;"
  putStrLn "invalid% is the share of all evaluations that were invalid at all."
  putStrLn "One evaluation = one expression against one training point. Only the"
  putStrLn "first error per evaluation is counted, so these are shares of 'what was"
  putStrLn "it caught doing first', not of every way each expression was wrong."

breakdownFor :: Options -> Problem -> IO ()
breakdownFor opts problem = do
  let dataSeed = mkSeed 4242
      (trainingResult, _) = generateNoisy problem (optTrainingSize opts) dataSeed
  case trainingResult of
    Left _ -> putStrLn ("  " ++ probName problem ++ ": could not generate data")
    Right training -> do
      let cfg = (optConfig opts) { cfgVariables = probVariables problem }
          frequencies = frequencyTable cfg training
          (initial, _) =
            rampedHalfAndHalfWith cfg frequencies (cfgPopulationSize cfg) (mkSeed 1)
          front = case evolve cfg training (mkSeed 1) of
            Right result -> map indExpr (erParetoFront result)
            Left _       -> []
      emit "gen0" (breakdownOver training initial)
      emit "front" (breakdownOver training front)
      -- How exposed a pre-/post-semantics benchmark comparison is on this
      -- problem. See TypedGP.DomainStats.divergesFromLegacyPow.
      putStrLn (concat
        [ padRight 16 "", padRight 7 "pow+-"
        , padRight 10 (percent (legacyPowExposure training initial))
        , "of gen0 evaluations differ under the old Pow semantics"
        ])
  where
    emit :: String -> DomainBreakdown -> IO ()
    emit label b =
      let invalid = breakdownInvalid b
          total = breakdownTotal b
          -- Share of invalid evaluations, which is the question. A share
          -- of all evaluations would be dominated by the valid majority
          -- and would say nothing about the mix.
          share :: DomainError -> String
          share e
            | invalid == 0 = "-"
            | otherwise = percent (fromIntegral (breakdownCount b e)
                                     / fromIntegral invalid)
      in putStrLn (concat
           [ padRight 16 (probName problem)
           , padRight 7 label
           , padRight 10 (if total == 0 then "-"
                       else percent (fromIntegral invalid / fromIntegral total))
           , padRight 10 (share DividedByZero)
           , padRight 10 (share LogOfNonPositive)
           , padRight 10 (share PowOfNegativeBase)
           , padRight 10 (share GammaAtPole)
           , padRight 9 (share ZetaAtPole)
           , padRight 10 (share Saturated)
           ])

-- | One problem, one variant, one seed. Returns a singleton list, or an
-- empty one if the data could not be generated.
runOne :: Options -> Problem -> Variant -> Word64 -> IO [ProblemResult]
runOne opts problem variant seed = do
  let dataSeed = mkSeed (seed * 7919 + 13)
      (trainingResult, dataSeed1) =
        generateNoisy problem (optTrainingSize opts) dataSeed
      (testResult, dataSeed2) =
        generateClean problem (probDomain problem) (optTestSize opts) dataSeed1
      (extrapResult, _) =
        generateClean problem (probExtrapolation problem) (optTestSize opts) dataSeed2
  case (trainingResult, testResult, extrapResult) of
    (Right training, Right test, Right extrap) -> do
      startedAt <- getMonotonicTime
      let cfg = (optConfig opts) { cfgVariables = probVariables problem }
          outcome = evolve cfg training (mkSeed seed)
      case outcome of
        Left problemText -> do
          putStrLn ("  " ++ probName problem ++ ": " ++ problemText)
          return []
        Right result -> do
          let discovered = erBestSimplified result
          -- Forced before the clock is read: `evolve` is lazy, so without
          -- this the timing would measure list construction rather than
          -- the search.
          finishedAt <- discovered `seq` getMonotonicTime
          let scored = scoreRun problem variant seed result discovered
                         test extrap (finishedAt - startedAt)
          -- The per-seed error is printed alongside the formula because
          -- recovery rate is a threshold statistic: it says whether the
          -- error crossed a fixed bar, never how close it came. Two arms
          -- with identical recovery can have completely different error
          -- distributions, and without this the table cannot show that.
          when (optVerbose opts && not (optJson opts)) $
            putStrLn ("    seed " ++ show seed
                        ++ ": nrmse=" ++ fixed 4 (prTestNRmse scored)
                        ++ (if prRecovered scored then " [recovered] " else " ")
                        ++ prFormula scored)
          return [scored]
    _ -> do
      putStrLn ("  " ++ probName problem ++ ": could not generate data")
      return []

scoreRun
  :: Problem
  -> Variant
  -> Word64
  -> EvolutionResult
  -> Expr
  -> Dataset
  -> Dataset
  -> Double
  -> ProblemResult
scoreRun problem variant seed result discovered test extrap elapsed = ProblemResult
  { prProblem = probName problem
  , prVariant = variantName variant
  , prSeed = seed
  , prRecovered = testScore < recoveryThreshold
  , prExact = testScore < exactThreshold
  , prTestNRmse = testScore
  , prTrainError = fitError (indFitness (erBest result))
  , prHoldoutError = erHoldoutError result
  , prExtrapNRmse = normalisedRmse predict extrap
  , prComplexity = countNodes discovered
  , prGenerations = erGenerationsRun result
  , prSeconds = elapsed
  , prOperators = sort (nubStrings (map opName (flatten discovered)))
  , prFalseDiscovery = not (null referencedDistractors)
  , prFormula = pretty discovered
  , prExpr = discovered
  }
  where
    predict :: [(String, Double)] -> Double
    predict env = eval env discovered

    testScore :: Double
    testScore = normalisedRmse predict test

    -- 'freeVariables', not 'variablesOf'. A false discovery means the
    -- formula depends on a dataset column it should not; a binder's own
    -- index is neither a dataset column nor a dependency, and counting it
    -- scored every binder-containing formula as a false discovery on
    -- problems that have no distractors at all — observed as 25% FDR on
    -- `polynomial`, whose only variable is relevant.
    --
    -- The bug predates binders in the sense that the two functions were
    -- interchangeable before one of them could bind anything. It is the
    -- same conflation the warning on 'TypedGP.Simplify.isFoldable' guards
    -- against, surfacing in a second place.
    referencedDistractors :: [String]
    referencedDistractors =
      [v | v <- freeVariables discovered, v `notElem` probRelevant problem]

nubStrings :: [String] -> [String]
nubStrings = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- Reporting -------------------------------------------------------------------

tableHeader :: [String]
tableHeader =
  [ replicate 88 '-'
  , header
  , replicate 88 '-'
  ]

tableFooter :: [String]
tableFooter = [replicate 88 '-', "", legend]

header :: String
header =
  padRight 16 "problem"
    ++ padRight 11 "variant"
    ++ padLeft 7 "runs"
    ++ padLeft 9 "recov"
    ++ padLeft 8 "exact"
    ++ padLeft 11 "test"
    ++ padLeft 11 "extrap"
    ++ padLeft 8 "size"
    ++ padLeft 9 "sec"
    ++ padLeft 8 "FDR"

renderRow :: ProblemSummary -> String
renderRow s =
  padRight 16 (psProblem s)
    ++ padRight 11 (psVariant s)
    ++ padLeft 7 (show (psRuns s))
    ++ padLeft 9 (annotateRecovery s)
    ++ padLeft 8 (percent (psExactRate s))
    ++ padLeft 11 (fixed 4 (psMedianTestNRmse s))
    ++ padLeft 11 (fixed 4 (psMedianExtrapNRmse s))
    ++ padLeft 8 (fixed 0 (psMedianComplexity s))
    ++ padLeft 9 (fixed 2 (psMedianSeconds s))
    ++ padLeft 8 (percent (psFalseDiscoveryRate s))

-- | Recovery rate, marked when it is too sparse to compare statistically.
--
-- A standing rule rather than a judgement made afresh each time a
-- low-base-rate family shows up in a table: the normal approximation behind
-- a two-proportion z-test needs roughly five successes /and/ five failures
-- per arm, and below that a z-score computed from these cells is not
-- meaningful however confident it looks.
--
-- The engine cannot know what a row will be compared against, so this marks
-- the cell rather than refusing anything. A marked cell is still a fine
-- description of what happened; it is only unfit to be one arm of a
-- significance test.
annotateRecovery :: ProblemSummary -> String
annotateRecovery s
  | tooSparse = percent (psRecoveryRate s) ++ "*"
  | otherwise = percent (psRecoveryRate s) ++ " "
  where
    successes, failures :: Double
    successes = psRecoveryRate s * fromIntegral (psRuns s)
    failures = fromIntegral (psRuns s) - successes

    tooSparse :: Bool
    tooSparse = psRuns s > 0 && (successes < 5.0 || failures < 5.0)

legend :: String
legend =
  "exact = share of seeds finding the target itself rather than an "
    ++ "approximation of it (normalised RMSE < 1e-6). recov counts anything "
    ++ "under 0.05, which on `nested` admitted four sprawling curve fits as "
    ++ "recoveries; the recov-minus-exact gap is how much of a row is "
    ++ "fitting rather than discovery.  "
    ++ "* = fewer than 5 successes or 5 failures; describes this run fine, but is "
    ++ "below where a two-proportion z-test against it means anything.  "
    ++ "recov = share of seeds reaching normalised RMSE < "
    ++ fixed 2 recoveryThreshold
    ++ " against the noiseless truth.  test/extrap = median normalised RMSE "
    ++ "in and out of the training range (1.0 = no better than predicting the "
    ++ "mean).  FDR = share of seeds whose formula referenced an irrelevant "
    ++ "variable."

summaryJson :: ProblemSummary -> JsonValue
summaryJson s = JObj
  [ ("problem", JStr (psProblem s))
  , ("variant", JStr (psVariant s))
  , ("runs", JInt (psRuns s))
  , ("recovery_rate", JNum (psRecoveryRate s))
  , ("exact_rate", JNum (psExactRate s))
  , ("median_test_nrmse", JNum (psMedianTestNRmse s))
  , ("median_extrapolation_nrmse", JNum (psMedianExtrapNRmse s))
  , ("median_complexity", JNum (psMedianComplexity s))
  , ("median_seconds", JNum (psMedianSeconds s))
  , ("false_discovery_rate", JNum (psFalseDiscoveryRate s))
  , ("operators_used", JArr (map JStr (psOperatorsUsed s)))
  ]

-- Argument parsing --------------------------------------------------------------

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go :: Options -> [String] -> Either String Options
    go opts [] = Right opts
    go opts (arg : rest) = case arg of
      "--help"    -> go opts { optShowHelp = True } rest
      "-h"        -> go opts { optShowHelp = True } rest
      "--json"    -> go opts { optJson = True } rest
      "--verbose" -> go opts { optVerbose = True } rest
      "--all-variants" -> go opts { optVariants = variants } rest
      "--domain-breakdown" -> go opts { optBreakdown = True } rest
      "--no-refine" -> go opts
        { optConfig = (optConfig opts) { cfgRefineConstants = False } } rest
      "--quick" -> go opts
        { optSeeds = 1
        , optTrainingSize = 60
        , optTestSize = 200
        , optConfig = (optConfig opts)
            { cfgPopulationSize = 200, cfgGenerations = 25 }
        } rest
      _ -> case rest of
        [] -> Left (arg ++ ": unknown option, or its value is missing")
        (value : rest') -> do
          opts' <- applyFlag arg value opts
          go opts' rest'

applyFlag :: String -> String -> Options -> Either String Options
applyFlag flag value opts = case flag of
  "--seeds" -> withInt (\n o -> o { optSeeds = n })
  "--train" -> withInt (\n o -> o { optTrainingSize = n })
  "--test" -> withInt (\n o -> o { optTestSize = n })
  "--pop" -> withInt (\n o -> o { optConfig = (optConfig o) { cfgPopulationSize = n } })
  "--gens" -> withInt (\n o -> o { optConfig = (optConfig o) { cfgGenerations = n } })
  -- The knobs most likely to be under evaluation. Exposed here so an
  -- A/B comparison is a change of flag rather than a rebuild.
  "--hoist-share" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgHoistMutationShare = p } })
  "--point-share" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgPointMutationShare = p } })
  "--parsimony" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgParsimony = p } })
  "--domain-penalty" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgDomainPenalty = p } })
  "--age-injection" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgAgeInjection = n } })
  -- Binder generation. On/off rather than valueless so a comparison loop
  -- can vary it by substituting one word.
  "--binders" ->
    Right opts { optConfig = (optConfig opts) { cfgEnableBinders = value == "on" } }
  "--binder-prob" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgBinderProb = p } })
  "--binder-terms" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgBinderMaxTerms = n } })
  "--binder-depth" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgBinderDepthLimit = n } })
  "--tournament" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgTournamentSize = n } })
  -- Exposed for the elitism ablation: elites are ranked by scalar fitness
  -- regardless of selection strategy, so setting this to 0 is how you ask
  -- whether elitism is confounding a strategy comparison.
  "--lexicase-elites" -> Right opts { optConfig = (optConfig opts) { cfgLexicaseElites = value == "on" } }
  -- The Phase 2 A/B. Taken as on/off rather than as a valueless flag so a
  -- benchmark loop can vary it by substituting one word, which is what the
  -- validation harness does.
  "--frequency-seeding" ->
    Right opts { optConfig = (optConfig opts) { cfgFrequencySeedingEnabled = value == "on" } }
  "--freq-top-k" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgFrequencySeedTopK = n } })
  "--freq-resolution" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgFrequencyResolution = n } })
  "--freq-weight" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgFrequencySeedWeight = p } })
  "--freq-threshold" ->
    withDouble (\p o -> o { optConfig = (optConfig o) { cfgFrequencySignalThreshold = p } })
  "--elitism" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgElitism = n } })
  "--refine-every" ->
    withInt (\n o -> o { optConfig = (optConfig o) { cfgRefineEvery = n } })
  -- The suite's recovery metric is normalised RMSE against the noiseless
  -- truth regardless of what drove the search, so swapping the fitness
  -- metric here produces a genuine like-for-like comparison.
  "--metric" -> case parseErrorMetric value of
    Just m  -> Right opts { optConfig = (optConfig opts) { cfgErrorMetric = m } }
    Nothing -> Left (flag ++ ": expected rmse, mse, mae or huber")
  "--selection" -> case parseSelectionStrategy value of
    Just s  -> Right opts { optConfig = (optConfig opts) { cfgSelection = s } }
    Nothing -> Left (flag ++ ": expected tournament, roulette, pareto or lexicase")
  "--problems" -> Right opts { optProblems = splitCommas value }
  "--variants" -> case mapM parseVariant (splitCommas value) of
    Just vs -> Right opts { optVariants = vs }
    Nothing -> Left (flag ++ ": unknown variant in " ++ show value)
  "--format" -> case value of
    "json" -> Right opts { optJson = True }
    "text" -> Right opts { optJson = False }
    _      -> Left (flag ++ ": expected text or json")
  _ -> Left ("unknown option: " ++ flag)
  where
    withInt :: (Int -> Options -> Options) -> Either String Options
    withInt f = case reads value :: [(Int, String)] of
      [(n, "")] -> Right (f n opts)
      _         -> Left (flag ++ ": expected an integer, got " ++ show value)

    withDouble :: (Double -> Options -> Options) -> Either String Options
    withDouble f = case parseDouble value of
      Just d  -> Right (f d opts)
      Nothing -> Left (flag ++ ": expected a number, got " ++ show value)

parseVariant :: String -> Maybe Variant
parseVariant name = case [v | v <- variants, variantName v == name] of
  (v : _) -> Just v
  []      -> Nothing

splitCommas :: String -> [String]
splitCommas text = case break (== ',') text of
  (field, [])           -> [field | not (null field)]
  (field, _ : leftover) -> [field | not (null field)] ++ splitCommas leftover

-- Formatting ------------------------------------------------------------------

fixed :: Int -> Double -> String
fixed places value
  | isNaN value = "NaN"
  | isInfinite value = "inf"
  -- Anything this large is a failed run; printing 14 digits of it just
  -- breaks the column alignment.
  | abs value >= 1.0e5 = "1e5+"
  | otherwise = showFFloat (Just places) value ""

percent :: Double -> String
percent v = fixed 0 (100.0 * v) ++ "%"

padLeft :: Int -> String -> String
padLeft width text = replicate (max 0 (width - length text)) ' ' ++ text

padRight :: Int -> String -> String
padRight width text = text ++ replicate (max 0 (width - length text)) ' '

usage :: [String]
usage =
  [ "typedgp-bench - family-spanning benchmark suite"
  , ""
  , "USAGE"
  , "  typedgp-bench [OPTIONS]"
  , ""
  , "  Runs each problem across several seeds and reports recovery rate,"
  , "  in-domain and extrapolation error, complexity, time, and false"
  , "  discovery rate. Fully deterministic: the same command gives the same"
  , "  numbers, so two runs can be diffed to evaluate an engine change."
  , ""
  , "OPTIONS"
  , "  --seeds N          Seeds per problem (default: 3)."
  , "  --problems a,b     Restrict to named problems or families."
  , "  --variants a,b     clean | noisy | irrelevant | hetero | outliers."
  , "  --all-variants     Run every variant (5x the work)."
  , "  --train N          Training examples (default: 100)."
  , "  --test N           Test/extrapolation examples (default: 400)."
  , "  --pop N            Population size (default: 500)."
  , "  --gens N           Generations (default: 80)."
  , "  --hoist-share P    Share of mutations that are hoist mutations."
  , "  --point-share P    Share of mutations that are point mutations."
  , "  --parsimony P      Fitness penalty per node."
  , "  --tournament N     Tournament size."
  , "  --metric M         rmse | mse | mae | huber. Recovery is always scored"
  , "                     by normalised RMSE against the truth, so this is a"
  , "                     like-for-like comparison of fitness metrics."
  , "  --selection S      tournament | roulette | pareto | lexicase |"
  , "                     age-fitness."
  , "  --binders on|off   Let generation emit binders (default: off)."
  , "  --binder-prob P    Chance an internal node is a binder."
  , "  --binder-terms N   Largest generated term count."
  , "  --binder-depth N   Binders permitted in a nest (1 = no nesting)."
  , "  --age-injection N  Fresh individuals injected per generation under"
  , "                     age-fitness selection (default: 1)."
  , "  --frequency-seeding on|off"
  , "                     Bias generated sin/cos arguments towards frequencies"
  , "                     detected in the data (default: on)."
  , "  --freq-top-k N     Candidate frequencies kept per variable."
  , "  --freq-weight P    Chance a trig node uses a detected frequency."
  , "  --freq-threshold R Peak-to-median ratio counting as signal."
  , "  --freq-resolution N  Frequencies scanned per variable."
  , "  --domain-penalty P Fitness penalty for the share of points where a"
  , "                     formula leaves its domain (default: 0, disabled)."
  , "  --elitism N        Individuals carried over verbatim (0 disables)."
  , "  --refine-every N   Generations between constant-refinement passes."
  , "  --no-refine        Disable constant refinement (the pre-LocalSearch"
  , "                     behaviour, for A/B comparison)."
  , "  --quick            Small, fast configuration for a smoke test."
  , "  --domain-breakdown Tabulate which domain errors actually occur, by"
  , "                     problem, instead of running the benchmark."
  , "  --format F         text | json (default: text)."
  , "  --verbose          Print the discovered formula for every run."
  , "  -h, --help         Show this message."
  , ""
  , "PROBLEMS"
  , "  polynomial rational power-law exponential logarithmic"
  , "  trigonometric mixed nested interaction"
  , ""
  , "EXAMPLES"
  , "  typedgp-bench --quick"
  , "  typedgp-bench --seeds 5 --format json > before.json"
  , "  typedgp-bench --problems power-law,exponential --verbose"
  , "  typedgp-bench --all-variants --seeds 5"
  ]
