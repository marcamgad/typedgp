-- | Command-line driver.
--
-- This module contains __no search logic__. Its job is: turn arguments
-- into a 'Config', obtain a 'Dataset', call into "TypedGP.Evolution", and
-- format what comes back. If you find yourself wanting to change how the
-- search behaves by editing this file, the knob you want belongs in
-- "TypedGP.Config" instead.
--
-- Cross-validation lives here rather than in the library because it is a
-- driver concern: it runs the same unchanged 'evolveOn' once per fold and
-- summarises the spread. Nothing about the evolution loop needs to know
-- that it is being called repeatedly.
module Main (main) where

import Control.Monad (forM, when)
import Data.List (sortOn)
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

import TypedGP.Checkpoint
  ( Checkpoint (..)
  , parseCheckpoint
  , renderCheckpoint
  )
import TypedGP.Config
  ( Config (..)
  , ErrorMetric (..)
  , defaultConfig
  , describeConfig
  , parseErrorMetric
  , parseSelectionStrategy
  )
import TypedGP.Data.Dataset
  ( Dataset
  , datasetSize
  , kFoldSplits
  , parseDataset
  , parseDouble
  , renderDataset
  , sampleDataset
  , splitDataset
  , splitOn
  )
import TypedGP.Evolution
  ( EvolutionResult (..)
  , GenStats (..)
  , StartPoint (..)
  , evolve
  , evolveIOOn
  , evolveOn
  , gsBestError
  , gsBestFitness
  , gsBestSize
  )
import TypedGP.Fitness (Fitness (..))
import TypedGP.Population
  ( Individual (..)
  , Population
  , populationFromExprs
  , populationIndividuals
  )
import TypedGP.Pretty (pretty)
import TypedGP.Random (Seed, mkSeed)
import TypedGP.Report
  ( JsonValue (..)
  , OutputFormat (..)
  , buildReport
  , encodeJson
  , parseOutputFormat
  , renderReportJson
  , renderReportText
  )
import TypedGP.Uncertainty (BootstrapResult, bootstrapConstants)

-- | Everything the CLI can be told, with the hyperparameters delegated to
-- a 'Config' rather than duplicated here.
data Options = Options
  { optConfig :: !Config
  , optSeed :: !Word64
  , optDataFile :: !(Maybe FilePath)
  , optSamples :: !Int
  , optSampleRange :: !(Double, Double)
  , optNoise :: !Double
  , optTrials :: !Int
  , optCvFolds :: !Int
  , optFormat :: !OutputFormat
  , optCheckpointEvery :: !Int
  , optCheckpointFile :: !FilePath
  , optResume :: !(Maybe FilePath)
  , optQuiet :: !Bool
  , optShowHelp :: !Bool
  , optDumpData :: !Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optConfig          = defaultConfig
  , optSeed            = 1
  , optDataFile        = Nothing
  , optSamples         = 60
  , optSampleRange     = (-3.0, 3.0)
  , optNoise           = 0.0
  , optTrials          = 1
  , optCvFolds         = 0
  , optFormat          = TextFormat
  , optCheckpointEvery = 0
  , optCheckpointFile  = "typedgp.checkpoint"
  , optResume          = Nothing
  , optQuiet           = False
  , optShowHelp        = False
  , optDumpData        = False
  }

-- | Seed for the built-in synthetic problem.
--
-- Fixed, and deliberately independent of @--seed@: @--seed@ varies the
-- /search/, not the /problem/. If changing the search seed also reshuffled
-- the training data, a multi-seed trial run would be measuring two things
-- at once and could not tell a robust configuration from a lucky dataset.
syntheticDataSeed :: Word64
syntheticDataSeed = 20260808

-- | The benchmark formula the engine is expected to rediscover:
-- @2x + sin(y)@.
benchmarkTarget :: [(String, Double)] -> Double
benchmarkTarget env = 2.0 * variable "x" + sin (variable "y")
  where
    variable :: String -> Double
    variable name = maybe 0.0 id (lookup name env)

main :: IO ()
main = do
  -- Force UTF-8 on stdout before anything is printed.
  --
  -- This is a root-cause fix, not a workaround for one character. GHC's
  -- default handle encoding follows the host locale, so under LANG=C (the
  -- default in minimal containers and many CI images) *any* non-ASCII
  -- character in any output string makes commitBuffer throw
  -- "invalid character". Replacing today's em dashes would leave the next
  -- one to rediscover the bug; pinning the encoding removes the class.
  hSetEncoding stdout utf8
  -- Line buffering so per-generation progress appears as it happens, even
  -- when stdout is a pipe.
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Left problem -> do
      putStrLn ("error: " ++ problem)
      putStrLn ""
      mapM_ putStrLn usage
      exitFailure
    Right opts
      | optShowHelp opts -> do
          mapM_ putStrLn usage
          exitSuccess
      | otherwise -> run opts

run :: Options -> IO ()
run opts = do
  loaded <- loadDataset opts
  case loaded of
    Left problem -> failWith problem
    Right dataset
      | optDumpData opts   -> putStr (renderDataset dataset)
      | optCvFolds opts > 1 -> runCrossValidation opts dataset
      | optTrials opts > 1  -> runTrials opts dataset
      | otherwise           -> runOnce opts dataset

failWith :: String -> IO ()
failWith problem = do
  putStrLn ("error: " ++ problem)
  exitFailure

-- | Obtain training data: from a file if given, otherwise the synthetic
-- benchmark.
loadDataset :: Options -> IO (Either String Dataset)
loadDataset opts = case optDataFile opts of
  Just path -> do
    contents <- readFile path
    return (parseDataset variables contents)
  Nothing
    | variables /= ["x", "y"] -> return (Left syntheticMismatch)
    | otherwise -> return (Right (syntheticDataset opts))
 where
  variables :: [String]
  variables = cfgVariables (optConfig opts)

  syntheticMismatch :: String
  syntheticMismatch =
    "the built-in synthetic problem is defined over variables x and y, but "
      ++ "--vars was set to " ++ unwords variables
      ++ ". Supply your own data with --data FILE, or drop --vars."

-- | The synthetic benchmark, optionally with Gaussian-ish noise added to
-- the targets.
--
-- Noise exists so the overfitting machinery can be exercised honestly: on
-- noiseless data every formula that fits training data perfectly also fits
-- holdout data perfectly, so no holdout gap can ever appear and the guard
-- would be untestable.
syntheticDataset :: Options -> Dataset
syntheticDataset opts = fst (sampleDataset variables range count noisyTarget seed)
  where
    variables :: [String]
    variables = cfgVariables (optConfig opts)

    range :: (Double, Double)
    range = optSampleRange opts

    count :: Int
    count = optSamples opts

    seed :: Seed
    seed = mkSeed syntheticDataSeed

    noisyTarget :: [(String, Double)] -> Double
    noisyTarget env = benchmarkTarget env + noiseFor env

    -- Deterministic pseudo-noise derived from the inputs themselves.
    --
    -- Not drawn from the PRNG, because sampleDataset computes targets from
    -- a pure function and threading a generator through it would change
    -- that interface for one flag. A high-frequency incommensurable
    -- combination of the inputs is uncorrelated with the smooth target in
    -- every way that matters here, which is all the test needs.
    noiseFor :: [(String, Double)] -> Double
    noiseFor env
      | optNoise opts <= 0 = 0.0
      | otherwise =
          let mixed = sum [v * 12.9898 + 78.233 | (_, v) <- env]
              unit = sin mixed * 43758.5453
          in optNoise opts * (unit - fromIntegral (floor unit :: Int) - 0.5) * 2.0

-- Single run -----------------------------------------------------------------

runOnce :: Options -> Dataset -> IO ()
runOnce opts dataset = do
  when wantBanner (mapM_ putStrLn (banner opts dataset))
  resumed <- loadResume opts dataset
  case resumed of
    Left problem -> failWith problem
    Right start -> do
      startedAt <- getMonotonicTime
      outcome <- evolveIOOn cfg training holdout start (reportGeneration opts)
      case outcome of
        Left problem -> failWith problem
        Right result -> do
          finishedAt <- getMonotonicTime
          let bootstrap = runBootstrap cfg training result
              report = buildReport cfg (optSeed opts) (finishedAt - startedAt)
                         bootstrap result
          case optFormat opts of
            TextFormat -> mapM_ putStrLn (renderReportText report)
            JsonFormat -> putStrLn (renderReportJson report)
          if erTargetReached result then exitSuccess else exitFailure
 where
  cfg :: Config
  cfg = optConfig opts

  wantBanner :: Bool
  wantBanner = not (optQuiet opts) && optFormat opts == TextFormat

  (training, holdout, searchSeed) = prepareSplit cfg (optSeed opts) dataset

  -- Resuming restores the population, generation counter and PRNG state
  -- from the file. The holdout split is *not* stored: it is re-derived
  -- from --seed, so a resume must use the same --seed to see the same
  -- split. Storing the split would make checkpoints far larger and would
  -- silently override a dataset the user had since corrected.
  loadResume :: Options -> Dataset -> IO (Either String StartPoint)
  loadResume options ds = case optResume options of
    Nothing -> return (Right (StartFresh searchSeed))
    Just path -> do
      contents <- readFile path
      case parseCheckpoint contents of
        Left problem -> return (Left ("cannot resume: " ++ problem))
        Right checkpoint
          | ckVariables checkpoint /= cfgVariables cfg ->
              return (Left (variableMismatch checkpoint))
          | otherwise -> do
              when wantBanner $ putStrLn $
                "Resuming from " ++ path ++ " at generation "
                  ++ show (ckGeneration checkpoint) ++ "."
              let pop = populationFromExprs cfg ds (ckExpressions checkpoint)
              return (Right (StartResumed (ckGeneration checkpoint) pop
                               (ckSeed checkpoint)))

  variableMismatch :: Checkpoint -> String
  variableMismatch checkpoint =
    "checkpoint was written for variables [" ++ unwords (ckVariables checkpoint)
      ++ "] but this run uses [" ++ unwords (cfgVariables cfg)
      ++ "]. Resuming would score expressions against the wrong columns."

-- | Split off a holdout set, reproducing exactly what 'evolve' would do
-- internally, and return the seed the search should start from.
prepareSplit :: Config -> Word64 -> Dataset -> (Dataset, Maybe Dataset, Seed)
prepareSplit cfg seed dataset = case cfgHoldout cfg of
  Nothing -> (dataset, Nothing, mkSeed seed)
  Just fraction ->
    let (training, holdout, next) = splitDataset fraction (mkSeed seed) dataset
    in (training, Just holdout, next)

-- | Bootstrap the discovered formula's constants, if asked.
--
-- Run against the training split only. The holdout has to stay untouched
-- by anything that shapes the reported formula, and refitting constants —
-- even only constants — would do exactly that.
runBootstrap :: Config -> Dataset -> EvolutionResult -> Maybe BootstrapResult
runBootstrap cfg training result
  | cfgBootstrapSamples cfg <= 0 = Nothing
  | otherwise = Just $
      bootstrapConstants cfg training (erBestSimplified result) (erFinalSeed result)

reportGeneration :: Options -> GenStats -> Population -> IO ()
reportGeneration opts stats pop = do
  when showProgress $ putStrLn $
    "gen " ++ padLeft 4 (show (gsGeneration stats))
      ++ " | best " ++ padLeft 10 (fixed 6 (gsBestFitness stats))
      ++ " | err " ++ padLeft 10 (fixed 6 (gsBestError stats))
      ++ holdoutColumn
      ++ " | mean " ++ padLeft 10 (fixed 4 (gsMeanFitness stats))
      ++ " | size " ++ padLeft 4 (show (gsBestSize stats))
      ++ " (avg " ++ fixed 1 (gsMeanSize stats) ++ ")"
      ++ " | " ++ pretty (gsBestSimplified stats)
  when shouldCheckpoint $
    writeFile (optCheckpointFile opts) (renderCheckpoint (checkpointOf stats pop))
 where
  showProgress :: Bool
  showProgress = not (optQuiet opts) && optFormat opts == TextFormat

  holdoutColumn :: String
  holdoutColumn = case gsHoldoutError stats of
    Nothing -> ""
    Just holdout -> " | hold " ++ padLeft 10 (fixed 6 holdout)

  shouldCheckpoint :: Bool
  shouldCheckpoint =
    optCheckpointEvery opts > 0
      && gsGeneration stats `mod` optCheckpointEvery opts == 0

  checkpointOf :: GenStats -> Population -> Checkpoint
  checkpointOf st population = Checkpoint
    { ckGeneration = gsGeneration st
    , ckSeed = gsSeed st
    , ckVariables = cfgVariables (optConfig opts)
    , ckExpressions = map indExpr (populationIndividuals population)
    }

-- Cross-validation ------------------------------------------------------------

-- | Run the search once per fold and report the spread of holdout errors.
--
-- The spread is the point. A low mean with a tight spread is evidence of a
-- stable relationship; the same mean with a wide spread means the search
-- found something different each time, and the average of those is not a
-- result.
runCrossValidation :: Options -> Dataset -> IO ()
runCrossValidation opts dataset = do
  when wantText (mapM_ putStrLn (banner opts dataset))
  when wantText $ do
    putStrLn ("Cross-validating over " ++ show (length folds) ++ " folds.")
    putStrLn ""
  outcomes <- forM (zip [1 :: Int ..] folds) runFold
  let errors = [e | Just e <- outcomes]
  case optFormat opts of
    TextFormat -> mapM_ putStrLn (crossValidationText opts errors)
    JsonFormat -> putStrLn (encodeJson (crossValidationJson opts outcomes errors))
  if length errors == length folds then exitSuccess else exitFailure
 where
  cfg :: Config
  cfg = optConfig opts

  wantText :: Bool
  wantText = optFormat opts == TextFormat && not (optQuiet opts)

  folds :: [(Dataset, Dataset)]
  folds = fst (kFoldSplits (optCvFolds opts) (mkSeed (optSeed opts)) dataset)

  runFold :: (Int, (Dataset, Dataset)) -> IO (Maybe Double)
  runFold (index, (training, validation)) = do
    let foldSeed = mkSeed (optSeed opts + fromIntegral index)
        outcome = evolveOn cfg training (Just validation) (StartFresh foldSeed)
    case outcome of
      Left problem -> do
        when wantText $ putStrLn ("  fold " ++ show index ++ ": error: " ++ problem)
        return Nothing
      Right result -> do
        let holdoutError = erHoldoutError result
        when wantText $ putStrLn $
          "  fold " ++ padLeft 2 (show index)
            ++ "  train err " ++ padLeft 10 (fixed 6 (fitError (indFitness (erBest result))))
            ++ "  holdout err " ++ padLeft 10 (maybe "n/a" (fixed 6) holdoutError)
            ++ "  " ++ pretty (erBestSimplified result)
        return holdoutError

crossValidationText :: Options -> [Double] -> [String]
crossValidationText opts errors
  | null errors = ["", "No fold produced a holdout error."]
  | otherwise =
      [ ""
      , replicate 72 '-'
      , "Cross-validated holdout " ++ show (cfgErrorMetric (optConfig opts))
      , "  folds      : " ++ show (length errors)
      , "  mean       : " ++ fixed 6 (mean errors)
      , "  std dev    : " ++ fixed 6 (stdDev errors)
      , "  min / max  : " ++ fixed 6 (minimum errors) ++ " / " ++ fixed 6 (maximum errors)
      , "  spread     : " ++ spreadVerdict errors
      ]

-- | Interpret the fold-to-fold spread.
--
-- Expressed relative to the mean, because an absolute standard deviation
-- is meaningless without knowing the scale of the target.
spreadVerdict :: [Double] -> String
spreadVerdict errors
  | m <= 0 = "all folds scored essentially zero error"
  | ratio < 0.25 = "tight (" ++ fixed 2 ratio ++ " of mean) - the relationship looks stable"
  | ratio < 0.75 = "moderate (" ++ fixed 2 ratio ++ " of mean) - treat the formula as provisional"
  | otherwise =
      "wide (" ++ fixed 2 ratio ++ " of mean) - the search found something "
        ++ "different on each fold, which is not a stable relationship"
  where
    m = mean errors
    ratio = stdDev errors / m

crossValidationJson :: Options -> [Maybe Double] -> [Double] -> JsonValue
crossValidationJson opts outcomes errors = JObj
  [ ("mode", JStr "cross-validation")
  , ("folds_requested", JInt (optCvFolds opts))
  , ("folds_completed", JInt (length errors))
  , ("metric", JStr (show (cfgErrorMetric (optConfig opts))))
  , ("fold_holdout_errors", JArr (map (maybe JNull JNum) outcomes))
  , ("mean", numberOrNull (mean errors))
  , ("std_dev", numberOrNull (stdDev errors))
  , ("min", numberOrNull (minimumOr 0 errors))
  , ("max", numberOrNull (maximumOr 0 errors))
  ]
  where
    numberOrNull :: Double -> JsonValue
    numberOrNull v = if null errors then JNull else JNum v

-- Trials ----------------------------------------------------------------------

-- | Repeat the run across consecutive seeds and report how often it
-- converged.
runTrials :: Options -> Dataset -> IO ()
runTrials opts dataset = do
  when wantText $ do
    mapM_ putStrLn (banner opts dataset)
    putStrLn ("Running " ++ show trials ++ " trials from seed " ++ show (optSeed opts) ++ ".")
    putStrLn ""
  outcomes <- mapM trial (zip [1 :: Int ..] seeds)
  let successes = length (filter fst outcomes)
  case optFormat opts of
    TextFormat -> do
      putStrLn ""
      putStrLn (replicate 72 '-')
      putStrLn $
        "Converged " ++ show successes ++ "/" ++ show trials
          ++ " (threshold " ++ fixed 4 (cfgTargetFitness cfg) ++ ")"
    JsonFormat -> putStrLn (encodeJson (trialsJson opts successes outcomes))
  if successes * 5 >= trials * 4  -- 80%, without floating point
    then exitSuccess
    else exitFailure
 where
  cfg :: Config
  cfg = optConfig opts

  wantText :: Bool
  wantText = optFormat opts == TextFormat && not (optQuiet opts)

  trials :: Int
  trials = optTrials opts

  seeds :: [Word64]
  seeds = [optSeed opts + fromIntegral k | k <- [0 .. trials - 1]]

  trial :: (Int, Word64) -> IO (Bool, Double)
  trial (index, seed) = case evolve cfg dataset (mkSeed seed) of
    Left problem -> do
      when wantText $ putStrLn (label index seed ++ "error: " ++ problem)
      return (False, 0.0)
    Right result ->
      let best = erBest result
          total = fitTotal (indFitness best)
          converged = erTargetReached result
      in do
        when wantText $ putStrLn $
          label index seed
            ++ (if converged then "ok   " else "miss ")
            ++ "fitness " ++ padLeft 10 (fixed 6 total)
            ++ "  gen " ++ padLeft 4 (show (erGenerationsRun result))
            ++ "  " ++ pretty (erBestSimplified result)
        return (converged, total)

  label :: Int -> Word64 -> String
  label index seed =
    "  trial " ++ padLeft 3 (show index) ++ " (seed " ++ padLeft 6 (show seed) ++ ")  "

trialsJson :: Options -> Int -> [(Bool, Double)] -> JsonValue
trialsJson opts successes outcomes = JObj
  [ ("mode", JStr "trials")
  , ("trials", JInt (optTrials opts))
  , ("converged", JInt successes)
  , ("threshold", JNum (cfgTargetFitness (optConfig opts)))
  , ("fitnesses", JArr [JNum f | (_, f) <- outcomes])
  ]

-- Shared presentation ----------------------------------------------------------

banner :: Options -> Dataset -> [String]
banner opts dataset =
  [ "TypedGP - symbolic regression"
  , ""
  , "Dataset: " ++ show (datasetSize dataset) ++ " points over "
      ++ unwords (cfgVariables (optConfig opts))
      ++ maybe " (synthetic: 2x + sin(y))" (" from " ++) (optDataFile opts)
  , "Seed:    " ++ show (optSeed opts)
  , "Config:"
  ]
    ++ describeConfig (optConfig opts)
    ++ [""]

-- Argument parsing ---------------------------------------------------------

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go :: Options -> [String] -> Either String Options
    go opts [] = Right opts
    go opts (arg : rest) = case arg of
      "--help"        -> go opts { optShowHelp = True } rest
      "-h"            -> go opts { optShowHelp = True } rest
      "--quiet"       -> go opts { optQuiet = True } rest
      "--dump-data"   -> go opts { optDumpData = True } rest
      "--no-simplify" -> go (overConfigIn opts (\c -> c { cfgSimplifyBest = False })) rest
      "--no-refine" -> go (overConfigIn opts (\c -> c { cfgRefineConstants = False })) rest
      "--lexicase-elites" ->
        go (overConfigIn opts (\c -> c { cfgLexicaseElites = True })) rest
      "--no-frequency-seeding" ->
        go (overConfigIn opts (\c -> c { cfgFrequencySeedingEnabled = False })) rest
      "--binders" ->
        go (overConfigIn opts (\c -> c { cfgEnableBinders = True })) rest
      -- A bare trailing argument is either an unknown flag or a known one
      -- whose value was forgotten; without a value we cannot tell which,
      -- so the message covers both rather than guessing wrong.
      _ -> case rest of
        [] -> Left (arg ++ ": unknown option, or its value is missing")
        (value : rest') -> do
          opts' <- applyFlag arg value opts
          go opts' rest'

    overConfigIn :: Options -> (Config -> Config) -> Options
    overConfigIn opts f = opts { optConfig = f (optConfig opts) }

applyFlag :: String -> String -> Options -> Either String Options
applyFlag flag value opts = case flag of
  "--pop"             -> intFlag (\n -> overConfig (\c -> c { cfgPopulationSize = n }))
  "--gens"            -> intFlag (\n -> overConfig (\c -> c { cfgGenerations = n }))
  "--init-depth"      -> intFlag (\n -> overConfig (\c -> c { cfgMaxInitialDepth = n }))
  "--max-depth"       -> intFlag (\n -> overConfig (\c -> c { cfgMaxDepth = n }))
  "--tournament"      -> intFlag (\n -> overConfig (\c -> c { cfgTournamentSize = n }))
  "--elitism"         -> intFlag (\n -> overConfig (\c -> c { cfgElitism = n }))
  "--chunk"           -> intFlag (\n -> overConfig (\c -> c { cfgParallelChunk = n }))
  "--bootstrap"       -> intFlag (\n -> overConfig (\c -> c { cfgBootstrapSamples = n }))
  "--refine-every"    -> intFlag (\n -> overConfig (\c -> c { cfgRefineEvery = n }))
  "--refine-iters"    -> intFlag (\n -> overConfig (\c -> c { cfgRefineIterations = n }))
  -- Documented in the help text since age-fitness landed, but the parse
  -- case was missing, so the flag errored as unknown.
  "--age-injection"   -> intFlag (\n -> overConfig (\c -> c { cfgAgeInjection = n }))
  -- Binders. These affect generation only; evaluation, printing and
  -- checkpointing handle a binder however it got there.
  "--binder-prob"     -> dblFlag (\p -> overConfig (\c -> c { cfgBinderProb = p }))
  "--binder-terms"    -> intFlag (\n -> overConfig (\c -> c { cfgBinderMaxTerms = n }))
  "--binder-depth"    -> intFlag (\n -> overConfig (\c -> c { cfgBinderDepthLimit = n }))
  -- Frequency seeding. Every field of the mechanism is reachable from the
  -- command line, per the one-place-for-hyperparameters rule; the on/off
  -- switch is the separate valueless --no-frequency-seeding above.
  "--freq-top-k"      -> intFlag (\n -> overConfig (\c -> c { cfgFrequencySeedTopK = n }))
  "--freq-resolution" -> intFlag (\n -> overConfig (\c -> c { cfgFrequencyResolution = n }))
  "--freq-weight"     -> dblFlag (\p -> overConfig (\c -> c { cfgFrequencySeedWeight = p }))
  "--freq-threshold"  -> dblFlag (\p -> overConfig (\c -> c { cfgFrequencySignalThreshold = p }))
  "--crossover"       -> dblFlag (\p -> overConfig (\c -> c { cfgCrossoverRate = p }))
  "--mutation"        -> dblFlag (\p -> overConfig (\c -> c { cfgMutationRate = p }))
  "--point-share"     -> dblFlag (\p -> overConfig (\c -> c { cfgPointMutationShare = p }))
  "--hoist-share"     -> dblFlag (\p -> overConfig (\c -> c { cfgHoistMutationShare = p }))
  "--parsimony"       -> dblFlag (\p -> overConfig (\c -> c { cfgParsimony = p }))
  -- A weight of 0, the default, skips the extra evaluation pass entirely
  -- rather than running it and multiplying the answer by zero.
  "--domain-penalty"  -> dblFlag (\p -> overConfig (\c -> c { cfgDomainPenalty = p }))
  "--terminal-prob"   -> dblFlag (\p -> overConfig (\c -> c { cfgTerminalProb = p }))
  "--const-prob"      -> dblFlag (\p -> overConfig (\c -> c { cfgConstProb = p }))
  "--const-jitter"    -> dblFlag (\p -> overConfig (\c -> c { cfgConstJitter = p }))
  "--target"          -> dblFlag (\p -> overConfig (\c -> c { cfgTargetFitness = p }))
  "--holdout"         -> dblFlag (\p -> overConfig (\c -> c { cfgHoldout = Just p }))
  "--const-range"     -> rangeFlag (\r -> overConfig (\c -> c { cfgConstRange = r }))
  "--sample-range"    -> rangeFlag (\r o -> o { optSampleRange = r })
  "--samples"         -> intFlag (\n o -> o { optSamples = n })
  "--noise"           -> dblFlag (\d o -> o { optNoise = d })
  "--trials"          -> intFlag (\n o -> o { optTrials = n })
  "--cv-folds"        -> intFlag (\n o -> o { optCvFolds = n })
  "--checkpoint-every" -> intFlag (\n o -> o { optCheckpointEvery = n })
  "--checkpoint-file" -> Right opts { optCheckpointFile = value }
  "--resume"          -> Right opts { optResume = Just value }
  "--data"            -> Right opts { optDataFile = Just value }
  "--seed"            -> case readWord value of
    Just w  -> Right opts { optSeed = w }
    Nothing -> Left (flag ++ ": expected a non-negative integer, got " ++ show value)
  "--format"          -> case parseOutputFormat value of
    Just f  -> Right opts { optFormat = f }
    Nothing -> Left (flag ++ ": expected text or json, got " ++ show value)
  "--vars"            -> case filter (not . null) (splitOn ',' value) of
    []   -> Left (flag ++ ": at least one variable name is required")
    vars -> Right (overConfig (\c -> c { cfgVariables = vars }) opts)
  "--metric"          -> case parseErrorMetric value of
    Just m  -> Right (overConfig (\c -> c { cfgErrorMetric = m }) opts)
    Nothing -> Left (flag ++ ": expected rmse, mse, mae or huber, got " ++ show value)
  -- Applies only when --metric huber is in force; silently ignored
  -- otherwise would be a trap, so it is rejected instead.
  "--huber-delta"     -> case parseDouble value of
    Just d | d > 0 -> case cfgErrorMetric (optConfig opts) of
      Huber _ -> Right (overConfig (\c -> c { cfgErrorMetric = Huber d }) opts)
      _ -> Left (flag ++ ": only applies with --metric huber (set that first)")
    _ -> Left (flag ++ ": expected a positive number, got " ++ show value)
  "--selection"       -> case parseSelectionStrategy value of
    Just s  -> Right (overConfig (\c -> c { cfgSelection = s }) opts)
    Nothing -> Left (flag ++ ": expected tournament, roulette, pareto or lexicase, got " ++ show value)
  _ -> Left ("unknown option: " ++ flag)
 where
  overConfig :: (Config -> Config) -> Options -> Options
  overConfig f o = o { optConfig = f (optConfig o) }

  intFlag :: (Int -> Options -> Options) -> Either String Options
  intFlag f = case readInt value of
    Just n  -> Right (f n opts)
    Nothing -> Left (flag ++ ": expected an integer, got " ++ show value)

  dblFlag :: (Double -> Options -> Options) -> Either String Options
  dblFlag f = case parseDouble value of
    Just d  -> Right (f d opts)
    Nothing -> Left (flag ++ ": expected a number, got " ++ show value)

  rangeFlag :: ((Double, Double) -> Options -> Options) -> Either String Options
  rangeFlag f = case splitOn ':' value of
    [lowText, highText] -> case (parseDouble lowText, parseDouble highText) of
      (Just low, Just high) -> Right (f (low, high) opts)
      _ -> Left (flag ++ ": expected LOW:HIGH, got " ++ show value)
    _ -> Left (flag ++ ": expected LOW:HIGH, got " ++ show value)

readInt :: String -> Maybe Int
readInt text = case reads text :: [(Int, String)] of
  [(n, "")] -> Just n
  _         -> Nothing

readWord :: String -> Maybe Word64
readWord text = case reads text :: [(Word64, String)] of
  [(w, "")] -> Just w
  _         -> Nothing

-- Formatting ---------------------------------------------------------------

fixed :: Int -> Double -> String
fixed places value
  | isNaN value = "NaN"
  | isInfinite value = if value > 0 then "Infinity" else "-Infinity"
  | otherwise = showFFloat (Just places) value ""

padLeft :: Int -> String -> String
padLeft width text = replicate (max 0 (width - length text)) ' ' ++ text

mean :: [Double] -> Double
mean [] = 0.0
mean xs = sum xs / fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev xs
  | length xs < 2 = 0.0
  | otherwise =
      let m = mean xs
      in sqrt (sum [(x - m) * (x - m) | x <- xs] / fromIntegral (length xs - 1))

minimumOr :: Double -> [Double] -> Double
minimumOr fallback xs = case sortOn id xs of
  (v : _) -> v
  []      -> fallback

maximumOr :: Double -> [Double] -> Double
maximumOr fallback xs = case reverse (sortOn id xs) of
  (v : _) -> v
  []      -> fallback

usage :: [String]
usage =
  [ "typedgp - symbolic regression by genetic programming"
  , ""
  , "USAGE"
  , "  typedgp [OPTIONS]"
  , ""
  , "  With no options, runs the built-in synthetic benchmark 2x + sin(y)."
  , "  Exit status is 0 when the run reached --target, 1 otherwise."
  , ""
  , "BEFORE YOU TRUST A RESULT"
  , "  Use --holdout 0.3 and check the holdout error, then --cv-folds 5 and"
  , "  check the spread, then --bootstrap 500 and check the intervals. A"
  , "  formula that survives all three is worth investigating; one that has"
  , "  only been fitted is not a finding. See the README checklist."
  , ""
  , "DATA"
  , "  --data FILE          Load training data instead of the synthetic problem."
  , "                       One example per line, whitespace and/or comma"
  , "                       separated, target value last. '#' starts a comment."
  , "  --vars a,b,c         Input variable names (default: x,y)."
  , "  --samples N          Synthetic examples to generate (default: 60)."
  , "  --sample-range LO:HI Range synthetic inputs are drawn from (default: -3:3)."
  , "  --noise A            Add deterministic pseudo-noise of amplitude A to the"
  , "                       synthetic targets (default: 0). Needed to make"
  , "                       overfitting observable at all."
  , "  --dump-data          Print the dataset in the input format and exit."
  , ""
  , "VALIDATION"
  , "  --holdout F          Hold out fraction F (0 < F <= 0.5) from fitting and"
  , "                       report its error separately. Strongly recommended."
  , "  --cv-folds N         K-fold cross-validation; reports the spread of"
  , "                       holdout error across folds."
  , "  --bootstrap N        Bootstrap resamples for confidence intervals on the"
  , "                       discovered constants (default: 0, off)."
  , ""
  , "SEARCH"
  , "  --pop N              Population size (default: 600)."
  , "  --gens N             Maximum generations (default: 60)."
  , "  --init-depth N       Max depth of the initial population (default: 4)."
  , "  --max-depth N        Hard depth ceiling for every operator (default: 8)."
  , "  --crossover P        Crossover probability (default: 0.70)."
  , "  --mutation P         Mutation probability (default: 0.25)."
  , "  --point-share P      Share of mutations that are point mutations (0.40)."
  , "  --hoist-share P      Share of mutations that are hoist mutations (0.10)."
  , "  --selection S        tournament | roulette | pareto | lexicase |"
  , "                       age-fitness."
  , "                       pareto reports the whole accuracy/simplicity front"
  , "                       instead of pre-committing to a --parsimony weight."
  , "  --binders            Let generation emit sum(i=lo..hi, body) binders."
  , "                       Off by default. Evaluation, printing and"
  , "                       checkpointing handle them regardless."
  , "  --binder-prob P      Chance an internal node is a binder (default: 0.1)."
  , "  --binder-terms N     Largest generated term count (default: 10)."
  , "  --binder-depth N     Binders permitted in a nest (default: 1, no nesting)."
  , "  --age-injection N    Fresh individuals injected per generation under"
  , "                       age-fitness selection (default: 1). Ignored by"
  , "                       every other strategy."
  , "  --tournament N       Tournament size (default: 5)."
  , "  --elitism N          Individuals carried over verbatim (default: 2)."
  , "  --parsimony P        Fitness penalty per node (default: 0.002)."
  , "  --domain-penalty P   Penalty for the share of points where the formula"
  , "                       leaves its domain: divides by zero, takes a log of a"
  , "                       negative, saturates. Default 0, which disables the"
  , "                       check and costs nothing."
  , "  --metric M           rmse | mse | mae | huber (default: rmse)."
  , "                       huber is quadratic near zero and linear beyond a"
  , "                       threshold: use it when the data has outliers."
  , "  --huber-delta D      Huber threshold, in units of the target's standard"
  , "                       deviation (default: 0.1). Needs --metric huber."
  , "  --terminal-prob P    Grow-method early-stop probability (default: 0.35)."
  , "  --const-prob P       Probability a terminal is a constant (default: 0.30)."
  , "  --const-range LO:HI  Range for generated constants (default: -5:5)."
  , "  --const-jitter P     Max constant perturbation per point mutation (0.50)."
  , "  --target P           Stop early at this fitness (default: 0.05)."
  , "  --no-simplify        Do not algebraically simplify elites."
  , "  --no-refine          Do not numerically optimise the elites. constants."
  , "  --lexicase-elites    Under --selection lexicase, choose elites with the"
  , "                       lexicase filter too, rather than by scalar fitness."
  , "  --refine-every N     Generations between refinement passes (default: 5)."
  , "  --refine-iters N     Simplex iterations per refinement (default: 120)."
  , ""
  , "FREQUENCY SEEDING"
  , "  Detects periodic structure in the data and biases the multiplier inside"
  , "  generated sin/cos arguments towards it. Inert when nothing is detected."
  , "  --no-frequency-seeding  Generate trig arguments without any bias."
  , "  --freq-top-k N       Candidate frequencies kept per variable (default: 3)."
  , "  --freq-weight P      Chance a trig node uses a detected frequency rather"
  , "                       than an arbitrary subtree (default: 0.5)."
  , "  --freq-threshold R   Peak-to-median ratio a spectral peak must clear to"
  , "                       count as signal (default: 3.0). Higher is stricter."
  , "  --freq-resolution N  Frequencies scanned per variable (default: 256)."
  , ""
  , "RUN"
  , "  --seed N             PRNG seed (default: 1). Determines the run entirely."
  , "  --trials N           Run N consecutive seeds and report the convergence"
  , "                       rate. Exit status is 0 when at least 80% converged."
  , "  --format F           text | json (default: text)."
  , "  --checkpoint-every N Write a checkpoint every N generations (0 = never)."
  , "  --checkpoint-file P  Where to write it (default: typedgp.checkpoint)."
  , "  --resume FILE        Resume a run from a checkpoint."
  , "                       A checkpoint stores only the population, generation"
  , "                       and PRNG state - NOT the config. Resuming with"
  , "                       different flags silently continues a DIFFERENT"
  , "                       search: drop --selection lexicase on resume and it"
  , "                       becomes a tournament run, with no warning. Repeat"
  , "                       the original command line, changing only --resume."
  , "  --chunk N            Individuals per parallel spark (default: 32)."
  , "  --quiet              Suppress the banner and per-generation lines."
  , "  -h, --help           Show this message."
  , ""
  , "EXAMPLES"
  , "  typedgp --holdout 0.3 --bootstrap 500"
  , "  typedgp --cv-folds 5 --quiet --format json"
  , "  typedgp --selection pareto --parsimony 0"
  , "  typedgp --noise 0.5 --holdout 0.3 --parsimony 0 --max-depth 12"
  ]
