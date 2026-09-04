-- | End-to-end tests for the evolution loop.
--
-- The tests here fall into two classes, and it is worth being explicit
-- about which is which:
--
--   * __Structural invariants__ (determinism, generation counting, early
--     stopping, monotone elitism, config validation). These hold exactly,
--     for every seed, and a failure is unambiguously a bug.
--
--   * __The convergence test__ — run the search against a known formula
--     and check it is rediscovered. This is the test that proves the
--     system actually works rather than merely type-checks, but it is
--     statistical: it asserts a /majority/ of seeds converge, not all of
--     them. Genetic programming does not converge on every seed and a test
--     that demanded it would be flaky by construction.
--
-- If the convergence assertion fails, check the structural ones first. If
-- they all pass, the search is wired up correctly and the failure is about
-- tuning ('cfgPopulationSize', 'cfgGenerations', 'cfgParsimony') rather
-- than correctness.
module EvolutionSpec (tests) where

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Data.Dataset
  ( DataPoint (..)
  , Dataset
  , datasetPoints
  , datasetSize
  , sampleDataset
  )
import Data.List (isInfixOf)

import TypedGP.Evolution
  ( EvolutionResult (..)
  , GenStats (..)
  , evolve
  , gsBestFitness
  , gsGeneration
  , runWarnings
  )
import TypedGP.Expr (Expr)
import TypedGP.Fitness (Fitness (..))
import TypedGP.Population (Individual (..))
import TypedGP.Random (Seed, mkSeed)
import TypedGP.Spectral (frequencyTable)

-- | The target: @2x + sin(y)@.
target :: [(String, Double)] -> Double
target env = 2.0 * variable "x" + sin (variable "y")
  where
    variable :: String -> Double
    variable name = maybe 0.0 id (lookup name env)

-- | 40 training points over @[-3, 3]@, from a fixed seed so the problem
-- instance is identical for every seed the search is run with.
benchmark :: Dataset
benchmark = fst (sampleDataset ["x", "y"] (-3.0, 3.0) 40 target (mkSeed 4242))

-- | Search settings for the test suite: smaller than 'defaultConfig' so
-- the suite stays quick, large enough to find a two-term formula.
testConfig :: Config
testConfig = defaultConfig
  { cfgPopulationSize = 400
  , cfgGenerations = 40
  , cfgTargetFitness = 0.05
  }

-- | Seeds the convergence rate is measured over.
convergenceSeeds :: [Seed]
convergenceSeeds = map mkSeed [1, 2, 3, 4, 5]

-- | A deliberately overfittable problem: few examples, real noise, no
-- parsimony pressure, and enough depth to memorise.
--
-- Noise is what makes this possible at all. On the noiseless benchmark a
-- formula that fits training data perfectly also fits holdout data
-- perfectly, so no gap can ever open and the guard would be untestable —
-- it would look like it worked while never having been exercised.
noisyBenchmark :: Dataset
noisyBenchmark = fst (sampleDataset ["x", "y"] (-3.0, 3.0) 24 noisyTarget (mkSeed 99))
  where
    noisyTarget :: [(String, Double)] -> Double
    noisyTarget env =
      let x = variable "x"
          y = variable "y"
          mixed = sin (x * 12.9898 + y * 78.233) * 43758.5453
          jitter = mixed - fromIntegral (floor mixed :: Int) - 0.5
      in 2.0 * x + sin y + 3.0 * jitter
      where
        variable name = maybe 0.0 id (lookup name env)

overfitConfig :: Config
overfitConfig = testConfig
  { cfgPopulationSize = 400
  , cfgGenerations = 60
  , cfgParsimony = 0.0
  , cfgMaxDepth = 14
  , cfgHoldout = Just 0.3
  , cfgTargetFitness = 0.0
  }

overfitRun :: Maybe EvolutionResult
overfitRun = case evolve overfitConfig noisyBenchmark (mkSeed 1) of
  Left _       -> Nothing
  Right result -> Just result

-- | Training error of the overfitting run's winner.
overfitTrainingError :: Double
overfitTrainingError = maybe 0.0 (fitError . indFitness . erBest) overfitRun

overfitHoldoutError :: Double
overfitHoldoutError = maybe 0.0 (\r -> maybe 0.0 id (erHoldoutError r)) overfitRun

runWith :: Config -> Seed -> Either String EvolutionResult
runWith cfg = evolve cfg benchmark

bestTotal :: EvolutionResult -> Double
bestTotal = fitTotal . indFitness . erBest

bestExprOf :: EvolutionResult -> Expr
bestExprOf = indExpr . erBest

-- | Total fitness for a seed, or 'Nothing' if the run failed outright.
fitnessFor :: Seed -> Maybe Double
fitnessFor s = case runWith testConfig s of
  Left _       -> Nothing
  Right result -> Just (bestTotal result)

achievedFitnesses :: [Double]
achievedFitnesses = [f | Just f <- map fitnessFor convergenceSeeds]

converged :: Int
converged = length (filter (<= cfgTargetFitness testConfig) achievedFitnesses)

tests :: [(String, Bool)]
tests =
  -- Fixture sanity --------------------------------------------------------
  [ ("the benchmark dataset has the requested size", datasetSize benchmark == 40)
  , ("the benchmark targets are varied, not constant",
      length (dedupeDoubles (map dpTarget (datasetPoints benchmark))) > 30)
  , ("every benchmark point binds both variables",
      all (\p -> map fst (dpInputs p) == ["x", "y"]) (datasetPoints benchmark))

  -- Configuration ---------------------------------------------------------
  , ("an invalid population size is rejected",
      isLeft (runWith testConfig { cfgPopulationSize = 1 } (mkSeed 1)))
  , ("elitism at or above the population size is rejected",
      isLeft (runWith testConfig { cfgElitism = 400 } (mkSeed 1)))
  , ("rates summing above 1 are rejected",
      isLeft (runWith testConfig { cfgCrossoverRate = 0.8, cfgMutationRate = 0.5 }
                (mkSeed 1)))
  , ("an empty variable list is rejected",
      isLeft (runWith testConfig { cfgVariables = [] } (mkSeed 1)))
  , ("every configuration problem is reported at once",
      case runWith testConfig { cfgPopulationSize = 0, cfgTournamentSize = 0 }
             (mkSeed 1) of
        Left message -> countSubstring "must" message >= 2
        Right _      -> False)

  -- Loop mechanics --------------------------------------------------------
  , ("a run is fully determined by its seed",
      case (runWith quickConfig (mkSeed 9), runWith quickConfig (mkSeed 9)) of
        (Right a, Right b) -> bestExprOf a == bestExprOf b && bestTotal a == bestTotal b
        _                  -> False)
  , ("different seeds explore differently",
      case (runWith quickConfig (mkSeed 9), runWith quickConfig (mkSeed 10)) of
        (Right a, Right b) -> erHistory a /= erHistory b
        _                  -> False)
  , ("an unreachable target runs the full generation budget",
      case runWith exhaustiveConfig (mkSeed 3) of
        Right result ->
          erGenerationsRun result == cfgGenerations exhaustiveConfig
            && length (erHistory result) == cfgGenerations exhaustiveConfig + 1
        Left _ -> False)
  , ("a trivially reachable target stops at generation 0",
      case runWith trivialTargetConfig (mkSeed 3) of
        Right result ->
          erGenerationsRun result == 0 && length (erHistory result) == 1
        Left _ -> False)
  , ("zero generations still produces the initial population",
      case runWith quickConfig { cfgGenerations = 0 } (mkSeed 3) of
        Right result -> length (erHistory result) == 1
        Left _       -> False)
  , ("generations are numbered consecutively from 0",
      case runWith exhaustiveConfig (mkSeed 4) of
        Right result ->
          map gsGeneration (erHistory result) == [0 .. cfgGenerations exhaustiveConfig]
        Left _ -> False)
  , ("with elitism the best fitness never gets worse",
      case runWith exhaustiveConfig (mkSeed 5) of
        Right result -> nonIncreasing (map gsBestFitness (erHistory result))
        Left _       -> False)
  , ("the reported best matches the best generation in the history",
      case runWith exhaustiveConfig (mkSeed 6) of
        Right result ->
          bestTotal result == minimumOr 0.0 (map gsBestFitness (erHistory result))
        Left _ -> False)
    -- Elitism already guarantees non-increase, so the informative
    -- assertion is that breeding produces a *strict* improvement — i.e.
    -- the operators are doing something and the loop is not just
    -- preserving generation zero.
  , ("breeding strictly improves on the initial population",
      case runWith exhaustiveConfig (mkSeed 7) of
        Right result -> case map gsBestFitness (erHistory result) of
          (initial : rest) -> minimumOr initial rest < initial
          []               -> False
        Left _ -> False)

  -- Convergence -----------------------------------------------------------
  , ("every seed gets clearly better than random",
      length achievedFitnesses == length convergenceSeeds
        && all (< 0.75) achievedFitnesses)
  , ("the best seed finds a very good fit",
      minimumOr 1.0e9 achievedFitnesses < 0.1)
  , ("a majority of seeds recover 2x + sin(y) to the target threshold",
      converged * 2 > length convergenceSeeds)

  -- Frequency seeding --------------------------------------------------------
    -- Seeding is a no-op on this benchmark, and that is the correct
    -- behaviour rather than a failure. The target is 2x + sin(y): viewed
    -- from y, the 2x term is not a function of y at all, so it enters the
    -- spectrum as noise with roughly 3.5x the amplitude of the sin(y)
    -- signal being looked for. Detrending removes only the *linear* part,
    -- and 2x is not linear in y. The peak therefore never clears the
    -- signal threshold, the detector correctly reports nothing, and
    -- generation falls back to its unbiased path.
    --
    -- This is design-note open question 2 (multivariate targets) showing
    -- up on the demo problem rather than only on `mixed`, and it is the
    -- reason the prediction for `mixed` is "no benefit".
    --
    -- 'SpectralSpec' proves the detector itself works on single-variable
    -- signals, so this asserts the fallback, not a broken detector.
  , ("seeding is inert on a target whose periodicity is masked by another variable",
      generationZeroFitness seedOnConfig == generationZeroFitness seedOffConfig)
  , ("that inertness is exact, not approximate",
      and [ bestAtSeed seedOnConfig s == bestAtSeed seedOffConfig s
          | s <- map mkSeed [1 .. 25]
          ])
    -- The cause, asserted directly rather than inferred from the two
    -- assertions above being equal. Those would also pass if seeding were
    -- wired up wrong and never consulted at all; this one pins the reason
    -- to the detector's own verdict on this dataset.
  , ("the detector finds no usable signal in 2x + sin(y)",
      not (any (not . null . snd) (frequencyTable seedOnConfig benchmark)))

  -- Holdout discipline ----------------------------------------------------
  , ("a holdout split is actually held back from fitting",
      case runWith testConfig { cfgHoldout = Just 0.25 } (mkSeed 3) of
        Right result ->
          erTrainingSize result + erHoldoutSize result == datasetSize benchmark
            && erHoldoutSize result > 0
        Left _ -> False)
  , ("no holdout means no holdout error",
      case runWith quickConfig (mkSeed 3) of
        Right result -> erHoldoutError result == Nothing && erHoldoutSize result == 0
        Left _       -> False)
  , ("a holdout error is reported when a split exists",
      case runWith quickConfig { cfgHoldout = Just 0.25 } (mkSeed 3) of
        Right result -> erHoldoutError result /= Nothing
        Left _       -> False)
  , ("every generation records a holdout error when split",
      case runWith quickConfig { cfgHoldout = Just 0.25 } (mkSeed 3) of
        Right result -> all (\g -> gsHoldoutError g /= Nothing) (erHistory result)
        Left _       -> False)

  -- The overfitting guard, on a real overfit --------------------------------
    -- This is the test that stops the holdout machinery from being
    -- decorative. It plants a genuinely overfittable problem — noisy
    -- targets, no parsimony pressure, depth to spare — and requires the
    -- reported holdout error to actually diverge from training error. If
    -- this ever passes trivially (both errors equal), the noise has stopped
    -- working and the guard is no longer being exercised.
  , ("the overfitting run produced a result at all", overfitRun /= Nothing)
  , ("the overfitting run measured a holdout error", overfitHoldoutError > 0)
  , ("training error on noisy data is driven low",
      overfitTrainingError < 1.5)
  , ("holdout error is visibly worse than training error",
      overfitHoldoutError > overfitTrainingError * 1.3)
  , ("disabling parsimony raises a warning",
      maybe False (any (isInfixOf "parsimony") . erWarnings) overfitRun)
  , ("a run without a holdout warns about it",
      case runWith quickConfig (mkSeed 3) of
        Right result -> any (isInfixOf "holdout") (erWarnings result)
        Left _       -> False)
  , ("a tiny dataset warns about the example-to-input ratio",
      any (isInfixOf "example")
        (runWarnings testConfig 6))
  , ("an adequate dataset does not raise the ratio warning",
      not (any (isInfixOf "examples per input") (runWarnings testConfig 400)))
  ]
 where
  -- A cheap config for tests that only care about loop mechanics, not
  -- about search quality.
  quickConfig :: Config
  quickConfig = testConfig { cfgPopulationSize = 60, cfgGenerations = 8 }

  -- Target 0 is unreachable in practice (it would need an exact fit with
  -- zero size penalty), so the run always uses its full budget.
  -- Frequency seeding is off here on purpose, and pinning it matters even
  -- though it happens to be inert on this benchmark today (see the seeding
  -- assertions above). These assertions are about the *breeding* loop, and
  -- they are sensitive to the exact contents of generation zero: leaving
  -- the flag at its default would make them silently dependent on a
  -- detector decision that is not what they are testing.
  exhaustiveConfig :: Config
  exhaustiveConfig = quickConfig
    { cfgTargetFitness = 0.0
    , cfgGenerations = 10
    , cfgFrequencySeedingEnabled = False
    }

  -- A target so loose that generation 0 satisfies it, exercising the
  -- early-stopping path.
  trivialTargetConfig :: Config
  trivialTargetConfig = quickConfig { cfgTargetFitness = 1.0e12 }

  seedOnConfig, seedOffConfig :: Config
  seedOnConfig = quickConfig { cfgFrequencySeedingEnabled = True }
  seedOffConfig = quickConfig { cfgFrequencySeedingEnabled = False }

  -- Best fitness at generation zero, before any breeding.
  generationZeroFitness :: Config -> Double
  generationZeroFitness cfg =
    case runWith cfg { cfgGenerations = 0 } (mkSeed 4) of
      Right result -> case map gsBestFitness (erHistory result) of
        (initial : _) -> initial
        []            -> 1.0e9
      Left _ -> 1.0e9

  bestAtSeed :: Config -> Seed -> Double
  bestAtSeed cfg s = case runWith cfg { cfgGenerations = 0 } s of
    Right result -> case map gsBestFitness (erHistory result) of
      (initial : _) -> initial
      []            -> 1.0e9
    Left _ -> 1.0e9

  isLeft :: Either String EvolutionResult -> Bool
  isLeft (Left _) = True
  isLeft _        = False

  nonIncreasing :: [Double] -> Bool
  nonIncreasing xs = and (zipWith (>=) xs (drop 1 xs))

  -- The fallback is used only for an empty list; it must NOT take part in
  -- the comparison, or 'minimumOr 0.0 [1,2,3]' would answer 0.0.
  minimumOr :: Double -> [Double] -> Double
  minimumOr fallback []       = fallback
  minimumOr _        (x : xs) = foldr min x xs

  dedupeDoubles :: [Double] -> [Double]
  dedupeDoubles = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

  countSubstring :: String -> String -> Int
  countSubstring needle haystack
    | null needle = 0
    | otherwise = length
        [ () | rest <- tails' haystack, take (length needle) rest == needle ]

  tails' :: [a] -> [[a]]
  tails' [] = [[]]
  tails' xs@(_ : rest) = xs : tails' rest
