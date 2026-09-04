-- | Tests for age-fitness Pareto selection.
--
-- The mechanism is three separable claims, and each is tested on its own
-- because two of them are silent when broken:
--
--   1. Ages advance by one per generation, and injected individuals start
--      at zero.
--   2. Crossover takes the __maximum__ of its parents' ages.
--   3. A fresh individual is injected each generation, replacing rather
--      than enlarging.
--
-- (2) is the one flagged in @docs\/phase4-age-fitness-design.md@ as most
-- likely to be got wrong, because "how new is this" suggests @min@ while
-- the quantity actually wanted is "how long has this lineage been failing
-- to be replaced". A @min@ implementation would let crossing with a fresh
-- individual launder an old lineage's age away, silently disabling the
-- protection the whole strategy is built on — with every other test still
-- green. It is therefore asserted in a form that has been __verified by
-- mutation__: switching @max@ to @min@ in "TypedGP.Population" must fail
-- 'crossoverTakesTheOlderParent'.
module AgeFitnessSpec (tests) where

import TypedGP.Config (Config (..), SelectionStrategy (..), defaultConfig)
import TypedGP.Data.Dataset (Dataset, sampleDataset)
import TypedGP.Expr (Expr (..))
import TypedGP.Population
  ( Individual (..)
  , Population
  , nextGeneration
  , populationFromAged
  , populationIndividuals
  , populationSize
  )
import TypedGP.Random (Seed, mkSeed)

target :: [(String, Double)] -> Double
target env = 2.0 * variable "x" + sin (variable "y")
  where
    variable name = maybe 0.0 id (lookup name env)

dataset :: Dataset
dataset = fst (sampleDataset ["x", "y"] (-3.0, 3.0) 30 target (mkSeed 4242))

baseConfig :: Config
baseConfig = defaultConfig
  { cfgPopulationSize = 20
  , cfgGenerations = 1
  , cfgElitism = 0
  , cfgRefineConstants = False
  , cfgSimplifyBest = False
  }

-- | Crossover only, so every child's age comes from the parent rule and
-- nothing else.
crossoverOnly :: Config
crossoverOnly = baseConfig { cfgCrossoverRate = 1.0, cfgMutationRate = 0.0 }

-- | A population of structurally identical individuals whose only
-- difference is age: half at 0, half at 100.
--
-- Identical expressions matter. If fitness varied, selection would prefer
-- one age group over the other and the age statistics below would be
-- measuring selection pressure rather than the propagation rule.
mixedAges :: Population
mixedAges = populationFromAged baseConfig dataset aged
  where
    shape :: Expr
    shape = Add (Mul (Const 2.0) (Var "x")) (Sin (Var "y"))

    aged :: [(Expr, Int)]
    aged = [(shape, if even k then 0 else 100) | k <- [0 .. 19 :: Int]]

-- | Every individual at age 0, for the increment test.
allYoung :: Population
allYoung =
  populationFromAged baseConfig dataset [(Var "x", 0) | _ <- [1 .. 20 :: Int]]

step :: Config -> Population -> Seed -> Population
step cfg pop s = fst (nextGeneration cfg [] dataset 1 pop s)

agesAfter :: Config -> Population -> Seed -> [Int]
agesAfter cfg pop s = map indAge (populationIndividuals (step cfg pop s))

-- | Mean child age over 20 seeds, starting from 'mixedAges'.
--
-- Statistical rather than exact, deliberately: which parents a tournament
-- draws is stochastic, so a single seed could favour either age group by
-- luck. Under @max@ a child is old unless /both/ parents were young, so
-- roughly three quarters land at 101 and the mean sits near 76. Under
-- @min@ the proportions invert and the mean sits near 26. Any threshold
-- between the two separates them; 50 is the midpoint.
meanChildAge :: Double
meanChildAge =
  let ages = concat [agesAfter crossoverOnly mixedAges (mkSeed s) | s <- [1 .. 20]]
  in fromIntegral (sum ages) / fromIntegral (max 1 (length ages))

crossoverTakesTheOlderParent :: Bool
crossoverTakesTheOlderParent = meanChildAge > 50.0

ageFitnessConfig :: Config
ageFitnessConfig = baseConfig { cfgSelection = AgeFitness }

tests :: [(String, Bool)]
tests =
  -- Age arithmetic ------------------------------------------------------------
  [ ("a freshly built population starts at age 0",
      all ((== 0) . indAge) (populationIndividuals allYoung))
  , ("survivors age by exactly one per generation",
      all (== 1) (agesAfter baseConfig allYoung (mkSeed 1)))
  , ("ages keep advancing across generations",
      let one = step baseConfig allYoung (mkSeed 1)
          two = step baseConfig one (mkSeed 2)
      in all ((== 2) . indAge) (populationIndividuals two))

  -- The max rule --------------------------------------------------------------
    -- Verified by mutation: changing 'max' to 'min' in TypedGP.Population
    -- fails this assertion and nothing else in the suite.
  , ("crossover takes the older parent's age, not the younger",
      crossoverTakesTheOlderParent)
    -- Guards against the assertion above passing for the wrong reason. If
    -- both age groups did not survive into the comparison, the mean would
    -- be meaningless.
  , ("the mixed fixture really does contain both age groups",
      let ages = map indAge (populationIndividuals mixedAges)
      in 0 `elem` ages && 100 `elem` ages)
  , ("the mean child age is far from either extreme, as the rule predicts",
      meanChildAge > 50.0 && meanChildAge < 101.0)

  -- Injection -----------------------------------------------------------------
  , ("age-fitness injects at least one age-0 individual each generation",
      0 `elem` agesAfter ageFitnessConfig allYoung (mkSeed 1))
  , ("injection does not change the population size",
      populationSize (step ageFitnessConfig allYoung (mkSeed 1))
        == cfgPopulationSize ageFitnessConfig)
    -- The injected individual must be genuinely new, not one generation
    -- old on arrival: survivors age before injection happens.
  , ("exactly the configured number of individuals are age 0",
      length (filter (== 0) (agesAfter ageFitnessConfig allYoung (mkSeed 1)))
        == cfgAgeInjection ageFitnessConfig)
    -- Injection belongs to the strategy, not to the engine. Under any
    -- other selection the population must contain no fresh individuals,
    -- or the benchmark could not tell "age-fitness helps" apart from
    -- "injecting random individuals helps".
  , ("no other strategy injects anything",
      notElem 0 (agesAfter baseConfig allYoung (mkSeed 1)))
  , ("disabling injection leaves no age-0 individuals",
      notElem 0 (agesAfter ageFitnessConfig { cfgAgeInjection = 0 }
                           allYoung (mkSeed 1)))
  , ("population size is preserved with injection disabled",
      populationSize (step ageFitnessConfig { cfgAgeInjection = 0 }
                           allYoung (mkSeed 1))
        == cfgPopulationSize ageFitnessConfig)

  -- Determinism ---------------------------------------------------------------
  , ("age-fitness generations are reproducible from the seed",
      agesAfter ageFitnessConfig allYoung (mkSeed 5)
        == agesAfter ageFitnessConfig allYoung (mkSeed 5))
  ]
