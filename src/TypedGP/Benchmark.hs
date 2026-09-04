{-# LANGUAGE BangPatterns #-}

-- | A benchmark suite spanning mathematical families.
--
-- == Why this exists
--
-- The shipped demo problem, @2x + sin(y)@, is recovered 10/10 by
-- generation 0-4 at the default population. That is not a measurement of
-- anything: with 600 individuals and a six-node target, ramped
-- half-and-half often produces the answer by chance, so the run exercises
-- the initialiser more than the search. Any claim that a change to the
-- engine /helped/ needs problems that can distinguish a better search from
-- a luckier one.
--
-- Nine families, each with a known closed-form truth, crossed with five
-- variants that model the ways real data is inconvenient. The families are
-- chosen so that different operators are load-bearing for each: a run that
-- improves on the trigonometric problem while regressing on the power law
-- has told you something a single benchmark cannot.
--
-- == What "recovered" means here
--
-- __Functional__ recovery, not syntactic. The discovered formula is scored
-- against a large, independently drawn sample of the /noiseless/ truth, and
-- counts as recovered when the normalised RMSE falls under
-- 'recoveryThreshold'.
--
-- Comparing expression trees for syntactic equality would be the wrong
-- measure and would mostly report noise: @x + x@, @2 * x@ and @x * 2@ are
-- the same function, and the engine has no reason to prefer one spelling.
-- Normalising by the target's standard deviation is what makes the number
-- comparable across families whose outputs differ by orders of magnitude —
-- @2 exp(0.7x)@ spans about 30 units where @2 sin(3x + 0.5)@ spans 4.
module TypedGP.Benchmark
  ( -- * Problems
    Problem (..)
  , Variant (..)
  , NoiseModel (..)
  , suite
  , variants
  , applyVariant
  , problemsNamed
  , variantName

    -- * Data generation
  , generateNoisy
  , generateClean

    -- * Scoring
  , ProblemResult (..)
  , ProblemSummary (..)
  , normalisedRmse
  , summarise
  , recoveryThreshold
  , exactThreshold
  , median
  ) where

import Data.List (nub, sortOn)
import Data.Word (Word64)

import TypedGP.Data.Dataset (Dataset, DataPoint (..), datasetPoints, mkDataset)
import TypedGP.Eval (Env, sanitize)
import TypedGP.Expr (Expr, VarName)
import TypedGP.Random (Seed, chance, nextGaussian, nextRange)

-- | How the targets are corrupted.
data NoiseModel
  = Noiseless
  | Gaussian !Double
    -- ^ Additive normal noise; the parameter is the standard deviation as
    -- a /fraction/ of the target's own spread, so one number means the
    -- same thing across families of wildly different scale.
  | Heteroscedastic !Double
    -- ^ Normal noise whose width grows with @|target|@. Breaks the
    -- constant-variance assumption that a plain squared-error metric
    -- silently makes.
  | Outliers !Double !Double
    -- ^ Contamination probability, and outlier size in units of the
    -- target's spread. Models transcription errors and sensor faults —
    -- rare, large, and devastating to a squared-error fit.
  deriving (Eq, Show)

-- | One benchmark problem.
data Problem = Problem
  { probName :: !String
  , probFamily :: !String
  , probFormula :: !String
    -- ^ The ground truth, for reporting.
  , probVariables :: ![VarName]
    -- ^ Every column presented to the search, including irrelevant ones.
  , probRelevant :: ![VarName]
    -- ^ The columns the truth actually depends on. Anything in
    -- 'probVariables' but not here is a distractor, and a formula that
    -- references one has made a false discovery.
  , probTruth :: Env -> Double
  , probDomain :: !(Double, Double)
    -- ^ Range each input is drawn from for training and in-domain test.
  , probExtrapolation :: !(Double, Double)
    -- ^ A wider range, used only for the extrapolation score. Never seen
    -- during fitting.
  , probNoise :: !NoiseModel
  }

-- | Nuisance factors, applied on top of any problem.
data Variant
  = Clean
  | Noisy
  | Irrelevant
  | HeteroscedasticNoise
  | WithOutliers
  deriving (Eq, Show)

variants :: [Variant]
variants = [Clean, Noisy, Irrelevant, HeteroscedasticNoise, WithOutliers]

variantName :: Variant -> String
variantName v = case v of
  Clean                -> "clean"
  Noisy                -> "noisy"
  Irrelevant           -> "irrelevant"
  HeteroscedasticNoise -> "hetero"
  WithOutliers         -> "outliers"

-- | Look up a variable, defaulting to zero. Every truth function below
-- uses this rather than a partial lookup.
at :: VarName -> Env -> Double
at name env = maybe 0.0 id (lookup name env)

-- | The nine families.
--
-- Domains are chosen so each problem is well posed: the rational function
-- stays clear of its pole at @x = -3@, and the logarithm stays on positive
-- inputs. That is deliberate — the point is to measure whether the search
-- can find a known relationship, not whether it survives a badly specified
-- one, which is what the protected operators are separately tested for.
suite :: [Problem]
suite =
  [ Problem
      { probName = "polynomial"
      , probFamily = "polynomial"
      , probFormula = "3x^2 - 2x + 7"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> let x = at "x" env in 3.0 * x * x - 2.0 * x + 7.0
      , probDomain = (-3.0, 3.0)
      , probExtrapolation = (-6.0, 6.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "rational"
      , probFamily = "rational"
      , probFormula = "(2x + 1) / (x + 3)"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> let x = at "x" env in (2.0 * x + 1.0) / (x + 3.0)
      , probDomain = (-1.0, 5.0)
      , probExtrapolation = (5.0, 15.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "power-law"
      , probFamily = "power"
      , probFormula = "4 x^1.7"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> 4.0 * (at "x" env ** 1.7)
      , probDomain = (0.5, 4.0)
      , probExtrapolation = (4.0, 8.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "exponential"
      , probFamily = "exponential"
      , probFormula = "2 exp(0.7x)"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> 2.0 * exp (0.7 * at "x" env)
      , probDomain = (-1.0, 3.0)
      , probExtrapolation = (3.0, 6.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "logarithmic"
      , probFamily = "logarithmic"
      , probFormula = "3 log(x) + 2"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> 3.0 * log (at "x" env) + 2.0
      , probDomain = (0.5, 6.0)
      , probExtrapolation = (6.0, 20.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "trigonometric"
      , probFamily = "trigonometric"
      , probFormula = "2 sin(3x + 0.5)"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> 2.0 * sin (3.0 * at "x" env + 0.5)
      , probDomain = (-3.0, 3.0)
      , probExtrapolation = (3.0, 6.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "mixed"
      , probFamily = "mixed"
      , probFormula = "2x^2 + exp(-x) + sin(y)"
      , probVariables = ["x", "y"]
      , probRelevant = ["x", "y"]
      , probTruth = \env ->
          let x = at "x" env
              y = at "y" env
          in 2.0 * x * x + exp (negate x) + sin y
      , probDomain = (-2.0, 2.0)
      , probExtrapolation = (-4.0, 4.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "nested"
      , probFamily = "nested"
      , probFormula = "exp(sin(x^2))"
      , probVariables = ["x"]
      , probRelevant = ["x"]
      , probTruth = \env -> let x = at "x" env in exp (sin (x * x))
      , probDomain = (-2.0, 2.0)
      , probExtrapolation = (2.0, 4.0)
      , probNoise = Noiseless
      }
  , Problem
      { probName = "interaction"
      , probFamily = "interaction"
      , probFormula = "x1 x2 + sin(x3)"
      , probVariables = ["x1", "x2", "x3"]
      , probRelevant = ["x1", "x2", "x3"]
      , probTruth = \env -> at "x1" env * at "x2" env + sin (at "x3" env)
      , probDomain = (-2.0, 2.0)
      , probExtrapolation = (-4.0, 4.0)
      , probNoise = Noiseless
      }
  ]

problemsNamed :: [String] -> [Problem]
problemsNamed [] = suite
problemsNamed wanted =
  [p | p <- suite, probName p `elem` wanted || probFamily p `elem` wanted]

-- | Apply a nuisance factor.
--
-- Noise levels are expressed as fractions of the target's own spread, so
-- @Noisy@ means the same difficulty on every family rather than being
-- trivial for the exponential and overwhelming for the sine.
applyVariant :: Variant -> Problem -> Problem
applyVariant variant problem = case variant of
  Clean -> problem
  Noisy -> problem { probNoise = Gaussian 0.05 }
  HeteroscedasticNoise -> problem { probNoise = Heteroscedastic 0.10 }
  WithOutliers -> problem { probNoise = Outliers 0.05 6.0 }
  Irrelevant -> problem
    { probVariables = probVariables problem ++ distractors
    -- probRelevant is left alone: that is what makes the extra columns
    -- detectable as false discoveries.
    }
  where
    distractors :: [VarName]
    distractors = ["noise1", "noise2"]

-- | Generate a training set, with the problem's noise model applied.
--
-- Returns 'Left' only if the row shape disagrees with the variable list,
-- which cannot happen for a problem built here — the rows are constructed
-- from 'probVariables' — but is propagated rather than swallowed so a
-- future problem with a mismatched definition fails loudly.
generateNoisy :: Problem -> Int -> Seed -> (Either String Dataset, Seed)
generateNoisy problem count seed0 =
  let (rows, seed1) = drawRows problem (probDomain problem) count seed0
      spread = spreadOf (map snd rows)
      (noisy, seed2) = applyNoise (probNoise problem) spread rows seed1
  in (mkDataset (probVariables problem) noisy, seed2)

-- | Generate a noiseless sample over a chosen range: the in-domain test
-- set and the extrapolation set both come from here.
generateClean :: Problem -> (Double, Double) -> Int -> Seed -> (Either String Dataset, Seed)
generateClean problem range count seed0 =
  let (rows, seed1) = drawRows problem range count seed0
  in (mkDataset (probVariables problem) rows, seed1)

-- | Draw inputs uniformly and evaluate the truth on them.
drawRows
  :: Problem
  -> (Double, Double)
  -> Int
  -> Seed
  -> ([([Double], Double)], Seed)
drawRows problem (low, high) count seed0 = go (max 0 count) seed0 []
  where
    go :: Int -> Seed -> [([Double], Double)] -> ([([Double], Double)], Seed)
    go 0 st acc = (acc, st)
    go k st acc =
      let (values, st1) = drawInputs (probVariables problem) st
          env = zip (probVariables problem) values
          target = sanitize (probTruth problem env)
      in go (k - 1) st1 ((values, target) : acc)

    drawInputs :: [VarName] -> Seed -> ([Double], Seed)
    drawInputs [] st = ([], st)
    drawInputs (_ : rest) st =
      let (value, st1) = nextRange low high st
          (others, st2) = drawInputs rest st1
      in (value : others, st2)

-- | Sample standard deviation of the targets, used to scale every noise
-- model. Falls back to 1 for a degenerate (constant) target so that noise
-- levels stay meaningful rather than collapsing to zero.
spreadOf :: [Double] -> Double
spreadOf values
  | count < 2 = 1.0
  | deviation <= 0 = 1.0
  | otherwise = deviation
  where
    count :: Int
    count = length values

    mean :: Double
    mean = sum values / fromIntegral count

    deviation :: Double
    deviation =
      sqrt (sum [(v - mean) * (v - mean) | v <- values] / fromIntegral (count - 1))

applyNoise
  :: NoiseModel
  -> Double
  -> [([Double], Double)]
  -> Seed
  -> ([([Double], Double)], Seed)
applyNoise model spread rows seed0 = case model of
  Noiseless -> (rows, seed0)
  Gaussian fraction -> perturb (\_ deviate -> deviate * fraction * spread)
  -- Width grows with the magnitude of the target, so the low end of the
  -- range is measured precisely and the high end is not.
  Heteroscedastic fraction ->
    perturb (\target deviate -> deviate * fraction * (abs target + spread))
  Outliers probability magnitude -> contaminate probability magnitude
  where
    perturb :: (Double -> Double -> Double) -> ([([Double], Double)], Seed)
    perturb scale = go rows seed0 []
      where
        go [] st acc = (reverse acc, st)
        go ((inputs, target) : rest) st acc =
          let (deviate, st1) = nextGaussian st
              shifted = sanitize (target + scale target deviate)
          in go rest st1 ((inputs, shifted) : acc)

    contaminate :: Double -> Double -> ([([Double], Double)], Seed)
    contaminate probability magnitude = go rows seed0 []
      where
        go [] st acc = (reverse acc, st)
        go ((inputs, target) : rest) st acc =
          let (hit, st1) = chance probability st
              (deviate, st2) = nextGaussian st1
              shifted
                | hit = sanitize (target + deviate * magnitude * spread)
                | otherwise = target
          in go rest st2 ((inputs, shifted) : acc)

-- Scoring --------------------------------------------------------------------

-- | RMSE divided by the spread of the true targets.
--
-- Scale-free, so one threshold means the same thing on every family. A
-- value of 1.0 is what predicting the mean would score, so anything at or
-- above 1 has learned nothing at all.
normalisedRmse :: (Env -> Double) -> Dataset -> Double
normalisedRmse predict ds = case datasetPoints ds of
  [] -> badScore
  points ->
    let residuals = [predict (dpInputs p) - dpTarget p | p <- points]
        meanSquare = sum [r * r | r <- residuals] / fromIntegral (length points)
        spread = spreadOf (map dpTarget points)
    in sanitize (sqrt meanSquare / spread)

-- | Score assigned when nothing can be measured.
badScore :: Double
badScore = 1.0e6

-- | Normalised RMSE below which a formula counts as having recovered the
-- relationship.
--
-- 0.05 means the residual is 5% of the target's own spread, which is
-- roughly "explains 99.75% of the variance". Loose enough that a genuinely
-- correct formula is not failed for floating-point dust or for a
-- differently-spelled equivalent.
--
-- __The claim this comment used to make — that it is \"strict enough that
-- an approximation of the right general shape does not pass\" — is false,
-- and was measured to be false on 2026-08-20.__ On @nested@
-- (@exp(sin(x^2))@, 5 nodes) four of tournament selection's fourteen
-- \"recoveries\" were sprawling approximations rather than the law. One,
-- at normalised RMSE 0.0323:
--
-- > sqrt(abs(abs(abs(abs(x / (0.7765 / x)) ^ 0.8178) ^ 2.3845 ^ sqrt(x))
-- >   ^ cos(x)) ^ (abs(x) + 0.0966 + abs(x) ^ 0.192)) + sin(1.6396 ^ abs(x))
--
-- So this threshold measures __\"fits well\"__, not __\"found the law\"__,
-- and the two come apart exactly where it matters. See 'exactThreshold'.
recoveryThreshold :: Double
recoveryThreshold = 0.05

-- | Normalised RMSE below which a formula is the target relationship
-- rather than an approximation of it.
--
-- Deliberately near machine noise. A formula that is algebraically the
-- truth evaluates to it bit-for-bit up to floating-point reassociation, so
-- the gap between "exact" and "very good approximation" is not a continuum
-- with a judgement call in the middle — it is several orders of magnitude,
-- and any threshold in that gap gives the same answer.
--
-- __Why this exists.__ Three separate mechanisms (lexicase elitism, the
-- stacked toggle combination, age-fitness Pareto) each showed markedly
-- better median error and smaller formulas at __identical recovery rate__.
-- All three turned out to be the same effect seen through a threshold that
-- cannot resolve it: they convert approximations into exact laws, which is
-- invisible to a pass\/fail count where both already pass.
--
-- On @nested@ that is recovery 70% vs 70% and exact 50% vs 70%.
exactThreshold :: Double
exactThreshold = 1.0e-6

-- | The outcome of one problem/variant/seed combination.
data ProblemResult = ProblemResult
  { prProblem :: !String
  , prVariant :: !String
  , prSeed :: !Word64
  , prRecovered :: !Bool
  , prExact :: !Bool
  , prTestNRmse :: !Double
    -- ^ Against the noiseless truth, in domain. The headline number.
  , prTrainError :: !Double
  , prHoldoutError :: !(Maybe Double)
  , prExtrapNRmse :: !Double
    -- ^ Against the noiseless truth, outside the training range.
  , prComplexity :: !Int
  , prGenerations :: !Int
  , prSeconds :: !Double
  , prOperators :: ![String]
  , prFalseDiscovery :: !Bool
    -- ^ Did the formula reference a variable the truth does not use?
  , prFormula :: !String
  , prExpr :: !Expr
  } deriving (Eq, Show)

-- | Aggregate over seeds.
data ProblemSummary = ProblemSummary
  { psProblem :: !String
  , psVariant :: !String
  , psRuns :: !Int
  , psRecoveryRate :: !Double
  , psExactRate :: !Double
  , psMedianTestNRmse :: !Double
  , psMedianExtrapNRmse :: !Double
  , psMedianComplexity :: !Double
  , psMedianSeconds :: !Double
  , psFalseDiscoveryRate :: !Double
  , psOperatorsUsed :: ![String]
  } deriving (Eq, Show)

summarise :: String -> String -> [ProblemResult] -> ProblemSummary
summarise problem variant results = ProblemSummary
  { psProblem = problem
  , psVariant = variant
  , psRuns = total
  , psRecoveryRate = rateOf prRecovered
  , psExactRate = rateOf prExact
  , psMedianTestNRmse = median (map prTestNRmse results)
  , psMedianExtrapNRmse = median (map prExtrapNRmse results)
  , psMedianComplexity = median (map (fromIntegral . prComplexity) results)
  , psMedianSeconds = median (map prSeconds results)
  , psFalseDiscoveryRate = rateOf prFalseDiscovery
  , psOperatorsUsed = nub (concatMap prOperators results)
  }
  where
    total :: Int
    total = length results

    rateOf :: (ProblemResult -> Bool) -> Double
    rateOf predicate
      | total == 0 = 0.0
      | otherwise =
          fromIntegral (length (filter predicate results)) / fromIntegral total

-- | Median of a sample. Zero for an empty one.
--
-- Median rather than mean throughout: symbolic regression results are
-- heavy-tailed — one seed that fails completely can score a thousand times
-- worse than the rest — and a mean would report that outlier instead of
-- the typical run.
median :: [Double] -> Double
median [] = 0.0
median xs =
  let sorted = sortOn id xs
      count = length sorted
      half = count `div` 2
  in if even count
       then 0.5 * (elemAt (half - 1) sorted + elemAt half sorted)
       else elemAt half sorted
  where
    elemAt :: Int -> [Double] -> Double
    elemAt i values = case drop i values of
      (v : _) -> v
      -- Unreachable: every index used above is inside the list.
      []      -> 0.0
