-- | Bootstrap confidence intervals for a discovered formula's constants.
--
-- Evolution returns a point estimate: @0.2984 * mom - vol / cos(vol)@. That
-- tells you nothing about how much the data actually pinned down @0.2984@.
-- Fitted to 20 examples it may be indistinguishable from 0.1 or 0.5;
-- fitted to 2000 it may be known to within a percent. Those are different
-- claims, and a report that renders them identically is misleading in the
-- cases where it matters most — anywhere a coefficient feeds a real
-- decision.
--
-- The method is the ordinary nonparametric bootstrap:
--
--   1. Fit the constants on the full dataset to get a point estimate.
--   2. Resample the dataset with replacement, and refit /only the
--      constants/ — the structure discovered by evolution is held fixed.
--   3. Repeat. The spread of the refitted values across resamples is an
--      empirical sampling distribution for each constant.
--
-- Holding the structure fixed is what makes this affordable, and it is
-- also what it means: these intervals answer "how well does the data
-- constrain this coefficient, given this formula shape", not "how sure are
-- we of the shape". Structural uncertainty is a much harder question and
-- is not what this module claims to measure. Run the search across several
-- seeds and compare the shapes it finds if you want evidence about that.
--
-- Refitting delegates to "TypedGP.LocalSearch" rather than re-running
-- evolution: the search space here is a handful of continuous parameters
-- with the topology already decided, which is a different and far easier
-- problem than the one the genetic algorithm solves.
--
-- The quality of that refit matters more here than anywhere else in the
-- package. An interval is only as good as the optimiser that produced the
-- values it is a spread over — an optimiser that stalls in the same place
-- on every resample reports a confidently narrow interval around the wrong
-- number.
module TypedGP.Uncertainty
  ( -- * Estimates
    ConstantEstimate (..)
  , BootstrapResult (..)

    -- * Bootstrap
  , bootstrapConstants

    -- * Presentation
  , annotateFormula
  ) where

import Data.List (sortOn)

import TypedGP.Config (Config (..))
import TypedGP.Data.Dataset (Dataset, datasetSize, resampleDataset)
import TypedGP.Expr (Expr)
import TypedGP.LocalSearch (constantsOf, optimiseConstants)
import TypedGP.Pretty (prettyWithConstants, showConstant)
import TypedGP.Random (Seed)

-- | What the bootstrap concluded about one constant.
data ConstantEstimate = ConstantEstimate
  { ceOrdinal :: !Int
    -- ^ Which constant this is, counting from 0 in pre-order — the same
    -- numbering "TypedGP.Pretty" uses, so the two can be matched up.
  , cePoint :: !Double
    -- ^ Value fitted on the full dataset.
  , ceMean :: !Double
    -- ^ Mean across bootstrap resamples. A large gap between this and
    -- 'cePoint' indicates a skewed sampling distribution, in which case
    -- the percentile interval is more trustworthy than the standard error.
  , ceStdError :: !Double
    -- ^ Standard deviation of the bootstrap estimates.
  , ceLower :: !Double
    -- ^ 2.5th percentile of the bootstrap estimates.
  , ceUpper :: !Double
    -- ^ 97.5th percentile.
  } deriving (Eq, Show)

-- | The outcome of a bootstrap analysis.
data BootstrapResult = BootstrapResult
  { brExpr :: !Expr
    -- ^ The formula with its constants at their full-data point estimates.
  , brEstimates :: ![ConstantEstimate]
  , brSamples :: !Int
    -- ^ Resamples actually performed.
  , brSeed :: !Seed
  } deriving (Eq, Show)

-- | Bootstrap the constants of a formula.
--
-- Returns an empty estimate list when the formula has no constants, or
-- when 'cfgBootstrapSamples' is zero — both are ordinary situations, not
-- errors.
bootstrapConstants :: Config -> Dataset -> Expr -> Seed -> BootstrapResult
bootstrapConstants cfg ds expr s0
  | samples <= 0 || null (constantsOf expr) || datasetSize ds < 2 =
      BootstrapResult
        { brExpr = fitted
        , brEstimates = []
        , brSamples = 0
        , brSeed = s0
        }
  | otherwise = BootstrapResult
      { brExpr = fitted
      , brEstimates = zipWith summarise [0 ..] byConstant
      , brSamples = samples
      , brSeed = s1
      }
  where
    samples :: Int
    samples = cfgBootstrapSamples cfg

    -- The point estimate is itself a refit: evolution optimised these
    -- constants under a parsimony penalty and against a possibly different
    -- split, so polishing them on the data being bootstrapped is what
    -- makes the intervals centre on something meaningful.
    fitted :: Expr
    fitted = optimiseConstants cfg ds expr

    pointValues :: [Double]
    pointValues = constantsOf fitted

    (draws, s1) = collect samples s0 []

    collect :: Int -> Seed -> [[Double]] -> ([[Double]], Seed)
    collect 0 st acc = (acc, st)
    collect k st acc =
      let (resampled, st') = resampleDataset st ds
          values = constantsOf (optimiseConstants cfg resampled fitted)
      in collect (k - 1) st' (values : acc)

    -- One list of bootstrap values per constant.
    byConstant :: [[Double]]
    byConstant =
      [ [ valueAt i row | row <- draws ] | i <- [0 .. length pointValues - 1] ]

    valueAt :: Int -> [Double] -> Double
    valueAt i row = case drop i row of
      (v : _) -> v
      -- Unreachable: every refit preserves the constant count.
      []      -> 0.0

    summarise :: Int -> [Double] -> ConstantEstimate
    summarise ordinal values = ConstantEstimate
      { ceOrdinal = ordinal
      , cePoint = pointAt ordinal
      , ceMean = mean values
      , ceStdError = standardDeviation values
      , ceLower = percentile 0.025 values
      , ceUpper = percentile 0.975 values
      }

    pointAt :: Int -> Double
    pointAt i = valueAt i pointValues

mean :: [Double] -> Double
mean [] = 0.0
mean xs = sum xs / fromIntegral (length xs)

-- | Sample standard deviation, with the @n - 1@ denominator.
--
-- The bootstrap draws are a sample from the resampling distribution, not
-- the whole of it, so the unbiased denominator is the right one.
standardDeviation :: [Double] -> Double
standardDeviation xs
  | count < 2 = 0.0
  | otherwise =
      let m = mean xs
          squares = sum [(x - m) * (x - m) | x <- xs]
      in sqrt (squares / fromIntegral (count - 1))
  where
    count :: Int
    count = length xs

-- | Nearest-rank percentile of an unsorted sample. Total for any input.
percentile :: Double -> [Double] -> Double
percentile p xs = case sortOn id xs of
  [] -> 0.0
  sorted ->
    let count = length sorted
        rank = max 0 (min (count - 1) (floor (p * fromIntegral count)))
    in case drop rank sorted of
         (v : _) -> v
         -- Unreachable: rank is clamped below count.
         []      -> 0.0

-- | Render the formula with each constant replaced by @value ± stderr@.
--
-- Uses "TypedGP.Pretty"'s constant-renderer hook, so the bracketing and
-- precedence rules are the ones the ordinary printer already gets right
-- rather than a second, subtly different implementation.
annotateFormula :: BootstrapResult -> String
annotateFormula result = prettyWithConstants renderer (brExpr result)
  where
    table :: [(Int, ConstantEstimate)]
    table = [(ceOrdinal e, e) | e <- brEstimates result]

    renderer :: Int -> Double -> (String, Bool)
    renderer ordinal value = case lookup ordinal table of
      Just estimate ->
        ( showConstant (cePoint estimate) ++ " ± " ++ showConstant (ceStdError estimate)
        , True
        )
      -- No bootstrap ran for this constant; print it plainly.
      Nothing -> (showConstant value, value < 0)
