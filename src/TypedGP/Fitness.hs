{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | Scoring an expression against a dataset.
--
-- __Lower is better__ everywhere in this package. Selection, elitism and
-- the early-stopping check all assume it, so an accuracy-style score would
-- have to be negated here rather than special-cased downstream.
--
-- Fitness is @error + parsimony * size + domainPenalty * invalidFraction@.
-- Splitting those contributions into separate fields of 'Fitness' rather
-- than returning a bare 'Double' means the CLI can report "this formula has
-- RMSE 0.03 and is being charged 0.04 for its size" — which is the
-- difference between a tuning signal and a mystery number.
--
-- The domain term is weighted zero by default, so unless it is switched on
-- fitness is the same two-part sum it has always been.
module TypedGP.Fitness
  ( Fitness (..)
  , evaluateFitness
  , errorOf
  , caseErrors
  , caseLoss
  , huberThresholdFor
  , predict
  , badFitness
  ) where

-- base 4.20 (GHC 9.10) moved foldl' into the Prelude. Before that it must
-- be imported; from that release on, importing it is a redundant import,
-- which -Werror rejects. The conditional satisfies both, which is what
-- lets the package build on GHC 9.4 through 9.10+ with no warnings on
-- either side of the split.
--
-- CPP is a language extension, not a package dependency, so this does not
-- touch the base-only constraint.
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

import TypedGP.Config (Config (..), ErrorMetric (..))
import TypedGP.Data.Dataset (DataPoint (..), Dataset, datasetPoints)
import TypedGP.Eval (eval, evalDomain, isValid, magnitudeCap, sanitize)
import TypedGP.Expr (Expr, countNodes)

-- | A scored individual's fitness, decomposed.
data Fitness = Fitness
  { fitError :: !Double
    -- ^ Raw prediction error under 'cfgErrorMetric'.
  , fitPenalty :: !Double
    -- ^ Size penalty: @cfgParsimony * countNodes@.
  , fitDomainPenalty :: !Double
    -- ^ Domain penalty: @cfgDomainPenalty * fitInvalidFraction@.
  , fitInvalidFraction :: !Double
    -- ^ Share of training points on which the expression left its
    -- mathematical domain — divided by zero, took a log of a negative,
    -- saturated, and so on.
    --
    -- __Reads @0@ when 'cfgDomainPenalty' is @0@__, which is the default,
    -- because the traversal that would compute it is skipped entirely
    -- there. That is a deliberate cost decision rather than an oversight
    -- (see 'invalidFractionOf'), but it does mean this is not a free
    -- diagnostic: reading it requires paying for it.
  , fitTotal :: !Double
    -- ^ @fitError + fitPenalty + fitDomainPenalty@. The value selection
    -- actually compares.
  } deriving (Eq, Show)

-- | The score given to an individual that cannot be scored at all.
--
-- Pinned to 'magnitudeCap' rather than to @1/0@ so that it stays finite
-- and comparable: an infinity here would make @mean fitness@ statistics
-- infinite for the whole generation.
badFitness :: Double
badFitness = magnitudeCap

-- | Score an expression. Total, and guaranteed to return finite fields —
-- 'TypedGP.Eval.eval' cannot produce @NaN@, and the aggregation below is
-- re-sanitised on the way out.
evaluateFitness :: Config -> Dataset -> Expr -> Fitness
evaluateFitness cfg ds expr =
  let err = errorOf (cfgErrorMetric cfg) ds expr
      penalty = sanitize (cfgParsimony cfg * fromIntegral (countNodes expr))
      invalid = invalidFractionOf cfg ds expr
      domainPenalty = sanitize (cfgDomainPenalty cfg * invalid)
      total = sanitize (err + penalty + domainPenalty)
  in Fitness
       { fitError = err
       , fitPenalty = penalty
       , fitDomainPenalty = domainPenalty
       , fitInvalidFraction = invalid
       , fitTotal = total
       }

-- | Share of training points on which @expr@ left its mathematical domain.
--
-- __Short-circuits to @0@ at zero weight, and that is load-bearing.__ The
-- domain traversal is a second pass over every point of every individual of
-- every generation; running it to multiply the answer by zero would put a
-- permanent cost on the engine's hot loop in exchange for nothing. Skipping
-- it is also what keeps every Phase 1 and Phase 2 benchmark number
-- reproducible bit-for-bit rather than merely close, since the default
-- weight is zero and the default path is therefore untouched.
invalidFractionOf :: Config -> Dataset -> Expr -> Double
invalidFractionOf cfg ds expr
  | cfgDomainPenalty cfg <= 0.0 = 0.0
  | otherwise = case datasetPoints ds of
      [] -> 0.0
      points ->
        let count :: Int -> DataPoint -> Int
            count !acc p =
              if isValid (evalDomain (dpInputs p) expr) then acc else acc + 1
            bad = foldl' count 0 points
        in fromIntegral bad / fromIntegral (length points)

-- | Aggregate prediction error over a dataset.
--
-- An empty dataset scores 'badFitness' rather than dividing by zero: with
-- no evidence, no expression can be preferred to another, and returning
-- the worst possible score keeps the run from silently "converging" on the
-- first random tree.
errorOf :: ErrorMetric -> Dataset -> Expr -> Double
errorOf metric ds expr = case datasetPoints ds of
  []     -> badFitness
  points ->
    let n = fromIntegral (length points) :: Double
        -- Strict accumulation: a lazy fold here builds one thunk per data
        -- point per individual per generation, which is the classic way
        -- this kind of loop eats all available memory.
        accumulate :: Double -> DataPoint -> Double
        accumulate !running point =
          running + contribution (eval (dpInputs point) expr - dpTarget point)
        summed = foldl' accumulate 0.0 points
    in sanitize (finish (summed / n))
 where
  contribution :: Double -> Double
  contribution = caseLoss metric huberThreshold

  -- Lazy, so datasets scored under any other metric never pay for it; when
  -- it is needed it costs one extra O(n) pass, shared across every
  -- residual in this call.
  huberThreshold :: Double
  huberThreshold = huberThresholdFor metric ds

  finish :: Double -> Double
  finish mean = case metric of
    RMSE -> sqrt (abs mean)  -- abs is belt-and-braces; a mean of squares is >= 0
    MSE  -> mean
    MAE  -> mean
    -- Reported as mean Huber loss, on its own scale rather than rescaled
    -- to look like an RMSE. Rescaling would make the number comparable at
    -- a glance and wrong in the tail, where the loss is deliberately no
    -- longer quadratic.
    Huber _ -> mean

-- | The loss a single residual contributes, under a metric and a
-- pre-resolved Huber threshold.
--
-- Factored out so that 'errorOf' and 'caseErrors' share the definition of
-- what a case costs. They differ only in how they traverse — 'errorOf'
-- folds strictly because it is the engine's hot path and must not allocate
-- an intermediate list per individual per generation, while 'caseErrors'
-- must produce that list because per-case errors are exactly what it is
-- for. Sharing the traversal instead of the loss would trade a correctness
-- risk for an allocation, which is the wrong way round.
caseLoss :: ErrorMetric -> Double -> Double -> Double
caseLoss metric threshold residual = case metric of
  RMSE -> residual * residual
  MSE  -> residual * residual
  MAE  -> abs residual
  -- Quadratic inside the threshold, linear outside it. The two pieces
  -- meet in both value and slope at |r| = threshold, which is what makes
  -- the loss smooth there and keeps the optimiser from seeing a kink.
  Huber _ ->
    let magnitude = abs residual
    in if magnitude <= threshold
         then 0.5 * residual * residual
         else threshold * (magnitude - 0.5 * threshold)

-- | Resolve the Huber threshold for a metric and dataset.
--
-- Returns the floor for every non-Huber metric, where it is unused. The
-- floor also keeps a degenerate constant-target dataset from collapsing
-- the threshold to zero, which would silently turn Huber into plain
-- absolute error.
huberThresholdFor :: ErrorMetric -> Dataset -> Double
huberThresholdFor metric ds = case metric of
  Huber multiple -> max huberFloor (multiple * targetSpread ds)
  _              -> huberFloor

-- | The error contributed by each individual training case, in dataset
-- order.
--
-- The per-case view that ε-lexicase selection needs. Aggregating this list
-- reproduces 'errorOf' up to the metric's final transformation, which is
-- the property that keeps the two definitions honest — 'FitnessSpec'
-- asserts it directly.
caseErrors :: ErrorMetric -> Dataset -> Expr -> [Double]
caseErrors metric ds expr =
  [ caseLoss metric threshold (eval (dpInputs point) expr - dpTarget point)
  | point <- datasetPoints ds
  ]
  where
    threshold :: Double
    threshold = huberThresholdFor metric ds

-- | Smallest Huber threshold that will ever be used.
--
-- Guards the degenerate case of a dataset whose targets are all identical,
-- where the spread is zero and an unfloored threshold would silently turn
-- Huber loss into absolute error.
huberFloor :: Double
huberFloor = 1.0e-12

-- | Sample standard deviation of a dataset's targets.
--
-- The scale that the Huber threshold is expressed in multiples of. Uses
-- the targets rather than a candidate's residuals precisely so that every
-- individual is scored against the same loss function — see the note on
-- 'TypedGP.Config.Huber'.
targetSpread :: Dataset -> Double
targetSpread ds = case map dpTarget (datasetPoints ds) of
  []      -> 0.0
  [_]     -> 0.0
  targets ->
    let count = fromIntegral (length targets) :: Double
        mean = sum targets / count
        squares = sum [(t - mean) * (t - mean) | t <- targets]
    in sqrt (squares / (count - 1.0))

-- | Predicted and actual value for every point, in dataset order.
--
-- Not used by the search; it exists so the CLI can show what a discovered
-- formula actually does, which is how you tell a real fit from a lucky
-- fitness number.
predict :: Dataset -> Expr -> [(Double, Double)]
predict ds expr =
  [ (eval (dpInputs point) expr, dpTarget point)
  | point <- datasetPoints ds
  ]
