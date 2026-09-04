-- | Tests for bootstrap confidence intervals.
--
-- The assertion that matters is that __interval width responds to data
-- quantity__. A bootstrap that reported the same interval for 20 examples
-- as for 400 would be worse than useless: it would look like a calibrated
-- uncertainty estimate while being a constant. So the central test fits
-- the same formula to a small and a large sample of the same noisy process
-- and requires the small sample to produce visibly wider intervals.
--
-- Noise is essential to these tests. On noiseless data every resample
-- admits the same perfect fit, every bootstrap estimate is identical, and
-- every interval collapses to zero width — which would make the test pass
-- for entirely the wrong reason.
module UncertaintySpec (tests) where

import Data.List (isInfixOf)

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Data.Dataset (Dataset, datasetSize, resampleDataset, sampleDataset)
import TypedGP.Expr (Expr (..))
import TypedGP.Fitness (errorOf)
import TypedGP.LocalSearch (constantsOf, optimiseConstants, withConstants)
import TypedGP.Random (mkSeed)
import TypedGP.Uncertainty
  ( BootstrapResult (..)
  , ConstantEstimate (..)
  , annotateFormula
  , bootstrapConstants
  )

-- | The true relationship the tests fit against: @y = 2x@, plus noise.
trueSlope :: Double
trueSlope = 2.0

-- | Deterministic pseudo-noise derived from the input.
--
-- Reproducible run to run, and uncorrelated with @x@ in any way the fit
-- can exploit, which is all these tests need from it.
noiseAt :: Double -> Double
noiseAt x =
  let mixed = sin (x * 12.9898) * 43758.5453
  in (mixed - fromIntegral (floor mixed :: Int)) - 0.5

noisyLinear :: Int -> Dataset
noisyLinear count = fst (sampleDataset ["x"] (-3.0, 3.0) count target (mkSeed 31))
  where
    target :: [(String, Double)] -> Double
    target env =
      let x = maybe 0.0 id (lookup "x" env)
      in trueSlope * x + noiseAt x

baseConfig :: Config
baseConfig = defaultConfig { cfgVariables = ["x"] }

-- | A clean power law, @y = 4 * x^1.7@, sampled over positive @x@ so it
-- stays real.
powerLawData :: Dataset
powerLawData = fst (sampleDataset ["x"] (0.5, 4.0) 50 target (mkSeed 77))
  where
    target :: [(String, Double)] -> Double
    target env = 4.0 * (maybe 0.0 id (lookup "x" env) ** 1.7)

-- | The right structure with deliberately wrong constants.
powerLawModel :: Expr
powerLawModel = Mul (Const 1.0) (Pow (Var "x") (Const 1.0))

bootstrapConfig :: Int -> Config
bootstrapConfig samples = baseConfig { cfgBootstrapSamples = samples }

-- | @c * x@, with the constant deliberately started away from the truth so
-- that refitting has something to do.
model :: Expr
model = Mul (Const 1.0) (Var "x")

smallResult :: BootstrapResult
smallResult =
  bootstrapConstants (bootstrapConfig 40) (noisyLinear 20) model (mkSeed 1)

largeResult :: BootstrapResult
largeResult =
  bootstrapConstants (bootstrapConfig 40) (noisyLinear 400) model (mkSeed 1)

firstEstimate :: BootstrapResult -> Maybe ConstantEstimate
firstEstimate result = case brEstimates result of
  (e : _) -> Just e
  []      -> Nothing

stdErrorOf :: BootstrapResult -> Double
stdErrorOf = maybe 0.0 ceStdError . firstEstimate

intervalWidthOf :: BootstrapResult -> Double
intervalWidthOf = maybe 0.0 (\e -> ceUpper e - ceLower e) . firstEstimate

pointOf :: BootstrapResult -> Double
pointOf = maybe 0.0 cePoint . firstEstimate

tests :: [(String, Bool)]
tests =
  -- Constant plumbing -----------------------------------------------------
  [ ("constantsOf finds constants in pre-order",
      constantsOf (Add (Const 1.0) (Mul (Const 2.0) (Var "x"))) == [1.0, 2.0])
  , ("constantsOf ignores variables", constantsOf (Var "x") == [])
  , ("withConstants replaces in pre-order",
      withConstants [7.0, 8.0] (Add (Const 1.0) (Mul (Const 2.0) (Var "x")))
        == Add (Const 7.0) (Mul (Const 8.0) (Var "x")))
  , ("withConstants round-trips through constantsOf",
      let e = Add (Const 1.5) (Sin (Const (-2.5)))
      in withConstants (constantsOf e) e == e)
  , ("withConstants tolerates too few values",
      withConstants [7.0] (Add (Const 1.0) (Const 2.0))
        == Add (Const 7.0) (Const 2.0))
  , ("withConstants tolerates too many values",
      withConstants [7.0, 8.0, 9.0] (Const 1.0) == Const 7.0)
  , ("withConstants leaves a variable-only expression alone",
      withConstants [7.0] (Var "x") == Var "x")

  -- The coupled-constant case, now solved -----------------------------------
    -- This block previously asserted the *opposite*: that the compass
    -- search could not untangle two coupled constants, with a note saying
    -- the assertion would have to be replaced when a real optimiser
    -- landed. It has (see "TypedGP.LocalSearch"), so this is that
    -- replacement.
    --
    -- The failure it documented was real: in @c1 * x^c2@ raising the
    -- exponent rescales the output, so a step in either constant alone
    -- looks worse unless the other moves with it, and a coordinate method
    -- stalls in the curved valley. The simplex moves the whole vector at
    -- once and reaches (4, 1.7).
  , ("the unfitted power law starts far from the data",
      errorOf (cfgErrorMetric baseConfig) powerLawData powerLawModel > 1.0)
  , ("the optimiser untangles two coupled constants",
      case constantsOf (optimiseConstants baseConfig powerLawData powerLawModel) of
        (coefficient : exponent' : _) ->
          abs (coefficient - 4.0) < 0.05 && abs (exponent' - 1.7) < 0.05
        _ -> False)
  , ("the refitted coupled case is near-exact",
      errorOf (cfgErrorMetric baseConfig) powerLawData
        (optimiseConstants baseConfig powerLawData powerLawModel) < 0.01)

  -- Refitting -------------------------------------------------------------
  , ("refitting improves the error",
      let ds = noisyLinear 200
          before = errorOf (cfgErrorMetric baseConfig) ds model
          after = errorOf (cfgErrorMetric baseConfig) ds (optimiseConstants baseConfig ds model)
      in after < before)
  , ("refitting recovers the true slope closely",
      let ds = noisyLinear 400
          fitted = constantsOf (optimiseConstants baseConfig ds model)
      in case fitted of
           (slope : _) -> abs (slope - trueSlope) < 0.1
           []          -> False)
  , ("refitting leaves a constant-free expression alone",
      optimiseConstants baseConfig (noisyLinear 20) (Var "x") == Var "x")

  -- Resampling ------------------------------------------------------------
  , ("a resample has the same size as the original",
      datasetSize (fst (resampleDataset (mkSeed 3) (noisyLinear 50))) == 50)
  , ("a resample differs from the original",
      fst (resampleDataset (mkSeed 3) (noisyLinear 50)) /= noisyLinear 50)

  -- The headline property -------------------------------------------------
  , ("a bootstrap produces one estimate per constant",
      length (brEstimates smallResult) == 1)
  , ("the bootstrap records how many resamples it ran",
      brSamples smallResult == 40)
  , ("both sample sizes recover roughly the right slope",
      abs (pointOf smallResult - trueSlope) < 0.3
        && abs (pointOf largeResult - trueSlope) < 0.3)
  , ("intervals are not degenerate", stdErrorOf smallResult > 0.0)
  , ("more data gives a tighter standard error",
      stdErrorOf largeResult < stdErrorOf smallResult)
  , ("more data gives a narrower percentile interval",
      intervalWidthOf largeResult < intervalWidthOf smallResult)
  , ("the tightening is substantial, not marginal",
      stdErrorOf largeResult * 2.0 < stdErrorOf smallResult)
  , ("the percentile interval brackets the point estimate",
      maybe False (\e -> ceLower e <= cePoint e && cePoint e <= ceUpper e)
        (firstEstimate largeResult))

  -- Degenerate inputs -----------------------------------------------------
  , ("zero resamples produce no estimates",
      null (brEstimates
              (bootstrapConstants (bootstrapConfig 0) (noisyLinear 50) model (mkSeed 1))))
  , ("an expression with no constants produces no estimates",
      null (brEstimates
              (bootstrapConstants (bootstrapConfig 20) (noisyLinear 50) (Var "x") (mkSeed 1))))

  -- Presentation ----------------------------------------------------------
  , ("the annotated formula carries a plus-or-minus",
      "±" `isInfixOf` annotateFormula smallResult)
  , ("the annotated formula still shows the structure",
      "x" `isInfixOf` annotateFormula smallResult)
  , ("an un-bootstrapped formula annotates without intervals",
      not ("±" `isInfixOf` annotateFormula
             (bootstrapConstants (bootstrapConfig 0) (noisyLinear 50) model (mkSeed 1))))
  ]
