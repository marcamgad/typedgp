-- | Tests for the error metrics, and for Huber loss in particular.
--
-- Every assertion measures 'errorOf' against a hand-computed value. The
-- fixture is arranged so that arithmetic can be done on paper:
--
--   * the dataset's targets have a sample standard deviation of __exactly
--     1__, so a Huber threshold expressed as a multiple of the spread is
--     also the raw threshold;
--   * @target == x@, so a model of @x + r@ has residual exactly @r@ at
--     /every/ point, and the mean loss equals the per-point loss.
--
-- Without the second trick a two-point fixture gives two different
-- residuals and the expected mean stops being something you can check by
-- inspection — which is how a test ends up quietly asserting a formula
-- against itself.
module FitnessSpec (tests) where

import TypedGP.Config (ErrorMetric (..), defaultHuberDelta)
import TypedGP.Data.Dataset (Dataset, mkDataset)
import TypedGP.Expr (Expr (..))
import TypedGP.Fitness (caseErrors, errorOf)

-- | Two points with mean 0 and sample variance @(0.5 + 0.5) / 1 = 1@.
unitSpreadInputs :: [Double]
unitSpreadInputs = [negate (1.0 / sqrt 2.0), 1.0 / sqrt 2.0]

-- | @target == x@ over those points.
unitSpreadData :: Maybe Dataset
unitSpreadData = case mkDataset ["x"] [([v], v) | v <- unitSpreadInputs] of
  Right ds -> Just ds
  Left _   -> Nothing

-- | Loss of the model @x + residual@, which misses every target by exactly
-- @residual@. A negative sentinel if the fixture failed to build, so a
-- broken fixture fails the assertions rather than passing vacuously.
lossWith :: ErrorMetric -> Double -> Double
lossWith metric residual = case unitSpreadData of
  Just ds -> errorOf metric ds (Add (Var "x") (Const residual))
  Nothing -> -1.0

huberAt :: Double -> Double -> Double
huberAt multiple = lossWith (Huber multiple)

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 1.0e-9

tests :: [(String, Bool)]
tests =
  -- The fixture ------------------------------------------------------------
  [ ("the fixture builds", unitSpreadData /= Nothing)
  , ("a perfect fit scores zero under every metric",
      all (\m -> approx (lossWith m 0.0) 0.0)
        [RMSE, MSE, MAE, Huber 1.0])

  -- The classic metrics ----------------------------------------------------
  , ("RMSE of a constant residual is that residual",
      approx (lossWith RMSE 3.0) 3.0)
  , ("MSE of a constant residual is its square",
      approx (lossWith MSE 3.0) 9.0)
  , ("MAE of a constant residual is its magnitude",
      approx (lossWith MAE (-3.0)) 3.0)

  -- Huber: shape -----------------------------------------------------------
    -- Threshold 1.0, residual 0.5, inside: 0.5 * 0.5^2 = 0.125.
  , ("Huber is quadratic inside the threshold",
      approx (huberAt 1.0 0.5) 0.125)
    -- Threshold 1.0, residual 3.0, outside: 1.0 * (3.0 - 0.5) = 2.5.
  , ("Huber is linear outside the threshold",
      approx (huberAt 1.0 3.0) 2.5)
    -- At the join both branches give 0.5 * 1^2 = 1 * (1 - 0.5) = 0.5.
  , ("the two branches agree at the threshold", approx (huberAt 1.0 1.0) 0.5)
  , ("Huber is continuous across the threshold",
      abs (huberAt 1.0 0.9999 - huberAt 1.0 1.0001) < 1.0e-3)
  , ("Huber is symmetric", approx (huberAt 1.0 (-2.5)) (huberAt 1.0 2.5))
  , ("Huber increases with the residual",
      and (zipWith (<) losses (drop 1 losses)))

  -- Huber: the robustness property -----------------------------------------
    -- This is the whole point. In the tail, doubling an already-large
    -- residual doubles the loss; squared error would quadruple it. That
    -- difference is what stops a handful of contaminated points from
    -- dictating the fit.
  , ("Huber grows linearly in the tail",
      abs ((huberAt 1.0 20.0 - huberAt 1.0 10.0) - 10.0) < 1.0e-6)
  , ("squared error grows quadratically over the same range",
      lossWith MSE 20.0 > 3.9 * lossWith MSE 10.0)
  , ("Huber charges far less than squared error for an outlier",
      huberAt 1.0 20.0 * 10.0 < lossWith MSE 20.0)
  , ("Huber and squared error agree on small residuals",
      approx (huberAt 1.0 0.1) (0.5 * lossWith MSE 0.1))

  -- Per-case errors ---------------------------------------------------------
    -- 'caseErrors' and 'errorOf' share a loss definition but not a
    -- traversal, so they could drift. These pin them together.
  , ("there is one case error per data point",
      length (casesWith MSE 2.0) == length unitSpreadInputs)
  , ("aggregating the case errors reproduces MSE",
      approx (meanOf (casesWith MSE 3.0)) (lossWith MSE 3.0))
  , ("aggregating the case errors reproduces MAE",
      approx (meanOf (casesWith MAE 3.0)) (lossWith MAE 3.0))
  , ("aggregating the case errors reproduces Huber",
      approx (meanOf (casesWith (Huber 1.0) 3.0)) (lossWith (Huber 1.0) 3.0))
    -- RMSE's final square root is applied to the aggregate, not per case,
    -- so the relationship is the squared one. Stated explicitly because
    -- assuming otherwise is the obvious way to misuse caseErrors.
  , ("RMSE relates to its case errors through the aggregate square root",
      approx (sqrt (meanOf (casesWith RMSE 3.0))) (lossWith RMSE 3.0))
  , ("case errors are never negative",
      all (>= 0.0) (concat [casesWith m r | m <- allMetrics, r <- [-4.0, 0.0, 2.5]]))
  , ("a perfect fit gives zero on every case",
      all (\m -> all (< 1.0e-12) (casesWith m 0.0)) allMetrics)

  -- Huber: the threshold ----------------------------------------------------
  , ("a smaller threshold clips sooner", huberAt 0.1 3.0 < huberAt 1.0 3.0)
  , ("a huge threshold degenerates to squared error",
      approx (huberAt 1.0e6 2.0) (0.5 * lossWith MSE 2.0))
    -- The threshold is a multiple of the target spread, which is 1 in this
    -- fixture; this pins that interpretation down rather than leaving it
    -- to the documentation.
  , ("the threshold is read as a multiple of the target spread",
      approx (huberAt 2.0 2.0) (0.5 * 2.0 * 2.0))
  , ("the default threshold is small enough to clip benchmark outliers",
      defaultHuberDelta < 0.5)
  , ("the default threshold is large enough to keep ordinary noise quadratic",
      defaultHuberDelta > 0.05)
  ]
  where
    losses :: [Double]
    losses = [huberAt 1.0 r | r <- [0.0, 0.5, 1.0, 2.0, 5.0, 10.0]]

    allMetrics :: [ErrorMetric]
    allMetrics = [RMSE, MSE, MAE, Huber 1.0]

    casesWith :: ErrorMetric -> Double -> [Double]
    casesWith metric residual = case unitSpreadData of
      Just ds -> caseErrors metric ds (Add (Var "x") (Const residual))
      Nothing -> []

    meanOf :: [Double] -> Double
    meanOf [] = 0.0
    meanOf xs = sum xs / fromIntegral (length xs)
