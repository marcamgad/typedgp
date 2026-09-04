-- | Tests for the constant optimiser.
--
-- Two layers. First, 'nelderMead' against textbook objective functions
-- with known minima — including Rosenbrock, whose curved valley is
-- precisely the geometry that defeats coordinate methods. Second, the
-- expression-level entry point on the shapes the benchmark suite says are
-- failing.
--
-- The comparison against 'compassSearch' is deliberate and is the point of
-- the module: the coupled cases are exactly where the old method stalls,
-- so the tests assert that the new one succeeds *and* that the old one
-- does not. If both ever pass, the distinction has evaporated and one of
-- them should be deleted.
module LocalSearchSpec (tests) where

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Data.Dataset (Dataset, sampleDataset)
import TypedGP.Expr (Expr (..))
import TypedGP.Fitness (errorOf)
import TypedGP.LocalSearch
  ( NelderMeadSettings (..)
  , compassSearch
  , constantsOf
  , defaultNelderMead
  , nelderMead
  , optimiseConstants
  , withConstants
  )
import TypedGP.Random (mkSeed)

optimise :: ([Double] -> Double) -> [Double] -> [Double]
optimise = nelderMead defaultNelderMead

compass :: ([Double] -> Double) -> [Double] -> [Double]
compass objective = compassSearch 200 1.0 1.0e-9 objective

near :: Double -> Double -> Double -> Bool
near tolerance actual expected = abs (actual - expected) <= tolerance

allNear :: Double -> [Double] -> [Double] -> Bool
allNear tolerance actual expected =
  length actual == length expected && and (zipWith (near tolerance) actual expected)

-- Textbook objectives ---------------------------------------------------------

-- | Minimum at (1, 1), inside a long curved valley. The standard test for
-- whether an optimiser can follow a ridge; coordinate methods famously
-- cannot.
rosenbrock :: [Double] -> Double
rosenbrock values = case values of
  (x : y : _) -> (1.0 - x) * (1.0 - x) + 100.0 * (y - x * x) * (y - x * x)
  _           -> 0.0

-- | Minimum at (3, -1, 0, 1). Four parameters, badly scaled.
powell :: [Double] -> Double
powell values = case values of
  (a : b : c : d : _) ->
    let t1 = a + 10.0 * b
        t2 = c - d
        t3 = b - 2.0 * c
        t4 = a - d
    in t1 * t1 + 5.0 * t2 * t2 + t3 * t3 * t3 * t3 + 10.0 * t4 * t4 * t4 * t4
  _ -> 0.0

-- | A simple separable bowl with minimum at (2, -3).
bowl :: [Double] -> Double
bowl values = case values of
  (x : y : _) -> (x - 2.0) * (x - 2.0) + (y + 3.0) * (y + 3.0)
  _           -> 0.0

-- Expression-level fixtures ----------------------------------------------------

baseConfig :: Config
baseConfig = defaultConfig { cfgVariables = ["x"] }

-- | @y = 4 x^1.7@ — the coupled case. Raising the exponent rescales the
-- output, so the coefficient must move with it.
powerLawData :: Dataset
powerLawData = fst (sampleDataset ["x"] (0.5, 4.0) 60 target (mkSeed 77))
  where
    target env = 4.0 * (maybe 0.0 id (lookup "x" env) ** 1.7)

powerLawModel :: Expr
powerLawModel = Mul (Const 1.0) (Pow (Var "x") (Const 1.0))

-- | @y = 2 sin(3x + 0.5)@ — frequency and phase, the shape the benchmark
-- suite fails hardest on.
sineData :: Dataset
sineData = fst (sampleDataset ["x"] (-3.0, 3.0) 80 target (mkSeed 91))
  where
    target env = 2.0 * sin (3.0 * maybe 0.0 id (lookup "x" env) + 0.5)

-- | Started at frequency 1, three local minima away from the truth.
sineModel :: Expr
sineModel =
  Mul (Const 1.0) (Sin (Add (Mul (Const 1.0) (Var "x")) (Const 0.0)))

-- | The same structure started near the true frequency, to show the
-- optimiser is capable once initialisation puts it in the right basin.
sineModelNear :: Expr
sineModelNear =
  Mul (Const 1.0) (Sin (Add (Mul (Const 2.7) (Var "x")) (Const 0.0)))

errorOn :: Dataset -> Expr -> Double
errorOn ds = errorOf (cfgErrorMetric baseConfig) ds

tests :: [(String, Bool)]
tests =
  -- Nelder-Mead on known objectives ----------------------------------------
  [ ("the simplex finds a separable minimum",
      allNear 1.0e-4 (optimise bowl [0.0, 0.0]) [2.0, -3.0])
  , ("the simplex finds a minimum from far away",
      allNear 1.0e-3 (optimise bowl [500.0, -500.0]) [2.0, -3.0])
    -- The headline capability: Rosenbrock's valley is curved, so progress
    -- requires moving both parameters together.
  , ("the simplex solves Rosenbrock",
      allNear 1.0e-3 (optimise rosenbrock [-1.2, 1.0]) [1.0, 1.0])
  , ("the simplex drives Rosenbrock's value to zero",
      rosenbrock (optimise rosenbrock [-1.2, 1.0]) < 1.0e-6)
  , ("the simplex solves a four-parameter problem",
      powell (optimise powell [3.0, -1.0, 0.0, 1.0]) < 1.0e-4)
  , ("the simplex handles a single parameter",
      allNear 1.0e-6 (optimise (\v -> case v of (x : _) -> (x - 7.0) ** 2.0; _ -> 0.0) [0.0]) [7.0])
  , ("the simplex leaves an empty parameter list alone", optimise bowl [] == [])
  , ("the simplex never worsens the starting point",
      bowl (optimise bowl [1.0, 1.0]) <= bowl [1.0, 1.0])
  , ("more iterations do not make things worse",
      let short = nelderMead defaultNelderMead { nmIterations = 5 } rosenbrock [-1.2, 1.0]
          long = optimise rosenbrock [-1.2, 1.0]
      in rosenbrock long <= rosenbrock short)

  -- Where compass search fails and the simplex does not ---------------------
    -- Both solve the separable bowl. This is the control: it shows the
    -- comparison below is about coupling, not about one method simply
    -- being broken.
  , ("compass search also solves a separable problem",
      allNear 1.0e-3 (compass bowl [0.0, 0.0]) [2.0, -3.0])
  , ("compass search fails on Rosenbrock",
      not (allNear 1.0e-2 (compass rosenbrock [-1.2, 1.0]) [1.0, 1.0]))
  , ("the simplex beats compass search on Rosenbrock",
      rosenbrock (optimise rosenbrock [-1.2, 1.0])
        < rosenbrock (compass rosenbrock [-1.2, 1.0]))

  -- The benchmark's actual failures -----------------------------------------
    -- These are the two shapes the suite reports as unsolved: the power
    -- law whose exponent is missed in every seed, and the sine whose
    -- frequency has to be found by luck. Handed the right structure, the
    -- optimiser must supply the numbers.
  , ("the optimiser recovers a power law's coefficient and exponent",
      case constantsOf (optimiseConstants baseConfig powerLawData powerLawModel) of
        (coefficient : exponent' : _) ->
          near 0.05 coefficient 4.0 && near 0.05 exponent' 1.7
        _ -> False)
  , ("the refitted power law is near-exact",
      errorOn powerLawData (optimiseConstants baseConfig powerLawData powerLawModel) < 0.01)
    -- A limitation, isolated precisely.
    --
    -- Coupling is solved (above); multimodality is not, and no local
    -- optimiser will solve it. The error of a sine as a function of its
    -- frequency has a local minimum near every frequency, so a simplex
    -- started at 1 and needing to reach 3 has several barriers in the way
    -- and stops at the first one. This is a property of the objective, not
    -- a deficiency in the method.
    --
    -- The pair of tests below separates the two causes: from a distant
    -- start the frequency is not found, and from a nearby start the same
    -- optimiser recovers all three constants exactly. So what is missing
    -- is *initialisation* -- a periodicity search that seeds trigonometric
    -- subtrees with a plausible frequency -- not a better optimiser.
  , ("a distant frequency is not recovered by local search alone",
      case constantsOf (optimiseConstants baseConfig sineData sineModel) of
        (_ : frequency : _) -> not (near 0.05 frequency 3.0)
        _ -> False)
  , ("local search still improves the distant-start sine",
      errorOn sineData (optimiseConstants baseConfig sineData sineModel)
        < errorOn sineData sineModel)
  , ("from a nearby frequency the same optimiser recovers the sine exactly",
      case constantsOf (optimiseConstants baseConfig sineData sineModelNear) of
        (amplitude : frequency : phase : _) ->
          near 0.05 amplitude 2.0 && near 0.05 frequency 3.0 && near 0.05 phase 0.5
        _ -> False)
  , ("the nearby-start sine refits to near-exact",
      errorOn sineData (optimiseConstants baseConfig sineData sineModelNear) < 0.01)
  , ("the optimiser improves on the starting point",
      errorOn powerLawData (optimiseConstants baseConfig powerLawData powerLawModel)
        < errorOn powerLawData powerLawModel)

  -- Totality ----------------------------------------------------------------
  , ("optimising a constant-free expression is a no-op",
      optimiseConstants baseConfig powerLawData (Var "x") == Var "x")
  , ("optimising never returns a worse expression",
      errorOn sineData (optimiseConstants baseConfig sineData sineModel)
        <= errorOn sineData sineModel)
  , ("constant round-tripping is preserved",
      let e = Add (Const 1.5) (Mul (Const 2.5) (Var "x"))
      in withConstants (constantsOf e) e == e)
  , ("the optimiser preserves the constant count",
      length (constantsOf (optimiseConstants baseConfig sineData sineModel))
        == length (constantsOf sineModel))
  , ("the optimiser preserves the structure",
      countOps (optimiseConstants baseConfig sineData sineModel) == countOps sineModel)
  ]
  where
    -- Structural fingerprint: the optimiser must change values only.
    countOps :: Expr -> Int
    countOps expr = case expr of
      Const _ -> 1
      Var _   -> 1
      Add a b -> 1 + countOps a + countOps b
      Sub a b -> 1 + countOps a + countOps b
      Mul a b -> 1 + countOps a + countOps b
      Div a b -> 1 + countOps a + countOps b
      Pow a b -> 1 + countOps a + countOps b
      Sin a   -> 1 + countOps a
      Cos a   -> 1 + countOps a
      Exp a   -> 1 + countOps a
      Log a   -> 1 + countOps a
      Sqrt a  -> 1 + countOps a
      Tanh a  -> 1 + countOps a
      Abs a   -> 1 + countOps a
      Gamma a -> 1 + countOps a
      Zeta a  -> 1 + countOps a
      Sum _ lo hi body -> 1 + countOps lo + countOps hi + countOps body
