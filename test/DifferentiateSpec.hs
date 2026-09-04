-- | Tests for symbolic differentiation.
--
-- The load-bearing test is __agreement with a central difference__ across
-- several hundred random expressions at several points each. Hand-written
-- rule checks confirm the rules are the ones intended; only the numeric
-- comparison confirms they are /right/, because it is the one check that
-- does not reuse the differentiator's own reasoning.
--
-- == Why the comparison points are chosen, not random
--
-- A central difference of 'TypedGP.Eval.eval' is not an estimate of the
-- symbolic derivative near a protected guard — it is an honest measurement
-- of a step function, and the two /should/ disagree there. Comparing at
-- such a point would test nothing except that protection exists.
--
-- So the numeric check runs only where the expression and its derivative
-- are both domain-valid, with 'TypedGP.Eval.Saturated' excluded (a
-- derivative that merely got large is still the right derivative). Points
-- where either is invalid are skipped, and a separate assertion checks
-- that enough points survive for the test to mean anything — otherwise it
-- could pass by skipping everything.
module DifferentiateSpec (tests) where

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Differentiate (differentiable, differentiate)
import TypedGP.Eval
  ( Env
  , DomainError (Saturated)
  , eval
  , evalDomain
  , evalResultError
  )
import TypedGP.Expr (Expr (..))
import TypedGP.Gen.Grow (Method (..), genExpr)
import TypedGP.Random (mkSeed)
import TypedGP.Simplify (simplify)

cfg :: Config
cfg = defaultConfig

derivOf :: Expr -> Maybe Expr
derivOf = fmap simplify . differentiate "x"

at :: Double -> Env
at x = [("x", x), ("y", 0.7)]

-- | Is this expression meaningful here, ignoring saturation?
--
-- 'Saturated' is a numeric-range condition rather than a domain condition
-- (see the amendment in docs/phase2-design.md §5), so an expression that
-- merely overflowed is still mathematically fine and its derivative is
-- still the right one.
meaningfulAt :: Env -> Expr -> Bool
meaningfulAt env expr = case evalResultError (evalDomain env expr) of
  Nothing        -> True
  Just Saturated -> True
  Just _         -> False

-- | Central difference of 'eval'.
--
-- Step chosen at 1e-5: large enough that catastrophic cancellation in the
-- subtraction does not dominate at double precision, small enough that the
-- O(h^2) truncation error stays well under the comparison tolerance.
numericDerivative :: Expr -> Double -> Double
numericDerivative expr x =
  (eval (at (x + h)) expr - eval (at (x - h)) expr) / (2.0 * h)
  where
    h = 1.0e-5

-- | Points to compare at. Irrational-ish and away from 0, so that no
-- variable lands exactly on a protected guard by construction.
samplePoints :: [Double]
samplePoints = [0.37, 1.23, 2.71, -0.61, -1.87, 3.14]

-- | 200 random trees, the same generator the simplifier property uses.
randomExprs :: [Expr]
randomExprs = [fst (genExpr cfg Grow 4 (mkSeed s)) | s <- [1 .. 200]]

-- | Every (expression, point) pair where the numeric comparison is
-- actually meaningful: the expression differentiates, and both it and its
-- derivative are in-domain at that point.
comparablePairs :: [(Double, Double)]
comparablePairs =
  [ (numericDerivative expr x, eval (at x) d)
  | expr <- randomExprs
  , Just d <- [derivOf expr]
  , x <- samplePoints
  , meaningfulAt (at x) expr
  , meaningfulAt (at x) d
    -- Both one-sided evaluations must also be in-domain, or the central
    -- difference straddles a guard and measures the step rather than the
    -- slope.
  , meaningfulAt (at (x + 1.0e-5)) expr
  , meaningfulAt (at (x - 1.0e-5)) expr
    -- A derivative near the magnitude cap cannot be compared to a finite
    -- difference in any useful way.
  , abs (eval (at x) d) < 1.0e6
  ]

-- | Relative tolerance. Loose because a central difference is only
-- second-order accurate and the expressions are arbitrary compositions;
-- the failures this is meant to catch are wrong /rules/, which are off by
-- factors, not by a few parts per thousand.
agrees :: (Double, Double) -> Bool
agrees (numeric, symbolic) =
  abs (numeric - symbolic) <= 1.0e-3 * (1.0 + abs symbolic)

-- | The derivative of a known formula, checked numerically at one point.
checkAt :: Expr -> Double -> Bool
checkAt expr x = case derivOf expr of
  Nothing -> False
  Just d  -> agrees (numericDerivative expr x, eval (at x) d)

tests :: [(String, Bool)]
tests =
  -- Structural rules ----------------------------------------------------------
  [ ("a constant differentiates to zero",
      derivOf (Const 3.0) == Just (Const 0.0))
  , ("the target variable differentiates to one",
      derivOf (Var "x") == Just (Const 1.0))
  , ("another variable differentiates to zero",
      derivOf (Var "y") == Just (Const 0.0))
  , ("a sum differentiates termwise",
      derivOf (Add (Var "x") (Var "x")) == Just (Const 2.0))
  , ("2x differentiates to 2",
      derivOf (Mul (Const 2.0) (Var "x")) == Just (Const 2.0))
  , ("x - x differentiates to zero",
      derivOf (Sub (Var "x") (Var "x")) == Just (Const 0.0))

  -- Refusals ------------------------------------------------------------------
    -- Nothing means "not differentiable by these rules", never "zero".
  , ("gamma refuses", derivOf (Gamma (Var "x")) == Nothing)
  , ("zeta refuses", derivOf (Zeta (Var "x")) == Nothing)
  , ("a refusal propagates to the whole expression",
      derivOf (Add (Var "x") (Zeta (Var "x"))) == Nothing)
  , ("a refusal buried in a product still propagates",
      derivOf (Mul (Sin (Var "x")) (Gamma (Var "x"))) == Nothing)
    -- Refusing is about the operator, not about whether the variable
    -- appears: zeta(y) is constant in x, but the rules still have nothing
    -- to say about zeta and must not pretend otherwise.
  , ("zeta of another variable still refuses",
      derivOf (Zeta (Var "y")) == Nothing)
  , ("everything else is accepted",
      all differentiable
        [ Var "x", Const 1.0, Sin (Var "x"), Cos (Var "x"), Exp (Var "x")
        , Log (Var "x"), Sqrt (Var "x"), Tanh (Var "x"), Abs (Var "x")
        , Pow (Var "x") (Const 2.0), Div (Var "x") (Var "y")
        ])
    -- A zero constant base refuses only when the exponent is symbolic; a
    -- constant exponent takes the power rule, which is fine there.
  , ("a zero base with a symbolic exponent refuses",
      not (differentiable (Pow (Const 0.0) (Var "x"))))
  , ("a zero base with a constant exponent is accepted",
      differentiable (Pow (Const 0.0) (Const 2.0)))

  -- Elementary derivatives, checked numerically --------------------------------
    -- These are the rules stated one by one. Each is compared against a
    -- central difference rather than against a hand-written expected
    -- expression, so a test cannot agree with the code by sharing its
    -- mistake.
  , ("d/dx sin(x) = cos(x)", checkAt (Sin (Var "x")) 0.9)
  , ("d/dx cos(x) = -sin(x)", checkAt (Cos (Var "x")) 0.9)
  , ("d/dx exp(x)", checkAt (Exp (Var "x")) 0.9)
  , ("d/dx tanh(x)", checkAt (Tanh (Var "x")) 0.9)
  , ("d/dx log|x| at a positive point", checkAt (Log (Var "x")) 2.3)
    -- The sign-sensitive half. log|x| has derivative 1/x for both signs,
    -- and a rule written for log(x) alone would be wrong here.
  , ("d/dx log|x| at a negative point", checkAt (Log (Var "x")) (-2.3))
  , ("d/dx sqrt|x| at a positive point", checkAt (Sqrt (Var "x")) 2.3)
  , ("d/dx sqrt|x| at a negative point", checkAt (Sqrt (Var "x")) (-2.3))
  , ("d/dx abs(x) at a positive point", checkAt (Abs (Var "x")) 2.3)
  , ("d/dx abs(x) at a negative point", checkAt (Abs (Var "x")) (-2.3))
  , ("the product rule", checkAt (Mul (Sin (Var "x")) (Exp (Var "x"))) 0.8)
  , ("the quotient rule", checkAt (Div (Sin (Var "x")) (Var "x")) 1.7)
  , ("the chain rule nests",
      checkAt (Sin (Exp (Mul (Const 0.5) (Var "x")))) 0.8)

  -- Powers --------------------------------------------------------------------
    -- Even exponents: |x|^n = x^n, so the textbook rule and the magnitude
    -- rule agree, and both signs must check out.
  , ("d/dx x^2 at a positive point", checkAt (Pow (Var "x") (Const 2.0)) 1.4)
  , ("d/dx x^2 at a negative point", checkAt (Pow (Var "x") (Const 2.0)) (-1.4))
    -- Odd exponents are where the textbook rule n*f^(n-1)*f' is WRONG
    -- under these semantics, because Pow means |base|^expo. This is the
    -- assertion that would fail if the old rule had been carried over.
  , ("d/dx x^3 at a positive point", checkAt (Pow (Var "x") (Const 3.0)) 1.4)
  , ("d/dx |x|^3 at a negative point", checkAt (Pow (Var "x") (Const 3.0)) (-1.4))
  , ("d/dx x^1 is the derivative of |x|",
      checkAt (Pow (Var "x") (Const 1.0)) (-1.4))
  , ("d/dx x^0.5 at a positive point", checkAt (Pow (Var "x") (Const 0.5)) 1.4)
  , ("d/dx |x|^0.5 at a negative point",
      checkAt (Pow (Var "x") (Const 0.5)) (-1.4))
  , ("d/dx x^-1", checkAt (Pow (Var "x") (Const (-1.0))) 1.9)
    -- Constant base. Loosened from a > 0 to a /= 0 by the magnitude
    -- semantics, so the negative case must actually work.
  , ("d/dx 2^x", checkAt (Pow (Const 2.0) (Var "x")) 1.1)
  , ("d/dx (-2)^x, which means |2|^x", checkAt (Pow (Const (-2.0)) (Var "x")) 1.1)
    -- Both symbolic. This case returned Nothing under the old semantics.
  , ("d/dx x^x at a positive point",
      checkAt (Pow (Var "x") (Var "x")) 1.6)
  , ("d/dx of a fully symbolic power at a negative base",
      checkAt (Pow (Var "x") (Add (Var "y") (Const 2.0))) (-1.4))

  -- The property --------------------------------------------------------------
    -- Not vacuous: if the domain filters excluded everything, every
    -- assertion below would pass while checking nothing at all.
  , ("the numeric comparison covers at least 300 points",
      length comparablePairs >= 300)
  , ("symbolic and numeric derivatives agree on random expressions",
      all agrees comparablePairs)
    -- Some random trees must actually contain a refusing operator, or the
    -- refusal path is untested by the property.
  , ("some random expressions are not differentiable",
      any (not . differentiable) randomExprs)
  , ("most random expressions are differentiable",
      length (filter differentiable randomExprs) > 100)
  ]
