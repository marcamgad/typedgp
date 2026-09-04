-- | Tests for domain validity.
--
-- The feature's whole purpose is to separate two things the engine
-- previously conflated: /numerical protection/ (keep the search alive) and
-- /mathematical validity/ (is this expression meaningful here). So the
-- assertions that matter are the ones where the two __disagree__ — where
-- protected arithmetic returns a perfectly ordinary finite number and the
-- expression is nonetheless not real-valued.
--
-- @log(-5)@ is the sharpest case. 'protectedLog' computes @log |x|@, so it
-- returns @log 5 ≈ 1.609@: finite, plausible, and wrong. No numeric guard
-- fires. If 'evalDomain' had been written by copying the conditions out of
-- the protection code it would miss exactly this, which is why the test
-- exists rather than being assumed.
--
-- The second invariant tested here is that 'evalDomain' never changes what
-- 'eval' computes. That is what lets the penalty default to zero and leave
-- every earlier benchmark reproducible.
module DomainSpec (tests) where

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Data.Dataset (Dataset, mkDataset)
import TypedGP.DomainStats
  ( DomainBreakdown (..)
  , breakdownCount
  , breakdownInvalid
  , breakdownOver
  , breakdownTotal
  , divergesFromLegacyPow
  , saturationShare
  )
import TypedGP.Eval
  ( DomainError (..)
  , eval
  , evalDomain
  , evalResultError
  , evalResultValue
  , isValid
  , magnitudeCap
  )
import TypedGP.Expr (Expr (..))
import TypedGP.Fitness (Fitness (..), evaluateFitness)

env :: [(String, Double)]
env = [("x", 2.0), ("y", -5.0), ("z", 0.0)]

errorAt :: Expr -> Maybe DomainError
errorAt = evalResultError . evalDomain env

valid :: Expr -> Bool
valid = isValid . evalDomain env

-- | Every expression appearing below, for the agreement property.
--
-- Listed explicitly rather than generated: the point is to cover the
-- operators whose protected and unprotected behaviour differ, and a
-- random generator would mostly produce arithmetic that cannot disagree.

allFixtures :: [Expr]
allFixtures =
  [ Const 1.5
  , Var "x"
  , Var "missing"
  , Add (Var "x") (Const 1.0)
  , Sub (Var "x") (Var "y")
  , Mul (Var "x") (Var "y")
  , Div (Var "x") (Var "z")
  , Div (Var "x") (Const 4.0)
  , Log (Var "y")
  , Log (Var "z")
  , Log (Var "x")
  , Sqrt (Var "y")
  , Pow (Var "y") (Const 0.5)
  , Pow (Var "y") (Const 3.0)
  , Pow (Var "z") (Const (-2.0))
  , Pow (Const 2.0) (Const 10.0)
  , Gamma (Const (-3.0))
  , Gamma (Const 5.0)
  , Zeta (Const 1.0)
  , Zeta (Const 2.0)
  , Exp (Const 1000.0)
  , Sin (Var "x")
  , Tanh (Var "y")
  , Abs (Var "y")
  , Add (Log (Var "y")) (Sin (Var "x"))
  , Mul (Div (Var "x") (Var "z")) (Log (Var "y"))
  ]

-- | A dataset on which @1/x@ is invalid at exactly one of four points.
--
-- Constructed so the expected fraction is an exact binary fraction, which
-- makes the assertion an equality rather than a tolerance.

quarterBad :: Maybe Dataset
quarterBad = case mkDataset ["x"] [([1.0], 1.0), ([2.0], 0.5), ([0.0], 0.0), ([4.0], 0.25)] of
  Right ds -> Just ds
  Left _   -> Nothing

penaltyConfig :: Double -> Config
penaltyConfig weight = defaultConfig
  { cfgVariables = ["x"]
  , cfgParsimony = 0.0
  , cfgDomainPenalty = weight
  }

fitnessOf :: Double -> Expr -> Maybe Fitness
fitnessOf weight expr =
  fmap (\ds -> evaluateFitness (penaltyConfig weight) ds expr) quarterBad

tests :: [(String, Bool)]
tests =

  -- The disagreements ---------------------------------------------------------
    -- The headline. Protection returns log 5; validity says this is not a
    -- real logarithm. A condition copied from protectedLog's own guard
    -- (abs x < epsilon) would call this valid.
  [ ("log of a negative is invalid even though it returns a finite number",
      errorAt (Log (Var "y")) == Just LogOfNonPositive)
  , ("...and the number it returns is still log|x|, unchanged",
      evalResultValue (evalDomain env (Log (Var "y"))) == eval env (Log (Var "y")))
  , ("log of zero is invalid", errorAt (Log (Var "z")) == Just LogOfNonPositive)
  , ("log of a positive is valid", valid (Log (Var "x")))
  , ("division by zero is invalid", errorAt (Div (Var "x") (Var "z")) == Just DividedByZero)
  , ("ordinary division is valid", valid (Div (Var "x") (Const 4.0)))
    -- A negative base is no longer an error of any kind. 'Pow' means
    -- @|base| ** expo@, which is defined for every base, so there is
    -- nothing to report. See docs/phase5-pow-semantics.md.
  , ("a negative base with a fractional exponent is valid",
      valid (Pow (Var "y") (Const 0.5)))
  , ("a negative base with an integer exponent is valid",
      valid (Pow (Var "y") (Const 3.0)))
    -- The named cost, asserted so it cannot regress unnoticed in either
    -- direction: (-5)^3 is +125 here, not -125.
  , ("an odd integer power of a negative base takes the magnitude",
      eval env (Pow (Var "y") (Const 3.0)) == 125.0)
    -- The constructor survives so the type and the checkpoint format are
    -- undisturbed, but nothing produces it any more. Asserted rather than
    -- assumed: a dead constructor that quietly comes back to life would be
    -- invisible to -Wall.
  , ("nothing produces PowOfNegativeBase any more",
      all (\e -> errorAt e /= Just PowOfNegativeBase) allFixtures)
    -- 0 ** -2 is a division by zero wearing a different hat, and is
    -- reported as one rather than getting its own constructor.
  , ("zero to a negative power reports division by zero",
      errorAt (Pow (Var "z") (Const (-2.0))) == Just DividedByZero)
  , ("an ordinary power is valid", valid (Pow (Const 2.0) (Const 10.0)))
  , ("gamma at a pole is invalid", errorAt (Gamma (Const (-3.0))) == Just GammaAtPole)
  , ("gamma away from a pole is valid", valid (Gamma (Const 5.0)))
  , ("zeta at its pole is invalid", errorAt (Zeta (Const 1.0)) == Just ZetaAtPole)
  , ("zeta away from its pole is valid", valid (Zeta (Const 2.0)))

  -- Saturation ----------------------------------------------------------------
  , ("exp(1000) saturates and is invalid", errorAt (Exp (Const 1000.0)) == Just Saturated)
  , ("saturation still reports the capped value",
      evalResultValue (evalDomain env (Exp (Const 1000.0))) == magnitudeCap)
  , ("an ordinary expression is not saturated", valid (Mul (Var "x") (Var "y")))

  -- Things that are fine ------------------------------------------------------
  , ("plain arithmetic is valid", valid (Add (Var "x") (Const 1.0)))
  , ("an unbound variable reads as 0 and is valid", valid (Var "missing"))
  , ("sin is valid everywhere", valid (Sin (Var "x")))
  , ("tanh is valid everywhere", valid (Tanh (Var "y")))
  , ("abs is valid everywhere", valid (Abs (Var "y")))
    -- Not an error constructor; recorded so the omission is a documented
    -- decision rather than a silent gap. See the open question in
    -- docs/phase3-domain-design.md.
  , ("sqrt of a negative is deliberately NOT flagged", valid (Sqrt (Var "y")))

  -- Propagation ---------------------------------------------------------------
  , ("an error in a subexpression propagates to the root",
      errorAt (Add (Log (Var "y")) (Sin (Var "x"))) == Just LogOfNonPositive)
    -- Left-to-right, so the division is seen before the log. Arbitrary but
    -- stable, and stability is what makes it testable.
  , ("the first error in a left-to-right traversal is the one kept",
      errorAt (Mul (Div (Var "x") (Var "z")) (Log (Var "y"))) == Just DividedByZero)

  -- The agreement property ----------------------------------------------------
    -- The invariant that lets the penalty default to zero without
    -- disturbing anything: evalDomain is eval plus a verdict, never eval
    -- with a different answer.
  , ("evalDomain never changes the value eval computes",
      all (\e -> evalResultValue (evalDomain env e) == eval env e) allFixtures)
  , ("every fixture yields a finite value",
      all (\e -> not (isNaN (evalResultValue (evalDomain env e)))
                   && not (isInfinite (evalResultValue (evalDomain env e))))
          allFixtures)

  -- The penalty ---------------------------------------------------------------
  -- The breakdown -------------------------------------------------------------
  , ("a breakdown counts one evaluation per expression per point",
      maybe False
        (\ds -> breakdownTotal (breakdownOver ds [Var "x", Sin (Var "x")]) == 8)
        quarterBad)
  , ("an all-valid population reports nothing invalid",
      maybe False
        (\ds -> breakdownInvalid (breakdownOver ds [Mul (Var "x") (Const 2.0)]) == 0)
        quarterBad)
    -- One of the four points has x = 0.
  , ("invalid evaluations are counted under the right constructor",
      maybe False
        (\ds -> let b = breakdownOver ds [Div (Const 1.0) (Var "x")]
                in breakdownCount b DividedByZero == 1
                     && breakdownInvalid b == 1
                     && dbValid b == 3)
        quarterBad)
  , ("valid and invalid counts partition the total",
      maybe False
        (\ds -> let b = breakdownOver ds [Div (Const 1.0) (Var "x"), Log (Var "x")]
                in dbValid b + breakdownInvalid b == breakdownTotal b)
        quarterBad)
    -- Zero rather than a division by zero. No invalid evaluations means
    -- saturation dominates nothing, which is the useful answer.
  , ("saturation share is 0 when nothing is invalid",
      maybe False
        (\ds -> saturationShare (breakdownOver ds [Var "x"]) == 0.0)
        quarterBad)
  , ("saturation share is 1 when everything invalid is saturated",
      maybe False
        (\ds -> saturationShare (breakdownOver ds [Exp (Const 1000.0)]) == 1.0)
        quarterBad)
  , ("an empty population counts nothing",
      maybe False (\ds -> breakdownTotal (breakdownOver ds []) == 0) quarterBad)

  -- Legacy-Pow archaeology ----------------------------------------------------
    -- The old semantics kept the sign for integer exponents. The two
    -- definitions differ on exactly one case: negative base, odd integer
    -- exponent.
  , ("a negative base with an odd integer exponent diverges",
      divergesFromLegacyPow env (Pow (Var "y") (Const 3.0)))
  , ("an even integer exponent does not diverge",
      not (divergesFromLegacyPow env (Pow (Var "y") (Const 2.0))))
  , ("a fractional exponent does not diverge",
      not (divergesFromLegacyPow env (Pow (Var "y") (Const 0.5))))
  , ("a positive base never diverges",
      not (divergesFromLegacyPow env (Pow (Var "x") (Const 3.0))))
    -- The case the design note's prediction missed: x / x is exactly 1.0
    -- by construction, so this is an odd integer power at every input
    -- rather than at almost none. This is why the benchmark numbers moved.
  , ("a structurally-produced exponent of 1 diverges",
      divergesFromLegacyPow env (Pow (Var "y") (Div (Var "x") (Var "x"))))
  , ("divergence is found in a nested subexpression",
      divergesFromLegacyPow env (Add (Sin (Var "x")) (Pow (Var "y") (Const 3.0))))
  , ("an expression with no Pow never diverges",
      not (divergesFromLegacyPow env (Add (Var "x") (Sin (Var "y")))))

  -- The penalty ---------------------------------------------------------------
  , ("the penalty fixture builds", quarterBad /= Nothing)
    -- At zero weight the traversal is skipped, so the fraction reads 0
    -- even though the expression really is invalid on a quarter of the
    -- points. Asserted so the short-circuit is a stated behaviour rather
    -- than a surprise to a future reader of fitInvalidFraction.
  , ("at zero weight the invalid fraction is not computed",
      maybe False (\f -> fitInvalidFraction f == 0.0)
        (fitnessOf 0.0 (Div (Const 1.0) (Var "x"))))
  , ("at zero weight the domain penalty is zero",
      maybe False (\f -> fitDomainPenalty f == 0.0)
        (fitnessOf 0.0 (Div (Const 1.0) (Var "x"))))
    -- One of four points has x = 0.
  , ("at non-zero weight the invalid fraction is measured exactly",
      maybe False (\f -> fitInvalidFraction f == 0.25)
        (fitnessOf 1.0 (Div (Const 1.0) (Var "x"))))
  , ("the penalty is weight times fraction",
      maybe False (\f -> fitDomainPenalty f == 0.5)
        (fitnessOf 2.0 (Div (Const 1.0) (Var "x"))))
  , ("a fully valid expression is charged nothing",
      maybe False (\f -> fitDomainPenalty f == 0.0)
        (fitnessOf 1.0 (Mul (Var "x") (Const 2.0))))
    -- The load-bearing claim: switching the penalty on must not disturb
    -- the error term, only add to the total.
  , ("the penalty adds to the total without touching the error",
      case (fitnessOf 0.0 (Div (Const 1.0) (Var "x")),
            fitnessOf 1.0 (Div (Const 1.0) (Var "x"))) of
        (Just off, Just on) ->
          fitError off == fitError on
            && fitTotal on > fitTotal off
            && fitTotal on == fitTotal off + 0.25
        _ -> False)
  ]
