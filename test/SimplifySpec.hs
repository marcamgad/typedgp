-- | Tests for the algebraic simplifier.
--
-- The headline test is the __eval-preservation property__: across several
-- hundred randomly generated expressions and several random environments
-- each, simplification must not change what an expression computes. A
-- simplifier that silently alters behaviour is worse than no simplifier,
-- and every individual rule below is only sound for a reason — so the
-- property is what checks the reasoning actually held.
--
-- It is asserted twice, deliberately: once up to a tolerance (the
-- conventional bar) and once as __exact bit equality__. The exact form is
-- the honest one here, because every admitted rule is supposed to be an
-- identity on IEEE doubles rather than an approximation. If the exact
-- assertion ever fails while the tolerant one passes, a rule has started
-- reassociating arithmetic and the difference will grow.
module SimplifySpec (tests) where

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Eval
  ( DomainError (DividedByZero)
  , Env
  , eval
  , evalDomain
  , evalResultError
  , magnitudeCap
  )
import TypedGP.Expr (Expr (..), countNodes, flatten, opName)
import TypedGP.Gen.Grow (Method (..), genExpr)
import TypedGP.Random (Seed, mkSeed, nextRange)
import TypedGP.Simplify (isFoldable, simplify, simplifyOnce)

cfg :: Config
cfg = defaultConfig

-- | 400 random trees, deep enough to contain nested structure worth
-- simplifying.
randomExprs :: [Expr]
randomExprs = [fst (genExpr cfg Grow 5 (mkSeed s)) | s <- [1 .. 400]]

-- | The values every rule's soundness argument turns on.
--
-- Random generation cannot reach these. 'genExpr' draws constants from a
-- continuous range, so the probability of it emitting @Const 1.0@ exactly
-- — the trigger for the @x ^ 1@ rule — is zero in practice. Every
-- structural rule below is guarded by an exact comparison against one of
-- these values, which means the random property test can never fire any
-- of them and has been proving something much weaker than it appears to.
--
-- Verified by mutation, 2026-08-19: restoring the unsound @x ^ 1 -> x@
-- rule leaves "simplification preserves eval, exactly" __passing__ over
-- all 400 random trees. It was two hand-written assertions that caught
-- that bug, not the property.
criticalConstants :: [Double]
criticalConstants = [0.0, 1.0, -1.0, 2.0, 3.0, 0.5, -0.5]

-- | Environments that put variables exactly on the protected primitives'
-- boundaries: zero (division, log, sqrt, the pole cases) and negatives
-- (the magnitude-taking cases in log, sqrt and pow).
criticalEnvs :: [Env]
criticalEnvs =
  [ zip (cfgVariables cfg) values
  | values <- [ [0.0, 0.0], [0.0, 1.0], [-3.0, -3.0], [-3.0, 2.0]
              , [1.0, -1.0], [2.0, 0.0], [-1.0, 0.0]
              ]
  ]

-- | Every rule's trigger shape, instantiated at every critical constant.
--
-- Built structurally rather than sampled, so adding a rule to
-- "TypedGP.Simplify" without adding its shape here is the one gap this
-- cannot close by itself — but every rule that exists today is covered,
-- and the shapes are cheap to extend.
--
-- The sub-expressions are deliberately sign-varying (@Var "x"@, @Sin@)
-- rather than obviously non-negative, because the interesting failures are
-- exactly the ones where a rule is sound for positive inputs and wrong for
-- negative ones.
criticalExprs :: [Expr]
criticalExprs =
  concat
    [ [ binary c, binaryLeft c, unaryPow c, powBase c ]
    | c <- criticalConstants
    ]
    ++ selfReferential
  where
    binary :: Double -> Expr
    binary c =
      Add (Sub (Mul (Div (Var "x") (Const c)) (Const c)) (Const c)) (Const c)

    -- The commuted forms. 'Simplify' matches @Add a (Const 0)@ and
    -- @Add (Const 0) b@ as separate rules, and 'binary' above only ever
    -- puts the constant on the right — so without this, three of the
    -- thirteen rules (@Add (Const 0) b@, @Mul (Const 1) b@,
    -- @Mul (Const 0) _@) had no trigger in this population at all.
    --
    -- Found by auditing the rule list against these shapes rather than by
    -- assuming the coverage was complete, after describing this population
    -- as covering "every rule" when it did not.
    binaryLeft :: Double -> Expr
    binaryLeft c =
      Add (Const c) (Mul (Const c) (Sub (Const c) (Var "x")))

    -- The shape that broke: a possibly-negative base under an exact
    -- constant exponent.
    unaryPow :: Double -> Expr
    unaryPow c = Pow (Var "x") (Const c)

    powBase :: Double -> Expr
    powBase c = Pow (Const c) (Var "x")

    -- Rules that fire on structural equality rather than on a constant.
    selfReferential :: [Expr]
    selfReferential =
      [ Sub (Var "x") (Var "x")
      , Div (Var "x") (Var "x")
      , Sub (Sin (Var "x")) (Sin (Var "x"))
      , Div (Sin (Var "x")) (Sin (Var "x"))
        -- x / x is exactly 1.0 by construction, which is how an exact
        -- integer exponent reaches Pow without any constant being 1.0.
        -- This is the shape that made the Pow semantics change move every
        -- benchmark number.
      , Pow (Var "x") (Div (Var "x") (Var "x"))
      , Pow (Var "x") (Sub (Var "x") (Var "x"))
      , Pow (Sin (Var "x")) (Div (Var "y") (Var "y"))
      ]

-- | Five random environments, so a rule that happens to be correct at one
-- point in the input space cannot pass by luck.
randomEnvs :: [Env]
randomEnvs = [envFrom (mkSeed s) | s <- [9001 .. 9005]]
  where
    envFrom :: Seed -> Env
    envFrom seed = fst (go (cfgVariables cfg) seed)

    go :: [String] -> Seed -> (Env, Seed)
    go [] st = ([], st)
    go (v : vs) st =
      let (value, st') = nextRange (-3.0) 3.0 st
          (rest, st'') = go vs st'
      in ((v, value) : rest, st'')

-- | Every (expression, environment) pair the property is checked over.
--
-- Random trees at random points, /plus/ every rule's trigger shape at
-- every boundary value. The second half is what makes this a regression
-- test for changes to "TypedGP.Eval" rather than a test of the
-- simplifier's typical behaviour.
checks :: [(Double, Double)]
checks =
  [ (eval env expr, eval env (simplify expr))
  | expr <- randomExprs ++ criticalExprs
  , env <- randomEnvs ++ criticalEnvs
  ]

-- | The critical half on its own, so a failure says which population
-- broke rather than only that something did.
criticalChecks :: [(Double, Double)]
criticalChecks =
  [ (eval env expr, eval env (simplify expr))
  | expr <- criticalExprs
  , env <- randomEnvs ++ criticalEnvs
  ]

closeEnough :: Double -> Double -> Bool
closeEnough a b = abs (a - b) <= 1.0e-9 * (1.0 + max (abs a) (abs b))

-- | One representative trigger for every rule in "TypedGP.Simplify",
-- listed in the same order as the rules themselves.
--
-- Kept in step with that module by hand, which is the residual weakness:
-- a rule added there without an entry here is still outside the net. What
-- this does buy is that a rule which /has/ an entry cannot silently lose
-- its coverage when 'criticalExprs' is refactored.
ruleTriggers :: [Expr]
ruleTriggers =
  [ Add (Var "x") (Const 0.0)
  , Add (Const 0.0) (Var "x")
  , Sub (Var "x") (Const 0.0)
  , Mul (Var "x") (Const 1.0)
  , Mul (Const 1.0) (Var "x")
  , Div (Var "x") (Const 1.0)
  , Mul (Var "x") (Const 0.0)
  , Mul (Const 0.0) (Var "x")
  , Pow (Var "x") (Const 1.0)
  , Pow (Var "x") (Const 0.0)
  , Pow (Const 1.0) (Var "x")
  , Sub (Var "x") (Var "x")
  , Div (Var "x") (Var "x")
  ]

-- | Does some node of @haystack@ match @needle@ up to its leaves, such
-- that simplification will fire the same rule?
--
-- Compares 'TypedGP.Expr.opName' and constant payloads rather than whole
-- subtrees, because the population instantiates each shape inside a larger
-- expression with different operands.
reduces :: Expr -> Expr -> Bool
reduces haystack needle = any matches (flatten haystack)
  where
    matches :: Expr -> Bool
    matches node = opName node == opName needle && sameConstants node needle

    sameConstants :: Expr -> Expr -> Bool
    sameConstants a b = constantsOf a == constantsOf b

    constantsOf :: Expr -> [Double]
    constantsOf e = case e of
      Add l r -> immediate l ++ immediate r
      Sub l r -> immediate l ++ immediate r
      Mul l r -> immediate l ++ immediate r
      Div l r -> immediate l ++ immediate r
      Pow l r -> immediate l ++ immediate r
      _       -> []

    -- Only a directly-adjacent constant counts; a constant buried deeper
    -- does not determine whether this node's rule fires.
    immediate :: Expr -> [Double]
    immediate (Const c) = [c]
    immediate _         = []

tests :: [(String, Bool)]
tests =
  -- The property ----------------------------------------------------------
  [ ("the property covers at least 2000 expression/environment pairs",
      length checks >= 2000)
  , ("simplification preserves eval, to tolerance",
      all (uncurry closeEnough) checks)
  , ("simplification preserves eval, exactly",
      all (uncurry (==)) checks)
  , ("simplification never grows an expression",
      all (\e -> countNodes (simplify e) <= countNodes e) randomExprs)
  , ("simplification is idempotent",
      all (\e -> simplify (simplify e) == simplify e) randomExprs)
  , ("simplification actually fires on some random trees",
      any (\e -> simplify e /= e) randomExprs)

  -- The standing regression check on TypedGP.Eval --------------------------
    -- This is the half that matters when a protected primitive changes.
    -- Every rule in Simplify.hs is a claim about what 'eval' computes, and
    -- those claims are argued in comments that nothing invalidates
    -- automatically. These assertions are what turn "re-check every rule"
    -- from a documented obligation into something the build enforces.
  , ("the critical population covers every rule's trigger at every boundary",
      length criticalChecks >= 250)
  , ("simplification preserves eval on every rule's trigger shape",
      all (uncurry (==)) criticalChecks)
    -- The rules only prove anything if they actually fire on this
    -- population. If a future refactor stopped them matching, the
    -- assertion above would pass vacuously and say nothing at all.
  , ("the critical population actually triggers rules",
      length (filter (\e -> simplify e /= e) criticalExprs) >= 10)
    -- Coverage, asserted per rule rather than claimed in prose. Each entry
    -- is a rule's trigger shape; the assertion is that some member of the
    -- critical population actually reduces it, so a rule with no trigger
    -- here shows up as a failure instead of as a silent gap.
    --
    -- This exists because the population was described as covering "every
    -- rule" while three commuted variants had no trigger at all. Prose
    -- coverage claims are not checkable; this is.
  , ("every rewrite rule has a trigger in the critical population",
      all (\shape -> any (`reduces` shape) criticalExprs) ruleTriggers)

  -- Identity rules --------------------------------------------------------
  , ("x + 0 becomes x", simplify (Add (Var "x") (Const 0.0)) == Var "x")
  , ("0 + x becomes x", simplify (Add (Const 0.0) (Var "x")) == Var "x")
  , ("x - 0 becomes x", simplify (Sub (Var "x") (Const 0.0)) == Var "x")
  , ("x * 1 becomes x", simplify (Mul (Var "x") (Const 1.0)) == Var "x")
  , ("1 * x becomes x", simplify (Mul (Const 1.0) (Var "x")) == Var "x")
  , ("x / 1 becomes x", simplify (Div (Var "x") (Const 1.0)) == Var "x")

  -- Exponentiation rules --------------------------------------------------
    -- x^1 is |x|, not x, because Pow means |base| ** expo. The rule that
    -- looks most obviously safe is the one the semantics change broke, and
    -- the eval-preservation property below is what caught it.
  , ("x^1 becomes abs(x), not x",
      simplify (Pow (Var "x") (Const 1.0)) == Abs (Var "x"))
  , ("...and only becomes x when the base cannot be negative",
      simplify (Pow (Abs (Var "x")) (Const 1.0)) == Abs (Var "x"))
  , ("the x^1 rule is eval-preserving on a negative base",
      eval [("x", -3.0)] (simplify (Pow (Var "x") (Const 1.0)))
        == eval [("x", -3.0)] (Pow (Var "x") (Const 1.0)))
  , ("x^0 becomes 1", simplify (Pow (Var "x") (Const 0.0)) == Const 1.0)
  , ("1^x becomes 1", simplify (Pow (Const 1.0) (Var "x")) == Const 1.0)
  , ("x^2 is left alone",
      simplify (Pow (Var "x") (Const 2.0)) == Pow (Var "x") (Const 2.0))
    -- Deliberately NOT simplified: protectedPow returns the pole sentinel
    -- 1 for a negative exponent, so 0^x is not identically 0.
  , ("0^x is deliberately not simplified",
      simplify (Pow (Const 0.0) (Var "x")) == Pow (Const 0.0) (Var "x"))
  , ("the x^0 rule agrees with eval at zero",
      eval [("x", 0.0)] (simplify (Pow (Var "x") (Const 0.0)))
        == eval [("x", 0.0)] (Pow (Var "x") (Const 0.0)))
  , ("powers of constants fold",
      simplify (Pow (Const 2.0) (Const 10.0)) == Const 1024.0)

  -- Absorbing and cancelling rules ----------------------------------------
  , ("x * 0 becomes 0", simplify (Mul (Var "x") (Const 0.0)) == Const 0.0)
  , ("0 * x becomes 0", simplify (Mul (Const 0.0) (Var "x")) == Const 0.0)
  , ("x - x becomes 0", simplify (Sub (Var "x") (Var "x")) == Const 0.0)
  , ("x / x becomes 1", simplify (Div (Var "x") (Var "x")) == Const 1.0)
  , ("a compound subtree cancels with itself",
      simplify (Sub (Sin (Var "y")) (Sin (Var "y"))) == Const 0.0)
    -- x / x is 1 even at x = 0, because protected division defines it so.
    -- The rule would be unsound without that guarantee, so it is worth an
    -- explicit test rather than trusting the comment.
  , ("x / x is 1 even where x is zero",
      eval [("x", 0.0)] (simplify (Div (Var "x") (Var "x")))
        == eval [("x", 0.0)] (Div (Var "x") (Var "x")))

  -- Constant folding ------------------------------------------------------
  , ("constants fold", simplify (Add (Const 1.0) (Const 2.0)) == Const 3.0)
  , ("nested constants fold to a single literal",
      simplify (Mul (Add (Const 1.0) (Const 2.0)) (Sub (Const 5.0) (Const 1.0)))
        == Const 12.0)
  , ("unary constants fold", simplify (Sin (Const 0.0)) == Const 0.0)
    -- Previously asserted that this folds to @Const 1.0@, the protected
    -- sentinel. It no longer folds at all, and that is the fix rather than
    -- a regression.
    --
    -- Folding it was eval-preserving — both sides give 1.0 — but it baked
    -- the sentinel in as an ordinary constant, so 'evalDomain' could no
    -- longer report the 'DividedByZero' that the unfolded expression
    -- carries. "TypedGP.Differentiate" tells callers to run derivatives
    -- through 'simplify' and to read validity off the result with
    -- 'evalDomain', so this was silently erasing the very thing that
    -- answer depends on.
  , ("a division by zero is not folded away",
      simplify (Div (Const 1.0) (Const 0.0)) == Div (Const 1.0) (Const 0.0))
  , ("...and its domain error survives simplification",
      evalResultError (evalDomain [] (simplify (Div (Const 1.0) (Const 0.0))))
        == Just DividedByZero)
  , ("a log of a negative is not folded away",
      simplify (Log (Const (-1.0))) == Log (Const (-1.0)))
    -- Saturation is excluded, per the §5 amendment: the value is right and
    -- the representation ran out, so a constant carrying it says the same
    -- thing. Folding here loses nothing.
  , ("a saturating constant expression still folds",
      simplify (Exp (Const 1000.0)) == Const magnitudeCap)
    -- Ordinary closed arithmetic is unaffected, so the change costs no
    -- everyday simplification.
  , ("clean constant arithmetic still folds",
      simplify (Add (Mul (Const 2.0) (Const 3.0)) (Const 1.0)) == Const 7.0)
  , ("a bare constant is left alone", simplify (Const 2.5) == Const 2.5)
  , ("a bare variable is left alone", simplify (Var "x") == Var "x")
  , ("isFoldable rejects leaves",
      not (isFoldable (Const 1.0)) && not (isFoldable (Var "x")))
  , ("isFoldable accepts a variable-free subtree",
      isFoldable (Add (Const 1.0) (Const 2.0)))
  , ("isFoldable rejects anything containing a variable",
      not (isFoldable (Add (Var "x") (Const 2.0))))

  -- Composition -----------------------------------------------------------
  , ("cancellation inside multiplication collapses entirely",
      simplify (Mul (Sub (Var "x") (Var "x")) (Var "y")) == Const 0.0)
  , ("one bottom-up pass reaches nested rules",
      simplifyOnce (Mul (Sub (Var "x") (Var "x")) (Const 1.0)) == Const 0.0)
  , ("a deep chain of identities collapses",
      simplify (Add (Mul (Add (Var "x") (Const 0.0)) (Const 1.0)) (Const 0.0))
        == Var "x")

  -- Deliberate omissions --------------------------------------------------
    -- Reassociation is not eval-preserving under IEEE arithmetic and the
    -- clamping in TypedGP.Eval, so it is intentionally absent. This test
    -- pins that decision down: if someone adds a flattening rule, it fails
    -- and they have to read the module note explaining why.
  , ("reassociation is deliberately not performed",
      simplify (Add (Add (Var "x") (Const 1.0)) (Const 2.0))
        == Add (Add (Var "x") (Const 1.0)) (Const 2.0))
  , ("division by a variable is not cancelled",
      simplify (Div (Mul (Var "x") (Var "y")) (Var "y"))
        == Div (Mul (Var "x") (Var "y")) (Var "y"))
  ]
