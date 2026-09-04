-- | Tests for the @Sum@ binder.
--
-- Structured around the four assertions the 2026-09-04 amendment to §2 of
-- @docs\/phase2-design.md@ names as the ones that make 'sealedIndices' real
-- rather than decorative — including the non-vacuity guard, without which
-- every sealing assertion here would pass on a tree with no binder in it.
--
-- The amendment's point was that the mitigation cannot be written before
-- the constructor it protects: with no binders, @sealedIndices@ could only
-- be @const []@, which passes any test written for it and protects
-- nothing. So these land together with the constructor, and the operator
-- assertions are __verified by mutation__ — neutering 'sealedIndices' to
-- @const []@ must fail them.
module BinderSpec (tests) where

import TypedGP.Checkpoint (parseExprS, renderExprS)
import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Differentiate (differentiate)
import TypedGP.Eval
  ( DomainError (IterationCapped)
  , eval
  , evalChecked
  , evalDomain
  , evalResultError
  , isValid
  , iterationCap
  )
import TypedGP.Expr
  ( Expr (..)
  , countNodes
  , flatten
  , freeVariables
  , hasBinder
  , sealedIndices
  , unsealedIndices
  , variablesOf
  )
import TypedGP.Gen.Grow (Method (..), genExpr)
import TypedGP.Ops.Crossover (crossover)
import TypedGP.Ops.Hoist (hoistMutation)
import TypedGP.Ops.Mutation (pointMutation, subtreeMutation)
import TypedGP.Pretty (pretty)
import TypedGP.Random (Seed, mkSeed)
import TypedGP.Simplify (simplify)

cfg :: Config
cfg = defaultConfig { cfgVariables = ["x", "y"] }

-- | @sum(i = 1 .. 3, i * x)@, wrapped so the binder is not at the root.
--
-- Pre-order layout, which the index assertions below refer to:
--
-- > 0 Add
-- > 1   Var x
-- > 2   Sum "i"          <- the binder node itself; a legal operator target
-- > 3     Const 1        <- lower bound, sealed
-- > 4     Const 3        <- upper bound, sealed
-- > 5     Mul            <- body, sealed
-- > 6       Var i        <- sealed
-- > 7       Var x        <- sealed
withBinder :: Expr
withBinder = Add (Var "x") theSum

theSum :: Expr
theSum = Sum "i" (Const 1.0) (Const 3.0) (Mul (Var "i") (Var "x"))

-- | The same shape with no binder, for the reproducibility property.
noBinder :: Expr
noBinder = Add (Var "x") (Mul (Var "y") (Var "x"))

env :: [(String, Double)]
env = [("x", 2.0), ("y", 5.0)]

-- | Every binder node appearing in an expression.
sumsIn :: Expr -> [Expr]
sumsIn e = [s | s@(Sum _ _ _ _) <- flatten e]

-- | Did an operator leave every surviving binder untouched?
--
-- A binder may legitimately disappear (replaced wholesale) or be
-- duplicated. What it may never do is come back /changed/, because that
-- means an operator reached inside it.
bindersIntact :: Expr -> Bool
bindersIntact child = all (== theSum) (sumsIn child)

-- | Run every operator over many seeds and check the invariant each time.
--
-- All three are exercised because they select sites independently and a
-- fix applied to one proves nothing about the others.
operatorsRespectSealing :: Bool
operatorsRespectSealing = all intact [1 .. 60 :: Int]
  where
    intact :: Int -> Bool
    intact k =
      let s = mkSeed (fromIntegral k)
          ((childA, childB), _) = crossover cfg withBinder withBinder s
          (mutated, _) = subtreeMutation cfg withBinder s
          (pointed, _) = pointMutation cfg withBinder s
          (hoisted, _) = hoistMutation withBinder s
      in all bindersIntact [childA, childB, mutated, pointed, hoisted]

-- | A range longer than 'iterationCap'.
oversized :: Expr
oversized =
  Sum "i" (Const 1.0) (Const (fromIntegral iterationCap * 10.0)) (Const 1.0)

-- | A binder whose body mentions nothing, with a range past the cap.
--
-- Empty 'variablesOf', so it looks foldable to 'isFoldable'; and long
-- enough that evaluating it truncates.
cappedClosed :: Expr
cappedClosed =
  Sum "i" (Const 1.0) (Const (fromIntegral iterationCap * 10.0)) (Const 5.0)

roundTrips :: Expr -> Bool
roundTrips e = parseExprS (renderExprS e) == Right e

tests :: [(String, Bool)]
tests =
  -- Non-vacuity ---------------------------------------------------------------
    -- The guard the amendment insists on. Without it every sealing
    -- assertion below passes trivially on a tree containing no binder,
    -- which is exactly the failure mode that made writing 'sealedIndices'
    -- early impossible.
  [ ("the fixture actually contains a binder", hasBinder withBinder)
  , ("sealedIndices is non-empty for it", not (null (sealedIndices withBinder)))
  , ("the control fixture contains no binder", not (hasBinder noBinder))

  -- What is sealed ------------------------------------------------------------
    -- Indices 3..7: both bounds and the whole body. The binder node
    -- itself (index 2) is NOT sealed — it must stay a legal target, or a
    -- binder could never be removed once created.
  , ("both bounds and the body are sealed",
      sealedIndices withBinder == [3, 4, 5, 6, 7])
  , ("the binder node itself is not sealed",
      2 `notElem` sealedIndices withBinder)
  , ("nothing is sealed in a binder-free tree",
      null (sealedIndices noBinder))
  , ("sealed and unsealed partition the tree",
      length (sealedIndices withBinder) + length (unsealedIndices withBinder)
        == countNodes withBinder)

  -- The reproducibility property ----------------------------------------------
    -- This is what keeps every benchmark number recorded before binders
    -- existed valid: with no binder, site selection draws from exactly
    -- the list it drew from before, so 'pick' consumes the same
    -- randomness and maps draw k to index k.
  , ("unsealed indices are the whole tree when there is no binder",
      unsealedIndices noBinder == [0 .. countNodes noBinder - 1])
  , ("unsealed indices are ascending and in range",
      let is = unsealedIndices withBinder
      in and (zipWith (<) is (drop 1 is))
           && all (\i -> i >= 0 && i < countNodes withBinder) is)

  -- The operators -------------------------------------------------------------
    -- Verified by mutation: neutering 'sealedIndices' to @const []@ must
    -- fail this. If it does not, the exclusion is not actually wired into
    -- site selection and the whole mechanism is decorative.
  , ("no operator ever reaches inside a binder", operatorsRespectSealing)
    -- Guards the assertion above against passing because the operators
    -- destroyed the binder every time rather than because they respected
    -- it. Some child must actually still contain one.
  , ("some operator output still contains the binder",
      any (\k -> let s = mkSeed (fromIntegral (k :: Int))
                     (m, _) = pointMutation cfg withBinder s
                 in hasBinder m)
          [1 .. 60])

  -- Evaluation ----------------------------------------------------------------
  , ("a sum evaluates to the sum of its terms",
      eval env (Sum "i" (Const 1.0) (Const 4.0) (Var "i")) == 10.0)
  , ("the body sees the bound index",
      eval env theSum == 12.0)  -- (1 + 2 + 3) * x, x = 2
  , ("bounds are rounded, not truncated",
      eval env (Sum "i" (Const 0.6) (Const 3.4) (Var "i")) == 6.0)  -- 1..3
  , ("an inverted range is the empty sum",
      eval env (Sum "i" (Const 5.0) (Const 1.0) (Var "i")) == 0.0)
  , ("a single-term range works",
      eval env (Sum "i" (Const 7.0) (Const 7.0) (Var "i")) == 7.0)
    -- The index shadows an outer binding of the same name rather than
    -- being overwritten by it, which falls out of Env being an
    -- association list that 'lookup' reads front-first.
  , ("the index shadows an outer variable of the same name",
      eval [("i", 99.0)] (Sum "i" (Const 1.0) (Const 3.0) (Var "i")) == 6.0)
  , ("an outer variable is still visible inside the body",
      eval env (Sum "i" (Const 1.0) (Const 2.0) (Var "x")) == 4.0)
  , ("evaluation stays finite for an oversized range",
      let v = eval env oversized in not (isNaN v) && not (isInfinite v))

  -- The iteration cap ---------------------------------------------------------
    -- A capped sum is reported as invalid because it is NOT the value of
    -- the expression — distinct from Saturated, which is the right value
    -- truncated by the representation.
  , ("exceeding the iteration cap is reported",
      evalResultError (evalDomain env oversized) == Just IterationCapped)
  , ("a sum within the cap is valid",
      isValid (evalDomain env theSum))
  , ("the capped value is the first iterationCap terms",
      eval env oversized == fromIntegral iterationCap)
  , ("an invalid body propagates out of the sum",
      not (isValid (evalDomain env
             (Sum "i" (Const 1.0) (Const 3.0) (Log (Const (-1.0)))))))

  -- Checked evaluation --------------------------------------------------------
    -- The index must not be reported as unbound inside its own body.
  , ("the bound index is not an unbound variable",
      evalChecked env (Sum "i" (Const 1.0) (Const 3.0) (Var "i"))
        == Right 6.0)
  , ("a genuinely unbound name in a body is still reported",
      evalChecked env (Sum "i" (Const 1.0) (Const 3.0) (Var "nope"))
        == Left "unbound variable: nope")

  -- Free versus mentioned variables -------------------------------------------
    -- The distinction the whole binder rests on, and the one that must
    -- never be quietly conflated. See the warning on
    -- 'TypedGP.Simplify.isFoldable'.
  , ("the bound index is not free",
      freeVariables theSum == ["x"])
  , ("but it is mentioned",
      "i" `elem` variablesOf theSum)
  , ("a mention in a bound is free, because bounds are outside the scope",
      freeVariables (Sum "i" (Var "i") (Const 3.0) (Const 1.0)) == ["i"])

  -- Differentiation -----------------------------------------------------------
  , ("differentiating under the sum differentiates the body",
      differentiate "x" (Sum "i" (Const 1.0) (Const 3.0) (Mul (Var "i") (Var "x")))
        /= Nothing)
  , ("the derivative is itself a sum with the same bounds",
      case differentiate "x" theSum of
        Just (Sum j lo hi _) -> j == "i" && lo == Const 1.0 && hi == Const 3.0
        _                    -> False)
  , ("the derivative agrees with eval",
      case differentiate "x" theSum of
        Just d  -> abs (eval env (simplify d) - 6.0) < 1.0e-9  -- d/dx (6x) = 6
        Nothing -> False)
    -- The capture bug the §2 amendment predicted. d/di inside Sum i is a
    -- request about a bound index, which is meaningless rather than zero,
    -- and a rule without this check would silently treat the bound index
    -- as the free variable of interest.
  , ("differentiating by a shadowed index refuses",
      differentiate "i" theSum == Nothing)
  , ("...but differentiating by a different name does not",
      differentiate "x" theSum /= Nothing)
    -- Leibniz' rule for a variable-limit sum needs boundary terms this
    -- AST cannot express, so a bound depending on the target refuses.
  , ("a target-dependent bound refuses",
      differentiate "x" (Sum "i" (Const 1.0) (Var "x") (Var "i")) == Nothing)
  , ("a bound depending on some other variable is fine",
      differentiate "x" (Sum "i" (Const 1.0) (Var "y") (Var "x")) /= Nothing)

  -- Checkpoint ----------------------------------------------------------------
    -- The reader dispatches on operator-name strings, which -Wall cannot
    -- check, so a round-trip case is the only thing standing between a
    -- new constructor and checkpoints that write cleanly and fail to load.
  , ("a binder round-trips through the checkpoint format",
      roundTrips theSum)
  , ("a nested binder round-trips", roundTrips withBinder)
  , ("a binder with computed bounds round-trips",
      roundTrips (Sum "k" (Add (Var "x") (Const 1.0)) (Mul (Var "y") (Const 2.0))
                          (Div (Var "k") (Var "x"))))

  -- Pretty printing -----------------------------------------------------------
  , ("a binder renders with its index and bounds",
      pretty (Sum "i" (Const 1.0) (Const 3.0) (Var "i")) == "sum(i=1..3, i)")

  -- Simplification ------------------------------------------------------------
    -- A closed binder — one whose body mentions no variable at all — has
    -- an empty 'variablesOf' and so looks foldable. Folding evaluates it,
    -- and for a range past 'iterationCap' that bakes the TRUNCATED value
    -- in as a constant, erasing the 'IterationCapped' verdict that
    -- 'evalDomain' would otherwise give.
    --
    -- Note this does not violate the simplifier's stated contract, which
    -- is eval-preservation: 'eval' agrees before and after, because both
    -- are the capped number. What is lost is domain preservation, which
    -- nothing had previously needed to state.
  , ("a capped closed binder is not folded into a constant",
      case simplify cappedClosed of
        Const _ -> False
        _       -> True)
  , ("...so the iteration cap survives simplification",
      evalResultError (evalDomain env (simplify cappedClosed))
        == Just IterationCapped)
    -- The small case is safe to fold and folding it is a real reduction,
    -- so the fix must not be "never touch a binder".
  , ("a small closed binder still folds",
      simplify (Sum "i" (Const 1.0) (Const 3.0) (Const 5.0)) == Const 15.0)
  , ("simplification preserves eval on binders",
      let e = Sum "i" (Const 1.0) (Const 4.0) (Mul (Var "i") (Var "x"))
      in eval env (simplify e) == eval env e)

  -- Generation ----------------------------------------------------------------
    -- The default. Nothing can emit a binder, so no run that does not ask
    -- for one ever sees one, and every number recorded before this
    -- constructor existed still stands — structurally, not statistically.
  , ("generation produces no binder by default",
      not (any (hasBinder . fst) generatedDefault))
    -- The reproducibility guarantee, asserted rather than argued: with
    -- binders off, generation is bit-for-bit what it was. The binder
    -- branch is taken before any operator draw and only when the flag is
    -- set, so a disabled run consumes no randomness for it; and
    -- 'genTerminalIn' draws from @cfgVariables ++ scope@, which is
    -- @cfgVariables@ in the same order when the scope is empty.
  , ("disabling binders reproduces generation exactly",
      map fst generatedDefault == map fst generatedDisabled)

    -- With the flag on, binders must actually appear — otherwise every
    -- assertion below is vacuous.
  , ("enabling binders actually produces some",
      any (hasBinder . fst) generatedEnabled)
  , ("but not in every tree", any (not . hasBinder . fst) generatedEnabled)

    -- Every generated binder must be well formed in the ways the design
    -- decisions promise, checked over the whole sample rather than on one
    -- example.
  , ("generated bounds are literal constants, never subtrees",
      all literalBounds (concatMap (sumsIn . fst) generatedEnabled))
  , ("generated bodies actually use their index",
      any usesItsIndex (concatMap (sumsIn . fst) generatedEnabled))
  , ("no generated binder has a free index",
      all (\e -> "i0" `notElem` freeVariables e) (map fst generatedEnabled))
    -- The depth limit bounds nesting, not tree depth, because stacked
    -- binders multiply iteration counts.
  , ("the default depth limit permits no nesting",
      all (not . hasNestedBinder) (map fst generatedEnabled))
  , ("raising the limit permits nesting",
      any hasNestedBinder
          (map fst [ genExpr nested Grow 6 (mkSeed s) | s <- [1 .. 400] ]))

    -- Generated binders must survive the operators intact, exactly as the
    -- hand-built one does. Generation is a second source of binders and a
    -- fix that works for hand-built trees proves nothing about it.
  , ("operators respect sealing on generated binders too",
      all generatedSealingHolds (filter (hasBinder . fst) generatedEnabled))

    -- Every generated binder must evaluate finitely and round-trip, since
    -- generation is the only route by which unusual shapes reach either.
  , ("every generated tree evaluates finitely",
      all (\e -> let v = eval env e
                 in not (isNaN v) && not (isInfinite v))
          (map fst generatedEnabled))
  , ("every generated tree round-trips through the checkpoint format",
      all (roundTrips . fst) generatedEnabled)
  ]
  where
    generatedDefault, generatedDisabled, generatedEnabled :: [(Expr, Seed)]
    generatedDefault = [genExpr cfg Grow 5 (mkSeed s) | s <- [1 .. 200]]
    generatedDisabled =
      [genExpr cfg { cfgEnableBinders = False } Grow 5 (mkSeed s) | s <- [1 .. 200]]
    generatedEnabled =
      [genExpr binders Grow 5 (mkSeed s) | s <- [1 .. 200]]

    binders :: Config
    binders = cfg { cfgEnableBinders = True, cfgBinderProb = 0.3 }

    nested :: Config
    nested = binders { cfgBinderDepthLimit = 2 }

    literalBounds :: Expr -> Bool
    literalBounds (Sum _ (Const _) (Const _) _) = True
    literalBounds _                             = False

    usesItsIndex :: Expr -> Bool
    usesItsIndex (Sum i _ _ body) = i `elem` variablesOf body
    usesItsIndex _                = False

    -- A binder anywhere strictly inside another binder.
    hasNestedBinder :: Expr -> Bool
    hasNestedBinder e =
      any (\s -> case s of
                   Sum _ lo hi body -> any hasBinder [lo, hi, body]
                   _                -> False)
          (sumsIn e)

    -- The operators run with binder generation OFF even though the parent
    -- was generated with it on. That isolation is the whole point of the
    -- test: with generation enabled, 'subtreeMutation' can legitimately
    -- graft a freshly generated subtree that contains a brand-new binder,
    -- and a child containing one would fail an "every binder came from the
    -- parent" check for a reason that has nothing to do with sealing.
    --
    -- Disabling generation here means any binder in a child must have come
    -- from the parent, so "is it unchanged?" becomes a clean question —
    -- the same principle as holding expressions identical in the
    -- age-propagation fixture.
    generatedSealingHolds :: (Expr, Seed) -> Bool
    generatedSealingHolds (parent, _) =
      let originals = sumsIn parent
          intact k =
            let s = mkSeed (fromIntegral (k :: Int))
                ((a, b), _) = crossover cfg parent parent s
                (m, _) = subtreeMutation cfg parent s
                (p, _) = pointMutation cfg parent s
                (h, _) = hoistMutation parent s
            in all (\child -> all (`elem` originals) (sumsIn child)) [a, b, m, p, h]
      in all intact [1 .. 10]
