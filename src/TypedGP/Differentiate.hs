-- | Symbolic partial differentiation.
--
-- Produces an 'Expr', not a number. That is the whole point: a
-- finite-difference routine gives a function you can sample, while this
-- gives a formula you can simplify, print, compare against derivative
-- observations, or check for dimensional consistency. Every downstream use
-- in the roadmap wants the formula.
--
-- == Why 'Maybe'
--
-- @Nothing@ means __"not differentiable by these rules"__, never "the
-- derivative is zero". Only 'Gamma' and 'Zeta' produce it: their
-- derivatives are the digamma function and zeta-prime, neither of which is
-- in this AST, and approximating them numerically inside a symbolic
-- differentiator would smuggle in the numeric approach with none of its
-- honesty. A differentiator that refuses on @zeta@ is more useful than one
-- that lies about it — the caller can skip the individual, fall back to a
-- finite difference for it, or drop @zeta@ from the operator set.
--
-- == Where the result is valid
--
-- Not tracked by a parallel mechanism, because it does not need to be. The
-- derivative is an 'Expr', so 'TypedGP.Eval.evalDomain' applied to /it/
-- reports exactly where it is meaningful: a quotient rule's @b = 0@ shows
-- up as 'TypedGP.Eval.DividedByZero' from the @Div@ the rule emitted.
--
-- One caveat when asking that question: __'TypedGP.Eval.Saturated' must be
-- excluded__. It is a numeric-range condition, not a domain condition (see
-- the amendment in @docs\/phase2-design.md@ §5) — a derivative that merely
-- got large is still the right derivative.
--
-- == The protected-operator problem, stated plainly
--
-- The chain rule does not apply to 'TypedGP.Eval.eval''s protected
-- operators near their guards. @d\/dx (a\/b)@ is the quotient rule only
-- where the quotient rule holds; @eval@ computes something with a step in
-- it, and the two disagree near @b = 0@ — precisely where evolved
-- individuals congregate, because protection makes that region survivable.
--
-- These rules are therefore correct for the /mathematical/ functions the
-- operators denote away from their guards, and 'evalDomain' on the result
-- is what says where that is. "DifferentiateSpec" checks them against
-- central differences at points chosen to be away from every guard, which
-- is the only place the comparison is meaningful.
module TypedGP.Differentiate
  ( differentiate
  , differentiable
  ) where

import TypedGP.Expr (Expr (..), VarName, freeVariables)

-- | Partial derivative with respect to one variable.
--
-- The result is unsimplified; run it through 'TypedGP.Simplify.simplify',
-- which collapses the large amount of @* 1@ and @+ 0@ the chain rule
-- generates.
differentiate :: VarName -> Expr -> Maybe Expr
differentiate target = go
  where
    go :: Expr -> Maybe Expr
    go expr = case expr of
      Const _ -> Just (Const 0.0)
      Var v   -> Just (Const (if v == target then 1.0 else 0.0))

      -- Exact. No protection is involved in any of these.
      Add a b -> Add <$> go a <*> go b
      Sub a b -> Sub <$> go a <*> go b
      Mul a b -> do
        a' <- go a
        b' <- go b
        Just (Add (Mul a' b) (Mul a b'))

      -- Quotient rule. Valid where |b| >= divisionEpsilon; outside that
      -- 'protectedDiv' returns its sentinel and the true function has a
      -- step, which no derivative describes. 'evalDomain' on this result
      -- reports the b = 0 points.
      Div a b -> do
        a' <- go a
        b' <- go b
        Just (Div (Sub (Mul a' b) (Mul a b')) (Mul b b))

      -- Total and smooth on the reals, so these are unconditionally exact.
      Sin a  -> chain (Cos a) a
      Cos a  -> chain (Sub (Const 0.0) (Sin a)) a
      Exp a  -> chain (Exp a) a
      Tanh a -> chain (Sub (Const 1.0) (Mul (Tanh a) (Tanh a))) a

      -- 'protectedLog' is @log |x|@, whose derivative is @1/x@ — the same
      -- expression for both signs, which is why no case split is needed.
      Log a -> do
        a' <- go a
        Just (Div a' a)

      -- 'protectedSqrt' is @sqrt |x|@, which is @|x| ** 0.5@ — exactly the
      -- constant-exponent power case, so it shares that rule rather than
      -- getting a second derivation that could drift out of step.
      Sqrt a -> powerRule a 0.5

      -- @d/dx |u| = sign(u) * u'@, and @sign(u)@ is @u / |u|@. Undefined
      -- at 0, where 'protectedDiv' substitutes its sentinel.
      Abs a -> chain (Div a (Abs a)) a

      Pow f g -> powRule f g

      -- Differentiating under the summation sign, with two conditions
      -- that both have to be checked rather than assumed.
      --
      -- 1. __The bounds must not depend on the target.__ If they do, the
      --    number of terms varies with x and d/dx is not the sum of the
      --    term derivatives; Leibniz' rule for a variable-limit sum needs
      --    boundary terms this AST cannot express. Refuse.
      -- 2. __The target must not be shadowed by this binder.__ Asking for
      --    d/di inside @Sum i@ is not a derivative with respect to the
      --    outer i; it is a request about a bound index, which is
      --    meaningless rather than zero. Without this check the rule
      --    would silently treat the bound index as the free variable of
      --    interest — the capture bug predicted in the 2026-09-04
      --    amendment to §2 of docs/phase2-design.md.
      Sum i lo hi body
        | i == target -> Nothing
        | target `elem` freeVariables lo -> Nothing
        | target `elem` freeVariables hi -> Nothing
        | otherwise -> Sum i lo hi <$> go body

      -- The digamma function and zeta-prime are not in this AST. Refusing
      -- is the honest answer; see the module header.
      Gamma _ -> Nothing
      Zeta _  -> Nothing

    -- | @d/dx outer(inner) = outer' * inner'@, where @outer'@ is supplied
    -- already differentiated with respect to its own argument.
    chain :: Expr -> Expr -> Maybe Expr
    chain outerDerivative inner = Mul outerDerivative <$> go inner

    -- | The three 'Pow' cases. See the amendment to
    -- @docs\/phase2-design.md@ §1, which supersedes the rules stated there.
    powRule :: Expr -> Expr -> Maybe Expr
    powRule f g = case (f, g) of
      -- Constant exponent: the common shape, and the only one with no
      -- domain condition beyond f /= 0.
      (_, Const c) -> powerRule f c

      -- Constant base. @|a| ** g@ is fine for every @a@ except zero, and
      -- @Log (Const a)@ is @log |a|@ — precisely the factor the rule needs.
      -- Under the pre-2026-08-19 semantics this required @a > 0@; taking
      -- the magnitude is what loosened it.
      (Const a, _)
        | a /= 0.0 -> do
            g' <- go g
            Just (Mul (Mul (Pow f g) (Log f)) g')
        | otherwise -> Nothing

      -- Both symbolic. @|f| ** g = exp (g * log |f|)@ for @f /= 0@, and
      -- again 'Log' is already the log-of-magnitude the derivation wants.
      -- This case returned 'Nothing' under the old semantics because it
      -- needed @log f@ with @f@ possibly negative.
      _ -> do
        f' <- go f
        g' <- go g
        Just (Mul (Pow f g)
                  (Add (Mul g' (Log f)) (Mul g (Div f' f))))

    -- | @d/dx |f| ** c = c * f * |f| ** (c - 2) * f'@.
    --
    -- The textbook form is @c * |f| ** (c-1) * sign(f) * f'@. This is the
    -- same thing, using @|f| ** (c-1) * sign(f) = f * |f| ** (c-2)@, which
    -- avoids needing a @signum@ operator the AST does not have.
    --
    -- @Pow f (Const (c - 2))@ /is/ @|f| ** (c-2)@ under current semantics,
    -- so no @Abs@ node is needed either.
    powerRule :: Expr -> Double -> Maybe Expr
    powerRule f c = do
      f' <- go f
      Just (Mul (Mul (Const c) (Mul f (Pow f (Const (c - 2.0))))) f')

-- | Can this expression be differentiated at all?
--
-- __Defined in terms of 'differentiate' rather than by a parallel case
-- analysis__, deliberately. A hand-written mirror would be faster and
-- would drift: the refusal conditions are not simply "contains 'Gamma' or
-- 'Zeta'" — @Pow (Const 0) g@ also refuses for a symbolic @g@ but /not/
-- for a constant one, and a second copy of that subtlety is a bug waiting
-- for the next rule change. Laziness keeps this cheap: only the 'Maybe'
-- constructor is forced, never the derivative expression inside it.
--
-- Independent of the variable. Which variable is chosen changes whether a
-- 'Var' differentiates to @1@ or @0@, never whether it differentiates.
differentiable :: Expr -> Bool
differentiable expr = case differentiate "" expr of
  Just _  -> True
  Nothing -> False
