-- | Algebraic simplification by rewrite rules, applied to a fixpoint.
--
-- Two jobs, one mechanism:
--
--   * __Bloat control that is deterministic rather than statistical.__ The
--     parsimony penalty in "TypedGP.Fitness" only makes large trees
--     less likely to be selected. This removes provably redundant
--     structure outright, so @x * 1 + (y - y)@ becomes @x@ and stays that
--     way.
--   * __Readability.__ The formula a user reads should be the simplest
--     expression of what was found, not an artefact of which random
--     mutations happened to survive.
--
-- == The correctness contract
--
-- Every rule here is __eval-preserving__: for all environments @env@,
-- @eval env e == eval env (simplify e)@, /exactly/, not approximately.
-- "SimplifySpec" checks this over several hundred random expressions.
-- A simplifier that silently changes behaviour is worse than none at all,
-- so the bar for admitting a rule is a proof sketch, written in the
-- comment next to it, not an intuition that it "looks like algebra".
--
-- == Why reassociation is deliberately absent
--
-- Flattening nested operators — @Add (Add x (Const a)) (Const b)@ into
-- @Add x (Const (a + b))@ — is __not__ eval-preserving here, for two
-- independent reasons, so it is not implemented:
--
--   1. IEEE 754 addition is not associative. @(x + a) + b@ and
--      @x + (a + b)@ differ in the last bits for most inputs.
--   2. Worse, 'TypedGP.Eval.sanitize' clamps every intermediate result to
--      +/-'TypedGP.Eval.magnitudeCap'. Take @x = 1e12@, @a = 1e12@,
--      @b = -1e12@: the nested form clamps @x + a@ to @1e12@ and then adds
--      @b@ to reach @0@, while the flattened form computes @a + b = 0@ and
--      returns @1e12@. Not a rounding difference — a completely different
--      answer.
--
-- Reason 2 is specific to this engine and easy to miss, which is exactly
-- why it is written down. The sound part of flattening — collapsing a nest
-- that contains no variables at all — is covered by constant folding below.
--
-- == Every rule here depends on "TypedGP.Eval", and that is a live hazard
--
-- These rules are not algebra. They are claims about what
-- 'TypedGP.Eval.eval' computes, and a change to a protected primitive can
-- invalidate one without touching this file. That is not hypothetical: the
-- @x ^ 1 -> x@ rule was sound, and argued sound in its own comment from
-- 'TypedGP.Eval.protectedPow's sign-keeping branch — until that branch was
-- removed when @Pow@ was redefined as @|base| ** expo@. The rule then
-- turned @+3@ into @-3@ at @x = -3@.
--
-- __The random property test did not catch that, and the reason is worth
-- knowing.__ Every structural rule here is guarded by an exact comparison
-- against a specific constant, and the generator draws constants from a
-- continuous range — so the probability of it ever emitting the
-- @Const 1.0@ that triggers this rule is zero in practice. Several hundred
-- random trees proved almost nothing about the rules. Two hand-written
-- assertions caught the bug; the property sailed through.
--
-- "SimplifySpec" now also checks a __critical population__: rule trigger
-- shapes instantiated at every boundary constant, evaluated at
-- environments that put variables on zero and on negatives. That is the
-- part that will fail next time a protected primitive changes.
--
-- Its coverage is __asserted, not assumed__. @ruleTriggers@ there lists one
-- representative trigger per rule below, and an assertion checks that the
-- population actually reduces each one — because a first version of that
-- population silently missed the three commuted rules (@Add (Const 0) b@,
-- @Mul (Const 1) b@, @Mul (Const 0) _@) while being described as covering
-- everything. Prose coverage claims are not checkable; that assertion is.
--
-- Verified by mutation: restoring the unsound @x ^ 1 -> x@ rule fails five
-- assertions, three of them property-level. Before the critical population
-- existed the same mutation failed zero property assertions.
--
-- __So: any change to a protected primitive in "TypedGP.Eval" requires
-- re-checking every rule below against the new definition.__ The critical
-- population is the safety net; a rule added here without a matching
-- trigger shape there is outside it.
module TypedGP.Simplify
  ( simplify
  , simplifyOnce
  , rewriteNode
  , isFoldable
  , maxPasses
  ) where

import TypedGP.Eval
  ( DomainError (Saturated)
  , eval
  , evalDomain
  , evalResultError
  )
import TypedGP.Expr (Expr (..), mapChildren, provablyNonNegative, variablesOf)

-- | Cap on rewrite passes.
--
-- Not needed for termination: every rule either shrinks the tree or leaves
-- it identical, and 'simplify' stops as soon as a pass changes nothing, so
-- the process is well-founded on tree size. The cap is insurance against a
-- future rule that violates that property — it turns an infinite loop into
-- a merely incomplete simplification.
maxPasses :: Int
maxPasses = 100

-- | Rewrite to a fixpoint.
simplify :: Expr -> Expr
simplify = go maxPasses
  where
    go :: Int -> Expr -> Expr
    go 0 expr = expr
    go n expr =
      let expr' = simplifyOnce expr
      in if expr' == expr then expr else go (n - 1) expr'

-- | One bottom-up pass: simplify the children, then rewrite the node.
--
-- Bottom-up matters. Rewriting children first means a rule at this node
-- sees them in their already-reduced form, so @(x - x) * 1@ collapses in a
-- single pass: the inner @x - x@ becomes @Const 0@, and only then does the
-- @* 1@ rule fire against it.
simplifyOnce :: Expr -> Expr
simplifyOnce expr = rewriteNode (mapChildren simplifyOnce expr)

-- | Apply the first matching rule at this node, assuming its children are
-- already simplified.
--
-- Unlike most matches on 'Expr' in this package, this one ends in a
-- catch-all rather than being exhaustive, so @-Wall@ will /not/ flag it
-- when a constructor is added. That is intentional and safe: a new
-- operator simply has no rules yet, which is a missed optimisation, never
-- a wrong answer. The recursion into children stays exhaustive because it
-- goes through 'mapChildren', which @-Wall@ does flag.
rewriteNode :: Expr -> Expr
rewriteNode expr
  -- Constant folding. Sound for a reason worth stating precisely: if a
  -- subtree contains no variables then 'eval' gives the same answer for
  -- every environment, so the empty environment is as good as any. This
  -- also subsumes the sound part of operator flattening — an all-constant
  -- nest of any depth collapses to one literal in a single step.
  --
  -- Folding to 'eval' rather than to hand-written arithmetic is what keeps
  -- it honest: the replacement is computed by the very interpreter the
  -- result must agree with, including its protected division and clamping.
  | isFoldable expr = Const (eval [] expr)
  | otherwise = case expr of
      -- Additive and multiplicative identities. Sound because 'eval'
      -- always returns a finite value, so @v + 0@, @v - 0@, @v * 1@ and
      -- @v / 1@ are all exactly @v@ with no special cases.
      Add a (Const c) | c == 0 -> a
      Add (Const c) b | c == 0 -> b
      Sub a (Const c) | c == 0 -> a
      Mul a (Const c) | c == 1 -> a
      Mul (Const c) b | c == 1 -> b
      Div a (Const c) | c == 1 -> a

      -- Multiplication by zero. Sound for the same finiteness reason:
      -- @v * 0@ cannot be NaN, which it could if @v@ were an infinity.
      -- (It may be @-0.0@ for negative @v@, which compares equal to @0@.)
      Mul _ (Const c) | c == 0 -> Const 0
      Mul (Const c) _ | c == 0 -> Const 0

      -- Exponentiation identities. All three follow directly from the
      -- rules in 'TypedGP.Eval.protectedPow', and each has to be checked
      -- against that definition rather than against school algebra:
      --
      --   * @x ^ 1@: 'protectedPow' means @|base| ** expo@, so this is
      --     @|x|@ — __not__ @x@. Rewriting it to @x@ was sound under the
      --     older semantics, which kept the sign for integer exponents,
      --     and became unsound the moment that branch was removed. It is
      --     only @x@ when the base cannot be negative in the first place.
      --   * @x ^ 0@: the very first rule of protectedPow returns 1
      --     unconditionally, including for a zero or negative base.
      --   * @1 ^ x@: base 1 is non-negative, and @1 ** e@ is 1 for every
      --     finite @e@ (which is all 'eval' can produce).
      --
      -- Notably absent: @0 ^ x -> 0@. It is false here, because a negative
      -- exponent takes the pole branch and yields the sentinel 1.
      --
      -- The @x ^ 1@ case is the reason these have to be checked against
      -- 'protectedPow' rather than against school algebra: the identity
      -- that looks most obviously safe is the one that broke.
      Pow a (Const c) | c == 1 -> if provablyNonNegative a then a else Abs a
      Pow _ (Const c) | c == 0 -> Const 1
      Pow (Const b) _ | b == 1 -> Const 1

      -- Self-cancellation. 'Expr' is pure and 'eval' deterministic, so
      -- both occurrences of a structurally identical subtree produce the
      -- same value, and @v - v@ is exactly 0.
      Sub a b | a == b -> Const 0

      -- @v / v@ is exactly 1 for every finite @v@ including zero: the
      -- near-zero branch of 'TypedGP.Eval.protectedDiv' returns 1.0 by
      -- definition, and every other value divides into itself to give 1.
      -- This rule is only sound because of that protection.
      Div a b | a == b -> Const 1

      _ -> expr

-- | Is this a non-leaf subtree with no variables, and therefore a constant?
--
-- Leaves are excluded so that 'simplify' leaves an ordinary @Const@
-- untouched instead of rewriting it to itself.
--
-- == Two conditions, and the second is not about variables at all
--
-- __Correction, 2026-09-04.__ An earlier version of this note claimed the
-- variable test made binders safe by accident, because a bound index would
-- be counted and so a binder would never look foldable. That is false for
-- the case that matters: a binder whose body mentions /nothing/ —
-- @Sum i 1 1000000 (Const 5)@ — contains no @Var@ node at all, has an
-- empty 'TypedGP.Expr.variablesOf', and folds.
--
-- Chasing that turned up a __pre-existing__ problem with no connection to
-- binders. Folding replaces a subtree with @Const (eval [] subtree)@, and
-- @eval@ is the /protected/ interpreter. So folding
-- @Div (Const 1) (Const 0)@ produced @Const 1.0@ — the protected sentinel,
-- baked in as an ordinary constant, with the 'TypedGP.Eval.DividedByZero'
-- that 'TypedGP.Eval.evalDomain' would have reported now unrecoverable.
-- Likewise @Log (Const (-1))@, and any capped @Sum@.
--
-- That is eval-preserving, so it never violated the contract stated above.
-- It breaks a different guarantee, introduced later and never restated
-- here: "TypedGP.Differentiate" tells callers to run a derivative through
-- 'simplify', and says 'evalDomain' on the result is what reports where
-- the derivative is valid. Folding was quietly erasing exactly that.
--
-- So folding now requires __both__:
--
--   1. no variables, so the subtree has one value for every environment; and
--   2. the subtree is domain-clean, so the constant that replaces it means
--      the same thing to 'evalDomain' as the subtree did.
--
-- 'TypedGP.Eval.Saturated' is excluded from (2), consistently with the §5
-- amendment in @docs\/phase2-design.md@: a saturated value is the right
-- value truncated by the representation, so folding it loses nothing. The
-- other conditions all mean "this is not the value of the expression".
isFoldable :: Expr -> Bool
isFoldable expr = case expr of
  Const _ -> False
  Var _   -> False
  _       -> null (variablesOf expr) && domainClean expr

-- | Would replacing this subtree with its value preserve what
-- 'TypedGP.Eval.evalDomain' says about it?
--
-- Only called on subtrees already known to contain no variables, so the
-- empty environment is the only one there is.
--
-- 'Saturated' is deliberately treated as clean: it means the value is
-- right and the representation ran out, so a constant carrying that value
-- says the same thing. Every other condition means the value is a
-- protected substitute rather than the expression's own, and folding it
-- would make that unrecoverable.
domainClean :: Expr -> Bool
domainClean expr = case evalResultError (evalDomain [] expr) of
  Nothing        -> True
  Just Saturated -> True
  Just _         -> False
