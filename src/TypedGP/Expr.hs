-- | The expression AST at the heart of the engine, plus the structural
-- helpers ('countNodes', 'subtreeAt', 'replaceAt', ...) that the genetic
-- operators are built from.
--
-- This module knows nothing about randomness, fitness or evolution. It is
-- deliberately the only place where the shape of a program is defined.
module TypedGP.Expr
  ( -- * The AST
    Expr (..)
  , VarName

    -- * Operator registry
  , BinaryOp (..)
  , UnaryOp (..)
  , binaryOps
  , unaryOps

    -- * Structural queries
  , childrenOf
  , mapChildren
  , opName
  , arity
  , isTerminal
  , countNodes
  , depth
  , flatten
  , nodeDepths
  , nodeDepthAt
  , subtreeAt
  , replaceAt
  , variablesOf
  , freeVariables
  , sealedIndices
  , unsealedIndices
  , hasBinder
  , provablyNonNegative
  , isEvenIntegerConst
  , isNonIntegerConst
  ) where

import Data.List (nub)

-- | Variable names are plain strings so a dataset can name its columns
-- freely. Kept as a type synonym rather than a newtype because it is used
-- as a bare key in 'TypedGP.Eval.Env' association lists.
type VarName = String

-- | A symbolic-regression program.
--
-- Constructors are deliberately __flat__: one constructor per operator, no
-- nested @data Op = ...@ wrapper. That keeps @-Wall@'s incomplete-pattern
-- check maximally informative — adding a constructor here makes GHC point
-- at every single site that must be updated ('eval', 'prettyPrec',
-- 'childrenOf', 'opName', 'arity', and the point-mutation dispatcher).
-- Treat that warning as the refactoring safety net; never disable it.
--
-- Adding an operator is therefore mechanical:
--
--   1. add the constructor here;
--   2. add it to 'binaryOps' or 'unaryOps' below, __with a weight__ (the
--      one manual step the compiler cannot flag for you — these lists
--      drive random generation and point mutation);
--   3. compile with @-Wall -Werror@ and fix every reported match site.
--
-- As of the special-function set the recipe has one addition worth calling
-- out: step 2 now requires choosing a __generation weight__, and the
-- choice matters. Operators are picked in proportion to weight, so adding
-- an operator at weight 1.0 does not merely make it available — it takes
-- probability away from every other operator. Nine unary operators picked
-- uniformly alongside four binary ones would produce trees that are mostly
-- nested unary chains. See 'unaryOps' for the weights actually used and
-- why.
--
-- The match sites the compiler will point you at, as of this version:
-- 'childrenOf', 'mapChildren', 'opName', 'replaceAt' here;
-- 'TypedGP.Eval.eval' and 'TypedGP.Eval.evalChecked';
-- 'TypedGP.Pretty.render'; 'TypedGP.Ops.Mutation.mutateNode';
-- 'TypedGP.Uncertainty.constantsOf' and 'TypedGP.Uncertainty.withConstants';
-- 'TypedGP.Checkpoint.renderExprS' (and its parser, which is /not/
-- compiler-checked — it matches on operator names, so a new operator
-- silently fails to parse until its name is added).
--
-- == Intended GADT upgrade path
--
-- Today every node has type @Double@, so the AST needs no type index. The
-- planned extension is a typed @Expr a@ carrying both @Double@ and @Bool@
-- results, which is what unlocks conditionals and comparisons:
--
-- > {-# LANGUAGE GADTs, KindSignatures #-}
-- > data Expr :: * -> * where
-- >   Const :: Double -> Expr Double
-- >   Var   :: VarName -> Expr Double
-- >   Add   :: Expr Double -> Expr Double -> Expr Double
-- >   Lt    :: Expr Double -> Expr Double -> Expr Bool
-- >   And   :: Expr Bool   -> Expr Bool   -> Expr Bool
-- >   If    :: Expr Bool -> Expr a -> Expr a -> Expr a
--
-- The consequences to plan for, so a future session does not re-derive
-- this design:
--
--   * @eval@ becomes @Env -> Expr a -> a@ and stays total; the @Div@ and
--     @Log@ protections in "TypedGP.Eval" are unchanged.
--   * @deriving (Eq, Show)@ stops working; hand-write a heterogeneous
--     @eqExpr :: Expr a -> Expr b -> Bool@ instead.
--   * The genetic operators are the real work: 'subtreeAt' / 'replaceAt'
--     must become type-aware, because a @Expr Bool@ subtree may only be
--     swapped with another @Expr Bool@. The standard solution is an
--     existential @data SomeExpr where SomeExpr :: TypeRep a -> Expr a ->
--     SomeExpr@ plus a runtime type witness, so crossover can filter
--     candidate points to those whose witnesses match. Budget that work
--     for the operators, not for the AST itself.
data Expr
  = Const !Double
  | Var !VarName
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Sin Expr
  | Cos Expr
  | Exp Expr
  | Log Expr
  | Sqrt Expr
  | Tanh Expr
  | Abs Expr
  | Gamma Expr
  | Zeta Expr
  | Sum !VarName Expr Expr Expr
    -- ^ @Sum i lo hi body@ — the index name, the lower and upper bounds,
    -- and the summand.
    --
    -- __The only constructor that binds a name__, and therefore the only
    -- one where a subtree means different things in different places.
    -- @lo@ and @hi@ are evaluated in the enclosing scope; @body@ is
    -- evaluated once per iteration with @i@ bound.
    --
    -- That makes it the only constructor the genetic operators cannot
    -- treat uniformly: lifting a subtree out of @body@ frees a bound @i@,
    -- and grafting one in captures a free @i@. Neither crashes, because
    -- 'TypedGP.Eval.eval' reads an unbound variable as @0@ — which is
    -- worse than crashing, since the expression silently becomes a
    -- different one. 'sealedIndices' is the defence; see §2 of
    -- @docs\/phase2-design.md@ and @docs\/phase6-binders-design.md@.
  deriving (Eq, Show)

-- | A binary operator, packaged so random generation and point mutation
-- can pick one without pattern-matching on 'Expr'.
data BinaryOp = BinaryOp
  { binaryOpName  :: !String
  , binaryOpWeight :: !Double
  , binaryOpBuild :: Expr -> Expr -> Expr
  }

-- | A unary operator. See 'BinaryOp'.
data UnaryOp = UnaryOp
  { unaryOpName  :: !String
  , unaryOpWeight :: !Double
  , unaryOpBuild :: Expr -> Expr
  }

-- | Every binary operator available to the search.
--
-- Step 2 of the "adding an operator" recipe above. This list is the single
-- place the compiler cannot check for you, so keep it adjacent to the
-- 'Expr' definition.
--
-- The four arithmetic operators carry equal weight and together outweigh
-- the whole unary set, which is what keeps generated trees branching
-- rather than degenerating into chains.
binaryOps :: [BinaryOp]
binaryOps =
  [ BinaryOp "+" 1.0 Add
  , BinaryOp "-" 1.0 Sub
  , BinaryOp "*" 1.0 Mul
  , BinaryOp "/" 1.0 Div
  -- Exponentiation earns its place ahead of any number of exotic unary
  -- functions: squares, cubes and square roots are the backbone of
  -- physical law, and before this constructor existed the only route to
  -- @x^3@ was @x * x * x@ — three nodes and two chances for crossover to
  -- break it, versus one node that mutation can retune continuously.
  --
  -- Weighted slightly below the arithmetic four because nested powers
  -- (@x^(y^z)@) blow up numerically far faster than nested sums, and
  -- because most of its value is concentrated in the shallow
  -- @variable ^ constant@ shape.
  , BinaryOp "^" 0.8 Pow
  ]

-- | Every unary operator available to the search. See 'binaryOps'.
--
-- == On the weights
--
-- These are registry data, not tunable hyperparameters, which is why they
-- live here beside the operators rather than in "TypedGP.Config" — they
-- describe the shape of the language, and changing one changes what the
-- search /is/ rather than how hard it looks.
--
-- Three tiers, for two different reasons:
--
--   * @sin@ and @cos@ at 0.6 — the original pair, still the most likely
--     to appear in a physical law.
--   * @exp@, @log@, @sqrt@, @tanh@, @abs@ at 0.4 — common in real models,
--     but each one added to the set dilutes the others, so they are
--     individually rarer than the trigonometric pair.
--   * @gamma@ and @zeta@ at 0.15 — deliberately rare. Not because they
--     are unimportant, but because they are the only operators here whose
--     evaluation is a numerical series rather than a machine instruction.
--     A tree that happens to nest three of them costs orders of magnitude
--     more per fitness evaluation than one made of arithmetic, and fitness
--     evaluation is the engine's entire inner loop.
--
-- Total unary weight is 3.5 against 4.0 binary, so roughly 53% of internal
-- nodes branch. Before the special functions the split was 4 against 1.2;
-- picking uniformly instead would now make it 4 against 9 and the trees
-- would be unrecognisable.
unaryOps :: [UnaryOp]
unaryOps =
  [ UnaryOp "sin"   0.60 Sin
  , UnaryOp "cos"   0.60 Cos
  , UnaryOp "exp"   0.40 Exp
  , UnaryOp "log"   0.40 Log
  , UnaryOp "sqrt"  0.40 Sqrt
  , UnaryOp "tanh"  0.40 Tanh
  , UnaryOp "abs"   0.40 Abs
  , UnaryOp "gamma" 0.15 Gamma
  , UnaryOp "zeta"  0.15 Zeta
  ]

-- | Immediate children, in evaluation order.
--
-- Generic traversals ('flatten', 'nodeDepths') are written in terms of this
-- so that they need no update when a constructor is added — only this one
-- match has to change.
childrenOf :: Expr -> [Expr]
childrenOf expr = case expr of
  Const _ -> []
  Var _   -> []
  Add a b -> [a, b]
  Sub a b -> [a, b]
  Mul a b -> [a, b]
  Div a b -> [a, b]
  Pow a b -> [a, b]
  Sin a   -> [a]
  Cos a   -> [a]
  Exp a   -> [a]
  Log a   -> [a]
  Sqrt a  -> [a]
  Tanh a  -> [a]
  Abs a   -> [a]
  Gamma a -> [a]
  Zeta a  -> [a]
  -- Bounds and body alike. Sealing keeps the operators out of all
  -- three; traversals still need to see them.
  Sum _ lo hi body -> [lo, hi, body]

-- | Rebuild a node with a function applied to each immediate child.
--
-- The structural half of any rewrite pass. "TypedGP.Simplify" recurses
-- with this rather than matching constructors itself, so that adding an
-- operator to 'Expr' makes @-Wall@ flag exactly one place — here — and the
-- new constructor's children keep getting visited for free. A rewrite
-- engine that matched constructors directly would silently stop recursing
-- into anything new.
mapChildren :: (Expr -> Expr) -> Expr -> Expr
mapChildren f expr = case expr of
  Const c -> Const c
  Var v   -> Var v
  Add a b -> Add (f a) (f b)
  Sub a b -> Sub (f a) (f b)
  Mul a b -> Mul (f a) (f b)
  Div a b -> Div (f a) (f b)
  Pow a b -> Pow (f a) (f b)
  Sin a   -> Sin (f a)
  Cos a   -> Cos (f a)
  Exp a   -> Exp (f a)
  Log a   -> Log (f a)
  Sqrt a  -> Sqrt (f a)
  Tanh a  -> Tanh (f a)
  Abs a   -> Abs (f a)
  Gamma a -> Gamma (f a)
  Zeta a  -> Zeta (f a)
  -- The index name is not an Expr and is carried through untouched. A
  -- rewrite that renamed it here would alpha-rename the binder without
  -- renaming the bound occurrences in the body.
  Sum i lo hi body -> Sum i (f lo) (f hi) (f body)

-- | Symbolic tag for a node's operator. Terminals report their kind rather
-- than their payload so that two different constants compare equal here.
-- Used by point mutation to avoid replacing an operator with itself.
opName :: Expr -> String
opName expr = case expr of
  Const _ -> "const"
  Var _   -> "var"
  Add _ _ -> "+"
  Sub _ _ -> "-"
  Mul _ _ -> "*"
  Div _ _ -> "/"
  Pow _ _ -> "^"
  Sin _   -> "sin"
  Cos _   -> "cos"
  Exp _   -> "exp"
  Log _   -> "log"
  Sqrt _  -> "sqrt"
  Tanh _  -> "tanh"
  Abs _   -> "abs"
  Gamma _ -> "gamma"
  Zeta _  -> "zeta"
  -- Deliberately ignores the index name, like every other payload here:
  -- point mutation uses this to avoid replacing an operator with itself,
  -- and two sums differing only in index name are the same operator.
  Sum {} -> "sum"

-- | Number of immediate children.
arity :: Expr -> Int
arity = length . childrenOf

-- | Is this a leaf (a constant or a variable)?
isTerminal :: Expr -> Bool
isTerminal = null . childrenOf

-- | Total number of nodes, counting the root.  Always @>= 1@.
countNodes :: Expr -> Int
countNodes expr = 1 + sum (map countNodes (childrenOf expr))

-- | Depth measured in edges: a bare terminal has depth @0@.
--
-- This convention matches "TypedGP.Gen.Grow", where a depth budget of @0@
-- means "emit a terminal", so a full tree generated with budget @d@ has
-- @depth == d@ exactly.
depth :: Expr -> Int
depth expr = case childrenOf expr of
  [] -> 0
  cs -> 1 + maximum (map depth cs)

-- | All nodes in pre-order. Index @0@ is the root.
--
-- Pre-order is the addressing scheme used by 'subtreeAt' and 'replaceAt';
-- every genetic operator addresses nodes by this index.
flatten :: Expr -> [Expr]
flatten expr = expr : concatMap flatten (childrenOf expr)

-- | Depth of every node, in the same pre-order as 'flatten'. The root is
-- at depth @0@.
nodeDepths :: Expr -> [Int]
nodeDepths = go 0
  where
    go :: Int -> Expr -> [Int]
    go d expr = d : concatMap (go (d + 1)) (childrenOf expr)

-- | Depth of the node at a given pre-order index, or 'Nothing' if the
-- index is out of range.
nodeDepthAt :: Int -> Expr -> Maybe Int
nodeDepthAt i expr = safeIndex i (nodeDepths expr)

-- | The subtree rooted at a pre-order index, or 'Nothing' when out of
-- range. Index @0@ returns the whole tree.
subtreeAt :: Int -> Expr -> Maybe Expr
subtreeAt i expr = safeIndex i (flatten expr)

-- | @replaceAt i new old@ substitutes @new@ for the subtree at pre-order
-- index @i@ of @old@. 'Nothing' when the index is out of range, so callers
-- never have to trust an index they did not derive from 'countNodes'.
replaceAt :: Int -> Expr -> Expr -> Maybe Expr
replaceAt idx new = go idx
  where
    go :: Int -> Expr -> Maybe Expr
    go k expr
      | k < 0     = Nothing
      | k == 0    = Just new
      | otherwise = case expr of
          -- k > 0 at a leaf means the index ran past this subtree.
          Const _ -> Nothing
          Var _   -> Nothing
          Add a b -> bin Add a b
          Sub a b -> bin Sub a b
          Mul a b -> bin Mul a b
          Div a b -> bin Div a b
          Pow a b -> bin Pow a b
          Sin a   -> un Sin a
          Cos a   -> un Cos a
          Exp a   -> un Exp a
          Log a   -> un Log a
          Sqrt a  -> un Sqrt a
          Tanh a  -> un Tanh a
          Abs a   -> un Abs a
          Gamma a -> un Gamma a
          Zeta a  -> un Zeta a
          -- Structurally uniform with the others: 'replaceAt' is the raw
          -- mechanism and does not enforce scope. Keeping operators out of
          -- a binder is 'sealedIndices'' job, at the point where candidate
          -- sites are chosen — not here, where a legitimate caller
          -- (checkpoint loading, a test) must still be able to rebuild any
          -- position.
          Sum i lo hi body -> ter (Sum i) lo hi body
      where
        un :: (Expr -> Expr) -> Expr -> Maybe Expr
        un build a = build <$> go (k - 1) a

        -- Pre-order layout of a three-child node is
        -- [self, first..., second..., third...].
        ter
          :: (Expr -> Expr -> Expr -> Expr)
          -> Expr -> Expr -> Expr -> Maybe Expr
        ter build a b c =
          let sizeA = countNodes a
              sizeB = countNodes b
              offset = k - 1
          in if offset < sizeA
               then (\a' -> build a' b c) <$> go offset a
               else if offset < sizeA + sizeB
                 then (\b' -> build a b' c) <$> go (offset - sizeA) b
                 else (\c' -> build a b c') <$> go (offset - sizeA - sizeB) c

        -- Pre-order layout of a binary node is [self, left..., right...],
        -- so the right child starts at 1 + size of the left child.
        bin :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Maybe Expr
        bin build a b =
          let leftSize = countNodes a
          in if k - 1 < leftSize
               then (\a' -> build a' b) <$> go (k - 1) a
               else (\b' -> build a b') <$> go (k - 1 - leftSize) b

-- | Distinct variable names occurring in an expression, in first-use order.
variablesOf :: Expr -> [VarName]
variablesOf expr = nub [v | Var v <- flatten expr]

-- | Total list indexing. Kept private: callers outside this module should
-- use 'subtreeAt' / 'nodeDepthAt', which document their index space.
safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs
  | n < 0     = Nothing
  | otherwise = case drop n xs of
      (x : _) -> Just x
      []      -> Nothing

-- | Is this expression non-negative for every input, by inspection alone?
--
-- __Conservative and one-sided.__ 'False' means "not provably non-negative",
-- never "provably negative". The only guarantee is that 'True' is correct.
--
-- Exists because 'TypedGP.Eval.protectedPow' silently takes the magnitude of
-- a negative base, so @sin(x) ^ 1.1@ computes @|sin x| ^ 1.1@ — a perfectly
-- ordinary rectified-sine shape, rendered in notation that makes it look
-- complex-valued. Knowing when the base /cannot/ be negative is what lets
-- the pretty-printer show the absolute value only where it is doing real
-- work, instead of wrapping every base in @abs@ and making output worse.
provablyNonNegative :: Expr -> Bool
provablyNonNegative expr = case expr of
  Const c -> c >= 0
  Abs _   -> True
  Exp _   -> True
  Sqrt _  -> True
  -- Squaring, in either of the two forms the search produces it.
  Mul a b -> a == b
  Pow a b -> provablyNonNegative a || isEvenIntegerConst b
  -- Everything else is either genuinely sign-varying (Var, Sin, Add, ...)
  -- or would need range analysis to settle. Both answer 'False' here, which
  -- is the safe direction.
  _ -> False

-- | An exponent that is literally an even integer, making the result
-- non-negative whatever the base does.
isEvenIntegerConst :: Expr -> Bool
isEvenIntegerConst (Pow _ _) = False
isEvenIntegerConst (Const c) =
  -- Guarded against the range where every Double is already an integer and
  -- 'round' would be both meaningless and an overflow route.
  abs c < 9.007199254740992e15
    && c == fromIntegral (round c :: Integer)
    && even (round c :: Integer)
isEvenIntegerConst _ = False

-- | An exponent that is a literal non-integer, so a negative base cannot
-- give a real result and 'TypedGP.Eval.protectedPow' takes the magnitude.
--
-- Deliberately only answers for a /literal/ exponent. A variable exponent
-- may be integer at some data points and not others, which makes the
-- protected operator discontinuous there; no rendering can honestly
-- summarise that, so this returns 'False' and the expression is shown
-- as written.
isNonIntegerConst :: Expr -> Bool
isNonIntegerConst (Const c) =
  abs c < 9.007199254740992e15 && c /= fromIntegral (round c :: Integer)
isNonIntegerConst _ = False

-- | Pre-order indices that lie strictly inside a binder node.
--
-- __The defence against variable capture and escape.__ The genetic
-- operators address nodes by a flat pre-order index over the whole tree and
-- know nothing about scope, so without this they would happily lift a
-- subtree mentioning a bound index out of its binder (freeing it), or graft
-- a foreign subtree mentioning the same name in (capturing it). Neither
-- crashes — 'TypedGP.Eval.eval' reads an unbound variable as @0@ — so both
-- silently turn one expression into a different one.
--
-- Excluding these indices from the operators' candidate sites makes a
-- 'Sum' __atomic__: it can be swapped, deleted or duplicated wholesale, and
-- nothing reaches inside it. Its body only ever changes by being generated
-- fresh.
--
-- __Bounds are sealed too, not only the body.__ @lo@ and @hi@ are evaluated
-- in the enclosing scope, so grafting into them carries no /capture/ risk —
-- but it carries an equal /cost/ risk, since an evolved bound is exactly
-- the runaway-iteration case guarded by
-- 'TypedGP.Config.cfgMaxIterations', and a binder dropped into a bound
-- nests one inside another. See @docs\/phase6-binders-design.md@ decision 1.
--
-- Deliberately conservative: binder bodies do not evolve, they are only
-- generated. That fails towards "this operator is less useful than it could
-- be" rather than "the search silently optimises nonsense".
sealedIndices :: Expr -> [Int]
sealedIndices root = go 0 root
  where
    -- @offset@ is the pre-order index of @expr@ within @root@.
    go :: Int -> Expr -> [Int]
    go offset expr = case expr of
      -- Everything below a binder is sealed, including any nested binder's
      -- own subtree, so no recursion into the children is needed here.
      Sum _ _ _ _ -> [offset + 1 .. offset + countNodes expr - 1]
      _           -> concat (childOffsets (offset + 1) (childrenOf expr))

    childOffsets :: Int -> [Expr] -> [[Int]]
    childOffsets _ [] = []
    childOffsets base (c : cs) =
      go base c : childOffsets (base + countNodes c) cs

-- | Does this expression contain a binder at all?
--
-- Cheaper than @not . null . sealedIndices@ and, more importantly, says
-- what it means at the call site: the operators can skip building an
-- exclusion list entirely for the overwhelmingly common case of a tree with
-- no binders in it.
hasBinder :: Expr -> Bool
hasBinder expr = case expr of
  Sum _ _ _ _ -> True
  _           -> any hasBinder (childrenOf expr)

-- | Free variables: every name that occurs unbound.
--
-- Distinct from 'variablesOf', which counts /every/ @Var@ occurrence
-- including bound ones. The difference is the whole point of a binder, and
-- conflating them is a live hazard:
--
--   * __Do not__ use this in 'TypedGP.Simplify.isFoldable'. A binder with
--     no free variables would become foldable, and folding evaluates it —
--     during elite simplification, once per generation. See the note there.
--   * __Do__ use it when asking whether a subtree may legally be moved,
--     which is what the less conservative future version of the operator
--     rules needs.
freeVariables :: Expr -> [VarName]
freeVariables = nub . go
  where
    go :: Expr -> [VarName]
    go expr = case expr of
      Var v -> [v]
      -- The index is bound in the body only. Bounds are evaluated in the
      -- enclosing scope, so a mention of @i@ there is free.
      Sum i lo hi body ->
        go lo ++ go hi ++ filter (/= i) (go body)
      _ -> concatMap go (childrenOf expr)

-- | Pre-order indices the genetic operators may legally address.
--
-- The complement of 'sealedIndices' over @[0 .. countNodes - 1]@, and the
-- function every operator should draw its site from.
--
-- __Bit-for-bit identical to @[0 .. countNodes e - 1]@ for any expression
-- containing no binder__, which is what keeps every run that does not use
-- binders reproducible against numbers recorded before they existed. The
-- fast path below is not merely an optimisation: it is that guarantee made
-- obvious rather than argued.
--
-- Combined with 'TypedGP.Random.pick', which draws @nextInt (length xs)@,
-- a binder-free tree consumes exactly the randomness the old
-- @nextInt (countNodes parent)@ did, and maps draw @k@ to index @k@.
unsealedIndices :: Expr -> [Int]
unsealedIndices expr
  | not (hasBinder expr) = [0 .. countNodes expr - 1]
  | otherwise = without (sealedIndices expr) [0 .. countNodes expr - 1]
  where
    -- Both lists are ascending, so one merge pass suffices; 'notElem' per
    -- candidate would be quadratic in tree size on the operators' hot path.
    without :: [Int] -> [Int] -> [Int]
    without _ [] = []
    without [] candidates = candidates
    without sealed@(s : ss) (c : cs)
      | c == s    = without ss cs
      | c < s     = c : without sealed cs
      | otherwise = without ss (c : cs)
