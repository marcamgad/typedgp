{-# LANGUAGE BangPatterns #-}

-- | Random expression generation: the two classic Koza methods, @grow@ and
-- @full@, plus the ramped half-and-half mixture used to seed a population.
--
-- All three respect a depth /budget/ measured in edges, matching
-- 'TypedGP.Expr.depth': a budget of @0@ means "emit a terminal now".
module TypedGP.Gen.Grow
  ( Method (..)
  , genExprWith
  , rampedHalfAndHalfWith
  , genExpr
  , genTerminal
  , rampedHalfAndHalf
  ) where

import TypedGP.Config (Config (..))
import TypedGP.Expr
  ( BinaryOp (..)
  , Expr (..)
  , UnaryOp (..)
  , VarName
  , binaryOps
  , unaryOps
  )
import TypedGP.Random (Seed, chance, nextInt, nextRange, pick, pickWeighted)
import TypedGP.Spectral (FrequencyTable, frequenciesFor)

-- | Which generation method to use.
data Method
  = Grow
    -- ^ May stop at a terminal at any depth (probability
    -- 'cfgTerminalProb'), giving irregular trees of varied shape.
  | Full
    -- ^ Only stops at the depth limit, giving a perfectly balanced tree.
  deriving (Eq, Show)

-- | An operator of either arity, so one uniform 'pick' can choose across
-- the whole function set. Local to this module: nothing outside needs to
-- see the two registries merged.
data OpChoice
  = ChoiceBinary BinaryOp
  | ChoiceUnary UnaryOp

-- | The full function set, each paired with its generation weight.
--
-- Non-empty by construction, since 'TypedGP.Expr.binaryOps' and
-- 'TypedGP.Expr.unaryOps' are literal lists.
--
-- Weighted rather than uniform. Uniform picking was fine when there were
-- four binary and two unary operators, but it does not survive adding
-- operators: nine unary against four binary would make roughly 70% of
-- internal nodes unary, and generated trees would be chains rather than
-- structures. Weighting keeps the branching ratio a property the registry
-- states explicitly instead of an accident of how many operators happen to
-- be defined.
opChoices :: [(Double, OpChoice)]
opChoices =
  [(binaryOpWeight op, ChoiceBinary op) | op <- binaryOps]
    ++ [(unaryOpWeight op, ChoiceUnary op) | op <- unaryOps]

-- | Build a trigonometric node with a linear argument, biasing the
-- multiplier towards a detected frequency when one exists.
--
-- The whole of frequency seeding lives here. Everything else in this
-- module, and every genetic operator downstream, is untouched — which is
-- what makes this change carry no determinism or capture risk: a
-- differently-initialised population is still an ordinary population.
--
-- Falls back to exactly the unbiased path when the table has nothing for
-- the chosen variable, so a run with detection disabled or with no
-- periodic signal is bit-for-bit what it was before.
genSeededTrig
  :: Config
  -> FrequencyTable
  -> Scope
  -> (Expr -> Expr)
  -- ^ 'Sin' or 'Cos'.
  -> Method
  -> Int
  -> Seed
  -> (Expr, Seed)
genSeededTrig cfg table scope build method budget s0 =
  -- Only variables that actually have frequencies are candidates. Picking
  -- from all of them and then discovering the chosen one has none would
  -- consume a draw for nothing, shifting the PRNG stream relative to an
  -- unseeded run — which would break the guarantee that a no-signal
  -- dataset generates exactly as it did before seeding existed.
  case pick seededVariables s0 of
    Nothing -> unbiased s0
    Just (name, s1) -> case frequenciesFor table name of
      -- Unreachable: seededVariables only contains variables with
      -- frequencies.
      [] -> unbiased s1
      candidates ->
        let (useSeed, s2) = chance (cfgFrequencySeedWeight cfg) s1
        in if not useSeed
             then unbiased s2
             else
               let (multiplier, s3) = pickFrequency candidates s2
                   (lo, hi) = cfgConstRange cfg
                   (offset, s4) = nextRange lo hi s3
               in ( build (Add (Mul (Const multiplier) (Var name)) (Const offset))
                  , s4
                  )
  where
    seededVariables :: [VarName]
    seededVariables =
      [name | name <- cfgVariables cfg, not (null (frequenciesFor table name))]

    -- The ordinary path: an arbitrary subtree under the trig node, exactly
    -- as generation behaved before seeding existed.
    unbiased :: Seed -> (Expr, Seed)
    unbiased st =
      let (child, st') = genExprIn cfg table scope method (budget - 1) st
      in (build child, st')

    pickFrequency :: [Double] -> Seed -> (Double, Seed)
    pickFrequency candidates st = case pick candidates st of
      Just result -> result
      -- Unreachable: the empty case is handled above.
      Nothing     -> (1.0, st)

-- | Generate a terminal: either a fresh random constant or a variable.
--
-- Falls back to a constant when the variable set is empty. 'validateConfig'
-- rules that out, but the fallback keeps this function total on its own
-- terms rather than relying on a caller's invariant.
genTerminal :: Config -> Seed -> (Expr, Seed)
genTerminal cfg = genTerminalIn cfg []

-- | Names a generated subtree may refer to beyond the dataset columns: the
-- indices of every binder it sits inside.
--
-- Threaded explicitly rather than derived from the tree, because the whole
-- point of a binder is that this differs between two structurally
-- identical positions. Its /length/ is also the current nesting depth,
-- which is what 'cfgBinderDepthLimit' bounds.
type Scope = [VarName]

-- | Generate a terminal that may also be a binder index currently in
-- scope.
--
-- Draws from @cfgVariables ++ scope@. With an empty scope that is exactly
-- @cfgVariables@ in the same order, so a binder-free run consumes
-- identical randomness and produces identical terminals — the property
-- that keeps every recorded benchmark number valid.
genTerminalIn :: Config -> Scope -> Seed -> (Expr, Seed)
genTerminalIn cfg scope s0 =
  let (wantConst, s1) = chance (cfgConstProb cfg) s0
  in if wantConst || null available
       then
         let (lo, hi) = cfgConstRange cfg
             (value, s2) = nextRange lo hi s1
         in (Const value, s2)
       else case pick available s1 of
         Just (name, s2) -> (Var name, s2)
         -- Unreachable: the null check above guarantees a non-empty list.
         Nothing         -> (Const 0.0, s1)
  where
    -- Dataset columns first, so the mapping from a PRNG draw to a name is
    -- unchanged whenever the scope is empty.
    available :: [VarName]
    available = cfgVariables cfg ++ scope

-- | Generate a random expression within a depth budget.
--
-- The returned tree always satisfies @depth result <= budget@, and under
-- 'Full' it satisfies @depth result == budget@ exactly.
genExpr :: Config -> Method -> Int -> Seed -> (Expr, Seed)
genExpr cfg = genExprWith cfg []

-- | Generation with a frequency table.
--
-- 'genExpr' is exactly this with an empty table, so a run with seeding
-- disabled — or one where no periodic signal was detected — is
-- bit-for-bit the pre-seeding behaviour rather than merely similar to it.
genExprWith :: Config -> FrequencyTable -> Method -> Int -> Seed -> (Expr, Seed)
genExprWith cfg table = genExprIn cfg table []

-- | Generation with a frequency table and a binder scope.
genExprIn
  :: Config -> FrequencyTable -> Scope -> Method -> Int -> Seed -> (Expr, Seed)
genExprIn cfg table scope method budget s0
  | budget <= 0 = genTerminalIn cfg scope s0
  | otherwise =
      let (stopEarly, s1) = case method of
            Full -> (False, s0)
            Grow -> chance (cfgTerminalProb cfg) s0
      in if stopEarly
           then genTerminalIn cfg scope s1
           else genFunction cfg table scope method budget s1

-- | Generate an internal node and recurse into its children with a
-- decremented budget.
genFunction
  :: Config -> FrequencyTable -> Scope -> Method -> Int -> Seed -> (Expr, Seed)
genFunction cfg table scope method budget s0
  -- The binder branch is taken /before/ any operator is drawn, and only
  -- when binders are switched on. That ordering is what makes a
  -- binder-free run bit-for-bit identical to the pre-binder engine: with
  -- 'cfgEnableBinders' off, 'binderAllowed' is 'False' without consuming
  -- any randomness at all, and the first draw is still the operator pick.
  | binderAllowed =
      let (wantBinder, s1) = chance (cfgBinderProb cfg) s0
      in if wantBinder
           then genBinder cfg table scope method budget s1
           else ordinary s1
  | otherwise = ordinary s0
  where
    -- Nesting is bounded by scope depth rather than by tree depth: a
    -- binder inside a binder multiplies iteration counts, so the limit
    -- that matters is how many are stacked, not how deep the tree is.
    binderAllowed :: Bool
    binderAllowed =
      cfgEnableBinders cfg
        && length scope < cfgBinderDepthLimit cfg
        -- A binder needs a body, so there is no point emitting one with
        -- no budget left for it.
        && budget >= 2

    ordinary :: Seed -> (Expr, Seed)
    ordinary st = case pickWeighted opChoices st of
      -- Unreachable: opChoices is built from two non-empty literal lists.
      Nothing -> genTerminalIn cfg scope st
      Just (ChoiceUnary op, s1)
        -- The one place seeding applies. Everything else generates as before.
        | tableHasSignal table && isTrigonometric op ->
            genSeededTrig cfg table scope (unaryOpBuild op) method budget s1
        | otherwise ->
            let (child, s2) = genExprIn cfg table scope method (budget - 1) s1
            in (unaryOpBuild op child, s2)
      Just (ChoiceBinary op, s1) ->
        let (left, s2)  = genExprIn cfg table scope method (budget - 1) s1
            (right, s3) = genExprIn cfg table scope method (budget - 1) s2
        in (binaryOpBuild op left right, s3)

-- | Generate a @Sum@ with constant bounds and a body that can see the index.
--
-- __Bounds are literal constants, never generated subtrees.__ An evolved
-- bound is the runaway-iteration case @docs\/phase2-design.md@ §3 warns
-- about, and since 'TypedGP.Expr.sealedIndices' makes the whole node
-- atomic, a bound generated as a subtree could never be repaired by the
-- operators either. The lower bound is fixed at 1 and only the term count
-- is drawn, which is the shape a summation almost always takes.
--
-- The bounds are still 'Const' nodes, so
-- 'TypedGP.LocalSearch.optimiseConstants' /can/ retune them numerically
-- even though the operators cannot restructure them — and could in
-- principle push one past 'TypedGP.Eval.iterationCap', where the cap
-- reports 'TypedGP.Eval.IterationCapped' rather than silently truncating.
genBinder
  :: Config -> FrequencyTable -> Scope -> Method -> Int -> Seed -> (Expr, Seed)
genBinder cfg table scope method budget s0 =
  let (terms, s1) = nextInt (max 1 (cfgBinderMaxTerms cfg)) s0
      name = freshIndex cfg scope
      (body, s2) =
        genExprIn cfg table (scope ++ [name]) method (budget - 1) s1
  in (Sum name (Const 1.0) (Const (fromIntegral terms + 1.0)) body, s2)

-- | An index name that collides with nothing currently visible.
--
-- Derived from the nesting depth rather than drawn, so it consumes no
-- randomness and two binders in one nest can never share a name — which
-- means the shadowing case, while handled correctly by 'eval', is never
-- actually produced by generation.
--
-- A dataset column could still be called @i0@, so the name is extended
-- with primes until it is genuinely fresh rather than assumed to be.
freshIndex :: Config -> Scope -> VarName
freshIndex cfg scope = disambiguate ("i" ++ show (length scope))
  where
    taken :: [VarName]
    taken = cfgVariables cfg ++ scope

    disambiguate :: VarName -> VarName
    disambiguate candidate
      | candidate `elem` taken = disambiguate (candidate ++ "'")
      | otherwise              = candidate

-- | Is this operator one whose argument a frequency would make sense in?
--
-- Decided by building a node and matching the constructor, rather than by
-- comparing 'unaryOpName' against string literals. Both are uncheckable by
-- @-Wall@ in the strict sense — the catch-all means a future periodic
-- operator would not be flagged — but a constructor match cannot be broken
-- by renaming an operator's display name, which a string comparison
-- silently would be.
-- | Does any variable have a detected frequency?
--
-- Checked before the seeded path is entered, so that a dataset with no
-- periodic structure consumes no extra randomness and generates exactly
-- as it did before seeding existed.
tableHasSignal :: FrequencyTable -> Bool
tableHasSignal = any (not . null . snd)

isTrigonometric :: UnaryOp -> Bool
isTrigonometric op = case unaryOpBuild op (Const 0.0) of
  Sin _ -> True
  Cos _ -> True
  _     -> False

-- | Ramped half-and-half: the standard way to seed a diverse population.
--
-- Depths are cycled over @[2 .. cfgMaxInitialDepth]@ and the method
-- alternates between 'Grow' and 'Full' each time the depth cycle wraps, so
-- the initial population covers both a range of sizes and a range of
-- shapes. Depth 2 is the floor because depth 0 is a bare terminal and
-- depth 1 admits only a handful of distinct trees — neither carries useful
-- genetic material.
rampedHalfAndHalf :: Config -> Int -> Seed -> ([Expr], Seed)
rampedHalfAndHalf cfg = rampedHalfAndHalfWith cfg []

-- | Ramped half-and-half with a frequency table threaded into generation.
rampedHalfAndHalfWith :: Config -> FrequencyTable -> Int -> Seed -> ([Expr], Seed)
rampedHalfAndHalfWith cfg table n s0 = go 0 s0 []
  where
    minDepth :: Int
    minDepth = 2

    maxDepth :: Int
    maxDepth = max minDepth (cfgMaxInitialDepth cfg)

    depthCycle :: Int
    depthCycle = maxDepth - minDepth + 1

    go :: Int -> Seed -> [Expr] -> ([Expr], Seed)
    go !k st acc
      | k >= n = (reverse acc, st)
      | otherwise =
          let budget = minDepth + (k `mod` depthCycle)
              method = if even (k `div` depthCycle) then Grow else Full
              (e, st') = genExprWith cfg table method budget st
          in go (k + 1) st' (e : acc)
