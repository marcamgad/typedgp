-- | The two structural mutation operators, and the dispatcher that chooses
-- between all available strategies.
--
-- The division of labour across @TypedGP.Ops.@ is:
--
--   * each __strategy__ owns a module and exports one operator —
--     'subtreeMutation' and 'pointMutation' here,
--     'TypedGP.Ops.Hoist.hoistMutation' next door;
--   * this module additionally owns the __dispatch policy__ ('mutate'),
--     because something has to read the config and pick.
--
-- Adding a fourth strategy (constant annealing, say) means a new module
-- plus one new branch in 'mutate' and one new share in
-- "TypedGP.Config" — never a new branch inside an existing operator.
--
-- The two operators here are __depth-safe__: given a parent within
-- 'cfgMaxDepth', the result is also within 'cfgMaxDepth'. That is
-- enforced structurally (by budgeting the replacement subtree against the
-- depth of the site it lands at), not by generate-and-reject. Hoist
-- mutation is depth-safe for free, since it only ever shrinks.
module TypedGP.Ops.Mutation
  ( mutate
  , subtreeMutation
  , pointMutation
  ) where

import Data.Maybe (fromMaybe)

import TypedGP.Config (Config (..))
import TypedGP.Expr
  ( BinaryOp (..)
  , Expr (..)
  , UnaryOp (..)
  , binaryOps
  , nodeDepthAt
  , opName
  , replaceAt
  , subtreeAt
  , unaryOps
  , unsealedIndices
  )
import TypedGP.Gen.Grow (Method (..), genExpr)
import TypedGP.Ops.Hoist (hoistMutation)
import TypedGP.Random (Seed, nextDouble, nextRange, pick, pickWeighted)

-- | Apply one mutation, choosing a strategy according to
-- 'cfgPointMutationShare' and 'cfgHoistMutationShare'.
--
-- The three do genuinely different jobs, and a run missing any of them is
-- worse off in a specific, predictable way:
--
--   * __subtree__ mutation is the source of new structure; without it the
--     search cannot escape the material it started with;
--   * __point__ mutation is local search — it tunes constants and swaps
--     operators without disturbing a working skeleton; without it the
--     search thrashes;
--   * __hoist__ mutation removes structure; without it a superfluous
--     wrapper such as @sin(sin(y))@ is a trap, reachable only by
--     regenerating the whole subtree correctly by luck.
--
-- A single uniform draw partitions @[0, 1)@ across the three, so the
-- shares cannot silently disagree: whatever is left after point and hoist
-- is subtree mutation, by construction.
mutate :: Config -> Expr -> Seed -> (Expr, Seed)
mutate cfg parent s0 =
  let (roll, s1) = nextDouble s0
      pointCut = cfgPointMutationShare cfg
      hoistCut = pointCut + cfgHoistMutationShare cfg
  in if roll < pointCut
       then pointMutation cfg parent s1
       else if roll < hoistCut
         then hoistMutation parent s1
         else subtreeMutation cfg parent s1

-- | Replace a uniformly chosen subtree with a freshly generated one.
--
-- The replacement's depth budget is @cfgMaxDepth - depth of the chosen
-- site@, so the result cannot exceed the global ceiling. It is also capped
-- at 'cfgMaxInitialDepth': a mutation is meant to be a perturbation, and
-- letting it graft a maximum-depth tree onto the root would make it a
-- restart instead.
subtreeMutation :: Config -> Expr -> Seed -> (Expr, Seed)
subtreeMutation cfg parent s0 =
  -- Unsealed sites only: grafting a freshly generated subtree into a
  -- binder body would capture its index. Same draw as before for any
  -- binder-free tree.
  case pick (unsealedIndices parent) s0 of
    Nothing -> (parent, s0)
    Just (site, s1) ->
      let
          -- Total by construction: site is an index of this tree, so the
          -- lookup succeeds; the default only silences the Maybe.
          siteDepth = fromMaybe 0 (nodeDepthAt site parent)
          budget = min (cfgMaxInitialDepth cfg) (max 0 (cfgMaxDepth cfg - siteDepth))
          (replacement, s2) = genExpr cfg Grow budget s1
      in (fromMaybe parent (replaceAt site replacement parent), s2)

-- | Replace a uniformly chosen node's /operator/ (or terminal value),
-- keeping its children.
--
-- Structure-preserving, so the depth of the tree is unchanged and the
-- depth cap needs no checking at all.
pointMutation :: Config -> Expr -> Seed -> (Expr, Seed)
pointMutation cfg parent s0 =
  case pick (unsealedIndices parent) s0 of
   Nothing -> (parent, s0)
   Just (site, s1) -> case subtreeAt site parent of
       -- Unreachable: site came from this tree's own index list.
       Nothing -> (parent, s1)
       Just node ->
         let (node', s2) = mutateNode cfg node s1
         in (fromMaybe parent (replaceAt site node' parent), s2)

-- | Mutate a single node in place, preserving its arity.
--
-- The exhaustive @case@ is intentional: when a constructor is added to
-- 'Expr', @-Wall@ points here and asks how that node should mutate. The
-- answer is nearly always "add it to the matching arity group", but the
-- compiler making you say so is the point.
mutateNode :: Config -> Expr -> Seed -> (Expr, Seed)
mutateNode cfg node s0 = case node of
  Const c ->
    let jitter = cfgConstJitter cfg
        (delta, s1) = nextRange (negate jitter) jitter s0
    in (Const (c + delta), s1)
  Var v -> case pick (filter (/= v) (cfgVariables cfg)) s0 of
    Just (v', s1) -> (Var v', s1)
    -- Single-variable problem: there is no other variable to swap to, so
    -- leave the node alone rather than burning randomness on a no-op.
    Nothing       -> (node, s0)
  Add a b -> rebuildBinary a b
  Sub a b -> rebuildBinary a b
  Mul a b -> rebuildBinary a b
  Div a b -> rebuildBinary a b
  Pow a b -> rebuildBinary a b
  Sin a   -> rebuildUnary a
  Cos a   -> rebuildUnary a
  Exp a   -> rebuildUnary a
  Log a   -> rebuildUnary a
  Sqrt a  -> rebuildUnary a
  Tanh a  -> rebuildUnary a
  Abs a   -> rebuildUnary a
  Gamma a -> rebuildUnary a
  Zeta a  -> rebuildUnary a
  -- Left alone, like the single-variable 'Var' case above and for the same
  -- reason: there is no legal move.
  --
  -- Point mutation swaps an operator for another of the same arity, and
  -- 'Sum' is the only three-child node, so there is nothing to swap to.
  -- The one other thing that could change here is the index name, and
  -- renaming it is alpha-renaming — it must rewrite every bound occurrence
  -- in the body to preserve meaning, which is a scope-aware traversal
  -- rather than a point edit, and is semantically a no-op even when done
  -- correctly.
  Sum _ _ _ _ -> (node, s0)
 where
  -- Filtering by name excludes the node's current operator, so a point
  -- mutation always actually changes something.
  --
  -- Weighted, matching random generation: an unweighted swap here would
  -- quietly reintroduce the uniform distribution that 'unaryOps' exists to
  -- avoid, and point mutation is frequent enough to undo the weighting
  -- within a few generations.
  rebuildBinary :: Expr -> Expr -> (Expr, Seed)
  rebuildBinary a b =
    case pickWeighted (weighted binaryOpWeight binaryOpName binaryOps) s0 of
      Just (op, s1) -> (binaryOpBuild op a b, s1)
      -- Unreachable while more than one binary operator is registered.
      Nothing       -> (node, s0)

  rebuildUnary :: Expr -> (Expr, Seed)
  rebuildUnary a =
    case pickWeighted (weighted unaryOpWeight unaryOpName unaryOps) s0 of
      Just (op, s1) -> (unaryOpBuild op a, s1)
      -- Unreachable while more than one unary operator is registered.
      Nothing       -> (node, s0)

  weighted :: (op -> Double) -> (op -> String) -> [op] -> [(Double, op)]
  weighted weightOf nameOf ops =
    [(weightOf op, op) | op <- ops, nameOf op /= opName node]
