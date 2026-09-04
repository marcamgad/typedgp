-- | Hoist mutation: replace a subtree with one of its own subtrees.
--
-- The operator that removes a superfluous wrapper. Given
-- @sin(sin(y)) + x + x@ it can produce @sin(y) + x + x@ in a single move —
-- something neither of the operators in "TypedGP.Ops.Mutation" can do:
-- subtree mutation would have to regenerate the whole subtree correctly by
-- chance, and point mutation can only swap @sin@ for @cos@ while keeping
-- the nesting.
--
-- It is also the engine's only /strictly shrinking/ operator, which makes
-- it the natural counterweight to bloat. Two properties fall out
-- structurally rather than being enforced by a check:
--
--   * node count strictly decreases whenever a non-leaf site is chosen,
--     because the replacement is a __proper__ subtree of what it replaces;
--   * depth never increases, so the 'TypedGP.Config.cfgMaxDepth' ceiling
--     needs no testing here at all.
--
-- Unlike the other operators this one takes no 'TypedGP.Config.Config'.
-- It has nothing to tune: the move is entirely determined by the tree and
-- two uniform draws. Giving it a config parameter for the sake of a
-- uniform operator signature would be a lie about what it depends on.
module TypedGP.Ops.Hoist
  ( hoistMutation
  ) where

import Data.Maybe (fromMaybe)

import TypedGP.Expr (Expr, replaceAt, subtreeAt, unsealedIndices)
import TypedGP.Random (Seed, pick)

-- | Pick a node uniformly, then replace it with one of its own proper
-- subtrees, picked uniformly.
--
-- A leaf has no proper subtree, so choosing one is a no-op. That is left
-- as a genuine no-op rather than being resampled: retrying until a
-- non-leaf turns up would bias the operator towards the top of the tree,
-- where the shrinking effect is most violent.
hoistMutation :: Expr -> Seed -> (Expr, Seed)
hoistMutation parent s0 =
  -- Unsealed sites only, so nothing is ever hoisted out of a binder body
  -- (which would free its bound index). Identical to the previous
  --  (countNodes parent)@ for any binder-free tree.
  case pick (unsealedIndices parent) s0 of
    Nothing -> (parent, s0)
    Just (site, s1) -> case subtreeAt site parent of
      -- Unreachable: site came from this tree's own index list.
      Nothing   -> (parent, s1)
      Just node -> hoistInto site node parent s1

-- | Replace the subtree at @site@ with a proper subtree of @node@, where
-- @node@ is already known to be the subtree sitting at @site@.
hoistInto :: Int -> Expr -> Expr -> Seed -> (Expr, Seed)
hoistInto site node parent s0
  | properSubtrees <= 0 = (parent, s0)
  | otherwise =
      -- Pre-order index 0 of a node is the node itself, so the proper
      -- subtrees are exactly indices 1 .. countNodes node - 1. Offsetting
      -- by one is what makes this a hoist rather than an identity.
      case pick donorCandidates s0 of
        Nothing -> (parent, s0)
        Just (donorIndex, s1) -> case subtreeAt donorIndex node of
          -- Unreachable: donorIndex came from node's own index list.
          Nothing    -> (parent, s1)
          Just donor -> (fromMaybe parent (replaceAt site donor parent), s1)
  where
    -- Proper subtrees that are also unsealed. Index 0 is the node itself,
    -- so hoisting it would be an identity; dropping it is what makes this
    -- a hoist. For a binder-free node this is [1 .. countNodes node - 1],
    -- the same candidates and the same draw as before.
    donorCandidates :: [Int]
    donorCandidates = filter (/= 0) (unsealedIndices node)

    properSubtrees :: Int
    properSubtrees = length donorCandidates
