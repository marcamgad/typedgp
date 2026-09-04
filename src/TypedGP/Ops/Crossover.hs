-- | Subtree crossover: the operator that actually recombines genetic
-- material between two parents.
module TypedGP.Ops.Crossover
  ( crossover
  ) where

import Data.Maybe (fromMaybe)

import TypedGP.Config (Config (..))
import TypedGP.Expr (Expr, depth, replaceAt, subtreeAt, unsealedIndices)
import TypedGP.Random (Seed, pick)

-- | Swap a uniformly chosen subtree of each parent, producing two
-- offspring.
--
-- Depth control is by __rejection per child__: if an offspring would
-- exceed 'cfgMaxDepth', that offspring is replaced by the parent it came
-- from. Rejecting one child does not discard the other, so a lucky swap is
-- never thrown away because its sibling was unlucky.
--
-- Rejection is the right choice here (unlike in mutation, where the depth
-- budget is computed up front): the donor subtree already exists, so there
-- is no budget to generate against, and truncating it would silently
-- change its semantics.
crossover :: Config -> Expr -> Expr -> Seed -> ((Expr, Expr), Seed)
crossover cfg parentA parentB s0 =
  -- Sites are drawn from the UNSEALED indices, which is what keeps a
  -- binder atomic: nothing inside one can be lifted out (freeing its
  -- bound index) or replaced (capturing a foreign one). For a tree with
  -- no binder this is exactly [0 .. countNodes - 1], so the draw and the
  -- resulting site are unchanged.
  case pick (unsealedIndices parentA) s0 of
   Nothing -> ((parentA, parentB), s0)
   Just (siteA, s1) -> case pick (unsealedIndices parentB) s1 of
    Nothing -> ((parentA, parentB), s1)
    Just (siteB, s2) ->
     case (subtreeAt siteA parentA, subtreeAt siteB parentB) of
       (Just donorA, Just donorB) ->
         let childA = fromMaybe parentA (replaceAt siteA donorB parentA)
             childB = fromMaybe parentB (replaceAt siteB donorA parentB)
         in ((capDepth parentA childA, capDepth parentB childB), s2)
       -- Unreachable: both sites were drawn from the parents' own index
       -- lists.
       _ -> ((parentA, parentB), s2)
  where
    capDepth :: Expr -> Expr -> Expr
    capDepth fallback child
      | depth child > cfgMaxDepth cfg = fallback
      | otherwise                     = child
