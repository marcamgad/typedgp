{-# LANGUAGE BangPatterns #-}

-- | Which domain errors actually occur, and how often.
--
-- Built to answer one specific question raised by Phase 3's null result:
-- __does 'Saturated' dominate the other five constructors?__ If it does,
-- a single 'TypedGP.Config.cfgDomainPenalty' cannot express the pressure
-- it was meant to, because it would be charging mostly for "this number
-- got large" rather than for "this expression is not meaningful here".
--
-- That matters beyond the penalty. Symbolic differentiation is built on
-- 'EvalResult', and a differentiator that treats saturation as equivalent
-- to a genuine domain violation will refuse to differentiate perfectly
-- well-behaved expressions — or, worse, accept the reverse. The taxonomy
-- needs to be measured before it is depended on, not after.
module TypedGP.DomainStats
  ( DomainBreakdown (..)
  , emptyBreakdown
  , breakdownOver
  , breakdownCount
  , breakdownTotal
  , breakdownInvalid
  , saturationShare
  , divergesFromLegacyPow
  , legacyPowExposure
  ) where

import TypedGP.Data.Dataset (DataPoint (..), Dataset, datasetPoints)
import TypedGP.Eval (DomainError (..), Env, eval, evalDomain, evalResultError)
import TypedGP.Expr (Expr (..))

-- | A tally of evaluations by outcome.
--
-- Counts are over (expression, data point) pairs — one evaluation each —
-- because that is the unit the penalty charges for.
--
-- Only the /first/ error of each evaluation is counted, matching
-- 'evalDomain'. So this measures "what was the expression first caught
-- doing", not "how many ways was it wrong", and a left-to-right bias is
-- inherent: an expression that both divides by zero and saturates is
-- recorded under whichever the traversal met first.
data DomainBreakdown = DomainBreakdown
  { dbValid :: !Int
  , dbDividedByZero :: !Int
  , dbLogOfNonPositive :: !Int
  , dbPowOfNegativeBase :: !Int
  , dbGammaAtPole :: !Int
  , dbZetaAtPole :: !Int
  , dbIterationCapped :: !Int
  , dbSaturated :: !Int
  } deriving (Eq, Show)

emptyBreakdown :: DomainBreakdown
emptyBreakdown = DomainBreakdown 0 0 0 0 0 0 0 0

-- | Tally every expression against every point of a dataset.
--
-- Strict in the accumulator throughout: this is run over whole populations
-- and a lazy fold would build one thunk per evaluation.
breakdownOver :: Dataset -> [Expr] -> DomainBreakdown
breakdownOver ds exprs = strictFold step emptyBreakdown pairs
  where
    points :: [DataPoint]
    points = datasetPoints ds

    pairs :: [(Expr, DataPoint)]
    pairs = [(e, p) | e <- exprs, p <- points]

    step :: DomainBreakdown -> (Expr, DataPoint) -> DomainBreakdown
    step !acc (e, p) = record acc (evalResultError (evalDomain (dpInputs p) e))

    -- Local and freshly named, so this module needs neither a CPP shim for
    -- the base 4.20 Prelude change nor a name that shadows it.
    strictFold :: (b -> a -> b) -> b -> [a] -> b
    strictFold f = go
      where
        go !acc []       = acc
        go !acc (x : xs) = go (f acc x) xs

record :: DomainBreakdown -> Maybe DomainError -> DomainBreakdown
record acc Nothing = acc { dbValid = dbValid acc + 1 }
record acc (Just e) = case e of
  DividedByZero     -> acc { dbDividedByZero = dbDividedByZero acc + 1 }
  LogOfNonPositive  -> acc { dbLogOfNonPositive = dbLogOfNonPositive acc + 1 }
  PowOfNegativeBase -> acc { dbPowOfNegativeBase = dbPowOfNegativeBase acc + 1 }
  GammaAtPole       -> acc { dbGammaAtPole = dbGammaAtPole acc + 1 }
  ZetaAtPole        -> acc { dbZetaAtPole = dbZetaAtPole acc + 1 }
  IterationCapped   -> acc { dbIterationCapped = dbIterationCapped acc + 1 }
  Saturated         -> acc { dbSaturated = dbSaturated acc + 1 }

-- | The count for one constructor.
--
-- A total function over the closed 'DomainError' set, so adding a
-- constructor without extending 'DomainBreakdown' is an
-- incomplete-pattern error rather than a silently missing column.
breakdownCount :: DomainBreakdown -> DomainError -> Int
breakdownCount b e = case e of
  DividedByZero     -> dbDividedByZero b
  LogOfNonPositive  -> dbLogOfNonPositive b
  PowOfNegativeBase -> dbPowOfNegativeBase b
  GammaAtPole       -> dbGammaAtPole b
  ZetaAtPole        -> dbZetaAtPole b
  IterationCapped   -> dbIterationCapped b
  Saturated         -> dbSaturated b

-- | Every evaluation counted, valid or not.
breakdownTotal :: DomainBreakdown -> Int
breakdownTotal b = dbValid b + breakdownInvalid b

-- | Evaluations that reported some domain error.
breakdownInvalid :: DomainBreakdown -> Int
breakdownInvalid b = sum (map (breakdownCount b) [minBound .. maxBound])

-- | 'Saturated' as a share of all /invalid/ evaluations.
--
-- The number the Phase 3 open question turns on. @0@ when nothing was
-- invalid, which is the useful answer there rather than a division by
-- zero: no invalid evaluations means saturation dominates nothing.
saturationShare :: DomainBreakdown -> Double
saturationShare b
  | invalid == 0 = 0.0
  | otherwise = fromIntegral (dbSaturated b) / fromIntegral invalid
  where
    invalid = breakdownInvalid b

-- | Does this expression, at this point, evaluate any @Pow@ node where the
-- previous and current semantics disagree?
--
-- __Archaeology, deliberately kept.__ @Pow@ used to keep the sign of a
-- negative base when the exponent was an integer and drop it otherwise
-- (see 'TypedGP.Eval.protectedPow'). The two definitions differ on exactly
-- one case: a __negative base with an odd integer exponent__, where the old
-- rule gave @-|b|^e@ and the new one gives @+|b|^e@.
--
-- The design note for that change predicted benchmark numbers would be
-- unaffected, on the grounds that an exponent landing exactly on an integer
-- has measure zero under continuous sampling. The numbers changed. This
-- function exists to measure /why/ rather than to guess: exact integers are
-- not measure zero when they are produced structurally, and this engine
-- produces them structurally all the time —
-- 'TypedGP.Eval.protectedDiv' returns exactly @1.0@ for @x / x@, and
-- @x - x@ is exactly @0@, so @a ^ (x / x)@ is an odd integer power for
-- every input rather than for almost none.
--
-- Kept rather than deleted because anyone comparing a pre-change benchmark
-- against a post-change one needs to know how exposed the comparison is,
-- and that question will outlive this conversation.
divergesFromLegacyPow :: Env -> Expr -> Bool
divergesFromLegacyPow env = go
  where
    go :: Expr -> Bool
    go expr = case expr of
      Pow a b ->
        let base = eval env a
            expo = eval env b
        in (base < 0.0 && isOddInteger expo) || go a || go b
      Const _ -> False
      Var _   -> False
      Add a b -> go a || go b
      Sub a b -> go a || go b
      Mul a b -> go a || go b
      Div a b -> go a || go b
      Sin a   -> go a
      Cos a   -> go a
      Exp a   -> go a
      Log a   -> go a
      Sqrt a  -> go a
      Tanh a  -> go a
      Abs a   -> go a
      Gamma a -> go a
      Zeta a  -> go a
      -- Bounds and body alike; a legacy-divergent Pow anywhere inside a
      -- binder still makes the whole expression divergent.
      Sum _ lo hi body -> go lo || go hi || go body

    isOddInteger :: Double -> Bool
    isOddInteger x =
      abs x < 9.007199254740992e15
        && x == fromIntegral (round x :: Integer)
        && odd (round x :: Integer)

-- | Share of (expression, point) pairs on which the two @Pow@ definitions
-- disagree. See 'divergesFromLegacyPow'.
legacyPowExposure :: Dataset -> [Expr] -> Double
legacyPowExposure ds exprs
  | total == 0 = 0.0
  | otherwise = fromIntegral diverging / fromIntegral total
  where
    pairs = [(e, p) | e <- exprs, p <- datasetPoints ds]
    total = length pairs
    diverging =
      length [() | (e, p) <- pairs, divergesFromLegacyPow (dpInputs p) e]
