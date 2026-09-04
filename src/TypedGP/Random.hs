{-# LANGUAGE ScopedTypeVariables #-}

-- | A hand-rolled, purely functional PRNG: xorshift64*.
--
-- This module has __zero knowledge of genetic programming__ — it could
-- shuffle a grocery list. That isolation is deliberate: it means
-- "TypedGP.Random"'s tests measure the generator alone, so a distribution
-- bug can never hide behind a GP bug (or vice versa).
--
-- Every function threads the state explicitly: @f :: ... -> Seed -> (a,
-- Seed)@. There is no global mutable seed anywhere in this package, which
-- is what makes a whole evolution run bit-for-bit reproducible from a
-- single 'Word64'.
module TypedGP.Random
  ( -- * State
    Seed
  , mkSeed
  , rawSeed
  , seedWord
  , splitSeed

    -- * Primitives
  , nextWord64
  , nextDouble

    -- * Derived distributions
  , nextRange
  , nextInt
  , nextBool
  , chance
  , nextGaussian

    -- * Collections
  , pick
  , pickWeighted
  , shuffle
  ) where

import Data.Bits (shiftL, shiftR, xor)
import Data.Word (Word64)

-- | Opaque generator state. The constructor is hidden so that the
-- "state is never zero" invariant established by 'mkSeed' cannot be
-- violated from outside — xorshift is absorbing at zero and would emit an
-- endless stream of zeros.
newtype Seed = Seed Word64
  deriving (Eq, Show)

-- Constants are written in hex, exactly as they appear in the papers, so
-- they can be checked by eye against the source material.

-- | Multiplier from Vigna's xorshift64* — the output scrambler that fixes
-- the weak low bits of a plain xorshift.
xorshiftMultiplier :: Word64
xorshiftMultiplier = 0x2545F4914F6CDD1D

-- | SplitMix64 finalisation constants, used by 'mkSeed' to decorrelate
-- neighbouring user seeds.
avalancheA, avalancheB :: Word64
avalancheA = 0xBF58476D1CE4E5B9
avalancheB = 0x94D049BB133111EB

-- | Golden-ratio increment, likewise from SplitMix64.
goldenGamma :: Word64
goldenGamma = 0x9E3779B97F4A7C15

-- | Build a generator from an arbitrary user-supplied number.
--
-- The input is run through the SplitMix64 avalanche first. Without it,
-- seeds @1@ and @2@ differ in a single bit and xorshift needs many steps to
-- diverge, so consecutive trial seeds (@--seed 1@, @--seed 2@, ...) would
-- produce visibly correlated runs.
mkSeed :: Word64 -> Seed
mkSeed w = Seed (ensureNonZero (avalanche (w + goldenGamma)))
  where
    avalanche :: Word64 -> Word64
    avalanche z0 =
      let z1 = (z0 `xor` (z0 `shiftR` 30)) * avalancheA
          z2 = (z1 `xor` (z1 `shiftR` 27)) * avalancheB
      in z2 `xor` (z2 `shiftR` 31)

    -- Zero is the one fixed point of the xorshift step.
    ensureNonZero :: Word64 -> Word64
    ensureNonZero 0 = goldenGamma
    ensureNonZero z = z

-- | Expose the raw state, for logging or for persisting a run's position.
seedWord :: Seed -> Word64
seedWord (Seed w) = w

-- | Rebuild a generator from a raw state word, __bypassing__ the avalanche
-- that 'mkSeed' applies.
--
-- This exists for deserialisation and nothing else. @rawSeed . seedWord@ is
-- the identity, which @mkSeed . seedWord@ deliberately is not — 'mkSeed'
-- scrambles its input, so round-tripping a checkpoint through it would
-- silently resume from a different position in the stream and quietly
-- destroy the reproducibility guarantee.
--
-- Zero is still rejected, since the invariant it protects (xorshift is
-- absorbing at zero) is a property of the algorithm, not of the caller.
rawSeed :: Word64 -> Seed
rawSeed 0 = Seed goldenGamma
rawSeed w = Seed w

-- | Derive an independent generator, returning it alongside the advanced
-- original. Used to hand a deterministic sub-stream to a nested
-- computation without serialising it through the caller's state.
splitSeed :: Seed -> (Seed, Seed)
splitSeed s0 =
  let (w, s1) = nextWord64 s0
  in (mkSeed w, s1)

-- | One xorshift64* step: a full 64 bits of output plus the next state.
--
-- The returned word is the /scrambled/ output; the stored state is the
-- unscrambled shift result. Keeping those distinct is what gives the
-- generator its output quality.
nextWord64 :: Seed -> (Word64, Seed)
nextWord64 (Seed s0) =
  let s1 = s0 `xor` (s0 `shiftR` 12)
      s2 = s1 `xor` (s1 `shiftL` 25)
      s3 = s2 `xor` (s2 `shiftR` 27)
  in (s3 * xorshiftMultiplier, Seed s3)

-- | Uniform in @[0, 1)@.
--
-- Built from the top 53 bits, which are the highest-quality bits of the
-- output word and exactly the mantissa width of a 'Double', so every
-- representable value in the range is reachable and none is favoured.
nextDouble :: Seed -> (Double, Seed)
nextDouble s0 =
  let (w, s1) = nextWord64 s0
  in (fromIntegral (w `shiftR` 11) * twoPowMinus53, s1)
  where
    twoPowMinus53 :: Double
    twoPowMinus53 = 1.0 / 9007199254740992.0  -- 1 / 2^53

-- | Uniform in @[lo, hi)@. Arguments given the wrong way round are
-- swapped rather than rejected, so this stays total.
nextRange :: Double -> Double -> Seed -> (Double, Seed)
nextRange a b s0 =
  let lo = min a b
      hi = max a b
      (u, s1) = nextDouble s0
  in (lo + u * (hi - lo), s1)

-- | Uniform integer in @[0, n)@. For @n <= 1@ the only valid answer is
-- @0@, returned without consuming randomness.
--
-- Uses plain modulo. The resulting bias is at most @n / 2^64@ — for the
-- population sizes and tree sizes here (@n@ in the thousands at most) that
-- is around @1e-16@, far below any effect the search could notice, and
-- rejection sampling would cost a variable number of steps for nothing.
nextInt :: Int -> Seed -> (Int, Seed)
nextInt n s0
  | n <= 1    = (0, s0)
  | otherwise =
      let (w, s1) = nextWord64 s0
      in (fromIntegral (w `mod` fromIntegral n), s1)

-- | A fair coin.
nextBool :: Seed -> (Bool, Seed)
nextBool s0 =
  let (w, s1) = nextWord64 s0
  in (odd (w `shiftR` 32), s1)

-- | 'True' with probability @p@. @p <= 0@ never fires and @p >= 1@ always
-- does, both without special-casing, because 'nextDouble' lands in
-- @[0, 1)@.
chance :: Double -> Seed -> (Bool, Seed)
chance p s0 =
  let (u, s1) = nextDouble s0
  in (u < p, s1)

-- | Choose one element uniformly. 'Nothing' for an empty list — the
-- 'Maybe' is what keeps this out of partial-function territory, and it
-- forces callers to say what an empty candidate set should mean.
pick :: [a] -> Seed -> Maybe (a, Seed)
pick [] _  = Nothing
pick xs s0 =
  let (i, s1) = nextInt (length xs) s0
  in fmap (\x -> (x, s1)) (safeIndex i xs)

-- | A standard normal deviate: mean 0, variance 1.
--
-- Box-Muller, which turns two uniforms into two independent normals; only
-- one is kept, since returning the pair would make the state threading
-- awkward for a caller that wants a single value. Wasting one deviate is
-- cheaper than the bookkeeping to cache it.
--
-- The @max@ guards against @u1 == 0@, which 'nextDouble' can return (its
-- range is half-open at the bottom) and which would make @log@ produce an
-- infinity.
nextGaussian :: Seed -> (Double, Seed)
nextGaussian s0 =
  let (u1, s1) = nextDouble s0
      (u2, s2) = nextDouble s1
      radius = sqrt (negate 2.0 * log (max smallestPositive u1))
      angle = 2.0 * pi * u2
  in (radius * cos angle, s2)
  where
    smallestPositive :: Double
    smallestPositive = 1.0e-300

-- | Choose one element with probability proportional to its weight.
--
-- Entries with a non-positive weight are excluded rather than clamped, so
-- a weight of zero is a clean way to disable a choice without removing it
-- from a registry.
--
-- 'Nothing' when nothing is selectable — an empty list, or every weight
-- zero. Both are real situations a caller has to decide about, so neither
-- is silently turned into an arbitrary pick.
pickWeighted :: forall a. [(Double, a)] -> Seed -> Maybe (a, Seed)
pickWeighted entries s0 = case usable of
  [] -> Nothing
  ((_, firstValue) : _) ->
    let (u, s1) = nextDouble s0
        target = u * total
    in case walk 0.0 target usable of
         Just value -> Just (value, s1)
         -- Only reachable through floating-point round-off leaving the
         -- running total a hair under the target on the final entry.
         Nothing -> Just (firstValue, s1)
  where
    usable :: [(Double, a)]
    usable = [entry | entry@(w, _) <- entries, w > 0, not (isNaN w)]

    total :: Double
    total = sum (map fst usable)

    walk :: Double -> Double -> [(Double, a)] -> Maybe a
    walk _ _ [] = Nothing
    walk running target ((w, value) : rest)
      | running' >= target = Just value
      | otherwise          = walk running' target rest
      where
        running' = running + w

-- | Uniform random permutation (Fisher–Yates, adapted to lists).
--
-- Quadratic in the list length because removing element @i@ walks the
-- prefix. Fine at the sizes used here; if profiling ever says otherwise,
-- this is the function to move onto a mutable array.
shuffle :: [a] -> Seed -> ([a], Seed)
shuffle xs0 s0 = go xs0 (length xs0) s0
  where
    go :: [a] -> Int -> Seed -> ([a], Seed)
    go [] _ st = ([], st)
    go ys n st =
      let (i, st1) = nextInt n st
          (chosen, rest) = removeAt i ys
      in case chosen of
           -- Unreachable: n is the length of ys, which is non-empty here,
           -- so nextInt returns an index strictly inside ys.
           Nothing -> (ys, st1)
           Just c  ->
             let (more, st2) = go rest (n - 1) st1
             in (c : more, st2)

-- | Split out the element at an index, if it exists.
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs
  | n < 0     = (Nothing, xs)
  | otherwise = case splitAt n xs of
      (before, y : after) -> (Just y, before ++ after)
      (_, [])             -> (Nothing, xs)

safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs
  | n < 0     = Nothing
  | otherwise = case drop n xs of
      (x : _) -> Just x
      []      -> Nothing
