-- | Tests for the PRNG, standing entirely on its own.
--
-- Nothing here imports 'TypedGP.Expr' or anything else from the genetic
-- programming side. If the search misbehaves, these tests tell you whether
-- the generator is the reason — which is exactly the isolation
-- "TypedGP.Random" was structured for.
--
-- The distribution checks use generous bands. They are a smoke test for
-- "this generator is not secretly broken, biased or short-cycled", not a
-- statistical certification; a tight band would make the suite flaky for
-- no added information.
module RandomSpec (tests) where

import Data.List (nub, sort)
import Data.Word (Word64)

import TypedGP.Random
  ( Seed
  , chance
  , mkSeed
  , nextBool
  , nextDouble
  , nextInt
  , nextRange
  , nextWord64
  , pick
  , pickWeighted
  , seedWord
  , shuffle
  , splitSeed
  )

-- | Draw @n@ values by iterating a state-threading generator.
samplesOf :: (Seed -> (a, Seed)) -> Int -> Seed -> [a]
samplesOf step n s0 = go n s0 []
  where
    go 0 _ acc = reverse acc
    go k s acc =
      let (x, s') = step s
      in go (k - 1) s' (x : acc)

sampleCount :: Int
sampleCount = 200000

uniforms :: [Double]
uniforms = samplesOf nextDouble sampleCount (mkSeed 12345)

mean :: [Double] -> Double
mean [] = 0.0
mean xs = sum xs / fromIntegral (length xs)

-- | Population variance.
variance :: [Double] -> Double
variance [] = 0.0
variance xs =
  let m = mean xs
  in mean (map (\x -> (x - m) * (x - m)) xs)

-- | Counts of @nextInt buckets@ draws falling in each bucket.
bucketCounts :: Int -> Int -> Seed -> [Int]
bucketCounts buckets draws s0 =
  [ length (filter (== b) sampled) | b <- [0 .. buckets - 1] ]
  where
    sampled :: [Int]
    sampled = samplesOf (nextInt buckets) draws s0

tests :: [(String, Bool)]
tests =
  -- Determinism -----------------------------------------------------------
  [ ("the same seed produces the same stream",
      samplesOf nextWord64 100 (mkSeed 7) == samplesOf nextWord64 100 (mkSeed 7))
  , ("different seeds produce different streams",
      samplesOf nextWord64 10 (mkSeed 1) /= samplesOf nextWord64 10 (mkSeed 2))
  , ("adjacent seeds diverge immediately (avalanche works)",
      firstWord (mkSeed 1) /= firstWord (mkSeed 2))
  , ("seed 0 does not collapse the state",
      seedWord (mkSeed 0) /= 0
        && length (nub (samplesOf nextWord64 50 (mkSeed 0))) == 50)

  -- Range -----------------------------------------------------------------
  , ("every uniform lands in [0, 1)", all (\u -> u >= 0.0 && u < 1.0) uniforms)
  , ("nextRange respects its bounds",
      all (\v -> v >= (-2.0) && v < 5.0)
        (samplesOf (nextRange (-2.0) 5.0) 20000 (mkSeed 99)))
  , ("nextRange swaps inverted bounds instead of failing",
      all (\v -> v >= 1.0 && v < 4.0)
        (samplesOf (nextRange 4.0 1.0) 5000 (mkSeed 21)))

  -- Distribution ----------------------------------------------------------
  , ("the mean is near 0.5", abs (mean uniforms - 0.5) < 0.005)
  , ("the variance is near 1/12",
      abs (variance uniforms - (1.0 / 12.0)) < 0.002)
  , ("the two halves of [0, 1) are evenly used",
      let lower = length (filter (< 0.5) uniforms)
      in abs (fromIntegral lower - fromIntegral sampleCount / 2.0)
           < (fromIntegral sampleCount :: Double) * 0.01)
  , ("no value repeats over a short window (no short cycle)",
      length (nub (samplesOf nextWord64 5000 (mkSeed 555))) == 5000)

  -- Integers --------------------------------------------------------------
  , ("nextInt stays inside its bound",
      all (\i -> i >= 0 && i < 10) (samplesOf (nextInt 10) 20000 (mkSeed 3)))
  , ("nextInt fills every bucket evenly",
      all withinBucketBand (bucketCounts 10 60000 (mkSeed 17)))
  , ("nextInt 1 is always 0 and consumes nothing",
      nextInt 1 (mkSeed 4) == (0, mkSeed 4))
  , ("nextInt 0 is total", fst (nextInt 0 (mkSeed 4)) == 0)
  , ("nextInt of a negative bound is total",
      fst (nextInt (-5) (mkSeed 4)) == 0)

  -- Booleans --------------------------------------------------------------
  , ("nextBool is balanced",
      let flips = samplesOf nextBool 40000 (mkSeed 8)
          heads = length (filter id flips)
      in abs (fromIntegral heads - 20000.0 :: Double) < 800.0)
  , ("chance 0 never fires",
      not (or (samplesOf (chance 0.0) 1000 (mkSeed 11))))
  , ("chance 1 always fires",
      and (samplesOf (chance 1.0) 1000 (mkSeed 11)))
  , ("chance 0.25 fires about a quarter of the time",
      let hits = length (filter id (samplesOf (chance 0.25) 40000 (mkSeed 12)))
      in abs (fromIntegral hits - 10000.0 :: Double) < 600.0)

  -- Collections -----------------------------------------------------------
  , ("pick on an empty list is Nothing", pick ([] :: [Int]) (mkSeed 1) == Nothing)
  , ("pick returns a member of the list",
      all (`elem` smallList) (samplesOf pickFromSmall 2000 (mkSeed 13)))
  , ("pick reaches every element",
      sort (nub (samplesOf pickFromSmall 4000 (mkSeed 14))) == smallList)
  , ("shuffle is a permutation",
      sort (fst (shuffle smallList (mkSeed 15))) == smallList)
  , ("shuffle preserves length",
      length (fst (shuffle [1 .. 50 :: Int] (mkSeed 16))) == 50)
  , ("shuffle actually reorders",
      fst (shuffle [1 .. 50 :: Int] (mkSeed 16)) /= [1 .. 50])
  , ("shuffle of an empty list is empty",
      fst (shuffle ([] :: [Int]) (mkSeed 1)) == [])
  , ("shuffle of a singleton is itself",
      fst (shuffle [42 :: Int] (mkSeed 1)) == [42])

  -- Weighted selection ----------------------------------------------------
  , ("weighted picking on an empty list is Nothing",
      pickWeighted ([] :: [(Double, Int)]) (mkSeed 1) == Nothing)
  , ("weighted picking with all-zero weights is Nothing",
      pickWeighted [(0.0, 1 :: Int), (0.0, 2)] (mkSeed 1) == Nothing)
  , ("weighted picking never returns a zero-weighted entry",
      notElem (2 :: Int)
        (samplesOf (weightedFrom [(1.0, 1), (0.0, 2), (1.0, 3)]) 2000 (mkSeed 5)))
  , ("weighted picking with one entry always returns it",
      all (== (7 :: Int))
        (samplesOf (weightedFrom [(0.25, 7)]) 500 (mkSeed 6)))
  , ("weighted picking respects the weights",
      let draws = samplesOf (weightedFrom [(3.0, 1 :: Int), (1.0, 2)]) 40000 (mkSeed 7)
          ones = length (filter (== 1) draws)
      -- 3:1 odds means 75%; +/- 2% is far outside sampling noise at n=40000.
      in abs (fromIntegral ones / 40000.0 - 0.75 :: Double) < 0.02)
  , ("weighted picking reaches a heavily disfavoured entry eventually",
      elem (2 :: Int)
        (samplesOf (weightedFrom [(100.0, 1), (1.0, 2)]) 5000 (mkSeed 8)))
  , ("weighted picking ignores negative weights",
      notElem (9 :: Int)
        (samplesOf (weightedFrom [(1.0, 1), (-5.0, 9)]) 1000 (mkSeed 9)))

  -- Splitting -------------------------------------------------------------
  , ("splitSeed yields two distinct generators",
      let (branch, continued) = splitSeed (mkSeed 31)
      in branch /= continued)
  , ("a split branch differs from the original stream",
      let (branch, _) = splitSeed (mkSeed 31)
      in samplesOf nextWord64 20 branch /= samplesOf nextWord64 20 (mkSeed 31))
  , ("splitSeed is deterministic", splitSeed (mkSeed 31) == splitSeed (mkSeed 31))
  ]
  where
    firstWord :: Seed -> Word64
    firstWord = fst . nextWord64

    smallList :: [Int]
    smallList = [1 .. 6]

    pickFromSmall :: Seed -> (Int, Seed)
    pickFromSmall s = case pick smallList s of
      Just result -> result
      -- smallList is a non-empty literal, so this cannot happen; returning
      -- a sentinel outside the list would fail the membership assertions
      -- loudly rather than silently passing.
      Nothing     -> (-1, s)

    -- Adapts pickWeighted to the (Seed -> (a, Seed)) shape samplesOf
    -- wants, substituting a sentinel outside the candidate set so a
    -- failure fails the membership assertions loudly.
    weightedFrom :: [(Double, Int)] -> Seed -> (Int, Seed)
    weightedFrom entries s = case pickWeighted entries s of
      Just result -> result
      Nothing     -> (-1, s)

    -- 60000 draws over 10 buckets expects 6000 each; +/- 400 is roughly
    -- five standard deviations (sigma ~ 73), so a correct generator will
    -- not trip this, and a badly biased one will.
    withinBucketBand :: Int -> Bool
    withinBucketBand count = count > 5600 && count < 6400
