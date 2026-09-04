-- | Tests for periodic-structure detection.
--
-- The detector is the whole basis of frequency seeding, so it is tested in
-- isolation against signals whose answer is known by construction. A
-- detector that returned plausible-looking noise would produce a search
-- that looks biased and is not, and nothing downstream would say so.
--
-- Frequencies are asserted in __generator units__ — the @c@ in
-- @sin(c*x + d)@ — because that is what 'dominantFrequencies' returns and
-- what the generator consumes. The transform works in @f@ where a
-- component is @exp(-2*pi*i*f*x)@, and the two differ by @2*pi@; getting
-- that conversion wrong would give a detector that finds the right peak
-- and reports a useless number.
module SpectralSpec (tests) where

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Data.Dataset (Dataset, mkDataset)
import TypedGP.Spectral (detrend, dominantFrequencies, frequencyTable, frequenciesFor)

cfg :: Config
cfg = defaultConfig { cfgVariables = ["x"] }

-- | Evenly spaced inputs over a few periods, which is the friendliest case
-- for detection; the irregular case is covered separately below.
evenInputs :: Int -> Double -> [Double]
evenInputs count width =
  [width * fromIntegral k / fromIntegral count | k <- [0 .. count - 1]]

datasetFrom :: [Double] -> (Double -> Double) -> Maybe Dataset
datasetFrom xs f = case mkDataset ["x"] [([x], f x) | x <- xs] of
  Right ds -> Just ds
  Left _   -> Nothing

detectOn :: Maybe Dataset -> [Double]
detectOn = maybe [] (\ds -> dominantFrequencies cfg ds "x")

topFrequency :: Maybe Dataset -> Maybe Double
topFrequency ds = case detectOn ds of
  (f : _) -> Just f
  []      -> Nothing

near :: Double -> Double -> Double -> Bool
near tolerance actual expected = abs (actual - expected) <= tolerance

-- Fixtures -------------------------------------------------------------------

-- | @sin(3x)@ over three full periods, 200 evenly spaced points.
cleanSine :: Maybe Dataset
cleanSine = datasetFrom (evenInputs 200 (2.0 * pi)) (\x -> sin (3.0 * x))

-- | The non-integer case the README already flags as a start point the
-- optimiser can close from.
awkwardSine :: Maybe Dataset
awkwardSine = datasetFrom (evenInputs 200 (2.0 * pi)) (\x -> sin (2.7 * x))

-- | A sine riding on a strong linear trend, which detrending must remove
-- before it swamps the peak.
trendedSine :: Maybe Dataset
trendedSine =
  datasetFrom (evenInputs 200 (2.0 * pi)) (\x -> 8.0 * x + sin (3.0 * x))

-- | Deterministic pseudo-noise with no periodic structure.
pureNoise :: Maybe Dataset
pureNoise = datasetFrom (evenInputs 200 (2.0 * pi)) jitter
  where
    jitter :: Double -> Double
    jitter x =
      let mixed = sin (x * 12.9898) * 43758.5453
      in mixed - fromIntegral (floor mixed :: Int) - 0.5

-- | A straight line: no periodicity at all, and detrending should leave
-- essentially nothing behind.
pureLine :: Maybe Dataset
pureLine = datasetFrom (evenInputs 200 (2.0 * pi)) (\x -> 3.0 * x + 1.0)

-- | A constant target: degenerate, and must not divide by zero anywhere.
flatTarget :: Maybe Dataset
flatTarget = datasetFrom (evenInputs 200 (2.0 * pi)) (const 5.0)

-- | The benchmark suite's @mixed@ target, @2x^2 + exp(-x) + sin(y)@, viewed
-- from @y@.
--
-- This exists because a pre-registered prediction about it was wrong, and
-- the correction deserves a test rather than a sentence. The design note
-- predicted the detector would find /nothing/ for @y@ here, since the
-- @x@ terms are far larger than the unit-amplitude @sin(y)@ and are not
-- linear in @y@, so detrending cannot remove them. The 20-seed validation
-- showed the two arms differing, which is only possible if something was
-- detected — so the strong form of the prediction was false.
--
-- @x@ is scrambled rather than swept alongside @y@ so the two are
-- uncorrelated, as they are when the suite samples them independently. If
-- they were correlated the @x@ terms would be partly removable by
-- detrending and the test would be measuring the wrong thing.
mixedTarget :: Maybe Dataset
mixedTarget =
  case mkDataset ["x", "y"] [(inputs k, value k) | k <- [0 .. 199 :: Int]] of
    Right ds -> Just ds
    Left _   -> Nothing
  where
    yAt, xAt :: Int -> Double
    yAt k = -3.0 + 6.0 * fromIntegral k / 199.0
    -- A golden-ratio stride: deterministic, and decorrelated from y.
    xAt k =
      let t = fromIntegral k * 0.6180339887498949
      in -3.0 + 6.0 * (t - fromIntegral (floor t :: Int))

    -- Positional, matching the variable list given to 'mkDataset'.
    inputs :: Int -> [Double]
    inputs k = [xAt k, yAt k]
    value k =
      let x = xAt k
      in 2.0 * x * x + exp (negate x) + sin (yAt k)

mixedCfg :: Config
mixedCfg = defaultConfig { cfgVariables = ["x", "y"] }

tests :: [(String, Bool)]
tests =
  -- Fixtures build -----------------------------------------------------------
  [ ("the fixtures build", all (/= Nothing) [cleanSine, awkwardSine, pureNoise])

  -- Detection ----------------------------------------------------------------
    -- The headline: a clean sin(3x) must report a multiplier near 3, in
    -- generator units. A detector reporting 3/(2*pi) would pass a
    -- "found the peak" test and still be useless.
  , ("a clean sin(3x) is detected at 3",
      maybe False (\f -> near 0.1 f 3.0) (topFrequency cleanSine))
  , ("a non-integer frequency is detected close",
      maybe False (\f -> near 0.1 f 2.7) (topFrequency awkwardSine))
  , ("detection survives a strong linear trend",
      maybe False (\f -> near 0.1 f 3.0) (topFrequency trendedSine))
  , ("at most top-k frequencies are returned",
      length (detectOn cleanSine) <= cfgFrequencySeedTopK cfg)
  , ("detected frequencies are positive", all (> 0) (detectOn cleanSine))
  , ("detection is deterministic", detectOn cleanSine == detectOn cleanSine)

  -- No-signal cases ----------------------------------------------------------
    -- The guarantee the whole design rests on: no spurious peak, so
    -- generation falls back to exactly its unbiased behaviour.
  , ("pure noise reports no signal", null (detectOn pureNoise))
  , ("a straight line reports no signal", null (detectOn pureLine))
  , ("a constant target reports no signal", null (detectOn flatTarget))
  , ("too few points reports no signal",
      null (detectOn (datasetFrom [0.0, 1.0, 2.0] (\x -> sin (3.0 * x)))))
  , ("an empty dataset reports no signal",
      null (detectOn (datasetFrom [] (\x -> sin (3.0 * x)))))

  -- The multivariate case ----------------------------------------------------
  , ("the mixed fixture builds", mixedTarget /= Nothing)
    -- The design note's prediction, confirmed: the real sin(y) signal is
    -- invisible under the much larger x terms, which detrending cannot
    -- remove because they are not linear in y.
  , ("the true sin(y) signal is not detected in 2x^2 + exp(-x) + sin(y)",
      maybe False
        (\ds -> null (dominantFrequencies mixedCfg ds "y"))
        mixedTarget)
    -- And the other half of the story, which the prediction missed. The
    -- benchmark's two seeding arms produced different recovery rates on
    -- `mixed`, which is impossible if nothing at all is detected. The
    -- difference comes from x: 2x^2 + exp(-x) is aperiodic, but it is not
    -- flat after detrending either, and its residual has enough shape to
    -- push a peak past the threshold. Seeding then biases trig arguments
    -- towards a frequency that does not exist in the target.
    --
    -- This is why the +10pp on `mixed` is reported as a perturbation
    -- rather than as the mechanism working.
  , ("but a spurious frequency is detected for x, which has no periodicity",
      maybe False
        (\ds -> not (null (dominantFrequencies mixedCfg ds "x")))
        mixedTarget)

  -- Detrending ---------------------------------------------------------------
  , ("detrending removes a pure line",
      all (\(_, y) -> abs y < 1.0e-9)
        (detrend [(x, 3.0 * x + 1.0) | x <- evenInputs 50 10.0]))
  , ("detrending preserves the number of points",
      length (detrend [(x, x) | x <- evenInputs 50 10.0]) == 50)
  , ("detrending a degenerate x leaves the data alone",
      detrend [(1.0, 2.0), (1.0, 5.0)] == [(1.0, 2.0), (1.0, 5.0)])
  , ("detrending an empty sample is empty", null (detrend []))

  -- The table ----------------------------------------------------------------
  , ("the table is empty when seeding is disabled",
      maybe False
        (\ds -> null (frequencyTable cfg { cfgFrequencySeedingEnabled = False } ds))
        cleanSine)
  , ("the table carries the detected frequency",
      maybe False
        (\ds -> case frequenciesFor (frequencyTable cfg ds) "x" of
                  (f : _) -> near 0.1 f 3.0
                  []      -> False)
        cleanSine)
  , ("an unknown variable has no frequencies",
      maybe False
        (\ds -> null (frequenciesFor (frequencyTable cfg ds) "nosuchvar"))
        cleanSine)
    -- The threshold must be doing real work: raised high enough, even a
    -- clean sine is rejected. If this passes at any threshold the detector
    -- is not actually consulting it.
  , ("a high enough threshold rejects even a clean sine",
      maybe False
        (\ds -> null (dominantFrequencies
                        cfg { cfgFrequencySignalThreshold = 1.0e6 } ds "x"))
        cleanSine)
  ]
