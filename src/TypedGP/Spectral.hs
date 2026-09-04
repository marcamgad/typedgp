-- | Detecting periodic structure in a dataset, so that generation can seed
-- trigonometric arguments with plausible frequencies.
--
-- == Why this exists
--
-- @2 sin(3x + 0.5)@ is the benchmark suite's hardest trigonometric target,
-- and the reason is understood rather than mysterious. From
-- @LocalSearchSpec@: started at frequency 1 the simplex does __not__
-- recover frequency 3, and started at 2.7 the __same optimiser__ recovers
-- all three constants exactly. The optimiser is not the weak link.
-- Frequency estimation is /multimodal/ — the error surface has a local
-- minimum near every frequency — and no local method crosses the barriers
-- between basins however good it is.
--
-- The fix therefore has to put the search in the right basin to begin
-- with, which means initialisation. This module supplies the candidate
-- frequencies; "TypedGP.Gen.Grow" biases towards them.
--
-- == The transform
--
-- A direct sum rather than a gridded FFT:
--
-- > S(f) = sum_k  y_k * exp(-2*pi*i*f*x_k)
--
-- evaluated over the __actual sample points__. This matters: a standard
-- DFT assumes evenly spaced samples, and the benchmark draws its inputs
-- uniformly at random, so they are not. The direct form is well defined
-- for arbitrary @x_k@ and costs the same. It does not remove the raised
-- noise floor that irregular sampling produces — that is what
-- 'TypedGP.Config.cfgFrequencySignalThreshold' is for.
--
-- Cost is a non-issue and that is what makes the naive form the right
-- choice: this runs __once per run__, against datasets of 60-400 points.
-- A radix-2 Cooley-Tukey would optimise something that does not appear in
-- the profile, and would need power-of-two padding with its own spectral
-- artefacts.
--
-- See @docs\/phase2-spectral-design.md@ for the alternatives considered.
module TypedGP.Spectral
  ( -- * Detection
    FrequencyTable
  , dominantFrequencies
  , frequencyTable
  , frequenciesFor

    -- * Internals, exposed for testing
  , detrend
  , spectrumAt
  , candidateFrequencies
  ) where

import Data.List (sortOn)

import TypedGP.Config (Config (..))
import TypedGP.Data.Dataset (DataPoint (..), Dataset, datasetPoints)
import TypedGP.Expr (VarName)

-- | Candidate frequencies per input variable.
--
-- An absent variable, or one mapped to the empty list, means "no periodic
-- signal detected" and generation falls back to its unbiased behaviour
-- exactly.
type FrequencyTable = [(VarName, [Double])]

-- | Build the table for every variable in the config.
--
-- Called __once per run__ from "TypedGP.Evolution"'s setup. Not per
-- generation, and not because of cost: the table is a property of the
-- /dataset/, and recomputing it per generation would invite it to later
-- depend on population state, which would couple initialisation to search
-- and destroy the isolation that makes this change risk-free.
frequencyTable :: Config -> Dataset -> FrequencyTable
frequencyTable cfg ds
  | not (cfgFrequencySeedingEnabled cfg) = []
  | otherwise = [(name, dominantFrequencies cfg ds name) | name <- cfgVariables cfg]

-- | Look up one variable's frequencies. Empty when nothing was detected.
frequenciesFor :: FrequencyTable -> VarName -> [Double]
frequenciesFor table name = case lookup name table of
  Just fs -> fs
  Nothing -> []

-- | The top-k candidate frequencies for one variable, or an empty list if
-- no peak stands out from the noise floor.
--
-- __Returned in generator units__, not in transform units. The transform
-- works in @f@ where a component looks like @exp(-2*pi*i*f*x)@, while the
-- generator emits @sin(c*x + d)@; the two differ by a factor of @2*pi@.
-- Returning raw @f@ would hand the generator a multiplier 6.28 times too
-- small, which would look like a working detector producing useless
-- constants — so the conversion happens here, once, and callers can use
-- these values directly as @c@.
dominantFrequencies :: Config -> Dataset -> VarName -> [Double]
dominantFrequencies cfg ds name
  | length samples < minimumSamples = []
  | peakMagnitude <= 0 = []
  -- The signal test: the strongest peak against the *median* magnitude,
  -- not the mean. The mean is dragged upward by the very peak being
  -- tested, which would make the test partly self-referential and let a
  -- flat spectrum pass.
  | peakMagnitude < cfgFrequencySignalThreshold cfg * noiseFloor = []
  | otherwise = take (max 1 (cfgFrequencySeedTopK cfg)) rankedFrequencies
  where
    samples :: [(Double, Double)]
    samples = detrend (samplesFor ds name)

    spectrum :: [(Double, Double)]
    spectrum = [(f, spectrumAt samples f) | f <- candidateFrequencies cfg samples]

    magnitudes :: [Double]
    magnitudes = map snd spectrum

    peakMagnitude :: Double
    peakMagnitude = maximum (0 : magnitudes)

    noiseFloor :: Double
    noiseFloor = max tinyMagnitude (medianOf magnitudes)

    -- Converted from transform units to generator units; see the note on
    -- this function.
    rankedFrequencies :: [Double]
    rankedFrequencies =
      [2.0 * pi * f | (f, _) <- sortOn (negate . snd) spectrum]

-- | Below this many points a spectrum is not meaningful at any threshold.
minimumSamples :: Int
minimumSamples = 8

-- | Floor on the noise estimate, so a spectrum that is identically zero
-- cannot make the threshold ratio infinite.
tinyMagnitude :: Double
tinyMagnitude = 1.0e-12

-- | @(x, y)@ pairs for one variable against the target.
samplesFor :: Dataset -> VarName -> [(Double, Double)]
samplesFor ds name =
  [ (x, dpTarget point)
  | point <- datasetPoints ds
  , Just x <- [lookup name (dpInputs point)]
  ]

-- | Subtract the least-squares line through the samples.
--
-- Without this, any linear trend in the target produces a large spurious
-- peak at the lowest frequencies that can easily outrank a genuine one.
-- Closed-form OLS; a degenerate spread (every @x@ identical) leaves the
-- data untouched rather than dividing by zero.
detrend :: [(Double, Double)] -> [(Double, Double)]
detrend samples
  | count < 2 = samples
  | abs varianceX < tinyMagnitude = samples
  | otherwise = [(x, y - (intercept + slope * x)) | (x, y) <- samples]
  where
    count :: Double
    count = fromIntegral (length samples)

    meanX, meanY :: Double
    meanX = sum (map fst samples) / count
    meanY = sum (map snd samples) / count

    varianceX :: Double
    varianceX = sum [(x - meanX) * (x - meanX) | (x, _) <- samples]

    covariance :: Double
    covariance = sum [(x - meanX) * (y - meanY) | (x, y) <- samples]

    slope :: Double
    slope = covariance / varianceX

    intercept :: Double
    intercept = meanY - slope * meanX

-- | Magnitude of the transform at one frequency.
--
-- Normalised by the sample count so the threshold ratio does not depend on
-- dataset size.
spectrumAt :: [(Double, Double)] -> Double -> Double
spectrumAt samples frequency
  | null samples = 0.0
  | otherwise = sqrt (real * real + imaginary * imaginary) / count
  where
    count :: Double
    count = fromIntegral (length samples)

    angle :: Double -> Double
    angle x = 2.0 * pi * frequency * x

    real :: Double
    real = sum [y * cos (angle x) | (x, y) <- samples]

    imaginary :: Double
    imaginary = negate (sum [y * sin (angle x) | (x, y) <- samples])

-- | The frequency grid the spectrum is evaluated on.
--
-- Expressed in the same units as the multiplier the generator will produce
-- — that is, @c@ in @sin(c*x + d)@ — so the resolution is chosen against
-- the range of constants the search would otherwise draw uniformly, not
-- against an abstract Nyquist limit that irregular sampling does not have
-- anyway.
--
-- The upper bound comes from the configured constant range, because a
-- detected frequency the generator could never have produced is useless.
candidateFrequencies :: Config -> [(Double, Double)] -> [Double]
candidateFrequencies cfg samples
  | null samples = []
  | otherwise =
      [ fromIntegral step * resolution
      | step <- [1 .. steps]
      ]
  where
    (low, high) = cfgConstRange cfg

    -- In S(f) above, f multiplies 2*pi*x, whereas the generator's constant
    -- multiplies x directly. Dividing by 2*pi converts between them, so a
    -- reported frequency can be used as a constant unchanged.
    maximumFrequency :: Double
    maximumFrequency = max 1.0 (max (abs low) (abs high)) / (2.0 * pi)

    steps :: Int
    steps = max 1 (cfgFrequencyResolution cfg)

    resolution :: Double
    resolution = maximumFrequency / fromIntegral steps

-- | Median of a sample. Zero for an empty one.
medianOf :: [Double] -> Double
medianOf [] = 0.0
medianOf values =
  let ordered = sortOn id values
      count = length ordered
      half = count `div` 2
  in if even count
       then 0.5 * (elemAt (half - 1) ordered + elemAt half ordered)
       else elemAt half ordered
  where
    elemAt :: Int -> [Double] -> Double
    elemAt index xs = case drop index xs of
      (v : _) -> v
      -- Unreachable: every index used above is inside the list.
      []      -> 0.0
