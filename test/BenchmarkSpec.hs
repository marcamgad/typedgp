-- | Tests for the benchmark suite's machinery.
--
-- These check the /measuring instrument/, not the search. A benchmark that
-- silently generates degenerate data, or whose "noisy" variant adds no
-- noise, would report confident numbers that mean nothing — and because
-- the whole point of the suite is to decide whether engine changes helped,
-- a broken instrument is worse than no instrument.
--
-- Nothing here asserts a recovery rate. Those are search outcomes: they
-- move with tuning, they are heavy-tailed across seeds, and pinning them
-- in a unit test would make the suite fail for reasons unrelated to
-- correctness.
module BenchmarkSpec (tests) where

import Data.List (nub)

import TypedGP.Benchmark
  ( Problem (..)
  , Variant (..)
  , applyVariant
  , generateClean
  , generateNoisy
  , median
  , normalisedRmse
  , problemsNamed
  , exactThreshold
  , recoveryThreshold
  , suite
  , variantName
  , variants
  )
import TypedGP.Data.Dataset (DataPoint (..), Dataset, datasetPoints, datasetSize)
import TypedGP.Random (Seed, mkSeed)

-- | Extract a dataset, or an empty stand-in. Generation failure is itself
-- asserted against below, so a silent fallback here cannot hide one.
datasetOr :: (Either String Dataset, Seed) -> Maybe Dataset
datasetOr (Right ds, _) = Just ds
datasetOr (Left _, _)   = Nothing

cleanOf :: Problem -> Int -> Maybe Dataset
cleanOf problem count =
  datasetOr (generateClean problem (probDomain problem) count (mkSeed 5))

noisyOf :: Problem -> Int -> Maybe Dataset
noisyOf problem count = datasetOr (generateNoisy problem count (mkSeed 5))

targetsOf :: Maybe Dataset -> [Double]
targetsOf = maybe [] (map dpTarget . datasetPoints)

finite :: Double -> Bool
finite v = not (isNaN v) && not (isInfinite v)

-- | Deviations introduced by a variant, paired with the clean target they
-- were applied to.
deviations :: Variant -> Problem -> [(Double, Double)]
deviations variant problem =
  zip (targetsOf (cleanOf problem 300)) (targetsOf (noisyOf (applyVariant variant problem) 300))

-- | The polynomial problem is the fixture for the noise tests: it has a
-- wide, smoothly varying target, so a change in spread is easy to see.
polynomial :: Maybe Problem
polynomial = case problemsNamed ["polynomial"] of
  (p : _) -> Just p
  []      -> Nothing

withPolynomial :: (Problem -> Bool) -> Bool
withPolynomial f = maybe False f polynomial

-- | Mean absolute deviation over the half of the sample with the smallest
-- targets, versus the half with the largest. Heteroscedastic noise should
-- make the second visibly bigger.
spreadByMagnitude :: [(Double, Double)] -> (Double, Double)
spreadByMagnitude pairs =
  let ordered = sortByMagnitude pairs
      half = length ordered `div` 2
      low = take half ordered
      high = drop half ordered
  in (meanAbsolute low, meanAbsolute high)
  where
    sortByMagnitude :: [(Double, Double)] -> [(Double, Double)]
    sortByMagnitude = foldr insertByMagnitude []

    insertByMagnitude p [] = [p]
    insertByMagnitude p (q : qs)
      | abs (fst p) <= abs (fst q) = p : q : qs
      | otherwise = q : insertByMagnitude p qs

    meanAbsolute :: [(Double, Double)] -> Double
    meanAbsolute [] = 0.0
    meanAbsolute xs = sum [abs (b - a) | (a, b) <- xs] / fromIntegral (length xs)

tests :: [(String, Bool)]
tests =
  -- The suite itself -------------------------------------------------------
  [ ("the suite spans nine problems", length suite == 9)
  , ("problem names are unique",
      length (nub (map probName suite)) == length suite)
  , ("families are distinguishable",
      length (nub (map probFamily suite)) >= 8)
  , ("every problem declares its formula", all (not . null . probFormula) suite)
  , ("every problem has at least one variable",
      all (not . null . probVariables) suite)
  , ("relevant variables are a subset of all variables",
      all (\p -> all (`elem` probVariables p) (probRelevant p)) suite)
  , ("clean problems have no distractors",
      all (\p -> probRelevant p == probVariables p) suite)
  , ("extrapolation ranges differ from training ranges",
      all (\p -> probExtrapolation p /= probDomain p) suite)
  , ("filtering by name works",
      map probName (problemsNamed ["nested"]) == ["nested"])
  , ("filtering by family works",
      not (null (problemsNamed ["trigonometric"])))
  , ("an empty filter returns everything",
      length (problemsNamed []) == length suite)
  , ("every variant has a distinct name",
      length (nub (map variantName variants)) == length variants)

  -- Data generation --------------------------------------------------------
  , ("every problem generates the requested number of examples",
      all (\p -> fmap datasetSize (cleanOf p 50) == Just 50) suite)
  , ("every problem generates a row per declared variable",
      all rowShapeMatches suite)
  , ("every truth is finite across its whole domain",
      all (\p -> all finite (targetsOf (cleanOf p 200))) suite)
  , ("every truth actually varies",
      all (\p -> length (nub (targetsOf (cleanOf p 50))) > 10) suite)
  , ("extrapolation samples are also finite",
      all extrapolationFinite suite)
  , ("generation is deterministic",
      targetsOf (cleanOf' 5) == targetsOf (cleanOf' 5))
  , ("different seeds give different samples",
      targetsOf (cleanOf' 5) /= targetsOf (cleanOf' 6))

  -- Noise models -----------------------------------------------------------
  , ("the clean variant adds no noise",
      withPolynomial (\p -> all (\(a, b) -> a == b) (deviations Clean p)))
  , ("the noisy variant actually perturbs the targets",
      withPolynomial (\p -> any (\(a, b) -> a /= b) (deviations Noisy p)))
  , ("noise is small relative to the signal",
      withPolynomial (\p ->
        let ds = deviations Noisy p
            typical = sum [abs (b - a) | (a, b) <- ds] / fromIntegral (length ds)
            spread = sum (map (abs . fst) ds) / fromIntegral (length ds)
        in typical > 0 && typical < 0.5 * spread))
  , ("noisy targets stay finite",
      withPolynomial (\p -> all (finite . snd) (deviations Noisy p)))
    -- The defining property of heteroscedastic noise: the error is wider
    -- where the signal is larger. Without this check the variant could be
    -- plain Gaussian noise and nothing would notice.
  , ("heteroscedastic noise widens with the target magnitude",
      withPolynomial (\p ->
        let (low, high) = spreadByMagnitude (deviations HeteroscedasticNoise p)
        in high > low * 1.5))
  , ("homoscedastic noise does not widen with magnitude",
      withPolynomial (\p ->
        let (low, high) = spreadByMagnitude (deviations Noisy p)
        in high < low * 1.5))
    -- Outliers are defined by being rare and large, so both halves of that
    -- have to hold: most points untouched, a few moved a long way.
  , ("outliers leave most points untouched",
      withPolynomial (\p ->
        let ds = deviations WithOutliers p
            untouched = length (filter (\(a, b) -> a == b) ds)
        in untouched * 2 > length ds))
  , ("outliers move a few points a long way",
      withPolynomial (\p ->
        let ds = deviations WithOutliers p
            worst = maximum (0 : [abs (b - a) | (a, b) <- ds])
            typicalNoise = maximum (0 : [abs (b - a) | (a, b) <- deviations Noisy p])
        in worst > typicalNoise * 3))

  -- The irrelevant-variable variant ----------------------------------------
  , ("the irrelevant variant adds columns",
      withPolynomial (\p ->
        length (probVariables (applyVariant Irrelevant p))
          > length (probVariables p)))
  , ("the added columns are not marked relevant",
      withPolynomial (\p ->
        let q = applyVariant Irrelevant p
        in any (`notElem` probRelevant q) (probVariables q)))
    -- Stated over the truth *function*, not over sampled points. Adding
    -- columns changes how much randomness sampling consumes, so the drawn
    -- points legitimately differ; what must not change is that the
    -- distractors have no effect on the answer at a given input.
  , ("distractor columns do not affect the truth",
      withPolynomial (\p ->
        let q = applyVariant Irrelevant p
            base = [("x", 1.5)]
            withNoise = base ++ [("noise1", 900.0), ("noise2", -900.0)]
        in probTruth q withNoise == probTruth p base))
  , ("distractors do not affect the truth anywhere in the domain",
      withPolynomial (\p ->
        let q = applyVariant Irrelevant p
        in all (\x -> probTruth q [("x", x), ("noise1", 7.0), ("noise2", -7.0)]
                        == probTruth p [("x", x)])
               [-3.0, -1.0, 0.0, 1.0, 3.0]))
  , ("the irrelevant variant still generates full rows",
      withPolynomial (\p -> rowShapeMatches (applyVariant Irrelevant p)))

  -- Scoring ----------------------------------------------------------------
  , ("a perfect prediction scores zero",
      withPolynomial (\p -> case cleanOf p 100 of
        Just ds -> normalisedRmse (probTruth p) ds < 1.0e-12
        Nothing -> False))
    -- Predicting the mean is the reference point the metric is normalised
    -- against, so it must land at 1. If this drifts, every threshold in the
    -- suite silently changes meaning.
  , ("predicting the mean scores about one",
      withPolynomial (\p -> case cleanOf p 400 of
        Just ds ->
          let targets = map dpTarget (datasetPoints ds)
              average = sum targets / fromIntegral (length targets)
              score = normalisedRmse (const average) ds
          in abs (score - 1.0) < 0.02
        Nothing -> False))
  , ("a worse prediction scores higher",
      withPolynomial (\p -> case cleanOf p 100 of
        Just ds -> normalisedRmse (const 0.0) ds > normalisedRmse (probTruth p) ds
        Nothing -> False))
  , ("scoring an empty dataset does not crash",
      withPolynomial (\p -> case cleanOf p 0 of
        Just ds -> finite (normalisedRmse (probTruth p) ds)
        Nothing -> False))
  , ("the recovery threshold is a sane fraction",
      recoveryThreshold > 0.0 && recoveryThreshold < 0.5)

  -- Exact recovery ---------------------------------------------------------
    -- The two thresholds measure different things and must not drift
    -- together. 'recoveryThreshold' asks "does this fit well";
    -- 'exactThreshold' asks "is this the law". On `nested` those came
    -- apart at 70% against 50%, which is why the second exists.
  , ("the exact threshold is far stricter than the recovery threshold",
      exactThreshold > 0.0 && exactThreshold * 1000.0 < recoveryThreshold)
    -- Near machine noise deliberately: an algebraically correct formula
    -- differs from the truth only by floating-point reassociation, so any
    -- threshold in the several-orders-of-magnitude gap between "exact" and
    -- "good approximation" gives the same verdict.
  , ("the exact threshold is near machine noise", exactThreshold <= 1.0e-6)
    -- Exact implies recovered, always. A summary where exact exceeded
    -- recov would be incoherent, and the two are computed from the same
    -- score by separate comparisons, so nothing structural enforces it.
  , ("anything exact is also a recovery", exactThreshold < recoveryThreshold)

  -- Median -----------------------------------------------------------------
  , ("median of an empty list is zero", median [] == 0.0)
  , ("median of a singleton is itself", median [4.0] == 4.0)
  , ("median of an odd sample is the middle", median [3.0, 1.0, 2.0] == 2.0)
  , ("median of an even sample averages the middle pair",
      median [1.0, 2.0, 3.0, 4.0] == 2.5)
  , ("median ignores order", median [9.0, 1.0, 5.0] == median [1.0, 5.0, 9.0])
    -- Median rather than mean is what keeps one catastrophic seed from
    -- dominating a whole row of the report.
  , ("median resists a single extreme outlier",
      median [1.0, 1.0, 1.0, 1.0e9] < 2.0)
  ]
  where
    cleanOf' :: Word -> Maybe Dataset
    cleanOf' s = case polynomial of
      Just p -> datasetOr (generateClean p (probDomain p) 40 (mkSeed (fromIntegral s)))
      Nothing -> Nothing

    rowShapeMatches :: Problem -> Bool
    rowShapeMatches p = case cleanOf p 20 of
      Just ds ->
        all (\point -> map fst (dpInputs point) == probVariables p)
            (datasetPoints ds)
      Nothing -> False

    extrapolationFinite :: Problem -> Bool
    extrapolationFinite p =
      case datasetOr (generateClean p (probExtrapolation p) 100 (mkSeed 9)) of
        Just ds -> all finite (map dpTarget (datasetPoints ds))
        Nothing -> False
