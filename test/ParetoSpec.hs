-- | Tests for NSGA-II multi-objective selection.
--
-- Written when 'Objectives' was generalised from a two-field record to a
-- list of minimised objectives, to support age-fitness Pareto alongside
-- the existing (error, size) scheme.
--
-- == Why this module did not exist before, and why that mattered
--
-- The Phase 4 design note claimed the generalisation was guarded by
-- "existing tests for the two-objective case". __There were none.__ The
-- suite had 679 passing assertions and not one of them touched
-- 'dominates', 'paretoRanks', 'crowdingDistancesFor' or 'nsga2Keys'; a
-- refactor of working code was about to be defended by a mitigation that
-- did not exist. Checking that claim before relying on it is the only
-- reason this file is here.
--
-- == The equivalence check
--
-- The intended validation was a bit-for-bit benchmark comparison of
-- @--selection pareto@ before and after the refactor. That was not
-- possible retroactively — the refactor landed before a baseline was
-- recorded.
--
-- What replaces it is stronger rather than weaker. 'referenceDominates'
-- below is a transcription of the __pre-refactor implementation__, and the
-- generalised code is asserted to agree with it across 3600 ordered pairs
-- of randomly generated objective vectors. A benchmark diff over three
-- problems could only detect differences large enough to move a recovery
-- rate; this detects any difference at all, on the function itself.
--
-- Crowding distance is checked differently, because a "reference" that
-- called the function under test would be vacuous — a first draft of this
-- module made exactly that mistake. 'handFront' instead pins values
-- computed on paper.
module ParetoSpec (tests) where

import TypedGP.Ops.Selection
  ( Objectives (..)
  , boundaryDistance
  , crowdingDistancesFor
  , dominates
  , errorAgeObjectives
  , errorSizeObjectives
  , nsga2Keys
  , paretoFrontIndices
  , paretoRanks
  )
import TypedGP.Random (Seed, mkSeed, nextRange)

-- Reference implementations --------------------------------------------------

-- | The pre-refactor 'dominates', transcribed.
--
-- Was: @objError a <= objError b && objSize a <= objSize b &&
-- (objError a < objError b || objSize a < objSize b)@.
referenceDominates :: (Double, Double) -> (Double, Double) -> Bool
referenceDominates (ae, as) (be, bs) =
  ae <= be && as <= bs && (ae < be || as < bs)

-- | A four-member front whose crowding distances are computable by hand.
--
-- All four are mutually non-dominated (each trades one objective against
-- the other), so they share rank 0 and none is filtered out.
--
-- Along either objective the sorted values are @[0, 1, 2, 3]@ with spread
-- @3@, so each interior member's neighbour gap is @2@ and its normalised
-- contribution is @2/3@. Summed over two objectives that is @4/3@, and the
-- two extremes are overwritten with 'boundaryDistance'.
--
-- This is the assertion that actually checks the generalisation: summing
-- one contribution per objective is precisely what replaced the two named
-- calls, and @4/3@ is arithmetic done on paper rather than copied from the
-- implementation's own output.
handFront :: [Objectives]
handFront =
  [ Objectives [0.0, 3.0]
  , Objectives [1.0, 2.0]
  , Objectives [2.0, 1.0]
  , Objectives [3.0, 0.0]
  ]

-- Fixtures --------------------------------------------------------------------

-- | Deterministic pseudo-random objective pairs, with plenty of ties: the
-- interesting dominance cases are equalities, not generic points.
randomPairs :: [(Double, Double)]
randomPairs = go (mkSeed 7) (60 :: Int)
  where
    go :: Seed -> Int -> [(Double, Double)]
    go _ 0 = []
    go s n =
      let (a, s1) = nextRange 0.0 4.0 s
          (b, s2) = nextRange 0.0 4.0 s1
          -- Rounded to a coarse grid so ties actually occur.
          quantise v = fromIntegral (round (v * 2.0) :: Int) / 2.0
      in (quantise a, quantise b) : go s2 (n - 1)

-- | __Two objectives, matching current usage deliberately.__
--
-- Both live schemes are two-objective: (error, size) for 'Pareto' and
-- (error, age) for 'AgeFitness'. This fixture matches that on purpose, so
-- the equivalence check against the pre-refactor two-field code is a
-- like-for-like comparison rather than an accident of the generator.
--
-- Three-objective coverage is deliberately thin — a handful of hand-built
-- cases below — because three-objective Pareto is explicitly deferred in
-- @docs/phase4-age-fitness-design.md@ for front-inflation reasons. If a
-- third objective is ever adopted, this fixture is the thing to widen
-- first, and this note is here so that is a decision rather than an
-- oversight.
asObjectives :: [(Double, Double)] -> [Objectives]
asObjectives pairs = [Objectives [e, s] | (e, s) <- pairs]

allPairsAgree :: Bool
allPairsAgree =
  and [ dominates (Objectives [ae, as]) (Objectives [be, bs])
          == referenceDominates (ae, as) (be, bs)
      | (ae, as) <- randomPairs
      , (be, bs) <- randomPairs
      ]

-- | A small hand-built front where the answer is checkable by eye.
--
-- Index 0 and 2 are non-dominated (each is better on one objective);
-- index 1 is dominated by 0 on both.
handBuilt :: [Objectives]
handBuilt =
  [ Objectives [1.0, 5.0]
  , Objectives [2.0, 6.0]
  , Objectives [3.0, 1.0]
  ]

tests :: [(String, Bool)]
tests =
  -- Equivalence with the pre-refactor implementation --------------------------
    -- The assertion the design note's "existing tests guard it" claim
    -- should have been. 3600 ordered pairs, including many ties.
  [ ("generalised dominance agrees with the two-field original on every pair",
      allPairsAgree)
  , ("the equivalence check covers at least 3000 comparisons",
      length randomPairs * length randomPairs >= 3000)
  , ("all four hand-front members share rank 0",
      paretoRanks handFront == [0, 0, 0, 0])
    -- The check that the summing generalisation is right: 2/3 per
    -- objective, two objectives, so 4/3 for each interior member.
    -- Computed on paper, not read off the implementation.
  , ("interior crowding distance is the sum over both objectives",
      case crowdingDistancesFor (paretoRanks handFront) handFront of
        [_, b, c, _] -> abs (b - 4.0/3.0) < 1.0e-9 && abs (c - 4.0/3.0) < 1.0e-9
        _            -> False)
    -- Twice the boundary distance, not once — and getting this wrong is
    -- what the hand computation was for. The boundary value is assigned
    -- per objective, and on this anti-diagonal front each extreme member
    -- is extreme on /both/ objectives, so it collects the boundary value
    -- twice. A front where a member is extreme on only one axis would
    -- give it boundary + an ordinary gap.
  , ("an extreme on both objectives collects the boundary distance twice",
      case crowdingDistancesFor (paretoRanks handFront) handFront of
        [a, _, _, d] -> a == 2.0 * boundaryDistance
                          && d == 2.0 * boundaryDistance
        _            -> False)
  , ("extremes still outrank interior members by a wide margin",
      case crowdingDistancesFor (paretoRanks handFront) handFront of
        [a, b, c, d] -> a > b && a > c && d > b && d > c
        _            -> False)

  -- Dominance ------------------------------------------------------------------
  , ("better on both dominates",
      dominates (Objectives [1.0, 1.0]) (Objectives [2.0, 2.0]))
  , ("better on one and equal on the other dominates",
      dominates (Objectives [1.0, 2.0]) (Objectives [2.0, 2.0]))
    -- Irreflexivity is what stops 'paretoRanks' deadlocking when two
    -- individuals have identical objectives: they must share a front
    -- rather than each waiting for the other to be peeled.
  , ("identical objectives do not dominate each other",
      not (dominates (Objectives [1.0, 1.0]) (Objectives [1.0, 1.0])))
  , ("a trade-off is not dominance",
      not (dominates (Objectives [1.0, 3.0]) (Objectives [3.0, 1.0]))
        && not (dominates (Objectives [3.0, 1.0]) (Objectives [1.0, 3.0])))
  , ("dominance is antisymmetric on the random population",
      and [ not (dominates a b && dominates b a)
          | a <- asObjectives randomPairs, b <- asObjectives randomPairs
          ])

  -- Three objectives, which is what the generalisation buys --------------------
  , ("dominance extends to three objectives",
      dominates (Objectives [1.0, 1.0, 1.0]) (Objectives [1.0, 2.0, 3.0]))
  , ("a third objective can block dominance",
      not (dominates (Objectives [1.0, 1.0, 9.0]) (Objectives [1.0, 2.0, 3.0])))
    -- Mismatched arity compares on the common prefix rather than throwing.
    -- Deliberate: an arity bug should surface as weaker selection in a
    -- benchmark, not as a crash mid-run.
  , ("mismatched arity compares on the common prefix",
      dominates (Objectives [1.0]) (Objectives [2.0, 0.0]))

  -- Ranks and fronts -----------------------------------------------------------
  , ("the hand-built front ranks as expected",
      paretoRanks handBuilt == [0, 1, 0])
  , ("the front contains exactly the non-dominated indices",
      paretoFrontIndices handBuilt == [0, 2])
  , ("every rank is non-negative",
      all (>= 0) (paretoRanks (asObjectives randomPairs)))
  , ("rank 0 is non-empty",
      not (null (paretoFrontIndices (asObjectives randomPairs))))
  , ("ranks and objectives have the same length",
      length (paretoRanks (asObjectives randomPairs)) == length randomPairs)
    -- A rank-0 member must be dominated by nobody, and a higher-ranked one
    -- must be dominated by somebody. This is the definition, checked
    -- against the implementation rather than assumed from it.
  , ("rank 0 members are dominated by nobody",
      let objs = asObjectives randomPairs
          ranks = paretoRanks objs
      in and [ not (any (`dominates` o) objs)
             | (o, r) <- zip objs ranks, r == 0
             ])
  , ("members above rank 0 are dominated by somebody",
      let objs = asObjectives randomPairs
          ranks = paretoRanks objs
      in and [ any (`dominates` o) objs
             | (o, r) <- zip objs ranks, r > 0
             ])
  , ("an empty population has no ranks", null (paretoRanks []))

  -- Crowding distance ----------------------------------------------------------
  , ("crowding distances match the population size",
      let objs = asObjectives randomPairs
      in length (crowdingDistancesFor (paretoRanks objs) objs) == length objs)
  , ("the extremes of a front get the boundary distance",
      let objs = handBuilt
          ds = crowdingDistancesFor (paretoRanks objs) objs
      in all (== boundaryDistance) [d | (d, r) <- zip ds (paretoRanks objs), r == 0])
  , ("crowding distances are never negative",
      let objs = asObjectives randomPairs
      in all (>= 0) (crowdingDistancesFor (paretoRanks objs) objs))
  , ("an empty population has no crowding distances",
      null (crowdingDistancesFor [] []))

  -- The selection key ----------------------------------------------------------
    -- nsga2Keys is what selection actually compares: rank first, crowding
    -- distance as the tie-break, packed into one Double.
  , ("keys match the population size",
      length (nsga2Keys (asObjectives randomPairs)) == length randomPairs)
  , ("a better rank always gives a smaller key",
      let objs = handBuilt
          keys = nsga2Keys objs
          ranks = paretoRanks objs
      in and [ ka < kb
             | (ka, ra) <- zip keys ranks
             , (kb, rb) <- zip keys ranks
             , ra < rb
             ])
  , ("keys are deterministic",
      nsga2Keys (asObjectives randomPairs) == nsga2Keys (asObjectives randomPairs))

  -- The smart constructors -----------------------------------------------------
  , ("errorSizeObjectives orders error before size",
      objectiveValues (errorSizeObjectives 1.5 9.0) == [1.5, 9.0])
  , ("errorAgeObjectives orders error before age",
      objectiveValues (errorAgeObjectives 1.5 3.0) == [1.5, 3.0])
    -- Both schemes are two-objective, so neither dilutes rank the way a
    -- combined (error, size, age) would. Pinned so a future "just add age
    -- to the existing pair" change has to argue with a test.
  , ("both schemes are two-objective",
      length (objectiveValues (errorSizeObjectives 0 0)) == 2
        && length (objectiveValues (errorAgeObjectives 0 0)) == 2)
  ]
