{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parent selection over a scored pool.
--
-- Like "TypedGP.Random", this module is generic: it selects from
-- @[Scored a]@ and knows nothing about 'TypedGP.Expr.Expr' or fitness
-- decomposition. That keeps the selection pressure logic testable against
-- hand-built pools of integers, with no genetic programming in the way.
--
-- Both strategies assume __lower fitness is better__, matching
-- "TypedGP.Fitness".
--
-- @ScopedTypeVariables@ (with explicit @forall@s below) is on so the local
-- helpers can be given signatures that mention the enclosing @a@ rather
-- than accidentally quantifying a fresh one.
module TypedGP.Ops.Selection
  ( Scored (..)
  , select
  , tournamentSelection
  , fitnessProportionalSelection

    -- * Per-generation shared work
  , SelectionContext
  , selectionContext
  , ctxCases

    -- * Lexicase
  , selectLexicase
  , lexicaseElites
  , narrowByCases
  , medianAbsoluteDeviation
  , medianOf

    -- * Multi-objective (NSGA-II)
  , Objectives (..)
  , errorSizeObjectives
  , errorAgeObjectives
  , dominates
  , paretoRanks
  , crowdingDistancesFor
  , nsga2Keys
  , paretoFrontIndices
  , boundaryDistance
  ) where

import Data.List (sortOn, transpose)

import TypedGP.Config (Config (..), SelectionStrategy (..))
import TypedGP.Random (Seed, nextDouble, pick, shuffle)

-- | Anything paired with a fitness score.
data Scored a = Scored
  { scoredValue :: a
  , scoredFitness :: !Double
    -- ^ The scalar the strategy orders by. Total fitness under
    -- 'Tournament' and 'FitnessProportional'; the crowded-comparison key
    -- under 'Pareto'.
  , scoredCaseErrors :: [Double]
    -- ^ Error on each individual training case, in dataset order. Used
    -- only by 'Lexicase'.
    --
    -- __Deliberately lazy__, and that is load-bearing rather than an
    -- oversight. Two consequences follow from it:
    --
    --   * Runs under any other strategy never force it, so they pay
    --     nothing for a field they do not read.
    --   * Under lexicase the pool is built once per generation and reused
    --     across every selection event in it, so the thunk is forced once
    --     and shared. Recomputing per selection would mean population ×
    --     population × cases evaluations per generation, which is the
    --     difference between affordable and unusable.
    --
    -- A strict field here would silently impose the first cost on every
    -- run in the project.
  } deriving (Eq, Show)

-- | Select one parent using the strategy named in the config.
--
-- 'Nothing' for an empty pool — callers must decide what an empty
-- population means rather than receiving a crash.
select :: Config -> SelectionContext -> [Scored a] -> Seed -> Maybe (a, Seed)
select cfg context pool s0 = case cfgSelection cfg of
  Tournament          -> tournamentSelection (cfgTournamentSize cfg) pool s0
  FitnessProportional -> fitnessProportionalSelection pool s0
  -- Pareto mode runs the same tournament, but over a different score:
  -- "TypedGP.Population" fills 'scoredFitness' with the crowded-comparison
  -- key from 'nsga2Keys' instead of scalarised fitness. A tournament on
  -- that key /is/ NSGA-II's selection operator — rank first, crowding
  -- distance as the tie-break — so no separate mechanism is needed here.
  -- Keeping it shared also means Pareto mode inherits the existing
  -- empty-pool and sampling behaviour rather than reimplementing it.
  Pareto              -> tournamentSelection (cfgTournamentSize cfg) pool s0
  -- Same mechanism as Pareto, over a different objective pair:
  -- 'TypedGP.Population' fills 'scoredFitness' from (error, age) instead
  -- of (error, size). The selector itself is identical because NSGA-II's
  -- selection operator is "tournament on the crowded-comparison key"
  -- regardless of what the objectives are.
  AgeFitness          -> tournamentSelection (cfgTournamentSize cfg) pool s0
  Lexicase            -> selectLexicase cfg (ctxCases context) pool s0

-- | Draw @k@ candidates with replacement and return the best.
--
-- Selection pressure is controlled entirely by @k@: @k == 1@ is a random
-- walk, large @k@ converges fast and loses diversity. Sampling /with/
-- replacement is intentional — it keeps each draw independent, so the
-- pressure does not drift as the pool is consumed.
tournamentSelection :: forall a. Int -> [Scored a] -> Seed -> Maybe (a, Seed)
tournamentSelection k pool s0
  | null pool = Nothing
  | otherwise = go (max 1 k) Nothing s0
  where
    go :: Int -> Maybe (Scored a) -> Seed -> Maybe (a, Seed)
    go 0 champion st = fmap (\c -> (scoredValue c, st)) champion
    go rounds champion st = case pick pool st of
      Just (entrant, st') -> go (rounds - 1) (Just (better entrant champion)) st'
      -- Unreachable: pool is non-empty, checked above.
      Nothing -> fmap (\c -> (scoredValue c, st)) champion

    better :: Scored a -> Maybe (Scored a) -> Scored a
    better entrant Nothing = entrant
    better entrant (Just champion)
      | scoredFitness entrant < scoredFitness champion = entrant
      | otherwise                                      = champion

-- | Roulette wheel over weights @1 / (1 + fitness)@.
--
-- The reciprocal is what turns a minimisation problem into the
-- maximisation that roulette needs, and the @+1@ keeps a perfect
-- individual (fitness 0) from taking infinite weight. Negative fitness is
-- clamped to 0, so a metric that could go negative degrades to "treated as
-- perfect" rather than producing a negative weight that corrupts the
-- cumulative walk.
fitnessProportionalSelection :: forall a. [Scored a] -> Seed -> Maybe (a, Seed)
fitnessProportionalSelection pool s0
  | null pool        = Nothing
  | totalWeight <= 0 = uniformFallback s0
  | otherwise =
      let (u, s1) = nextDouble s0
          target = u * totalWeight
      in case walk 0.0 target pool of
           Just value -> Just (value, s1)
           -- Only reachable through floating-point round-off leaving the
           -- accumulated weight a hair under the target on the last
           -- element; a uniform draw is a harmless resolution.
           Nothing -> uniformFallback s1
  where
    weightOf :: Scored a -> Double
    weightOf sc = 1.0 / (1.0 + max 0.0 (scoredFitness sc))

    totalWeight :: Double
    totalWeight = sum (map weightOf pool)

    walk :: Double -> Double -> [Scored a] -> Maybe a
    walk _ _ [] = Nothing
    walk !running target (sc : rest)
      | running' >= target = Just (scoredValue sc)
      | otherwise          = walk running' target rest
      where
        running' = running + weightOf sc

    uniformFallback :: Seed -> Maybe (a, Seed)
    uniformFallback st = fmap (\(sc, st') -> (scoredValue sc, st')) (pick pool st)

-- Lexicase selection ---------------------------------------------------------

-- | Work that is shared by every selection event within one generation.
--
-- Exists because selection is called once per offspring — hundreds of
-- times per generation — against an unchanging pool. Anything derived
-- purely from the pool must be computed once here rather than inside the
-- per-event path.
--
-- Lazy, so strategies that need none of it pay nothing.
-- @data@ with a strict field, deliberately, __not__ a newtype. A newtype is
-- erased at compile time, so @ctxCases context@ would be literally the
-- defining expression at every one of the ~500 selection call sites in a
-- generation, and nothing would stop it being recomputed at each. Making
-- it a real constructor with a strict field forces the work once, at
-- construction, and turns every later access into a field load.
--
-- This was measured, not guessed: as a newtype the benchmark did not
-- finish two seeds in nine minutes; the cost model said it should take
-- about ten seconds.
data SelectionContext = SelectionContext
  { ctxCases :: ![(Double, [Double])]
    -- ^ One entry per training case: its epsilon band, and every
    -- candidate's error on it __in pool order__.
    --
    -- Transposed relative to 'scoredCaseErrors', and that is the point.
    -- Walking cases while indexing into per-candidate lists costs
    -- @O(caseIndex)@ per lookup, which measured 145x slower than
    -- tournament selection; with the matrix transposed once per
    -- generation, a case is a single flat list and filtering it is one
    -- linear pass with no indexing at all.
  }

-- | Per-case epsilon over the whole population: the median absolute
-- deviation of every candidate's error on that case.
--
-- __This is the semi-dynamic variant__, and the choice was made by
-- measurement rather than preference. The dynamic variant recomputes the
-- deviation over the surviving candidates at every case of every selection
-- event, which put two sorts inside the innermost loop and measured __50x
-- slower__ than tournament selection on the benchmark suite — 164 seconds
-- per seed against 3.4. Semi-dynamic computes the bands once per
-- generation and leaves only comparisons in the loop.
--
-- The band is therefore fixed for a generation while the /best/ error it
-- is measured from still comes from the surviving pool, which is what the
-- name means and what La Cava et al. (2016) report as generally the
-- strongest of the three variants anyway.
selectionContext :: [Scored a] -> SelectionContext
selectionContext pool = SelectionContext
  { ctxCases = [(medianAbsoluteDeviation row, row) | row <- caseRows]
  }
  where
    -- 'transpose' truncates to the shortest input row, so a malformed pool
    -- yields fewer cases rather than a ragged matrix.
    caseRows :: [[Double]]
    caseRows = transpose (map scoredCaseErrors pool)

-- | epsilon-lexicase selection.
--
-- Filters the pool case by case, in a fresh random order each time, keeping
-- only candidates within a tolerance of the best error on the current case,
-- and stops as soon as one candidate remains.
--
-- == Why not just compare aggregate fitness
--
-- A scalar mean cannot distinguish an individual that is /uniquely
-- excellent on a few cases and mediocre elsewhere/ from one that is
-- uniformly mediocre. The first is a partial solution worth keeping; the
-- second is not. Selecting on individual cases preserves specialists long
-- enough for crossover to combine them, which is precisely the failure
-- this project has already measured — runs freezing on @x + x@ with the
-- @sin@ term never found.
--
-- == The epsilon
--
-- Plain lexicase assumes exact pass\/fail per case. With real-valued
-- errors exact ties essentially never happen, so the first case would
-- always reduce the pool to a single candidate and selection would
-- degenerate into "best on one random case". The tolerance is what makes
-- it applicable to regression.
--
-- It is the median absolute deviation of the population's errors on that
-- case — scale-free, and adapting to each case's own difficulty rather
-- than imposing one number across cases whose errors differ by orders of
-- magnitude. It is supplied by 'selectionContext' rather than computed
-- here, because computing it per selection event was measured at 50x the
-- cost of tournament selection.
--
-- See @docs\/phase1-lexicase-design.md@ for the variant comparison and the
-- cost analysis.
-- Survivors are tracked as a boolean mask over the pool rather than as a
-- shrinking list of candidates. That keeps every step a flat zip against
-- the case row, which is already in pool order — no indexing, no lookups,
-- one linear pass per case.
selectLexicase
  :: forall a. Config
  -> [(Double, [Double])]
  -- ^ Cases as @(epsilon, errors in pool order)@, from 'selectionContext'.
  -> [Scored a]
  -> Seed
  -> Maybe (a, Seed)
selectLexicase cfg cases pool s0
  | null pool = Nothing
  | otherwise =
      let (finalMask, s1) = narrowByCases cfg cases (map (const True) pool) s0
          survivors = [c | (c, alive) <- zip pool finalMask, alive]
      in fmap (\(winner, s2) -> (scoredValue winner, s2)) (pick survivors s1)

-- | The lexicase filter itself: shuffle the cases, then narrow a candidate
-- mask case by case until one survivor remains or the cases run out.
--
-- Extracted so that elite selection can use the same filter as ordinary
-- selection. That matters more than code reuse: if elites were chosen by a
-- different criterion from the rest of the generation, elitism would be
-- pulling the population back towards exactly what lexicase exists to
-- avoid.
narrowByCases
  :: Config -> [(Double, [Double])] -> [Bool] -> Seed -> ([Bool], Seed)
narrowByCases cfg cases mask0 s0 =
  let (order, s1) = shuffle (applyCap cases) s0
  in (narrow order mask0, s1)
  where
    -- A cap is safe only because the order is shuffled first: a capped
    -- prefix is then an unbiased random subset of cases rather than a
    -- fixed favoured subset.
    applyCap :: [(Double, [Double])] -> [(Double, [Double])]
    applyCap order
      | cfgLexicaseMaxCases cfg <= 0 = order
      | otherwise = take (cfgLexicaseMaxCases cfg) order

    narrow :: [(Double, [Double])] -> [Bool] -> [Bool]
    narrow [] mask = mask
    narrow ((tolerance, errors) : remaining) mask
      -- One candidate left: no further case can separate anything.
      | aliveCount mask <= 1 = mask
      | otherwise = narrow remaining (keepWithinEpsilon tolerance errors mask)

    keepWithinEpsilon :: Double -> [Double] -> [Bool] -> [Bool]
    keepWithinEpsilon tolerance errors mask =
      case [e | (e, True) <- zip errors mask] of
        -- Nothing alive: leave the mask untouched rather than narrowing an
        -- empty set further.
        [] -> mask
        living ->
          -- The band width is precomputed; only the best error comes from
          -- the surviving pool. That is the semi-dynamic split.
          let best = minimum living
          -- '<=', not '<'. When every candidate ties on a case the
          -- deviation is 0, and a strict comparison would discard the best
          -- candidate along with the rest and empty the pool.
          in [alive && e <= best + tolerance | (e, alive) <- zip errors mask]

    aliveCount :: [Bool] -> Int
    aliveCount = length . filter id

-- | Choose @wanted@ distinct elites using the lexicase filter itself,
-- rather than by scalar fitness.
--
-- == Why this exists
--
-- Elitism under tournament is neutral scaffolding: it preserves the best by
-- the same scalar the selector already orders by, so it amplifies what
-- selection is doing. Under lexicase it is not neutral — the elites are
-- drawn from a /different criterion/ than everything else in the
-- generation, every generation, pulling the population back towards the
-- mean-error generalists lexicase exists to stop privileging.
--
-- That is not speculation: an ablation on the @nested@ benchmark measured
-- scalar elitism doing 30 points of work that had nothing to do with the
-- selector, and compounding with tournament while barely helping lexicase.
--
-- Each elite is drawn by a fresh filter with its own random case order, and
-- winners are excluded from the mask for subsequent draws so the elite set
-- is distinct. Drawing with replacement would let one individual occupy
-- every elite slot and collapse the very diversity elitism is meant to
-- carry forward.
lexicaseElites
  :: forall a. Config -> SelectionContext -> [Scored a] -> Int -> Seed -> ([a], Seed)
lexicaseElites cfg context pool wanted s0
  | wanted <= 0 || null pool = ([], s0)
  | otherwise = go wanted (map (const True) pool) s0 []
  where
    go :: Int -> [Bool] -> Seed -> [a] -> ([a], Seed)
    go 0 _ st acc = (reverse acc, st)
    go remaining mask st acc
      -- Fewer distinct candidates than elite slots: return what there is
      -- rather than repeating anyone.
      | not (or mask) = (reverse acc, st)
      | otherwise =
          let (finalMask, st1) = narrowByCases cfg (ctxCases context) mask st
              aliveIndices = [i | (i, True) <- zip [0 :: Int ..] finalMask]
          in case pick aliveIndices st1 of
               -- Unreachable: the mask had at least one live candidate and
               -- narrowing never empties a non-empty mask.
               Nothing -> (reverse acc, st1)
               Just (index, st2) -> case valueAtIndex index pool of
                 Nothing -> (reverse acc, st2)
                 Just winner ->
                   go (remaining - 1) (excludeIndex index mask) st2 (winner : acc)

    valueAtIndex :: Int -> [Scored a] -> Maybe a
    valueAtIndex index candidates = case drop index candidates of
      (c : _) -> Just (scoredValue c)
      -- Unreachable: the index came from a mask the same length as pool.
      []      -> Nothing

    -- Verified by mutation: neutering this to @\_ mask -> mask@ fails
    -- exactly three assertions in LexicaseSpec (distinctness, and the two
    -- pool-exhaustion cases) while leaving the rest green. The tests check
    -- what they claim to.
    excludeIndex :: Int -> [Bool] -> [Bool]
    excludeIndex index mask =
      [alive && i /= index | (i, alive) <- zip [0 :: Int ..] mask]

-- | Median absolute deviation: the median of the absolute deviations from
-- the median.
--
-- A robust scale estimate, which is what makes it the right tolerance
-- here — a single wildly wrong candidate should not widen the band enough
-- to admit everyone. Non-negative by construction, since every term is an
-- absolute value, and exactly zero when all inputs are equal.
medianAbsoluteDeviation :: [Double] -> Double
medianAbsoluteDeviation [] = 0.0
medianAbsoluteDeviation values =
  medianOf [abs (v - centre) | v <- values]
  where
    centre :: Double
    centre = medianOf values

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

-- Multi-objective selection ------------------------------------------------

-- | A vector of objectives, __all minimised__.
--
-- A list rather than a fixed record, because NSGA-II is defined for @k@
-- objectives and the two-objective form is a specialisation of it rather
-- than a design. Two schemes use this: 'errorSizeObjectives' for
-- 'TypedGP.Config.Pareto' and 'errorAgeObjectives' for
-- 'TypedGP.Config.AgeFitness'.
--
-- Integer-valued objectives (node count, age) are held as 'Double' so the
-- crowding-distance arithmetic can treat every dimension identically.
--
-- __All members of one call must have the same length.__ Nothing enforces
-- it in the type; every producer here builds a whole population's vectors
-- from one function, so the invariant holds by construction at the call
-- sites that exist. 'dominates' degrades safely rather than crashing if it
-- is ever violated — see below.
newtype Objectives = Objectives
  { objectiveValues :: [Double]
  } deriving (Eq, Show)

-- | (prediction error, tree size). The 'TypedGP.Config.Pareto' scheme.
errorSizeObjectives :: Double -> Double -> Objectives
errorSizeObjectives err size = Objectives [err, size]

-- | (prediction error, lineage age). The 'TypedGP.Config.AgeFitness'
-- scheme.
--
-- Deliberately __not__ (error, size, age). Pareto fronts grow quickly with
-- objective count, and with two of three objectives being small integers
-- with heavy ties, a three-objective front on a population of 500 is large
-- enough that rank stops carrying information and selection pressure comes
-- entirely from crowding distance. See @docs\/phase4-age-fitness-design.md@.
errorAgeObjectives :: Double -> Double -> Objectives
errorAgeObjectives err age = Objectives [err, age]

-- | Pareto dominance: no worse on any objective, and strictly better on at
-- least one.
--
-- The strictness requirement is what makes this a strict partial order,
-- and in particular makes it irreflexive — two individuals with identical
-- objectives do not dominate each other, so they share a front instead of
-- deadlocking the peel in 'paretoRanks'.
--
-- 'zipWith' truncates to the shorter vector, so mismatched lengths compare
-- on their common prefix rather than throwing. That is a deliberate
-- degradation: an arity bug should show up as weaker selection in a
-- benchmark, not as a crash in the middle of a long run.
dominates :: Objectives -> Objectives -> Bool
dominates (Objectives as) (Objectives bs) =
  and (zipWith (<=) as bs) && or (zipWith (<) as bs)

-- | Crowding distance assigned to the extremes of each front.
--
-- A large finite number rather than a true infinity: these values are fed
-- into arithmetic in 'nsga2Keys', and keeping them finite means no path
-- can produce @Inf - Inf@ and hence @NaN@ — which would silently corrupt
-- every comparison it touched, exactly the failure mode
-- "TypedGP.Eval" is careful to prevent elsewhere.
boundaryDistance :: Double
boundaryDistance = 1.0e18

-- | Non-dominated sort: the front index of every individual, @0@ being the
-- Pareto front itself.
--
-- This is NSGA-II's /fast/ non-dominated sort, not the naive repeated
-- scan. The naive version re-tests every surviving pair on every peel,
-- which is cubic once the population splits into many fronts; this does
-- the pairwise comparisons once up front and then peels using domination
-- counts, so the whole thing is quadratic in the population and linear in
-- the number of dominance relations. At a population of 600 that is the
-- difference between a fraction of a generation's budget and dominating it.
paretoRanks :: [Objectives] -> [Int]
paretoRanks objs
  | null objs = []
  | otherwise = map snd (sortOn fst (peel 0 initialCounts))
  where
    population :: Int
    population = length objs

    indexed :: [(Int, Objectives)]
    indexed = zip [0 ..] objs

    -- For each individual, everything it dominates. Computed once.
    dominatedLists :: [[Int]]
    dominatedLists = [[j | (j, b) <- indexed, a `dominates` b] | (_, a) <- indexed]

    -- How many individuals dominate each one, derived from the same
    -- relation by tallying rather than by a second quadratic scan.
    initialCounts :: [Int]
    initialCounts = tally population (concat dominatedLists)

    -- A count of -1 marks an individual already assigned to a front.
    -- Nothing can decrement it back into contention, because an individual
    -- reaches a front only when its count is 0, and a count of 0 means it
    -- appears in no dominatedLists entry at all.
    peel :: Int -> [Int] -> [(Int, Int)]
    peel rank counts
      | null front = []
      | otherwise = [(i, rank) | i <- front] ++ peel (rank + 1) nextCounts
      where
        front = [i | (i, c) <- zip [0 ..] counts, c == 0]
        released = concat [js | (js, c) <- zip dominatedLists counts, c == 0]
        decrements = tally population released
        nextCounts = zipWith step counts decrements

        step :: Int -> Int -> Int
        step c d
          | c == 0    = -1
          | c < 0     = c
          | otherwise = c - d

-- | Occurrence count of each of @0 .. n-1@ in a list of indices.
--
-- Sorting once and sweeping keeps this linearithmic; the obvious
-- @[length (filter (== i) xs) | i <- [0 .. n-1]]@ is quadratic and is what
-- makes naive implementations of the sort above so slow.
tally :: Int -> [Int] -> [Int]
tally n xs = go 0 (sortOn id xs)
  where
    go :: Int -> [Int] -> [Int]
    go i remaining
      | i >= n = []
      | otherwise =
          let rest = dropWhile (< i) remaining
              (here, later) = span (== i) rest
          in length here : go (i + 1) later

-- | Crowding distance for each individual, measured within its own front.
--
-- Takes the ranks as an argument rather than recomputing them, so callers
-- that need both pay for the non-dominated sort once.
--
-- The distance is the normalised perimeter of the cuboid spanned by an
-- individual's two neighbours along each objective. Its purpose is
-- diversity: preferring high crowding distance spreads survivors along the
-- front instead of letting them pile up wherever the front happens to be
-- densest. The extremes of each front get 'boundaryDistance' so the ends
-- of the frontier are never discarded.
crowdingDistancesFor :: [Int] -> [Objectives] -> [Double]
crowdingDistancesFor ranks objs
  | null objs = []
  | otherwise = map snd (sortOn fst (concatMap frontDistances fronts))
  where
    tagged :: [(Int, Objectives, Int)]
    tagged = zip3 [0 ..] objs ranks

    highestRank :: Int
    highestRank = maximum (0 : ranks)

    fronts :: [[(Int, Objectives)]]
    fronts = [[(i, o) | (i, o, r) <- tagged, r == wanted] | wanted <- [0 .. highestRank]]

    -- How many objectives every member carries. Read from the data rather
    -- than fixed at two, which is the whole point of the generalisation.
    arity :: Int
    arity = case objs of
      (Objectives vs : _) -> length vs
      []                  -> 0

    frontDistances :: [(Int, Objectives)] -> [(Int, Double)]
    frontDistances members
      -- With two or fewer members everyone is an extreme.
      | length members <= 2 = [(i, boundaryDistance) | (i, _) <- members]
      | otherwise =
          -- Sum each objective's contribution, index by index. The
          -- two-objective case is exactly the previous
          -- @errorPart + sizePart@; nothing about it changes.
          let perObjective =
                [ sortOn fst (spreadAlong (component k) members)
                | k <- [0 .. arity - 1]
                ]
              ids = map fst (sortOn fst [(i, ()) | (i, _) <- members])
          in zip ids (foldr (zipWith (+)) (map (const 0.0) ids)
                            (map (map snd) perObjective))

    -- The k-th objective, or 0 if absent. Absent cannot arise for vectors
    -- built by the smart constructors, which are all the same length.
    component :: Int -> Objectives -> Double
    component k (Objectives vs) = case drop k vs of
      (v : _) -> v
      []      -> 0.0

    -- Contribution of one objective to every member's crowding distance.
    spreadAlong :: (Objectives -> Double) -> [(Int, Objectives)] -> [(Int, Double)]
    spreadAlong f members =
      let ordered = sortOn snd [(i, f o) | (i, o) <- members]
          ids = map fst ordered
          values = map snd ordered
          count = length ordered
          spread = maximum values - minimum values
          -- Neighbour differences. The padding at each end is arbitrary
          -- because both ends are overwritten with boundaryDistance below.
          previous = 0 : values
          following = drop 1 values ++ [0]
          gaps = zipWith (-) following previous
          scaled
            -- Every member identical along this objective: it separates
            -- nobody, so it contributes nothing rather than dividing by 0.
            | spread <= 0 = replicate count 0.0
            | otherwise = map (/ spread) gaps
      in [ (i, if k == 0 || k == count - 1 then boundaryDistance else d)
         | (k, i, d) <- zip3 [0 ..] ids scaled
         ]

-- | The crowded-comparison key: one scalar per individual, lower is better.
--
-- Encodes NSGA-II's two-level ordering into a single number so that any
-- existing selection mechanism can consume it. The integer part is the
-- Pareto rank and the fractional part is a decreasing function of crowding
-- distance, so the key lands in @[rank, rank + 0.9]@ — a more crowded
-- individual always loses to a less crowded one of the same rank, and can
-- never overtake anything of a better rank.
nsga2Keys :: [Objectives] -> [Double]
nsga2Keys objs = zipWith key ranks (crowdingDistancesFor ranks objs)
  where
    ranks :: [Int]
    ranks = paretoRanks objs

    key :: Int -> Double -> Double
    key rank crowding = fromIntegral rank + 0.9 / (1.0 + max 0.0 crowding)

-- | Indices of the non-dominated individuals — the Pareto front itself.
--
-- This is what a Pareto run reports: every formula that no other formula
-- beats on both accuracy and simplicity at once.
paretoFrontIndices :: [Objectives] -> [Int]
paretoFrontIndices objs = [i | (i, r) <- zip [0 ..] (paretoRanks objs), r == 0]
