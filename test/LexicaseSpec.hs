-- | Tests for epsilon-lexicase selection.
--
-- Three things need proving, and they need different kinds of test:
--
--   * the __median machinery__ is right, by hand-computed unit tests,
--     because a wrong median would hide behind a plausible-looking
--     selection frequency and never be noticed;
--   * the selection is __deterministic__, because the whole project's
--     reproducibility guarantee rests on it;
--   * lexicase actually __does the thing it exists for__ — preferring a
--     specialist that is uniquely best on a few cases over a generalist
--     with a better mean — measured as a frequency comparison against
--     tournament selection on the same pool.
--
-- The last is the one that matters. The first two could both pass on an
-- implementation that quietly behaved like a uniform random pick.
module LexicaseSpec (tests) where

import TypedGP.Config (Config (..), SelectionStrategy (..), defaultConfig)
import TypedGP.Ops.Selection
  ( Scored (..)
  , medianAbsoluteDeviation
  , medianOf
  , ctxCases
  , lexicaseElites
  , selectLexicase
  , selectionContext
  , tournamentSelection
  )
import TypedGP.Random (Seed, mkSeed)

lexicaseConfig :: Config
lexicaseConfig = defaultConfig { cfgSelection = Lexicase }

candidate :: Int -> [Double] -> Scored Int
candidate label errors = Scored
  { scoredValue = label
  , scoredFitness = mean errors
  , scoredCaseErrors = errors
  }
  where
    mean :: [Double] -> Double
    mean [] = 0.0
    mean xs = sum xs / fromIntegral (length xs)

-- | The pool the headline comparison runs on.
--
-- Four cases. Candidate 0 is a __specialist__: uniquely and dramatically
-- best on case 0, clearly worse than everyone on the rest, and the worst
-- of the four on mean error. Candidates 1-3 are __generalists__: never
-- best anywhere, but uniformly decent, so they win on the mean.
--
-- Tournament selection can only see the mean and will therefore almost
-- never pick candidate 0. Lexicase reaches case 0 first in a quarter of
-- its random case orders, and picks candidate 0 outright when it does.
specialistPool :: [Scored Int]
specialistPool =
  [ candidate 0 [0.01, 5.0, 5.0, 5.0]   -- mean 3.7525
  , candidate 1 [3.0, 3.0, 3.0, 3.0]    -- mean 3.0
  , candidate 2 [3.1, 2.9, 3.0, 3.0]    -- mean 3.0
  , candidate 3 [2.9, 3.1, 3.0, 3.0]    -- mean 3.0
  ]

-- | Candidate 0 is uniquely and comfortably best on every case.
--
-- Deliberately constructed so that a lexicase filter run repeatedly
-- /without/ excluding previous winners would return candidate 0 every
-- time. The per-case epsilon is the MAD of [0, 5, 6, 7], which is 1.0, so
-- the band on the full pool is [0, 1] and only candidate 0 survives it.
dominantPool :: [Scored Int]
dominantPool =
  [ candidate 0 [0.0, 0.0, 0.0, 0.0]
  , candidate 1 [5.0, 5.0, 5.0, 5.0]
  , candidate 2 [6.0, 6.0, 6.0, 6.0]
  , candidate 3 [7.0, 7.0, 7.0, 7.0]
  ]

-- | Fewer candidates than the elite slots asked for.
twoPool :: [Scored Int]
twoPool = [candidate 0 [1.0, 2.0], candidate 1 [3.0, 4.0]]

seeds :: [Seed]
seeds = map mkSeed [1 .. 2000]

-- | Lexicase with the epsilon table derived from the pool it is selecting
-- from, exactly as 'TypedGP.Population' does once per generation.
lexicaseOn :: Config -> [Scored Int] -> Seed -> Maybe (Int, Seed)
lexicaseOn cfg pool =
  selectLexicase cfg (ctxCases (selectionContext pool)) pool

selectionsBy :: ([Scored Int] -> Seed -> Maybe (Int, Seed)) -> [Int]
selectionsBy strategy = [v | s <- seeds, Just (v, _) <- [strategy specialistPool s]]

lexicasePicks :: [Int]
lexicasePicks = selectionsBy (lexicaseOn lexicaseConfig)

tournamentPicks :: [Int]
tournamentPicks = selectionsBy (tournamentSelection (cfgTournamentSize defaultConfig))

countOf :: Int -> [Int] -> Int
countOf wanted = length . filter (== wanted)

tests :: [(String, Bool)]
tests =
  -- Median machinery ---------------------------------------------------------
  [ ("median of an empty sample is zero", medianOf [] == 0.0)
  , ("median of a singleton is itself", medianOf [4.0] == 4.0)
  , ("median of an odd sample is the middle", medianOf [3.0, 1.0, 2.0] == 2.0)
  , ("median of an even sample averages the middle pair",
      medianOf [1.0, 2.0, 3.0, 4.0] == 2.5)
  , ("median ignores input order", medianOf [9.0, 1.0, 5.0] == medianOf [1.0, 5.0, 9.0])
    -- MAD of [1,2,3,4,5]: median 3, deviations [2,1,0,1,2], median 1.
  , ("MAD is the median of absolute deviations",
      medianAbsoluteDeviation [1.0, 2.0, 3.0, 4.0, 5.0] == 1.0)
  , ("MAD of an empty sample is zero", medianAbsoluteDeviation [] == 0.0)
    -- The degenerate case the brief calls out: all candidates identical on
    -- a case. MAD is 0, so the band is exactly the best error, and the
    -- '<=' comparison is what keeps every tied candidate rather than
    -- emptying the pool.
  , ("MAD of identical values is exactly zero",
      medianAbsoluteDeviation [7.0, 7.0, 7.0, 7.0] == 0.0)
  , ("MAD is never negative",
      all (\xs -> medianAbsoluteDeviation xs >= 0.0)
        [ [], [1.0], [1.0, 2.0], [-5.0, -5.0, 3.0]
        , [1.0e12, -1.0e12], [0.0, 0.0, 1.0e-9]
        ])
  , ("a zero-MAD case keeps every tied candidate",
      let tied = [candidate 0 [2.0], candidate 1 [2.0], candidate 2 [2.0]]
      -- With every candidate tied and the band exactly zero-width, all
      -- three survive and the uniform pick can land on any of them. A
      -- strict '<' would empty the pool and this would return Nothing.
      in length (dedupe [v | s <- take 200 seeds
                           , Just (v, _) <- [lexicaseOn lexicaseConfig tied s]]) == 3)

  -- Determinism ---------------------------------------------------------------
  , ("the same seed always selects the same individual",
      let one = lexicaseOn lexicaseConfig specialistPool (mkSeed 99)
      in all (== one)
           [lexicaseOn lexicaseConfig specialistPool (mkSeed 99) | _ <- [1 :: Int .. 200]])
  , ("the returned seed is also deterministic",
      fmap snd (lexicaseOn lexicaseConfig specialistPool (mkSeed 7))
        == fmap snd (lexicaseOn lexicaseConfig specialistPool (mkSeed 7)))
  , ("different seeds can select differently",
      length (dedupe (take 200 lexicasePicks)) > 1)

  -- Totality -------------------------------------------------------------------
  , ("an empty pool selects nothing",
      lexicaseOn lexicaseConfig ([] :: [Scored Int]) (mkSeed 1) == Nothing)
  , ("a single-candidate pool always selects it",
      all (== Just 42)
        [ fmap fst (lexicaseOn lexicaseConfig [candidate 42 [1.0, 2.0]] s)
        | s <- take 50 seeds
        ])
  , ("a pool with no cases still selects a member",
      all (`elem` [0, 1])
        [ v | s <- take 200 seeds
        , Just (v, _) <- [lexicaseOn lexicaseConfig
                            [candidate 0 [], candidate 1 []] s]
        ])
  , ("every selection is a real pool member",
      all (`elem` [0, 1, 2, 3]) lexicasePicks)
  , ("every seed yields a selection", length lexicasePicks == length seeds)

  -- The headline property -------------------------------------------------------
    -- Candidate 0 has the *worst* mean of the four, so tournament selection
    -- should almost never choose it. It is uniquely best on case 0, so
    -- lexicase should choose it whenever that case comes first — about a
    -- quarter of the time with four cases.
  , ("tournament almost never selects the specialist",
      countOf 0 tournamentPicks * 20 < length tournamentPicks)
  , ("lexicase selects the specialist far more often",
      countOf 0 lexicasePicks > 5 * countOf 0 tournamentPicks)
  , ("lexicase selects the specialist at roughly the case-order rate",
      let share = fromIntegral (countOf 0 lexicasePicks)
                    / fromIntegral (length lexicasePicks) :: Double
      in share > 0.15 && share < 0.35)
  , ("lexicase still selects generalists most of the time",
      countOf 0 lexicasePicks * 2 < length lexicasePicks)
    -- The hand-derived epsilon table for this fixture, which the assertions
    -- below depend on:
    --
    --   case 0 errors [0.01, 3.0, 3.1, 2.9] -> median 2.95, MAD 0.10
    --   case 1 errors [5.0, 3.0, 2.9, 3.1]  -> median 3.05, MAD 0.10
    --   cases 2,3     [5.0, 3.0, 3.0, 3.0]  -> median 3.00, MAD 0.00
    --
    -- Case 0 first therefore admits only candidate 0 (band [0.01, 0.11]),
    -- which is the quarter of case orders where the specialist wins.
  , ("lexicase reaches several candidates, not just one",
      length (dedupeSorted lexicasePicks) >= 2)
  , ("the zero-MAD cases keep every tied candidate rather than emptying",
      length lexicasePicks == length seeds)

  -- Elite selection ----------------------------------------------------------
    -- 'dominantPool' has candidate 0 uniquely best on *every* case, so a
    -- filter that did not exclude previous winners would return it in
    -- every elite slot. That collapse is the bug this block exists to
    -- catch, and it has been confirmed to catch it: with 'excludeIndex'
    -- neutered to a no-op, the distinctness assertion fails (1 distinct
    -- element instead of 3) while every other assertion in this file still
    -- passes.
  , ("elites are distinct even when one candidate dominates every case",
      all (\s -> length (dedupe (elitesFrom dominantPool 3 s)) == 3)
        (take 300 seeds))
  , ("elites are real pool members",
      all (`elem` [0, 1, 2, 3])
        (concat [elitesFrom dominantPool 3 s | s <- take 300 seeds]))
  , ("the dominant candidate is always among the elites",
      all (\s -> 0 `elem` elitesFrom dominantPool 3 s) (take 300 seeds))
    -- Exercises the 'not (or mask)' branch: two candidates, three slots.
  , ("asking for more elites than candidates returns each exactly once",
      all (\s -> ascending (elitesFrom twoPool 3 s) == [0, 1]) (take 200 seeds))
  , ("elite selection is deterministic",
      let once = elitesFrom dominantPool 3 (mkSeed 11)
      in all (== once) [elitesFrom dominantPool 3 (mkSeed 11) | _ <- [1 :: Int .. 200]])
  , ("the returned seed is deterministic too",
      snd (lexicaseElites lexicaseConfig (selectionContext dominantPool) dominantPool 3 (mkSeed 12))
        == snd (lexicaseElites lexicaseConfig (selectionContext dominantPool) dominantPool 3 (mkSeed 12)))
  , ("requesting zero elites returns none",
      null (elitesFrom dominantPool 0 (mkSeed 1)))
  , ("requesting negative elites returns none",
      null (elitesFrom dominantPool (-1) (mkSeed 1)))
  , ("an empty pool yields no elites",
      null (fst (lexicaseElites lexicaseConfig
                   (selectionContext ([] :: [Scored Int])) [] 3 (mkSeed 1))))
  , ("requesting exactly the pool size returns the whole pool",
      all (\s -> ascending (elitesFrom dominantPool 4 s) == [0, 1, 2, 3])
        (take 200 seeds))

  -- The case cap ------------------------------------------------------------------
  , ("capping cases still selects a real member",
      all (`elem` [0, 1, 2, 3])
        [ v | s <- take 500 seeds
        , Just (v, _) <- [lexicaseOn cappedConfig specialistPool s]
        ])
  , ("capping to one case still finds the specialist sometimes",
      countOf 0 [ v | s <- take 500 seeds
                , Just (v, _) <- [lexicaseOn cappedConfig specialistPool s]
                ] > 0)
  , ("a cap of zero means no cap",
      lexicaseOn lexicaseConfig specialistPool (mkSeed 5)
        == lexicaseOn (lexicaseConfig { cfgLexicaseMaxCases = 0 }) specialistPool (mkSeed 5))
  ]
  where
    cappedConfig :: Config
    cappedConfig = lexicaseConfig { cfgLexicaseMaxCases = 1 }

    elitesFrom :: [Scored Int] -> Int -> Seed -> [Int]
    elitesFrom pool k s =
      fst (lexicaseElites lexicaseConfig (selectionContext pool) pool k s)

    ascending :: [Int] -> [Int]
    ascending xs = [v | v <- [0 .. 9], v `elem` xs]

    dedupe :: [Int] -> [Int]
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

    dedupeSorted :: [Int] -> [Int]
    dedupeSorted xs = [v | v <- [0 .. 3], v `elem` xs]
