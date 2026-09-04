-- | Numerical optimisation of an expression's constants, with its
-- structure held fixed.
--
-- == Why this is a separate concern from evolution
--
-- Genetic programming is a search over /structures/. It is very good at
-- that and quite bad at the continuous problem hiding inside each one:
-- constants move only by random jitter in point mutation, which is a poor
-- way to solve a smooth k-dimensional minimisation. The benchmark suite
-- shows the split plainly — @4 x^1.7@ has its /shape/ found in every one
-- of 20 seeds while the exponent is missed, and @2 sin(3x + 0.5)@ fails
-- almost entirely because the frequency has to land inside the sine by
-- luck.
--
-- So: let evolution choose the shape, and hand the numbers to an optimiser.
--
-- == Why Nelder-Mead and not the compass search
--
-- 'compassSearch' came first and is kept here for comparison. It moves one
-- coordinate at a time, which is fatal on the shapes that matter: in
-- @c1 * x^c2@ the two constants are coupled, because raising the exponent
-- rescales the whole output. Any step in @c2@ alone looks worse unless
-- @c1@ moves with it, so the search stalls in the curved valley — the
-- textbook failure mode of coordinate descent, and one this package has a
-- regression test for.
--
-- 'nelderMead' moves the whole parameter vector at once, following the
-- valley rather than crossing it. It is derivative-free, which is required
-- here: the protected operators in "TypedGP.Eval" make the error surface
-- genuinely discontinuous, so a gradient method would be differentiating
-- something whose derivative does not exist exactly where individuals
-- congregate.
module TypedGP.LocalSearch
  ( -- * Constants in an expression
    constantsOf
  , withConstants

    -- * Optimisation
  , optimiseConstants
  , nelderMead
  , compassSearch

    -- * Settings
  , NelderMeadSettings (..)
  , defaultNelderMead
  ) where

import Data.List (sortOn)

import TypedGP.Config (Config (..))
import TypedGP.Data.Dataset (Dataset)
import TypedGP.Expr (Expr (..))
import TypedGP.Fitness (errorOf)

-- Constants ------------------------------------------------------------------

-- | Every constant in the expression, in pre-order.
constantsOf :: Expr -> [Double]
constantsOf expr = case expr of
  Const c -> [c]
  Var _   -> []
  Add a b -> constantsOf a ++ constantsOf b
  Sub a b -> constantsOf a ++ constantsOf b
  Mul a b -> constantsOf a ++ constantsOf b
  Div a b -> constantsOf a ++ constantsOf b
  Pow a b -> constantsOf a ++ constantsOf b
  Sin a   -> constantsOf a
  Cos a   -> constantsOf a
  Exp a   -> constantsOf a
  Log a   -> constantsOf a
  Sqrt a  -> constantsOf a
  Tanh a  -> constantsOf a
  Abs a   -> constantsOf a
  Gamma a -> constantsOf a
  Zeta a  -> constantsOf a
  -- Bounds and body, in pre-order, matching 'flatten'. Constants inside
  -- a binder are still constants and are still worth optimising; the
  -- ordinal numbering that 'withConstants' relies on must agree with
  -- this order exactly.
  Sum _ lo hi body -> constantsOf lo ++ constantsOf hi ++ constantsOf body

-- | Replace the constants, in pre-order, with the supplied values.
--
-- Extra values are ignored and missing ones leave the original constant in
-- place, so this is total for any list length. Callers always pass a list
-- produced by 'constantsOf' on the same expression, which makes those
-- cases unreachable in practice, but a length mismatch should degrade to a
-- slightly wrong fit rather than a crash.
withConstants :: [Double] -> Expr -> Expr
withConstants values expr = fst (go expr values)
  where
    go :: Expr -> [Double] -> (Expr, [Double])
    go node remaining = case node of
      Const c -> case remaining of
        (v : rest) -> (Const v, rest)
        []         -> (Const c, [])
      Var v   -> (Var v, remaining)
      Add a b -> binary Add a b
      Sub a b -> binary Sub a b
      Mul a b -> binary Mul a b
      Div a b -> binary Div a b
      Pow a b -> binary Pow a b
      Sin a   -> unary Sin a
      Cos a   -> unary Cos a
      Exp a   -> unary Exp a
      Log a   -> unary Log a
      Sqrt a  -> unary Sqrt a
      Tanh a  -> unary Tanh a
      Abs a   -> unary Abs a
      Gamma a -> unary Gamma a
      Zeta a  -> unary Zeta a
      -- Bounds then body, matching 'constantsOf' exactly. The two
      -- traversals share an ordinal numbering, and a disagreement between
      -- them would silently write optimised values into the wrong slots.
      Sum i lo hi body ->
        let (lo', rest1) = go lo remaining
            (hi', rest2) = go hi rest1
            (body', rest3) = go body rest2
        in (Sum i lo' hi' body', rest3)
     where
      unary build a =
        let (a', rest) = go a remaining
        in (build a', rest)

      binary build a b =
        let (a', rest1) = go a remaining
            (b', rest2) = go b rest1
        in (build a' b', rest2)

-- Nelder-Mead ----------------------------------------------------------------

-- | Tuning for the simplex search.
--
-- These are numerical-method parameters rather than search hyperparameters,
-- so they live here beside the algorithm rather than in "TypedGP.Config" —
-- the only reason to change one is a change to the method. The /budget/
-- for running it does belong in the config, and does.
data NelderMeadSettings = NelderMeadSettings
  { nmIterations :: !Int
    -- ^ Maximum simplex updates per restart.
  , nmRestarts :: !Int
    -- ^ Times to rebuild the simplex around the current best.
    --
    -- Nelder-Mead is prone to collapsing into a degenerate simplex and
    -- then reporting a point that is not a minimum. Restarting from the
    -- best point with a fresh, full-sized simplex is the standard remedy
    -- and costs far less than it recovers.
  , nmInitialStep :: !Double
    -- ^ Size of the initial simplex, relative to each coordinate.
  , nmTolerance :: !Double
    -- ^ Stop when the spread of objective values across the simplex falls
    -- below this, relative to their magnitude.
  } deriving (Eq, Show)

defaultNelderMead :: NelderMeadSettings
defaultNelderMead = NelderMeadSettings
  { nmIterations = 120
  , nmRestarts = 2
  , nmInitialStep = 0.5
  , nmTolerance = 1.0e-10
  }

-- Standard reflection, expansion, contraction and shrink coefficients.
-- Every move is expressed through 'along', so these are the multipliers on
-- the vector from the centroid to the worst vertex.
reflectionFactor, expansionFactor, contractionFactor, shrinkFactor :: Double
reflectionFactor = -1.0
expansionFactor = -2.0
contractionFactor = 0.5
shrinkFactor = 0.5

-- | Minimise a function of @n@ real parameters, without derivatives.
--
-- Returns the best parameter vector found. Total: an empty starting point
-- is returned unchanged, and every internal list access is guarded.
nelderMead :: NelderMeadSettings -> ([Double] -> Double) -> [Double] -> [Double]
nelderMead settings objective start
  | null start = start
  | otherwise = restartLoop (max 0 (nmRestarts settings)) start
  where
    restartLoop :: Int -> [Double] -> [Double]
    restartLoop remaining origin =
      let improved = descend (max 1 (nmIterations settings)) (buildSimplex origin)
      in if remaining <= 0 then improved else restartLoop (remaining - 1) improved

    -- The initial simplex: the origin, plus one vertex displaced along
    -- each axis. The displacement scales with the coordinate's own
    -- magnitude so that a constant near 1000 and one near 0.001 are both
    -- perturbed meaningfully, with an absolute floor so a coordinate
    -- sitting exactly at zero still moves.
    buildSimplex :: [Double] -> [(Double, [Double])]
    buildSimplex origin =
      sortOn fst (scored origin : [scored (nudge i origin) | i <- [0 .. length origin - 1]])

    nudge :: Int -> [Double] -> [Double]
    nudge axis point =
      [ if index == axis then value + stepFor value else value
      | (index, value) <- zip [0 :: Int ..] point
      ]

    stepFor :: Double -> Double
    stepFor value = nmInitialStep settings * (1.0 + abs value)

    scored :: [Double] -> (Double, [Double])
    scored point = (objective point, point)

    descend :: Int -> [(Double, [Double])] -> [Double]
    descend 0 simplex = bestOf simplex
    descend iterations simplex
      | converged simplex = bestOf simplex
      | otherwise = descend (iterations - 1) (update simplex)

    bestOf :: [(Double, [Double])] -> [Double]
    bestOf ((_, point) : _) = point
    -- Unreachable: the simplex always has length n+1 >= 2.
    bestOf []               = start

    converged :: [(Double, [Double])] -> Bool
    converged simplex = case (simplex, splitLast simplex) of
      (((bestValue, _) : _), Just (_, (worstValue, _))) ->
        worstValue - bestValue
          <= nmTolerance settings * (abs bestValue + nmTolerance settings)
      _ -> True

    -- One simplex update. The simplex is kept sorted ascending by
    -- objective value, so the head is the best vertex and the last is the
    -- worst.
    update :: [(Double, [Double])] -> [(Double, [Double])]
    update simplex = case splitLast simplex of
      -- Unreachable: non-empty by construction.
      Nothing -> simplex
      Just (better, (worstValue, worstPoint)) ->
        let centre = centroid (map snd better)
            reflected = scored (along centre reflectionFactor worstPoint)
            bestValue = case better of
              ((v, _) : _) -> v
              []           -> worstValue
            secondWorstValue = case splitLast better of
              Just (_, (v, _)) -> v
              Nothing          -> worstValue
        in if fst reflected < bestValue
             then
               -- The reflection improved on the whole simplex, so the
               -- search is heading somewhere useful; try going further.
               let expanded = scored (along centre expansionFactor worstPoint)
               in accept better (if fst expanded < fst reflected then expanded else reflected)
             else if fst reflected < secondWorstValue
               then accept better reflected
               else
                 -- The reflection was no better than the second worst
                 -- vertex, so the minimum is probably inside the simplex:
                 -- contract towards the centroid rather than away.
                 let contracted =
                       if fst reflected < worstValue
                         then scored (along centre (negate contractionFactor) worstPoint)
                         else scored (along centre contractionFactor worstPoint)
                 in if fst contracted < min worstValue (fst reflected)
                      then accept better contracted
                      -- Even contraction failed: pull every vertex towards
                      -- the best one and try again from a smaller simplex.
                      else shrink simplex

    accept :: [(Double, [Double])] -> (Double, [Double]) -> [(Double, [Double])]
    accept better replacement = sortOn fst (replacement : better)

    shrink :: [(Double, [Double])] -> [(Double, [Double])]
    shrink simplex = case simplex of
      ((_, bestPoint) : rest) ->
        sortOn fst
          ( head' simplex
          : [scored (along bestPoint shrinkFactor point) | (_, point) <- rest]
          )
      -- Unreachable: non-empty by construction.
      [] -> simplex
      where
        head' :: [(Double, [Double])] -> (Double, [Double])
        head' (entry : _) = entry
        head' []          = (objective start, start)

-- | @along centre t point@ is @centre + t * (point - centre)@,
-- componentwise.
--
-- Every simplex move is one of these, which keeps the geometry in a single
-- place: reflection is @t = -1@, expansion @-2@, outside contraction
-- @-0.5@, inside contraction @0.5@.
along :: [Double] -> Double -> [Double] -> [Double]
along centre t point =
  zipWith (\c p -> c + t * (p - c)) centre point

-- | Componentwise mean of a set of points.
--
-- Matching on the first point rather than testing for emptiness gives the
-- accumulator its width for free, with no unreachable branch for the
-- coverage checker to flag.
centroid :: [[Double]] -> [Double]
centroid [] = []
centroid (first : rest) = map (/ count) (foldr (zipWith (+)) zeroes (first : rest))
  where
    count :: Double
    count = fromIntegral (1 + length rest)

    zeroes :: [Double]
    zeroes = map (const 0.0) first

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x : xs) = fmap (\(front, back) -> (x : front, back)) (splitLast xs)

-- Compass search -------------------------------------------------------------

-- | Coordinate-wise pattern search: try a step up and down on each
-- parameter, keep improvements, halve the step when a whole sweep fails.
--
-- Retained for comparison and for the regression test that documents its
-- limitation. It is a perfectly good optimiser for /separable/ problems
-- and markedly cheaper than 'nelderMead'; it simply cannot follow a curved
-- valley, because it never moves two parameters at once.
compassSearch :: Int -> Double -> Double -> ([Double] -> Double) -> [Double] -> [Double]
compassSearch rounds initialStep minimumStep objective start
  | null start = start
  | otherwise = descend rounds initialStep start
  where
    count :: Int
    count = length start

    descend :: Int -> Double -> [Double] -> [Double]
    descend 0 _ values = values
    descend remaining step values
      | step < minimumStep = values
      | otherwise =
          let swept = foldl (sweep step) values [0 .. count - 1]
          in if objective swept < objective values
               then descend (remaining - 1) step swept
               else descend (remaining - 1) (step * 0.5) values

    sweep :: Double -> [Double] -> Int -> [Double]
    sweep step values axis =
      let up = adjust axis (+ step) values
          down = adjust axis (subtract step) values
      in pickBest (values : [up, down])

    pickBest :: [[Double]] -> [Double]
    pickBest candidates = case sortOn objective candidates of
      (best : _) -> best
      -- Unreachable: the candidate list always contains the current point.
      []         -> start

    adjust :: Int -> (Double -> Double) -> [Double] -> [Double]
    adjust axis f values =
      [if index == axis then f v else v | (index, v) <- zip [0 :: Int ..] values]

-- Expression-level entry point -------------------------------------------------

-- | Refit an expression's constants to a dataset, leaving its structure
-- alone.
--
-- Finds a local optimum, not a global one, which is the right ambition: it
-- starts from values evolution already selected, and the job is to polish
-- them rather than to rediscover the problem.
optimiseConstants :: Config -> Dataset -> Expr -> Expr
optimiseConstants cfg ds expr
  | null start = expr
  | otherwise = withConstants improved expr
  where
    start :: [Double]
    start = constantsOf expr

    objective :: [Double] -> Double
    objective values = errorOf (cfgErrorMetric cfg) ds (withConstants values expr)

    settings :: NelderMeadSettings
    settings = defaultNelderMead
      { nmIterations = max 1 (cfgRefineIterations cfg) }

    candidate :: [Double]
    candidate = nelderMead settings objective start

    -- Guard against a pathological run making things worse. The simplex
    -- method is not monotone in the way a line search is — a restart can
    -- in principle land somewhere poorer — so the result is only adopted
    -- if it actually improved on the starting point.
    improved :: [Double]
    improved
      | objective candidate <= objective start = candidate
      | otherwise = start
