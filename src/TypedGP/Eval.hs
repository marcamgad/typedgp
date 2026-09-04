{-# LANGUAGE BangPatterns #-}
-- | The interpreter: 'Expr' plus an environment gives a 'Double'.
--
-- Every arithmetic result in this module passes through 'sanitize', so
-- 'eval' is __total and always returns a finite 'Double'__. That property
-- is load-bearing: a single @NaN@ escaping into a fitness score poisons
-- every comparison it takes part in (@NaN < x@ and @NaN > x@ are both
-- 'False'), which silently corrupts selection for the rest of the run.
module TypedGP.Eval
  ( -- * Environments
    Env

    -- * Evaluation
  , eval
  , evalChecked
    -- * Domain validity
  , EvalResult (..)
  , DomainError (..)
  , evalDomain
  , evalResultValue
  , evalResultError
  , isValid

    -- * Protected primitives
  , protectedDiv
  , protectedLog
  , protectedSqrt
  , protectedExp
  , protectedPow
  , protectedGamma
  , protectedZeta
  , sanitize

    -- * Special function kernels
  , gammaFunction
  , logGammaPositive
  , zetaEulerMaclaurin

    -- * Numeric guard rails
  , divisionEpsilon
  , magnitudeCap
  , iterationCap
  , binderRange
  , gammaPoleEpsilon
  , zetaPoleEpsilon
  ) where

import TypedGP.Expr (Expr (..), VarName)

-- | Variable bindings for one evaluation. An association list is the
-- simplest thing that works; problems here have a handful of variables, so
-- the linear 'lookup' is not the bottleneck (fitness is dominated by the
-- number of data points, not the number of columns).
type Env = [(VarName, Double)]

-- | Denominators smaller than this in magnitude are treated as zero.
divisionEpsilon :: Double
divisionEpsilon = 1.0e-9

-- | Results are clamped to @[-magnitudeCap, magnitudeCap]@.
--
-- Chosen well below @1e308@ so that a later @x * x@ in a fitness metric
-- cannot overflow to infinity: @1e12 * 1e12 == 1e24@, still finite.
magnitudeCap :: Double
magnitudeCap = 1.0e12

-- | Force a 'Double' into the finite, bounded range the rest of the engine
-- assumes. @NaN@ collapses to @0@ rather than to a cap, because @NaN@
-- carries no sign information to preserve.
sanitize :: Double -> Double
sanitize x
  | isNaN x                 = 0.0
  | isInfinite x, x > 0     = magnitudeCap
  | isInfinite x            = negate magnitudeCap
  | x > magnitudeCap        = magnitudeCap
  | x < negate magnitudeCap = negate magnitudeCap
  | otherwise               = x

-- | Division that degrades instead of exploding. Dividing by (near) zero
-- yields @1.0@, the conventional genetic-programming sentinel: it is the
-- multiplicative identity, so the surrounding expression stays meaningful
-- and the individual is not automatically killed off.
protectedDiv :: Double -> Double -> Double
protectedDiv a b
  | abs b < divisionEpsilon = 1.0
  | otherwise               = sanitize (a / b)

-- | Logarithm of @|x|@, with @log 0@ mapped to @0@.
--
-- Exported ahead of a @Log@ constructor in "TypedGP.Expr": the protection
-- policy belongs here, next to the other guards, not in whichever module
-- adds the operator later.
protectedLog :: Double -> Double
protectedLog x
  | abs x < divisionEpsilon = 0.0
  | otherwise               = sanitize (log (abs x))

-- | Square root of @|x|@, so negative inputs stay real.
protectedSqrt :: Double -> Double
protectedSqrt = sanitize . sqrt . abs

-- | Exponential, clamped.
--
-- @exp@ overflows to infinity above about 709, which 'sanitize' turns into
-- the cap. Worth its own name rather than an inline @sanitize (exp x)@
-- because overflow is the common case here, not the exotic one: an evolved
-- tree containing @exp(exp(x))@ saturates almost everywhere.
protectedExp :: Double -> Double
protectedExp = sanitize . exp

-- | Exponentiation, total over the whole real plane.
--
-- Real exponentiation is the messiest domain in the operator set, because
-- it is undefined on a two-dimensional /region/ rather than at isolated
-- points: @base ** expo@ is not real for any negative base with a
-- non-integer exponent. IEEE returns @NaN@ across that whole quadrant, so
-- an unprotected @Pow@ would be the single largest source of poisoned
-- fitness scores in the engine.
--
-- The rules, in the order they are applied:
--
-- [@expo == 0@] Always @1@, including @0 ** 0@. Choosing @1@ rather than
--   leaving it undefined keeps @x ** 0@ a safe simplification.
--
-- [@base == 0@] @0@ for a positive exponent; the @1.0@ pole sentinel for a
--   negative one, matching 'protectedDiv' — @0 ** -2@ is a division by
--   zero wearing a different hat.
--
-- [@base > 0@] Ordinary @**@, clamped. This is the overwhelmingly common
--   case and is exactly the textbook function.
--
-- [@base < 0@] The magnitude, @|base| ** expo@ — the same convention
--   'protectedLog' and 'protectedSqrt' already use, so the engine has one
--   house rule rather than three.
--
-- __That last rule is the operator's definition, not a fallback.__ This is
-- the resolution recorded in @docs\/phase5-pow-semantics.md@, and it is a
-- deliberate change from an earlier implementation that kept the sign for
-- integer exponents and dropped it otherwise. That version was
-- discontinuous at every integer: @(-0.6) ** y@ gave @+0.36@ at @y = 2@,
-- @-0.216@ at @y = 3@ and @+0.6 ** 2.5@ at @y = 2.5@. On continuously
-- distributed data the integer points have measure zero, which is why it
-- went unnoticed until the domain breakdown forced a look, and it is not
-- something a derivative rule can be written against.
--
-- So @Pow@ /means/ @|base| ** expo@. Under that reading the operator is
-- total, continuous, and differentiable away from @base == 0@, and the
-- negative-base case stops being an error because there was never an
-- error — only an underspecified operator.
--
-- The cost, stated rather than buried: @(-2) ** 3@ is @+8@ here, not
-- @-8@. An odd integer power of a negative base is well defined and this
-- can no longer spell it. The function remains reachable as
-- @Mul x (Mul x x)@, which is exact and continuous, so what is lost is a
-- compact spelling rather than an expressible shape — but it is a trade.
-- If a benchmark carrying a genuine odd power ever regresses, suspect this
-- first.
protectedPow :: Double -> Double -> Double
protectedPow base expo
  -- Cannot arise from 'eval', which never produces either; present so the
  -- function is total for direct callers.
  | isNaN base || isNaN expo = 0.0
  | expo == 0.0 = 1.0
  | base == 0.0 = if expo > 0.0 then 0.0 else 1.0
  | otherwise = sanitize (abs base ** expo)

-- Special functions ---------------------------------------------------------

-- | Distance within which an argument counts as sitting on a pole of the
-- gamma function.
gammaPoleEpsilon :: Double
gammaPoleEpsilon = 1.0e-9

-- | Likewise for the single pole of the Riemann zeta function at @s = 1@.
zetaPoleEpsilon :: Double
zetaPoleEpsilon = 1.0e-9

-- | Lanczos parameter @g@, paired with the coefficient table below. The
-- two must be changed together — the coefficients are derived for this
-- specific @g@.
lanczosG :: Double
lanczosG = 7.0

-- | Lanczos coefficients for @g = 7@, @n = 9@.
--
-- These are the widely circulated values from Lanczos' 1964 paper as
-- tabulated in Numerical Recipes and Boost.Math; they give roughly 15
-- significant digits over the half-plane where they are applied. They are
-- reproduced verbatim rather than derived, so they should be checked
-- against a published table if ever edited.
lanczosCoefficients :: [Double]
lanczosCoefficients =
  [ 0.99999999999980993
  , 676.5203681218851
  , -1259.1392167224028
  , 771.32342877765313
  , -176.61502916214059
  , 12.507343278686905
  , -0.13857109526572012
  , 9.9843695780195716e-6
  , 1.5056327351493116e-7
  ]

-- | @log (gamma z)@ for @z >= 0.5@, by the Lanczos approximation.
--
-- Computed in log space deliberately. The direct product
-- @t**(z+0.5) * exp(-t) * series@ overflows and underflows independently
-- for quite modest @z@, losing precision long before the result itself
-- would overflow; in log space the cancellation happens before the
-- exponential.
logGammaPositive :: Double -> Double
logGammaPositive z =
  let shifted = z - 1.0
      series = case lanczosCoefficients of
        (c0 : rest) ->
          c0 + sum (zipWith termAt [1 :: Int ..] rest)
          where
            termAt i c = c / (shifted + fromIntegral i)
        -- Unreachable: the coefficient table is a non-empty literal.
        [] -> 1.0
      t = shifted + lanczosG + 0.5
  in 0.5 * log (2.0 * pi) + (shifted + 0.5) * log t - t + log series

-- | The gamma function on the reals.
--
-- Uses the Lanczos approximation above @0.5@ and Euler's reflection
-- formula @gamma(z) * gamma(1-z) = pi / sin(pi z)@ below it. The recursion
-- terminates immediately: @z < 0.5@ implies @1 - z > 0.5@, which is the
-- base case.
--
-- Unprotected — it will happily return an infinity at a pole. Callers
-- should use 'protectedGamma'.
gammaFunction :: Double -> Double
gammaFunction z
  | z >= 0.5  = exp (logGammaPositive z)
  | otherwise = pi / (sin (pi * z) * gammaFunction (1.0 - z))

-- | Gamma, with its poles handled.
--
-- Gamma has simple poles at every non-positive integer. As with
-- 'protectedDiv', those yield the sentinel @1.0@: it is the multiplicative
-- identity, so an individual that strays onto a pole is neutralised rather
-- than killed, and the surrounding expression keeps whatever meaning it
-- had.
protectedGamma :: Double -> Double
protectedGamma x
  | isGammaPole x = 1.0
  | otherwise     = sanitize (gammaFunction x)

isGammaPole :: Double -> Bool
isGammaPole x
  -- Guarded before 'round', which has no meaningful answer for either.
  | isNaN x || isInfinite x = True
  | x > 0.0 = False
  -- Safe: 'eval' clamps to magnitudeCap (1e12), far inside Int's range.
  | otherwise = abs (x - fromIntegral (round x :: Int)) < gammaPoleEpsilon

-- | Number of explicit terms in the Euler-Maclaurin sum for zeta.
zetaTerms :: Int
zetaTerms = 24

-- | @B(2j) / (2j)!@ for @j = 1..4@, the Euler-Maclaurin correction
-- coefficients: @1/12@, @-1/720@, @1/30240@, @-1/1209600@.
zetaCorrectionCoefficients :: [Double]
zetaCorrectionCoefficients =
  [ 1.0 / 12.0
  , -1.0 / 720.0
  , 1.0 / 30240.0
  , -1.0 / 1209600.0
  ]

-- | Below this the Euler-Maclaurin expansion stops being trustworthy and
-- the reflection formula takes over.
--
-- With @M@ correction terms the expansion is valid for @s > 1 - 2M@; four
-- terms therefore give @s > -7@. This is not a tuning knob — it is
-- determined by the length of 'zetaCorrectionCoefficients' and must move
-- with it.
zetaContinuationFloor :: Double
zetaContinuationFloor = -7.0

-- | Below this, reflection would need @gamma(1 - s)@ beyond the range of a
-- 'Double', so the result saturates instead.
zetaReflectionFloor :: Double
zetaReflectionFloor = -160.0

-- | Riemann zeta by Euler-Maclaurin summation.
--
-- > zeta(s) = sum_{k=1}^{N-1} k^-s
-- >         + N^(1-s)/(s-1)
-- >         + N^-s/2
-- >         + sum_j B(2j)/(2j)! * (s)_(2j-1) * N^(-s-2j+1)
--
-- where @(s)_m@ is the rising factorial. This is the standard textbook
-- formula (Edwards, /Riemann's Zeta Function/, ch. 6; also NIST DLMF
-- 25.2.9).
--
-- Worth knowing: this is not merely a fast way to sum a convergent series,
-- it /is/ the analytic continuation. It gives correct values below the
-- line of convergence, so no separate branch is needed for the critical
-- strip or for moderate negatives — @zeta(0) = -1/2@ and
-- @zeta(-1) = -1/12@ both fall straight out. Unprotected at @s = 1@, where
-- the second term is a genuine pole.
zetaEulerMaclaurin :: Double -> Double
zetaEulerMaclaurin s =
  explicitSum + poleTerm + halfTerm + corrections
  where
    n :: Double
    n = fromIntegral zetaTerms

    explicitSum :: Double
    explicitSum = sum [fromIntegral k ** negate s | k <- [1 .. zetaTerms - 1]]

    poleTerm :: Double
    poleTerm = n ** (1.0 - s) / (s - 1.0)

    halfTerm :: Double
    halfTerm = 0.5 * (n ** negate s)

    corrections :: Double
    corrections = sum (zipWith correction [1 :: Int ..] zetaCorrectionCoefficients)

    correction :: Int -> Double -> Double
    correction j coefficient =
      let order = 2 * j - 1
      in coefficient
           * risingFactorial s order
           * n ** (negate s - fromIntegral order)

    risingFactorial :: Double -> Int -> Double
    risingFactorial base order =
      product [base + fromIntegral i | i <- [0 .. order - 1]]

-- | Riemann zeta, with its pole handled and its far tail saturated.
--
-- Three regimes: Euler-Maclaurin where it is valid, the functional
-- equation
-- @zeta(s) = 2^s pi^(s-1) sin(pi s/2) gamma(1-s) zeta(1-s)@
-- below that, and saturation where even the reflected form exceeds a
-- 'Double'. The pole at @s = 1@ yields the same @1.0@ sentinel the other
-- protected operations use.
--
-- The trivial zeros at negative even integers come out of the @sin@ factor
-- for free, which is a useful check that the reflection is wired up right.
protectedZeta :: Double -> Double
protectedZeta s
  | isNaN s || isInfinite s = 1.0
  | abs (s - 1.0) < zetaPoleEpsilon = 1.0
  | s >= zetaContinuationFloor = sanitize (zetaEulerMaclaurin s)
  | s < zetaReflectionFloor = sanitize (magnitudeCap * signum (sin (pi * s / 2.0)))
  | otherwise = sanitize (zetaReflected s)

zetaReflected :: Double -> Double
zetaReflected s =
  2.0 ** s
    * pi ** (s - 1.0)
    * sin (pi * s / 2.0)
    * gammaFunction (1.0 - s)
    * zetaEulerMaclaurin (1.0 - s)

-- | Evaluate an expression. Total: unbound variables read as @0@ and every
-- operator is protected, so this never throws and never returns @NaN@.
--
-- Use 'evalChecked' when you want an unbound variable to be an error
-- (parsing a user-supplied formula) rather than a zero (scoring an evolved
-- individual against a dataset, where a stray variable is just a bad gene).
eval :: Env -> Expr -> Double
eval env = go
  where
    go :: Expr -> Double
    go expr = case expr of
      Const c -> sanitize c
      Var v   -> maybe 0.0 sanitize (lookup v env)
      Add a b -> sanitize (go a + go b)
      Sub a b -> sanitize (go a - go b)
      Mul a b -> sanitize (go a * go b)
      Div a b -> protectedDiv (go a) (go b)
      Pow a b -> protectedPow (go a) (go b)
      Sin a   -> sanitize (sin (go a))
      Cos a   -> sanitize (cos (go a))
      Exp a   -> protectedExp (go a)
      Log a   -> protectedLog (go a)
      Sqrt a  -> protectedSqrt (go a)
      -- tanh and abs are total and bounded on the reals, so they need no
      -- protection beyond the usual clamp.
      Tanh a  -> sanitize (tanh (go a))
      Abs a   -> sanitize (abs (go a))
      Gamma a -> protectedGamma (go a)
      Zeta a  -> protectedZeta (go a)
      Sum name lo hi body -> sumOver env name (go lo) (go hi) body

-- Domain validity -----------------------------------------------------------

-- | Why an expression was not mathematically meaningful at a point.
--
-- Distinct from /numerical/ trouble. Every one of these conditions has a
-- protected fallback that produces a perfectly finite number; the point of
-- naming them is that the number is a fiction.
data DomainError
  = DividedByZero
    -- ^ A denominator within 'divisionEpsilon' of zero, or @0 ** negative@,
    -- which is the same fault wearing a different hat.
  | LogOfNonPositive
    -- ^ @log@ of zero or a negative. Note this covers the whole
    -- non-positive half-line: 'protectedLog' computes @log |x|@, so
    -- @log (-5)@ returns a finite, plausible, wrong number rather than
    -- tripping any numeric guard.
  | PowOfNegativeBase
    -- ^ A negative base with a non-integer exponent — not a real number.
  | GammaAtPole
    -- ^ Gamma at a non-positive integer.
  | ZetaAtPole
    -- ^ Zeta at its single pole, @s = 1@.
  | IterationCapped
    -- ^ A 'Sum' whose range exceeded 'iterationCap', so only the first
    -- 'iterationCap' terms were added.
    --
    -- Grouped with the genuine domain errors rather than with
    -- 'Saturated', and the distinction is the one drawn in the §5
    -- amendment: a capped sum is __not the value of the expression at
    -- all__, whereas a saturated one is the right value, truncated by the
    -- representation.
  | Saturated
    -- ^ A result clamped at 'magnitudeCap'. Not a domain error in the
    -- mathematical sense, but indistinguishable from one downstream: the
    -- value reported is not the value of the expression.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | An evaluation that knows whether it was telling the truth.
--
-- 'Invalid' carries the protected fallback alongside the error, which is a
-- deliberate deviation from the shape proposed in @docs\/phase2-design.md@
-- §5. Evaluation is a tree traversal: a subexpression that leaves its
-- domain still has to hand a number to whatever encloses it, or the whole
-- expression collapses to an error and the engine loses the fitness value
-- it needs in order to rank the individual at all. Carrying the value
-- keeps the traversal single-pass. See @docs\/phase3-domain-design.md@.
--
-- So this is __not__ a partiality type. Every 'EvalResult' has a usable
-- 'Double' inside it, and 'evalResultValue' is total.
data EvalResult
  = Valid !Double
  | Invalid !DomainError !Double
  deriving (Eq, Show)

-- | The number, whether or not it was meaningful.
--
-- Total by construction: 'Invalid' carries its fallback.
evalResultValue :: EvalResult -> Double
evalResultValue (Valid x)     = x
evalResultValue (Invalid _ x) = x

evalResultError :: EvalResult -> Maybe DomainError
evalResultError (Valid _)     = Nothing
evalResultError (Invalid e _) = Just e

isValid :: EvalResult -> Bool
isValid (Valid _)     = True
isValid Invalid {}    = False

-- | Evaluate, tracking domain validity.
--
-- Produces exactly the same 'Double' as 'eval' for every input — the
-- protection policy is unchanged and this shares it — while additionally
-- reporting the first domain violation encountered in a left-to-right
-- traversal. Only the first is kept: the penalty downstream asks "was this
-- point out of domain at all", so a count or a set would cost the hot loop
-- something to answer a question nobody puts.
--
-- Left-to-right is arbitrary but stable, which makes it testable.
--
-- This is a second traversal rather than a replacement for 'eval'.
-- Rewriting 'eval' in terms of it would be tidier and would silently
-- change the performance profile of the engine's hot loop.
evalDomain :: Env -> Expr -> EvalResult
evalDomain env = go
  where
    go :: Expr -> EvalResult
    go expr = case expr of
      Const c -> saturating (sanitize c) c
      Var v   -> let raw = maybe 0.0 id (lookup v env)
                 in saturating (sanitize raw) raw
      Add a b -> arith (+) a b
      Sub a b -> arith (-) a b
      Mul a b -> arith (*) a b

      Div a b -> binary a b $ \x y ->
        if abs y < divisionEpsilon
          then Invalid DividedByZero 1.0
          else saturating (protectedDiv x y) (x / y)

      Pow a b -> binary a b $ \base expo ->
        let value = protectedPow base expo
        in if base == 0.0 && expo < 0.0
             -- A pole, and the same one 'protectedDiv' guards.
             then Invalid DividedByZero value
             -- A negative base is no longer an error. 'Pow' means
             -- @|base| ** expo@ (see 'protectedPow'), which is defined
             -- for every base, so there is nothing here to report.
             -- 'PowOfNegativeBase' survives as a constructor only so the
             -- type and the checkpoint format are undisturbed; nothing
             -- produces it, and a test asserts that.
             else saturatingAt value

      Log a -> unary a $ \x ->
        let value = protectedLog x
        -- Deliberately the whole non-positive half-line, not the
        -- epsilon-neighbourhood 'protectedLog' guards on. The two are
        -- different predicates and conflating them is the bug this
        -- module now exists to stop making.
        in if x <= 0.0 then Invalid LogOfNonPositive value else saturatingAt value

      Gamma a -> unary a $ \x ->
        let value = protectedGamma x
        in if isGammaPole x then Invalid GammaAtPole value else saturatingAt value

      Zeta a -> unary a $ \s ->
        let value = protectedZeta s
        in if abs (s - 1.0) < zetaPoleEpsilon
             then Invalid ZetaAtPole value
             else saturatingAt value

      -- Total and in-domain everywhere on the reals. They can still
      -- saturate, which is the only thing left to report.
      Sin a  -> unary a (saturatingAt . sanitize . sin)
      Cos a  -> unary a (saturatingAt . sanitize . cos)
      Tanh a -> unary a (saturatingAt . sanitize . tanh)
      Abs a  -> unary a (saturatingAt . sanitize . abs)
      Exp a  -> unary a (saturatingAt . protectedExp)
      -- 'Sqrt' means @sqrt |x|@, for exactly the reason 'Pow' means
      -- @|base| ** expo@: 'protectedSqrt' already computes it, and reading
      -- it that way makes the operator total, continuous, and
      -- differentiable away from 0 rather than undefined on half the line.
      -- Settled consistently with the Pow decision rather than left open;
      -- see docs/phase5-pow-semantics.md.
      Sqrt a -> unary a (saturatingAt . protectedSqrt)

      -- The binder case. Three things can go wrong and they are reported
      -- in priority order: a child that is already invalid, the iteration
      -- cap truncating the range, then ordinary saturation.
      --
      -- The body is checked with the index bound, and its verdict is
      -- taken from the FIRST iteration only. Checking every term would
      -- multiply the cost of a domain query by the range length, and a
      -- body that leaves its domain at some index almost always does so
      -- at the first one it is evaluated at. Stated because it is a real
      -- limitation: a body invalid only at, say, i = 700 is not reported.
      Sum name lo hi body ->
        let loR = go lo
            hiR = go hi
            loV = evalResultValue loR
            hiV = evalResultValue hiR
            (indices, capped) = binderRange loV hiV
            value = sumOver env name loV hiV body
            firstBody = case indices of
              (i : _) -> evalResultError
                           (evalDomain ((name, fromIntegral i) : env) body)
              []      -> Nothing
        in case evalResultError loR of
             Just e -> Invalid e value
             Nothing -> case evalResultError hiR of
               Just e -> Invalid e value
               Nothing -> case firstBody of
                 Just e -> Invalid e value
                 Nothing
                   | capped    -> Invalid IterationCapped value
                   | otherwise -> saturatingAt value

    -- Propagate an earlier error if there was one, otherwise judge this
    -- node. The first error in the traversal is the one that survives.
    unary :: Expr -> (Double -> EvalResult) -> EvalResult
    unary a f = case go a of
      Invalid e x -> Invalid e (evalResultValue (f x))
      Valid x     -> f x

    binary :: Expr -> Expr -> (Double -> Double -> EvalResult) -> EvalResult
    binary a b f =
      let ra = go a
          rb = go b
          -- Computed once. The node's own verdict is discarded when a
          -- child already failed, but its *value* is still the value of
          -- this subexpression and the enclosing node needs it.
          here = f (evalResultValue ra) (evalResultValue rb)
      in case evalResultError ra of
           Just e  -> Invalid e (evalResultValue here)
           Nothing -> case evalResultError rb of
             Just e  -> Invalid e (evalResultValue here)
             Nothing -> here

    arith :: (Double -> Double -> Double) -> Expr -> Expr -> EvalResult
    arith f a b = binary a b $ \x y -> saturating (sanitize (f x y)) (f x y)

    -- | Did 'sanitize' actually clamp? Compared against the cap rather
    -- than detected inside 'sanitize', so the protection code stays
    -- untouched. A genuine result of exactly ±magnitudeCap is
    -- indistinguishable from a clamp, and treating it as one is harmless.
    saturating :: Double -> Double -> EvalResult
    saturating value raw
      | isNaN raw || isInfinite raw = Invalid Saturated value
      | otherwise                   = saturatingAt value

    saturatingAt :: Double -> EvalResult
    saturatingAt value
      | abs value >= magnitudeCap = Invalid Saturated value
      | otherwise                 = Valid value

-- | Like 'eval', but reports unbound variables instead of defaulting them.
-- Arithmetic protection is identical.
evalChecked :: Env -> Expr -> Either String Double
evalChecked env = go
  where
    go :: Expr -> Either String Double
    go expr = case expr of
      Const c -> Right (sanitize c)
      Var v   -> case lookup v env of
        Just x  -> Right (sanitize x)
        Nothing -> Left ("unbound variable: " ++ v)
      Add a b -> bin (+) a b
      Sub a b -> bin (-) a b
      Mul a b -> bin (*) a b
      Div a b -> protectedDiv <$> go a <*> go b
      Pow a b -> protectedPow <$> go a <*> go b
      Sin a   -> un sin a
      Cos a   -> un cos a
      Tanh a  -> un tanh a
      Abs a   -> un abs a
      -- Already total and clamped, so they bypass the sanitising wrapper
      -- rather than being clamped twice.
      Exp a   -> protectedExp <$> go a
      Log a   -> protectedLog <$> go a
      Sqrt a  -> protectedSqrt <$> go a
      Gamma a -> protectedGamma <$> go a
      Zeta a  -> protectedZeta <$> go a
      -- Bounds are checked in the enclosing scope; the body is checked
      -- with the index bound, so a body mentioning the index is not an
      -- unbound variable. Reports the first unbound name in
      -- left-to-right order like every other case here.
      Sum name lo hi body -> do
        loV <- go lo
        hiV <- go hi
        _ <- evalChecked ((name, 0.0) : env) body
        Right (sumOver env name loV hiV body)
     where
      un :: (Double -> Double) -> Expr -> Either String Double
      un f a = (sanitize . f) <$> go a

      bin :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
      bin f a b = sanitize <$> (f <$> go a <*> go b)

-- Binders --------------------------------------------------------------------

-- | Maximum terms any one 'Sum' will evaluate.
--
-- __A guard rail, not a hyperparameter, and that is a deliberate deviation
-- from §3 of @docs\/phase2-design.md@__, which specified a
-- @cfgMaxIterations@ field in "TypedGP.Config".
--
-- Two reasons it lives here instead. First, "TypedGP.Config"'s own stated
-- rule: numeric guard rails are "correctness invariants of the interpreter,
-- not search knobs; tuning them changes what the language /means/" — and
-- how many terms a sum has is exactly that. Second, and decisively, 'eval'
-- takes no 'TypedGP.Config.Config' and is called from everywhere; adding
-- one to reach a single integer would be a signature change across the
-- whole engine in service of a knob nobody should turn.
--
-- 1000 follows §3's figure. It is a termination guarantee: without it a
-- single evolved @Sum@ with bounds @-1e12 .. 1e12@ makes one 'eval' call
-- effectively non-terminating, and 'eval' runs once per data point per
-- individual per generation.
iterationCap :: Int
iterationCap = 1000

-- | The integer range a 'Sum' actually iterates over, and whether the
-- 'iterationCap' truncated it.
--
-- Shared by 'eval' and 'evalDomain' so the two cannot disagree about what
-- was summed — the value and the verdict are derived from one decision.
--
-- Bounds are __rounded to nearest__ rather than truncated: @2.9999@ and
-- @3.0@ are indistinguishable after any floating-point arithmetic and
-- almost always mean the same intended bound.
--
-- @lo > hi@ is the empty sum, which is @0@ by the usual convention, so
-- @Sum i 1 0 body@ is harmless rather than a special case.
binderRange :: Double -> Double -> ([Integer], Bool)
binderRange lo hi
  | from > to = ([], False)
  | wanted > toInteger iterationCap =
      ([from .. from + toInteger iterationCap - 1], True)
  | otherwise = ([from .. to], False)
  where
    -- Sanitised first, so an infinite or NaN bound becomes a finite (if
    -- enormous) one and is then handled by the cap rather than overflowing
    -- 'round'.
    from = round (sanitize lo) :: Integer
    to = round (sanitize hi) :: Integer
    wanted = to - from + 1

-- | Evaluate a 'Sum', extending the environment with the bound index.
--
-- Shadowing falls out of 'Env' being an association list with 'lookup'
-- taking the first match: an inner @i@ simply hides an outer one, with no
-- scope-tracking machinery.
sumOver :: Env -> VarName -> Double -> Double -> Expr -> Double
sumOver env name lo hi body =
  sanitize (strictFold step 0.0 (fst (binderRange lo hi)))
  where
    step :: Double -> Integer -> Double
    step !acc i = sanitize (acc + eval ((name, fromIntegral i) : env) body)

    -- Local, so this module needs no CPP shim for the base 4.20 Prelude
    -- change and no name that shadows it.
    strictFold :: (b -> a -> b) -> b -> [a] -> b
    strictFold f = walk
      where
        walk !acc []       = acc
        walk !acc (x : xs) = walk (f acc x) xs
