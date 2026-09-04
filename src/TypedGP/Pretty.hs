-- | Rendering an 'Expr' as readable infix mathematics.
--
-- Separate from "TypedGP.Eval" on purpose: formatting concerns (a LaTeX
-- backend, S-expression output, configurable precision) grow independently
-- of evaluation semantics, and neither should be able to break the other.
--
-- The precedence machinery is shared by every renderer via
-- 'prettyWithConstants', which parameterises how each constant leaf is
-- printed. That is what lets "TypedGP.Uncertainty" print
-- @(0.297 +/- 0.02) * x@ without duplicating the bracketing rules — the
-- one part of a pretty-printer that is genuinely easy to get subtly wrong.
--
-- A future @prettyLatex@ should extend the same way: parameterise the
-- operator symbols, do not copy the traversal.
module TypedGP.Pretty
  ( pretty
  , prettyPrec
  , prettyWithConstants
  , ConstRenderer
  , defaultConstRenderer
  , showConstant
  ) where

import Numeric (showEFloat, showFFloat)

import TypedGP.Expr (Expr (..), provablyNonNegative)

-- | How to render a constant leaf.
--
-- The 'Int' is the leaf's ordinal in pre-order (the 0th constant, the 1st,
-- ...), which is how a caller keyed by position — such as a table of
-- bootstrap confidence intervals — finds the right annotation.
--
-- The 'Bool' in the result says whether the rendered text must be
-- bracketed when it appears as an operand. The renderer decides, because
-- only it knows what it produced: a bare negative number needs brackets,
-- and so does anything containing spaces or an operator.
type ConstRenderer = Int -> Double -> (String, Bool)

-- | Print constants as plain numbers, bracketing negatives.
defaultConstRenderer :: ConstRenderer
defaultConstRenderer _ c = (showConstant c, c < 0)

-- | Render an expression with the minimum number of parentheses.
pretty :: Expr -> String
pretty = prettyPrec 0

-- | Render at a given surrounding precedence. Parentheses appear only when
-- the context binds more tightly than the node being rendered.
prettyPrec :: Int -> Expr -> String
prettyPrec prec expr = fst (render defaultConstRenderer prec 0 expr)

-- | Render with a custom constant renderer. See 'ConstRenderer'.
prettyWithConstants :: ConstRenderer -> Expr -> String
prettyWithConstants renderer expr = fst (render renderer 0 0 expr)

-- | The shared traversal.
--
-- Threads two things: the surrounding precedence (down) and the running
-- constant ordinal (left to right). The ordinal has to be threaded rather
-- than computed per-node, because it counts constants encountered so far
-- in pre-order — which is exactly the numbering
-- 'TypedGP.Expr.flatten' produces, so callers can match against it.
--
-- Precedences follow ordinary mathematical convention: @+@ and @-@ at 6,
-- @*@ and @/@ at 7, everything atomic above both.
render :: ConstRenderer -> Int -> Int -> Expr -> (String, Int)
render renderer prec ordinal expr = case expr of
  Const c ->
    let (text, needsBrackets) = renderer ordinal c
    in (parensIf (prec > 0 && needsBrackets) text, ordinal + 1)
  Var v   -> (v, ordinal)
  Add a b -> infixOp 6 " + " a b
  Sub a b -> infixOp 6 " - " a b
  Mul a b -> infixOp 7 " * " a b
  Div a b -> infixOp 7 " / " a b
  -- Binds tighter than multiplication and associates to the right, as in
  -- ordinary mathematical notation: @x ^ y ^ z@ means @x ^ (y ^ z)@, and
  -- @(x ^ y) ^ z@ is the form that needs brackets.
  -- Rendered as |a| ^ b, because that is the operator's definition.
  --
  -- 'TypedGP.Eval.protectedPow' means @|base| ** expo@ for every base (see
  -- @docs\/phase5-pow-semantics.md@), so @sin(x) ^ 1.1@ computes
  -- @|sin x| ^ 1.1@ — an ordinary rectified-sine shape wearing notation
  -- that looks like it should not be real-valued. Printing it as it is
  -- computed is the difference between a formula a reader can interpret
  -- and one they will discard as broken.
  --
  -- The condition is only that the base is not /already/ known
  -- non-negative, so the @abs@ appears exactly where it changes the
  -- meaning and nowhere else. It deliberately does __not__ also require a
  -- literal non-integer exponent: under the current semantics the
  -- magnitude is taken whatever the exponent is, so restricting the
  -- rendering to literal exponents would leave @(-0.6) ^ y@ printing
  -- without the absolute value while still computing with it — quietly
  -- reintroducing the exact problem this rendering exists to remove.
  Pow a b
    | provablyNonNegative a -> infixOpRight 8 " ^ " a b
    | otherwise             -> infixOpRight 8 " ^ " (Abs a) b
  Sin a   -> call "sin" a
  Cos a   -> call "cos" a
  Exp a   -> call "exp" a
  Log a   -> call "log" a
  Sqrt a  -> call "sqrt" a
  Tanh a  -> call "tanh" a
  Abs a   -> call "abs" a
  -- Spelled out rather than as the conventional single-letter Greek. The
  -- existing convention here is lowercase ASCII function names, and these
  -- strings also have to survive the checkpoint wire format, where an
  -- operator name is a bare token.
  Gamma a -> call "gamma" a
  Zeta a  -> call "zeta" a
  -- Rendered in the conventional form rather than as a function call,
  -- because the index name has to appear and no call syntax carries it.
  -- The bounds and body are rendered at precedence 0: the enclosing
  -- "sum(i=..." brackets already isolate them.
  Sum i lo hi body ->
    let (loText, o1) = render renderer 0 ordinal lo
        (hiText, o2) = render renderer 0 o1 hi
        (bodyText, o3) = render renderer 0 o2 body
    in ( "sum(" ++ i ++ "=" ++ loText ++ ".." ++ hiText ++ ", " ++ bodyText ++ ")"
       , o3
       )
 where
  -- All four infix operators are left-associative, so the right operand is
  -- rendered one precedence level higher: @a - b - c@ needs no brackets but
  -- @a - (b - c)@ does.
  infixOp :: Int -> String -> Expr -> Expr -> (String, Int)
  infixOp p sym a b =
    let (leftText, ordinal') = render renderer p ordinal a
        (rightText, ordinal'') = render renderer (p + 1) ordinal' b
    in (parensIf (prec > p) (leftText ++ sym ++ rightText), ordinal'')

  -- The mirror image of 'infixOp': the /left/ operand is rendered one
  -- level higher, so a nested power on the left gets brackets and one on
  -- the right does not.
  infixOpRight :: Int -> String -> Expr -> Expr -> (String, Int)
  infixOpRight p sym a b =
    let (leftText, ordinal') = render renderer (p + 1) ordinal a
        (rightText, ordinal'') = render renderer p ordinal' b
    in (parensIf (prec > p) (leftText ++ sym ++ rightText), ordinal'')

  -- A function call brings its own brackets, so the argument resets to 0.
  call :: String -> Expr -> (String, Int)
  call name a =
    let (argText, ordinal') = render renderer 0 ordinal a
    in (name ++ "(" ++ argText ++ ")", ordinal')

-- | Format a constant compactly: fixed notation with trailing zeros
-- removed for human-sized numbers, scientific notation at the extremes.
showConstant :: Double -> String
showConstant c
  | isNaN c                 = "NaN"
  | isInfinite c, c > 0     = "Infinity"
  | isInfinite c            = "-Infinity"
  | magnitude >= 1.0e6      = showEFloat (Just 3) c ""
  | magnitude > 0
  , magnitude < 1.0e-4      = showEFloat (Just 3) c ""
  | otherwise               = normaliseZero (trimTrailingZeros (showFFloat (Just 4) c ""))
  where
    magnitude = abs c

-- | Drop the noise from a fixed-point rendering: @"2.0000"@ becomes
-- @"2"@, @"0.5000"@ becomes @"0.5"@.
--
-- Implemented by stripping from the reversed string so it stays total —
-- no @last@/@init@ and no guard to get wrong. Only applied when a decimal
-- point is present, otherwise @"100"@ would be eaten down to @"1"@.
trimTrailingZeros :: String -> String
trimTrailingZeros str
  | '.' `elem` str = reverse (dropWhile (== '.') (dropWhile (== '0') (reverse str)))
  | otherwise      = str

-- | @-0.00001@ rounds to the string @"-0"@, which reads as a bug.
normaliseZero :: String -> String
normaliseZero "-0" = "0"
normaliseZero s    = s

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s
