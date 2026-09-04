-- | Saving and restoring a run in progress.
--
-- Once a run takes long enough that losing it to a crash, a full disk or a
-- laptop lid is a real cost rather than a hypothetical one, the ability to
-- resume is worth more than almost any tuning improvement. This module is
-- deliberately boring so that it can be relied on.
--
-- == Format
--
-- Plain text, same spirit as the dataset format: line oriented, readable
-- and diffable. No binary encoding and therefore no serialisation
-- dependency.
--
-- Unlike the dataset format there are no @#@ comments, because @#@ is the
-- constant sigil below and stripping comments would truncate every
-- expression containing a literal. Blank lines are ignored. These files
-- are written by the program, not by hand, so the loss is nil.
--
-- > typedgp-checkpoint 1
-- > generation 12
-- > seed 17364915523049283
-- > variables x y
-- > individuals 3
-- > (+ (* #2.0 $x) (sin $y))
-- > (- $x #1.5)
-- > $y
--
-- Expressions use a prefix S-expression grammar with sigils on the leaves:
-- @#@ introduces a constant and @$@ a variable. The sigils are not
-- decoration — variable names come from user data and may look exactly
-- like numbers or operators, so without them @(+ 2 x)@ would be ambiguous
-- for a dataset whose columns are named @2@ and @x@.
--
-- == What is deliberately not saved
--
-- __Fitness.__ It is recomputed on load, which costs one evaluation pass
-- and buys two things: the file stays valid if the dataset or error metric
-- changed between runs, and there is no way for a stale score to be
-- restored alongside the tree it no longer describes. Silent
-- score/structure mismatch is exactly the kind of bug a checkpoint format
-- should refuse to make possible.
--
-- __The entire 'TypedGP.Config.Config'.__ No selection strategy, no error
-- metric, no hyperparameter. A resumed run takes all of that from the
-- command line.
--
-- That second one is a genuine footgun and is called out here because
-- nothing in the format can catch it: __resuming with different flags
-- silently continues a different search__. Resume a @--selection lexicase@
-- run without repeating the flag and it quietly becomes a tournament run
-- from generation N, with no error and no warning, because the population
-- of expressions is equally valid under either strategy.
--
-- Variable names /are/ stored and /are/ checked on resume, so the one
-- mismatch that would produce outright nonsense — scoring expressions
-- against the wrong columns — is caught. Everything else is the caller's
-- responsibility. The pragmatic advice, which the CLI help repeats: resume
-- with the same command line you started with, changing only @--resume@.
--
-- Storing the config instead was considered and rejected: it would make a
-- checkpoint override flags the user just typed, which is a worse
-- surprise than the one above, and it would freeze hyperparameters that
-- are often deliberately changed on resume (a longer @--gens@ being the
-- obvious case).
module TypedGP.Checkpoint
  ( Checkpoint (..)
  , renderCheckpoint
  , parseCheckpoint

    -- * Expression wire format
  , renderExprS
  , parseExprS
  ) where

import Data.Word (Word64)

import TypedGP.Data.Dataset (parseDouble)
import TypedGP.Expr (Expr (..), VarName)
import TypedGP.Random (Seed, rawSeed, seedWord)

-- | A run frozen between generations.
data Checkpoint = Checkpoint
  { ckGeneration :: !Int
  , ckSeed :: !Seed
  , ckVariables :: ![VarName]
    -- ^ Recorded so a resume against a mismatched dataset is caught rather
    -- than producing nonsense.
  , ckExpressions :: ![Expr]
    -- ^ The population's programs, in order. Fitness is recomputed on
    -- load; see the module notes.
  } deriving (Eq, Show)

formatVersion :: Int
formatVersion = 2

-- Rendering -----------------------------------------------------------------

renderCheckpoint :: Checkpoint -> String
renderCheckpoint checkpoint = unlines $
  [ "typedgp-checkpoint " ++ show formatVersion
  , "generation " ++ show (ckGeneration checkpoint)
  , "seed " ++ show (seedWord (ckSeed checkpoint))
  , "variables " ++ unwords (ckVariables checkpoint)
  , "individuals " ++ show (length (ckExpressions checkpoint))
  ]
    ++ map renderExprS (ckExpressions checkpoint)

-- | Render an expression in the prefix wire format.
--
-- Constants go through 'show' rather than the pretty-printer: this has to
-- round-trip exactly, and the pretty-printer deliberately rounds for
-- readability.
renderExprS :: Expr -> String
renderExprS expr = case expr of
  Const c -> '#' : show c
  Var v   -> '$' : v
  Add a b -> node "+" [a, b]
  Sub a b -> node "-" [a, b]
  Mul a b -> node "*" [a, b]
  Div a b -> node "/" [a, b]
  Pow a b -> node "^" [a, b]
  Sin a   -> node "sin" [a]
  Cos a   -> node "cos" [a]
  Exp a   -> node "exp" [a]
  Log a   -> node "log" [a]
  Sqrt a  -> node "sqrt" [a]
  Tanh a  -> node "tanh" [a]
  Abs a   -> node "abs" [a]
  Gamma a -> node "gamma" [a]
  Zeta a  -> node "zeta" [a]
  -- The index is written as an ordinary '$name' leaf, so the existing
  -- tokeniser and its escaping rules cover it and no new atom kind is
  -- introduced. Reading it back checks that the slot really is a Var.
  Sum i lo hi body -> node "sum" [Var i, lo, hi, body]
  where
    node :: String -> [Expr] -> String
    node name children =
      "(" ++ unwords (name : map renderExprS children) ++ ")"

-- Parsing -------------------------------------------------------------------

parseCheckpoint :: String -> Either String Checkpoint
parseCheckpoint input = do
  rows <- Right (significantLines input)
  case rows of
    [] -> Left "checkpoint is empty"
    (header : rest) -> do
      checkHeader header
      (generation, rest1) <- takeField "generation" rest
      generationValue <- readIntField "generation" generation
      (seedText, rest2) <- takeField "seed" rest1
      seedValue <- readWordField "seed" seedText
      (variablesText, rest3) <- takeField "variables" rest2
      (countText, rest4) <- takeField "individuals" rest3
      expectedCount <- readIntField "individuals" countText
      expressions <- mapM parseExprS rest4
      if length expressions /= expectedCount
        then Left $
          "checkpoint claims " ++ show expectedCount ++ " individual(s) but "
            ++ "contains " ++ show (length expressions)
        else Right Checkpoint
          { ckGeneration = generationValue
          , ckSeed = rawSeed seedValue
          , ckVariables = words variablesText
          , ckExpressions = expressions
          }

-- | Drop blank lines. Emphatically /not/ comment-stripping: see the module
-- notes on why @#@ cannot introduce a comment here.
significantLines :: String -> [String]
significantLines input =
  [ trimmed
  | raw <- lines input
  , let trimmed = trim raw
  , not (null trimmed)
  ]

checkHeader :: String -> Either String ()
checkHeader header = case words header of
  ["typedgp-checkpoint", version] -> case reads version :: [(Int, String)] of
    [(v, "")]
      -- Older files are still readable: version 1 predates the binder
      -- constructors, so a v1 checkpoint is a strict subset of what a v2
      -- reader accepts. A NEWER file is rejected, because this build
      -- cannot know what it contains.
      | v >= 1 && v <= formatVersion -> Right ()
      | otherwise -> Left $
          "checkpoint format version " ++ show v ++ " is not supported "
            ++ "(this build writes version " ++ show formatVersion ++ ")"
    _ -> Left ("unreadable checkpoint version: " ++ version)
  _ -> Left "not a typedgp checkpoint (bad first line)"

takeField :: String -> [String] -> Either String (String, [String])
takeField name rows = case rows of
  [] -> Left ("checkpoint is missing the '" ++ name ++ "' field")
  (row : rest) -> case span (/= ' ') row of
    (key, value)
      | key == name -> Right (trim value, rest)
      | otherwise -> Left $
          "expected '" ++ name ++ "' in the checkpoint header, found '"
            ++ key ++ "'"

readIntField :: String -> String -> Either String Int
readIntField name text = case reads text :: [(Int, String)] of
  [(v, "")] -> Right v
  _         -> Left ("checkpoint field '" ++ name ++ "' is not an integer: " ++ text)

readWordField :: String -> String -> Either String Word64
readWordField name text = case reads text :: [(Word64, String)] of
  [(v, "")] -> Right v
  _         -> Left ("checkpoint field '" ++ name ++ "' is not a number: " ++ text)

-- | Parse one expression in the prefix wire format.
parseExprS :: String -> Either String Expr
parseExprS text = do
  (expr, leftover) <- parseTokens (tokenize text)
  case leftover of
    [] -> Right expr
    _  -> Left ("trailing input after expression: " ++ text)

data Token
  = TOpen
  | TClose
  | TAtom String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : rest)
  | c == '('  = TOpen : tokenize rest
  | c == ')'  = TClose : tokenize rest
  | isSpace c = tokenize rest
  | otherwise =
      let (atom, remaining) = span (\x -> x /= '(' && x /= ')' && not (isSpace x)) (c : rest)
      in TAtom atom : tokenize remaining

parseTokens :: [Token] -> Either String (Expr, [Token])
parseTokens tokens = case tokens of
  [] -> Left "unexpected end of expression"
  (TClose : _) -> Left "unexpected ')'"
  (TAtom atom : rest) -> do
    leaf <- parseLeaf atom
    Right (leaf, rest)
  (TOpen : TAtom op : rest) -> case op of
    "+"   -> binary Add rest
    "-"   -> binary Sub rest
    "*"   -> binary Mul rest
    "/"   -> binary Div rest
    "^"   -> binary Pow rest
    -- This table is NOT compiler-checked: it dispatches on a string, so a
    -- new operator compiles fine here and only fails at run time, when a
    -- checkpoint containing it cannot be reloaded. Adding a constructor
    -- means adding a line here, and CheckpointSpec's round-trip over every
    -- registered operator is what actually catches an omission.
    "sin"   -> unary Sin rest
    "cos"   -> unary Cos rest
    "exp"   -> unary Exp rest
    "log"   -> unary Log rest
    "sqrt"  -> unary Sqrt rest
    "tanh"  -> unary Tanh rest
    "abs"   -> unary Abs rest
    "gamma" -> unary Gamma rest
    "zeta"  -> unary Zeta rest
    -- Not compiler-checked: this table matches on operator-name strings,
    -- which is exactly the blind spot invariant 7 exists for. A
    -- constructor added to the writer without an entry here produces
    -- checkpoints that write cleanly and fail to load, so CheckpointSpec
    -- carries a round-trip case for every operator.
    "sum"   -> binder rest
    _       -> Left ("unknown operator: " ++ op)
  (TOpen : _) -> Left "expected an operator after '('"
  where
    unary :: (Expr -> Expr) -> [Token] -> Either String (Expr, [Token])
    unary build rest = do
      (a, rest1) <- parseTokens rest
      rest2 <- expectClose rest1
      Right (build a, rest2)

    -- A binder is four sub-expressions where the first must be a bare
    -- variable atom. Anything else is a malformed checkpoint rather than
    -- something to coerce, so it is rejected with a message naming what
    -- was found.
    binder :: [Token] -> Either String (Expr, [Token])
    binder rest = do
      (nameExpr, rest1) <- parseTokens rest
      (lo, rest2) <- parseTokens rest1
      (hi, rest3) <- parseTokens rest2
      (body, rest4) <- parseTokens rest3
      rest5 <- expectClose rest4
      case nameExpr of
        Var i -> Right (Sum i lo hi body, rest5)
        other -> Left ("sum index must be a variable, found: " ++ show other)

    binary :: (Expr -> Expr -> Expr) -> [Token] -> Either String (Expr, [Token])
    binary build rest = do
      (a, rest1) <- parseTokens rest
      (b, rest2) <- parseTokens rest1
      rest3 <- expectClose rest2
      Right (build a b, rest3)

    expectClose :: [Token] -> Either String [Token]
    expectClose (TClose : rest) = Right rest
    expectClose _               = Left "expected ')'"

parseLeaf :: String -> Either String Expr
parseLeaf atom = case atom of
  ('#' : number) -> case parseDouble number of
    Just v  -> Right (Const v)
    Nothing -> Left ("unreadable constant: " ++ atom)
  ('$' : name)
    | null name -> Left "empty variable name"
    | otherwise -> Right (Var name)
  _ -> Left ("expected a leaf beginning with '#' or '$', found: " ++ atom)

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\r\n\f\v"

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
