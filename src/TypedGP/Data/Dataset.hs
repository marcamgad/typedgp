-- | Training data, and the plain-text format for getting it in and out.
--
-- Kept separate from "TypedGP.Fitness" so that scoring is agnostic to
-- where the numbers came from. Today that means a synthetic sampler and a
-- whitespace-delimited text loader; tomorrow it can mean a real file
-- format, and neither "TypedGP.Fitness" nor "TypedGP.Evolution" has to
-- notice.
--
-- The 'Dataset' constructor is not exported. Every value of this type
-- therefore satisfies the invariant that /every point binds exactly the
-- declared variables/, which is what lets 'TypedGP.Fitness' evaluate
-- against it without per-row checking.
module TypedGP.Data.Dataset
  ( -- * Types
    DataPoint (..)
  , Dataset
  , datasetVariables
  , datasetPoints
  , datasetSize

    -- * Construction
  , mkDataset
  , sampleDataset

    -- * Splitting and resampling
  , splitDataset
  , kFoldSplits
  , resampleDataset

    -- * Plain-text format
  , parseDataset
  , renderDataset

    -- * Text helpers
  , parseDouble
  , splitOn
  ) where

import Data.List (dropWhileEnd)

import TypedGP.Expr (VarName)
import TypedGP.Random (Seed, nextRange, pick, shuffle)

-- | One training example: an assignment to every input variable, and the
-- value the evolved program is supposed to produce.
--
-- 'dpInputs' is deliberately shaped as an association list so it can be
-- handed straight to 'TypedGP.Eval.eval' as an environment.
data DataPoint = DataPoint
  { dpInputs :: ![(VarName, Double)]
  , dpTarget :: !Double
  } deriving (Eq, Show)

-- | A validated collection of training examples over a fixed variable set.
data Dataset = Dataset
  { datasetVariables :: ![VarName]
    -- ^ Input column names, in the order they appear in the text format.
  , datasetPoints :: ![DataPoint]
    -- ^ The examples. Every one binds exactly 'datasetVariables'.
  } deriving (Eq, Show)

-- | Number of training examples.
datasetSize :: Dataset -> Int
datasetSize = length . datasetPoints

-- | Build a dataset from rows of @(inputs, target)@, checking that every
-- row has one value per declared variable.
mkDataset :: [VarName] -> [([Double], Double)] -> Either String Dataset
mkDataset vars rows
  | null vars = Left "a dataset needs at least one input variable"
  | otherwise = do
      points <- mapM toPoint (zip [1 :: Int ..] rows)
      Right Dataset { datasetVariables = vars, datasetPoints = points }
  where
    expected :: Int
    expected = length vars

    toPoint :: (Int, ([Double], Double)) -> Either String DataPoint
    toPoint (rowNo, (inputs, target))
      | length inputs /= expected = Left $
          "row " ++ show rowNo ++ ": expected " ++ show expected
            ++ " input value(s), got " ++ show (length inputs)
      | otherwise = Right DataPoint
          { dpInputs = zip vars inputs
          , dpTarget = target
          }

-- | Draw @n@ examples with each input sampled uniformly from a range, and
-- targets computed by a known function.
--
-- This is how the synthetic benchmark problems are built. It bypasses
-- 'mkDataset' because the invariant holds by construction: inputs are
-- generated one per declared variable.
sampleDataset
  :: [VarName]
  -> (Double, Double)
  -- ^ Inclusive-exclusive range each input is drawn from.
  -> Int
  -- ^ Number of examples. Negative values are treated as zero.
  -> ([(VarName, Double)] -> Double)
  -- ^ The ground-truth function. Compatible with 'TypedGP.Eval.Env'.
  -> Seed
  -> (Dataset, Seed)
sampleDataset vars (lo, hi) n target s0 =
  let (points, s1) = go (max 0 n) s0 []
  in (Dataset { datasetVariables = vars, datasetPoints = points }, s1)
  where
    go :: Int -> Seed -> [DataPoint] -> ([DataPoint], Seed)
    go 0 st acc = (reverse acc, st)
    go k st acc =
      let (inputs, st1) = drawRow vars st
          point = DataPoint { dpInputs = inputs, dpTarget = target inputs }
      in go (k - 1) st1 (point : acc)

    drawRow :: [VarName] -> Seed -> ([(VarName, Double)], Seed)
    drawRow [] st = ([], st)
    drawRow (v : vs) st =
      let (value, st1) = nextRange lo hi st
          (rest, st2) = drawRow vs st1
      in ((v, value) : rest, st2)

-- | Shuffle, then hold out the given fraction. Returns
-- @(training, holdout, nextSeed)@.
--
-- The shuffle is not optional. Real datasets arrive sorted by time, by
-- class, or by whatever the collection process imposed, and slicing such a
-- file in place would put a systematically different population in the
-- holdout set — which reports as a generalisation failure when it is
-- really a sampling artefact.
--
-- The fraction is clamped to @[0, 1]@ rather than rejected, so this stays
-- total; 'TypedGP.Config.validateConfig' is where an unreasonable holdout
-- is refused with an explanation.
splitDataset :: Double -> Seed -> Dataset -> (Dataset, Dataset, Seed)
splitDataset fraction s0 ds =
  let points = datasetPoints ds
      total = length points
      (shuffled, s1) = shuffle points s0
      wanted = round (clampFraction fraction * fromIntegral total) :: Int
      holdoutSize = max 0 (min total wanted)
      (holdout, training) = splitAt holdoutSize shuffled
  in ( ds { datasetPoints = training }
     , ds { datasetPoints = holdout }
     , s1
     )

-- | Shuffle once, then partition into @k@ folds, returning one
-- @(training, validation)@ pair per fold.
--
-- Shuffling once up front rather than per fold is what makes the folds
-- disjoint: every example appears in exactly one validation set, which is
-- the property that makes the spread of scores across folds interpretable.
--
-- @k@ below 2, or above the number of examples, is clamped into range.
kFoldSplits :: Int -> Seed -> Dataset -> ([(Dataset, Dataset)], Seed)
kFoldSplits requestedK s0 ds
  | total < 2 = ([], s0)
  | otherwise = (map foldAt [0 .. k - 1], s1)
  where
    points :: [DataPoint]
    points = datasetPoints ds

    total :: Int
    total = length points

    k :: Int
    k = max 2 (min total requestedK)

    (shuffledPoints, s1) = shuffle points s0

    -- Fold i takes every i-th example rather than a contiguous block.
    -- After a shuffle the two are equivalent in distribution, but this
    -- also gives evenly sized folds when total does not divide by k,
    -- with no remainder-handling special case to get wrong.
    foldAt :: Int -> (Dataset, Dataset)
    foldAt i =
      let tagged = zip [0 :: Int ..] shuffledPoints
          validation = [p | (j, p) <- tagged, j `mod` k == i]
          training = [p | (j, p) <- tagged, j `mod` k /= i]
      in (ds { datasetPoints = training }, ds { datasetPoints = validation })

-- | Draw @n@ examples with replacement, where @n@ is the dataset's own
-- size. The bootstrap resample used by "TypedGP.Uncertainty".
--
-- With replacement is the whole point: roughly 63% of the original
-- examples appear in any given resample, some more than once, and it is
-- that variation between resamples which the bootstrap turns into a
-- sampling distribution.
resampleDataset :: Seed -> Dataset -> (Dataset, Seed)
resampleDataset s0 ds =
  let points = datasetPoints ds
      (drawn, s1) = go (length points) points s0 []
  in (ds { datasetPoints = drawn }, s1)
  where
    go :: Int -> [DataPoint] -> Seed -> [DataPoint] -> ([DataPoint], Seed)
    go 0 _ st acc = (acc, st)
    go k source st acc = case pick source st of
      Just (point, st') -> go (k - 1) source st' (point : acc)
      -- Only when the dataset is empty, in which case there is nothing to
      -- draw and looping further would not help.
      Nothing -> (acc, st)

clampFraction :: Double -> Double
clampFraction f
  | isNaN f   = 0.0
  | f < 0.0   = 0.0
  | f > 1.0   = 1.0
  | otherwise = f

-- | Parse the plain-text format.
--
-- One example per line, values separated by whitespace and\/or commas, the
-- last value on each line being the target:
--
-- > # x y target
-- > 1.0  0.0   2.0
-- > -2.5, 1.0, -4.158
--
-- Blank lines are skipped and @#@ starts a comment that runs to end of
-- line. Errors name the offending line so a bad file is diagnosable
-- without a debugger.
parseDataset :: [VarName] -> String -> Either String Dataset
parseDataset vars input = do
    rows <- mapM parseLine significant
    mkDataset vars rows
  where
    significant :: [(Int, String)]
    significant =
      [ (lineNo, stripped)
      | (lineNo, raw) <- zip [1 :: Int ..] (lines (dropByteOrderMark input))
      , let stripped = takeWhile (/= '#') raw
      , any (not . isSpaceChar) stripped
      ]

    expected :: Int
    expected = length vars + 1

    parseLine :: (Int, String) -> Either String ([Double], Double)
    parseLine (lineNo, text) =
      let tokens = words (map commaToSpace text)
      in if length tokens /= expected
           then Left $
             "line " ++ show lineNo ++ ": expected " ++ show expected
               ++ " value(s), got " ++ show (length tokens)
           else do
             values <- mapM (readValue lineNo) tokens
             -- splitAt is total, and the length check above guarantees
             -- exactly one trailing target value.
             case splitAt (expected - 1) values of
               (inputs, [target]) -> Right (inputs, target)
               _ -> Left ("line " ++ show lineNo ++ ": malformed row")

    readValue :: Int -> String -> Either String Double
    readValue lineNo token = case parseDouble token of
      Just v  -> Right v
      Nothing -> Left ("line " ++ show lineNo ++ ": not a number: " ++ token)

    commaToSpace :: Char -> Char
    commaToSpace ',' = ' '
    commaToSpace c   = c

-- | Render a dataset back to the format 'parseDataset' accepts, with a
-- header comment naming the columns. Round-trips.
renderDataset :: Dataset -> String
renderDataset ds = unlines (header : map renderRow (datasetPoints ds))
  where
    header :: String
    header = "# " ++ unwords (datasetVariables ds ++ ["target"])

    -- 'show' rather than a prettier formatter: this output is meant to be
    -- re-read exactly, so full precision beats readability.
    renderRow :: DataPoint -> String
    renderRow p =
      unwords (map (show . snd) (dpInputs p) ++ [show (dpTarget p)])

-- | Parse a decimal number, a little more permissively than 'reads'.
--
-- Accepts a leading @+@ and a bare leading @.@ (as in @.5@), neither of
-- which is a valid Haskell literal, because both are common in
-- hand-written data files. Everything else defers to 'reads', which
-- already handles exponents.
parseDouble :: String -> Maybe Double
parseDouble raw =
  let text = dropWhile isSpaceChar (dropWhileEnd isSpaceChar raw)
      (sign, unsigned) = case text of
        ('-' : rest) -> (-1.0, rest)
        ('+' : rest) -> (1.0, rest)
        _            -> (1.0, text)
      normalised = case unsigned of
        ('.' : _) -> '0' : unsigned
        _         -> unsigned
  in case reads normalised :: [(Double, String)] of
       [(value, "")] -> Just (sign * value)
       _             -> Nothing

-- | Split on a delimiter, keeping empty fields. @splitOn ',' "a,,b"@ is
-- @["a", "", "b"]@.
splitOn :: Char -> String -> [String]
splitOn delimiter text = case break (== delimiter) text of
  (field, [])           -> [field]
  (field, _ : leftover) -> field : splitOn delimiter leftover

-- | Drop a leading UTF-8 byte order mark, if present.
--
-- Not a nicety. Excel's CSV export, Windows Notepad, and PowerShell's
-- @Set-Content -Encoding utf8@ all prepend U+FEFF, so a large fraction of
-- real data files start with one. GHC decodes it as a single character
-- rather than as bytes, and without this it lands on the first line and is
-- read as a data value — producing "line 1: expected 2 value(s), got 1",
-- which is a genuinely baffling message for a file that looks correct in
-- every editor.
--
-- Only stripped from the very start of the input; a U+FEFF anywhere else
-- is real content and stays.
dropByteOrderMark :: String -> String
dropByteOrderMark ('\xFEFF' : rest) = rest
dropByteOrderMark text              = text

-- | ASCII whitespace test. Avoids importing "Data.Char" for one predicate
-- and keeps the accepted grammar explicit: these are exactly the
-- characters the text format treats as separators.
isSpaceChar :: Char -> Bool
isSpaceChar c = c `elem` " \t\r\n\f\v"
