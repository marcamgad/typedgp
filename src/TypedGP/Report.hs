-- | A run's result as data, plus renderers for humans and for machines.
--
-- Separating the /result/ from its /rendering/ is what makes the run
-- scriptable. Text output is for reading; JSON output is for diffing two
-- runs, feeding a notebook, or asserting on in a test without parsing
-- prose.
--
-- The JSON encoder is written by hand. A typeclass-driven library would be
-- a dependency, and the whole encoder is about eighty lines — most of
-- which is escaping rules that a library would also have to implement.
--
-- One rule is worth stating because it is the usual source of invalid
-- output: JSON has no representation for @NaN@ or @Infinity@. Both are
-- emitted as @null@ rather than as bare tokens that no parser accepts.
-- "TypedGP.Eval" makes those values rare, not impossible — an empty
-- dataset still yields a sentinel error — so the encoder handles them
-- instead of assuming they cannot arise.
module TypedGP.Report
  ( -- * The report
    Report (..)
  , HistoryPoint (..)
  , FrontEntry (..)
  , buildReport

    -- * Output selection
  , OutputFormat (..)
  , parseOutputFormat

    -- * Rendering
  , renderReportText
  , renderReportJson

    -- * JSON
  , JsonValue (..)
  , encodeJson
  , escapeJsonString
  ) where

import Data.List (intercalate)
import Data.Word (Word64)
import Numeric (showFFloat, showHex)

import TypedGP.Checkpoint (renderExprS)
import TypedGP.Config (Config (..))
import TypedGP.Evolution
  ( EvolutionResult (..)
  , GenStats (..)
  , gsBestError
  , gsBestFitness
  , gsBestSize
  )
import TypedGP.Expr (Expr, countNodes)
import TypedGP.Fitness (Fitness (..))
import TypedGP.Population (Individual (..))
import TypedGP.Pretty (pretty)
import TypedGP.Uncertainty (BootstrapResult (..), ConstantEstimate (..), annotateFormula)

-- | How the CLI should print a result.
data OutputFormat
  = TextFormat
  | JsonFormat
  deriving (Eq, Show)

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat s = case s of
  "text" -> Just TextFormat
  "json" -> Just JsonFormat
  _      -> Nothing

-- | One generation, flattened for reporting.
data HistoryPoint = HistoryPoint
  { hpGeneration :: !Int
  , hpBestFitness :: !Double
  , hpBestError :: !Double
  , hpHoldoutError :: !(Maybe Double)
  , hpMeanFitness :: !Double
  , hpBestSize :: !Int
  , hpMeanSize :: !Double
  , hpFormula :: !String
  } deriving (Eq, Show)

-- | One member of the Pareto front.
data FrontEntry = FrontEntry
  { feSize :: !Int
  , feError :: !Double
  , feFormula :: !String
  } deriving (Eq, Show)

-- | Everything worth knowing about a finished run.
data Report = Report
  { repFormula :: !String
    -- ^ The simplified best formula, pretty-printed.
  , repFormulaSExpr :: !String
    -- ^ The same expression in the machine-readable wire format, so a
    -- consumer of the JSON can reload it exactly rather than re-parsing
    -- the human rendering.
  , repExpr :: !Expr
  , repSize :: !Int
  , repTrainingError :: !Double
  , repSizePenalty :: !Double
  , repTotalFitness :: !Double
  , repHoldoutError :: !(Maybe Double)
  , repTrainingSize :: !Int
  , repHoldoutSize :: !Int
  , repMetric :: !String
  , repGenerations :: !Int
  , repTargetReached :: !Bool
  , repTargetFitness :: !Double
  , repElapsedSeconds :: !Double
  , repSeed :: !Word64
  , repWarnings :: ![String]
  , repHistory :: ![HistoryPoint]
  , repParetoFront :: ![FrontEntry]
  , repConstants :: ![ConstantEstimate]
  , repAnnotatedFormula :: !(Maybe String)
    -- ^ The formula with bootstrap intervals substituted for its
    -- constants, when a bootstrap ran.
  } deriving (Eq, Show)

buildReport
  :: Config
  -> Word64
  -- ^ The seed the run started from.
  -> Double
  -- ^ Elapsed wall-clock seconds.
  -> Maybe BootstrapResult
  -> EvolutionResult
  -> Report
buildReport cfg seed elapsed bootstrap result = Report
  { repFormula = pretty simplified
  , repFormulaSExpr = renderExprS simplified
  , repExpr = simplified
  , repSize = countNodes simplified
  , repTrainingError = fitError fitness
  , repSizePenalty = fitPenalty fitness
  , repTotalFitness = fitTotal fitness
  , repHoldoutError = erHoldoutError result
  , repTrainingSize = erTrainingSize result
  , repHoldoutSize = erHoldoutSize result
  , repMetric = show (cfgErrorMetric cfg)
  , repGenerations = erGenerationsRun result
  , repTargetReached = erTargetReached result
  , repTargetFitness = cfgTargetFitness cfg
  , repElapsedSeconds = elapsed
  , repSeed = seed
  , repWarnings = erWarnings result
  , repHistory = map toHistoryPoint (erHistory result)
  , repParetoFront = map toFrontEntry (erParetoFront result)
  , repConstants = maybe [] brEstimates bootstrap
  , repAnnotatedFormula = fmap annotateFormula bootstrap
  }
  where
    simplified :: Expr
    simplified = erBestSimplified result

    fitness :: Fitness
    fitness = indFitness (erBest result)

    toHistoryPoint :: GenStats -> HistoryPoint
    toHistoryPoint stats = HistoryPoint
      { hpGeneration = gsGeneration stats
      , hpBestFitness = gsBestFitness stats
      , hpBestError = gsBestError stats
      , hpHoldoutError = gsHoldoutError stats
      , hpMeanFitness = gsMeanFitness stats
      , hpBestSize = gsBestSize stats
      , hpMeanSize = gsMeanSize stats
      , hpFormula = pretty (gsBestSimplified stats)
      }

    toFrontEntry :: Individual -> FrontEntry
    toFrontEntry ind = FrontEntry
      { feSize = countNodes (indExpr ind)
      , feError = fitError (indFitness ind)
      , feFormula = pretty (indExpr ind)
      }

-- Text rendering ------------------------------------------------------------

-- | Human-readable rendering, one line per element.
renderReportText :: Report -> [String]
renderReportText report =
  warningBlock
    ++ [ replicate 72 '-'
       , "Best formula : " ++ repFormula report
       , "Size         : " ++ show (repSize report) ++ " nodes"
       , "Metric       : " ++ repMetric report
       , "Training err : " ++ fixed 6 (repTrainingError report)
           ++ "   (" ++ show (repTrainingSize report) ++ " examples)"
       ]
    ++ holdoutBlock
    ++ [ "Total fitness: " ++ fixed 6 (repTotalFitness report)
           ++ "   (error + size penalty " ++ fixed 6 (repSizePenalty report) ++ ")"
       , "Generations  : " ++ show (repGenerations report)
       , "Elapsed      : " ++ fixed 2 (repElapsedSeconds report) ++ "s"
       , "Seed         : " ++ show (repSeed report)
       , "Verdict      : " ++ verdict
       ]
    ++ annotatedBlock
    ++ constantsBlock
    ++ frontBlock
  where
    verdict :: String
    verdict
      | repTargetReached report =
          "converged (at or below target " ++ fixed 4 (repTargetFitness report) ++ ")"
      | otherwise =
          "did not reach target " ++ fixed 4 (repTargetFitness report)

    warningBlock :: [String]
    warningBlock
      | null (repWarnings report) = []
      | otherwise =
          [""]
            ++ [replicate 72 '!']
            ++ concatMap renderWarning (zip [1 :: Int ..] (repWarnings report))
            ++ [replicate 72 '!']

    renderWarning :: (Int, String) -> [String]
    renderWarning (n, message) =
      map (indentContinuation n) (zip [0 :: Int ..] (wrapText 66 message))

    indentContinuation :: Int -> (Int, String) -> String
    indentContinuation n (0, line) = "WARNING " ++ show n ++ ": " ++ line
    indentContinuation _ (_, line) = "           " ++ line

    -- The holdout comparison is the single most decision-relevant thing in
    -- the report, so it is stated as a ratio and interpreted in words
    -- rather than left as two numbers for the reader to divide.
    holdoutBlock :: [String]
    holdoutBlock = case repHoldoutError report of
      Nothing -> ["Holdout err  : not measured (no --holdout split)"]
      Just holdout ->
        [ "Holdout err  : " ++ fixed 6 holdout
            ++ "   (" ++ show (repHoldoutSize report) ++ " examples held out)"
        , "Generalises  : " ++ generalisationVerdict (repTrainingError report) holdout
        ]

    annotatedBlock :: [String]
    annotatedBlock = case repAnnotatedFormula report of
      Nothing -> []
      Just annotated ->
        ["", "With bootstrap standard errors:", "  " ++ annotated]

    constantsBlock :: [String]
    constantsBlock
      | null (repConstants report) = []
      | otherwise =
          ["", "Constant estimates (95% bootstrap percentile interval):"]
            ++ map renderConstant (repConstants report)

    renderConstant :: ConstantEstimate -> String
    renderConstant estimate =
      "  #" ++ show (ceOrdinal estimate)
        ++ "  " ++ padLeft 12 (fixed 4 (cePoint estimate))
        ++ "  ± " ++ padLeft 10 (fixed 4 (ceStdError estimate))
        ++ "   [" ++ fixed 4 (ceLower estimate)
        ++ ", " ++ fixed 4 (ceUpper estimate) ++ "]"

    frontBlock :: [String]
    frontBlock
      | length (repParetoFront report) < 2 = []
      | otherwise =
          ["", "Pareto front (accuracy against simplicity):"]
            ++ map renderFront (repParetoFront report)

    renderFront :: FrontEntry -> String
    renderFront entry =
      "  " ++ padLeft 4 (show (feSize entry)) ++ " nodes  err "
        ++ padLeft 10 (fixed 6 (feError entry)) ++ "   " ++ feFormula entry

-- | Put a plain-language interpretation on the training/holdout gap.
--
-- The thresholds are conventional rules of thumb, not theory, and are
-- described as such. The point is that a reader who does not already know
-- what ratio should worry them still gets told.
generalisationVerdict :: Double -> Double -> String
generalisationVerdict training holdout
  | holdout <= training * 1.5 + tiny = "holdout error is close to training error - a good sign"
  | holdout <= training * 3.0 + tiny = "holdout error is noticeably worse - treat with caution"
  | otherwise =
      "holdout error is far worse than training error - this formula "
        ++ "very likely fits noise, not signal"
  where
    -- Keeps the ratios meaningful when both errors are near zero, where a
    -- pure ratio would be dominated by floating-point dust.
    tiny :: Double
    tiny = 1.0e-9

-- JSON rendering ------------------------------------------------------------

renderReportJson :: Report -> String
renderReportJson = encodeJson . toJson

toJson :: Report -> JsonValue
toJson report = JObj
  [ ("formula", JStr (repFormula report))
  , ("formula_sexpr", JStr (repFormulaSExpr report))
  , ("size", JInt (repSize report))
  , ("metric", JStr (repMetric report))
  , ("training_error", JNum (repTrainingError report))
  , ("size_penalty", JNum (repSizePenalty report))
  , ("total_fitness", JNum (repTotalFitness report))
  , ("holdout_error", maybe JNull JNum (repHoldoutError report))
  , ("training_examples", JInt (repTrainingSize report))
  , ("holdout_examples", JInt (repHoldoutSize report))
  , ("generations", JInt (repGenerations report))
  , ("target_fitness", JNum (repTargetFitness report))
  , ("target_reached", JBool (repTargetReached report))
  , ("elapsed_seconds", JNum (repElapsedSeconds report))
  , ("seed", JNum (fromIntegral (repSeed report)))
  , ("warnings", JArr (map JStr (repWarnings report)))
  , ("annotated_formula", maybe JNull JStr (repAnnotatedFormula report))
  , ("constants", JArr (map constantJson (repConstants report)))
  , ("pareto_front", JArr (map frontJson (repParetoFront report)))
  , ("history", JArr (map historyJson (repHistory report)))
  ]
  where
    constantJson :: ConstantEstimate -> JsonValue
    constantJson estimate = JObj
      [ ("ordinal", JInt (ceOrdinal estimate))
      , ("point", JNum (cePoint estimate))
      , ("bootstrap_mean", JNum (ceMean estimate))
      , ("std_error", JNum (ceStdError estimate))
      , ("ci_lower", JNum (ceLower estimate))
      , ("ci_upper", JNum (ceUpper estimate))
      ]

    frontJson :: FrontEntry -> JsonValue
    frontJson entry = JObj
      [ ("size", JInt (feSize entry))
      , ("error", JNum (feError entry))
      , ("formula", JStr (feFormula entry))
      ]

    historyJson :: HistoryPoint -> JsonValue
    historyJson point = JObj
      [ ("generation", JInt (hpGeneration point))
      , ("best_fitness", JNum (hpBestFitness point))
      , ("best_error", JNum (hpBestError point))
      , ("holdout_error", maybe JNull JNum (hpHoldoutError point))
      , ("mean_fitness", JNum (hpMeanFitness point))
      , ("best_size", JInt (hpBestSize point))
      , ("mean_size", JNum (hpMeanSize point))
      , ("formula", JStr (hpFormula point))
      ]

-- | A JSON document.
data JsonValue
  = JNull
  | JBool Bool
  | JInt Int
  | JNum Double
  | JStr String
  | JArr [JsonValue]
  | JObj [(String, JsonValue)]
  deriving (Eq, Show)

-- | Encode with two-space indentation.
--
-- Indented rather than compact because these files get read by people and
-- diffed between runs at least as often as they get parsed.
encodeJson :: JsonValue -> String
encodeJson = go 0
  where
    go :: Int -> JsonValue -> String
    go _ JNull = "null"
    go _ (JBool True) = "true"
    go _ (JBool False) = "false"
    go _ (JInt n) = show n
    go _ (JNum d)
      -- JSON cannot express either, and emitting the bare Haskell spelling
      -- would produce a document no parser accepts.
      | isNaN d || isInfinite d = "null"
      | otherwise = show d
    go _ (JStr s) = escapeJsonString s
    go _ (JArr []) = "[]"
    go depth (JArr items) =
      "[\n"
        ++ intercalate ",\n" [indent (depth + 1) ++ go (depth + 1) v | v <- items]
        ++ "\n" ++ indent depth ++ "]"
    go _ (JObj []) = "{}"
    go depth (JObj fields) =
      "{\n"
        ++ intercalate ",\n"
             [ indent (depth + 1) ++ escapeJsonString k ++ ": " ++ go (depth + 1) v
             | (k, v) <- fields
             ]
        ++ "\n" ++ indent depth ++ "}"

    indent :: Int -> String
    indent depth = replicate (depth * 2) ' '

-- | Quote and escape a string as a JSON literal.
--
-- Control characters below U+0020 must be escaped or the document is
-- invalid; everything else is passed through as UTF-8, which JSON permits
-- and which keeps formulas readable in the output.
escapeJsonString :: String -> String
escapeJsonString s = '"' : concatMap escapeChar s ++ "\""
  where
    escapeChar :: Char -> String
    escapeChar c = case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _ | c < ' '   -> "\\u" ++ pad4 (showHex (fromEnum c) "")
        | otherwise -> [c]

    pad4 :: String -> String
    pad4 h = replicate (max 0 (4 - length h)) '0' ++ h

-- Shared formatting helpers -------------------------------------------------

fixed :: Int -> Double -> String
fixed places value
  | isNaN value = "NaN"
  | isInfinite value = if value > 0 then "Infinity" else "-Infinity"
  | otherwise = showFFloat (Just places) value ""

padLeft :: Int -> String -> String
padLeft width text = replicate (max 0 (width - length text)) ' ' ++ text

-- | Break a message into lines of at most @width@ characters, on word
-- boundaries. Warnings are long prose and unwrapped they are unreadable.
wrapText :: Int -> String -> [String]
wrapText width message = go (words message) []
  where
    go :: [String] -> [String] -> [String]
    go [] [] = []
    go [] current = [unwords (reverse current)]
    go (w : ws) current
      | null current = go ws [w]
      | length (unwords (reverse (w : current))) <= width = go ws (w : current)
      | otherwise = unwords (reverse current) : go (w : ws) []
