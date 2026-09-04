-- | Tests for the structured report and its hand-rolled JSON encoder.
--
-- Most of the value here is in the escaping and number rules, because
-- those are where a hand-written encoder produces output that /looks/
-- right and is not actually valid JSON. The two classic failures are
-- unescaped control characters and bare @NaN@\/@Infinity@ tokens, and both
-- are reachable in this program: formulas are strings built from user
-- variable names, and an empty dataset yields a sentinel error value.
module ReportSpec (tests) where

import Data.List (isInfixOf)

import TypedGP.Config (Config (..), defaultConfig)
import TypedGP.Data.Dataset (Dataset, datasetSize, parseDataset, sampleDataset)
import TypedGP.Evolution (evolve)
import TypedGP.Random (mkSeed)
import TypedGP.Report
  ( JsonValue (..)
  , OutputFormat (..)
  , Report (..)
  , buildReport
  , encodeJson
  , escapeJsonString
  , parseOutputFormat
  , renderReportJson
  , renderReportText
  )

-- | A small, quick problem: these tests are about reporting, not search.
testConfig :: Config
testConfig = defaultConfig
  { cfgPopulationSize = 60
  , cfgGenerations = 4
  , cfgHoldout = Just 0.25
  }

benchmark :: Dataset
benchmark = fst (sampleDataset ["x", "y"] (-3.0, 3.0) 40 target (mkSeed 4242))
  where
    target :: [(String, Double)] -> Double
    target env = 2.0 * variable "x" + sin (variable "y")
      where
        variable name = maybe 0.0 id (lookup name env)

-- | A real report from a real (tiny) run.
sampleReport :: Maybe Report
sampleReport = case evolve testConfig benchmark (mkSeed 5) of
  Left _ -> Nothing
  Right result -> Just (buildReport testConfig 5 1.25 Nothing result)

reportField :: (Report -> Bool) -> Bool
reportField f = maybe False f sampleReport

jsonText :: String
jsonText = maybe "" renderReportJson sampleReport

textLines :: [String]
textLines = maybe [] renderReportText sampleReport

nonFinite :: Double
nonFinite = 1.0 / 0.0

notANumber :: Double
notANumber = 0.0 / 0.0

tests :: [(String, Bool)]
tests =
  -- Data file robustness ---------------------------------------------------
    -- Excel, Notepad and PowerShell all emit a UTF-8 BOM. Before this was
    -- handled, such a file failed with "line 1: expected 2 value(s), got 1"
    -- while looking perfectly correct in every editor.
  [ ("a leading UTF-8 BOM is ignored",
      case parseDataset ["x"] "\xFEFF# comment\n1 2\n3 4\n" of
        Right ds -> datasetSize ds == 2
        Left _   -> False)
    -- The \& is load-bearing: Haskell numeric escapes are greedy, so
    -- "\xFEFF1" is the single character U+FEFF1, not a BOM followed by
    -- '1'. Without the terminator this test silently checks the wrong
    -- thing (and fails, which is how it was found).
  , ("a BOM directly before data is ignored",
      case parseDataset ["x"] "\xFEFF\&1 2\n" of
        Right ds -> datasetSize ds == 1
        Left _   -> False)
  , ("a file without a BOM still parses",
      case parseDataset ["x"] "1 2\n3 4\n" of
        Right ds -> datasetSize ds == 2
        Left _   -> False)

  -- Format selection ------------------------------------------------------
  , ("text format parses", parseOutputFormat "text" == Just TextFormat)
  , ("json format parses", parseOutputFormat "json" == Just JsonFormat)
  , ("an unknown format is rejected", parseOutputFormat "yaml" == Nothing)

  -- String escaping -------------------------------------------------------
  , ("a plain string is quoted", escapeJsonString "abc" == "\"abc\"")
  , ("double quotes are escaped", escapeJsonString "a\"b" == "\"a\\\"b\"")
  , ("backslashes are escaped", escapeJsonString "a\\b" == "\"a\\\\b\"")
  , ("newlines are escaped", escapeJsonString "a\nb" == "\"a\\nb\"")
  , ("tabs are escaped", escapeJsonString "a\tb" == "\"a\\tb\"")
  , ("carriage returns are escaped", escapeJsonString "a\rb" == "\"a\\rb\"")
  , ("other control characters become \\u escapes",
      escapeJsonString "a\SOHb" == "\"a\\u0001b\"")
  , ("\\u escapes are padded to four digits",
      escapeJsonString "\SOH" == "\"\\u0001\"")
  , ("non-ASCII passes through as UTF-8", escapeJsonString "±" == "\"±\"")

  -- Numbers ---------------------------------------------------------------
  , ("infinity encodes as null", encodeJson (JNum nonFinite) == "null")
  , ("negative infinity encodes as null",
      encodeJson (JNum (negate nonFinite)) == "null")
  , ("NaN encodes as null", encodeJson (JNum notANumber) == "null")
  , ("ordinary doubles encode as themselves", encodeJson (JNum 2.5) == "2.5")
  , ("integers encode without a decimal point", encodeJson (JInt 42) == "42")
  , ("negative integers encode", encodeJson (JInt (-7)) == "-7")

  -- Structure -------------------------------------------------------------
  , ("null encodes", encodeJson JNull == "null")
  , ("booleans encode", encodeJson (JBool True) == "true"
      && encodeJson (JBool False) == "false")
  , ("an empty array is compact", encodeJson (JArr []) == "[]")
  , ("an empty object is compact", encodeJson (JObj []) == "{}")
  , ("an array indents its elements",
      encodeJson (JArr [JInt 1, JInt 2]) == "[\n  1,\n  2\n]")
  , ("an object indents its fields",
      encodeJson (JObj [("a", JInt 1)]) == "{\n  \"a\": 1\n}")
  , ("nesting increases indentation",
      encodeJson (JObj [("a", JArr [JInt 1])]) == "{\n  \"a\": [\n    1\n  ]\n}")
  , ("object keys are escaped like any other string",
      encodeJson (JObj [("a\"b", JNull)]) == "{\n  \"a\\\"b\": null\n}")

  -- A real report ---------------------------------------------------------
  , ("a report is produced at all", reportField (const True))
  , ("the report records the seed it was given", reportField ((== 5) . repSeed))
  , ("the report records elapsed time",
      reportField ((== 1.25) . repElapsedSeconds))
  , ("the report records a formula", reportField (not . null . repFormula))
  , ("the report records a reloadable s-expression",
      reportField (\r -> "(" `isInfixOf` repFormulaSExpr r
                          || "$" `isInfixOf` repFormulaSExpr r))
  , ("the report records the holdout split size",
      reportField ((> 0) . repHoldoutSize))
  , ("the report records a holdout error",
      reportField (\r -> repHoldoutError r /= Nothing))
  , ("the report has one history point per generation",
      reportField (\r -> length (repHistory r) == repGenerations r + 1))
  , ("no bootstrap means no constant estimates",
      reportField (null . repConstants))
  , ("no bootstrap means no annotated formula",
      reportField (\r -> repAnnotatedFormula r == Nothing))

  -- Rendered output -------------------------------------------------------
  , ("json output mentions the formula key",
      "\"formula\"" `isInfixOf` jsonText)
  , ("json output mentions the holdout key",
      "\"holdout_error\"" `isInfixOf` jsonText)
  , ("json output mentions warnings", "\"warnings\"" `isInfixOf` jsonText)
  , ("json output contains no bare NaN token", not ("NaN" `isInfixOf` jsonText))
  , ("json output contains no bare Infinity token",
      not ("Infinity" `isInfixOf` jsonText))
  , ("json output is balanced",
      countChar '{' jsonText == countChar '}' jsonText
        && countChar '[' jsonText == countChar ']' jsonText)
  , ("text output reports the holdout error",
      any ("Holdout err" `isInfixOf`) textLines)
  , ("text output interprets generalisation",
      any ("Generalises" `isInfixOf`) textLines)
  , ("text output states a verdict", any ("Verdict" `isInfixOf`) textLines)
  ]
  where
    countChar :: Char -> String -> Int
    countChar c = length . filter (== c)
