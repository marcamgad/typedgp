-- | Hand-rolled test runner.
--
-- No HUnit, no QuickCheck — the whole package depends on @base@ alone, and
-- the test suite is not an exception to that rule.
--
-- The contract is deliberately tiny: a suite is a name plus a list of
-- named @Bool@ assertions, and the runner prints each one and exits
-- non-zero if any failed. Assertions are evaluated inside 'try' so that a
-- bottom (an unexpected exception, a pattern-match failure in library
-- code) is reported as a failed assertion with its message, rather than
-- killing the run and hiding every test after it.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import System.Exit (exitFailure, exitSuccess)

import qualified AgeFitnessSpec
import qualified BenchmarkSpec
import qualified BinderSpec
import qualified CheckpointSpec
import qualified DifferentiateSpec
import qualified DomainSpec
import qualified EvolutionSpec
import qualified ExprSpec
import qualified FitnessSpec
import qualified LexicaseSpec
import qualified LocalSearchSpec
import qualified OpsSpec
import qualified ParetoSpec
import qualified RandomSpec
import qualified ReportSpec
import qualified SimplifySpec
import qualified SpectralSpec
import qualified UncertaintySpec

-- | A named group of named assertions.
type Suite = (String, [(String, Bool)])

suites :: [Suite]
suites =
  [ ("Expr / Eval / Pretty", ExprSpec.tests)
  , ("Random",               RandomSpec.tests)
  , ("Fitness / metrics",    FitnessSpec.tests)
  , ("Simplify",             SimplifySpec.tests)
  , ("LocalSearch",          LocalSearchSpec.tests)
  , ("Domain validity",      DomainSpec.tests)
  , ("Differentiate",        DifferentiateSpec.tests)
  , ("Spectral",             SpectralSpec.tests)
  , ("Lexicase",             LexicaseSpec.tests)
  , ("Pareto / NSGA-II",     ParetoSpec.tests)
  , ("Age-fitness",         AgeFitnessSpec.tests)
  , ("Binders",             BinderSpec.tests)
  , ("Gen / Ops",            OpsSpec.tests)
  , ("Checkpoint",           CheckpointSpec.tests)
  , ("Benchmark",            BenchmarkSpec.tests)
  , ("Report / JSON",        ReportSpec.tests)
  , ("Uncertainty",          UncertaintySpec.tests)
  , ("Evolution",            EvolutionSpec.tests)
  ]

main :: IO ()
main = do
  failureCounts <- mapM runSuite suites
  let failed = sum failureCounts
      total = sum (map (length . snd) suites)
      passed = total - failed
  putStrLn ""
  putStrLn (replicate 64 '=')
  putStrLn (show passed ++ " / " ++ show total ++ " assertions passed")
  if failed == 0
    then do
      putStrLn "OK"
      exitSuccess
    else do
      putStrLn (show failed ++ " FAILED")
      exitFailure

-- | Run one suite, returning its failure count.
runSuite :: Suite -> IO Int
runSuite (name, assertions) = do
  putStrLn ""
  putStrLn (name ++ " (" ++ show (length assertions) ++ ")")
  putStrLn (replicate 64 '-')
  outcomes <- mapM runAssertion assertions
  return (length (filter not outcomes))

runAssertion :: (String, Bool) -> IO Bool
runAssertion (name, condition) = do
  outcome <- guarded condition
  case outcome of
    Right True -> do
      putStrLn ("  [pass ] " ++ name)
      return True
    Right False -> do
      putStrLn ("  [FAIL ] " ++ name)
      return False
    Left err -> do
      putStrLn ("  [ERROR] " ++ name ++ ": " ++ show err)
      return False

-- | Force an assertion, catching anything it throws.
--
-- Split out purely so the 'SomeException' annotation has somewhere to live
-- without cluttering the caller.
guarded :: Bool -> IO (Either SomeException Bool)
guarded = try . evaluate
