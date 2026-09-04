-- | Tests for checkpoint serialisation.
--
-- The reason this file exists is one specific hole in the safety net.
-- Every other place that handles an 'Expr' constructor is a @case@, so
-- @-Wall@ flags it when the AST grows. 'TypedGP.Checkpoint.parseExprS'
-- dispatches on an operator /name/ — a string — so a new operator compiles
-- perfectly and then fails at run time, when a checkpoint containing it
-- turns out to be unloadable.
--
-- The round-trip test below closes that hole by walking
-- 'TypedGP.Expr.unaryOps' and 'TypedGP.Expr.binaryOps' themselves, so it
-- covers whatever is registered rather than whatever someone remembered to
-- list here.
module CheckpointSpec (tests) where

import Data.List (isInfixOf)

import TypedGP.Checkpoint
  ( Checkpoint (..)
  , parseCheckpoint
  , parseExprS
  , renderCheckpoint
  , renderExprS
  )
import TypedGP.Expr
  ( BinaryOp (..)
  , Expr (..)
  , UnaryOp (..)
  , binaryOps
  , unaryOps
  )
import TypedGP.Random (mkSeed, rawSeed, seedWord)

-- | One expression per registered operator, built through the registry so
-- that a newly added operator is covered automatically.
everyOperatorExpr :: [Expr]
everyOperatorExpr =
  [unaryOpBuild op (Var "x") | op <- unaryOps]
    ++ [binaryOpBuild op (Var "x") (Const 2.5) | op <- binaryOps]

roundTrips :: Expr -> Bool
roundTrips expr = parseExprS (renderExprS expr) == Right expr

-- | The leading word of each header line, before the expression block.
--
-- The header ends at the "individuals" count; everything after it is
-- expressions.
headerFieldsOf :: String -> [String]
headerFieldsOf rendered =
  [ key
  | row <- takeWhile (not . isExpressionLine) (lines rendered)
  , key <- take 1 (words row)
  , key /= "individuals"
  ]
  where
    isExpressionLine :: String -> Bool
    isExpressionLine row = case take 1 (dropWhile (== ' ') row) of
      "(" -> True
      "$" -> True
      "#" -> True
      _   -> False

sampleCheckpoint :: Checkpoint
sampleCheckpoint = Checkpoint
  { ckGeneration = 17
  , ckSeed = mkSeed 123456789
  , ckVariables = ["x", "y"]
  , ckExpressions =
      [ Add (Mul (Const 2.0) (Var "x")) (Sin (Var "y"))
      , Gamma (Var "x")
      , Zeta (Sqrt (Abs (Var "y")))
      , Var "y"
      ]
  }

tests :: [(String, Bool)]
tests =
  -- Expression round-trip ---------------------------------------------------
  [ ("every registered operator round-trips through the wire format",
      all roundTrips everyOperatorExpr)
  , ("the operator coverage list is not accidentally empty",
      length everyOperatorExpr == length unaryOps + length binaryOps)
  , ("a bare variable round-trips", roundTrips (Var "x"))
  , ("a bare constant round-trips", roundTrips (Const 2.5))
  , ("a negative constant round-trips", roundTrips (Const (-1.25)))
  , ("a very small constant round-trips", roundTrips (Const 1.0e-12))
  , ("a very large constant round-trips", roundTrips (Const 1.0e12))
  , ("a deeply nested expression round-trips",
      roundTrips (Gamma (Zeta (Tanh (Exp (Log (Sqrt (Abs (Var "q")))))))))
  , ("a variable named like a number round-trips",
      roundTrips (Add (Var "2") (Const 2.0)))
  , ("a variable named like an operator round-trips",
      roundTrips (Add (Var "sin") (Var "x")))

  -- Rendering shape ---------------------------------------------------------
  , ("constants carry the # sigil", renderExprS (Const 1.0) == "#1.0")
  , ("variables carry the $ sigil", renderExprS (Var "x") == "$x")
  , ("operators render in prefix form",
      renderExprS (Add (Var "x") (Var "y")) == "(+ $x $y)")
  , ("unary operators render in prefix form",
      renderExprS (Gamma (Var "x")) == "(gamma $x)")

  -- Parser rejection --------------------------------------------------------
  , ("an unknown operator is rejected",
      case parseExprS "(frobnicate $x)" of
        Left message -> "unknown operator" `isInfixOf` message
        Right _      -> False)
  , ("a leaf without a sigil is rejected",
      case parseExprS "x" of
        Left _  -> True
        Right _ -> False)
  , ("an unbalanced expression is rejected",
      case parseExprS "(+ $x $y" of
        Left _  -> True
        Right _ -> False)
  , ("trailing input is rejected",
      case parseExprS "$x $y" of
        Left _  -> True
        Right _ -> False)
  , ("wrong arity is rejected",
      case parseExprS "(sin $x $y)" of
        Left _  -> True
        Right _ -> False)
  , ("an empty string is rejected",
      case parseExprS "" of
        Left _  -> True
        Right _ -> False)

  -- Checkpoint round-trip ---------------------------------------------------
  , ("a checkpoint round-trips",
      parseCheckpoint (renderCheckpoint sampleCheckpoint) == Right sampleCheckpoint)
    -- The PRNG state must survive exactly. mkSeed scrambles its input, so
    -- a checkpoint reloaded through mkSeed rather than rawSeed would resume
    -- from a different position in the stream and silently break
    -- reproducibility -- which is the whole promise of the seed.
  , ("the seed survives serialisation exactly",
      case parseCheckpoint (renderCheckpoint sampleCheckpoint) of
        Right restored -> ckSeed restored == ckSeed sampleCheckpoint
        Left _         -> False)
  , ("rawSeed inverts seedWord", rawSeed (seedWord (mkSeed 42)) == mkSeed 42)
  , ("mkSeed is deliberately not the inverse of seedWord",
      mkSeed (seedWord (mkSeed 42)) /= mkSeed 42)
  , ("the generation number survives",
      case parseCheckpoint (renderCheckpoint sampleCheckpoint) of
        Right restored -> ckGeneration restored == 17
        Left _         -> False)
  , ("blank lines are tolerated",
      parseCheckpoint (renderCheckpoint sampleCheckpoint ++ "\n\n\n")
        == Right sampleCheckpoint)

  -- The format's scope ------------------------------------------------------
    -- Pinning what a checkpoint does *not* contain, so the blind spot has a
    -- test rather than being tribal knowledge.
    --
    -- The format is generation, seed, variables, expressions. It carries no
    -- Config: not the selection strategy, not the error metric, not any
    -- hyperparameter. A resumed run takes all of that from the command
    -- line. That is why adding a SelectionStrategy or ErrorMetric
    -- constructor needs no Checkpoint change.
    --
    -- If a future change starts serializing config, these assertions fail
    -- and whoever made that change has to come here and extend the
    -- round-trip coverage to the new fields -- which is the whole point.
  , ("a checkpoint carries exactly four header fields",
      length (headerFieldsOf (renderCheckpoint sampleCheckpoint)) == 4)
  , ("the header fields are the documented ones",
      headerFieldsOf (renderCheckpoint sampleCheckpoint)
        == ["typedgp-checkpoint", "generation", "seed", "variables"])
  , ("no selection strategy is serialized",
      not (any (`isInfixOf` renderCheckpoint sampleCheckpoint)
             ["tournament", "roulette", "pareto", "lexicase", "selection"]))
  , ("no error metric is serialized",
      not (any (`isInfixOf` renderCheckpoint sampleCheckpoint)
             ["rmse", "mse", "mae", "huber", "metric"]))

  -- Checkpoint rejection ----------------------------------------------------
  , ("a foreign file is rejected",
      case parseCheckpoint "hello world\n" of
        Left message -> "not a typedgp checkpoint" `isInfixOf` message
        Right _      -> False)
  , ("an empty file is rejected",
      case parseCheckpoint "" of
        Left _  -> True
        Right _ -> False)
  , ("a future format version is rejected",
      case parseCheckpoint "typedgp-checkpoint 99\n" of
        Left message -> "not supported" `isInfixOf` message
        Right _      -> False)
  , ("a miscounted individual list is rejected",
      case parseCheckpoint (unlines
             [ "typedgp-checkpoint 1"
             , "generation 0"
             , "seed 12345"
             , "variables x"
             , "individuals 5"
             , "$x"
             ]) of
        Left message -> "individual" `isInfixOf` message
        Right _      -> False)
  , ("a missing header field is rejected",
      case parseCheckpoint "typedgp-checkpoint 1\nseed 1\n" of
        Left message -> "generation" `isInfixOf` message
        Right _      -> False)
  ]
