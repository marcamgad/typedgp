-- | Tests for random generation, the genetic operators and selection.
--
-- These are invariant tests rather than value tests: the operators are
-- stochastic, so the useful question is not "what did it produce" but
-- "does everything it produces satisfy the properties the evolution loop
-- relies on" — depth stays capped, nodes are conserved, selection returns
-- a real population member.
--
-- Each invariant is checked across many seeds, which is what turns "it
-- worked once" into evidence.
module OpsSpec (tests) where

import TypedGP.Config (Config (..), SelectionStrategy (..), defaultConfig)
import TypedGP.Expr
  ( Expr (..)
  , countNodes
  , depth
  , flatten
  , opName
  , replaceAt
  , subtreeAt
  , unaryOps
  , unaryOpName
  , freeVariables
  )
import TypedGP.Gen.Grow (Method (..), genExpr, rampedHalfAndHalf)
import TypedGP.Ops.Crossover (crossover)
import TypedGP.Ops.Hoist (hoistMutation)
import TypedGP.Ops.Mutation (mutate, pointMutation, subtreeMutation)
import TypedGP.Ops.Selection
  ( Scored (..)
  , fitnessProportionalSelection
  , tournamentSelection
  )
import TypedGP.Random (Seed, mkSeed)

cfg :: Config
cfg = defaultConfig

-- | A config with an effectively unlimited depth ceiling.
--
-- Used for the node-conservation test: with no ceiling, crossover never
-- falls back to a parent, so the conservation law holds exactly and the
-- test measures the swap itself rather than the rejection policy.
unlimitedCfg :: Config
unlimitedCfg = cfg { cfgMaxDepth = 1000 }

-- | Seeds every invariant is checked against.
seeds :: [Seed]
seeds = map mkSeed [1 .. 200]

-- | A random tree within the configured initial depth, per seed.
treeFor :: Seed -> Expr
treeFor s = fst (genExpr cfg Grow (cfgMaxInitialDepth cfg) s)

-- | A deeper random tree, at the hard ceiling — the worst case for the
-- operators' depth handling.
deepTreeFor :: Seed -> Expr
deepTreeFor s = fst (genExpr cfg Full (cfgMaxDepth cfg) s)

-- | @2x + sin(y)@, as a fixed structural fixture.
sample :: Expr
sample = Add (Mul (Const 2.0) (Var "x")) (Sin (Var "y"))

-- | A hand-built scored pool. Fitness 0.5 at index 3 is the unique best
-- (lower is better).
pool :: [Scored Int]
pool =
  -- Record syntax rather than positional: adding a field to Scored should
  -- not silently change what these fixtures mean. The per-case errors are
  -- empty because these tests exercise the scalar strategies only;
  -- LexicaseSpec has its own fixtures with real case vectors.
  [ scoredWith 0 9.0
  , scoredWith 1 4.0
  , scoredWith 2 7.0
  , scoredWith 3 0.5
  , scoredWith 4 6.0
  ]
  where
    scoredWith :: Int -> Double -> Scored Int
    scoredWith value fitness = Scored
      { scoredValue = value
      , scoredFitness = fitness
      , scoredCaseErrors = []
      }

poolValues :: [Int]
poolValues = map scoredValue pool

tests :: [(String, Bool)]
tests =
  -- Generation ------------------------------------------------------------
  [ ("full generation hits its depth budget exactly",
      all (\s -> depth (fst (genExpr cfg Full 4 s)) == 4) seeds)
  , ("full generation at budget 0 is a terminal",
      all (\s -> depth (fst (genExpr cfg Full 0 s)) == 0) seeds)
  , ("grow generation stays within its budget",
      all (\s -> depth (fst (genExpr cfg Grow 5 s)) <= 5) seeds)
  , ("grow generation produces varied shapes",
      length (dedupe (map (\s -> depth (fst (genExpr cfg Grow 5 s))) seeds)) > 1)
  , ("generated trees only use configured variables",
      all (usesOnlyConfiguredVars . treeFor) seeds)
  , ("ramped half-and-half produces the requested count",
      length (fst (rampedHalfAndHalf cfg 50 (mkSeed 1))) == 50)
  , ("ramped half-and-half respects the initial depth cap",
      all (\e -> depth e <= max 2 (cfgMaxInitialDepth cfg))
        (fst (rampedHalfAndHalf cfg 200 (mkSeed 2))))
  , ("ramped half-and-half varies its trees",
      length (dedupe (fst (rampedHalfAndHalf cfg 50 (mkSeed 3)))) > 10)

  -- Operator coverage and weighting ---------------------------------------
    -- Every registered operator must actually be reachable. An operator
    -- with a zero or omitted weight would compile, pass every structural
    -- test, and simply never appear in a single tree.
  , ("random generation reaches every registered unary operator",
      all (`elem` generatedOpNames) (map unaryOpName unaryOps))
  , ("random generation reaches every registered binary operator",
      all (`elem` generatedOpNames) ["+", "-", "*", "/"])
    -- The weighting has to survive contact with the generator, not just
    -- exist in the registry. Binary operators carry 4.0 of the 7.5 total,
    -- so they should be the majority of internal nodes; the expensive
    -- special functions carry 0.15 each and should be markedly rarer than
    -- the trigonometric pair at 0.60.
  , ("binary operators remain the majority of internal nodes",
      countOps ["+", "-", "*", "/"] > countOps (map unaryOpName unaryOps))
  , ("gamma and zeta are rarer than sin and cos",
      countOps ["gamma", "zeta"] < countOps ["sin", "cos"])
  , ("the expensive operators still appear at all",
      countOps ["gamma", "zeta"] > 0)
  , ("point mutation can reach the new operators",
      not (null (pointMutationOpNames `intersect` ["exp", "log", "sqrt", "tanh", "abs"])))

  -- replaceAt node arithmetic ---------------------------------------------
  , ("replaceAt conserves the node count arithmetic",
      all replaceCountHolds [0 .. countNodes sample - 1])
  , ("replaceAt conserves node arithmetic on random trees",
      all (replaceCountHoldsIn . treeFor) seeds)

  -- Crossover -------------------------------------------------------------
  , ("crossover conserves total nodes when depth is unconstrained",
      all crossoverConservesNodes seeds)
  , ("crossover respects the depth cap",
      all crossoverRespectsDepth seeds)
  , ("crossover respects the depth cap on maximum-depth parents",
      all crossoverRespectsDepthDeep seeds)
  , ("crossover children are non-trivial",
      any crossoverChangedSomething seeds)

  -- Mutation --------------------------------------------------------------
  , ("subtree mutation respects the depth cap",
      all (\s -> depth (fst (subtreeMutation cfg (treeFor s) s)) <= cfgMaxDepth cfg)
        seeds)
  , ("subtree mutation respects the cap on maximum-depth trees",
      all (\s -> depth (fst (subtreeMutation cfg (deepTreeFor s) s))
                   <= cfgMaxDepth cfg)
        seeds)
  , ("repeated subtree mutation never escapes the depth cap",
      depth (iterateMutation 60 sample) <= cfgMaxDepth cfg)
  , ("point mutation preserves the node count",
      all (\s -> countNodes (fst (pointMutation cfg (treeFor s) s))
                   == countNodes (treeFor s))
        seeds)
  , ("point mutation preserves the depth",
      all (\s -> depth (fst (pointMutation cfg (treeFor s) s))
                   == depth (treeFor s))
        seeds)
  , ("point mutation actually changes trees",
      any (\s -> fst (pointMutation cfg (treeFor s) s) /= treeFor s) seeds)
  , ("the mutation dispatcher respects the depth cap",
      all (\s -> depth (fst (mutate cfg (deepTreeFor s) s)) <= cfgMaxDepth cfg)
        seeds)
  , ("the mutation dispatcher only uses configured variables",
      all (\s -> usesOnlyConfiguredVars (fst (mutate cfg (treeFor s) s))) seeds)

  -- Hoist mutation --------------------------------------------------------
  , ("hoist mutation never grows a tree",
      all (\s -> countNodes (fst (hoistMutation (deepTreeFor s) s))
                   <= countNodes (deepTreeFor s))
        seeds)
  , ("hoist mutation never deepens a tree",
      all (\s -> depth (fst (hoistMutation (deepTreeFor s) s))
                   <= depth (deepTreeFor s))
        seeds)
  , ("hoist mutation usually shrinks",
      length (filter hoistShrank seeds) > 50)
  , ("hoist mutation leaves a bare terminal alone",
      all (\s -> fst (hoistMutation (Var "x") s) == Var "x") seeds)
  , ("hoist mutation only uses configured variables",
      all (\s -> usesOnlyConfiguredVars (fst (hoistMutation (treeFor s) s))) seeds)
    -- The motivating case, taken from an actual failed run: the search got
    -- stuck on sin(sin(y)) + x + x, which neither subtree nor point
    -- mutation can repair in one move. Hoist can, and this asserts it.
  , ("hoist mutation removes a superfluous wrapper",
      any (\s -> fst (hoistMutation doubleSin s) == singleSin) seeds)
  , ("hoist mutation is reachable through the dispatcher",
      any (\s -> countNodes (fst (mutate cfg (deepTreeFor s) s))
                   < countNodes (deepTreeFor s))
        seeds)

  -- Selection -------------------------------------------------------------
  , ("tournament selection on an empty pool is Nothing",
      tournamentSelection 3 ([] :: [Scored Int]) (mkSeed 1) == Nothing)
  , ("tournament selection returns a pool member",
      all (`elem` poolValues) (selectedBy (tournamentSelection 3)))
  , ("a large tournament always finds the best",
      all (== 3) (selectedBy (tournamentSelection 400)))
  , ("a tournament of one still returns a member",
      all (`elem` poolValues) (selectedBy (tournamentSelection 1)))
  , ("tournament selection beats a random draw on average",
      meanFitnessOf (selectedBy (tournamentSelection 3)) < poolMeanFitness)
  , ("roulette selection on an empty pool is Nothing",
      fitnessProportionalSelection ([] :: [Scored Int]) (mkSeed 1) == Nothing)
  , ("roulette selection returns a pool member",
      all (`elem` poolValues) (selectedBy fitnessProportionalSelection))
  , ("roulette selection beats a random draw on average",
      meanFitnessOf (selectedBy fitnessProportionalSelection) < poolMeanFitness)
  , ("roulette selection reaches more than one individual",
      length (dedupe (selectedBy fitnessProportionalSelection)) > 1)
  , ("the strategy setting is honoured",
      cfgSelection cfg == Tournament)
  ]
 where
  -- Operator coverage helpers ---------------------------------------------
  -- A large sample of internal-node operator names, drawn from trees
  -- generated across every seed.
  generatedOpNames :: [String]
  generatedOpNames =
    [ opName node
    | s <- seeds
    , node <- flatten (fst (genExpr cfg Grow 6 s))
    , not (isLeafName (opName node))
    ]

  isLeafName :: String -> Bool
  isLeafName n = n == "const" || n == "var"

  countOps :: [String] -> Int
  countOps wanted = length (filter (`elem` wanted) generatedOpNames)

  intersect :: [String] -> [String] -> [String]
  intersect xs ys = [x | x <- xs, x `elem` ys]

  -- Operator names produced by point-mutating a unary node, which is the
  -- path that has to honour the weights independently of generation.
  pointMutationOpNames :: [String]
  pointMutationOpNames =
    [ opName (fst (pointMutation cfg (Sin (Var "x")) s)) | s <- seeds ]

  -- Structural helpers ----------------------------------------------------
  dedupe :: Eq a => [a] -> [a]
  dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

  -- Delegates to variablesOf rather than matching constructors here. A
  -- test duplicating a traversal is a second place to update whenever the
  -- AST grows, and this one had exactly that problem: adding the special
  -- functions broke it, and the -Wall incomplete-pattern error was the
  -- only reason that got noticed.
  -- 'freeVariables', not 'variablesOf'. A binder's own index is not a
  -- dataset column and never will be, so counting it would fail this
  -- assertion for a correct expression the moment 'cfgEnableBinders' is
  -- switched on in this module's config.
  --
  -- Latent rather than broken today, since binders are off here — which is
  -- exactly what made the identical conflation in the benchmark's
  -- false-discovery rate survive unnoticed until a binder appeared.
  usesOnlyConfiguredVars :: Expr -> Bool
  usesOnlyConfiguredVars expr =
    all (`elem` cfgVariables cfg) (freeVariables expr)

  donor :: Expr
  donor = Sin (Add (Var "y") (Const 1.0))

  -- Swapping a subtree changes the node count by exactly the difference
  -- between the two subtrees. An off-by-one in replaceAt's left/right
  -- index split breaks this immediately.
  replaceCountHolds :: Int -> Bool
  replaceCountHolds i = replaceCountAt sample i

  replaceCountHoldsIn :: Expr -> Bool
  replaceCountHoldsIn tree = all (replaceCountAt tree) [0 .. countNodes tree - 1]

  replaceCountAt :: Expr -> Int -> Bool
  replaceCountAt tree i = case (subtreeAt i tree, replaceAt i donor tree) of
    (Just removed, Just result) ->
      countNodes result == countNodes tree - countNodes removed + countNodes donor
    _ -> False

  -- Crossover helpers -----------------------------------------------------
  parentsFor :: Seed -> (Expr, Expr)
  parentsFor s = (treeFor s, treeFor (snd (genExpr cfg Grow 1 s)))

  deepParentsFor :: Seed -> (Expr, Expr)
  deepParentsFor s = (deepTreeFor s, deepTreeFor (snd (genExpr cfg Grow 1 s)))

  crossoverConservesNodes :: Seed -> Bool
  crossoverConservesNodes s =
    let (parentA, parentB) = parentsFor s
        ((childA, childB), _) = crossover unlimitedCfg parentA parentB s
    in countNodes childA + countNodes childB
         == countNodes parentA + countNodes parentB

  crossoverRespectsDepth :: Seed -> Bool
  crossoverRespectsDepth s =
    let (parentA, parentB) = parentsFor s
        ((childA, childB), _) = crossover cfg parentA parentB s
    in depth childA <= cfgMaxDepth cfg && depth childB <= cfgMaxDepth cfg

  crossoverRespectsDepthDeep :: Seed -> Bool
  crossoverRespectsDepthDeep s =
    let (parentA, parentB) = deepParentsFor s
        ((childA, childB), _) = crossover cfg parentA parentB s
    in depth childA <= cfgMaxDepth cfg && depth childB <= cfgMaxDepth cfg

  crossoverChangedSomething :: Seed -> Bool
  crossoverChangedSomething s =
    let (parentA, parentB) = parentsFor s
        ((childA, childB), _) = crossover unlimitedCfg parentA parentB s
    in childA /= parentA || childB /= parentB

  -- Hoist helpers ---------------------------------------------------------
  hoistShrank :: Seed -> Bool
  hoistShrank s =
    countNodes (fst (hoistMutation (deepTreeFor s) s)) < countNodes (deepTreeFor s)

  -- sin(sin(y)) + x + x, and the tree hoisting should be able to reach.
  doubleSin :: Expr
  doubleSin = Add (Add (Sin (Sin (Var "y"))) (Var "x")) (Var "x")

  singleSin :: Expr
  singleSin = Add (Add (Sin (Var "y")) (Var "x")) (Var "x")

  iterateMutation :: Int -> Expr -> Expr
  iterateMutation rounds start = go rounds start (mkSeed 77)
    where
      go 0 expr _ = expr
      go k expr s =
        let (expr', s') = subtreeMutation cfg expr s
        in go (k - 1) expr' s'

  -- Selection helpers -----------------------------------------------------
  selectedBy :: ([Scored Int] -> Seed -> Maybe (Int, Seed)) -> [Int]
  selectedBy strategy =
    [ value | s <- seeds, Just (value, _) <- [strategy pool s] ]

  fitnessOf :: Int -> Double
  fitnessOf value =
    sum [ scoredFitness sc | sc <- pool, scoredValue sc == value ]

  meanFitnessOf :: [Int] -> Double
  meanFitnessOf [] = 1.0e9
  meanFitnessOf values =
    sum (map fitnessOf values) / fromIntegral (length values)

  poolMeanFitness :: Double
  poolMeanFitness =
    sum (map scoredFitness pool) / fromIntegral (length pool)
