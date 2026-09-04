-- | Every tunable knob in the engine, in one record.
--
-- The rule this module exists to enforce: __no hyperparameter literal may
-- appear anywhere else__. Not in the evolution loop, not in the genetic
-- operators, not in the generator. Anything you might want to change
-- between two runs lives here and is threaded explicitly, which is what
-- makes the system scriptable from the CLI instead of recompile-to-tune.
--
-- Two things deliberately do /not/ live here:
--
--   * The __PRNG seed__. It is a run parameter, not a hyperparameter, and
--     it is threaded explicitly through every stochastic function (see
--     "TypedGP.Random"). Storing it here as well would create a second
--     source of truth for the one piece of state that must stay
--     unambiguous.
--   * __Numeric guard rails__ ('TypedGP.Eval.divisionEpsilon',
--     'TypedGP.Eval.magnitudeCap'). Those are correctness invariants of
--     the interpreter, not search knobs; tuning them changes what the
--     language /means/.
--
-- 'ErrorMetric' and 'SelectionStrategy' are defined here rather than in
-- "TypedGP.Fitness" and "TypedGP.Ops.Selection" purely to break an import
-- cycle: those modules need the 'Config', so the 'Config' cannot need them.
module TypedGP.Config
  ( Config (..)
  , ErrorMetric (..)
  , SelectionStrategy (..)
  , defaultHuberDelta
  , defaultConfig
  , validateConfig
  , describeConfig
  , parseErrorMetric
  , parseSelectionStrategy
  ) where

import Data.List (nub)

-- | How prediction error is summarised over a dataset.
data ErrorMetric
  = RMSE  -- ^ Root mean squared error. Same units as the target.
  | MSE   -- ^ Mean squared error. Punishes outliers hardest.
  | MAE   -- ^ Mean absolute error. Fully robust, but less efficient than
          -- 'Huber' when the data is actually clean.
  | Huber !Double
    -- ^ Quadratic for small residuals, linear beyond a threshold: the
    -- standard compromise between the efficiency of squared error and the
    -- robustness of absolute error.
    --
    -- The parameter is the threshold __in units of the target's standard
    -- deviation__, not in raw units. That choice is deliberate and matters
    -- in two ways:
    --
    --   * __Scale-free.__ One value means the same thing on a target
    --     spanning 30 units and one spanning 4, so a single setting works
    --     across a whole benchmark suite instead of needing a per-problem
    --     tuning pass.
    --   * __Identical for every individual.__ The threshold is derived
    --     from the /data/, never from a candidate's own residuals. An
    --     adaptive per-individual threshold would be more statistically
    --     conventional and would quietly break selection: individuals
    --     would be scored on differently-shaped loss functions and their
    --     fitnesses would no longer be comparable.
    --
    -- See 'defaultHuberDelta' for the default and its rationale.
  deriving (Eq, Show)

-- | Default Huber threshold, in units of the target's standard deviation.
--
-- Chosen against the noise levels the benchmark suite actually uses: its
-- ordinary noise is 5% of the target spread and its outlier contamination
-- is 600%. At 0.1 the honest points sit comfortably in the quadratic
-- region — so a clean dataset behaves almost exactly like squared error —
-- while contamination lands far out in the linear tail and stops
-- dominating the fit.
--
-- The classic 1.345-sigma rule is stated in units of the /residual/ scale,
-- which is not available here without measuring it per individual, for the
-- reason given above.
defaultHuberDelta :: Double
defaultHuberDelta = 0.1

-- | How parents are drawn from a scored population.
data SelectionStrategy
  = Tournament           -- ^ Best of @cfgTournamentSize@ random entrants.
  | FitnessProportional  -- ^ Roulette wheel over @1 / (1 + fitness)@.
  | Lexicase
    -- ^ epsilon-lexicase: filter candidates case by case in random order,
    -- keeping those within a median-absolute-deviation of the best on each.
    --
    -- Selects on individual training cases rather than on an aggregate, so
    -- an individual that is uniquely good on a few cases survives even
    -- when its mean error is poor. Aggregation cannot tell such a
    -- specialist apart from a uniformly mediocre individual, and the
    -- specialist is the one worth keeping.
    --
    -- Markedly more expensive per selection than 'Tournament'; see
    -- @docs\/phase1-lexicase-design.md@.
  | AgeFitness
    -- ^ Age-fitness Pareto over (error, lineage age), both minimised,
    -- with a fresh random individual injected each generation.
    --
    -- Attacks premature convergence from a different direction than every
    -- other mechanism here: parsimony, crowding distance and lexicase all
    -- preserve variation that already exists, while this one keeps
    -- introducing new variation after generation 0. A freshly injected
    -- individual would normally be eliminated immediately, since random
    -- trees are bad; being young makes it non-dominated, which buys its
    -- lineage a few generations to become competitive.
    --
    -- Deliberately (error, age) rather than (error, size, age) — see
    -- @docs/phase4-age-fitness-design.md@. Parsimony pressure remains
    -- available here through 'cfgParsimony', which costs no objective.
  | Pareto
    -- ^ NSGA-II style multi-objective selection over (error, size).
    --
    -- Ignores 'cfgParsimony' entirely: instead of committing to one
    -- accuracy-versus-simplicity exchange rate before the search starts,
    -- it keeps the whole non-dominated frontier and reports it, so the
    -- trade-off is chosen after seeing the real options.
  deriving (Eq, Show)

-- | The complete hyperparameter set for one run.
data Config = Config
  { cfgPopulationSize :: !Int
    -- ^ Individuals per generation. The dominant cost knob.
  , cfgGenerations :: !Int
    -- ^ Maximum generations to run. The search may stop earlier if
    -- 'cfgTargetFitness' is reached.
  , cfgMaxInitialDepth :: !Int
    -- ^ Depth ceiling for the ramped half-and-half initial population.
    -- Kept well below 'cfgMaxDepth' so crossover has room to grow trees.
  , cfgMaxDepth :: !Int
    -- ^ Hard depth ceiling enforced by every genetic operator. This is the
    -- primary defence against bloat.
  , cfgCrossoverRate :: !Double
    -- ^ Probability an offspring is produced by subtree crossover.
  , cfgMutationRate :: !Double
    -- ^ Probability an offspring is produced by mutation. Whatever
    -- probability is left over after crossover and mutation is
    -- reproduction (copy a selected parent unchanged).
  , cfgPointMutationShare :: !Double
    -- ^ Given that mutation was chosen, the probability it is a point
    -- mutation. See 'cfgHoistMutationShare' for how the remainder splits.
  , cfgHoistMutationShare :: !Double
    -- ^ Given that mutation was chosen, the probability it is a hoist
    -- mutation. Point and hoist shares are taken first; whatever
    -- probability is left over is subtree mutation.
    --
    -- Hoist is the only strictly shrinking operator, so in principle this
    -- is the knob to raise when trees bloat.
    --
    -- On the evidence for the default: measured against @typedgp-bench@
    -- at 20 seeds across the four problems large enough to bloat, 0.10
    -- recovers 54% against 41% at 0.0 and 45% at 0.30, and is best or
    -- tied on every one of them individually.
    --
    -- That is z ~ 1.6 (p ~ 0.11) on the pooled comparison — suggestive
    -- rather than settled. It is nonetheless the first signal of any kind:
    -- two earlier attempts to measure this against the six-node demo
    -- problem found nothing either way, because a target that small leaves
    -- no bloat for a shrinking operator to act on.
    --
    -- Note also that median tree size does /not/ fall as this rises, so
    -- whatever benefit exists is probably not bloat control. The largest
    -- gain is on the deliberately nested benchmark problem, which fits the
    -- operator's original motivation: escaping a superfluous wrapper such
    -- as @sin(sin(y))@ in a single move.
  , cfgTournamentSize :: !Int
    -- ^ Entrants per tournament. Higher means stronger selection pressure
    -- and faster, less diverse convergence.
  , cfgElitism :: !Int
    -- ^ Best individuals copied verbatim into the next generation. At
    -- least 1 makes best-so-far fitness monotone.
  , cfgParsimony :: !Double
    -- ^ Fitness penalty per node. The parsimony pressure that keeps
    -- solutions readable; set to 0 to disable.
  , cfgEnableBinders :: !Bool
    -- ^ May random generation emit a @Sum@ binder?
    --
    -- __Default 'False', and that default is load-bearing.__ With it off
    -- nothing can produce a binder, so every benchmark number recorded
    -- before binders existed stays reproducible — and the property is
    -- structural rather than statistical, since generation is the only
    -- route by which one could enter a run.
    --
    -- Gates generation only. 'TypedGP.Eval.eval', the pretty-printer, the
    -- checkpoint format and the genetic operators all handle binders
    -- unconditionally; gating /evaluation/ would make a hand-written or
    -- checkpoint-loaded expression behave differently from a generated
    -- one, which is a worse failure than not generating them.
  , cfgBinderProb :: !Double
    -- ^ Chance that an internal node is a binder rather than an ordinary
    -- operator, when binders are enabled and nesting allows.
    --
    -- Drawn before the operator pick, and only when 'cfgEnableBinders' is
    -- set, so a disabled run consumes no randomness for it.
    --
    -- No measurement behind the default; binders have no benchmark yet.
  , cfgBinderMaxTerms :: !Int
    -- ^ Largest term count a generated binder may be given. Bounds are
    -- generated as @1 .. k@ with @k@ drawn from this, never as arbitrary
    -- subtrees — an evolved bound is the runaway-iteration case, and a
    -- sealed one could never be repaired by the operators.
  , cfgBinderDepthLimit :: !Int
    -- ^ How many binders may be nested. @1@ means none inside another.
    --
    -- Bounds nesting rather than tree depth, because stacked binders
    -- multiply iteration counts: the quantity that matters is how many are
    -- stacked, not how deep the tree is.
  , cfgAgeInjection :: !Int
    -- ^ Fresh random individuals injected each generation under
    -- 'AgeFitness' selection. Ignored by every other strategy.
    --
    -- They replace the worst-ranked survivors rather than enlarging the
    -- population. Default 1, following the original age-fitness Pareto
    -- formulation; __there is no measurement behind that figure in this
    -- project yet__, and it is a starting point rather than a tuned value.
  , cfgDomainPenalty :: !Double
    -- ^ Fitness penalty per unit share of training points on which an
    -- individual leaves its mathematical domain — dividing by zero, taking
    -- a log of a negative, raising a negative base to a fractional power,
    -- or saturating at the magnitude cap.
    --
    -- Protected arithmetic keeps such an individual /alive/ by substituting
    -- a sentinel value, which is what stops the search collapsing. The cost
    -- is that an individual scoring well /because/ it divides by zero on
    -- 40% of the data is indistinguishable from one that does not. This
    -- weight is what makes the difference visible to selection.
    --
    -- __Defaults to 0, and that default is load-bearing.__ At zero the
    -- domain traversal is skipped rather than run and multiplied out, so
    -- the engine's hot loop is untouched and every benchmark number
    -- recorded before this feature existed stays reproducible bit-for-bit
    -- rather than merely closely. Turning it on costs a second evaluation
    -- pass over every point of every individual.
    --
    -- Scale is comparable to 'cfgParsimony': both are additive terms on a
    -- fitness whose main component is a normalised error, so @0.1@ means
    -- "being out of domain everywhere costs about a tenth of the error
    -- range". No principled derivation is claimed for that figure.
  , cfgErrorMetric :: !ErrorMetric
  , cfgSelection :: !SelectionStrategy
  , cfgTerminalProb :: !Double
    -- ^ In grow-method generation, the probability of stopping at a
    -- terminal before the depth budget is exhausted.
  , cfgConstProb :: !Double
    -- ^ Given that a terminal is being emitted, the probability it is a
    -- constant rather than a variable.
  , cfgConstRange :: !(Double, Double)
    -- ^ Inclusive-exclusive range for freshly generated constants.
  , cfgConstJitter :: !Double
    -- ^ Maximum absolute perturbation applied to a constant by point
    -- mutation. This is the engine's only local (non-structural) search.
  , cfgVariables :: ![String]
    -- ^ The input columns available to evolved programs. Must match the
    -- dataset the run is scored against.
  , cfgTargetFitness :: !Double
    -- ^ Stop early once the best total fitness reaches this. Also the
    -- success threshold reported by the CLI's trial mode.
  , cfgParallelChunk :: !Int
    -- ^ Individuals per spark during parallel fitness evaluation. Too
    -- small and spark overhead dominates; too large and the last chunk
    -- straggles.
  , cfgHoldout :: !(Maybe Double)
    -- ^ Fraction of the data held out from fitting and scored separately.
    --
    -- Selection only ever sees the training split. The holdout error is
    -- computed purely to be /reported/ — the gap between the two is the
    -- only cheap evidence available about whether a formula found a real
    -- relationship or memorised the sample.
  , cfgSimplifyBest :: !Bool
    -- ^ Run "TypedGP.Simplify" on the elites each generation.
    --
    -- Because simplification is eval-preserving, a simplified elite has
    -- identical error and no more nodes, so its total fitness can only
    -- improve. That makes this a free deterministic counterweight to
    -- bloat, on top of the statistical pressure from 'cfgParsimony'.
  , cfgLexicaseMaxCases :: !Int
    -- ^ Cap on training cases considered per lexicase selection event.
    -- @0@ (the default) means use all of them.
    --
    -- Bounds the worst-case cost on large datasets, where the case count
    -- is the term that grows without limit. It is safe to expose only
    -- because the case order is shuffled per selection: a capped prefix is
    -- then an unbiased random subset rather than a fixed favoured subset,
    -- so the cap degrades selectivity gracefully instead of biasing it.
    --
    -- Off by default because capping trades selectivity for speed and
    -- there is no measurement yet to justify a particular trade.
  , cfgFrequencySeedingEnabled :: !Bool
    -- ^ Detect periodic structure in the data before the run, and bias the
    -- multiplier inside generated @sin@\/@cos@ arguments towards the
    -- frequencies found.
    --
    -- Purely a generation-time bias: it touches no operator, no evaluator
    -- and no simplification, so a differently-initialised population is
    -- still an ordinary population and nothing downstream changes.
  , cfgFrequencySeedTopK :: !Int
    -- ^ How many spectral peaks to keep per variable.
    --
    -- Three rather than one because a true frequency can land second
    -- behind a harmonic or a sampling artefact, so taking only the argmax
    -- is fragile — and three is few enough not to dilute the bias.
  , cfgFrequencySeedWeight :: !Double
    -- ^ Probability that a trigonometric multiplier is drawn from the
    -- detected frequencies rather than from the uniform constant range.
    --
    -- __Must not be 1.0__, and not merely out of caution: a detected peak
    -- can be wrong. Irregular sampling, harmonics and noise all produce
    -- confident-looking peaks in the wrong place. Reserving mass for the
    -- uniform draw means a wrong detection costs efficiency instead of
    -- making the true frequency unreachable — the failure degrades rather
    -- than becoming absolute.
    --
    -- The default of 0.5 is argued rather than guessed. Drawing uniformly
    -- from the default constant range lands within +/-0.05 of a specific
    -- frequency about 1% of the time, so 0.5 is roughly a fifty-fold
    -- improvement on exactly the step that was failing, while half of all
    -- draws stay unbiased and can still find a frequency the detector
    -- missed. Raising it to 0.9 would buy little on top — the bottleneck
    -- is already gone at 0.5 — while making a wrong detection much more
    -- expensive.
  , cfgFrequencySignalThreshold :: !Double
    -- ^ How far the strongest peak must stand above the spectrum's
    -- /median/ magnitude before it counts as signal.
    --
    -- Median, not mean: the mean is dragged upward by the very peak being
    -- tested, which would make the test partly self-referential and let a
    -- flat spectrum pass. Below this ratio the detector reports nothing
    -- and generation is bit-for-bit its unbiased self, which is directly
    -- testable and is tested.
  , cfgFrequencyResolution :: !Int
    -- ^ Number of points on the frequency grid the spectrum is evaluated
    -- at, spanning the constant range.
    --
    -- Not in the original design note; added because the grid spacing is a
    -- genuine tunable and invariant 6 forbids it living as a literal in
    -- "TypedGP.Spectral". 256 points across the default range resolves a
    -- frequency to about +/-0.06, comfortably inside the basin width the
    -- simplex can then close from.
  , cfgLexicaseElites :: !Bool
    -- ^ Under 'Lexicase', choose elites with the lexicase filter itself
    -- rather than by scalar fitness. No effect under other strategies.
    --
    -- Elitism under tournament is neutral scaffolding — it preserves the
    -- best by the same scalar the selector already orders by. Under
    -- lexicase it is not neutral: the elites come from a /different
    -- criterion/ than the rest of the generation, pulling the population
    -- back towards the mean-error generalists lexicase exists to stop
    -- privileging.
    --
    -- An ablation on @nested@ measured scalar elitism doing 30 points of
    -- recovery that had nothing to do with the selector, compounding with
    -- tournament while barely helping lexicase — which is the shape that
    -- interaction predicts.
    --
    -- Defaults to 'False' so that the Phase 1 benchmark numbers in the
    -- README remain exactly reproducible. Flip it only alongside a fresh
    -- measurement.
  , cfgRefineConstants :: !Bool
    -- ^ Numerically optimise the elites' constants during the run.
    --
    -- Genetic programming searches structures well and continuous
    -- parameters badly — constants otherwise move only by random jitter in
    -- point mutation. With this on, the elites' constants are handed to
    -- the simplex optimiser in "TypedGP.LocalSearch" and solved properly.
    --
    -- Applied to elites only. Refitting the whole population would cost
    -- more than the fitness evaluation it is meant to improve, and elites
    -- are the individuals whose genes actually propagate.
  , cfgRefineEvery :: !Int
    -- ^ Generations between refinement passes. 1 refines every
    -- generation; larger values trade accuracy for speed.
  , cfgRefineIterations :: !Int
    -- ^ Simplex updates allowed per refinement. The budget knob — the
    -- method's own parameters live with the method.
  , cfgBootstrapSamples :: !Int
    -- ^ Bootstrap resamples used to put confidence intervals on the
    -- discovered constants. Zero disables the analysis.
  } deriving (Eq, Show)

-- | Defaults tuned for the shipped @2x + sin(y)@ demo problem: large
-- enough to find it reliably, small enough to finish in seconds.
defaultConfig :: Config
defaultConfig = Config
  { cfgPopulationSize     = 600
  , cfgGenerations        = 60
  , cfgMaxInitialDepth    = 4
  , cfgMaxDepth           = 8
  , cfgCrossoverRate      = 0.70
  , cfgMutationRate       = 0.25
  , cfgPointMutationShare = 0.40
  , cfgHoistMutationShare = 0.10
  , cfgTournamentSize     = 5
  , cfgElitism            = 2
  , cfgParsimony          = 0.002
  , cfgDomainPenalty      = 0.0
  , cfgEnableBinders      = False
  , cfgBinderProb         = 0.1
  , cfgBinderMaxTerms     = 10
  , cfgBinderDepthLimit   = 1
  , cfgAgeInjection       = 1
  , cfgErrorMetric        = RMSE
  , cfgSelection          = Tournament
  , cfgTerminalProb       = 0.35
  , cfgConstProb          = 0.30
  , cfgConstRange         = (-5.0, 5.0)
  , cfgConstJitter        = 0.50
  , cfgVariables          = ["x", "y"]
  , cfgTargetFitness      = 0.05
  , cfgParallelChunk      = 32
  , cfgHoldout            = Nothing
  , cfgSimplifyBest       = True
  , cfgLexicaseMaxCases   = 0
  , cfgFrequencySeedingEnabled = True
  , cfgFrequencySeedTopK  = 3
  , cfgFrequencySeedWeight = 0.5
  , cfgFrequencySignalThreshold = 3.0
  , cfgFrequencyResolution = 256
  , cfgLexicaseElites     = False
  , cfgRefineConstants    = True
  , cfgRefineEvery        = 5
  , cfgRefineIterations   = 120
  , cfgBootstrapSamples   = 0
  }

-- | Check a configuration for self-consistency, collecting /all/ problems
-- rather than stopping at the first.
--
-- Every invariant the rest of the engine relies on is asserted here, so
-- downstream modules can be written without defensive re-checking. In
-- particular a validated 'Config' guarantees a non-empty population and a
-- non-empty variable list.
validateConfig :: Config -> Either [String] Config
validateConfig cfg
  | null problems = Right cfg
  | otherwise     = Left problems
  where
    problems = concat
      [ check (cfgPopulationSize cfg >= 2)
          "population size must be at least 2"
      , check (cfgGenerations cfg >= 0)
          "generation count must not be negative"
      , check (cfgMaxInitialDepth cfg >= 1)
          "maximum initial depth must be at least 1"
      , check (cfgMaxDepth cfg >= cfgMaxInitialDepth cfg)
          "maximum depth must be at least the maximum initial depth"
      , probability "crossover rate" (cfgCrossoverRate cfg)
      , probability "mutation rate" (cfgMutationRate cfg)
      , check (cfgCrossoverRate cfg + cfgMutationRate cfg <= 1.0)
          "crossover rate plus mutation rate must not exceed 1"
      , probability "point mutation share" (cfgPointMutationShare cfg)
      , probability "hoist mutation share" (cfgHoistMutationShare cfg)
      , check (cfgPointMutationShare cfg + cfgHoistMutationShare cfg <= 1.0)
          "point and hoist mutation shares must not exceed 1 together"
      , probability "terminal probability" (cfgTerminalProb cfg)
      , probability "constant probability" (cfgConstProb cfg)
      , check (cfgTournamentSize cfg >= 1)
          "tournament size must be at least 1"
      , check (cfgElitism cfg >= 0)
          "elitism must not be negative"
      , check (cfgElitism cfg < cfgPopulationSize cfg)
          "elitism must be smaller than the population size"
      , check (cfgParsimony cfg >= 0)
          "parsimony coefficient must not be negative"
      , check (cfgDomainPenalty cfg >= 0)
          "domain penalty must not be negative"
      , check (cfgAgeInjection cfg >= 0)
          "age injection count must not be negative"
      , check (cfgBinderProb cfg >= 0 && cfgBinderProb cfg <= 1)
          "binder probability must be between 0 and 1"
      , check (cfgBinderMaxTerms cfg >= 1)
          "binder term count must be at least 1"
      , check (cfgBinderDepthLimit cfg >= 0)
          "binder depth limit must not be negative"
      , check (cfgConstJitter cfg >= 0)
          "constant jitter must not be negative"
      , check (fst (cfgConstRange cfg) <= snd (cfgConstRange cfg))
          "constant range must be ordered low to high"
      , check (not (null (cfgVariables cfg)))
          "at least one variable is required"
      , check (all (not . null) (cfgVariables cfg))
          "variable names must not be empty"
      , check (length (nub (cfgVariables cfg)) == length (cfgVariables cfg))
          "variable names must be distinct"
      , check (cfgTargetFitness cfg >= 0)
          "target fitness must not be negative"
      , check (cfgParallelChunk cfg >= 1)
          "parallel chunk size must be at least 1"
      , check (cfgBootstrapSamples cfg >= 0)
          "bootstrap sample count must not be negative"
      , probability "frequency seed weight" (cfgFrequencySeedWeight cfg)
      , check (cfgFrequencySeedTopK cfg >= 1)
          "frequency seed top-k must be at least 1"
      , check (cfgFrequencySignalThreshold cfg > 0)
          "frequency signal threshold must be positive"
      , check (cfgFrequencyResolution cfg >= 1)
          "frequency resolution must be at least 1"
      , check (cfgLexicaseMaxCases cfg >= 0)
          "lexicase case cap must not be negative"
      , check (cfgRefineEvery cfg >= 1)
          "constant refinement interval must be at least 1"
      , case cfgErrorMetric cfg of
          Huber delta -> check (delta > 0)
            "the Huber threshold must be positive"
          _ -> []
      , check (cfgRefineIterations cfg >= 1)
          "constant refinement iteration budget must be at least 1"
      -- An upper bound as well as a lower one: holding out 95% leaves so
      -- little to fit on that the run is meaningless, and it is far more
      -- likely to be a typo than an intention.
      , case cfgHoldout cfg of
          Nothing -> []
          Just fraction -> check (fraction > 0.0 && fraction <= 0.5)
            "holdout fraction must lie in (0, 0.5]"
      ]

    check :: Bool -> String -> [String]
    check True  _   = []
    check False msg = [msg]

    probability :: String -> Double -> [String]
    probability label p =
      check (p >= 0.0 && p <= 1.0) (label ++ " must lie in [0, 1]")

-- | Human-readable dump of a configuration, one setting per line. Used by
-- the CLI so a logged run records exactly what produced it.
describeConfig :: Config -> [String]
describeConfig cfg =
  [ field "population"      (show (cfgPopulationSize cfg))
  , field "generations"     (show (cfgGenerations cfg))
  , field "init depth"      (show (cfgMaxInitialDepth cfg))
  , field "max depth"       (show (cfgMaxDepth cfg))
  , field "crossover"       (show (cfgCrossoverRate cfg))
  , field "mutation"        (show (cfgMutationRate cfg))
  , field "point share"     (show (cfgPointMutationShare cfg))
  , field "hoist share"     (show (cfgHoistMutationShare cfg))
  , field "selection"       (show (cfgSelection cfg))
  , field "tournament"      (show (cfgTournamentSize cfg))
  , field "elitism"         (show (cfgElitism cfg))
  , field "parsimony"       (show (cfgParsimony cfg))
  , field "domainPenalty"   (show (cfgDomainPenalty cfg))
  , field "ageInjection"    (show (cfgAgeInjection cfg))
  , field "binders"        (show (cfgEnableBinders cfg))
  , field "metric"          (show (cfgErrorMetric cfg))
  , field "terminal prob"   (show (cfgTerminalProb cfg))
  , field "const prob"      (show (cfgConstProb cfg))
  , field "const range"     (show (cfgConstRange cfg))
  , field "const jitter"    (show (cfgConstJitter cfg))
  , field "variables"       (unwords (cfgVariables cfg))
  , field "target fitness"  (show (cfgTargetFitness cfg))
  , field "parallel chunk"  (show (cfgParallelChunk cfg))
  , field "holdout"         (maybe "none" show (cfgHoldout cfg))
  , field "simplify best"   (show (cfgSimplifyBest cfg))
  , field "lexicase cap"   (show (cfgLexicaseMaxCases cfg))
  , field "freq seeding"   (show (cfgFrequencySeedingEnabled cfg))
  , field "freq top-k"     (show (cfgFrequencySeedTopK cfg))
  , field "freq weight"    (show (cfgFrequencySeedWeight cfg))
  , field "lexicase elite" (show (cfgLexicaseElites cfg))
  , field "refine consts"   (show (cfgRefineConstants cfg))
  , field "refine every"    (show (cfgRefineEvery cfg))
  , field "bootstrap"       (show (cfgBootstrapSamples cfg))
  ]
  where
    field :: String -> String -> String
    field label value = "  " ++ pad label ++ " : " ++ value

    pad :: String -> String
    pad s = s ++ replicate (max 0 (14 - length s)) ' '

-- | Parse a metric name, case-insensitively.
parseErrorMetric :: String -> Maybe ErrorMetric
parseErrorMetric s = case lowercase s of
  "rmse"  -> Just RMSE
  "mse"   -> Just MSE
  "mae"   -> Just MAE
  "huber" -> Just (Huber defaultHuberDelta)
  _       -> Nothing

-- | Parse a selection strategy name, case-insensitively. @roulette@ is
-- accepted as an alias for fitness-proportional selection.
parseSelectionStrategy :: String -> Maybe SelectionStrategy
parseSelectionStrategy s = case lowercase s of
  "tournament"          -> Just Tournament
  "roulette"            -> Just FitnessProportional
  "fitnessproportional" -> Just FitnessProportional
  "proportional"        -> Just FitnessProportional
  "pareto"              -> Just Pareto
  "lexicase"            -> Just Lexicase
  "epsilon-lexicase"    -> Just Lexicase
  "nsga2"               -> Just Pareto
  "agefitness"          -> Just AgeFitness
  "age-fitness"         -> Just AgeFitness
  "afpo"                -> Just AgeFitness
  _                     -> Nothing

-- | ASCII-only lowering. Sufficient here: every accepted keyword is ASCII,
-- and this avoids pulling in locale-dependent case folding.
lowercase :: String -> String
lowercase = map lower
  where
    lower :: Char -> Char
    lower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c
