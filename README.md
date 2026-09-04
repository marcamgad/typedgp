# TypedGP

[![CI](https://github.com/marcamgad/typedgp/actions/workflows/ci.yml/badge.svg)](https://github.com/marcamgad/typedgp/actions/workflows/ci.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

**Zero-dependency, deterministic, statistically honest symbolic regression in
Haskell.** `base` only; every run reproducible from one 64-bit seed; every
performance claim carries a sample size and a significance test, including the
ones that came out null.

A genetic programming engine for symbolic regression, written from scratch in
Haskell with **no third-party dependencies** â€” `base` only. The expression AST,
the PRNG, mutation, crossover, selection, fitness and the evolution loop are all
hand-written.

Given a table of inputs and outputs, it searches for a formula that reproduces
them. The shipped benchmark is `2x + sin(y)`.

```
gen   0 | best   2.914132 | err   2.902132 | mean     8.4412 | size    6 (avg 11.3) | x * 2 - 0.41
gen   7 | best   0.318871 | err   0.306871 | mean     3.1180 | size    6 (avg 13.9) | x + x - cos(y)
gen  19 | best   0.012044 | err   0.000044 | mean     1.9930 | size    6 (avg 15.2) | x + x + sin(y)
```

## Building

Requires GHC and cabal. If you do not have them, install
[GHCup](https://www.haskell.org/ghcup/) â€” on Windows:

```bash
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) } catch { Write-Error $_ }
```

Then:

```bash
cabal build
```

```bash
cabal test
```

```bash
cabal run typedgp
```

Everything compiles with `-Wall -Werror`.

## Using it

```bash
cabal run typedgp -- --help
```

Run the built-in benchmark across ten seeds and report the convergence rate
(exit status 0 when at least 80% converge):

```bash
cabal run typedgp -- --trials 10 --quiet
```

Fit your own data:

```bash
cabal run typedgp -- --data mydata.txt --vars a,b --gens 200 --target 0.01
```

The data format is one example per line, whitespace and/or comma separated, with
the target value last. `#` starts a comment.

```
# x y target
1.0  0.0   2.0
-2.5, 1.0, -4.158
```

## Architecture

```
src/TypedGP/
  Expr.hs             AST + structural helpers (countNodes, subtreeAt, replaceAt)
  Eval.hs             interpreter, with protected division/log/sqrt
  Pretty.hs           Expr -> readable infix maths, with a constant-render hook
  Simplify.hs         eval-preserving rewrite rules, applied to a fixpoint
  Random.hs           xorshift64* PRNG, pure, explicit state
  Gen/Grow.hs         random expression generation (grow / full / ramped)
  LocalSearch.hs      Nelder-Mead over an expression's constants
  Ops/Mutation.hs     subtree + point mutation, and the mutation dispatcher
  Ops/Hoist.hs        hoist mutation (the only shrinking operator)
  Ops/Crossover.hs    subtree crossover
  Ops/Selection.hs    tournament, roulette, NSGA-II Pareto, Îµ-lexicase
  Fitness.hs          error metrics + parsimony penalty
  Data/Dataset.hs     DataPoint, sampling, splitting, k-folds, resampling
  Population.hs       population type, parallel scoring, generational step
  Evolution.hs        the evolve loop, holdout discipline, run warnings
  Uncertainty.hs      bootstrap confidence intervals for fitted constants
  Report.hs           structured result + hand-rolled JSON encoder
  Checkpoint.hs       population/seed serialisation for --resume
  Config.hs           every hyperparameter, in one record
app/Main.hs           CLI only â€” no search logic
test/                 hand-rolled runner, no HUnit/QuickCheck
```

Four properties hold throughout, and are the reason the code is shaped this way:

**Determinism.** Every stochastic function has the form `... -> Seed -> (a, Seed)`.
There is no global mutable seed anywhere. A whole run is reproducible from one
64-bit number, which is what makes `--trials` a meaningful measurement and a
failing generation reproducible in isolation.

**No partial functions in public APIs.** No `head`, `!!`, `fromJust`, or
incomplete matches are reachable from outside a module. Where an internal helper
relies on a prior check to make a case impossible, there is a comment saying so.

**Protected arithmetic.** `eval` is total and always returns a finite `Double`.
Division by (near) zero yields `1.0`; `NaN` collapses to `0`; results are clamped
to Â±1e12. This matters more than it looks: a single `NaN` in a fitness score
poisons every comparison it takes part in â€” `NaN < x` and `NaN > x` are both
`False` â€” silently corrupting selection for the rest of the run.

**One place for hyperparameters.** No tunable number appears outside
`Config.hs`. Adding a knob means adding a field and a CLI flag, not hunting
through loop bodies.

### The operator set

Arithmetic (`+ - * /`), trigonometry (`sin cos`), and the special functions
`exp log sqrt tanh abs gamma zeta`.

Everything is protected: `eval` is total and always returns a finite `Double`.
Division by near-zero gives `1.0`; `log` and `sqrt` take magnitudes; `gamma`
returns the same `1.0` sentinel at its poles (the non-positive integers) and
`zeta` at its single pole (`s = 1`); results clamp to Â±1e12.

`gamma` uses the Lanczos approximation (`g = 7`, 9 coefficients) with Euler
reflection below `z = 0.5`. `zeta` uses Eulerâ€“Maclaurin summation, which is not
just a fast series but the *analytic continuation* â€” so `zeta(0) = -1/2` and
`zeta(-1) = -1/12` fall straight out of the same formula, with no special
casing, and the trivial zeros at negative even integers come out of the
reflection branch's `sin` factor. All of these are checked against closed forms
in `ExprSpec`.

### Adding an operator

Three steps, two of which the compiler drives:

1. Add the constructor to `Expr`.
2. Add it to `binaryOps` or `unaryOps` in the same file, **with a weight** â€” the
   one manual step, which is why those lists live next to the type.
3. Compile. `-Wall -Werror` reports every pattern match that needs updating
   (`eval`, `evalChecked`, `childrenOf`, `mapChildren`, `opName`, `replaceAt`,
   the pretty-printer, the point-mutation dispatcher, and the constant
   traversals in `Uncertainty`). That error is the refactoring safety net; never
   disable it.

Two warnings from having done this once:

- **The weight is not a formality.** Operators are picked in proportion to
  weight, so adding one at weight 1.0 takes probability from every other
  operator. Nine unary operators picked uniformly alongside four binary ones
  would make ~70% of internal nodes unary and generated trees would be chains
  rather than structures. `gamma` and `zeta` are deliberately down-weighted
  further, because they are the only operators whose evaluation is a numerical
  series rather than a machine instruction, and fitness evaluation is the entire
  inner loop.
- **One site is *not* compiler-checked**: `Checkpoint`'s parser dispatches on
  operator *names*, so a new operator compiles fine and then fails at run time
  when a checkpoint containing it cannot be reloaded. `CheckpointSpec` closes
  that hole by round-tripping whatever is in the registries, rather than a
  hand-listed set.

`Expr.hs` carries a design note on the intended GADT upgrade path (a typed
`Expr a` with `Bool` and `Double` variants, unlocking `If` and comparisons),
including the part that is actually hard â€” making crossover type-aware so a
`Expr Bool` subtree is only ever swapped with another `Expr Bool`.

### Structure search and constant search are different problems

Genetic programming is a search over *structures*. It is good at that and bad
at the continuous problem hiding inside each one â€” constants otherwise move only
by random jitter in point mutation, which is a poor way to solve a smooth
k-dimensional minimisation.

So the two are separated. Evolution picks the shape; `LocalSearch.hs` solves the
numbers inside it with a Nelder-Mead simplex, applied to the elites every
`--refine-every` generations (default 5). Derivative-free by necessity: the
protected operators in `Eval.hs` make the error surface genuinely
discontinuous, so a gradient method would be differentiating something whose
derivative does not exist exactly where evolved individuals congregate.

The method matters, not just the idea. An earlier compass (coordinate) search
is kept in the same module for comparison, and there is a test asserting it
**fails** where the simplex succeeds. On `c1 * x^c2` the two constants are
coupled â€” raising the exponent rescales the output, so a step in either alone
looks worse unless the other moves with it â€” and a coordinate method stalls in
the curved valley. The simplex moves the whole vector at once and reaches
(4, 1.7). Same story on Rosenbrock, which is the textbook version of that
geometry.

**What this does not fix, and why.** `2 sin(3x + 0.5)` is still mostly missed,
because frequency estimation is *multimodal* rather than merely coupled: the
error has a local minimum near every frequency, so a simplex started at 1 stops
at the first barrier. Two tests isolate this â€” from a distant start the
frequency is not recovered; from a start of 2.7 the *same* optimiser recovers
all three constants exactly. What is missing is **initialisation** (a
periodicity search that seeds trigonometric subtrees), not a better optimiser.

### Performance

In the order the work was staged:

1. Correctness first, unoptimised â€” plain lists and naive recursion.
2. Strict accumulators (`BangPatterns`) in the fitness folds and the generation
   loop, where lazy accumulation over hundreds of generations is the classic
   space leak.
3. Parallel fitness evaluation via `par`/`pseq` from `GHC.Conc` (which ships
   inside `base`). Scoring a population is embarrassingly parallel and is the
   real bottleneck; `Population.parMapChunked` sparks chunks of
   `cfgParallelChunk` individuals. `Individual`'s strict fields are what make
   forcing each element to WHNF sufficient to do the arithmetic on the spark.
4. Unboxed arrays for population storage â€” **not done**, deliberately. Profile
   before doing it.

## Testing

`cabal test` runs a hand-written runner: named `Bool` assertions, printed
pass/fail, non-zero exit on any failure. Assertions are forced inside `try`, so
an unexpected exception is reported as one failed assertion instead of killing
the run.

- **ExprSpec** â€” hand-computed evaluation results, protected division on a
  literal zero, `replaceAt` index arithmetic at every position, exact
  pretty-printer output including parenthesisation.
- **RandomSpec** â€” mean and variance of 200k uniforms against the expected
  uniform distribution, bucket occupancy for `nextInt`, no repeats over a 5000-
  sample window (a short cycle would show up here), shuffle-is-a-permutation.
  Imports nothing from the GP side, so a failure here is unambiguously the PRNG.
- **OpsSpec** â€” invariants over 200 seeds: crossover conserves total node count
  when unconstrained, every operator respects the depth cap (including on
  maximum-depth parents), point mutation preserves size and depth exactly,
  selection returns a real pool member and beats a random draw.
- **EvolutionSpec** â€” determinism, generation numbering, early stopping,
  monotone best-fitness under elitism, config validation, and the end-to-end
  convergence test against `2x + sin(y)`.

The convergence assertion is statistical by nature: it requires a *majority* of
seeds to reach the threshold, not all of them. GP does not converge on every
seed, and a test demanding it would be flaky by construction. The structural
assertions around it are exact â€” if they pass and convergence fails, the problem
is tuning, not wiring.

## Before you trust a result

Symbolic regression will *always* hand you a formula. It has no way to
decline. Most of the work of telling a discovery from a coincidence is
procedural rather than computational, so it is written down here once instead
of being re-derived on each project.

Work through this in order. Each step is cheap; skipping them is how a fitted
curve becomes a believed mechanism.

1. **Check the holdout error, not the training error.**
   Run with `--holdout 0.3`. The output prints both, plus a plain-language
   verdict on the gap. Training error alone cannot distinguish a discovered
   relationship from a memorised sample â€” a formula with enough nodes can fit
   any finite dataset exactly. If holdout error is much worse than training
   error, you have found noise.

2. **Check the cross-validation spread, not just its mean.**
   Run with `--cv-folds 5`. A low mean with a *wide* spread means the search
   found something different on each fold, and the average of five unrelated
   answers is not an answer. The output reports the standard deviation as a
   fraction of the mean and interprets it.

3. **Do not ignore the input-to-example warning.**
   Below roughly 10 examples per input variable the search can usually fit the
   sample exactly while learning nothing generalisable, and the run says so.
   You may have a good reason to proceed â€” the warning does not stop the run â€”
   but it should change how you describe the result.

4. **Check the coefficient intervals, not just the coefficients.**
   Run with `--bootstrap 500`. `0.2984` and `0.30 Â± 0.25` are different claims,
   and only one of them is worth acting on. Intervals that are wide relative to
   the coefficient mean the data does not pin that number down, whatever the
   error metric says.

5. **If outliers are plausible, do not use the default metric.**
   Squared error under 5% contamination costs a factor of ten in recovery on
   this project's own benchmark â€” 5% against ~52% for `--metric mae` or
   `--metric huber`. Contaminated data does not announce itself, so the
   question to ask is whether your collection process *could* produce a bad
   reading, not whether you have spotted one.

6. **Sanity-check the formula against domain knowledge.**
   Does it have the right sign? The right units, if your inputs have units?
   Does it behave sensibly outside the range of the training data â€” and do you
   need it to? This step cannot be automated and is the one most often skipped.

7. **Prefer the simplest formula on the Pareto front that is accurate enough.**
   Run with `--selection pareto`. It reports a ladder of formulas at increasing
   complexity rather than one answer at a complexity weight you had to guess in
   advance. The simplest rung that meets your accuracy requirement is almost
   always the better scientific claim.

A formula that survives all seven is worth investigating further. One that has
only been fitted is not a finding.

## âš  Benchmark numbers below predate the `Pow` semantics change (2026-08-19)

`Pow` was redefined as `|base| ^ expo` (see `docs/phase5-pow-semantics.md`).
The design note predicted this would leave every recorded number unchanged,
because it only affects negative bases at exact-integer exponents and those
have measure zero under continuous sampling.

**That prediction was wrong.** Computed exponents hit exact integers
structurally: `protectedDiv x x` returns exactly `1.0`, so `a ^ (x / x)` is an
odd integer power at *every* input. Measured exposure is small â€” 1% of
generation-zero evaluations on `mixed`, under 1% on the rest â€” but the search
is chaotic, and one changed fitness in generation zero changes which parents
are selected, after which the run is unrelated to its predecessor.

Re-measured after the change:

| problem | recorded below | after the change |
|---|---|---|
| `trigonometric` | 100% | 95%\* |
| `mixed` | 25%, 0.0708 / 0.3675 | 25%, 0.0870 / 0.4938 |
| `nested` | 55%, 0.0153 / 0.0630 | 70%, 0.0043 / 0.0031 |
| `rational` | 5%\* | 0%\* |
| `logarithmic` | 60%, 0.0333 | 60%, 0.0370 |

Pooled recovery moved 49/100 to 50/100: **z = 0.14, p = 0.89.** No individual
problem moved significantly either â€” `nested`'s 55% â†’ 70% is z = 0.98,
p = 0.33, and the starred cells are too sparse for a z-test to mean anything.
This is **resampling noise, not evidence for or against the change.**

Every table below was measured on the pre-change engine. They are left as
recorded rather than silently patched, because patching them with numbers that
were never measured would be worse than labelling them. The *comparisons*
within each table remain valid â€” both arms of every A/B were run on the same
engine â€” so the conclusions stand; only the absolute values are stale.

## Benchmark suite

`2x + sin(y)` is recovered 10/10 by generation 0â€“4, which measures the
initialiser more than the search. `typedgp-bench` runs nine mathematical
families instead, so a change to the engine can be shown to help â€” or shown
not to.

```bash
cabal run typedgp-bench -- --quick
```

```bash
cabal run typedgp-bench -- --seeds 5 --format json > before.json
```

Nine families â€” polynomial, rational, power law, exponential, logarithmic,
trigonometric, mixed, nested, interaction â€” crossed with five variants: `clean`,
`noisy`, `irrelevant` (adds distractor columns), `hetero` (noise that widens
with the signal), `outliers` (5% contamination at 6Ïƒ).

Reported per problem: recovery rate, in-domain and extrapolation error,
complexity, time, and false discovery rate. Everything is seeded explicitly, so
two runs of the same command are byte-identical and a diff means a real change.

Two measurement choices worth knowing:

- **Recovery is functional, not syntactic.** A formula counts as recovered when
  its normalised RMSE against a fresh sample of the *noiseless* truth falls
  under 0.05. Comparing expression trees would mostly report noise â€” `x + x`,
  `2 * x` and `x * 2` are the same function and the engine has no reason to
  prefer a spelling.
- **Normalised, and reported as medians.** Errors are divided by the target's
  own spread, so one threshold means the same thing on `2 exp(0.7x)` (range ~30)
  and `2 sin(3x + 0.5)` (range 4); 1.0 is what predicting the mean scores.
  Medians rather than means because results are heavy-tailed â€” one failed seed
  can score a thousand times worse than the rest.

### What it currently says

At default settings (population 500, 80 generations, **20 seeds**, clean
variant):

| | recovery | test nRMSE | extrapolation | median nodes |
|---|---|---|---|---|
| power-law | 100% | 0.0058 | 0.1936 | 19 |
| exponential | 85% | 0.0213 | 0.9025 | 18 |
| interaction | 85% | 0.0000 | 0.0000 | 6 |
| polynomial | 80% | 0.0045 | 0.0080 | 18 |
| logarithmic | 70% | 0.0278 | 4.6854 | 19 |
| nested | 55% | 0.0153 | 0.0630 | 8 |
| trigonometric | 35% | 0.4277 | 0.4518 | 14 |
| mixed | 15% | 0.1010 | 0.3891 | 24 |
| rational | 5% | 0.1819 | 4.5861 | 12 |

**Use 20 seeds, not 3.** The same table at 3 seeds reported `interaction` and
`exponential` at 100%, `logarithmic` at 33% and `trigonometric` at 0% â€” errors
of 20 to 30 percentage points against the numbers above. Symbolic regression
outcomes are Bernoulli trials with high variance; a handful of seeds cannot
separate a real improvement from a lucky draw. Budget the compute.

The picture this gives is far more useful than "10/10 on the demo problem":

- **`rational` is the standing failure â€” 1 of 20.** Runs settle on a sigmoid
  like `tanh(0.59x) + 0.24`: a genuinely good fit to `(2x+1)/(x+3)` that is the
  wrong function, at a tidy 6 nodes. This is the sharpest false-discovery case
  in the suite â€” low error, low complexity, wrong answer.
- **`trigonometric` and `mixed` are next, at 35% and 15%.** `2 sin(3x + 0.5)`
  needs the frequency 3 *inside* the sine. Constant optimisation (below) helped
  here but cannot finish the job, because frequency estimation is multimodal:
  the remaining fix is periodicity-seeded initialisation, not a better
  optimiser.
- **Extrapolation is where fits die.** `logarithmic` scores 0.03 in domain and
  4.7 outside it. In-domain error alone would call that a success.
- **The difficulty gradient is real.** On `interaction` (3 seeds): clean 100% â†’
  noisy 100% â†’ irrelevant 67% â†’ hetero 33% â†’ outliers 0%. Outliers break every
  problem in the suite â€” but only under squared error. See below.

`BenchmarkSpec` tests the instrument rather than the search â€” that
heteroscedastic noise really does widen with the signal, that outliers really
are rare and large, that predicting the mean really does score 1.0. It asserts
no recovery rates: those move with tuning, and pinning them would make the suite
fail for reasons unrelated to correctness.

## Known limits of the demo problem

`2x + sin(y)` is recovered 10/10 at the default settings, usually by generation
0â€“4. That is a weak result to lean on: with a population of 600 and a six-node
target, ramped half-and-half often produces the answer by chance, so the default
run exercises the initialiser more than the search.

Two consequences, both measured rather than assumed:

- **Shrink the population and the search's real weakness shows.** At `--pop 50`
  the default tournament size of 5 samples 10% of the population per selection.
  Diversity collapses within a few generations and 7/10 runs freeze on `x + x`
  â€” the linear term found exactly, the `sin` term never found at all â€” for the
  full 60-generation budget. Dropping to `--tournament 2` recovers 6/10 and
  moves nine of ten runs out of that degenerate optimum.
- **Hoist mutation is unvalidated at the population level.** It is correct in
  isolation (7 assertions, including that it turns `sin(sin(y)) + x + x` into
  `sin(y) + x + x` in one move, which no other operator can do). But varying
  `--hoist-share` over 0.0 / 0.05 / 0.1 / 0.2 / 0.4 changed neither the
  convergence rate nor the mean tree size by more than seed noise. The
  benchmark cannot exercise it: the target is six nodes and parsimony pressure
  is already active, so there is no bloat to remove.

Both point the same way, and `typedgp-bench` above supplies the harder targets
and larger solutions these findings said were missing â€” formulas on
`polynomial` and `mixed` run to 26â€“46 nodes, so there is finally real bloat for
a shrinking operator to remove.

### Robust loss on the `outliers` variant

The `outliers` variant (5% of points displaced by 6Ïƒ) was previously written off
as a known failure. It is not a failure of the engine â€” it is a failure of the
*default metric*. 20 seeds, nine problems, recovery rate:

| | RMSE | MAE | Huber |
|---|---|---|---|
| power-law | 5% | **100%** | 90% |
| interaction | 5% | **95%** | 80% |
| polynomial | 5% | **70%** | **70%** |
| exponential | 5% | 65% | **70%** |
| nested | 15% | 60% | **65%** |
| trigonometric | 5% | **35%** | **35%** |
| mixed | 0% | 20% | **30%** |
| logarithmic | 5% | **30%** | 25% |
| rational | 0% | **5%** | 0% |
| **pooled** | **5.0%** | **53.3%** | **51.7%** |
| total nodes | 246 | 136 | **104** |

**Squared error is the whole problem.** RMSE â†’ MAE or Huber takes pooled
recovery from 5% to ~52%, a ten-fold improvement at z â‰ˆ 9.8. Under 5%
contamination at 6Ïƒ, a squared penalty makes the outliers worth more than the
signal, and the search obediently fits them â€” at 246 total nodes, twice the
complexity of either robust metric, because it is contorting itself to reach
points that are noise.

**But MAE and Huber are statistically tied on recovery** (96 vs 93 successes of
180, z â‰ˆ 0.32). MAE wins on four problems, Huber on three, two tie. So the
honest accounting is that **MAE â€” which was already in the codebase â€” would have
fixed this all along.** The gap was never a missing metric; it was that RMSE is
the default and nobody had run the variant with `--metric mae`. Adding Huber did
not rescue the outliers case. Measuring did.

Where Huber does earn its place is **complexity**: 104 total nodes against MAE's
136, a 24% simpler set of formulas for the same recovery rate, and markedly
better on `nested` (test 0.0030 vs 0.0172, extrapolation 0.0078 vs 0.0621).
Quadratic treatment of small residuals gives it a sharper gradient near a good
fit than MAE's constant slope, and it shows.

One caveat against the default: on `rational`, Huber collapses to a **2-node
constant** and scores 0%. A threshold of 0.1Ïƒ is aggressive, and on a problem
the search cannot fit anyway it appears to prefer conceding to everything over
fitting anything. If you use Huber on a problem that resists fitting, raise
`--huber-delta` before concluding the data is hopeless.

**Practical guidance:** keep RMSE for clean data, switch to `--metric mae` or
`--metric huber` the moment outliers are plausible. The cost of being wrong in
that direction is small; the cost of being wrong the other way is a factor of
ten in recovery.

### Does Îµ-lexicase selection help?

`--selection lexicase` filters candidates case by case in random order,
keeping those within a median-absolute-deviation of the best on each, instead
of ordering on a single aggregate. The point is that a mean cannot distinguish
an individual that is *uniquely excellent on a few cases* from one that is
uniformly mediocre.

Measured at 20 seeds, both arms at **population 200 over 40 generations** â€”
lower than the headline table above, because lexicase is too slow to run the
usual settings (see cost, below). The budget is identical on both sides, so the
comparison is fair; the absolute rates are not comparable to the other tables.

| | tournament | lexicase | change | z | p |
|---|---|---|---|---|---|
| interaction | 35% | **95%** | +60 | **3.98** | **<0.0001** |
| nested | 40% | 15% | âˆ’25 | 1.77 | 0.077 |
| trigonometric | 15% | 30% | +15 | 1.14 | 0.26 |
| power-law | 90% | 80% | âˆ’10 | 0.83 | 0.41 |
| exponential | 60% | 50% | âˆ’10 | 0.63 | 0.53 |
| logarithmic | 30% | 35% | +5 | 0.34 | 0.73 |
| mixed | 10% | 15% | +5 | 0.46 | 0.65 |
| rational | 0% | 5% | +5 | 1.01 | 0.31 |
| polynomial | 60% | 60% | 0 | 0.00 | 1.00 |
| **pooled** | **37.8%** | **42.8%** | +5.0 | **0.97** | **0.33** |

**Nine families were compared, so the multiple-comparisons correction matters
and is applied.** At an uncorrected Î± = 0.05 you would expect roughly one
spurious "significant" result from nine tests by chance alone. The Bonferroni
threshold is Î±/9 â‰ˆ 0.0055. Reading the table against that:

**One confirmed effect.** `interaction` at z â‰ˆ 3.98, p < 0.0001, survives
Bonferroni by more than an order of magnitude. The target is `x1*x2 + sin(x3)`:
a sum of terms that *different data points stress differently*, so an
individual that has captured one term is a genuine partial solution â€” exactly
what an aggregate mean averages away and lexicase preserves. This is the
largest single-problem effect measured anywhere in this project.

**One effect that turned out not to be about lexicase at all.** `nested` at
p â‰ˆ 0.077 does not survive correction, and an ablation (below) shows the
mechanism I first proposed for it was wrong. It is an elitism interaction, not
a property of lexicase.

**One non-effect.** `trigonometric`'s +15 points is z â‰ˆ 1.14, p â‰ˆ 0.26. On 20
seeds a 15-point swing is ordinary noise, and it belongs in this table with its
number attached rather than as a bare delta that reads like a third finding.

So: lexicase is **not** a general improvement, and the single thing this
experiment establishes is that it is dramatically better on one problem whose
shape it is theoretically suited to. Everything else is within noise. That
argues for choosing the strategy per problem rather than switching the default.

#### The elitism confound, resolved

Elites are chosen by *scalar* fitness regardless of which strategy fills the
rest of the population. Under lexicase that is a genuine confound: the elites
are exactly the mean-error generalists lexicase exists to stop privileging. So
both problems were rerun with elitism disabled, 20 seeds, same budget:

| | elitism 2 | elitism 0 |
|---|---|---|
| `nested`, tournament | 40% | **10%** |
| `nested`, lexicase | 15% | **10%** |
| **gap** | **âˆ’25** | **0** |
| `interaction`, tournament | 35% | 30% |
| `interaction`, lexicase | 95% | 90% |
| **gap** | **+60** | **+60** |

**The `nested` regression is not a lexicase effect.** With elitism off the two
strategies are indistinguishable â€” 10% each, gap exactly zero. What actually
happens is that scalar-ranked elitism helps *tournament* enormously on that
problem (40% â†’ 10% when removed, z â‰ˆ 2.2, p â‰ˆ 0.03) and barely helps lexicase
at all (15% â†’ 10%). Tournament and elitism both rank by the same scalar, so
they compound; lexicase optimises something else, so it gains nothing from
them. Remove the prop and neither strategy can do `exp(sin(xÂ²))`.

That is a materially different claim from the one this section originally
made. "Lexicase hurts monolithic problems" was wrong; "on this problem
scalar elitism is doing nearly all the work, and it only compounds with a
scalar selector" is what the data supports. The mechanistic story that made
the p = 0.08 feel credible was a plausible-sounding prior that the ablation
did not confirm â€” which is the entire reason for running it.

**The `interaction` effect is robust to elitism**, holding at +60 points with
elitism off (z â‰ˆ 3.87, p â‰ˆ 0.0001). It is a property of the selection
strategy, not an artefact of what elitism preserves alongside it.

#### Does lexicase-consistent elitism help? No.

The ablation raises an obvious follow-on. If scalar elitism only compounds with
a scalar selector, then under lexicase the elites are chosen by the very
criterion lexicase exists to avoid â€” so choosing them *with the lexicase filter
instead* ought to be free improvement, and might be suppressing some of
`interaction`'s upside rather than merely failing to help on `nested`.

Implemented as `--lexicase-elites` (each elite drawn by a fresh lexicase filter
with its own random case order, without replacement) and measured at 20 seeds:

| | scalar elites | lexicase elites | z |
|---|---|---|---|
| interaction | 95% | 90% | 0.60 |
| trigonometric | 30% | 25% | 0.35 |
| nested | 15% | 20% | 0.42 |
| **pooled** | **46.7%** | **45.0%** | **0.18** |

**Nothing.** Pooled z â‰ˆ 0.18 (p â‰ˆ 0.85), every problem within a single seed of
its counterpart. The hypothesis is not supported.

In hindsight it is consistent with the ablation above rather than contradicted
by it: `interaction` scored +60 with elitism at 2 *and* at 0, so elitism is
essentially irrelevant to that problem. Given that, changing *how* elites are
chosen was never likely to matter there â€” the earlier result already implied
this one, and running it was how that became visible rather than assumed.

One thing that did move, reported because it is the sort of detail a recovery
rate hides: median test error improved on two of three problems
(`trigonometric` 0.44 â†’ 0.22, `nested` 0.39 â†’ 0.21) while recovery did not. On
20 seeds that may be noise, and recovery is the headline metric, so this is not
being claimed as a win â€” only noted so it can be checked again if the option is
ever revisited.

The flag is kept, defaulting **off**, which also keeps every number in this
section exactly reproducible.

**Elite distinctness verified by direct test (2026-08-18); the null stands.**
A null result is only as trustworthy as the implementation that produced it â€”
had `lexicaseElites` silently returned the same individual in every slot, the
"no effect" reading would have been measuring a collapsed elite set rather than
the mechanism. `LexicaseSpec` now covers distinctness against a pool where one
candidate dominates every case, pool exhaustion, and determinism; and the
distinctness assertions were confirmed to *fail* when the exclusion step is
deliberately neutered, so they are checking what they claim to rather than
passing vacuously.

**Cost is the real objection.** Lexicase examines the whole population at every
selection event, of which there is one per offspring, making it O(popÂ²) per
generation where tournament is O(pop):

| population | generations | tournament | lexicase | ratio |
|---|---|---|---|---|
| 100 | 10 | 0.07s | 0.51s | 7Ã— |
| 200 | 40 | 0.55s | 7.8s | 14Ã— |
| 500 | 80 | 3.2s | 129s | 40Ã— |

The ratio grows with population and no constant-factor work removes it. At the
suite's usual population of 500 a 20-seed comparison would take about six hours
per arm. `docs/phase1-lexicase-design.md` records the two performance bugs
found and fixed on the way here â€” an `O(caseIndex)` list lookup, and a
`newtype` that defeated the per-generation sharing it existed to provide â€” plus
why dynamic Îµ was abandoned for semi-dynamic.

### Does frequency seeding help?

Frequency seeding (`TypedGP.Spectral`) runs a direct-sum DFT over each variable
against the target, and when a spectral peak clears a peak-to-median threshold
it biases the multiplier inside generated `sin`/`cos` arguments towards the
detected frequency. It touches initialisation only; every genetic operator
downstream is unchanged. With nothing detected, generation is bit-for-bit what
it was before the feature existed.

20 seeds per arm, `--selection tournament`, population 500, 80 generations.
Tournament rather than lexicase on purpose: these are two independent Phase
changes and confounding them would make neither interpretable.

| problem | seeding off | seeding on | delta | z | p |
|---|---|---|---|---|---|
| `trigonometric` | 35% (7/20) | **100%** (20/20) | **+65pp** | 4.39 | 0.00001 |
| `mixed`         | 15% (3/20) | 25% (5/20) | +10pp | 0.79 | 0.43 |
| `nested`        | 55% (11/20) | 55% (11/20) | 0pp | 0.00 | 1.00 |

Three comparisons, so the Bonferroni-corrected threshold is 0.05/3 = 0.017.
`trigonometric` clears it by three orders of magnitude. `mixed` does not clear
it and is reported as a null. `nested` is the diversity-narrowing canary: its
two arms are identical in recovery, median test error, extrapolation error
*and* median size, which is the signature of no detection at all rather than of
a detection that happened to cancel out. Seeding does not narrow the search on
problems it has no opinion about.

The `trigonometric` result is the largest single effect measured in this
project. It is also the one the mechanism was designed for, and its size is
consistent with the diagnosis that motivated Phase 2: the failure mode was
never structural, it was that the search had to find a real-valued multiplier
inside `sin(cx + d)` by drifting a constant, and handing it the right basin at
initialisation removes the hard part.

#### The `mixed` prediction, and the half of it that was wrong

`docs/phase2-spectral-design.md` open question 2 predicted before the run that
`mixed` â€” `2xÂ² + exp(-x) + sin(y)` â€” would see no benefit, because the `x`
terms are far larger than the unit-amplitude `sin(y)` and are not linear in
`y`, so detrending cannot remove them and the real signal stays buried.

**The weak form held: the +10pp is not significant (z = 0.79, p = 0.43).** The
detector genuinely does not find `sin(y)`, and `SpectralSpec` now asserts that.

**The strong form was wrong.** It predicted the two arms would be *identical*,
since a detector finding nothing makes seeding inert. They were not identical,
which sent me back to look, and the reason was a variable the prediction never
considered: the detector fires **spuriously on `x`**. `2xÂ² + exp(-x)` has no
periodicity at all, but it is not flat after linear detrending either, and its
residual has enough curvature to push a peak past the threshold. Seeding then
biases trig arguments towards a frequency that does not exist in the target.

So the honest reading of the `mixed` row is not "the mechanism did nothing" but
"the mechanism did the wrong thing, and the wrong thing was not measurably
harmful at this sample size". The +10pp is a perturbed search, not a working
one. Both facts are pinned by assertions in `test/SpectralSpec.hs` so a future
change cannot alter the story silently.

That points at the real limitation. The spectrum is computed against the raw
target, so every variable is analysed as though it were the only one. The fix
is to analyse the *residual* after removing what the other variables explain,
which is a Phase 2.5 change rather than a tweak to this one. Raising the
threshold is not the fix â€” it would suppress the spurious `x` peak and the
genuine `trigonometric` peaks together.

### Does penalising domain violations help?

Protected arithmetic keeps the search alive by substituting a sentinel when an
expression leaves its domain â€” `1.0` for a division by zero, `log |x|` for a
log of a negative. That is what stops the search collapsing, and it is also a
blind spot: an individual scoring well *because* it divides by zero on 40% of
the data is indistinguishable from one that does not.

`TypedGP.Eval.evalDomain` makes the difference visible, and `--domain-penalty`
charges for it. **Default 0, which disables the check entirely** â€” at zero
weight the traversal is skipped rather than run and multiplied out, so the
engine's hot loop is untouched.

#### The correctness check

At weight 0 the numbers must be *identical* to the pre-Phase-3 ones, not
merely close. This is a pass/fail assertion, not a statistical one.

| problem | Phase 2 result | Phase 3, defaults |
|---|---|---|
| `trigonometric` | 100%, 0.0000 / 0.0000, size 12 | 100%, 0.0000 / 0.0000, size 12 |
| `mixed` | 25%, 0.0708 / 0.3675, size 25 | 25%, 0.0708 / 0.3675, size 25 |
| `nested` | 55%, 0.0153 / 0.0630, size 8 | 55%, 0.0153 / 0.0630, size 8 |

Every column matches exactly. **Pass.**

#### The measurement, which is a null

20 seeds per arm, tournament selection, on the two families whose true
solutions sit nearest to poles and were therefore the most likely place for an
effect.

| problem | penalty 0 | penalty 0.1 | delta | z | p |
|---|---|---|---|---|---|
| `rational` | 5% (1/20) | 0% (0/20) | âˆ’5pp | 1.01 | 0.31 |
| `logarithmic` | 60% (12/20) | 50% (10/20) | âˆ’10pp | 0.64 | 0.52 |
| **pooled** | 13/40 (32.5%) | 10/40 (25%) | **âˆ’7.5pp** | **0.74** | **0.46** |

**The penalty does not help, and the point estimate is negative.** Nothing
here is significant â€” 0.46 pooled is a long way from any threshold, corrected
or not â€” so the honest statement is "no measurable effect", not "it hurts".
But the direction is worth stating plainly rather than reporting the null and
leaving the sign unmentioned: two problems out of two moved down.

The `rational` row should not be read as a comparison at all. One recovery
against zero is below the count where a two-proportion z-test means anything;
the normal approximation is not valid there and the number is printed for
completeness, not for inference.

Median tree size grew in both arms that had the penalty on (7 â†’ 9,
18 â†’ 22), which is at least consistent with a mechanism: charging for domain
violations pushes the search towards larger, more defensively-structured
expressions. That is a hypothesis suggested by two data points, not a finding.

#### So why keep it

Because recovery rate was never the argument for it. `docs/phase2-design.md`
Â§5 introduced domain validity as **a prerequisite for symbolic
differentiation**, not as a search improvement: a differentiation rule that is
"valid only where `f > 0`" needs somewhere to put that condition, and a
differentiator built on top of silently-clamped protected arithmetic will
confidently differentiate wrong answers near domain boundaries. That is what
this phase delivers, and it is unaffected by the penalty measuring nothing.

The penalty itself stays at weight 0 â€” costing nothing, changing nothing, and
available to anyone who wants to measure it on their own data.

`docs/phase3-domain-design.md` records the one place the implementation
deviates from Â§5's specification (`Invalid` must carry the protected fallback,
or a tree traversal has no number to hand upward) and the open question that
is now the leading suspect for the null: `Saturated` is far more common than
the genuine domain errors, so a single weight may be measuring "does this
overflow" rather than "is this meaningful".

### Recovery rate was measuring the wrong thing, and here is what it was

Three separate investigations â€” the Phase 1 lexicase-elites ablation, the
Phase 3 stacked smoke test, and Phase 4's age-fitness result â€” each found
markedly better median error and smaller formulas at **identical recovery
rate**. Three mechanisms, same signature, and `nested` recurring as the site
of it. That is convergent enough to be a fact about the metric rather than
three coincidences, so it was worth an afternoon.

`nested` is `exp(sin(xÂ²))` â€” five nodes. Per-seed inspection of all 20 runs:

| | recovery (nRMSE < 0.05) | **exact** (nRMSE < 1e-6) | approximations counted as recoveries |
|---|---|---|---|
| tournament | 14/20 (70%) | **10/20 (50%)** | **4** |
| age-fitness | 14/20 (70%) | **14/20 (70%)** | **0** |

Every one of age-fitness's fourteen recoveries is the law itself. Four of
tournament's are not â€” they are sprawling curve fits that squeaked under a 5%
bar. This is one of them, scored as a recovery at nRMSE 0.0323:

```
sqrt(abs(abs(abs(abs(x / (0.7765 / x)) ^ 0.8178) ^ 2.3845 ^ sqrt(x))
  ^ cos(x)) ^ (abs(x) + 0.0966 + abs(x) ^ 0.192)) + sin(1.6396 ^ abs(x))
```

**The recovery threshold conflates "found the law" with "fitted it to within
5%".** Mechanisms that improve solution quality convert approximations into
exact laws â€” which moves median error and median size a great deal and moves
a pass/fail count not at all, because both categories already pass. One
explanation for all three prior observations.

It also explains the median size gap: age-fitness's recoveries are all
minimal (5 nodes), while tournament's four curve fits drag its median to 8.

The engine finds the law in several spellings, all genuinely correct â€”
`exp(sin(x*x))`, `exp(sin(abs(x)^2))`, `2.7183^sin(x*x)`, and
`abs(-6.6601)^(sin(x*x)*0.5274)`, which is `e^sin(xÂ²)` because
`ln(6.6601) Ã— 0.5274 = 1.0000`.

#### What changed as a result

`typedgp-bench` now reports an **`exact`** column alongside `recov`
(`exactThreshold = 1e-6`). The gap between them is the share of a row that is
curve-fitting rather than discovery, and the legend says so.

The threshold is near machine noise deliberately: a formula that is
algebraically the truth evaluates to it bit-for-bit up to floating-point
reassociation, so "exact" and "very good approximation" are separated by
several orders of magnitude rather than by a judgement call. Any threshold in
that gap gives the same answer.

`recoveryThreshold`'s Haddock previously claimed it was "strict enough that an
approximation of the right general shape does not pass". **That claim was
false and is now corrected in place**, with the counterexample above recorded
next to it.

#### The gap is not specific to `nested`

Spot-checked at 8 seeds immediately after adding the column:

| problem | recov | exact | gap |
|---|---|---|---|
| `polynomial` | 62% | **38%** | **24pp** |
| `rational` | 0% | 0% | 0 |
| `trigonometric` | 100% | 100% | 0 |

So `nested` was where the pattern was noticed, not where it lives.
`polynomial` â€” `3xÂ² âˆ’ 2x + 7`, the most elementary problem in the suite â€”
has a quarter of its "recoveries" being approximations. `trigonometric`
has none, which is consistent: seeding hands it the exact frequency, so
runs that succeed succeed exactly.

This means the gap varies by problem, and a comparison between two
mechanisms on a problem with a large gap can move `exact` substantially
while `recov` sits still. That is precisely the failure mode that took
three phases to notice.

**Every recovery-rate number recorded in this README predates the `exact`
column.** They remain valid as recovery rates; they are simply not evidence
about law-discovery. Notably, the comparisons that turned on *median error*
rather than recovery â€” the lexicase-elites addendum, the stacked smoke test,
the age-fitness `nested` row â€” were tracking the real effect all along, and
were the ones flagged as puzzling at the time.

### Recovery rate hides things, and here is one of them

Recovery rate is a **threshold statistic**: it counts seeds whose normalised
RMSE crossed 0.05, and says nothing about how close the rest came. Two arms
with identical recovery can have completely different error distributions.

This turned up twice â€” once in the Phase 1 lexicase-elites ablation (median
test error improved on two of three problems while recovery did not move), and
again in the Phase 3 stacked smoke test (in-domain error consistently better
while recovery was flat-to-down). Two independent mechanisms producing the
same signature is a pattern, not a coincidence, so it was worth one cheap
check: tabulate the per-seed error distribution rather than the pass/fail
count, on the problem where the gap looked starkest.

`interaction`, 20 seeds per arm, population 120, 30 generations. Every seed's
normalised RMSE, sorted:

```
tournament   0.000 0.000 0.038 0.442 0.479 0.496 0.503 0.508 0.509 0.510
             0.518 0.518 0.529 0.532 0.536 0.551 0.655 0.779 0.838 0.892

stacked      0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
             0.000 0.089 0.108 0.148 0.222 0.225 0.226 0.279 0.344 0.532
```

| | recovery | median nRMSE |
|---|---|---|
| tournament | 3/20 (15%) | 0.510 |
| stacked (lexicase + seeding + penalty) | 11/20 (55%) | 0.000 |

z = 2.65, p = 0.008.

**The mass shifted down, uniformly, and eleven seeds went to exact recovery.**
This is not a tail effect and it is not a few outliers dragging a median â€”
more than half the distribution collapsed to zero. Recovery rate was hiding a
large, broad improvement.

It also corrects the read in the stacked smoke test above, which showed
`interaction` at 20% in both arms. That was five seeds, and five seeds could
not have detected this. The smoke test's job was to catch pathologies, and it
did that; it was never powered to see an effect of this size, and reporting
its flat rows as evidence of no effect would have been wrong.

**Caveats, stated plainly.** `interaction` was chosen *because* it looked
starkest in the smoke test, which is a selection effect â€” picking the most
extreme-looking cell and then testing it inflates the apparent size. This is
an **upper bound on one problem**, not a suite-wide estimate.

And a correction to the framing above: the stacked arm varies **two** things,
not three. Frequency seeding is on by default, so it was on in *both* arms and
cannot explain the difference between them. (That is narrower than "seeding is
not a factor" â€” it is closed properly by the second 2Ã—2 below, which varies
seeding directly and finds it worth exactly zero here.)

#### Which mechanism? Two 2Ã—2s

First, selection Ã— domain penalty, with seeding on throughout (20 seeds per
cell). **Measured before the `Pow` semantics change:**

| | penalty 0 | penalty 0.1 |
|---|---|---|
| **tournament** | 3/20 (15%) | 3/20 (15%) |
| **lexicase** | 11/20 (55%) | 11/20 (55%) |

The domain penalty moves recovery by exactly zero in both selection regimes â€”
a second, independent confirmation of the null measured on `rational` and
`logarithmic`.

That left one thing unclosed, and the first version of this section
overstated it: seeding was on in **all four** cells, so while it could not
explain the *difference* between arms, nothing established that it
contributes nothing on its own, or that lexicase's effect does not depend on
it. Selection Ã— seeding, penalty 0 throughout, **all four cells re-run after
the `Pow` change**:

| | seeding off | seeding on | row total |
|---|---|---|---|
| **tournament** | 2/20 (10%) | 3/20 (15%) | 5/40 (12.5%) |
| **lexicase** | **12/20 (60%)** | **11/20 (55%)** | **23/40 (57.5%)** |
| column total | 14/40 (35%) | 14/40 (35%) | |

**Lexicase: +45pp, z = 4.22, p â‰ˆ 0.00002.**
**Seeding: 14/40 either way â€” a delta of exactly zero, z = 0.00.**

Lexicase reaches 60% with seeding *off*, so the effect does not depend on it
in any way. Seeding's two within-row deltas (+5pp under tournament, âˆ’5pp under
lexicase) are one seed each and point in opposite directions.

That null is the expected result rather than a disappointment: `interaction`'s
target has no periodic component, so the spectral detector should find nothing
and seeding should be inert.

Seen together with the frequency-seeding table above, seeding has now been
measured on four problems: **one strong positive where periodicity is present
(`trigonometric`, 35% â†’ 100%, z = 4.39) and three correct nulls where it is
not (`mixed`, `nested`, `interaction`).** A mechanism that fires hard exactly
where its precondition holds and is inert everywhere else is the shape a
working detector should produce â€” and it is a more convincing result than the
`trigonometric` number alone, because the nulls are what rule out "it perturbs
the search and sometimes that helps".

**All three toggles are now characterised on this problem** â€” lexicase +45pp
and highly significant, seeding exactly 0, domain penalty exactly 0. The
caveat from above still applies in full: `interaction` was chosen *because* it
looked starkest, so +45pp is an **upper bound on one problem**, not a
suite-wide estimate of what lexicase is worth.

This also sharpens the Phase 1 lexicase result. That comparison found no
significant recovery effect and a suggestive improvement in median error;
`interaction` was not among the problems it covered, and here the same
mechanism produces a large recovery effect on a problem chosen for exactly the
error-versus-recovery signature Phase 1 flagged. The two are consistent: on
some problems lexicase improves fit without crossing the threshold, and on at
least one it improves fit enough to cross it in half the seeds.

### What actually goes wrong: the domain error breakdown

`typedgp-bench --domain-breakdown` tabulates which `DomainError` constructors
occur, over the initial random population (`gen0`) and over the final Pareto
front (`front`). Shares are of *invalid* evaluations.

The hypothesis going in was that `Saturated` would swamp the rest, making a
single penalty weight measure "does this overflow" rather than "is this
meaningful". **That was wrong.** Saturation is 1â€“16% of generation zero and
**0% of every surviving front**.

What dominates is `PowOfNegativeBase` â€” around 60% of generation zero on every
problem, and **100% of the invalid evaluations among final-front individuals**
on `exponential`, `mixed` and `nested`.

| problem | sample | invalid% | div0 | logâ‰¤0 | pow<0 | saturated |
|---|---|---|---|---|---|---|
| `exponential` | front | 15% | 0% | 0% | **100%** | 0% |
| `logarithmic` | front | 1% | 0% | 100% | 0% | 0% |
| `mixed` | front | **68%** | 0% | 0% | **100%** | 0% |
| `nested` | front | **75%** | 0% | 0% | **100%** | 0% |

On `nested`, three quarters of the reported front's evaluations are out of
domain. That is the scenario `docs/phase2-design.md` Â§5 was written about â€”
"an individual scoring well *because* it divides by zero on 40% of the data" â€”
arriving through `Pow` instead of `Div`, at nearly twice the illustrative
rate.

**But the constructor is conflating two different things.** `protectedPow`
computes `|base| ** expo` for a negative base, which is not nonsense: it is a
smooth, even-symmetric, perfectly differentiable real function. Pulled from
real fronts:

| as it used to print | what it actually computes |
|---|---|
| `sin(x) ^ 1.1114` | `\|sin x\| ^ 1.1114` â€” a rectified sine |
| `(-0.6219) ^ y` | `0.6219 ^ y` â€” an exponential decay |

Both read as broken output and are legitimate discoveries. **The
pretty-printer now renders the absolute value** where it changes the meaning
â€” when the exponent is a literal non-integer and the base is not provably
non-negative â€” so `sin(x) ^ 1.1114` prints as `abs(sin(x)) ^ 1.1114`. It is
deliberately conservative: `abs(x) ^ 1.1`, `(x * x) ^ 0.5` and
`sin(x) ^ 3` are all left alone, because in those cases the absolute value
would be noise or an outright lie.

Chasing the second row turned up something nobody designed. `protectedPow`
keeps the sign for integer exponents and drops it otherwise, so `(-0.6) ^ y`
is **discontinuous at every integer `y`** â€” `+0.36` at 2, `-0.216` at 3,
`+0.6^2.5` at 2.5. On continuous data those points have measure zero, which is
why it went unnoticed. It is not something to build a differentiator on.

`docs/phase5-pow-semantics.md` weighs the two ways out â€” reinterpret `Pow` as
`|base| ^ expo`, or split it and keep textbook exponentiation â€” and recommends
reinterpreting, with the cost (`(-2)^3` becomes `+8`) stated rather than
buried.

Separately, `Saturated` has been **formally amended out of the domain-error
family** in `docs/phase2-design.md` Â§5. It is a numeric-range condition, not a
domain condition: `exp(1000)` is perfectly well defined and perfectly
differentiable, and only the magnitude cap truncated it. Anything asking "is
this meaningful here" must exclude it.

### Symbolic differentiation

`TypedGP.Differentiate.differentiate :: VarName -> Expr -> Maybe Expr`
produces a *formula*, not a number. That is the point â€” a finite-difference
routine gives a function you can sample, while this gives an expression you
can simplify, print, or compare against derivative observations.

**`Nothing` means "not differentiable by these rules", never "the derivative
is zero".** Only `Gamma` and `Zeta` produce it: their derivatives are the
digamma function and zeta-prime, neither of which is in this AST, and
approximating them numerically inside a symbolic differentiator would smuggle
in the numeric approach with none of its honesty. A differentiator that
refuses on `zeta` is more useful than one that lies about it.

**Where a derivative is valid needs no separate machinery.** The derivative is
itself an `Expr`, so `evalDomain` applied to it reports exactly where it is
meaningful â€” a quotient rule's `b = 0` surfaces as `DividedByZero` from the
`Div` the rule emitted. `Saturated` must be excluded when asking, since a
derivative that merely got large is still the right derivative.

#### The `Pow` rules changed, and two of the three got better

`docs/phase2-design.md` Â§1 specified these before `Pow` was redefined as
`|base| ^ expo`. All three cases needed amending:

| case | Â§1 as written | after the semantics change |
|---|---|---|
| constant exponent | `n Â· f^(n-1) Â· f'` | **wrong for odd `n`** â€” is `c Â· f Â· \|f\|^(c-2) Â· f'` |
| constant base | valid only `a > 0`, else `Nothing` | valid for all `a â‰  0` |
| both symbolic | `Nothing` | expressible, valid for `f â‰  0` |

The first is a genuine trap: `n Â· f^(n-1) Â· f'` assumes `Pow f n = f^n`, and
under `|f|^n` it is wrong whenever `n` is odd â€” while agreeing for even `n`,
which is exactly how it would survive casual testing.

The other two improve for one reason worth stating: **`Log` in this AST is
already `log|x|`**, which is precisely the factor the general power rule
needs. The `f > 0` restriction that forced `Nothing` was an artefact of
pairing an unprotected `log` with a protected `Pow`. Reinterpreting `Pow`
didn't only remove a discontinuity â€” it made the general rule expressible in
the AST's own operators.

#### How it is tested

Hand-written rule checks confirm the rules are the ones intended. Only the
**central-difference comparison** confirms they are right, because it is the
one check that does not reuse the differentiator's own reasoning: 200 random
expressions at 6 points each, comparing `eval` of the symbolic derivative
against a numeric one.

The comparison runs *only where both the expression and its derivative are
domain-valid*, including at both one-sided evaluation points. A central
difference across a protected guard is an honest measurement of a step
function, and comparing there would test nothing except that protection
exists. A separate assertion requires at least 300 surviving points, so the
test cannot pass by skipping everything.

Verified by mutation: replacing the power rule with Â§1's textbook form fails
**6 assertions**, including the random property.

```
[FAIL] d/dx sqrt|x| at a negative point
[FAIL] d/dx x^2 at a negative point
[FAIL] d/dx |x|^3 at a negative point
[FAIL] d/dx x^1 is the derivative of |x|
[FAIL] d/dx |x|^0.5 at a negative point
[FAIL] symbolic and numeric derivatives agree on random expressions
```

**Not yet built:** `canonicalize`, listed alongside differentiation in Â§1's
recommended order. Nothing consumes derivatives yet either â€” the roadmap's
derivative-aware fitness is a separate piece, and computing derivatives for
every individual every generation on the off-chance would be the wrong
default.

### Does age-fitness Pareto help?

Age-fitness Pareto (`--selection age-fitness`) minimises **(error, lineage
age)** and injects one fresh random individual per generation. It attacks
premature convergence from a different direction than everything else here:
parsimony, crowding distance and lexicase all preserve variation that already
exists, while this one keeps introducing new variation after generation 0.

20 seeds per cell. Two elitism settings, because the Phase 1 ablation showed
scalar elitism can do more work than the strategy under test â€” and under AFPO
it preserves the oldest fittest lineages every generation, in direct tension
with the mechanism.

| problem | tournament | age-fitness | | tournament | age-fitness |
|---|---|---|---|---|---|
| | *elitism 2* | *elitism 2* | | *elitism 0* | *elitism 0* |
| `rational` | 0%\* | 20%\* | | 0%\* | 5%\* |
| `trigonometric` | 95%\* | 95%\* | | 60% | 65% |
| `mixed` | 25% | 15%\* | | 0%\* | 5%\* |
| `nested` | 70% | 70% | | 65% | 75% |
| **pooled** | **38/80 (47.5%)** | **40/80 (50%)** | | **25/80 (31.3%)** | **30/80 (37.5%)** |

**Pooled at elitism 2: +2.5pp, z = 0.32, p = 0.75.
Pooled at elitism 0: +6.25pp, z = 0.83, p = 0.41.**

**This is a null, in both elitism regimes.** Neither pooled comparison is
close to significance, and the elitism arm â€” included specifically because it
was the confound that overturned a Phase 1 conclusion â€” does not rescue it.

#### Checking the pre-registered prediction

Written before the numbers existed: *help on `rational` and `mixed`; nothing
on `trigonometric` or `nested`; weak prior, null likely.*

| predicted | observed (elitism 2) | verdict |
|---|---|---|
| help on `rational` | 0% â†’ 20% | right direction, not significant, sparse cell |
| help on `mixed` | 25% â†’ 15% | **wrong direction** |
| nothing on `trigonometric` | 95% â†’ 95% | correct |
| nothing on `nested` | 70% â†’ 70% | correct |

Half right. Both predicted nulls held; of the two predicted positives, one
moved up and one moved down. `rational` going 0/20 â†’ 4/20 is the largest
single movement in the table and is exactly the shape AFPO is supposed to
produce â€” but 0 versus 4 successes is below where a two-proportion test means
anything, which is why both cells carry the sparse marker.

#### The cost, which matters for a null

Age-fitness is **3â€“13Ã— slower** per run: `rational` 1.96s â†’ 26.6s,
`nested` 5.16s â†’ 17.8s. It inherits NSGA-II's O(populationÂ²) non-dominated
sort, the same cost `--selection pareto` pays. A mechanism that is
statistically indistinguishable from tournament and an order of magnitude
more expensive is not one to switch on by default, and it is not.

#### One signal worth recording, which is not a recovery result

On `nested`, age-fitness reaches **median test error 0.0000 and median
extrapolation error 0.0000** against tournament's 0.0043 / 0.0031 â€” at
identical 70% recovery, with **median size 5 against 8**. The same holds at
elitism 0.

Perfect median error in *and* out of the training range, from smaller
formulas, while the pass/fail count does not move. That is the third
independent appearance of the pattern documented above under "Recovery rate
hides things": a mechanism improving solution quality in ways a threshold
statistic cannot see. It is a lead, not a win, and it is not what this phase
set out to measure.

### Does constant optimisation help?

Measured properly: 20 seeds, both arms from the same binary, differing only in
`--no-refine`. The control reproduced the pre-`LocalSearch` numbers *exactly*,
digit for digit, on all nine problems â€” which confirms both that refinement
consumes no randomness and that nothing else changed underneath the comparison.

| | recovery | test nRMSE | extrapolation | nodes |
|---|---|---|---|---|
| polynomial | 80 â†’ 80% | 0.0174 â†’ **0.0045** | 0.1439 â†’ **0.0080** | 30 â†’ **18** |
| power-law | 100 â†’ 100% | 0.0078 â†’ 0.0058 | 0.2080 â†’ 0.1936 | 21 â†’ 19 |
| exponential | 80 â†’ **85%** | 0.0288 â†’ 0.0213 | 1.0243 â†’ 0.9025 | 20 â†’ 18 |
| logarithmic | 60 â†’ **70%** | 0.0405 â†’ 0.0278 | 5.6104 â†’ 4.6854 | 20 â†’ 19 |
| trigonometric | 20 â†’ **35%** | 0.4696 â†’ 0.4277 | 0.4961 â†’ 0.4518 | 14 â†’ 14 |
| mixed | 5 â†’ **15%** | 0.0999 â†’ 0.1010 | 0.4765 â†’ 0.3891 | 30 â†’ **24** |
| interaction | 80 â†’ **85%** | 0.0000 â†’ 0.0000 | 0.0000 â†’ 0.0000 | 6 â†’ 6 |
| rational | 0 â†’ 5% | 0.1472 â†’ 0.1819 | 4.0525 â†’ 4.5861 | 6 â†’ 12 |
| nested | 70 â†’ **55%** | 0.0022 â†’ 0.0153 | 0.0037 â†’ 0.0630 | 6 â†’ 8 |
| **pooled** | 55.0 â†’ 58.9% | | | 153 â†’ 138 |

**Do not read the recovery column as a proven win.** Pooled, that is 99 â†’ 106
successes out of 180, a 3.9 point gain at **z â‰ˆ 0.75 (p â‰ˆ 0.46)** â€” comfortably
inside noise. Every individual problem's change is inside noise too, including
the `nested` regression. A sign test over the seven problems that moved (six up,
one down) gives p â‰ˆ 0.13: suggestive, not established.

The continuous metrics carry more weight, because a median over 20 seeds has far
more statistical power than a binary rate over the same runs:

- **`polynomial` improves 3.9Ã— on in-domain error and 18Ã— on extrapolation while
  shrinking from 30 nodes to 18.** That combination is the mechanism visible in
  the data: with accurate constants the search stops spending nodes to
  compensate for bad ones.
- Total median complexity across the suite falls 153 â†’ 138 nodes. Simplification
  was a side effect, not the goal.
- Cost is **+14% wall clock** (3.32s â†’ 3.78s median per run), which is a good
  trade for the error reduction.

**`nested` is the one that got worse on every metric**, and it should not be
waved away. `exp(sin(xÂ²))` has essentially no free constants in its ideal form,
so refinement cannot help it, while sharpening the elites raises selection
pressure toward them and plausibly costs the diversity that problem needs. That
is a hypothesis; the 15-point drop is itself within noise (z â‰ˆ 1.0).

### Does hoist mutation help?

Re-running that question against the suite, at 20 seeds on the four problems
with enough structure to bloat:

| recovery | hoist 0.0 | **0.10** | 0.30 |
|---|---|---|---|
| polynomial | 65% | **80%** | 70% |
| logarithmic | 40% | **60%** | 55% |
| nested | 55% | **70%** | 45% |
| mixed | 5% | 5% | **10%** |
| **pooled (80 runs each)** | 41% | **54%** | 45% |

This is the first evidence that hoist mutation does anything at the population
level. It was introduced on a mechanism argument, and two earlier attempts to
measure it â€” both against the six-node demo problem â€” found nothing either way.

Read it carefully, though. The pooled difference between 0.0 and 0.10 is 12.5
percentage points on 80 runs per arm, which is **z â‰ˆ 1.6, p â‰ˆ 0.11** â€” short of
conventional significance. What makes it more than nothing is the consistency:
0.10 is best or tied on all four problems, and those problems are independent.
Suggestive, not settled; it would take a few hundred seeds to call properly.

Two things worth noting about *how* it appears to help:

- **Not by shrinking trees.** Median node counts do not fall with hoist share
  (polynomial 25 â†’ 30 â†’ 24; nested 11 â†’ 6 â†’ 18). Whatever is happening is not
  the bloat control the operator was nominally added for.
- **The biggest single gain is on `nested`** (`exp(sin(x^2))`), 55% â†’ 70% â€” a
  problem that is *entirely* about nested wrappers. That matches hoist's
  original motivation exactly: it was built to turn `sin(sin(y))` into `sin(y)`
  in one move, which no other operator can do.

The default is 0.10, which this supports â€” but it was chosen before this
evidence existed, so treat the agreement as luck rather than as vindication.
