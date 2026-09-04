# Phase 4 — age-fitness Pareto

Written before implementation. Earlier notes cover NSGA-II selection over
(error, size); none of them says how a third notion — the *age* of a lineage —
should interact with it, and there are at least four decisions here where the
obvious choice is not the right one.

---

## What the mechanism is

Age-fitness Pareto optimisation (Schmidt & Lipson) attacks premature
convergence differently from every mechanism already in this engine. The
existing defences — parsimony, crowding distance, lexicase's case-by-case
filtering — all operate on what an individual *is*. AFPO operates on how long
its lineage has been around:

1. Every individual carries an **age**: generations elapsed since the oldest
   genetic material in it entered the population.
2. Selection minimises **(error, age)** as a Pareto problem. A young
   individual with mediocre error is non-dominated, because nothing older
   beats it on both.
3. Each generation, **one fresh random individual with age 0** is injected.

The point is (3) combined with (2). A newly injected individual would normally
be eliminated immediately — random trees are terrible. Age protection buys its
lineage a few generations to become competitive, which is a continuous supply
of genuinely new material rather than a one-off diverse initialisation that
converges away by generation 20.

This is a real difference from what is already here, which is the argument for
building it: every existing diversity mechanism preserves variation that
*already exists*, and none introduces new variation after generation 0.

---

## Decision 1: age is (error, age), not (error, size, age)

The tempting move is to add age as a third objective to the existing Pareto
selection, giving (error, size, age). **Recommendation: do not.**

Pareto fronts grow rapidly with objective count. With two objectives a typical
population has a small front; with three, the fraction of mutually
non-dominated individuals rises sharply, because dominating now requires being
no worse on *three* axes. In the limit where everything is non-dominated,
rank carries no information and selection pressure comes entirely from
crowding distance — which is a diversity heuristic, not a fitness one.

Concretely: this engine's population is 500. Three objectives on 500
individuals, two of which (size, age) are small integers with heavy ties, is
a recipe for a first front of a hundred-plus individuals.

So `AgeFitness` is its own strategy over **(error, age)**, parallel to
`Pareto`'s (error, size), not an extension of it. Parsimony pressure remains
available on this path through `cfgParsimony`, which is a scalar term inside
`fitTotal` and costs no objective.

The cost of this choice, stated: you cannot get size-Pareto and age-Pareto
simultaneously. That is a real limitation and it is deliberate — the
alternative is a third objective that weakens both.

## Decision 2: generalise `Objectives` rather than adding a field

`Objectives` is currently a two-field record with `objError` and `objSize`,
and `dominates`, `crowdingDistancesFor` and `nsga2Keys` all name those fields
directly.

| option | verdict |
|---|---|
| Add `objAge`, set to `0` when unused | A permanently-zero field is a lie in the type. Worse, `dominates` would compare it, and a constant objective makes *every* comparison tie on that axis — subtly weakening the existing (error, size) selection. |
| Parallel `AgeObjectives` type + duplicated NSGA-II | Two copies of non-dominated sorting and crowding distance. This project has been bitten twice by duplicated logic drifting; not doing it a third time. |
| **Generalise to a list of minimised objectives** | **Recommended.** NSGA-II is defined for *k* objectives; the current code is a specialisation, not a design. |

So: `Objectives` becomes a list, with `objectives :: [Double]` and smart
constructors naming the two schemes. `dominates` compares componentwise;
`crowdingDistancesFor` folds `spreadAlong` over each index instead of calling
it twice by name.

This is a refactor of working, tested code, which is a risk. The mitigation is
that `ParetoSpec`-style assertions already exist for the two-objective case
and must keep passing unchanged — if generalising breaks (error, size)
behaviour, the existing tests say so. Any change in existing Pareto results is
a bug, not an expected consequence.

## Decision 3: where age lives, and how it propagates

Age has to live on `Individual`, which currently holds only `indExpr` and
`indFitness`. A parallel array indexed alongside the population would avoid
touching a core type, and would be wrong: every operation that reorders,
filters or samples the population would have to reorder the array in step, and
one that forgot would silently mis-attribute ages.

Propagation rules:

| event | resulting age |
|---|---|
| initial population | `0` |
| injected individual | `0` |
| crossover of parents `p`, `q` | `max (age p) (age q)` |
| mutation of parent `p` | `age p` |
| reproduction (copy) of `p` | `age p` |
| elite carried over | its own age |
| end of each generation | every surviving age `+ 1` |

`max` for crossover, not `min` or the mean. Age tracks the *oldest* genetic
material present, so a child of an old lineage and a new one is only as
protected as its oldest part — otherwise crossing with a fresh individual
would launder an old lineage's age away and defeat the mechanism entirely.

That is the single most important rule here and the easiest to get wrong,
because `min` is what you would pick if you thought of age as "how new is
this". It is not; it is "how long has this been failing to be replaced".

## Decision 4: injection replaces, it does not grow

One fresh individual per generation, replacing the worst-ranked survivor
rather than enlarging the population. Growing the population would make
`cfgPopulationSize` a lie and change the cost profile of every run.

Injection count is a config knob (`cfgAgeInjection`, default 1) rather than a
literal, per the one-place-for-hyperparameters rule. Default 1 follows the
original AFPO formulation; there is no measurement behind it yet and the
Haddock will say so.

---

## Reproducibility and the default

`AgeFitness` is a new `SelectionStrategy` constructor, so it is off unless
selected. Nothing about the existing paths changes — with one exception that
must be verified rather than assumed: **generalising `Objectives` touches code
that `Pareto` runs.** The validation below therefore includes a bit-for-bit
check of `Pareto` before and after the refactor, which is a correctness
assertion, not a statistical one.

Adding `indAge` to `Individual` changes a type that `Checkpoint` handles.
Invariant 7 applies: if age is serialised it needs a parser entry *and* a
`CheckpointSpec` round-trip case. **Recommendation: do not serialise it.**
`Checkpoint` deliberately stores no `Config` and no strategy, and a resumed
run already cannot know it was an age-fitness run. Storing an age that a
resumed run may silently ignore is worse than storing nothing; resumed
individuals get age 0, which is documented and testable.

---

## Validation

1. **Correctness, not statistics:** `--selection pareto` must return numbers
   identical to those recorded before the `Objectives` generalisation.
2. **Mechanism tests:** age propagates by `max` through crossover (verified by
   mutation — changing it to `min` must fail a test), injection happens once
   per generation, ages increment, injected individuals have age 0.
3. **20-seed benchmark**, `AgeFitness` against `Tournament` and against
   `Pareto`, pooled recovery-rate z-score with the sparse-cell rule applied.

**Prediction, recorded before running.** AFPO targets premature convergence,
so it should help most where the search plateaus early and least where it
already succeeds. On this suite that points at `rational` and `mixed` (low
recovery, high early-plateau) and predicts nothing on `trigonometric` (already
100% with seeding) or `nested`.

The honest prior is weak. Lexicase is the only mechanism measured so far with
a large effect, and it works by a completely different route — case-level
selection pressure rather than lineage protection. There is no reason from
this codebase's own evidence to expect AFPO to be large, and a null is a
perfectly likely outcome worth reporting as one.

## Result, 2026-08-20

**Null.** Pooled over four problems at 20 seeds: +2.5pp at elitism 2
(z = 0.32, p = 0.75) and +6.25pp at elitism 0 (z = 0.83, p = 0.41). Full
table in the README.

The prediction above was half right — both predicted nulls held,
`rational` moved up (0/20 → 4/20, sparse and not significant) and `mixed`
moved *down*. The stated weak prior was the correct prior.

Three things worth carrying forward:

1. **The cost is the decisive fact, not the point estimate.** Age-fitness
   runs 3–13× slower because it inherits NSGA-II's O(population²)
   non-dominated sort. Indistinguishable from tournament and an order of
   magnitude dearer is a clear "off by default", independent of whether the
   small positive point estimate is real.
2. **Open question 1 is answered: elitism is not the confound here.** The
   comparison is null in *both* elitism regimes, so unlike the Phase 1
   lexicase result, scalar elitism is not masking or manufacturing this
   effect. Elitism does matter enormously in absolute terms — tournament
   drops from 95% to 60% on `trigonometric` when it is removed — but it
   does not change the age-fitness verdict.
3. **`nested` shows the recovery-hides-quality pattern for a third time.**
   Median test *and* extrapolation error both reach exactly 0.0000 under
   age-fitness against 0.0043 / 0.0031, at identical 70% recovery and with
   median size 5 against 8. Smaller formulas, perfect median error in and
   out of range, no movement in the pass/fail count.

## Open questions

1. **Interaction with elitism.** Elites are carried over by scalar fitness
   regardless of strategy. Under AFPO that preserves the *oldest*, fittest
   lineages every generation, which is in direct tension with the mechanism.
   The Phase 1 elitism ablation found scalar elitism was doing more work than
   the strategy under test; the same confound applies here and the validation
   should include `--elitism 0`.
2. **Does injection alone explain any effect?** Injecting a random individual
   per generation is a diversity intervention on its own, independent of age
   Pareto. If AFPO shows an effect, the follow-up ablation is injection with
   ordinary tournament selection — otherwise "age-fitness helps" may mean
   "injecting fresh material helps", which is a different and cheaper claim.

   **Closed without running, 2026-08-20.** The premise did not arrive: there
   is no effect to attribute. Running an ablation to decompose a null would
   spend 40 seeds establishing which of two mechanisms is responsible for
   nothing. The `injectFresh` guard that ties injection to `AgeFitness` is
   kept — and tested — so the question is still *answerable* if a future
   result makes it worth asking.

3. **Why is `nested` different?** It is the one problem where age-fitness
   produced visibly better solutions (exact median error in and out of range,
   smaller formulas) without moving recovery. `nested` is also the problem
   where the domain breakdown found 75% of the Pareto front out of domain,
   and the one the hoist-mutation evidence singled out. Something about it
   responds to structural interventions that recovery rate cannot see. Not
   chased here; noted because three separate mechanisms have now pointed at
   the same problem.

---

## Follow-up: open question 3 answered, 2026-08-20

**Why `nested` decouples recovery from error quality: it does not, and neither
does anything else. The metric could not see the difference.**

`nested` is `exp(sin(x^2))`, five nodes. Per-seed inspection of all twenty
runs in each arm:

| | recovery (nRMSE < 0.05) | exact (nRMSE < 1e-6) |
|---|---|---|
| tournament | 14/20 | **10/20** |
| age-fitness | 14/20 | **14/20** |

All fourteen of age-fitness's recoveries are the law. Four of tournament's
are sprawling curve fits under a 5% bar — one at nRMSE 0.0323 runs to
twenty-odd nodes of nested `abs`/`sqrt`/`Pow`.

So the three observations that led here (lexicase elitism, the stacked smoke
test, age-fitness) were one effect, not three: **mechanisms that improve
solution quality convert approximations into exact laws, which a pass/fail
count at 5% cannot register because both categories already pass.** It also
explains the median-size gap — age-fitness's recoveries are all minimal at 5
nodes, tournament's four curve fits drag its median to 8.

Acted on rather than logged: `typedgp-bench` now reports an `exact` column,
`exactThreshold` is defined and tested, and `recoveryThreshold`'s Haddock —
which claimed to be "strict enough that an approximation of the right general
shape does not pass" — is corrected in place with the counterexample beside
it.

The gap is not specific to this problem. `polynomial` shows 62% recovery
against 38% exact.

**This does not change the Phase 4 verdict.** Age-fitness is still a null on
pooled recovery and still 3–13× slower. What it changes is that the `nested`
row was a real signal rather than noise, and that every future comparison has
a metric that can see it.
