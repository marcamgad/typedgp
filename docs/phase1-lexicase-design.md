# Phase 1 design note: ε-lexicase selection

**Status: proposal, written before implementation.** Argue with it here rather
than in the diff.

## Recommendation

Add **dynamic ε-lexicase** as a fourth `SelectionStrategy`, alongside
`Tournament`, `FitnessProportional` and `Pareto`. Default unchanged
(`Tournament`), enabled with `--selection lexicase`.

The mechanism, precisely as implemented:

1. Shuffle the case indices (one case = one training data point).
2. Walk the shuffled order. At each case, keep only candidates whose error on
   that case is `<= best + ε`, where `best` is the smallest error on that case
   among the *surviving* candidates and `ε` is the median absolute deviation of
   those same errors.
3. Stop when one candidate remains or the cases run out; return a uniform pick
   among the survivors.

## Why lexicase at all

Tournament selection compares individuals on a single scalar — the aggregate
error. That aggregation is lossy in a specific way that matters here: an
individual that is *uniquely excellent on a handful of cases and mediocre
elsewhere* is indistinguishable from one that is uniformly mediocre, because
both land at the same mean. The first is a partial solution worth preserving;
the second is not.

That is exactly the failure this project has already measured. `2x + sin(y)` at
`--pop 50` freezes on `x + x` in 7/10 runs: the linear term is found, and the
individuals that happen to capture the `sin` component are outcompeted on
aggregate error before the two can be recombined. Lexicase selects on
*individual cases in random order*, so an individual that is best on the cases
where `sin(y)` dominates survives a selection event even if its mean is poor.

## Why the ε, and which variant

Plain lexicase assumes exact pass/fail per case. With real-valued errors exact
ties essentially never occur, so the very first case would reduce the pool to a
single candidate every time — degenerating into "best on one random case",
which is high-variance noise rather than selection. The ε band is what makes it
applicable to regression at all.

Three published variants differ in where `best` and `ε` are measured from:

| variant | `ε` measured over | `best` measured over | cost per selection | notes |
|---|---|---|---|---|
| static | whole population, once per generation | current pool | cheapest — no sorting in the selection loop | ε cannot adapt as the pool narrows |
| semi-dynamic | whole population, once per generation | current pool | cheap | La Cava et al. (2016) report this as generally strongest |
| **dynamic** | current pool | current pool | two medians per case, per selection | most adaptive; what this note implements |

**Chosen: dynamic**, matching the brief's specification that ε is the MAD "of the
candidates' errors on that case". This is the most expensive of the three and
not the literature's usual first choice, which is worth stating plainly. It is
also a roughly ten-line change to switch to semi-dynamic if the benchmark says
the cost is not repaid — the ε computation is isolated in one function
specifically so that swap stays cheap. See open questions.

The comparison is `<=`, not `<`. With `ε = 0` — every candidate identical on a
case, which is common on easy cases where many individuals are exactly right —
a strict `<` would discard *every* candidate including the best, emptying the
pool. `<=` keeps all tied candidates, which is the intended semantics.

## Cost, honestly

Per selection event, worst case (no early termination):

```
O(cases × pool) comparisons  +  O(cases × pool log pool) for the medians
```

against tournament's `O(tournamentSize)`. At population 500 and 100 cases that
is a real difference, and the benchmark's wall-clock column will show it.

Two things make it affordable rather than prohibitive:

- **The case-error matrix is computed once per generation, not once per
  selection.** Selection happens ~500 times per generation; recomputing
  500 × 500 × 100 evaluations would be catastrophic. Implementation: the
  per-case errors live in a **lazy** field on `Scored`, populated when the pool
  is built in `nextGeneration`. Laziness means non-lexicase runs never compute
  it and pay nothing; sharing means lexicase computes it exactly once and every
  selection event in that generation reuses it.
- **Early termination is the common case.** The pool typically collapses to one
  candidate within a handful of cases, so the worst-case bound is rarely
  approached.

### On `cfgLexicaseMaxCases`

**Implement it, defaulting to 0 = use all cases.**

Justification for including it despite the default being "no cap": it bounds
worst-case cost on large datasets, where `cases` is the term that grows without
limit. Because the case order is shuffled per selection, a capped prefix is an
unbiased random subset of cases rather than a fixed favoured subset — so the cap
degrades selectivity gracefully instead of biasing it. That property is what
makes the knob safe to expose; without the shuffle it would not be.

Justification for the default being off: capping is a speed/selectivity trade
that should be made against measured cost, and there is no measurement yet.

## Where the code lives

The brief specifies:

```haskell
selectLexicase :: Config -> Dataset -> Population -> Seed -> (Individual, Seed)
```

**This signature cannot be implemented as written**, for two reasons:

1. **Import cycle.** `Population` imports `Ops.Selection` (for `Scored`,
   `select`, and the NSGA-II machinery). `Ops.Selection` therefore cannot
   import `Population`.
2. **It would break the module's stated design.** `Ops/Selection.hs` documents
   itself as generic — it selects from `[Scored a]` and knows nothing about
   `Expr` or `Dataset`, which is what lets its pressure logic be tested against
   hand-built pools of integers with no genetic programming in the way. That
   property is worth more than signature fidelity.

Implemented instead as:

```haskell
selectLexicase :: forall a. Config -> [Scored a] -> Seed -> Maybe (a, Seed)
```

with the per-case errors carried on `Scored` itself. `Maybe` rather than a bare
pair, matching the existing `select`/`tournamentSelection` convention, because
an empty pool is a real case and invariant 3 forbids a partial function.

The brief's intended signature is available one layer up, in `Population`, where
`Dataset` and `Individual` are both in scope — that is the right home for it.

## Checkpoint (invariant 7)

**No change required, and this is a finding rather than an omission.**
`Checkpoint`'s format is generation, seed, variable names, and expressions. It
does not serialize `SelectionStrategy`, `ErrorMetric`, or any other `Config`
field — a resumed run takes its strategy from the command line, not the file.
So a new selection strategy has nothing to round-trip.

The same was true when `Huber` was added to `ErrorMetric`.

Rather than leave that as tribal knowledge, `CheckpointSpec` gains an explicit
test pinning the format's scope, so that if a future change *does* start
serializing config, the blind spot is already covered by a failing test.

## Testing

Beyond the three tests the brief specifies (determinism over 200 calls; the
specialist-vs-generalist frequency comparison against tournament; MAD
non-negativity and the `ε = 0` tie case), the ε machinery gets direct unit tests
with hand-computed medians, because a wrong median is the most likely bug here
and would be invisible behind a plausible-looking selection frequency.

## Amendment (post-implementation): two performance findings

Written after implementing and measuring. Both change what the note above
proposed, so they are recorded here rather than applied silently.

### 1. Dynamic ε was not affordable — switched to semi-dynamic

The note chose **dynamic** ε (recomputed over the surviving pool at every
case) as specified in the brief, while flagging it as the most expensive
variant and listing "switch to semi-dynamic if the cost is not repaid" as
open question 1.

The cost was not repaid, and not marginally: the first benchmark attempt
measured **164 seconds per seed against tournament's 3.4** and had to be
killed. Open question 1 is therefore closed by measurement in favour of
**semi-dynamic** — ε computed once per generation over the whole
population, `best` still taken from the surviving pool. That also aligns
with La Cava et al. (2016), which the comparison table already noted as the
literature's preferred variant.

### 2. The dominant cost was not the medians at all

Switching to semi-dynamic removed all sorting from the selection loop and
**the runtime did not move** — still ~457s per seed. The real costs were
two things the note did not anticipate:

- **`O(caseIndex)` list indexing.** Candidates stored their errors as a
  per-candidate list, so reading "candidate `c`'s error on case `i`" was a
  `drop i`, executed across the whole 500-candidate pool on every case of
  every selection. Fixed by transposing the matrix once per generation into
  per-case rows and tracking survivors as a boolean mask, so filtering a
  case is one flat linear pass with no indexing.
- **A `newtype` defeating the sharing it was supposed to provide.**
  `SelectionContext` was a `newtype`, which GHC erases; `ctxCases context`
  was therefore the defining expression itself at every selection call
  site, free to be recomputed ~500 times per generation. Changing it to
  `data` with a strict field — a real heap object, so access is a field
  load — was the single largest win.

Measured effect of the two together, at population 100 over 10
generations: **0.51s against tournament's 0.07s**, where before the fixes
the same work did not complete.

### 3. Remaining cost, and what it means for the benchmark

Lexicase is **inherently O(pop²) per generation** — it examines the whole
population at every selection event, of which there are one per offspring —
where tournament is O(pop), sampling a fixed handful each time. So the
ratio grows with population, and no amount of constant-factor work removes
it:

| population | generations | tournament | lexicase | ratio |
|---|---|---|---|---|
| 100 | 10 | 0.07s | 0.51s | 7x |
| 500 | 80 | 3.2s | 129s | 40x |

At the benchmark's usual settings a 20-seed nine-family comparison would
take roughly six hours per arm. The validation was therefore run with
**both arms at population 200 over 40 generations** — a fair comparison,
since the budget is identical on both sides, but the absolute recovery
rates are not comparable to the README's headline table, which uses
population 500. That is stated wherever the numbers appear.

## Open questions

1. **Dynamic vs semi-dynamic ε.** The literature favours semi-dynamic; the
   brief specifies dynamic. If the benchmark shows the wall-clock cost is not
   repaid, semi-dynamic is the first thing to try, and the ε computation is
   isolated to make that a small change.
2. ~~**Interaction with elitism.**~~ **CLOSED by ablation.** Elites are chosen
   by scalar `rankingKeys`, not by lexicase, so under lexicase they are exactly
   the generalists it exists to stop privileging. Measured at 20 seeds on
   `nested` and `interaction`, with elitism 2 against elitism 0:

   - `nested`: the −25 point gap **vanishes entirely** with elitism off (10%
     vs 10%). The regression was never a lexicase effect — scalar elitism
     helps tournament hugely there (40% → 10% when removed) and lexicase
     barely at all, because tournament ranks by the same scalar elitism does
     and compounds with it.
   - `interaction`: the +60 point gap is **unchanged** with elitism off
     (30% vs 90%, z ≈ 3.87). That effect is a property of the selection
     strategy.

   The confound was real and it mattered: it invalidated one of the two
   per-problem claims in the original writeup. It did not touch the other.

   **Follow-on, also now answered: lexicase-consistent elitism does not
   help.** Implemented as `cfgLexicaseElites` — each elite drawn by its own
   lexicase filter, without replacement — and measured at 20 seeds on
   `interaction`, `trigonometric` and `nested`: pooled **z ≈ 0.18
   (p ≈ 0.85)**, every problem within one seed of its counterpart.

   Consistent with the ablation rather than surprising, in hindsight:
   `interaction` scored +60 at elitism 2 *and* at elitism 0, so elitism was
   already irrelevant there, and changing the criterion for something
   irrelevant changes nothing. The earlier result implied this one; running
   it is what made that visible instead of assumed.

   Kept as an option, defaulting off, so the Phase 1 numbers stay
   reproducible.

   **Elite distinctness verified by direct test (2026-08-18); the null
   stands.** A null is only as good as the code that produced it — a
   `lexicaseElites` that collapsed to the same individual in every slot
   would have produced "no effect" for the wrong reason entirely.
   `LexicaseSpec` covers distinctness on a pool where one candidate
   dominates every case, both pool-exhaustion branches, and determinism.
   The distinctness assertions were then confirmed to fail under a
   deliberate mutation of the exclusion step (and only those assertions
   failed), which is the evidence that they test the property rather than
   pass vacuously.
3. **Interaction with the holdout split.** Cases are training cases only, which
   is correct — but it does mean lexicase sees the training set at a finer
   grain than any other strategy, which could plausibly increase overfitting.
   The benchmark reports holdout error, so this is measurable rather than
   speculative.
4. **Does it help at all?** Tournament is a strong baseline and lexicase's
   published wins are largest on program-synthesis problems with discrete
   cases, not continuous regression. A null result is a real possibility and
   will be reported as one.
