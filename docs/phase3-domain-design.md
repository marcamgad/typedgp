# Phase 3 — domain validity

`docs/phase2-design.md` §5 specifies this feature. This note exists because
the implementation deviates from that specification in one structural way, and
because two decisions §5 leaves open have to be made before any code is
written.

Read §5 first; this note assumes it.

---

## The problem, restated

`eval` returns a bare `Double`, so a protected fallback is indistinguishable
from a real value. `protectedDiv 5 0` returns `1.0` and nothing downstream can
tell that apart from an expression that genuinely evaluates to `1.0`. The
consequence is that an individual can score well *because* of protection:
`Eval.hs`'s own Haddock on `protectedPow` already calls the negative-base case
"a real lie about mathematics, told to keep the search numerically alive".

Today that lie is untracked. Phase 3 tracks it.

---

## Deviation from §5: `Invalid` must carry the fallback value

§5 proposes:

```haskell
data EvalResult
  = Valid Double
  | Invalid DomainError
```

This is the wrong shape, for a reason that only shows up when you try to use
it. Evaluation is a *tree traversal*: if `Div a b` divides by zero, the
traversal still needs a number to hand to whatever encloses it, or the whole
expression collapses to a single error and the engine loses the fitness value
it actually needs to rank the individual.

There are three ways out.

| option | single-pass? | keeps §5's type | notes |
|---|---|---|---|
| A. `Invalid DomainError` as specified, and call `eval` separately for the value | no — 2× traversal | yes | doubles the cost of the hot loop |
| B. `Invalid DomainError Double` — the error *and* the protected fallback it used | yes | no | the deviation |
| C. `(Double, Maybe DomainError)` | yes | no | same content as B, worse at the call site |

**Recommendation: B.** The extra `Double` is the value the protected operator
substituted, so an `Invalid` result is fully informative: it says both "this
was out of domain" and "here is the lie that was told instead". A is a real
cost — fitness evaluation is the engine's hot loop and doubling it to compute
something that is off by default is not defensible. C carries identical
information but reads worse: `Invalid DividedByZero 1.0` states the
relationship between the error and the value, while a tuple leaves it implied.

Consequence worth being explicit about: `Valid`/`Invalid` is a claim about
*mathematical* validity, not about whether a number came out. Every
`EvalResult` has a usable `Double` inside it. `evalResultValue` extracts it and
is total.

## Only the first error is kept

An expression can be out of domain in several ways at once. Keeping all of
them means an accumulating structure in the hot loop; keeping a count means
deciding whether `log(log(x))` failing twice is worse than failing once.

Neither pays for itself when the penalty is a *fraction of points* rather than
a severity score. The first error encountered in a left-to-right traversal is
kept, and that is enough to answer the question the penalty asks: was this
point out of domain at all?

Left-to-right is an arbitrary but stable choice, so it is testable.

---

## What counts as a domain error

§5 lists six constructors. Mapping them onto the existing guards in `Eval.hs`:

| constructor | condition | protected fallback |
|---|---|---|
| `DividedByZero` | `abs b < divisionEpsilon` in `Div`; also `0 ** negative` in `Pow`, which is the same fault | `1.0` |
| `LogOfNonPositive` | `x <= 0` in `Log` — **not** `abs x < epsilon` | `0.0` or `log |x|` |
| `PowOfNegativeBase` | `base < 0` with a non-integer exponent | `abs base ** expo` |
| `GammaAtPole` | `isGammaPole x` | `1.0` |
| `ZetaAtPole` | `abs (s - 1) < zetaPoleEpsilon` | `1.0` |
| `Saturated` | `sanitize` actually clamped | `±magnitudeCap` |

Two rows need justification.

**`LogOfNonPositive` fires on the whole non-positive half-line, not just near
zero.** `protectedLog` computes `log |x|`, so `log(-5)` returns `log 5` and
never trips the epsilon guard — it returns a perfectly finite, perfectly
wrong number. That is precisely the case this phase exists to catch, and a
condition copied mechanically from the protection code would miss it. The
protection guard and the validity condition are *different predicates*; this
is the clearest example of why conflating them was the original problem.

**`Saturated` is detected by comparing against the cap, not by intercepting
`sanitize`.** A result equal to `±magnitudeCap` is either a clamp or an
astronomically unlikely coincidence, and treating the coincidence as
saturation is harmless. Detecting `NaN`-collapse-to-zero separately was
considered and rejected: `eval` is structured so `NaN` cannot escape any
protected operator, so it would be unreachable, and an unreachable constructor
is worse than no constructor.

`Sqrt` of a negative is deliberately **not** an error constructor. §5 does not
list one, and adding `SqrtOfNegative` now would be scope creep — but the
reasoning is worth recording, because it is not obvious. `protectedSqrt`
computes `sqrt |x|`, which is exactly as much of a lie as `protectedLog`'s.
The asymmetry is inherited from §5's list rather than justified, and it is an
open question below rather than a decision.

---

## The penalty

A new `Config` field:

```haskell
cfgDomainPenalty :: !Double   -- default 0.0
```

Fitness gains `penalty * invalidFraction`, where `invalidFraction` is the
share of training points on which the individual produced any `Invalid`.

**Default 0.0, which is load-bearing.** At zero weight the domain traversal is
not run at all — fitness takes the existing `eval` path unchanged — so every
Phase 1 and Phase 2 number stays bit-for-bit reproducible, and the cost of the
feature is zero for anyone not using it. This is not merely a conservative
default; it is what makes the earlier phases' benchmarks still valid as
published.

Scale-wise the penalty is comparable to `cfgParsimony`: both are additive
terms on a fitness whose main component is a normalised error, so a weight of
`0.1` means "being out of domain everywhere costs about as much as a tenth of
the total error range". No principled derivation is claimed for that figure;
it is a starting point for measurement.

---

## What is deliberately not in this phase

- **No change to `eval`.** It keeps its signature, its behaviour and its
  Haddock. `evalDomain` is a second, separate traversal. Rewriting `eval` in
  terms of `evalDomain` would be tidier and would silently change the hot
  loop's performance profile, which is not a trade worth making blind.
- **No use of `EvalResult` in `evalChecked`.** Different question (unbound
  variables), different callers (formula parsing).
- **No differentiation.** That is the phase this one unblocks.

---

## Validation

Per the standing rule, a 20-seed benchmark run. But the honest prediction is
recorded first: **with the default weight of 0.0 the numbers must be
*identical*, not merely indistinguishable.** That is a correctness assertion,
not a statistical one, and if it fails the feature is wired up wrong.

The measurement that has content is a run at non-zero weight, which asks
whether penalising domain violations helps, hurts, or does nothing. There is
no strong prior. The `rational` and `logarithmic` families are where it should
show up if anywhere, since they are the ones whose true solutions sit near
poles.

Additionally, and separately from this feature: a **stacked smoke test** with
lexicase selection, frequency seeding and a non-zero domain penalty all
enabled at once, across the full nine-family suite. Not an ablation, and not
powered to detect interactions — just a check that three independent
search-modifying toggles do not do something pathological together, run before
Phase 4 adds a fourth and the combinatorics get worse.

### Stacked smoke test result, 2026-08-19

5 seeds, population 120, 30 generations, all nine families. Small on purpose:
lexicase is roughly an order of magnitude more expensive per generation, and
the question is "does anything break", not "which is better".

| | baseline (tournament, seeding on, no penalty) | stacked (lexicase + seeding + penalty 0.1) |
|---|---|---|
| pooled recovery | 17/45 (37.8%) | 14/45 (31.1%) |
| seconds per run | 0.2 – 0.7 | 3.9 – 6.7 |

z = 0.67, p ≈ 0.50.

**Nothing pathological.** No crashes, no `NaN`, no family collapsing to zero
recovery that did not start there, no size explosion. That is the whole
deliverable and the bar it had to clear.

What it is **not**: evidence that the stack is good. At 5 seeds almost nothing
would reach significance, and the comparison is confounded by lexicase's known
cost. Reading the −6.7pp as a real regression would be exactly the overreach
this project's statistical rule exists to stop.

> **Correction, 2026-08-19.** A 20-seed follow-up on `interaction` found
> lexicase moving recovery 15% → 55% (z = 2.65, p = 0.008) on the very problem
> this table reports as flat at 20% in both arms. Five seeds per arm could not
> have seen an effect that size.
>
> So the conclusion above should be read as **"no catastrophic pathology
> detected"**, not "no meaningful effect". That is all a 5-seed budget can
> support, and the distinction matters going forward: a smoke test of this
> size rules out crashes and collapses, not regressions or improvements.
>
> A second correction to the framing: this arm varies **two** things, not
> three. Frequency seeding is on by default and was therefore on in *both*
> arms, so it was never a candidate explanation for any difference here.

One observation that was not the question asked, recorded because it is a
lead rather than a result: **in-domain test error is consistently better in
the stacked arm while recovery is flat or down** — `polynomial` 0.201 → 0.083,
`interaction` 0.536 → 0.108, `nested` 0.718 → 0.388. That is the signature of
lexicase improving typical-case fit without crossing the exact-recovery
threshold, and it suggests recovery rate alone may under-report what lexicase
does. Not investigated here.

---

## Open questions

1. **`Sqrt` of a negative.** Not an error constructor, inherited from §5's
   list. `protectedSqrt` tells the same kind of lie `protectedLog` does, so
   the omission looks like an oversight in the original spec rather than a
   decision. Deferred rather than fixed, so this phase implements what was
   specified and reviewed; worth revisiting when differentiation needs
   `sqrt`'s domain condition to be checkable.

   **Closed, 2026-08-19.** Not an oversight after all — the right answer, for
   the reason given in `docs/phase5-pow-semantics.md`. `Sqrt` *means*
   `sqrt |x|`: total, continuous, differentiable away from 0. There is no
   domain error to report and no constructor to add.

   The comparison to `protectedLog` in the original wording is what was
   wrong. `log|x|` has a genuine pole at 0 — unbounded, not merely undefined
   — which no reinterpretation removes, so `LogOfNonPositive` stays a real
   domain condition. `sqrt|x|` is finite and continuous there. The two
   protections look alike and the functions do not.
2. **Fraction versus severity.** The penalty counts *points*, so an
   expression barely out of domain and one catastrophically out of domain
   score the same. Severity weighting is possible but needs a scale, and
   there is no obvious one that is not arbitrary.
3. **Interaction with `Saturated`.** Saturation is much more common than the
   genuine domain errors — `exp(exp(x))` saturates almost everywhere — so a
   single penalty weight may end up dominated by it, measuring "does this
   overflow" rather than "is this meaningful". If the non-zero-weight
   benchmark shows nothing, splitting the weight is the first thing to try.

   **Promoted to leading suspect, 2026-08-19.** The benchmark showed nothing:
   pooled 32.5% → 25% at weight 0.1, z = 0.74, p = 0.46, with both problems
   moving down. Before anyone tunes the weight, the measurement to run is a
   *breakdown by constructor* — what share of flagged points are `Saturated`
   versus the five real domain errors. If saturation dominates, a single
   weight cannot express the intended pressure and the null says nothing
   about whether penalising genuine violations would work.

   **Run, and the hypothesis was wrong, 2026-08-19.** `typedgp-bench
   --domain-breakdown`, population 500, 100 training points, all nine
   families. Shares are of *invalid* evaluations.

   | problem | sample | invalid% | div0 | log≤0 | pow<0 | gamma | zeta | saturated |
   |---|---|---|---|---|---|---|---|---|
   | polynomial | gen0 | 18% | 7% | 30% | **60%** | 0% | 0% | 2% |
   | polynomial | front | 0% | – | – | – | – | – | – |
   | rational | gen0 | 15% | 9% | 25% | **58%** | 0% | 0% | 7% |
   | rational | front | 0% | – | – | – | – | – | – |
   | power-law | gen0 | 11% | 15% | 21% | **54%** | 0% | 0% | 10% |
   | power-law | front | 0% | – | – | – | – | – | – |
   | exponential | gen0 | 16% | 9% | 29% | **60%** | 0% | 0% | 3% |
   | exponential | front | 15% | 0% | 0% | **100%** | 0% | 0% | 0% |
   | logarithmic | gen0 | 11% | 14% | 18% | **52%** | 0% | 0% | 16% |
   | logarithmic | front | 1% | 0% | 100% | 0% | 0% | 0% | 0% |
   | trigonometric | gen0 | 17% | 11% | 26% | **61%** | 0% | 1% | 1% |
   | trigonometric | front | 0% | – | – | – | – | – | – |
   | mixed | gen0 | 20% | 1% | 37% | **61%** | 1% | 0% | 1% |
   | mixed | front | **68%** | 0% | 0% | **100%** | 0% | 0% | 0% |
   | nested | gen0 | 18% | 7% | 30% | **61%** | 0% | 0% | 1% |
   | nested | front | **75%** | 0% | 0% | **100%** | 0% | 0% | 0% |
   | interaction | gen0 | 21% | 2% | 38% | **59%** | 1% | 0% | 1% |
   | interaction | front | 0% | – | – | – | – | – | – |

   `Saturated` is 1–16% of generation zero and **0% of every surviving
   front**. It dominates nothing. The suspicion is dead and splitting the
   weight would have been a fix for a problem that does not exist.

   What actually dominates is `PowOfNegativeBase`: ~60% of generation zero
   everywhere, and **100% of the invalid evaluations among final-front
   individuals** on `exponential`, `mixed` and `nested`.

   ### The finding that matters more than the one being looked for

   On `nested`, **75% of the final Pareto front's evaluations are out of
   domain**, and on `mixed`, 68%. These are the individuals that survived
   selection and get reported as discoveries. This is precisely the scenario
   §5 was written about — "an individual scoring well *because* it divides by
   zero on 40% of the data" — arriving through `Pow` rather than `Div`, at
   nearly twice the rate used as the illustrative worst case.

   **But `PowOfNegativeBase` is conflating two different things,** and that
   is the real conclusion. `protectedPow` computes `|base| ** expo` for a
   negative base, which is not nonsense — it is a smooth, even-symmetric,
   perfectly well-behaved real function. The search has very likely found
   that `|x|^c` fits, which is a legitimate discovery rendered in misleading
   notation, rather than exploiting an undefined region. The constructor
   currently cannot tell those apart:

   - `x^0.5` where `x < 0` and the fit *depends* on the fallback — genuinely
     undefined, and the derivative does not exist.
   - `|x|^0.5` in all but name — a real function with a real derivative
     everywhere except 0.

   ### Consequence for Phase 5

   This changes what differentiation should do, which is exactly why it was
   worth measuring before building on the type rather than after:

   1. **Do not gate differentiability on `isValid`.** On `nested` that would
      refuse to differentiate three quarters of the front, most of it
      wrongly.
   2. **`Saturated` must be excluded from any Phase 5 validity condition.**
      It is not a domain error at all — the expression is fine, the number
      got big. Grouping it with the genuine five was a taxonomy mistake
      inherited from §5's list, and the data says it costs nothing to fix
      because it is rare among survivors.
   3. **`PowOfNegativeBase` needs splitting or reinterpreting** before a
      derivative rule can consume it. The honest reading of `Pow` with a
      negative base is that the engine's operator *is* `|base| ** expo`, in
      which case the correct derivative is that of the even function and
      almost nothing is undefined. Deciding that is Phase 5 design work, not
      something to settle here.

---

## Validation result, 2026-08-19

**Reproducibility: pass.** At the default weight of 0, `trigonometric`,
`mixed` and `nested` returned identical recovery, in-domain error,
extrapolation error *and* median size to their Phase 2 values across all
twelve columns. The short-circuit works and the earlier phases' numbers
survive unchanged.

**Penalty at 0.1: null, with a negative point estimate.** See the README
table. `rational` 5% → 0% and `logarithmic` 60% → 50%, pooled z = 0.74,
p = 0.46. Not significant, so the claim is "no measurable effect" rather than
"it hurts" — but both problems moved the same way and the sign is recorded
rather than omitted.

The `rational` row is one recovery against zero, which is below the count at
which the normal approximation behind a two-proportion z-test is meaningful.
It is reported for completeness, not as evidence.
