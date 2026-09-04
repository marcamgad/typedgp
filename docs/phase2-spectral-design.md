# Phase 2 design note: FFT-seeded initialisation for periodic structure

**Status: proposal, written before implementation.**

Not to be confused with `docs/phase2-design.md`, which is the *mathematical*
Phase 2 (differentiation, binders). This is the search-algorithm Phase 2.

## The problem, already diagnosed

`trigonometric` (`2 sin(3x + 0.5)`) is the suite's second-worst family at 35%
recovery, and the cause is understood rather than mysterious. From
`LocalSearchSpec`, established by two tests that isolate it:

- started at frequency 1, the simplex **does not** recover frequency 3;
- started at frequency 2.7, the **same optimiser** recovers all three constants
  exactly.

So the optimiser is not the weak link. Frequency estimation is *multimodal* —
the error surface has a local minimum near every frequency — and a local method
cannot cross the barriers between basins however good it is. The fix has to put
the search in the right basin to begin with, which means initialisation.

## Recommendation

A **naive O(n²) DFT** of the detrended targets against each variable, computed
**once per run**, whose top peaks bias the constant drawn for `c` when
generating `sin(c*v + d)` / `cos(c*v + d)`. Falls back to today's exact
behaviour when no peak stands out.

## Why a DFT at all, and which one

| approach | finds the basin? | cost | new code | notes |
|---|---|---|---|---|
| more restarts of the simplex | no | linear in restarts | none | restarts sample the *same* basin structure; the barrier is still there |
| random multi-start over frequency | sometimes | high | small | unguided; wastes most starts, and the useful range is unbounded |
| autocorrelation peak | yes, roughly | O(n²) | small | needs evenly spaced samples, which the suite does not have |
| **naive DFT** | **yes** | **O(n² ) once** | **~80 lines** | **chosen** |
| radix-2 Cooley–Tukey | yes | O(n log n) once | ~150 lines | premature: at n = 100 the naive form is microseconds |

Cost is a non-issue and that decides it. The DFT runs **once per run**, not per
generation or per individual, against datasets of 60–400 points. At n = 400
that is 160,000 multiply-adds — comparable to *one* fitness evaluation of one
individual, against a run that performs millions. Cooley–Tukey would be
optimising something that does not appear in the profile, and it would need
power-of-two padding, which introduces its own spectral artefacts.

### The sampling problem, which is real

A DFT assumes evenly spaced samples. The benchmark draws inputs **uniformly at
random** from a range, so they are not evenly spaced, and this is the main
threat to the whole approach.

Mitigation: evaluate the transform directly as a sum over the actual sample
points rather than assuming a grid —

```
S(f) = Σ_k  y_k · exp(-2πi f x_k)
```

— which is the Lomb–Scargle-adjacent form and is well-defined for arbitrary
`x_k`. It costs the same as the gridded version and removes the assumption
entirely. What it does *not* remove is that irregular sampling raises the noise
floor of the spectrum, which is precisely what the signal-threshold check
below is for.

### Detrending

A linear trend puts a large spurious peak at the lowest frequencies that can
swamp a real one. Two-line closed-form OLS on `(x, y)`, subtract, then
transform. `Dataset.hs` has no regression code to reuse, so this lives in the
new module.

## What gets biased, and how much

Only the multiplier on the variable inside a trigonometric call. Concretely,
when `Gen/Grow.hs` builds a constant that will be used as `c` in `sin(c*v + d)`,
it draws from the detected frequencies with probability
`cfgFrequencySeedWeight`, and from today's uniform range otherwise.

**The weight must not be 1.0**, and the reason is not caution for its own sake:
a detected peak can be *wrong*. Irregular sampling, harmonics of the true
frequency, and noise can all produce a confident-looking peak at the wrong
place. Reserving mass for the uniform draw means a wrong detection costs
efficiency rather than making the true frequency unreachable — the failure mode
degrades instead of becoming absolute.

**Proposed default: 0.5.** Justification rather than a guess: the informative
comparison is against the status quo, where the chance of drawing a constant
within ±0.05 of a specific frequency from a uniform `(-5, 5)` range is about
1%. At 0.5 the seeded frequency becomes roughly a coin flip per trigonometric
node, which is a ~50× improvement in the thing that was failing, while half of
all draws remain unbiased and can still find a frequency the detector missed.
Going to 0.8 or 0.9 would buy little on top of that — the bottleneck is already
gone at 0.5 — while making a wrong detection much more expensive. This number
should be re-measured, not trusted.

**`cfgFrequencySignalThreshold`, proposed 3.0.** The ratio of the dominant peak
to the *median* spectral magnitude. Median rather than mean, because the mean is
dragged up by the peak being tested and would make the test partly
self-referential. Below the threshold the detector reports no signal and
generation is bit-for-bit today's behaviour — which is testable directly, and
will be tested.

**`cfgFrequencySeedTopK`, proposed 3.** A true frequency can land second behind
a harmonic or a sampling artefact, so taking only the argmax is fragile. Three
is enough to be robust to that without diluting the bias.

## What this must not touch

`eval`, `Simplify.hs`, and every genetic operator. The entire change is a
generation-time bias, which is what makes it carry zero determinism or
capture risk: a differently-initialised population is still a valid population,
and every operator downstream is unchanged.

The frequency table is computed once in `Evolution.hs`'s setup and threaded
into `Gen/Grow.hs`. It must **not** be recomputed per generation — not for
cost, but because it is a property of the *dataset*, and recomputing it would
invite someone to later make it depend on the population, which would couple
initialisation to search state and break the isolation above.

## Validation plan

Per the brief, and with two additions of my own:

- `trigonometric` and `mixed` at 20 seeds, seeding on vs off, pooled recovery
  with z-score.
- **`nested` as a canary.** Constant refinement regressed it by narrowing
  diversity, and frequency seeding narrows the initial distribution in a
  similar way. If `nested` drops, the mechanism is suspect regardless of what
  `trigonometric` does.
- **Baseline against `tournament`, not lexicase.** Phase 1 leaves selection a
  free variable; running this against lexicase would move two mechanisms on the
  one family where it matters most, which is the exact confound Phase 1 spent
  its ablation disentangling.
- **Bonferroni**: whatever the family count in the final table, α is corrected
  against it and stated.

## Open questions

1. **Harmonics.** A true frequency `f` produces peaks at `2f`, `3f`. Top-k will
   often contain a harmonic rather than a second genuine frequency, which
   wastes bias mass. Detecting and collapsing harmonics is possible but adds a
   heuristic; not proposed for a first cut, and worth checking whether it shows
   up in the `mixed` result.
2. **Multivariate targets.** The transform is computed per variable against the
   raw target. For `2x² + exp(-x) + sin(y)` the `x` terms are a large
   non-periodic component that will dominate the spectrum computed against
   `y`. Detrending removes only the *linear* part of that. This is the most
   likely reason for `mixed` to see no benefit even if `trigonometric` does.

   **Sharpened, 2026-08-18, before the validation run.** This is no longer a
   speculation. `EvolutionSpec`'s benchmark target is `2x + sin(y)` — the same
   shape, one degree milder — and on it the detector reports *no signal at all*
   for `y`. Seeding is bit-for-bit inert there, and there is now an assertion
   pinning that (`"the detector finds no usable signal in 2x + sin(y)"`). The
   arithmetic is unkind: over `x ∈ [-3,3]` the `2x` term has standard deviation
   ≈ 3.5 against `sin(y)`'s amplitude of 1, so viewed from `y` the signal sits
   under noise three and a half times its size, and detrending cannot touch it
   because `2x` is not linear *in `y`*. `mixed` replaces `2x` with
   `2x² + exp(-x)`, which is larger still.

   So the prediction is not merely "no benefit on `mixed`" but the stronger,
   more falsifiable **"the detector will report no frequencies for `y` on
   `mixed`, and the on/off arms will be statistically indistinguishable."**
   If `mixed` *does* move, this reasoning is wrong somewhere and that is the
   more interesting outcome.

   **Answered, 2026-08-19 — and deferred rather than queued.** Half right.
   The detector does miss `sin(y)` exactly as argued, and `mixed` showed no
   significant movement (15% → 25%, z = 0.79, p = 0.43, n = 20 per arm). But
   the *strong* form was wrong: the arms were not identical, because the
   detector fires **spuriously on `x`**. `2x² + exp(-x)` has no periodicity,
   yet it is not flat after linear detrending either, and its residual clears
   the peak-to-median threshold. Seeding then biases trig arguments towards a
   frequency the target does not contain. Both facts are now asserted in
   `test/SpectralSpec.hs`.

   The correct fix is known: compute the spectrum against the *residual* left
   after removing what the other variables explain, rather than against the
   raw target. Raising the threshold is explicitly the wrong fix — it would
   suppress the spurious `x` peak and the genuine `trigonometric` peaks
   together, and `trigonometric` is where the whole effect lives.

   **It is not being built, and is not a queued phase.** The regression it
   would fix was not measurably harmful: `mixed` moved in the *right*
   direction, non-significantly, while doing the wrong thing internally. There
   is no evidence yet that a correct multivariate spectrum is worth the
   machinery, and building it now would be speculative work justified by a
   mechanism story rather than by a number. This stays here as a known,
   diagnosed limitation until something downstream actually needs it — a
   problem where the spurious peak measurably hurts, or a later phase that
   depends on per-variable structure being trustworthy.
3. **Does it help at all?** `trigonometric` failing is *consistent with* bad
   initialisation but has not been proven to be caused by it. If seeding lands
   the right basin and recovery still does not move, the diagnosis was wrong
   and that is the finding.

   **Answered, 2026-08-19.** It helps, and the diagnosis was right.
   `trigonometric` went 35% → 100% across 20 seeds (z = 4.39, p ≈ 0.00001,
   clearing the Bonferroni threshold of 0.05/3 for the three problems tested).
   The failure mode was never structural; it was that a real-valued multiplier
   inside `sin(cx + d)` had to be reached by drifting a constant.
4. **Combined lexicase + seeding ablation on `trigonometric`.**
   **Closed without running, 2026-08-19.** This was going to be the Phase 2.5
   check that the two independent search modifications compose. It is not
   worth 20 seeds: seeding alone takes `trigonometric` to 20/20. There is no
   headroom above a ceiling, so the comparison could only confirm a ceiling
   effect that arithmetic already states. Recorded here as closed rather than
   deferred so it does not sit as a phantom open item.

   Note this closes the question *on `trigonometric` specifically*. Whether
   the toggles interact badly in general is a different question, and is
   covered by the stacked smoke test in Phase 3's validation.
