# What does `Pow` mean when the base is negative?

Written before any differentiation code, because the answer determines the
derivative rule rather than following from it. Deciding this inside
`differentiate`'s pattern match on `Pow` would be settling a semantic
question by momentum.

---

## Why this is now the load-bearing question

`typedgp-bench --domain-breakdown` (full table in
`docs/phase3-domain-design.md`) found that `PowOfNegativeBase` is **100% of
the invalid evaluations among final-front individuals** on `exponential`,
`mixed` and `nested` — and on `nested`, 75% of the front's evaluations are
flagged at all. The search is not occasionally straying into this case. It is
living there.

Two real examples, pulled from actual fronts:

| as rendered before this work | what `protectedPow` computes |
|---|---|
| `sin(x) ^ 1.1114` | `\|sin x\| ^ 1.1114` — a rectified sine |
| `(-0.6219) ^ y` | `0.6219 ^ y` at non-integer `y`, sign-flipped at integer `y` |

The first is a legitimate shape in misleading notation. The second is
something worse, and it is the finding that most affects the decision below.

## The discontinuity nobody designed

`protectedPow` currently does two different things depending on the exponent:

```haskell
| base > 0             = base ** expo
| isIntegerValued expo = negativeBaseSign expo * (abs base ** expo)  -- keeps the sign
| otherwise            = abs base ** expo                            -- drops the sign
```

For a negative base, the sign of the result therefore **flips depending on
whether the exponent happens to land exactly on an integer**. `(-0.6) ^ y` is
`+0.36` at `y = 2`, `-0.216` at `y = 3`, and `+|0.6|^2.5` at `y = 2.5`. The
function is discontinuous at every integer.

This was not a design decision. It is what falls out of handling two cases
separately, each of which is locally defensible. On continuously-distributed
data the integer points have measure zero, so the search almost never sees
them and the operator behaves like `|base| ** expo` — which is exactly why it
went unnoticed until the breakdown forced a look.

A discontinuous, nowhere-differentiable-at-integers operator is not something
to build a differentiator on top of. This has to be settled either way.

---

## The two honest options

### (a) Reinterpret — `Pow` means `|base| ** expo`

Change what the operator *is*: for any base, the result is
`sign-free`, `|base| ** expo`. The negative-base case stops being an error
because there was never an error, only an underspecified operator.

- Derivative: `d/dx |u|^c = c * |u|^(c-1) * sign(u) * u'`, defined
  everywhere except `u = 0`.
- `PowOfNegativeBase` disappears as a domain error entirely.
- The operator becomes **continuous and total**, which the current one is not.

### (b) Split — `Pow` means ordinary real exponentiation

Keep the textbook meaning. A negative base with a non-integer exponent is
genuinely undefined, treated like `1/x` at `x = 0`: no value, no derivative,
a real domain restriction.

- Derivative: the standard rule, guarded by `base > 0` (or an integer
  exponent).
- `PowOfNegativeBase` stays and becomes load-bearing.
- Requires **also** fixing the discontinuity above, since "ordinary real
  exponentiation" cannot mean two different things either side of an integer.

## Comparison

| | (a) reinterpret as `\|base\|^expo` | (b) split, keep real exponentiation |
|---|---|---|
| honesty about current behaviour | matches what the engine already does on continuous data | matches the notation, not the implementation |
| continuity | total and continuous everywhere | undefined on a 2-D region; discontinuity must be removed separately |
| differentiability | everywhere except `u = 0` | only where `base > 0` or the exponent is an integer |
| effect on the search | none — same numbers, so Phase 1–3 benchmarks stay reproducible | large: ~75% of `nested`'s front becomes non-differentiable and, if penalised, unviable |
| effect on `Pretty` | the `abs` rendering just added becomes the *definition*, not a hint | rendering must instead flag "undefined here" |
| loses anything? | yes — `(-2)^3 = -8` is no longer expressible | no |
| risk | quietly redefines an operator's meaning | quietly invalidates most of what the search has found |

## Recommendation: (a), with one reservation stated

**Take (a).** Three reasons, in order of weight:

1. **It is what the engine already computes**, on every dataset that is not
   contrived to hit exact integers. (a) is a change to the *documentation and
   the derivative rule*; (b) is a change to the *search*. Phase 3 established
   that the default path must stay bit-for-bit reproducible, and (a) keeps
   that for free while (b) cannot.
2. **It removes a real discontinuity** rather than requiring a separate fix
   for it. Under (a) the integer/non-integer split in `protectedPow` collapses
   into one branch and the operator becomes total and continuous.
3. **The search has already voted.** `|x|^c` shapes are 75% of `nested`'s
   front and they *fit*. Declaring the engine's most productive operator
   mostly-undefined, on the strength of notation rather than behaviour, would
   be choosing a convention over evidence.

**A fourth reason, added 2026-08-19 after implementing differentiation.** Not
available when the decision was taken, and it would have mattered:

Under (b), the general power rule `d/dx f^g = f^g (g' log f + g f'/f)` needs
`log f` with `f` possibly negative, which is why §1 of `phase2-design.md`
specified `Nothing` for the fully-symbolic case. Under (a) that restriction
evaporates, because **`Log` in this AST is already `log|x|`** — precisely the
factor the derivation requires. The constant-base case loosens from `a > 0` to
`a ≠ 0` for the same reason.

And the constant-exponent rule turns out to need no `signum` operator either,
since `|f|^(c-1) · sign(f) = f · |f|^(c-2)`, with `Pow` supplying `|f|^(c-2)`
directly.

So (a) does not merely remove a discontinuity — it makes the general power
rule **expressible at all** in the AST's existing operators, with no new
constructor and no case split on a sign the AST cannot represent. The `f > 0`
condition that forced a `Nothing` was an artefact of pairing an unprotected
`log` with a protected `Pow`, not a fact about exponentiation.

This is recorded here rather than left as an incidental convenience because it
is evidence the recommendation was better founded than the argument made for
it — the note's job is to hold what is known and why, including what arrived
after the decision.

**The reservation, which is real:** (a) genuinely loses `(-2)^3 = -8`. An odd
integer power of a negative base is well defined, useful, and expressible
today. Under (a) it silently becomes `+8`.

That cost is smaller than it looks, because the operator set already has the
alternative: an odd integer power is `Mul x (Mul x x)`, which the search
reaches easily and which is exact, continuous and differentiable. What is lost
is a *compact spelling*, not an expressible function. Set against a
discontinuity at every integer, that is a good trade — but it is a trade, and
if a future benchmark shows recovery falling on a problem with a genuine odd
power, this is the first thing to suspect.

## Consequences to implement

1. `protectedPow` collapses the two negative-base branches into
   `abs base ** expo`, dropping `negativeBaseSign` and its
   `isIntegerValued` call. **Done.**
2. `PowOfNegativeBase` is removed from the conditions `evalDomain` reports —
   the case is no longer an error. **The constructor stays** so the
   `DomainError` type and `CheckpointSpec` are undisturbed, but nothing
   produces it, and that must be asserted by a test rather than assumed.
   **Done**, with the assertion.
3. `Pretty` renders `|base| ^ expo` whenever the base is not provably
   non-negative, dropping the current restriction to literal non-integer
   exponents — under (a) the absolute value is the meaning, not a hint about
   one case. **Done.**
4. **Not anticipated, found by the test suite: the simplifier's `x^1 -> x`
   rule became unsound and had to be fixed.** Under (a), `x^1` is `|x|`, so
   at `x = -3` the rule turned `+3` into `-3`. It is now
   `if provablyNonNegative a then a else Abs a`.

   Worth recording as a warning rather than a footnote. `Simplify.hs`'s own
   comment had argued the rule sound *specifically* from the sign-keeping
   branch — "for a negative base the exponent is an integer, so the sign
   branch gives `-1 * |v| = v`" — so the reasoning was correct when written
   and silently invalidated by a change three modules away.

   **Correction:** an earlier version of this note, and the module header in
   `Simplify.hs`, credited the eval-preservation property test with catching
   this. It did not. Verified by mutation on 2026-08-19: restoring the
   unsound rule leaves "simplification preserves eval, exactly" **passing**
   over all 400 random trees. Two hand-written assertions caught it.

   The reason is structural and applies to every rule in the module. Each is
   guarded by an exact comparison against a specific constant, and the
   generator draws constants from a continuous range — so the random
   population can never trigger any of them. Several hundred random trees
   were proving almost nothing about the rules they appeared to cover.

   `SimplifySpec` now also checks a **critical population**: rule trigger
   shapes at every boundary constant, evaluated at environments placing
   variables on zero and on negatives, including the structurally-exact
   cases (`Pow x (Div x x)`) that pure constant enumeration misses.

   **Correction to the first version of this paragraph.** It claimed the
   population covered "every rule". It did not — `criticalExprs` only ever
   placed constants on the *right*, so the three commuted rules
   (`Add (Const 0) b`, `Mul (Const 1) b`, `Mul (Const 0) _`) had no trigger
   at all. Three of thirteen rules were uncovered while being described as
   covered. Fixed by adding a mirrored `binaryLeft` shape.

   To stop that recurring, coverage is now **asserted rather than claimed**:
   `ruleTriggers` lists one representative trigger per rule and an assertion
   checks that some member of the critical population actually reduces each
   one. A rule with no trigger is a test failure instead of a prose error.

   **Mutation transcript, 2026-08-19.** Restoring `Pow a (Const c) | c == 1
   -> a`:

   ```
   [FAIL] simplification preserves eval, to tolerance
   [FAIL] simplification preserves eval, exactly
   [FAIL] simplification preserves eval on every rule's trigger shape
   [FAIL] x^1 becomes abs(x), not x
   [FAIL] the x^1 rule is eval-preserving on a negative base
   631 / 636 assertions passed
   5 FAILED
   ```

   Five, not the three previously reported from memory. Three are property
   assertions — the two whole-population checks and the critical-population
   check — and two are the hand-written cases. Before the critical
   population existed, the same mutation failed only the two hand-written
   ones and every property assertion passed.

   The residual gap, stated: `ruleTriggers` is maintained by hand, so a rule
   added to `Simplify.hs` without an entry there is still outside the net.
5. The derivative rule for `Pow` is written against `|u|^c`. **Pending —
   Phase 5 implementation.**

## `Sqrt`, decided here rather than deferred

`Sqrt` of a negative (`docs/phase3-domain-design.md` open question 1) is the
same question in a different operator: `protectedSqrt` computes `sqrt |x|`.

This was initially left open on the grounds that it was not blocking. That was
wrong — Phase 5 needs a derivative rule for *every* operator, so writing
`Sqrt`'s rule forces the question regardless of whether this note claims it in
scope. Deferring it would only have meant deciding it implicitly inside a
pattern match, which is the exact failure this note exists to prevent.

**Decided consistently with (a): `Sqrt` means `sqrt |x|`.** Total, continuous,
non-negative everywhere, and differentiable away from `x = 0` with derivative
`sign(u) / (2 * sqrt|u|) * u'`. There is no `SqrtOfNegative` domain error and
there will not be one, for the same reason `PowOfNegativeBase` stopped being
produced: there is no error, only an operator whose definition was previously
left implicit in its protection code.

This closes open question 1 in `docs/phase3-domain-design.md`.

Note the asymmetry with `Log`, which is *not* being reinterpreted.
`protectedLog` also takes a magnitude, but `log|x|` has a genuine pole at
`x = 0` that no reinterpretation removes — the function is unbounded there,
not merely undefined — so `LogOfNonPositive` remains a real domain condition.
`|x|^c` and `sqrt|x|` are finite and continuous at 0; `log|x|` is not. The
distinction is about whether the reinterpreted function is well behaved, not
about which protection convention was used.

## What this does not decide

Nothing outstanding. Every operator whose protection takes a magnitude has now
been given an explicit meaning: `Pow` and `Sqrt` are reinterpreted, `Log`
keeps its domain restriction for the reason above.

## Validation

Because (a) claims to change nothing about what the engine computes on real
data, the check is a **correctness assertion, not a statistical one**: the
full nine-family suite at the seeds already recorded must return identical
numbers. Any movement means the negative-base case was being hit at exact
integers somewhere, which would itself be worth knowing.

### Result, 2026-08-19: the assertion failed, and reason 1 above is falsified

| problem | recorded | after (a) |
|---|---|---|
| `trigonometric` | 100%, 0.0000 / 0.0000, 12 | 95%, 0.0000 / 0.0000, 12 |
| `mixed` | 25%, 0.0708 / 0.3675, 25 | 25%, 0.0870 / 0.4938, 26 |
| `nested` | 55%, 0.0153 / 0.0630, 8 | 70%, 0.0043 / 0.0031, 8 |
| `rational` | 5%, 0.2122 / 4.3965, 7 | 0%, 0.2109 / 3.6436, 8 |
| `logarithmic` | 60%, 0.0333 / 4.6854, 18 | 60%, 0.0370 / 4.7275, 16 |

**Reason 1 of the recommendation — "keeps Phase 1–3 bit-for-bit
reproducible" — is simply false, and it was one of the three grounds the
decision was taken on.** It has to be marked wrong here rather than quietly
dropped.

### Why, measured rather than guessed

The argument for reason 1 was that an exponent landing exactly on an integer
has measure zero under continuous sampling. That is true of *generated
constants* and false of *computed* exponents: `protectedDiv x x` returns
exactly `1.0` by construction and `x - x` is exactly `0`, so `a ^ (x / x)` is
an odd integer power at **every** input rather than almost none.

`legacyPowExposure` measures how often the two definitions actually disagree
on a generation-zero population: **`mixed` 1%, the other four 0% to the
nearest whole percent.**

So the exposure is tiny, and that is the point. The search is a chaotic
dynamical system — one individual's fitness changing in generation zero
changes which parents are selected, after which the two runs are unrelated.
Reproducibility required *zero* divergence, and "measure zero under continuous
sampling" is a much weaker claim than zero. That gap is the whole error.

### Does the decision survive?

Yes, on two legs instead of three, and the middle leg is **strengthened by the
evidence that killed the first**:

- ~~Reason 1: preserves reproducibility.~~ **False.**
- Reason 2: removes a real discontinuity. **Stronger now.** The numbers moved
  because the sign-flipping branch was genuinely being exercised — the search
  really was riding a discontinuity, which is exactly the pathology (a)
  removes. A zero-exposure result would have made (a) cosmetic; a nonzero one
  makes it substantive.
- Reason 3: the search has already voted for `|x|^c` shapes. **Unchanged.**

### What the moved numbers do and do not show

They are **resampling noise, not evidence about (a)'s merits.** Pooled across
the five problems, recovery went 49/100 to 50/100 (z = 0.14, p = 0.89), and no
individual problem moved significantly — `nested`'s eye-catching 55% → 70% is
z = 0.98, p = 0.33, and the `trigonometric` and `rational` cells are both too
sparse for a z-test to mean anything. Reading `nested` as an improvement
caused by (a) would be exactly the overreach this project's rules exist to
stop.

The honest summary: (a) reseeded every trajectory, the suite landed in a
statistically indistinguishable place, and every benchmark number recorded
before this change is now a measurement of a slightly different engine.
