# Phase 6 — binders: `Sum`

§2 of `docs/phase2-design.md` (and its 2026-09-04 amendment) covers the
capture problem, the sealing mitigation, and the four surfaces that must be
updated together. This note does **not** repeat any of that. It records the
decisions §2 leaves open, and the scope of this increment.

---

## Scope: `Sum` only

§2's recommended order pairs `Sum` and `Product`, with `Integral` after. This
increment does **`Sum` alone**.

`Sum` and `Product` are structurally identical — same shape, different fold —
so building both costs little extra code and doubles the test surface for the
same amount of learning. `Sum` is the minimal thing that exercises every gap
the amendment lists exactly once: capture, sealing, the checkpoint format
change, the differentiation shadowing bug, and the cost cap. If it lands
cleanly, `Product` is a near-copy against a proven design; if it does not, the
mistake is found once instead of twice.

`Integral` is genuinely different — continuous, Simpson's rule, its own
accuracy question — and is not a copy of anything here.

## Decision 1: the whole node is sealed, not just the body

§2 says "binder bodies are sealed", then describes the intent as "a `Sum` node
is atomic to the genetic operators". Those are not the same rule, and the
difference matters.

`Sum i lo hi body` evaluates `lo` and `hi` in the **enclosing** scope and
`body` in the extended one. So grafting into `lo`/`hi` carries no capture
risk — but it carries a **cost** risk that is just as bad: crossover could
drop an arbitrary subtree into a bound, and an evolved bound is exactly the
`-1e12 .. 1e12` case §3 warns about. It could also place a `Sum` inside a
bound, creating nesting the depth limit was meant to prevent.

**`sealedIndices` therefore covers every index strictly inside a `Sum` node,
bounds included.** The node is atomic, as §2's prose intends; only its own
index is a legal operator target.

## Decision 2: bounds are rounded, and an empty range is zero

`eval` produces `Double`s, and a sum needs integers.

- **Rounded to nearest**, not truncated. Truncation makes `2.9999` and `3.0`
  behave differently for what is almost always the same intended bound, and
  those two are indistinguishable after any floating-point arithmetic.
- **`lo > hi` is the empty sum, which is `0`.** The standard convention, and
  the one that makes `Sum i 1 0 body` harmless rather than a special case.
- Bounds are sanitised first, so `±magnitudeCap` bounds become a finite (if
  enormous) range and are then handled by the cap below rather than by
  overflow.

## Decision 3: exceeding the iteration cap is a domain error, not a silent truncation

§3 specifies `cfgMaxIterations` (default 1000) as "a hard cap on Sum/Product
terms". It does not say what the cap *does*, and the obvious reading —
evaluate the first 1000 terms — silently returns a number that is not the sum
of the expression as written. That is precisely the class of lie Phase 3
exists to make visible.

So the cap follows the house protection pattern exactly:

- `eval` evaluates the first `cfgMaxIterations` terms and returns that,
  staying total and finite as it must.
- `evalDomain` reports **`IterationCapped`**, a new `DomainError`
  constructor, so anything asking "is this meaningful here" can tell.

`DomainError` is not serialised, so this costs no checkpoint work. It does
force a case in `DomainStats`, whose `breakdownCount` is exhaustive — that is
a compile error, which is the intended behaviour.

`IterationCapped` sits with the five genuine domain conditions rather than
with `Saturated`: a capped sum is not the value of the expression at all,
whereas a saturated one is the right value, truncated. That distinction is the
§5 amendment's, applied to a new case.

## Decision 4: off by default, so every recorded number survives

> **Superseded by what was actually built — see the Result section.**
> Generation was not implemented in this increment, so there is no code path
> for a flag to gate and `cfgEnableBinders` was not added. The property this
> decision exists to guarantee holds anyway, and more strongly: nothing can
> generate a binder at all. The flag arrives with generation.

`cfgEnableBinders` (default `False`) gates **generation only**. When off,
`Gen/Grow` never emits a `Sum`, so no run that does not ask for binders can
contain one, and every benchmark number recorded before this change stays
bit-for-bit reproducible.

`eval`, `Pretty`, `Checkpoint` and the operators handle `Sum` unconditionally
whether or not the flag is set. Gating *evaluation* would make a
hand-written or checkpoint-loaded expression behave differently from a
generated one, which is a worse failure than not generating them.

## Decision 5: cost accounting counts the node, not the iterations

`countNodes` and `depth` treat `Sum` as one node with three children. They
measure *structure*, and every consumer of them — parsimony, the depth cap,
crossover site selection — is asking a structural question.

This does mean parsimony under-charges a `Sum`: a five-node body iterated a
thousand times costs far more to evaluate than five nodes elsewhere. That is a
real gap and it is deliberate for this increment, because the alternative is
making `countNodes` data-dependent, which would make tree size depend on the
values of the bounds. `cfgMaxIterations` is the defence against runaway cost;
parsimony is not.

---

## Validation

1. **Reproducibility, as a correctness assertion:** with `cfgEnableBinders`
   off, the benchmark suite must return numbers identical to those recorded
   now. The flag defaults off, so this is the claim that the constructor's
   mere existence changed nothing.
2. **The four sealing tests from §2's amendment**, including the non-vacuity
   guard and the `sealedIndices = const []` mutation check.
3. **Checkpoint round-trip** for an expression containing a `Sum`, with a
   format-version bump.
4. **Differentiation**, including the shadowing case: `d/di` inside `Sum i`
   must refuse rather than treat the bound index as free.

No benchmark claim is made for this phase. Binders are an expressiveness
change, and whether they help is a separate question that needs problems whose
truth actually contains a summation — which this suite does not have.

---

## Result, 2026-09-04

`Sum` implemented, sealed, and tested. 767 assertions green, zero warnings,
40 of them new in `test/BinderSpec.hs`.

### The mutation check the §2 amendment demanded

Neutering `sealedIndices` to `const []`:

```
[FAIL] sealedIndices is non-empty for it
[FAIL] both bounds and the body are sealed
[FAIL] no operator ever reaches inside a binder
764 / 767 assertions passed
3 FAILED
```

The third is the one that matters. It is the assertion that runs crossover,
subtree mutation, point mutation and hoist over 60 seeds against a tree
containing a binder and checks that every surviving binder is structurally
unchanged. Its failing under the mutation is what distinguishes a mitigation
that is wired into site selection from one that merely exists.

The non-vacuity guard (`sealedIndices is non-empty`) and the counterpart
guard on the operator assertion (`some operator output still contains the
binder`) are both present, so neither can pass by the binder having been
destroyed or never having been there.

### What `-Wall` caught that reading did not

Eleven sites across nine modules. Two of them — `LocalSearch` and
`Ops/Mutation` — had been cleared as safe by the hand audit in the §2
amendment, on the grounds that their catch-alls were on tuples rather than on
`Expr`. That was true of the catch-alls and false of the modules: both also
contain exhaustive `Expr` matches, which the audit missed and the compiler
did not.

Worth recording as a limit on that kind of audit: grepping for catch-alls
finds the places a new constructor is handled *silently*, not the places it is
handled *at all*.

### Scope actually delivered

`Sum` only, as planned. Generation is **not** implemented — nothing in
`Gen/Grow` emits a binder, and there is an assertion pinning that across 200
random trees. So a binder can currently enter a run only by being hand-built
or loaded from a checkpoint.

That makes decision 4's `cfgEnableBinders` unnecessary for now and it is not
added: a flag gating a code path that does not exist would be exactly the kind
of inert machinery this project has been trimming. It goes in with generation.

The reproducibility claim consequently holds by construction rather than by
measurement — no run that does not hand-build a binder can contain one — and
is additionally pinned by the assertion that `unsealedIndices` equals
`[0 .. countNodes - 1]` on any binder-free tree, which is what makes site
selection consume identical randomness to the pre-binder code.

### Reproducibility, verified

`--problems polynomial,rational,trigonometric --seeds 8`, the same command
run before the constructor existed:

| problem | recorded | after |
|---|---|---|
| `polynomial` | 62% / 38% | 62% / 38% |
| `rational` | 0% / 0% | 0% / 0% |
| `trigonometric` | 100% / 100% | 100% / 100% |

Identical on every column, error and size figures included.

That is not luck. Site selection changed from `nextInt (countNodes parent)` to
`pick (unsealedIndices parent)`, and `pick` draws `nextInt (length xs)` — so
on a binder-free tree, where `unsealedIndices` is exactly
`[0 .. countNodes - 1]`, the draw is the same value and maps to the same
index. The fast path in `unsealedIndices` exists to make that identity
obvious rather than argued, and `BinderSpec` asserts it directly.

Contrast with the `Pow` semantics change, which was also predicted to be
reproducible and was not: there the prediction rested on a measure-zero
argument that computed values falsified. Here it rests on the two code paths
being the same list, which is checkable.

---

## Generation, 2026-09-04

Implemented behind `cfgEnableBinders` (default `False`), with
`cfgBinderProb` (0.1), `cfgBinderMaxTerms` (10) and `cfgBinderDepthLimit`
(1). CLI flags in both binaries: `--binders`, `--binder-prob`,
`--binder-terms`, `--binder-depth`.

778 assertions green, zero warnings.

### How the reproducibility guarantee is preserved

Two places consume randomness differently once binders exist, and both are
arranged so a disabled run is bit-for-bit unchanged:

1. **The binder branch is taken before any operator draw, and only when the
   flag is set.** With it off, `binderAllowed` is `False` without consuming
   anything, and the first draw is still the operator pick.
2. **`genTerminalIn` draws from `cfgVariables ++ scope`.** With an empty
   scope that is `cfgVariables` in the same order, so the same draw maps to
   the same name.

Asserted directly (`disabling binders reproduces generation exactly`, 200
trees) and confirmed end to end: the 8-seed suite returns 62%/38%, 0%/0%,
100%/100% — identical to the recorded values.

### Bounds are literal, and why that is not laziness

Generated bounds are `Const 1` and `Const k`, never subtrees. An evolved
bound is §3's runaway-iteration case, and since the node is sealed, a bound
generated as a subtree could never be repaired by the operators either.

The bounds are still `Const` nodes, so `optimiseConstants` can retune them
numerically even though the operators cannot restructure them. That is the
one route by which a bound can grow, and `iterationCap` catches it with
`IterationCapped` rather than silent truncation.

### A pre-existing bug the binder work exposed

The benchmark's false-discovery rate used `variablesOf`, which counts every
`Var` occurrence — **including a binder's own bound index**. So every
formula containing a `Sum` was scored as referencing an irrelevant variable,
producing **25% FDR on `polynomial`, whose only variable is relevant**.

Fixed to `freeVariables`; the same run then reports 0% FDR with every other
column unchanged.

Worth noting what kind of bug this is. The two functions were
interchangeable until one of them could bind something, so this was latent
and unobservable rather than introduced. It is the third place the
free-versus-mentioned distinction has mattered, after `isFoldable` (guarded
by a comment) and `differentiate`'s shadowing check (guarded by a test) —
which suggests treating any remaining `variablesOf` call site as suspect
until checked.

### Not measured

Whether binders help. `--binders on` on `polynomial` gives 25% recovery
against 62% off, at 3× the wall clock — which is the expected result of
handing a search a construct the target does not contain, and is not
evidence about binders. A real answer needs a problem whose truth is a
summation, and the suite has none.
