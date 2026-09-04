# Phase 2 design note: differentiation, summation, integration

**Status: proposal. Nothing here is implemented.** Phase 1 (the pointwise
special functions, and `Pow`) is merged and tested; this note is the thing to
argue with before any binder-node code exists.

It answers the four questions Phase 2 was gated on, plus a fifth that Phase 1
forced into the open.

---

## 0. What Phase 1 already taught us

Three findings from actually building the special functions changed my
recommendations below. Worth stating first, because they are evidence rather
than opinion.

**Protected operators are not differentiable, and that is not a detail.**
`protectedDiv` returns `1.0` when `|b| < 1e-9`. That is a step discontinuity:
the function jumps from `a/1e-9` to `1.0` at the boundary. `sanitize` adds two
more, clamping at `±1e12`. `protectedPow` adds a whole discontinuous *region* —
for negative bases it silently switches to `|base| ** expo`. So the function
`eval` actually computes is piecewise, with discontinuities whose locations
depend on the input data.

**`^` is used, but constants are the bottleneck.** On a `4 * x^1.7` power law,
150 generations at population 1000 across three seeds found `^`-bearing
structures but never the clean form. Handing the *correct* structure to the
existing compass search does not fix it either — the coupled constants defeat
coordinate descent (`UncertaintySpec` pins this). Structure search is not the
weak link on that problem.

**One class of site is not compiler-checked.** `Checkpoint`'s parser dispatches
on operator name strings. Phase 2's binder nodes will need parser support that
`-Wall` cannot verify, so `CheckpointSpec`'s registry-driven round-trip must be
extended to cover them.

---

## 1. Differentiation strategy

**Recommendation: symbolic differentiation, on a restricted "analytic subset"
of `Expr`, returning `Maybe Expr`.**

### The three options

| | Numeric (central difference) | Symbolic, total | Symbolic, partial (recommended) |
|---|---|---|---|
| Rule per constructor | no | yes | yes |
| Agrees with `eval` | yes, by construction | no | no, but *provably* so |
| Cost per fitness eval | 2 extra `eval`s per derivative | one-off, then free | one-off, then free |
| Gives a *second representation* | no | yes | yes |
| Behaviour at protected boundaries | tracks the discontinuity | silently wrong | refuses |

Numeric differentiation is the tempting answer, and it is what I would have
recommended before Phase 1. It needs no rule per constructor, it is trivially
consistent with `eval` being the single source of truth, and it cannot be
*wrong* — a central difference of a step function is a large number, which is
an honest report of a step.

I am recommending against it anyway, for one reason: **it does not produce an
expression.** Upgrade 2's real payoff is not the numbers `f'(x)`, it is having
`f'` as an `Expr` — something that can be simplified, printed, compared against
a second dataset of derivative observations, or checked for dimensional
consistency. A finite-difference routine gives you a function you can sample.
Symbolic differentiation gives you a *formula you can reason about*, and every
downstream upgrade in the roadmap (derivative-aware fitness, residual analysis,
dimensional analysis) wants the formula.

### Why `Maybe Expr`, and what the "analytic subset" is

Here is the thing to be honest about: **symbolic differentiation of this AST is
mathematically unsound**, because the chain rule does not apply to `eval`'s
protected operators. `d/dx (a/b)` is `(a'b - ab')/b²` only where the quotient
rule holds; `eval` computes something with a step in it, and the two disagree
near `b = 0` — which is precisely where evolved individuals congregate, because
the protection makes that region survivable.

Pretending otherwise would produce a `differentiate` that returns a
confident-looking expression which is simply wrong on part of the domain, with
nothing in the type or the output to say so. That is the same failure mode the
simplifier's eval-preservation property exists to prevent, and it should be
prevented the same way.

So:

```haskell
differentiate :: VarName -> Expr -> Maybe Expr
```

`Nothing` means "this expression is not differentiable by these rules" rather
than "the derivative is zero". Concretely:

- `Const`, `Var`, `Add`, `Sub`, `Mul` — always fine. No protection is involved;
  the textbook rules are exact.
- `Sin`, `Cos`, `Tanh`, `Exp` — always fine. Total, smooth, unprotected.
- `Div` — the quotient rule is returned, *and* the result is only valid where
  `|denominator| >= divisionEpsilon`. See §5 on how that is tracked.
- `Log`, `Sqrt` — `protectedLog` is `log |x|`, whose derivative is `1/x`
  everywhere except the guarded region around zero. Same treatment as `Div`.
- `Abs` — derivative `signum x`, undefined at `0`. Returned with the same
  caveat.
- `Pow` — see below.
- `Gamma`, `Zeta` — **`Nothing`.** Their derivatives are the digamma and the
  derivative of zeta, neither of which is in the AST, and approximating them
  numerically inside a symbolic differentiator would smuggle the numeric
  approach in through the back door with none of its honesty.

A `differentiate` that refuses on `zeta` is more useful than one that lies
about it, because the caller can then decide: skip this individual, fall back
to a finite difference for it, or exclude `zeta` from the operator set for runs
where derivatives matter.

### The `Pow` rule specifically

The general rule, as proposed:

```
d/dx f^g  =  f^g * ( g' * log f  +  g * f' / f )
```

This is correct **only for `f > 0`**, because it is derived by writing
`f^g = exp(g log f)`, and `log f` requires a positive base. That matters here
more than in a textbook, because `protectedPow` explicitly permits negative
bases.

Three cases, and the implementation should split them:

1. **`g` is a constant** (`Pow f (Const n)`) — by far the common case, and the
   one worth getting right. The power rule `n * f^(n-1) * f'` applies for *all*
   `f`, including negative and zero, when `n` is an integer. No `log` appears,
   so no domain restriction is introduced. Emit this whenever the exponent is a
   literal.
2. **`f` is a constant** (`Pow (Const a) g`) — reduces to
   `a^g * log a * g'`, valid for `a > 0`. For `a <= 0`, emit `Nothing`.
3. **Both symbolic** — the general rule, valid only where `f > 0`, which cannot
   be established statically. Emit the rule *with* an `f > 0` domain condition
   attached (§5), or `Nothing` if the domain machinery is not built.

Case 1 alone covers `x^2`, `x^3`, `x^0.5` and every `variable ^ constant` shape,
which is where the evidence says the value is.

> **Amendment, 2026-08-19 — all three cases change, and two get better.**
>
> Written when `Pow` kept the sign of a negative base for integer exponents.
> It now means `|base| ^ expo` for every base (`docs/phase5-pow-semantics.md`),
> which rewrites all three rules. The section above is left intact because its
> *reasoning* is the thing worth preserving; the rules it lands on are
> superseded by these.
>
> **Case 1 — constant exponent. The stated rule is wrong for odd exponents.**
> `n * f^(n-1) * f'` assumed `Pow f n = f^n`. Under `|f|^n` the derivative is
> `n * |f|^(n-1) * sign(f) * f'`, which differs from the stated rule whenever
> `n` is odd. The two agree for even `n`, which is why this is easy to miss.
>
> It is expressible without any `signum` operator, because
> `|f|^(n-1) * sign(f) = f * |f|^(n-2)`. So:
>
> ```
> d/dx Pow f (Const c)  =  c * f * Pow f (Const (c-2)) * f'
> ```
>
> — using `Pow` for `|f|^(c-2)`, which is exactly what it now means. Checks
> out at `c = 2` (`2f·f'`), `c = 1` (`sign(f)·f'`, the derivative of `|f|`),
> `c = 3` (`3f|f|·f'`) and `c = 0.5`. Valid for all `f` except `f = 0`, where
> `Pow f (Const (c-2))` hits the pole sentinel for `c < 2`.
>
> **Case 2 — constant base. The domain restriction loosens.** The old rule was
> valid only for `a > 0`, emitting `Nothing` otherwise. Since the operator now
> takes the magnitude, `|a|^g` is fine for every `a` except zero, and
> `log|a|` is precisely what `protectedLog` computes:
>
> ```
> d/dx Pow (Const a) g  =  Pow (Const a) g * Log (Const a) * g'      -- a /= 0
> ```
>
> **Case 3 — both symbolic. It stops being `Nothing`.** This is the one that
> genuinely improves. The general rule was rejected because it needs `log f`
> and `f` could be negative. But `Log` in this AST *is* `log|f|`, and
> `|f|^g = exp(g · log|f|)` holds for every `f /= 0`:
>
> ```
> d/dx Pow f g  =  Pow f g * (g' * Log f  +  g * f' / f)             -- f /= 0
> ```
>
> So reinterpreting `Pow` did not merely remove a discontinuity — it made the
> general power rule expressible in the AST's own operators, because the
> protected `Log` is the exact function the rule requires. The `f > 0`
> condition that motivated returning `Nothing` was an artefact of pairing an
> unprotected `log` with a protected `Pow`.
>
> **Where each rule is valid is not tracked separately.** The derivative is
> itself an `Expr`, so `evalDomain` applied to it reports exactly where it is
> meaningful — the `f = 0` conditions above surface as `DividedByZero` from
> the `Div` or the `Pow` pole. §5's domain machinery composes with this for
> free rather than needing a parallel mechanism. `Saturated` must be excluded
> when asking that question, per the §5 amendment above.

### Cost

Symbolic differentiation is a one-off per individual, not per data point, and
the result is an `Expr` that `simplify` immediately shrinks. That is
categorically cheaper than numeric differentiation's two extra `eval` calls per
point per derivative. It is only worth doing at all if something consumes the
result — do not compute derivatives for every individual every generation on
the off-chance.

---

## 2. Bound variables and scope

`Sum`, `Product` and `Integral` introduce an index that exists only inside their
own body. The AST needs:

```haskell
| Sum      VarName Expr Expr Expr   -- index, lower, upper, body
| Integral VarName Expr Expr Expr   -- variable, lower, upper, integrand
```

### `eval` is the easy part

`Env` is an association list and `lookup` takes the first match, so binding an
index is `((name, value) : env)` — shadowing falls out for free, with no
scope-tracking machinery. The body is evaluated once per iteration with a
different binding.

### The genetic operators are the hard part, and they are currently unsafe

There is no type system yet, and `Ops/Crossover.hs`, `Ops/Mutation.hs` and
`Ops/Hoist.hs` all address nodes by a flat pre-order index over the whole tree.
Nothing stops:

- **Escape** — crossover lifting a subtree that mentions `i` out of a `Sum`
  body and into a context where `i` is unbound. It will not crash (`eval`
  reads unbound variables as `0`), which is worse: it silently becomes a
  different, usually nonsensical, expression.
- **Capture** — grafting a foreign subtree that happens to mention `i` *into*
  a `Sum` body, where it is suddenly captured by the binder and means something
  entirely different from what it meant in the donor.

Both are the classic variable-capture problem, and both are invisible at run
time because of the permissive `Env`.

**Recommendation for the first cut: binder bodies are sealed.** Add to
`Expr.hs`:

```haskell
-- | Pre-order indices that lie inside some binder's body.
sealedIndices :: Expr -> [Int]
```

and have all three operators exclude those indices from their candidate points.
The effect is that a `Sum` node is atomic to the genetic operators: it can be
swapped, deleted or duplicated wholesale, but nothing reaches inside it. Its
body only ever changes by being generated fresh.

This is deliberately conservative — it means summation bodies do not evolve,
only get generated — but it is *sound*, it is about fifteen lines, and it fails
in the direction of "this operator is less useful than it could be" rather than
"the search silently optimises nonsense".

The less conservative version, once the sealed version works: allow crossover
between two points that are inside binder bodies *with the same index name*,
and allow any swap where neither subtree mentions any bound name. That needs a
`freeVariables :: Expr -> [VarName]` helper and a scope-aware traversal. It is
the right end state; it should not be the first attempt.

### Amendment, 2026-09-04 — the sealing plan is incomplete, and one part of it cannot be built first

This section was written before Phase 3 (domain validity) and Phase 5
(differentiation) existed, and it names three modules — `Ops/Crossover.hs`,
`Ops/Mutation.hs`, `Ops/Hoist.hs` — as the surface that binders threaten. That
list was audited against the current code on 2026-09-04. It is **not
complete**, and the sealing recommendation has a sequencing problem that is
worth naming before anyone relies on it.

#### The audit

Every module that either walks `Expr` positionally or matches on it with a
catch-all was checked. Most are safe:

| module | why it is safe |
|---|---|
| `Differentiate.differentiate` | `go` is an exhaustive `case`; its only catch-all is on a `(f, g)` tuple inside `powRule`. A new constructor is a `-Wincomplete-patterns` error, which forces a decision. |
| `LocalSearch` | its catch-all is on a simplex tuple, not on `Expr`. |
| `Ops/Crossover` | its catch-all is on a `Maybe` pair, and is already marked unreachable. |
| `Expr.provablyNonNegative` | `_ -> False`. Conservative in the safe direction: an unknown constructor is simply never claimed non-negative. |
| `Simplify.rewriteNode` | `_ -> expr`. A new constructor gets no rewrite rules, which is a missed optimisation, never a wrong answer — as its own comment says. |
| `Pretty` / `Report` | number `Const` leaves by pre-order ordinal. A binder adds no `Const` of its own, so existing ordinals still line up. |

Two are not.

#### Gap 1: `Checkpoint` is not mentioned in this section at all

It should be, because invariant 7 is precisely about it. The writer is an
exhaustive `case` (`Sin a -> node "sin" [a]`), so adding a constructor is a
compile error there. **The reader dispatches on operator-name strings**
(`"sin" -> unary Sin rest`, with `_ -> Left "unknown operator"`), which
`-Wall` cannot check. Adding `Sum` to the writer alone produces checkpoints
that write cleanly and fail to load — the exact blind spot invariant 7 exists
for.

Worse, the wire format has no shape for this. Every node is currently
`operator + children`; `Sum` is `operator + a bound name + children`. That is
a **format change**, not a new entry in a table, and it needs a version bump
and a decision about how a bound name is escaped.

Required before any binder lands: a reader entry, a `CheckpointSpec`
round-trip case, and a format-version decision.

#### Gap 2: `Simplify.isFoldable` — the analysis below was wrong, and the real bug was bigger

`isFoldable` is `null (variablesOf expr)`, and `variablesOf` is
`nub [v | Var v <- flatten expr]` — **every `Var` occurrence, including bound
ones**. So `Sum "i" lo hi body` would report `i` as a variable and refuse to
fold. Conservative, correct, and entirely accidental.

The danger is the obvious future improvement. This section already proposes a
`freeVariables` helper for the less-conservative operator rules. If
`variablesOf` is ever replaced by, or redefined as, free variables, then
`Sum "i" 1 1000000 (Const 5)` becomes foldable — and folding evaluates it, via
`eval []`, **during simplification of the elites, every generation**. §3 caps
iteration count for the *search* path; it says nothing about the simplifier,
which is a different call site.

So: **`isFoldable` must not use free-variable analysis**, or must exclude
binder nodes explicitly. Whichever is chosen needs a comment saying why, next
to the code, because the "improvement" is otherwise irresistible.

#### Gap 3: `sealedIndices` cannot be built before the thing it protects

The mitigation above is a function that returns pre-order indices lying inside
binder bodies. With no binder constructors, it can only be `const []` — which
is vacuously correct, passes any test written for it, and protects nothing.

**The mitigation therefore cannot precede the risk.** `sealedIndices`, its
tests, and the binder constructors must land in one change. Writing it early
to "have the safety net in place" would produce exactly the phantom net this
project has now hit twice — the Phase 4 note's "existing tests will guard the
refactor" (there were none) and `recoveryThreshold`'s "strict enough that an
approximation does not pass" (it was not).

The tests that make it real, none of which can be written before the
constructors exist:

1. `sealedIndices` on a tree containing a binder returns a **non-empty** list
   — the non-vacuity guard, without which every other assertion here passes
   trivially.
2. Crossover, mutation and hoist, run against a tree with a binder over many
   seeds, never produce a child whose binder body differs from a parent's.
3. Mutation-verified: neutering `sealedIndices` to `const []` must **fail**
   assertion 2. If it does not, the exclusion is not wired in.
4. A capture case built by hand: grafting a donor mentioning `i` into a `Sum i`
   body is impossible through the public operators.

#### Gap 4: differentiation did not exist when this was written

`differentiate` is exhaustive, so a binder forces a decision rather than
allowing a silent wrong answer. The decision still has to be made, and it
splits:

- `d/dx (Sum i lo hi body)` where `x` is free — is `Sum i lo hi (d body/dx)`
  when the bounds do not depend on `x`, and **is not** when they do.
- `d/di` with respect to a *bound* index is meaningless, not zero. It must
  refuse, and the refusal must be distinguishable from "this is constant in
  `i`" — which the current `Maybe Expr` can express but only if the rule
  actually checks whether the target name is shadowed by an enclosing binder.

The second is a genuine capture bug of its own: differentiating with respect
to `i` inside `Sum i` would otherwise silently treat the bound index as the
free variable of interest.

### Alpha-equivalence

Two `Sum` nodes differing only in index name are semantically identical but
structurally distinct, so `Eq Expr` will call them different, and `Simplify`'s
`Sub a b -> Const 0` rule will not fire on them. This is a correctness-preserving
inefficiency, not a bug. Renaming every binder to a canonical index at
construction time would fix it; that is a canonicalisation pass and belongs with
Upgrade 2's `canonicalize`, not here.

---

## 3. Cost and termination

`Sum` and `Integral` are the first constructors whose evaluation cost is not
O(nodes). An evolved `Sum` with bounds `-1e12 .. 1e12`, or nested three deep,
makes a single `eval` call effectively non-terminating — and `eval` is called
once per data point per individual per generation.

Three knobs, all in `Config.hs` per the no-literals rule:

```haskell
, cfgIntegrationSteps :: !Int   -- Simpson's rule subdivisions (default 32)
, cfgMaxIterations    :: !Int   -- hard cap on Sum/Product terms (default 1000)
, cfgBinderDepthLimit :: !Int   -- nested binders permitted (default 1)
```

- **`Integral`** by composite Simpson's rule with a *fixed* `cfgIntegrationSteps`
  subdivisions. Fixed, not adaptive: adaptive quadrature has data-dependent
  cost, which reintroduces exactly the unpredictability the cap exists to
  remove. Cost per evaluation is then a known constant.
- **`Sum`/`Product`** clamp `upper - lower` to `cfgMaxIterations`. Bounds are
  rounded to integers; a lower bound above the upper yields the empty sum
  (`0`) or empty product (`1`), which are the correct identities and keep the
  operator total.
- **`cfgBinderDepthLimit`** is the one that actually bounds the blow-up. Two
  nested sums at 1000 iterations each is 10⁶ body evaluations for *one* data
  point. Generation should refuse to place a binder inside a binder beyond this
  depth, the same way depth budgets already work in `Gen/Grow.hs`.

**Generation weight**: binders must be registered far below the pointwise
operators — 0.05 or so against `Add`'s 1.0 — and, unlike the Phase 1
weights, this one is defensible from cost alone. Phase 1 already showed that
weight is what governs whether an operator floods the trees; here the
consequence of getting it wrong is not ugly formulas but a search that makes
no progress per unit time. Consider gating them behind a `cfgEnableBinders`
flag defaulting to `False`, so ordinary regression runs never pay for a feature
they do not use.

---

## 4. Total versus partial derivative

**Phase 2 should implement the partial derivative only, and say so in the
operator's name.**

With the current AST, every `Var` is an independent symbol. There is no way to
express "`y` depends on `x`", so `∂f/∂x` (holding other variables fixed) and
`df/dx` (accounting for dependencies) coincide — every other variable
differentiates to zero because there is nothing to say it should not.

Calling the operator `Deriv` and documenting it as *total* would therefore be a
promise the AST cannot keep. Call it `PartialDeriv`, or name the function
`partialDerivative`, so the limitation is visible at the use site.

Supporting genuine total derivatives needs one of:

- **Declared dependencies** — a config-level statement that `y = y(x)`, which
  makes `dy/dx` a fresh symbol the search must also discover. This is the
  smallest change and interacts well with Upgrade 5's dimensional annotations,
  since both are per-variable metadata.
- **An implicit-function constructor** — a node asserting `F(x, y) = 0`, with
  `dy/dx = -(∂F/∂x)/(∂F/∂y)`. Considerably more machinery, and it makes the
  protected-division problem worse, since that quotient is exactly where things
  are undefined.

Neither belongs in Phase 2. The partial case covers the motivating uses
(derivative-aware fitness, residual structure analysis) and can be extended
later without changing its meaning.

---

## 5. The prerequisite: domain validity (roadmap Upgrade 6)

This was not one of the original four questions, but Phase 1 turned it into a
blocker for doing §1 properly, so it belongs in this note.

Every recommendation above that says "valid only where `b ≠ 0`" or "valid only
where `f > 0`" needs somewhere to *put* that condition. Right now there is
nowhere: `eval` returns a bare `Double` and a protected fallback is
indistinguishable from a real value.

The proposal:

```haskell
data EvalResult
  = Valid Double
  | Invalid DomainError

data DomainError
  = DividedByZero
  | LogOfNonPositive
  | PowOfNegativeBase
  | GammaAtPole
  | ZetaAtPole
  | Saturated          -- clamped at magnitudeCap
```

> **Amendment, 2026-08-19 — `Saturated` does not belong in this type.**
>
> Measured after implementation (`typedgp-bench --domain-breakdown`, and the
> table in `docs/phase3-domain-design.md`): saturation is 1–16% of an initial
> population's invalid evaluations and **0% of every surviving Pareto front**.
> But frequency is not the argument. The argument is that it is a different
> kind of fact.
>
> The other five say *this expression has no real value at this point*. That
> is a property of the mathematics and it is what a derivative rule needs to
> know. `Saturated` says *the value exists and is large enough that the
> engine's magnitude cap truncated it*. `exp(1000)` is perfectly well defined
> and perfectly differentiable; only the representation ran out.
>
> Grouping them meant `isValid` answered two questions at once and neither
> cleanly. **`Saturated` is hereby reclassified as a numeric-range condition,
> orthogonal to domain validity.** Anything asking "is this meaningful here" —
> including every Phase 5 differentiation rule — must exclude it. It stays a
> `DomainError` constructor for now because removing it would be a larger
> refactor than this amendment warrants, but its meaning is fixed by this
> paragraph, not by its membership of the type.
>
> Read the list above as five domain conditions plus one range condition that
> happens to share a constructor space with them.

with `eval` keeping its current total, always-finite behaviour as
`evalProtected` (nothing in the search loop has to change on day one), and
fitness gaining a *domain penalty* proportional to the fraction of training
points on which an individual was `Invalid`.

Why this is worth the churn:

- It separates **numerical protection** (keep the search alive) from
  **mathematical validity** (is this expression meaningful here), which are
  currently conflated. An individual scoring well *because* it divides by zero
  on 40% of the data is currently indistinguishable from one that does not.
- It is what makes §1's `Maybe Expr` honest. "Differentiable, subject to
  `f > 0`" becomes a checkable claim rather than a comment.
- It gets more important with every operator added, because each one brings its
  own excluded region. `Pow` alone contributes a two-dimensional one.

**Sequencing recommendation: do Upgrade 6 before, or alongside, symbolic
differentiation** — not after. Retrofitting domain conditions onto a
`differentiate` that already returns bare `Expr` means revisiting every rule.

---

## Recommended order

1. **Domain validity** (`EvalResult`, domain penalty). Unblocks the rest and
   pays for itself immediately by exposing individuals that survive on
   protection.
2. ~~**Constant optimisation** (roadmap Upgrade 4).~~ **DONE.** Built as
   `TypedGP.LocalSearch`: a Nelder-Mead simplex wired into the generational
   step, refining the elites' constants every `cfgRefineEvery` generations.
   The compass search it replaces is retained for comparison, with tests
   asserting the simplex succeeds on the coupled cases where it fails.

   One finding from doing it, which changes the sequencing below: it fixes
   *coupling* but not *multimodality*. `2 sin(3x + 0.5)` remains unsolved
   because the error has a local minimum near every frequency, and no local
   optimiser crosses those. That makes periodicity-seeded initialisation a
   separate, still-open item rather than something constant optimisation
   subsumes — see the roadmap's frequency-search entry.
3. **`partialDerivative :: VarName -> Expr -> Maybe Expr`**, plus
   `canonicalize`. No new constructors, so no genetic-operator risk at all.
4. **`Sum`/`Product`**, sealed bodies, behind `cfgEnableBinders`.
5. **`Integral`**, same machinery, fixed-step Simpson.

Steps 1–3 add no binder nodes and therefore carry none of §2's capture risk.
If Phase 2 stops after step 3, the engine still gains the second
representation (`f` and `f'`) that Upgrade 2 is really about — and the AST is
unchanged, which means every genetic operator, the checkpoint format and the
simplifier all keep working untouched.

---

## Open questions for review

1. Is `Maybe Expr` the right refusal mechanism for `differentiate`, or should
   it be `Either DomainCondition Expr` so a caller can see *why* and decide
   whether the condition holds on its data?
2. Are sealed binder bodies too conservative to be useful? A `Sum` whose body
   never evolves may not be worth having at all — if so, it is better to know
   that before building it.
3. Should `Gamma`/`Zeta` be excluded from the operator set entirely when
   derivative-aware fitness is active, rather than making `differentiate`
   partial?
4. The benchmark question is genuinely blocking measurement here. None of the
   above can be shown to *help* against `2x + sin(y)`, which is recovered 10/10
   by generation 0–4 already. A family-spanning benchmark suite (polynomial,
   rational, power law, exponential, logarithmic, trigonometric, nested,
   interaction, plus noisy and irrelevant-variable variants) is a prerequisite
   for evaluating Phase 2, not a follow-up to it.
