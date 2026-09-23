# Performance notes

A record of what has been measured about where TypedGP spends its time, and
of which optimisations did and did not pay. Written because two of the
conclusions below were first reached wrongly from noisy measurements, and the
method that finally worked is worth keeping.

---

## 1. Parallel fitness evaluation did not parallelise (fixed 2026-09-23)

`parMapChunked` sparked `forceSpine mapped`, a fresh `()` thunk that nothing
else referenced, and returned `mapped`. Sparks are weak references, so almost
every spark was garbage-collected before a worker reached it.

| | before | after |
|---|---|---|
| sparks converted, `-N8` | 182 / 1296 | 985 / 1296 |
| sparks GC'd | 1114 | 215 |

Results are bit-identical. The wall-clock gain on 8 cores was modest (≈1.17×)
because much of each generation is serial: breeding, selection, and
constant refinement of elites (≈23% of runtime by itself, from comparing
default runs with `--no-refine`).

## 2. Closure compilation: exact, no gain, removed

A `TypedGP.Compile` module turned an `Expr` into a closure once per
individual, with each `Var` resolved to a column position. It was checked
bit-for-bit against `eval` over 20k+ expression/environment pairs, and a
mutation (swapped `Sub` operands) was caught. It was correct. It was also
not faster, so it was taken out rather than kept as a second implementation
of the semantics that would have to be kept in sync forever.

| measurement | result |
|---|---|
| microbenchmark, 20k random trees × 100 points | ~1.09× (all trees), ~1.14× (no `gamma`/`zeta`) |
| allocation in that microbenchmark | **+20%** (683 MB vs 570 MB) |
| full benchmark, A/B alternated, `-N1` | 13.1 / 13.7 s (`eval`) vs 13.8 / 13.9 s (`compile`) |
| re-run after the tournament fix (§4), 3 alternations | medians 8.0 s (`eval`) vs 8.2 s (`compile`) |
| allocation, `polynomial`, 3 seeds | 3.99 GB (`eval`) vs 5.85 GB (`compile`), **+47%** |

The allocation increase is the explanation: a closure call returns a
heap-boxed `Double`, while the interpreter's known recursive function lets
GHC return it unboxed. The dispatch that compilation saves is roughly repaid
in boxing. **Rearranging the per-point tree walk does not lift the ceiling.**

## 3. Where the time actually goes

From a profiling build (`--enable-profiling --profiling-detail=toplevel-functions`,
`polynomial`, one seed, `-N1`, taken while the closure compiler was wired
in, hence its names in the first row):

| cost centre | time | alloc |
|---|---|---|
| expression evaluation (`compile` + `sanitize` + `valueAt` + `errorOf`) | ~64% | ~47% |
| `countNodes` | 10.6% | 20.1% |
| `pick` + `safeIndex` (tournament selection over a list) | 11.1% | ~0% |

**Profiling inflates small hot functions.** Cost centres block inlining, so a
tiny function called millions of times looks far more expensive than it is
in an optimised build. Rewriting `countNodes` as an allocation-free
accumulator cut total allocation by 4% (1.80 GB → 1.73 GB) but produced no
wall-clock change distinguishable from noise — nowhere near the 10.6% the
profile suggested.

## 4. Tournament draws from an array: ~1.3× faster, kept

Tournament selection used `pick` on the 500-element pool list: `length`
then a walk to the index, on every draw of every selection event, about a
thousand events per generation. `SelectionContext` now carries the pool as
an array (`GHC.Arr`, in `base`), built once per generation, and
`tournamentFrom` draws from it in O(1).

It consumes exactly the random numbers `pick` did: one `nextInt size` per
draw, then the element at that index. Every benchmark result reproduced
bit-for-bit (polynomial / rational / trigonometric, 8 seeds: 62%/38%,
0%/0%, 100%/100%, same errors and sizes).

| `polynomial`, 3 seeds, `-N1`, alternated | before | after | ratio |
|---|---|---|---|
| batch 1, run 1 | 9.39 s | 6.80 s | 1.38 |
| batch 1, run 2 | 8.31 s | 6.97 s | 1.19 |
| batch 1, run 3 | 8.92 s | 6.72 s | 1.33 |
| batch 2, run 1 | 11.91 s | 8.66 s | 1.38 |
| batch 2, run 2 | 9.05 s | 7.39 s | 1.22 |
| batch 2, run 3 | 9.86 s | 9.13 s | 1.08 |
| batch 2, run 4 | 14.30 s | 10.09 s | 1.42 |

The after-run is faster in every pair; the median ratio is 1.33. Batch 2 ran
on a busier machine, which is why its absolute times are higher and why the
comparison is paired rather than pooled. ("Before" is the build with the
closure compiler still wired in, so batch 2's allocation drop — 5.87 GB to
3.99 GB — includes removing it.)

## 5. How to measure here

Two mistaken conclusions came from the same errors, so they are rules now:

- **A single wall-clock run is ±15% noise on a ~3 s job.** Time differences
  under about 20% cannot be read off one run each. Alternate A/B runs, repeat,
  and compare medians.
- **Varying the dataset changes the search, not just the workload.**
  Different data makes the search evolve different trees, so "time at 100
  points vs 400 points" compares two different trajectories. An apparent
  superlinear cost in data size turned out to be exactly this.
- **Allocation is deterministic.** The same command allocates the same number
  of bytes every time, which makes `+RTS -s` bytes-allocated the most
  reliable single metric for small changes.
- Use the profiler to find *where* to look, and timing A/Bs to decide whether
  a change paid.

## 6. What would actually be faster

Evaluation is the majority of the cost, and evaluating one point at a time is
the ceiling. The change with real headroom is **column-wise evaluation**:
evaluate each node over *all* points at once, so dispatch is paid once per
node per individual rather than once per node per point, and the inner loops
run over unboxed `Double` buffers. That stays within the zero-dependency rule
— `Foreign.Marshal.Array` and `Storable Double` are in `base`. It is a larger
change than closure compilation and needs its own design note first.

Smaller, independent candidates, each needing an A/B before being kept:

- Constant refinement runs on elites serially and could be parallelised.
- `shuffle` is O(n²) via list removal. It is harmless for the one-off holdout
  split, but lexicase calls it once per selection event on the case list.
