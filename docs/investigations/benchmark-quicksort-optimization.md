# Benchmark Investigation: Quicksort

## Executive Summary

**Benchmark:** quicksort (functional quicksort with three-way partition)
**Current Dark status:** direct full-size run succeeds locally, but `run_benchmarks.sh` still skips the Dark quicksort benchmark
**Current benchmark table context:** Rust 6,506,788 instructions; OCaml 47,643,021 instructions (7.32x Rust); Dark has no table entry because it is skipped

Current local evidence changes the old status: `benchmarks/problems/quicksort/dark/main.dark` now compiles and runs through `./dark -r`, producing checksum `600660212`. The benchmark harness still has `SKIP_BENCHMARKS=("quicksort")`, so `benchmarks/RESULTS.md` does not yet contain a current Dark instruction count for this benchmark.

The remaining compiler evidence still points at allocation-heavy and call-heavy list partitioning. Each non-base quicksort partition allocates three predicate closures, performs three full `Stdlib.List.filter_i64` traversals over the same list, recursively sorts two partitions, and appends twice. This makes a single-pass partition implementation or list-filter specialization the most durable quicksort-specific optimization target, but the first benchmark action should be to remove or revalidate the stale skip so future investigations can use a current Dark instruction count.

## Benchmark Implementation

The Dark benchmark uses the same high-level algorithm as Rust and OCaml: pick the middle element as pivot, split the list into `< pivot`, `== pivot`, and `> pivot`, recursively sort left and right, then concatenate.

```dark
def quicksort(arr: List<Int64>) : List<Int64> =
    let len = Stdlib.List.length<Int64>(arr) in
    if len <= 1 then arr
    else
        let pivot = getAtOrDefault(arr, len / 2, 0) in
        let left = Stdlib.List.filter<Int64>(arr, (x: Int64) => x < pivot) in
        let middle = Stdlib.List.filter<Int64>(arr, (x: Int64) => x == pivot) in
        let right = Stdlib.List.filter<Int64>(arr, (x: Int64) => x > pivot) in
        Stdlib.List.append<Int64>(
            Stdlib.List.append<Int64>(quicksort(left), middle),
            quicksort(right))
```

Rust and OCaml use equivalent three-way partitioning, but Rust partitions into vectors and OCaml partitions simple cons lists. Dark partitions through the current `Stdlib.List` representation.

## Current Evidence

### Direct Full-Size Run Now Succeeds

Local command:

```bash
./dark -r benchmarks/problems/quicksort/dark/main.dark
```

Result:

```text
600660212
Exit code: 0
```

This means the prior direct-run OOM status is stale. `benchmarks/run_benchmarks.sh` still skips quicksort, and the current `benchmarks/RESULTS.md` table therefore still shows no Dark instruction count for quicksort.

### Current ANF Shape

The optimized ANF for `quicksort` still contains the expensive high-level structure:

```text
let TempId 975 = ClosureAlloc(__closure_9, [t973])
let TempId 976 = Stdlib.List.filter_i64(t968, t975)
let TempId 978 = ClosureAlloc(__closure_10, [t973])
let TempId 979 = Stdlib.List.filter_i64(t968, t978)
let TempId 981 = ClosureAlloc(__closure_11, [t973])
let TempId 982 = Stdlib.List.filter_i64(t968, t981)
let TempId 984 = quicksort(t976)
let TempId 985 = Stdlib.List.append_i64(t984, t979)
let TempId 986 = quicksort(t982)
let TempId 987 = Stdlib.List.append_i64(t985, t986)
```

The three closure bodies are trivial comparisons against the captured pivot:

```text
Function __closure_9:  t1 < t2
Function __closure_10: t6 == t7
Function __closure_11: t11 > t12
```

The LIR preserves the same shape: three 16-byte closure allocations, three calls to `Stdlib.List.filter_i64`, two normal recursive calls to `quicksort`, and two calls to `Stdlib.List.append_i64`. These recursive calls are not self-tail calls because concatenation and cleanup work remain after each recursive result.

### Filter Implementation Has Changed

Older notes described `filter` in terms of `isEmpty`, `head`, and `tail`. Current ANF no longer matches that shape. `Stdlib.List.filter_i64` computes the list measure and delegates to `Stdlib.List.__filterByIndexHelper_i64`, which walks by index:

```text
let TempId 919 = Stdlib.__FingerTree.getAt_i64(t912, t914)
...
let TempId 931 = ClosureCall(t913, [t928])
if t931 then
    let TempId 932 = Stdlib.__FingerTree.push_i64(t916, t928)
    let TempId 934 = Stdlib.List.__filterByIndexHelper_i64(...)
else
    let TempId 934 = TailCall(Stdlib.List.__filterByIndexHelper_i64, ...)
```

At completion, the accumulated result is reversed through `Stdlib.List.__reverseByIndexHelper_i64`. This is a useful status correction: the old redundant `isEmpty + head + None` evidence is stale, but each quicksort partition still pays for three independent indexed traversals, option unpacking from `getAt_i64`, closure calls, and result-list construction.

## Durable Optimization Opportunities

### 1. Re-enable or Rebaseline Dark Quicksort in the Benchmark Harness

The direct full-size program now succeeds, but the benchmark runner still skips it. Before ranking quicksort optimizations by measured impact, remove or revalidate that skip and collect a current Dark instruction count. Until then, the investigation can only compare compiler IR shape against Rust and OCaml baselines, not current Dark benchmark performance.

### 2. Single-Pass Three-Way Partition

The benchmark currently traverses the same list three times:

- `filter(arr, x < pivot)`
- `filter(arr, x == pivot)`
- `filter(arr, x > pivot)`

A single traversal that builds `(left, middle, right)` would remove two full list scans per partition and reduce intermediate list churn. This is the most direct quicksort-specific optimization because it preserves the benchmark algorithm while targeting the dominant current IR shape.

### 3. Specialize Trivial Filter Predicates

The three predicates are closure-allocated wrappers around primitive integer comparisons. A specialization that inlines simple captured comparisons into list traversal would remove closure allocation and indirect closure calls from every partition. This is smaller than single-pass partitioning, because it keeps three traversals, but it is a reusable optimization for filter-heavy code.

### 4. Improve Sequential List Traversal

Current `filter_i64` traverses by repeated indexed `getAt_i64` and reverses the accumulated result at the end. For sequential list operations, an iterator or destructuring helper that avoids per-element indexed lookup and option materialization would reduce overhead across `filter`, `reverse`, and checksum-like folds.

## Current Next Step

Update the benchmark harness status for quicksort before making compiler optimization decisions from benchmark numbers. The documentation status is now:

- direct Dark full-size quicksort succeeds locally,
- the full benchmark table still omits Dark quicksort because the harness skips it,
- the strongest current IR evidence is three independent filter traversals with trivial closure predicates.
