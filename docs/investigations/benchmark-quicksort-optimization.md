# Benchmark Investigation: Quicksort

## Current Result

The direct-payload skew RAL plus a list-tailored quicksort reduces the Dark
full-size run from 61,351,602,854 to 90,824,056 instructions. Against Rust's
6,506,788 instructions, the gap is now 14.0x instead of roughly 9,429x. This is
a 675.5x reduction in Dark instructions, and the benchmark still produces the
expected checksum.

Command:

```bash
./benchmarks/run_benchmarks.sh quicksort
```

## Implementation

The old implementation selected the middle element, traversed the input three
times with closure-based `filter`, recursively sorted two partitions, and used
two persistent-list appends. That amplified the FingerTree's allocation and
traversal costs at every recursion level.

The current implementation is tailored to skew lists:

- the head is the pivot, avoiding an indexed pivot lookup
- `partition3` classifies values in one traversal
- each bucket uses worst-case O(1) prepend
- `quicksortInto(arr, suffix)` constructs directly in front of a known suffix
- `prependAll` places the equal bucket without copying the sorted left side

```dark
let quicksortInto(arr: List<Int64>, suffix: List<Int64>) : List<Int64> =
    match arr with
    | [] -> suffix
    | [only] -> [only, ...suffix]
    | [pivot, ..._] ->
        let (less, equal, greater) = partition3(arr, pivot, [], [], []) in
        let sortedGreater = quicksortInto(greater, suffix) in
        let equalAndGreater = prependAll(equal, sortedGreater) in
        quicksortInto(less, equalAndGreater)
```

This preserves immutable semantics and reference-counted allocation. It does
not rely on mutation or an array representation.

## Remaining Gap

Rust still has contiguous mutable storage, in-place partitioning, and much
lower allocation and reference-count traffic. A 14.0x gap is therefore
practical for the immutable-list benchmark, but it is not evidence that a
persistent list should replace arrays for sorting workloads. Further large
gains would likely require a separate array type, uniqueness-aware mutation,
or compiler optimization of linear RC traffic rather than another list shape.
