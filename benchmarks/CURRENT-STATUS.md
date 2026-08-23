# Benchmark Suite

Micro-benchmark suite for the Darklang compiler, inspired by standard micro-benchmark suites.

## Sources

- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [kostya/benchmarks](https://github.com/kostya/benchmarks)
- [Programming Language Benchmarks](https://programming-language-benchmarks.vercel.app/)
- [plb2 (Programming Language Benchmark v2)](https://github.com/attractivechaos/plb2)
- [Julia Micro-Benchmarks](https://julialang.org/benchmarks/)

---

## Currently Working ✓

These benchmarks compile and run reliably.

| Benchmark     | Category       | What It Tests                           |
| ------------- | -------------- | --------------------------------------- |
| factorial     | Recursion      | Constant recursive workload, multiplication |
| fib           | Recursion      | Exponential recursion, addition         |
| sum_to_n      | Tail Recursion | Constant-bound tail-call loops           |
| ackermann     | Deep Recursion | Extreme recursion depth, call overhead  |
| tak           | Recursion      | Takeuchi function, nested calls         |
| binary_trees  | Memory         | Heap allocation, tree traversal         |
| primes        | Arithmetic     | Integer ops, conditionals, loops        |
| collatz       | Iteration      | Collatz sequence steps                  |
| edigits       | Numerical      | Canonical 1,000-digit, 10-iteration fixed-point computation |
| leibniz       | Numerical      | Float arithmetic, pi approximation      |
| nqueen        | Backtracking   | N-Queens via bitwise operations         |
| fannkuch      | Permutations   | Pancake flipping, permutation generation |
| merkletrees   | Tree/Hashing   | Recursive tree hashing                  |
| mandelbrot    | Numerical      | Complex number iteration, fractal       |
| matmul        | Numerical      | Generated 100x100 matrix multiplication checksum |
| nbody         | Numerical      | Canonical 500,000-step five-body simulation |
| pisum         | Numerical      | Repeated floating-point reciprocal-square sums |
| quicksort     | Sorting        | Functional quicksort over generated list |

---

## Known Regression: `fasta` (allocator guard hot-path overhead)

`fasta` regressed from `2,436,771,495` to `2,446,245,270` instructions after allocator guard changes.

Investigation summary:

- Delta: `+9,473,775` instructions, `+1,894,755` branches
- Ratio: exactly `5.0` extra instructions per extra branch
- Cause: per-allocation heap bounds checks currently recompute heap-end and add one extra conditional branch on every bump allocation

Suggested fix:

1. Hoist heap-end into a dedicated register in heap init.
2. Use one shared OOM trap label instead of inlining trap code at each allocation site.
3. Keep checks only on bump-allocation fallback paths.

Update from latest run (`2026-03-03_204818`):

- Dark `fasta`: `2,446,245,402` instructions (`+132`, effectively unchanged vs `2,446,245,270`)
- Shared OOM trap label is implemented, which reduced code size, but hot-path instruction regression remains
- Remaining regression is still from per-allocation heap-end recomputation and branch in bump allocation

Update from run (`2026-03-04_091307`):

- Dark `fasta`: `2,457,610,200` instructions (`+11,364,798`, `+0.46%` vs previous latest `2,446,245,402`)
- The regression is still consistent with the same per-allocation bounds-check hot-path cost

Details: see `docs/investigations/benchmark-fasta-optimization.md` ("2026-03-03 Regression Investigation").

---

## Implemented But Limited

These benchmarks have Dark implementations but are not canonical comparisons.

| Benchmark    | Status                       | Limitation                                                     |
| ------------ | ---------------------------- | -------------------------------------------------------------- |
| fannkuch     | Diagnostic only (reduced)      | Rust correctly enumerates n=9; Dark correctly enumerates n=8 because n=9 exhausts its fixed heap |
| nsieve       | Diagnostic only (incomparable) | Rust runs 100 full sieves; Dark's persistent Dict and bump allocator support only one practical run |

---

## Implementation Coverage

These benchmarks are in the suite for other languages and their current Dark
implementation coverage is:

All benchmark directories now have a Dark implementation. Some implementations
remain reduced or blocked as documented above.

---

## Feature Requirements Summary

| Feature                                       | Benchmarks Blocked                              |
| --------------------------------------------- | ----------------------------------------------- |
| More heap capacity or reclaimable list storage | fannkuch n=9 parity                             |
| Reclaimable persistent allocations            | nsieve 100-run parity                           |
| Mutable arrays / efficient indexed vectors    | None; arrays would still improve numeric efficiency |

---

## Notes

- Quicksort and spectral norm both run at full size in the canonical routine profile.
- All 21 benchmarks have matching Rust and Dark quick variants. Twenty quick
  pairs are comparable; nsieve remains diagnostic only.
- The binary-trees benchmark constructs `Tree<Int64>` values with recursive
  `Leaf` and `Node` variants, traverses them, and releases the complete graph on
  every iteration.
- The mandelbrot "negative float bug" was actually a semantic mismatch - the Dark code was counting escaped points while the Rust reference counts points in the set. Fixed.
- Rust sources remain the original reference workloads and are built with the
  original `rustc -C opt-level=3` command. Fairness changes are made in Dark or
  by excluding a pair, not by reshaping Rust.
- The Dark pisum translation returns the last of 500 rounds, squares each
  integer before converting it to float, and carries a result-dependent exact
  zero into the next round so effect-free call hoisting cannot erase 499 rounds.
- The Dark edigits translation uses one decimal digit per list element and the
  same 1,000-digit, ten-iteration fixed-point algorithm as Rust.
- Dark matmul materializes the complete product before its checksum pass, as
  Rust does, and Dark primes uses the same floating-point square-root bound.
- `PARITY.json` locks every audited full and quick source plus its expected
  output; only comparable full pairs may enter the 19-benchmark routine profile
  or canonical ratio.
