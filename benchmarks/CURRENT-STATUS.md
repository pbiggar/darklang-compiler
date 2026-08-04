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
| factorial     | Recursion      | Basic recursion, multiplication         |
| fib           | Recursion      | Exponential recursion, addition         |
| sum_to_n      | Tail Recursion | Tail call optimization (currently slow) |
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
| nsieve        | Numerical      | Sieve of Eratosthenes using Dict-backed composites |
| pisum         | Numerical      | Floating-point reciprocal-square sum    |
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

These benchmarks have implementations but are limited by stack depth or bugs.

| Benchmark     | Status            | Limitation                                                     |
| ------------- | ----------------- | -------------------------------------------------------------- |
| nbody         | Working (reduced) | Uses 5,000 simulation steps (full: 500,000 steps)              |
| spectral_norm | Working (reduced) | Computes the full power-iteration algorithm at 3-vector size (full: n=100) |

---

## Implementation Coverage

These benchmarks are in the suite for other languages and their current Dark
implementation coverage is:

All benchmark directories now have a Dark implementation. Some implementations
remain reduced or blocked as documented above.

---

## Feature Requirements Summary

| Feature                                  | Benchmarks Blocked                                  |
| ---------------------------------------- | --------------------------------------------------- |
| **Allocator capacity / allocation pressure** | nbody (full)                                  |
| Mutable arrays / efficient indexed numeric vectors | spectral_norm (full n=100)              |

---

## Notes

- Closure capture in quicksort predicates now works, and quicksort is enabled in the full benchmark suite.
- The mandelbrot "negative float bug" was actually a semantic mismatch - the Dark code was counting escaped points while the Rust reference counts points in the set. Fixed.
- The pisum Dark benchmark now runs at the full benchmark size (`500` rounds, `n=10000`) and is tracked as working.
- The nsieve Dark benchmark now runs at the full benchmark size (`n=100000`) and is tracked as working.
- The edigits Dark benchmark now runs the canonical 1,000-digit computation ten times. It packs 15 decimal digits into each `Int64` chunk to keep immutable indexed updates bounded.
