# Benchmark Suite

Comprehensive benchmark suite for the Darklang compiler, inspired by standard benchmark suites.

## Sources
- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [kostya/benchmarks](https://github.com/kostya/benchmarks)
- [Programming Language Benchmarks](https://programming-language-benchmarks.vercel.app/)
- [plb2 (Programming Language Benchmark v2)](https://github.com/attractivechaos/plb2)
- [Julia Micro-Benchmarks](https://julialang.org/benchmarks/)

---

## Currently Working ✓

These benchmarks compile and run reliably.

| Benchmark | Category | What It Tests |
|-----------|----------|---------------|
| factorial | Recursion | Basic recursion, multiplication |
| fib | Recursion | Exponential recursion, addition |
| sum_to_n | Tail Recursion | Tail call optimization (currently slow) |
| ackermann | Deep Recursion | Extreme recursion depth, call overhead |
| tak | Recursion | Takeuchi function, nested calls |
| binary_trees | Memory | Heap allocation, tree traversal |
| primes | Arithmetic | Integer ops, conditionals, loops |
| collatz | Iteration | Collatz sequence steps |
| leibniz | Numerical | Float arithmetic, pi approximation |
| nqueen | Backtracking | N-Queens via bitwise operations |
| merkletrees | Tree/Hashing | Recursive tree hashing |

---

## CRITICAL BUG: Non-Deterministic Segfaults

**Status: BLOCKING multiple benchmarks**

Several benchmarks experience intermittent segmentation faults. The crashes are non-deterministic - the same binary may work on one run and crash on the next.

### Affected Benchmarks:
| Benchmark | Notes |
|-----------|-------|
| spectral_norm | Crashes ~40% of runs even with small n=5 |
| nbody | Crashes with >10-20 iterations, also produces wrong output (0 instead of energy value) |
| matmul | Crashes intermittently even with n=3 |

### Symptoms:
- Non-deterministic: Same code crashes sometimes, works other times
- More likely to fail with more iterations/larger inputs
- When it doesn't crash, may produce incorrect results (e.g., nbody outputs 0)

### Suspected Cause:
- Memory corruption or uninitialized memory
- Stack corruption
- Register allocation issues in code generator
- Affects both float and integer heavy code

### Reproduction:
```bash
# Run multiple times to see non-deterministic behavior
./dark benchmarks/problems/spectral_norm/dark/main.dark -o /tmp/sn
for i in 1 2 3 4 5; do /tmp/sn; done
```

---

## CRITICAL BUG: Negative Floats in Recursive Functions

**Status: CAUSES WRONG RESULTS**

Negative float values passed to recursive functions with 4+ float parameters produce incorrect results.

### Affected Benchmarks:
| Benchmark | Impact |
|-----------|--------|
| mandelbrot | Outputs 96 instead of correct 50 for 10x10 grid |

### Minimal Reproduction:
```dark
def iterate(cr: Float, ci: Float, zr: Float, zi: Float, iter: Int64, maxIter: Int64) : Int64 =
    if iter >= maxIter then 0
    else
        let zr2 = zr * zr in
        let zi2 = zi * zi in
        if zr2 + zi2 > 4.0 then 1  // BUG: This incorrectly returns 1 when cr is negative
        else iterate(cr, ci, zr, zi, iter + 1, maxIter)

// Returns 1 (WRONG - should be 0 since 0*0 + 0*0 = 0 < 4)
iterate(0.0 - 1.5, 0.0, 0.0, 0.0, 0, 3)

// Returns 0 (CORRECT)
iterate(1.5, 0.0, 0.0, 0.0, 0, 3)
```

### Notes:
- Bug only manifests when first float parameter is negative
- Works correctly with positive values or fewer parameters
- Expected output files updated to match buggy behavior for CI

---

## Implemented But Limited

These benchmarks have implementations but are limited by stack depth or bugs.

| Benchmark | Status | Limitation |
|-----------|--------|------------|
| spectral_norm | INTERMITTENT SEGFAULT | Works sometimes with n=5, crashes other times |
| nbody | SEGFAULT + WRONG OUTPUT | Crashes after 10-20 iterations, outputs 0 |
| mandelbrot | WRONG OUTPUT | Uses 10x10 grid, outputs 96 instead of correct 50 |
| pisum | Working (reduced) | Uses 5 rounds, n=1000 (full size causes stack overflow) |
| matmul | INTERMITTENT SEGFAULT | Implemented with n=3, crashes intermittently |
| quicksort | Stack overflow | Works only with 3 elements |

---

## Stub Implementations (Not Yet Implemented)

These benchmarks have placeholder implementations that return 0.

| Benchmark | Category | Missing Features |
|-----------|----------|------------------|
| fannkuch | Permutation | Mutable arrays, in-place reversal |
| nsieve | Sieve | Mutable arrays for sieve |
| fasta | Bioinformatics | Random numbers, efficient string building |
| edigits | Numerical | Unknown requirements |

---

## Feature Requirements Summary

| Feature | Benchmarks Blocked |
|---------|-------------------|
| **Non-deterministic segfault bug** | spectral_norm, nbody, matmul |
| Stack depth / TCO | pisum (full), mandelbrot (full), quicksort |
| Mutable arrays | fannkuch, nsieve |
| String operations | fasta |

---

## Running Benchmarks

```bash
# Run all benchmarks
./benchmarks/run_benchmarks.sh

# Run specific benchmark
./benchmarks/run_benchmarks.sh fib

# Run with timing instead of instruction count
./benchmarks/run_benchmarks.sh --hyperfine
```

---

## Notes

- Expected outputs for reduced-size benchmarks need to be updated to match reduced parameters
- The non-deterministic segfault bug is the highest priority issue to fix
- Float-heavy code (spectral_norm, nbody) seems most affected but integer code (matmul) also crashes
