# Benchmark Suite

Comprehensive benchmark suite for the Darklang compiler, inspired by standard benchmark suites.

## Sources
- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [kostya/benchmarks](https://github.com/kostya/benchmarks)
- [Programming Language Benchmarks](https://programming-language-benchmarks.vercel.app/)
- [plb2 (Programming Language Benchmark v2)](https://github.com/attractivechaos/plb2)
- [Julia Micro-Benchmarks](https://julialang.org/benchmarks/)

---

## Currently Implemented ✓

| Benchmark | Category | What It Tests |
|-----------|----------|---------------|
| factorial | Recursion | Basic recursion, multiplication |
| fib | Recursion | Exponential recursion, addition |
| sum_to_n | Tail Recursion | Tail call optimization (currently slow) |
| ackermann | Deep Recursion | Extreme recursion depth, call overhead |
| tak | Recursion | Takeuchi function, nested calls |
| binary_trees | Memory | Heap allocation, tree traversal |
| primes | Arithmetic | Integer ops, conditionals, loops |

---

## Stub Implementations (Awaiting Features)

These benchmarks have Python/Rust reference implementations and expected outputs,
but the Dark implementation is a stub pending required language features.

| Benchmark | Category | Missing Features |
|-----------|----------|------------------|
| nbody | Physics/Float | Float sqrt, Float comparisons |
| mandelbrot | Numerical | Float comparisons (>, <, >=, <=) |
| spectral_norm | Matrix/Numerical | Float sqrt, Float division, Array indexing |
| fannkuch | Permutation | Mutable arrays, In-place reversal |
| quicksort | Sorting | List filtering with closures |
| nqueen | Backtracking | Bit manipulation tricks (two's complement) |
| matmul | Numerical | 2D array indexing, nested iteration |
| pisum | Numerical | Float division, Int to Float conversion |
| collatz | Iteration | TCO (implementable but may stack overflow) |
| leibniz | Numerical | Float division, Float negation, TCO |

---

## Ready to Implement (Have Required Features)

### meteor-contest
**Category:** Puzzle Solving
**Tests:** Backtracking, recursion, bit manipulation
**Features Needed:** Bitwise ops (we have them), recursion
**Priority:** MEDIUM

### nsieve (Sieve of Eratosthenes)
**Category:** Array / Memory
**Tests:** Array operations, memory access patterns
**Features Needed:** Mutable arrays or can adapt with Dict
**Priority:** MEDIUM

---

## Need Float Support

### n-body
**Category:** Physics Simulation
**Tests:** Floating point arithmetic, records/structs, loops
**Features Needed:** Float arithmetic, sqrt
**Status:** Floats exist but may need more stdlib functions
**Priority:** HIGH - Classic benchmark

### spectral-norm
**Category:** Numerical / Matrix
**Tests:** Matrix computation, floating point precision
**Features Needed:** Float arrays, sqrt
**Priority:** HIGH

### mandelbrot
**Category:** Numerical / Graphics
**Tests:** Complex number arithmetic (simulated with tuples), iteration
**Features Needed:** Float comparison, output formatting
**Priority:** HIGH - Visually interesting

---

## Need String/IO Features

### fasta
**Category:** Bioinformatics / Output
**Tests:** String generation, weighted random selection, I/O throughput
**Features Needed:** Efficient string building, random numbers
**Priority:** MEDIUM

### reverse-complement
**Category:** Bioinformatics / String
**Tests:** String transformation, I/O, lookup tables
**Features Needed:** String indexing, file I/O
**Priority:** MEDIUM

### k-nucleotide
**Category:** Bioinformatics / Hash
**Tests:** Hash table operations, string processing
**Features Needed:** String slicing, efficient Dict
**Priority:** MEDIUM

---

## Need Advanced Features

### pidigits
**Category:** Arbitrary Precision
**Tests:** Bignum arithmetic, mathematical algorithms
**Features Needed:** Arbitrary precision integers (BigInt)
**Status:** NOT IMPLEMENTABLE without BigInt library
**Priority:** LOW

### regex-redux
**Category:** Text Processing
**Tests:** Regular expression matching, string replacement
**Features Needed:** Regex library
**Status:** NOT IMPLEMENTABLE without regex
**Priority:** LOW

### thread-ring / chameneos-redux
**Category:** Concurrency
**Tests:** Thread synchronization, message passing
**Features Needed:** Threads, synchronization primitives
**Status:** NOT IMPLEMENTABLE without concurrency
**Priority:** LOW (but important for real-world perf)

---

## Other Interesting Benchmarks

### brainfuck
**Category:** Interpreter
**Tests:** Interpreter implementation, instruction dispatch
**Features Needed:** String indexing, mutable state
**Priority:** MEDIUM - Tests language implementation capability

### json-parse
**Category:** Parsing
**Tests:** JSON parsing, tree construction
**Features Needed:** String processing, ADTs
**Priority:** MEDIUM

### mergesort
**Category:** Sorting
**Tests:** List splitting, merging, recursion
**Features Needed:** List operations
**Priority:** HIGH when list operations improve

### base64
**Category:** Encoding
**Tests:** Byte manipulation, encoding/decoding
**Features Needed:** Byte arrays, bitwise ops
**Priority:** LOW

---

## Feature Requirements Summary

| Feature | Benchmarks Blocked |
|---------|-------------------|
| Tail Call Optimization | sum_to_n (slow), all recursive benchmarks |
| Float stdlib (sqrt, etc.) | n-body, spectral-norm, mandelbrot |
| Efficient List operations | quicksort, mergesort, fannkuch |
| BigInt | pidigits |
| Regex | regex-redux |
| Threads | thread-ring, chameneos-redux |
| String indexing | brainfuck, fasta, k-nucleotide |
| Random numbers | fasta |

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
