# Fasta Benchmark Optimization Investigation

## Overview

This investigation analyzes why the Dark compiler performs worse than Rust and OCaml on the fasta benchmark, which generates pseudo-random DNA sequences using a linear congruential generator (LCG) and computes a checksum.

## Current Local Evidence

Measured in the current `benchmarks/RESULTS.md`, `fasta` runs at
`2,493,333,492` Dark instructions, or `116.3x` the Rust baseline. The benchmark
still produces the expected checksum `830939461`.

The current Dark executable built for `benchmarks/problems/fasta/dark/main.dark`
is `209,760` bytes on Linux ARM64. That means the older shared OOM trap note
claiming the binary had shrunk to about `114 KB` is no longer current for this
checkout.

### Allocator Guard Cost Remains Allocation-Sensitive

`withHeapBoundsCheck` in `6_CodeGen.fs` adds a bounds-check sequence on every bump allocation:

1. `MOVZ` load heap-size high bits
2. `ADD` compute heap end from heap base
3. `ADD` compute next pointer
4. `CMP` compare next pointer to heap end
5. `B.LE` branch to fast path

That is 5 additional instructions and 1 additional conditional branch per allocation attempt.

`fasta` performs enough allocations that this overhead remains relevant to both
instruction count and generated code size. Current IR evidence still shows large
constant-list construction in `_start` through repeated `RawAlloc(16)` nodes,
plus per-iteration heap allocation for the `makeRandomFasta` return tuple.
The remaining allocator-bound cost is still dominated by per-allocation heap-end
recomputation (`MOVZ + ADD`) plus the extra bounds-check branch.

### Remaining Fix Strategy

The direct dedicated-register heap-end hoist experiment was rejected because
reserving an allocatable callee-saved register caused larger benchmark
regressions than the per-allocation instruction savings justified. See
`docs/investigations/rejected-experiments.md`.

Remaining allocator-bound candidates should avoid permanently removing a
general-purpose register from allocation. Possible directions include:

1. Recompute heap end only where register pressure is already low, if that can
   be proven locally.
2. Keep checks only on the bump path (free-list pop path should remain
   check-free).

Avoid moving bump allocation to a helper function call in the hot path:
call/return and call-site save/restore overhead would likely outweigh the
current regression.

## Benchmark Characteristics

The fasta benchmark has three hot loops:
1. `makeRepeatFasta`: Iterates over a repeated ALU sequence (200,000 iterations)
2. `makeRandomFasta`: Generates random DNA using LCG (300,000 + 500,000 = 800,000 iterations)
3. `selectRandom`: Called within makeRandomFasta to select DNA character based on cumulative probability (up to 15 comparisons per call)

## Key Findings

### 1. List vs Array Access Pattern

**Dark**: Uses immutable finger trees for lists, requiring O(log n) access per element
**Rust**: Uses direct array indexing with O(1) access
**OCaml**: Uses mutable arrays with O(1) access

#### Evidence from IR

Dark's `getAlu` function (ANF after optimization):
```
Function getAlu:
let TempId 1286 = getAlu(t1281, t1283)
  -> Calls Stdlib.FingerTree.getAt_i64 which involves:
     - Tag checking
     - Multiple function calls for tree traversal
     - HeapLoad operations
```

Dark LIR for `makeRepeatFasta`:
```
X22 <- Mov(Imm 287)
X20 <- Sdiv(X21, Reg X22)      ; Division for modulo
X19 <- Msub(X20, X22, X21)     ; Remainder
SaveRegs([X1, X2, X3, X4, X5, X6, X7], [])
ArgMoves(X0 <- Stack -16, X1 <- Reg X19)
X20 <- Call(Stdlib.FingerTree.getAt_i64, ...)  ; FUNCTION CALL per element access!
RestoreRegs([X1, X2, X3, X4, X5, X6, X7], [])
```

Rust disassembly (tight loop with direct array access):
```asm
ldrb w0, [x13, x0]   ; Direct byte load from array
madd x8, x15, x0, x8  ; Multiply-accumulate for checksum
```

**Impact**: ~10-50x slowdown on list access patterns (200,000+ operations)

### 2. Function Call Overhead in selectRandom

**Dark**: Calls `Stdlib.List.length_tup_i64_f64` on every iteration of selectRandom
**Rust**: Precomputes cumulative probability thresholds, uses unrolled comparisons
**OCaml**: Uses `Array.iter` with closures, also O(1) array access

#### Evidence from LIR

Dark's `selectRandom` body:
```
selectRandom_body:
  SaveRegs([], [D2])
  ArgMoves(X0 <- Reg X24, X1 <- Reg X23)
  X22 <- Call(Stdlib.FingerTree.getAt_tup_i64_f64, ...)  ; Tree access
  RestoreRegs([], [D2])
  ...
  X19 <- Call(Stdlib.List.length_tup_i64_f64, ...)  ; Length call EVERY iteration
```

Rust disassembly shows fully unrolled comparisons:
```asm
fcmp d23, d0        ; Compare against preloaded threshold
b.pl 87c8           ; Branch if >=
mov w3, #0x0        ; Return index 0
...
fcmp d23, d1        ; Compare against next threshold
b.pl 87d8
mov w3, #0x10       ; Return index 1
; ... continues for all 15 thresholds
```

**Impact**: ~5-20x slowdown in selectRandom due to function calls + tree traversal

### 3. Tuple Allocation and Access

**Dark**: Creates heap-allocated tuples, requires HeapLoad for field access
**Rust**: Uses struct fields directly in registers
**OCaml**: Also heap allocates but OCaml runtime is highly optimized for this

#### Evidence from LIR

Dark's makeRandomFasta return path:
```
makeRandomFasta_L0:
  X19 <- HeapAlloc(16)           ; Allocate tuple on heap
  HeapStore(X19, 0, Reg X25)     ; Store first field
  HeapStore(X19, 8, Reg X20)     ; Store second field
```

Rust keeps values in registers throughout the loop without heap allocation.

**Impact**: ~2-5x slowdown due to heap allocation overhead

### 4. Lack of Loop Strength Reduction

**Dark**: Computes `i + 1` twice per iteration in makeRandomFasta
**Rust/OCaml**: Use single increment

#### Evidence from ANF

```
let TempId 1359 = t1346 + 1     ; First computation of i+1
let TempId 1360 = t1357 * t1359
...
let TempId 1364 = t1346 + 1     ; Second computation of i+1 (redundant!)
```

**Impact**: Minor (~5-10% overhead)

### 5. Float Division Instead of Multiplication

**Dark**: Uses division by constant `139968.0`
**Rust**: Uses multiplication by reciprocal (precomputed)
**OCaml**: Also uses division but in native OCaml floats

#### Evidence

Dark LIR:
```
D0 <- IntToFloat(X22)
fv1001 <- FLoad(float[139968])
D0 <- FDiv(D0, fv1001)          ; Division (slow)
```

**Impact**: ~2-3x slowdown on this specific operation (division vs multiplication)

## Instruction Count Comparison

| Function | Dark (estimated ops/iter) | Rust (ops/iter) | Ratio |
|----------|---------------------------|-----------------|-------|
| makeRepeatFasta | ~50-100 (with calls) | ~10 | 5-10x |
| makeRandomFasta | ~100-200 (with calls) | ~30-40 | 3-5x |
| selectRandom | ~30-50 (with calls) | ~15-20 (unrolled) | 2x |

## Optimization Suggestions

### Optimization 1: Convert Known-Length Lists to Arrays at Compile Time

**Title**: Static List-to-Array Conversion for Constant Lists

**Impact Estimate**: 5-10x improvement in list access patterns

**Root Cause**: The ALU sequence and probability tables are known at compile time but stored as finger trees, requiring O(log n) access.

**Implementation Approach**:
1. In ANF optimization phase, detect list literals with constant values
2. Convert to array representation (flat memory) at compile time
3. Replace `List.getAt` calls with direct memory loads
4. Files to modify:
   - `src/DarkCompiler/ANFOptimizer.fs` - Add constant list detection
   - `src/DarkCompiler/LIRGen.fs` - Generate direct array access

**Evidence**:
```
// Dark source
let aluBytes = [71, 71, 67, 67, ...]  // 287 constant integers
// Currently generates:
let TempId 1366 = RawAlloc(8)
let TempId 1367 = RawSet(t1366, 0, 71)
let TempId 1368 = t1366 | 5
// ... 287 allocations
```

### Optimization 2: Inline Small Table Lookups

**Title**: Specialize selectRandom for Small Cumulative Tables

**Impact Estimate**: 3-5x improvement in random selection

**Root Cause**: selectRandom is called 800,000 times with tables of 4 or 15 elements, but each call involves tree traversal and length calculation.

**Implementation Approach**:
1. Detect when table argument is a small constant-size list
2. Generate specialized unrolled comparison sequence
3. Eliminate List.getAt and List.length calls
4. Files to modify:
   - `src/DarkCompiler/ANFOptimizer.fs` - Add specialization pass
   - `src/DarkCompiler/Inliner.fs` - Enable aggressive inlining for these patterns

**Evidence from Rust** (ideal code generation):
```asm
fcmp d23, d0        ; Compare r < cumulative[0]
b.pl next           ; If >= continue
mov w3, #0          ; Return character 0
```

## Summary

The primary performance gap between Dark and Rust/OCaml on the fasta benchmark comes from:

1. **Data structure overhead** (finger trees vs arrays): 5-10x
2. **Function call overhead** in hot loops: 3-5x

Combined estimated improvement potential: **10-30x** if all optimizations are implemented.

## Recommended Priority

1. **High**: Static List-to-Array Conversion (biggest impact)
2. **High**: Specialize selectRandom for small tables
