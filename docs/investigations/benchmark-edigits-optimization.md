# Edigits Benchmark Optimization Investigation

## Executive Summary

The **edigits** benchmark computes digits of e using Taylor series with fixed-point arithmetic. Dark now runs the canonical 1,000-digit computation for ten iterations and produces the shared checksum `2162278`.

**Current Status:** Working at canonical settings. The focused 2026-08-04 Cachegrind run measured:
- Dark: 523,088,517 instructions (38.4x Rust)
- Rust: 13,613,164 instructions (baseline)
- OCaml: 36,973,499 instructions (2.72x)
- Node: 493,137,288 instructions (36.2x)
- Python: 939,948,865 instructions (69.0x)

**Resolved parity gap:** The old Dark benchmark used one `Int64` list element per decimal digit, ran only 50 digits for one iteration, and carried a Dark-specific expected output. Promoting that implementation unchanged to canonical settings completed but cost 55,678,531,639 instructions (4,090x Rust). Packing 15 decimal digits into each overflow-safe `Int64` chunk reduces the working set from 1,010 digit elements to 68 chunks and cuts that canonical instruction count by 99.1% without adding mutable arrays.

**Root-cause classification:** Benchmark implementation plus benchmark settings/expected-output and status-documentation drift. The previously documented stack overflow is not reproducible on current `main`; no compiler/runtime change was required.

The analysis below records the original digit-at-a-time implementation and remains useful for future mutable-array or persistent-list optimization work.

## Benchmark Algorithm Analysis

### Historical Core Algorithm
The benchmark computes e using Taylor series: `e = 1 + 1/1! + 1/2! + 1/3! + ...`

### Hot Paths
1. **divideByN**: Divides each digit by n, propagating carry (called 50x * precision iterations)
2. **addWithCarry**: Adds term to e with carry propagation (called 50x * precision iterations)
3. **computeChecksum**: Sums digits weighted by position (called 10x)

### Per-Iteration Complexity
- Outer loop: 50 iterations (1 to 50)
- Each `divideByN`: precision (~1010) list operations
- Each `addWithCarry`: precision list operations
- Total: ~50 * 2 * 1010 = ~101,000 list operations per digit computation

## Performance Analysis

### 1. Data Structure Mismatch (CRITICAL)

**Rust (tight array loop) - Lines 5283-5293:**
```asm
8514:   ldr  x9, [x20, x11, lsl #3]    // Array load: O(1)
8518:   madd x10, x10, x24, x9         // carry * 10 + digit
851c:   cmp  x11, #0x3f2               // Compare index
8524:   udiv x12, x10, x8              // newDigit = current / n
8528:   str  x12, [x20, x11, lsl #3]   // Array store: O(1)
8530:   msub x10, x12, x8, x10         // newCarry = current % n
853c:   b.cc 8514                      // Loop back
```
**Key:** 7 instructions per iteration, all O(1) memory ops.

**OCaml (mutable array) - Lines 3720-3765:**
```asm
53368:  add  x25, x0, x1, lsl #2       // Compute array address
5336c:  ldur x3, [x25, #-4]            // Array load: O(1)
53374:  madd x3, x21, x5, x3           // carry * 10 + digit
5338c:  sdiv x6, x6, x5                // newDigit = current / n
533bc:  asr  x12, x4, #1               // Compute mod
533c8:  msub x14, x16, x12, x13        // newCarry
533b8:  stur x9, [x11, #-4]            // Array store: O(1)
53404:  b.hi 5335c                     // Loop back
```
**Key:** ~15 instructions per iteration, O(1) memory ops (boxed integers add overhead).

**Dark (FingerTree getAt/setAt) - LIR Lines 51599-51642:**
```
divideByN_L1:
    SaveRegs([], [])
    ArgMoves(X0 <- X26, X1 <- X24)
    Call(Stdlib.FingerTree.getAt_i64, ...)     // O(log n) tree traversal!
    RestoreRegs([], [])
    X19 <- HeapLoad(X20, 0)                    // Check Some/None
    Cmp(X19, Imm 1)                            // Option tag check
    CondBranch(...)
    ...
    Call(Stdlib.FingerTree.setAt_i64, ...)     // O(log n) tree modification!
    ...
    Jump(divideByN_body)
```
**Key:** 2 function calls per iteration, each O(log n). For precision=1010:
- log2(1010) ≈ 10 levels of tree traversal
- Each traversal involves multiple pointer chases, allocations, tag checks

### 2. Reference Counting Overhead

**Dark MIR shows explicit RC operations in addWithCarry:**
```
addWithCarry_L1:
    RefCountInc(v1393, size=16)    // Increment RC on Option
    RefCountInc(v1395, size=16)    // Increment RC on Option
    v1397 <- HeapAlloc(16)         // Allocate tuple
    ...
addWithCarry_L4:
    RefCountDec(v1397, size=16)    // Decrement tuple
    RefCountDec(v2022, size=16)    // Decrement Option
    RefCountDec(v2021, size=16)    // Decrement Option
```

Each iteration of `addWithCarry` performs:
- 2 RefCountInc operations
- 1 HeapAlloc for tuple
- 3 RefCountDec operations
- Potential free list operations

### 3. Option Type Overhead

Every `List.getAt` returns `Option<Int64>`:
```
divideByN_L4:
    X19 <- HeapLoad(X20, 0)    // Load Option tag
    Cmp(X19, Imm 1)            // Check if None (tag=1) vs Some (tag=0)
    CondBranch(EQ, L3, L4)     // Branch on None
divideByN_L4:
    X19 <- HeapLoad(X20, 8)    // Load actual value from Some
```

In the tight inner loop, we know indices are always valid, but:
- Option allocation occurs on every getAt
- Tag check and branching on every access
- Additional HeapLoad to unwrap the value

### 4. Tuple Allocation in Pattern Matching

**Dark addWithCarry creates a tuple to pattern match:**
```
addWithCarry_L1:
    ...
    X22 <- HeapAlloc(16)           // Allocate (eOpt, termOpt) tuple
    HeapStore(X22, 0, Reg X23)     // Store first Option
    HeapStore(X22, 8, Reg X21)     // Store second Option
    X19 <- HeapLoad(X22, 0)        // Load back first Option
    X19 <- HeapLoad(X19, 0)        // Load first Option's tag
    ...
    X19 <- HeapLoad(X22, 8)        // Load back second Option
    X19 <- HeapLoad(X19, 0)        // Load second Option's tag
```

This creates a heap-allocated tuple every iteration just for pattern matching.

### 5. Instruction Count Comparison

For the inner `divideByN` loop (per iteration):

| Operation | Rust | OCaml | Dark (estimated) |
|-----------|------|-------|------------------|
| Array/List access | 1 instr | 2 instr | ~50 instr (FingerTree) |
| Carry computation | 2 instr | 3 instr | 3 instr |
| Division/mod | 2 instr | 4 instr | 4 instr |
| Array/List update | 1 instr | 2 instr | ~50 instr (FingerTree) |
| Index increment | 1 instr | 2 instr | 2 instr |
| Loop overhead | 1 instr | 2 instr | 5 instr (call/ret/saves) |
| RC operations | 0 | 0 | ~10 instr |
| Option handling | 0 | 0 | ~5 instr |
| **Total** | **~8** | **~15** | **~130** |

**Estimated instruction ratio: Dark/Rust ≈ 16x per iteration**

With O(log n) FingerTree overhead, actual ratio could be **50-100x** for large lists.

## Optimization Suggestions

### Optimization 1: Array Primitive Type (High Impact)

**Title:** Add mutable Array<T> primitive type for O(1) random access

**Impact Estimate:** 10-50x improvement for array-heavy benchmarks

**Root Cause:**
- Current `List<T>` is implemented as immutable FingerTree
- Every getAt/setAt is O(log n) vs O(1) for arrays
- Benchmark IR shows `Call(Stdlib.FingerTree.getAt_i64, ...)` and `Call(Stdlib.FingerTree.setAt_i64, ...)` in hot loops

**Evidence from IR:**
```
divideByN_L1:
    Call(Stdlib.FingerTree.getAt_i64, [Reg X26, Reg X24])  // O(log n)
    ...
    Call(Stdlib.FingerTree.setAt_i64, [Reg X26, Reg X24, Reg X19])  // O(log n)
```

**Implementation Approach:**
1. Add `Array<T>` type to type system in `src/types.rs`
2. Implement `Stdlib.Array.make`, `Array.get`, `Array.set` with O(1) semantics
3. Generate inline memory access in LIR (like Rust's `ldr`/`str` with offset)
4. Consider mutation semantics (copy-on-write vs true mutation)

**Files to Modify:**
- `src/types.rs` - Add Array type
- `src/stdlib/array.rs` - Implement Array operations
- `src/ir/lir.rs` - Add array access lowering
- `src/codegen/arm64.rs` - Generate efficient array access

### Optimization 2: Inline List Access for Known-Bounds Access (Medium Impact)

**Title:** Specialize List.getAt/setAt when bounds are statically known

**Impact Estimate:** 2-5x improvement

**Root Cause:**
- Every `List.getAt` returns `Option<T>` requiring allocation and checks
- In tight loops with valid indices, this is pure overhead

**Evidence from LIR:**
```
divideByN_L4:
    X19 <- HeapLoad(X20, 0)    // Load Option tag
    Cmp(X19, Imm 1)            // Check None vs Some
    CondBranch(EQ, L3, L4)     // Branch
    X19 <- HeapLoad(X20, 8)    // Unwrap value
```

**Implementation Approach:**
1. Add `List.getUnsafe(list, idx)` intrinsic that returns `T` directly
2. Perform bounds checking at ANF optimization when indices can be proven valid
3. Lower to direct memory access without Option wrapping

**Files to Modify:**
- `src/stdlib/list.rs` - Add getUnsafe/setUnsafe
- `src/ir/anf_optimize.rs` - Add bounds analysis
- `src/ir/lir.rs` - Generate direct access

### Optimization 3: Eliminate Tuple Allocation in Multi-Pattern Match (Medium Impact)

**Title:** Lower multi-value pattern matches without intermediate allocation

**Impact Estimate:** 2-3x improvement for functions using tuple patterns

**Root Cause:**
The addWithCarry function pattern matches on `(eOpt, termOpt)`:
```dark
match (eOpt, termOpt) with
| (Some(eDigit), Some(termDigit)) -> ...
```

This compiles to tuple heap allocation:
```
X22 <- HeapAlloc(16)
HeapStore(X22, 0, Reg X23)
HeapStore(X22, 8, Reg X21)
```

**Evidence from MIR:**
```
addWithCarry_L1:
    v1397 <- HeapAlloc(16)
    HeapStore(v1397, 0, v1393)
    HeapStore(v1397, 8, v1395)
```

**Implementation Approach:**
1. Detect patterns like `match (a, b) with | (P1, P2) -> ...`
2. Rewrite to nested matches or direct register comparisons
3. Avoid heap allocation for temporary tuples

**Files to Modify:**
- `src/ir/ast_to_anf.rs` - Pattern match lowering
- `src/ir/anf_optimize.rs` - Tuple elimination pass

### Optimization 4: Reduce Reference Counting in Hot Loops (Medium Impact)

**Title:** Defer reference counting operations for loop-local values

**Impact Estimate:** 1.5-2x improvement

**Root Cause:**
Every iteration performs RefCountInc/Dec:
```
addWithCarry_L1:
    RefCountInc(X23, 16)
    RefCountInc(X21, 16)
    ...
addWithCarry_L4:
    RefCountDec(X22, 16)
    RefCountDec(X21, 16)
    RefCountDec(X23, 16)
```

**Evidence from LIR:**
```
51674:  RefCountInc(X23, 16)
51675:  RefCountInc(X21, 16)
...
51727:  RefCountDec(X22, 16)
51728:  RefCountDec(X21, 16)
51729:  RefCountDec(X23, 16)
```

**Implementation Approach:**
1. Identify values that are loop-local and don't escape
2. Batch RC increments at loop entry, decrements at exit
3. Use RC elision for values provably not shared

**Files to Modify:**
- `src/ir/rc_insertion.rs` - Add loop-aware RC optimization
- `src/ir/mir_optimize.rs` - Loop analysis pass

### Optimization 5: Inline Small Stdlib Functions (Low Impact)

**Title:** Inline constant-returning tag functions

**Impact Estimate:** 1.1-1.2x improvement

**Root Cause:**
Tag functions like `__TAG_SINGLE()` are called repeatedly:
```
SaveRegs([], [])
X19 <- Call(Stdlib.FingerTree.__TAG_SINGLE, [])
RestoreRegs([], [])
```

These just return a constant (e.g., `return 1`).

**Evidence from LIR:**
```
Stdlib.FingerTree.__TAG_SINGLE:
    X0 <- Mov(Imm 1)
    Ret
```

**Implementation Approach:**
1. Mark trivial functions (return constant) for mandatory inlining
2. Replace call sites with immediate values during LIR optimization

**Files to Modify:**
- `src/ir/lir_optimize.rs` - Add trivial function inlining
- `src/ir/inline.rs` - Identify trivial functions

## Priority Ranking

1. **Array Primitive Type** - Critical for this and similar benchmarks
2. **Tuple Elimination** - High-frequency allocation in pattern matches
3. **Reference Counting Optimization** - Pervasive overhead
4. **Inline List Access** - Removes Option overhead
5. **Inline Tag Functions** - Minor but easy win

## Expected Improvement

With Array type alone: **10-50x improvement** (matching OCaml's 2.72x baseline)
With all optimizations: **Could approach Rust's performance** (within 2-3x)

## Validation Outcome

1. Promoted Dark to the shared 1,000-digit, ten-iteration settings.
2. Removed the reduced Dark-only expected-output override so the shared canonical checksum applies.
3. Reproduced the digit-at-a-time canonical implementation at 55,678,531,639 instructions.
4. Reworked the benchmark around 15-digit `Int64` chunks and measured 523,088,517 instructions with matching output.

A mutable array type could narrow the remaining 38.4x Rust performance gap, but it is no longer required for benchmark functionality parity.

## Appendix: Full IR Dumps

See `/tmp/dark_ir_dump.txt` for complete Dark compiler output including:
- ANF before/after optimization
- MIR with control flow
- LIR before/after register allocation

See `/tmp/rust_disasm.txt` and `/tmp/ocaml_disasm.txt` for comparison assembly.
