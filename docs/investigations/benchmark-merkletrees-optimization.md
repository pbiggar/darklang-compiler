# Merkletrees Benchmark Optimization Investigation

## Executive Summary

The Dark compiler currently generates **733,993,597 instructions** for the
merkletrees benchmark, which is **6.48x slower** than Rust
(113,304,119 instructions) and faster than OCaml (1,004,581,199 instructions at
8.87x).

This investigation currently tracks **3 remaining optimization opportunities**
and **2 opportunities that no longer reproduce**. The most impactful remaining
issue is a duplicate `buildTree` call in `benchmark` after `verifyTree` is
inlined, which rebuilds the same tree twice per iteration.

## Benchmark Overview

The merkletrees benchmark:
- Builds complete binary Merkle trees of depth 15 (32,768 leaves)
- Runs 50 iterations
- Each iteration builds a tree, verifies it by rebuilding, and accumulates a checksum
- Core hot path: `hashLoop` (8-iteration FNV-1a hash) called from `buildTree`
- Computes and compares root hashes only; despite the benchmark source comment,
  it does not allocate tree nodes in the current Dark, Rust, or OCaml programs.

## Performance Data

Current Cachegrind evidence for Dark, with cached Rust and OCaml baselines:

| Language | Instructions | vs Rust | Data refs | Branches | Mispred |
|----------|-------------:|--------:|----------:|---------:|--------:|
| Rust     | 113,304,119 | 1.00x | 19,760,299 | 3,322,450 | 12.8% |
| **Dark** | **733,993,597** | **6.48x** | **104,856,036** | **65,535,215** | **15.1%** |
| OCaml    | 1,004,581,199 | 8.87x | 226,897,520 | 124,769,269 | 8.6% |

The Dark run has about 5.3x Rust's data references and about 19.7x Rust's
branches. The branch delta is consistent with the current evidence below:
`benchmark` rebuilds the same tree twice per iteration, and `hashLoop` remains a
small recursive loop rather than Rust's unrolled straight-line hash sequence.

## Optimization Opportunities

### 1. CRITICAL: Duplicate buildTree Call in Benchmark Function

**Impact: ~50% reduction expected (eliminates half the work)**

#### Root Cause

After inlining `verifyTree`, the Dark compiler generates **two separate calls** to `buildTree` with identical arguments instead of reusing the first call's result.

#### Evidence from current ANF after inlining:

```
let TempId 42 = buildTree(t37, t39)
let TempId 62 = buildTree(t37, t39)   // duplicate call
let TempId 63 = t62 == t42
```

#### Evidence from current MIR:

```
v42 <- Call(buildTree, [v37, v39])
v62 <- Call(buildTree, [v37, v39])   // duplicate call
v63 <- v62 == v42 : TFunction ([TInt64; TInt64], TInt64)
```

#### Evidence from current LIR:

```
ArgMoves(X0 <- Reg X22, X1 <- Reg X24)
X20 <- Call(buildTree, [Reg X22, Reg X24])
...
ArgMoves(X0 <- Reg X22, X1 <- Reg X24)
X19 <- Call(buildTree, [Reg X22, Reg X24])   // duplicate call
```

#### Analysis

The original Dark code:
```dark
let root = buildTree(depth, i) in
let verified = verifyTree(depth, i, root) in
```

Where `verifyTree` is:
```dark
let verifyTree(depth, leafStart, expectedRoot) =
    buildTree(depth, leafStart) == expectedRoot
```

After inlining `verifyTree`, this should become:
```
let root = buildTree(depth, i)
let verified = (buildTree(depth, i) == root)  // Should reuse root!
```

The compiler should recognize that `buildTree(depth, i)` is a pure function and
the second call is redundant with `root`. Existing CSE does not eliminate this
recursive function call.

#### Implementation Approach

1. Extend CSE to safely cover pure function calls, including recursive calls.
2. Hash the `(function, args)` tuple and check for an existing binding.
3. Replace duplicate calls with a reference to the existing result.

#### Files to Modify
- `src/DarkCompiler/ANFOptimization.fs` - Extend CSE to eligible calls
- `src/DarkCompiler/ANF.fs` - May need purity annotations

---

### 2. Partial Loop Unrolling for hashLoop

**Status: factor-two counted-loop unrolling implemented**

#### Root Cause

The `hashLoop` function iterates 8 times (fixed count) but Dark compiles it as a recursive tail-call loop, while Rust completely unrolls it into 8 inline XOR/MUL sequences.

#### Evidence from current Dark LIR:

```asm
hashLoop_L1:
  X2 <- Eor(X2, X1)
  X2 <- Mul(X2, Reg X4)
  X3 <- Add(X3, Imm 1)
  Jump(Label "hashLoop_body")
hashLoop_body:
  Cmp(X3, Imm 8)
  CondBranch(GE, Label "hashLoop_L2", Label "hashLoop_L1")
```

Factor-two MIR unrolling now executes two consecutive scalar iterations per
backedge and uses a safe remainder return for an odd final trip. This reduces
the routine benchmark from 724,164,728 to 684,843,728 instructions (5.4%). The
loop is still not fully unrolled across its fixed eight iterations.

#### Evidence from Rust disassembly (lines 5327-5349):

```asm
; Completely unrolled - no loop, no branches
ca090109   eor x9, x8, x9
9b0a7d29   mul x9, x9, x10
ca080129   eor x9, x9, x8
9b0a7d29   mul x9, x9, x10
ca080129   eor x9, x9, x8
9b0a7d29   mul x9, x9, x10
... (repeated 8 times total)
```

Total: 16 instructions (2 per iteration, no loop overhead)

#### Implementation Approach

1. Consider full unrolling when the exact small trip count is known
2. Inline `hashLoop` entirely into callers before evaluating further growth

#### Files to Modify
- `src/DarkCompiler/ANFOptimization.fs` - Add unrolling heuristics
- `src/DarkCompiler/ANFInlining.fs` - Consider aggressive inlining for small recursive functions

---

### 3. Function Call Overhead for hashLoop

**Impact: ~15-20% reduction (combined with unrolling)**

#### Root Cause

Dark makes actual function calls to `hashLoop` from `buildTree`, incurring:
- Stack frame setup/teardown
- Register save/restore
- Branch prediction overhead

Rust inlines `hash()` into `build_tree()` completely.

#### Evidence from Dark LIR (buildTree_L0, lines 619-626):

```
SaveRegs([], [])
ArgMoves(X0 <- Imm -3750763034362895579, X1 <- Reg v18, X2 <- Imm 0)
v10077 <- Call(hashLoop, [Imm -3750763034362895579, Reg v18, Imm 0])
RestoreRegs([], [])
```

#### Evidence from Rust disassembly (build_tree):

The hash code is completely inlined - no `call` instructions to a separate hash function.

#### Implementation Approach

1. Mark `hashLoop` as an inlining candidate
2. Inline small recursive functions with known bounds
3. After inlining, loop unrolling can eliminate the recursion entirely

#### Files to Modify
- `src/DarkCompiler/ANFInlining.fs` - Increase inlining threshold for recursive functions with small bodies

---

## Comparison with Rust

### Rust build_tree (lines 5306-5350):

```asm
; Function is ~100 instructions
; Hash loop is completely unrolled inline
; No function calls except recursive build_tree calls
; Zero-cost abstraction - hash_pair and hash are both inlined
```

### Dark buildTree:

```asm
; Function makes 3 function calls per invocation:
;   - 2x buildTree (recursive)
;   - 1x hashLoop (should be inlined)
; Plus duplicate buildTree call in benchmark
```

## Recommended Priority

1. **Eliminate duplicate buildTree call** - largest remaining benchmark-specific opportunity.
2. **Inline hashLoop into buildTree** - removes hot-path call overhead.
3. **Full unrolling for fixed small trip counts** - factor-two unrolling is complete; reassess only after inlining.

## Estimated Combined Impact

With all optimizations implemented, Dark could potentially achieve:
- Current: 733,993,597 instructions (6.48x vs Rust)
- Target: ~300-400M instructions (~3-4x vs Rust)

Dark is already ahead of OCaml on this benchmark by instruction count; the
remaining opportunities would move it closer to Rust.
