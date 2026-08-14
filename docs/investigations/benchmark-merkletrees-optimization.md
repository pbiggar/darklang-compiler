# Merkletrees Benchmark Optimization Investigation

## Executive Summary

The Dark compiler currently generates **416,150,237 instructions** for the
merkletrees benchmark, which is **3.34x** the audited Rust instruction count
(124,776,610).

Bounded recursive-loop unrolling now removes both loop control and call overhead
from the fixed eight-round hash. The intentional second `buildTree` execution
remains: verification rebuilds the tree in every reference implementation.

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
| Rust     | 124,776,610 | 1.00x | 32,868,514 | 3,323,129 | 12.8% |
| **Dark** | **416,150,237** | **3.34x** | **78,642,352** | **6,553,846** | **12.8%** |
| OCaml    | 1,004,581,199 | 8.05x | 226,897,520 | 124,769,269 | 8.6% |

The Dark run now has about 2.4x Rust's data references and 2.0x Rust's branches.
The remaining work includes the intentional verification rebuild; `hashLoop`
call sites now use an unrolled straight-line hash sequence.

## Findings

### 1. REQUIRED: Two buildTree Executions

**Status: intentionally preserved for reference parity**

#### Source behavior

After inlining `verifyTree`, the Dark compiler correctly retains two separate
calls to `buildTree` with identical arguments. The second execution verifies
the first result and is also present in the Rust and OCaml reference sources.

#### Evidence from current ANF after inlining:

```
let TempId 42 = buildTree(t37, t39)
let TempId 62 = buildTree(t37, t39)   // verification rebuild
let TempId 63 = t62 == t42
```

#### Evidence from current MIR:

```
v42 <- Call(buildTree, [v37, v39])
v62 <- Call(buildTree, [v37, v39])   // verification rebuild
v63 <- v62 == v42 : TFunction ([TInt64; TInt64], TInt64)
```

#### Evidence from current LIR:

```
ArgMoves(X0 <- Reg X22, X1 <- Reg X24)
X20 <- Call(buildTree, [Reg X22, Reg X24])
...
ArgMoves(X0 <- Reg X22, X1 <- Reg X24)
X19 <- Call(buildTree, [Reg X22, Reg X24])   // verification rebuild
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

After inlining `verifyTree`, this becomes:
```
let root = buildTree(depth, i)
let verified = (buildTree(depth, i) == root)
```

Eliminating the second call would change the audited workload rather than
optimize its execution, so call CSE must not be applied to this case.

---

### 2. RESOLVED: Loop Unrolling for hashLoop

**Measured impact: 42.5% reduction in whole-benchmark instructions**

#### Previous Root Cause

The `hashLoop` function iterates 8 times (fixed count) but Dark compiles it as a recursive tail-call loop, while Rust completely unrolls it into 8 inline XOR/MUL sequences.

#### Evidence from Dark LIR before bounded unrolling:

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

Each iteration paid loop control overhead: three body instructions plus a
compare and branch in `hashLoop_body`.

Factor-two MIR unrolling subsequently reduced the loop to four backedges. The
bounded ANF expansion supersedes that partial optimization at eligible call
sites by removing the recursive call and all eight iterations of loop control.

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

#### Implementation

ANF inlining recognizes direct scalar recursion with a literal bound and entry,
proves unit induction, and clones the primitive body. Default caps allow no more
than eight iterations or 48 expanded bindings at one call site. The generated
`buildTree` paths contain eight ordered XOR/multiply pairs and no `hashLoop`
call.

The transformation preserves the original ANF primitive operations, including
wrapping `Int64` multiplication and XOR. Quick and routine Cachegrind counts
both improved by 42.5%.

---

### 3. RESOLVED: Function Call Overhead for hashLoop

**Impact: included in the measured 42.5% whole-benchmark reduction**

#### Previous Root Cause

Dark made actual function calls to `hashLoop` from `buildTree`, incurring:
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

#### Implementation

The bounded-loop expansion happens at eligible literal-entry call sites, so it
simultaneously removes recursive-loop control and the caller's function call.
Non-eligible calls retain the original recursive function.

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
; Internal nodes make 2x buildTree recursive calls.
; The hash itself is eight inlined XOR/multiply pairs.
; Verification intentionally invokes buildTree a second time.
```

## Recommended Priority

The two `buildTree` executions are intentional parity behavior and must remain.
Further work should target general compiler overhead without removing the
verification rebuild.

## Estimated Combined Impact

Current bounded unrolling produces 416,150,237 instructions (3.34x Rust), down
from 724,164,737 (5.80x). Dark remains ahead of OCaml by instruction count while
preserving the reference workload's two tree builds.
