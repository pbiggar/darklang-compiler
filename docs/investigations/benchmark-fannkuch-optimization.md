# Fannkuch Benchmark Optimization Investigation

## Summary

The fannkuch benchmark computes the maximum number of "pancake flips" needed to sort any permutation of n elements. The algorithm generates all n! permutations and counts flips for each.

**Key Finding:**
Dark uses **immutable linked lists (FingerTrees)** for representing permutations, while Rust and OCaml use **mutable arrays** with in-place operations. This is the dominant source of performance difference.

## Benchmark Source Code

### Dark (`benchmarks/problems/fannkuch/dark/main.dark`)
```dark
def reversePrefix(list: List<Int64>, k: Int64) : List<Int64> =
    let prefix = Stdlib.List.take<Int64>(list, k + 1) in
    let suffix = Stdlib.List.drop<Int64>(list, k + 1) in
    Stdlib.List.append<Int64>(Stdlib.List.reverse<Int64>(prefix), suffix)

def countFlips(perm: List<Int64>, flips: Int64) : Int64 =
    let first = getAt(perm, 0) in
    if first == 0 then flips
    else countFlips(reversePrefix(perm, first), flips + 1)
```

### Rust (`benchmarks/problems/fannkuch/rust/main.rs`)
```rust
fn fannkuch(n: usize) -> i32 {
    let mut perm: Vec<usize> = (0..n).collect();
    // ...
    while p[0] != 0 {
        let k = p[0];
        p[..=k].reverse();  // In-place array reversal
        flips += 1;
    }
}
```

### OCaml (`benchmarks/problems/fannkuch/ocaml/main.ml`)
```ocaml
let fannkuch n =
  let perm = Array.init n (fun i -> i) in
  (* ... *)
  for i = 0 to k / 2 do  (* In-place array swap *)
    let tmp = p.(i) in
    p.(i) <- p.(k - i);
    p.(k - i) <- tmp
  done;
```

## Analysis

### Data Structure Difference (Root Cause)

**Rust/OCaml**: Use mutable arrays with O(1) element access and O(k) in-place reversal.

**Dark**: Uses immutable FingerTree-based lists requiring:
- `getAt(list, i)`: O(log n) lookup through FingerTree
- `take(list, k)`: O(log n) split operation, allocates new tree nodes
- `drop(list, k)`: O(log n) split operation, allocates new tree nodes
- `reverse(list)`: O(n) operation creating new nodes
- `append(a, b)`: O(log n) concatenation

### Hot Loop: `countFlips` Function

**Rust inner loop** (~10 instructions per flip):
```asm
; Load first element
85c0: ldr    x1, [x20]           ; Load perm[0]
; Check if done
85c4: cbz    x1, 8560            ; If 0, exit
; Reverse slice in-place (vectorized)
85f8: ldp    q2, q3, [x20]       ; Load 32 bytes
8600: ext    v2.16b, v2.16b, ... ; Byte-swap via SIMD
8618: stp    q3, q2, [x10, #-32] ; Store reversed
; Increment counter
85cc: add    w8, w8, #0x1
```

**Dark inner loop** (~50+ instructions per flip, multiple function calls):
```
countFlips_body:
  ; Call getAt - O(log n) FingerTree lookup
  v12764 <- Call(getAt, [Reg v12763, Imm 0])

countFlips_L1:  ; When first != 0
  ; Call takeHelper - O(log n) split + allocation
  v12768 <- Call(Stdlib.List.__takeHelper_i64, [...])

  ; Call drop - O(log n) split + allocation
  v12771 <- Call(Stdlib.List.drop_i64, [...])

  ; Call reverse - O(n) traversal + allocation
  v12772 <- Call(Stdlib.List.reverse_i64, [Reg v12768])

  ; Call append - O(log n) concat + allocation
  v12773 <- Call(Stdlib.List.append_i64, [...])

  ; Loop back
  Jump(Label "countFlips_body")
```

### Hot Loop: `rotateLeft` Function

**Rust** (~15 instructions, in-place):
```rust
let t = perm[0];
for j in 0..i {
    perm[j] = perm[j + 1];
}
perm[i] = t;
```

**Dark** (7 function calls, multiple allocations):
```
rotateLeft_body:
  v12752 <- Call(getAt, [Reg v1704, Imm 0])        ; Get first
  v12753 <- Call(Stdlib.List.drop_i64, [...])      ; Drop 1
  v12755 <- Call(Stdlib.List.__takeHelper_i64, ...) ; Take i
  v12758 <- Call(Stdlib.List.drop_i64, [...])      ; Get rest
  v12759 <- Call(Stdlib.FingerTree.singleton_i64, ...) ; Wrap element
  v12761 <- Call(Stdlib.List.append_i64, [...])    ; First append
  TailCall(Stdlib.List.append_i64, [...])          ; Second append
```

### Memory Allocation Analysis

Each flip in Dark allocates:
1. FingerTree nodes for `take` result
2. FingerTree nodes for `drop` result
3. FingerTree nodes for `reverse` result
4. FingerTree nodes for `append` result

For n=9 (standard benchmark), this means **hundreds of allocations per permutation**, vs **zero allocations** per permutation in Rust/OCaml.

## Identified Optimization Opportunities

### 1. Add Mutable Array Type with In-Place Operations

**Impact: 10-50x performance improvement (estimated)**

**Root Cause:**
Dark lacks a mutable array type. The immutable FingerTree-based List requires O(log n) operations and allocations for operations that should be O(1) with arrays.

**Evidence from Dark IR:**
```
reversePrefix:
  v12743 <- Call(Stdlib.List.__takeHelper_i64, [...])  ; Allocates
  v12746 <- Call(Stdlib.List.drop_i64, [...])          ; Allocates
  v12748 <- Call(Stdlib.List.__reverseHelper_i64, ...) ; Allocates
  TailCall(Stdlib.List.append_i64, [...])              ; Allocates
```

**Evidence from Rust (in-place operations):**
```asm
85f8: ldp    q2, q3, [x20]       ; Load from memory
8618: stp    q3, q2, [x10, #-32] ; Store back (reversed)
```

**Implementation Approach:**
1. Add `Array<T>` type to Dark's type system
2. Implement `Array.get`, `Array.set`, `Array.reverseInPlace` as primitives
3. Lower to direct memory operations in LIR
4. Optionally: Add escape analysis to auto-promote immutable Lists to Arrays

**Files to Modify:**
- `src/DarkCompiler/Types.fs` - Add Array type
- `src/DarkCompiler/Stdlib.fs` - Add Array module with operations
- `src/DarkCompiler/Codegen.fs` - Lower Array ops to memory instructions

---

### 2. Specialize FingerTree for Small Lists

**Impact: 2-5x performance improvement for fannkuch**

**Root Cause:**
Fannkuch typically operates on lists of 6-9 elements. FingerTree has significant overhead for small lists due to tag checking, indirection, and node traversal.

**Evidence from Dark IR (tag checking overhead):**
```
Stdlib.FingerTree.getAt_i64:
  v11890 <- Call(Stdlib.FingerTree.__getTag_i64, [...])
  v11891 <- Call(Stdlib.FingerTree.__TAG_LEAF, [...])
  Cmp(v11890, Reg v11891)
  Branch(...)  ; Branch to different cases
```

For a 6-element list, `getAt(0)` requires:
1. Check if tree is Empty/Single/Deep
2. Navigate through FingerTree structure
3. Check node type (Leaf/Node2/Node3)
4. Extract value

**Implementation Approach:**
1. Add "small list" representation (inline array up to 8-16 elements)
2. Use tag bits to distinguish small lists from FingerTrees
3. Implement fast paths for small list operations
4. Automatic promotion to FingerTree when list grows

**Files to Modify:**
- `src/DarkCompiler/Stdlib/FingerTree.fs` - Add small list representation
- `src/DarkCompiler/Codegen.fs` - Add fast paths for small lists

---

### 3. Inline Hot Stdlib Functions

**Impact: 20-30% performance improvement**

**Root Cause:**
Every list operation involves a function call with full save/restore of registers. For `countFlips`, this is 5 function calls per iteration.

**Evidence from Dark LIR:**
```
countFlips_L1:
  SaveRegs([], [])
  v12768 <- Call(Stdlib.List.__takeHelper_i64, [...])
  RestoreRegs([], [])
  SaveRegs([], [])
  v12771 <- Call(Stdlib.List.drop_i64, [...])
  RestoreRegs([], [])
  SaveRegs([], [])
  v12772 <- Call(Stdlib.List.reverse_i64, [...])
  RestoreRegs([], [])
  SaveRegs([], [])
  v12773 <- Call(Stdlib.List.append_i64, [...])
  RestoreRegs([], [])
```

**Implementation Approach:**
1. Mark hot Stdlib functions as candidates for inlining
2. Extend ANF inliner to handle recursive Stdlib functions
3. Add inline threshold based on function size and call site frequency
4. Special case: inline `getAt` for constant indices (common pattern)

**Files to Modify:**
- `src/DarkCompiler/Passes/Inlining.fs` - Add Stdlib function inlining
- `src/DarkCompiler/Stdlib.fs` - Mark functions with `[<Inline>]` attribute

---

### 4. Optimize `getAt(list, 0)` Pattern

**Impact: 10-20% performance improvement**

**Root Cause:**
`getAt(list, 0)` (getting the first element) is extremely common in fannkuch. This should be O(1) but currently requires full FingerTree traversal.

**Evidence from Dark source:**
```dark
def countFlips(perm: List<Int64>, flips: Int64) : Int64 =
    let first = getAt(perm, 0) in  // Called every iteration
    ...

def rotateLeft(list: List<Int64>, i: Int64) : List<Int64> =
    let first = getAt(list, 0) in  // Called every rotation
    ...
```

**Implementation Approach:**
1. Add `List.head` function that's O(1) for FingerTrees
2. Pattern-match on `getAt(list, 0)` and replace with `head(list)`
3. Implement `head` as direct prefix access in FingerTree

**Files to Modify:**
- `src/DarkCompiler/Stdlib/List.fs` - Add `head` function
- `src/DarkCompiler/Passes/Optimization.fs` - Pattern match `getAt(_, 0)`

---

### 5. Reference Counting Overhead Reduction

**Impact: 5-15% performance improvement**

**Root Cause:**
Every list operation involves `RefCountInc`/`RefCountDec` calls, even when the list is only used linearly (single use before discard).

**Evidence from Dark LIR:**
```
getAt_L0:
  v12744 <- HeapLoad(v12738, 8)
  RefCountDec(v12738, 16)  // Decrement even though we just loaded
  v12745 <- Mov(Reg v12744)

nextPerm_L0:
  RefCountInc(v12791, 24)  // Increment for tuple
  RefCountInc(v12790, 24)  // Increment for tuple
  v12822 <- HeapAlloc(24)
```

**Implementation Approach:**
1. Add linearity analysis to detect single-use values
2. Eliminate RefCountInc/RefCountDec for linear values
3. Use "move semantics" for values that are consumed immediately

**Files to Modify:**
- `src/DarkCompiler/Passes/LinearityAnalysis.fs` - New pass
- `src/DarkCompiler/Passes/RefCountOptimization.fs` - Eliminate redundant ref counts

## Priority Recommendation

1. **Highest Priority**: Add mutable Array type (#1) - This is the fundamental issue
2. **High Priority**: Specialize for small lists (#2) - Good ROI for benchmarks
3. **Medium Priority**: Optimize getAt(0) pattern (#4) - Quick win
4. **Medium Priority**: Inline hot Stdlib functions (#3) - General improvement
5. **Lower Priority**: Reference counting optimization (#5) - More complex

## Appendix: Data Structure Comparison

| Operation | Dark (FingerTree) | Rust (Vec) | OCaml (Array) |
|-----------|-------------------|------------|---------------|
| get(i) | O(log n) + alloc check | O(1) | O(1) |
| reverse prefix k | O(n) + O(k) allocs | O(k) in-place | O(k) in-place |
| rotate left by 1 | O(log n) + allocs | O(i) in-place | O(i) in-place |
| set(i, v) | O(log n) + allocs | O(1) | O(1) |
