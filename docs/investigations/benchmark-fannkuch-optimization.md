# Fannkuch Benchmark Optimization Investigation

> Historical note: the measurements below used a permutation counter initialized
> with `0..n-1`, which skipped permutations and incorrectly returned `23` for
> `n=9`. The audited implementation initializes counts with `1..n` and returns
> the correct `30`. Rust retains `n=9`; Dark runs the complete `n=8` traversal
> because correct `n=9` exhausts its fixed heap, so full fannkuch is no longer a
> canonical comparison. Both quick variants correctly use `n=4` and return `4`.

## Summary

The fannkuch benchmark computes the maximum number of "pancake flips" needed to sort any permutation of n elements. The algorithm generates all n! permutations and counts flips for each.

**Key Finding:**
Dark uses **immutable SkewList-backed lists** for representing permutations, while Rust and OCaml use **mutable arrays** with in-place operations. Current cachegrind evidence shows this remains the dominant source of the performance difference: Dark executes 6,606,716,903 instructions for `fannkuch`, 491.1x Rust and 211.9x OCaml.

## Benchmark Source Code

### Dark (`benchmarks/problems/fannkuch/dark/main.dark`)
```dark
let reversePrefix(list: List<Int64>, k: Int64) : List<Int64> =
    let prefix = Stdlib.List.take<Int64>(list, k + 1) in
    let suffix = Stdlib.List.drop<Int64>(list, k + 1) in
    Stdlib.List.append<Int64>(Stdlib.List.reverse<Int64>(prefix), suffix)

let countFlips(perm: List<Int64>, flips: Int64) : Int64 =
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

### Current Benchmark Evidence

Current local cachegrind evidence from `./benchmarks/run_benchmarks.sh fannkuch`:

| Language | Instructions | vs Rust | Data Refs | Branches |
|----------|--------------|---------|-----------|----------|
| Rust | 13,453,488 | baseline | 5,901,371 | 2,031,853 |
| OCaml | 31,185,180 | 2.3x | 10,530,754 | 5,946,748 |
| Dark | 6,606,716,903 | 491.1x | 3,025,615,009 | 896,553,196 |

The Dark result is not just branch-heavy. The decisive gap is data traffic: the Dark run performs roughly 513x the Rust data references, matching the list reconstruction and reference-count churn visible in LIR. A correctness smoke check with `./dark -r benchmarks/problems/fannkuch/dark/main.dark` prints `23` and exits 0.

### Data Structure Difference (Root Cause)

**Rust/OCaml**: Use mutable arrays with O(1) element access and O(k) in-place reversal.

**Dark**: Uses immutable SkewList-based lists requiring:
- `getAt(list, i)`: O(log n) lookup through SkewList
- `take(list, k)`: O(k) traversal and reconstruction
- `drop(list, k)`: O(k) traversal
- `reverse(list)`: O(n) head/tail traversal and prepend reconstruction
- `append(a, b)`: O(length(a)) reconstruction

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
  ; Call getAt - O(log n) SkewList lookup
  v12764 <- Call(getAt, [Reg v12763, Imm 0])

countFlips_L1:  ; When first != 0
  ; Call takeHelper - O(k) traversal + allocation
  v12768 <- Call(Stdlib.List.__takeHelper_i64, [...])

  ; Call drop - O(k) traversal
  v12771 <- Call(Stdlib.List.drop_i64, [...])

  ; Call reverse - O(n) traversal + allocation
  v12772 <- Call(Stdlib.List.reverse_i64, [Reg v12768])

  ; Call append - O(n) reconstruction + allocation
  v12773 <- Call(Stdlib.List.append_i64, [...])

  ; Loop back
  Jump(Label "countFlips_body")
```

Current `-vvv --dump-anf --dump-mir --dump-lir` output confirms the same high-level shape after optimization: `reversePrefix` still takes, drops, reverses, and concatenates persistent lists, then `countFlips` tail-recurses on the reconstructed list. The skew implementation performs these traversals through head/tail and prepend rather than indexed FingerTree reconstruction.

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
  v12759 <- Call(Stdlib.Internal.SkewList.singleton_i64, ...) ; Wrap element
  v12761 <- Call(Stdlib.List.append_i64, [...])    ; First append
  TailCall(Stdlib.List.append_i64, [...])          ; Second append
```

Current LIR still keeps this as list reconstruction. `rotateLeft` contains `Stdlib.List.__takeHelper_i64` calls for the middle/rest slices and a final SkewList concatenation path instead of lowering to a bounded shift over contiguous storage.

### Hot Loop: `nextPerm` Count Updates

The existing investigation focused on prefix reversal and rotation. Current IR shows another per-permutation cost: updating the small `count` list is also a SkewList rewrite.

```
nextPerm:
  ci <- getAt(count, i)
  if ci > 1 then
    setAt(count, i, ci - 1)
  else
    setAt(count, i, i)
    nextPerm(newPerm, newCount, i + 1, n)
```

In LIR, both `setAt` paths rebuild the affected digit spine and tree path through `Stdlib.Internal.SkewList.__digitsSetAt_i64` and `Stdlib.Internal.SkewList.__treeSetAt_i64`, with reference-count cleanup along the path. Rust and OCaml update `count[i]` in place inside the permutation-generation loop, so this cost is unique to the current Dark representation.

### Memory Allocation Analysis

Each flip in Dark allocates:
1. SkewList nodes for `take` result
2. SkewList nodes for `drop` result
3. SkewList nodes for `reverse` result
4. SkewList nodes for `append` result

For n=9 (standard benchmark), this means **many list allocations per flip and rotation**. Rust is not allocation-free for every permutation in this benchmark because it clones the working `Vec` before counting flips for a non-zero first element; current optimized assembly shows a 72-byte `__rust_alloc` in that path. The important distinction is still that Rust pays for one contiguous copy before the flip-counting loop, then performs each prefix reversal in place, while Dark reconstructs multiple SkewList lists inside the loop.

## Identified Optimization Opportunities

### 1. Add Mutable Array Type with In-Place Operations

**Impact: 10-50x performance improvement (estimated)**

**Root Cause:**
Dark lacks a mutable array type. The immutable SkewList-based List requires O(log n) operations and allocations for operations that should be O(1) with arrays. This applies to both `perm` and the less-obvious `count` state list.

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
- `src/DarkCompiler/AST.fs` - Add Array type
- `src/DarkCompiler/stdlib/Array.dark` and `src/DarkCompiler/Stdlib.fs` - Add Array module with operations and primitive registrations
- `src/DarkCompiler/passes/arm64/6_CodeGen.fs` and `src/DarkCompiler/passes/x64/6_CodeGen.fs` - Lower Array ops to memory instructions

---

### 2. Specialize SkewList for Small Lists

**Impact: 2-5x performance improvement for fannkuch**

**Root Cause:**
Fannkuch typically operates on lists of 6-9 elements. SkewList has significant overhead for small lists due to tag checking, indirection, and node traversal.

**Evidence from Dark IR (tag checking overhead):**
```
Stdlib.Internal.SkewList.getAt_i64:
  v11890 <- Call(Stdlib.Internal.SkewList.__getTag_i64, [...])
  v11891 <- Call(Stdlib.Internal.SkewList.__TAG_LEAF, [...])
  Cmp(v11890, Reg v11891)
  Branch(...)  ; Branch to different cases
```

For a 6-element list, `getAt(0)` requires:
1. Check if tree is Empty/Single/Deep
2. Navigate through SkewList structure
3. Check node type (Leaf/Node2/Node3)
4. Extract value

**Implementation Approach:**
1. Add "small list" representation (inline array up to 8-16 elements)
2. Use tag bits to distinguish small lists from SkewLists
3. Implement fast paths for small list operations
4. Automatic promotion to SkewList when list grows

**Files to Modify:**
- `src/DarkCompiler/stdlib/__SkewList.dark` - Add small list representation
- `src/DarkCompiler/passes/arm64/6_CodeGen.fs` and `src/DarkCompiler/passes/x64/6_CodeGen.fs` - Add fast paths for small lists when they become compiler-recognized runtime shapes

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
- `src/DarkCompiler/passes/2.4_ANF_Inlining.fs` - Add Stdlib function inlining policy
- `src/DarkCompiler/Stdlib.fs` - Mark functions with `[<Inline>]` attribute

---

### 4. Replace `getAt(list, 0)` With `head`

**Impact: 10-20% performance improvement**

**Root Cause:**
`getAt(list, 0)` is already O(1) on the skew representation, but fannkuch still pays for bounds checks and option construction. Direct `head` use can remove that wrapper work in a hot loop.

**Evidence from Dark source:**
```dark
let countFlips(perm: List<Int64>, flips: Int64) : Int64 =
    let first = getAt(perm, 0) in  // Called every iteration
    ...

let rotateLeft(list: List<Int64>, i: Int64) : List<Int64> =
    let first = getAt(list, 0) in  // Called every rotation
    ...
```

**Implementation Approach:**
1. Use the existing O(1) `List.head` operation in source where possible
2. Pattern-match on `getAt(list, 0)` and replace it with `head(list)` when safe
3. Inline the direct prefix access for hot monomorphic call sites

**Files to Modify:**
- `src/DarkCompiler/stdlib/List.dark` - Add `head` function
- `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` - Pattern match `getAt(_, 0)` if this is kept as an ANF rewrite

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
3. **Medium Priority**: Optimize getAt(0) and `setAt` on known-small lists (#4 plus the `count` update evidence) - Quick win for the current source shape
4. **Medium Priority**: Inline hot Stdlib functions (#3) - General improvement
5. **Lower Priority**: Reference counting optimization (#5) - More complex

## Appendix: Data Structure Comparison

| Operation | Dark (SkewList) | Rust (Vec) | OCaml (Array) |
|-----------|-------------------|------------|---------------|
| get(i) | O(log n) + alloc check | O(1) | O(1) |
| reverse prefix k | O(n) + O(k) allocs | O(k) in-place | O(k) in-place |
| rotate left by 1 | O(log n) + allocs | O(i) in-place | O(i) in-place |
| set(i, v) | O(log n) + allocs | O(1) | O(1) |
