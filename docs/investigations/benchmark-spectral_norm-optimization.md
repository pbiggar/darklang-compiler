# Spectral Norm Benchmark Optimization Investigation

## Summary

The spectral_norm benchmark computes the spectral norm of an infinite matrix A using the power iteration method. The Dark implementation is severely simplified and incomplete - it only computes a single matrix element rather than the full spectral norm algorithm with matrix-vector products.

**Performance Results (instruction counts):**
- Rust: 5,093,977 (n=100, 10 iterations)
- OCaml: 22,589,955 (n=100, 10 iterations) - 4.43x slower than Rust
- **Dark: Not benchmarkable (incomplete implementation)**

**Key Issues:**
1. **Incomplete implementation**: Dark only computes `matA(0,0) = 1.0` instead of the full algorithm
2. **Missing mutable arrays**: Dark lacks array primitives needed for efficient vector operations
3. **Missing loops/iteration**: Dark lacks `for`/`while` loops, requiring recursive implementations

## Benchmark Source Code

### Dark (`benchmarks/problems/spectral_norm/dark/main.dark`)
```dark
// Spectral Norm Benchmark - Dark implementation
// Simplified: computes single matrix-vector product

// Matrix element A(i,j) = 1 / ((i+j)*(i+j+1)/2 + i + 1)
def matA(i: Int64, j: Int64) : Float =
    let ij = i + j in
    let div = ij * (ij + 1) / 2 + i + 1 in
    1.0 / Stdlib.Int64.toFloat(div)

// Compute just the first row sum to verify algorithm works
let row0 = matA(0, 0) in
let scaled = Stdlib.Float.toInt(row0 * 1000000000.0) in
scaled
```

**Output:** `1000000000` (incorrect - should be `1274219991`)

### Rust (`benchmarks/problems/spectral_norm/rust/main.rs`)
```rust
fn a(i: usize, j: usize) -> f64 {
    1.0 / ((i + j) * (i + j + 1) / 2 + i + 1) as f64
}

fn av(n: usize, v: &[f64], out: &mut [f64]) {
    for i in 0..n {
        let mut s = 0.0;
        for j in 0..n {
            s += a(i, j) * v[j];
        }
        out[i] = s;
    }
}

fn atv(n: usize, v: &[f64], out: &mut [f64]) {
    for i in 0..n {
        let mut s = 0.0;
        for j in 0..n {
            s += a(j, i) * v[j];
        }
        out[i] = s;
    }
}

fn atav(n: usize, v: &[f64], out: &mut [f64], tmp: &mut [f64]) {
    av(n, v, tmp);
    atv(n, tmp, out);
}

fn spectral_norm(n: usize) -> f64 {
    let mut u = vec![1.0; n];
    let mut v = vec![0.0; n];
    let mut tmp = vec![0.0; n];

    for _ in 0..10 {
        atav(n, &u, &mut v, &mut tmp);
        atav(n, &v, &mut u, &mut tmp);
    }

    let mut vbv = 0.0;
    let mut vv = 0.0;
    for i in 0..n {
        vbv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    (vbv / vv).sqrt()
}
```

### OCaml (`benchmarks/problems/spectral_norm/ocaml/main.ml`)
```ocaml
let a i j =
  1.0 /. float_of_int ((i + j) * (i + j + 1) / 2 + i + 1)

let av n v out =
  for i = 0 to n - 1 do
    let s = ref 0.0 in
    for j = 0 to n - 1 do
      s := !s +. a i j *. v.(j)
    done;
    out.(i) <- !s
  done

let atv n v out =
  for i = 0 to n - 1 do
    let s = ref 0.0 in
    for j = 0 to n - 1 do
      s := !s +. a j i *. v.(j)
    done;
    out.(i) <- !s
  done
```

## Analysis

### Problem 1: Missing Mutable Arrays

The spectral norm algorithm fundamentally requires:
1. **O(1) random access to arrays** for vector operations
2. **In-place mutation** of vectors (u, v, tmp)
3. **Dense storage** of n floating-point values

**Rust/OCaml approach:**
- Allocate contiguous arrays: `vec![0.0; n]` or `Array.make n 0.0`
- Direct indexed access: `v[j]` or `v.(j)` - single load instruction
- In-place updates: `out[i] = s` or `out.(i) <- s` - single store instruction

**Dark's limitation:**
- Only has immutable `List<T>` (linked list) - O(n) access
- Has `Dict<K,V>` (HAMT) - O(log n) access with allocation overhead
- No mutable array primitive for O(1) random access

**Evidence from Rust inner loop (highly optimized, 8 instructions):**
```asm
9440: 8b0b012c    add x12, x9, x11        ; i + j
9444: fc6b7a63    ldr d3, [x19, x11, lsl #3]  ; v[j] - SINGLE LOAD
9448: 9100056b    add x11, x11, #0x1      ; j++
944c: 9b0c318c    madd x12, x12, x12, x12  ; (i+j)*(i+j+1)
9454: 8b4c054c    add x12, x10, x12, lsr #1  ; /2 + i + 1
9458: 9e630182    ucvtf d2, x12           ; int to float
945c: 1e621802    fdiv d2, d0, d2         ; 1.0 / div
9460: 1e620862    fmul d2, d3, d2         ; * v[j]
9464: 1e622821    fadd d1, d1, d2         ; s += ...
9468: 54fffec1    b.ne 9440               ; loop back
```

**Evidence from OCaml inner loop (bounds-checked but still efficient, ~20 instructions):**
```asm
547f8: f9400ff8    ldr x24, [sp, #24]      ; load v base address
547fc: f85f8309    ldur x9, [x24, #-8]     ; load array length (for bounds check)
54800: eb49243f    cmp x1, x9, lsr #9      ; bounds check
54804: 54000602    b.cs 548c4              ; branch to error if out of bounds
54808: 8b010b0a    add x10, x24, x1, lsl #2  ; compute offset
5480c: fc5fc142    ldur d2, [x10, #-4]     ; v[j] - LOAD
54818: 97ffffc4    bl 54728                ; call a(i,j) function
5481c: fd400003    ldr d3, [x0]            ; get float result
54824: 1e660864    fmul d4, d3, d6         ; a(i,j) * v[j]
5482c: 1e642800    fadd d0, d0, d4         ; s += ...
```

### Problem 2: Missing Loop Constructs

Dark lacks `for` and `while` loops, requiring all iteration to be expressed as recursion.

**What would be needed for spectral_norm:**
```dark
// Hypothetical Dark with loops
def av(n: Int64, v: Array<Float>, out: Array<Float>) : Unit =
    for i in 0 to n - 1 do
        let s = Stdlib.Ref.make(0.0) in
        for j in 0 to n - 1 do
            Stdlib.Ref.set(s, Stdlib.Ref.get(s) + matA(i, j) * Stdlib.Array.get(v, j))
        done;
        Stdlib.Array.set(out, i, Stdlib.Ref.get(s))
    done
```

**Current recursive alternative would require:**
```dark
// This would work but is complex and less efficient
def avInner(i: Int64, j: Int64, n: Int64, v: ???, s: Float) : Float =
    if j >= n then s
    else avInner(i, j + 1, n, v, s + matA(i, j) * getV(v, j))

def avOuter(i: Int64, n: Int64, v: ???, out: ???) : ??? =
    if i >= n then out
    else
        let s = avInner(i, 0, n, v, 0.0) in
        avOuter(i + 1, n, v, setOut(out, i, s))
```

### Problem 3: Function Call Overhead in Hot Loop

Even with arrays, calling `matA(i, j)` as a separate function in the inner loop adds overhead:

**OCaml calls `a(i,j)` as a function in inner loop:**
```asm
54818: 97ffffc4    bl 54728 <camlMain__a_267>  ; function call
```

**Rust inlines the entire computation:**
```asm
; No function call - all computation inline
944c: 9b0c318c    madd x12, x12, x12, x12  ; (i+j)*(i+j+1)
9454: 8b4c054c    add x12, x10, x12, lsr #1  ; /2 + i + 1
9458: 9e630182    ucvtf d2, x12           ; convert to float
945c: 1e621802    fdiv d2, d0, d2         ; 1.0 / div
```

**Current Dark status: the trivial `matA(0, 0)` call is now inlined before MIR.**

The current simplified benchmark still does not exercise a hot loop, but `./dark --dump-anf
benchmarks/problems/spectral_norm/dark/main.dark` shows that the only call site is expanded
during reference-count insertion:

```
=== ANF (after RC insertion) ===
Function _start:
let TempId 16 = 0
let TempId 17 = 0
let TempId 18 = t16 + t17
let TempId 19 = t18 + 1
let TempId 20 = t18 * t19
let TempId 21 = t20 >> 1
let TempId 22 = t21 + t16
let TempId 23 = t22 + 1
let TempId 24 = Int64ToFloat(t23)
let TempId 25 = 1 / t24
...
```

The MIR and LIR for `_start` likewise contain no `Call(matA, ...)`; LIR has already reduced the
constant input to the equivalent `1.0 / 1.0` path:

```
_start:
  Label "_start_body":
    X1 <- Mov(Imm 1)
    D0 <- Int64ToFloat(X1)
    D1 <- FLoad(float[1])
    D0 <- FDiv(D1, D0)
    D1 <- FLoad(float[1000000000])
    D0 <- FMul(D0, D1)
    X1 <- FloatToInt64(D0)
```

The standalone `matA` function is still emitted:
```
matA:
  Label "matA_body":
    v10011 <- Add(v0, Reg v1)           ; i + j
    v10012 <- Add(v10011, Imm 1)        ; ij + 1
    v10013 <- Mul(v10011, Reg v10012)   ; ij * (ij + 1)
    v10014 <- Lsr_imm(v10013, #1)       ; / 2
    v10015 <- Add(v10014, Reg v0)       ; + i
    v10016 <- Add(v10015, Imm 1)        ; + 1
    fv10017 <- IntToFloat(v10016)       ; convert to float
    fv1000 <- FLoad(float[1])           ; load 1.0
    fv10018 <- FDiv(fv1000, fv10017)    ; 1.0 / div
```

So the old note that the current benchmark does not inline `matA` is stale for this constant call
site. The remaining open question is whether the inliner keeps doing this for non-constant
`matA(i, j)` calls inside a recursive or eventual array-backed inner loop; the current benchmark
cannot prove that because it only computes `matA(0, 0)`.

## Identified Optimization Opportunities

### 1. Add Mutable Array Primitive

**Impact: Required for correct implementation; 10-100x performance vs Dict-based workaround**

**Root Cause:**
Dark lacks mutable array primitives. The spectral norm algorithm requires O(1) random access to vectors containing n floating-point values.

**Proposed Solution:**
Add `Stdlib.Array` module with:
```dark
module Stdlib.Array
  def make<T>(size: Int64, default: T) : Array<T>
  def get<T>(arr: Array<T>, index: Int64) : T
  def set<T>(arr: Array<T>, index: Int64, value: T) : Unit
  def length<T>(arr: Array<T>) : Int64
```

**Implementation Approach:**
1. Add `TArray of Type` to AST type representation
2. Implement array allocation using `mmap` or `brk` syscalls
3. Implement `get` as bounds check + single load instruction
4. Implement `set` as bounds check + single store instruction
5. Add array memory management (reference counting or explicit free)

**Evidence - Rust's array access compiles to single instruction:**
```asm
fc6b7a63    ldr d3, [x19, x11, lsl #3]  ; v[j] = base + j * 8
```

**Files to Modify:**
- `src/DarkCompiler/AST.fs` - Add TArray type
- `src/DarkCompiler/Stdlib.fs` - Add Array module definition
- `src/DarkCompiler/passes/6_CodeGen.fs` - Implement array intrinsics
- `src/DarkCompiler/passes/2.5_RefCountInsertion.fs` - Handle array lifetime

---

### 2. Add Loop Constructs (for/while)

**Impact: Required for natural expression of nested iteration; improves code clarity**

**Root Cause:**
The spectral norm algorithm has nested loops (O(n²) operations). Expressing this as recursion is verbose and may not optimize as well.

**Proposed Solution:**
Add `for` and `while` expressions:
```dark
for i in 0 to n - 1 do
    // body that can use i
done

while condition do
    // body
done
```

**Implementation Approach:**
1. Add `EFor` and `EWhile` to AST expressions
2. Desugar to tail-recursive functions in ANF transformation (or directly to MIR loops)
3. Ensure tail call optimization converts these to efficient loops

**Alternative: Range-based fold**
```dark
Stdlib.Int64.range(0, n - 1) |> Stdlib.List.fold(init, fn(acc, i) -> ...)
```

This already works but creates intermediate list allocations.

**Files to Modify:**
- `src/DarkCompiler/AST.fs` - Add EFor, EWhile expressions
- `src/DarkCompiler/Parser.fs` - Parse loop syntax
- `src/DarkCompiler/passes/2_AST_to_ANF.fs` - Transform loops to ANF

---

### 3. Aggressive Function Inlining in Hot Loops

**Impact: ~2-4x performance improvement for numeric code**

**Root Cause:**
Small numeric functions like `matA(i, j)` should be inlined into their call sites to avoid function call overhead. In the inner loop of spectral_norm, `matA` would be called n² times.

**Evidence - Current Dark does inline the simplified constant call:**
```
Function _start:
let TempId 18 = t16 + t17
let TempId 20 = t18 * t19
let TempId 21 = t20 >> 1
let TempId 25 = 1 / t24
```

**Evidence - Rust fully inlines the computation:**
The entire `a(i,j)` computation is inlined into the loop body, eliminating call overhead.

**Proposed Solution:**
Keep this as a conditional opportunity for the full implementation:
1. Confirm that `matA(i, j)` is still inlined when `i` and `j` are dynamic loop variables.
2. If it is not, enhance the inlining pass for small pure numeric functions in loops.
3. If it is, remove this as a separate spectral_norm blocker and track only the array/full-implementation work.

**Files to Modify:**
- `src/DarkCompiler/passes/2.4_Inlining.fs` - Enhance inlining heuristics

---

### 4. Complete the Dark Implementation

**Impact: Required to run the benchmark at all**

**Root Cause:**
The current Dark implementation only computes `matA(0,0)` and returns a constant value. It needs the full algorithm.

**Once arrays and loops are available, implementation would be:**
```dark
def matA(i: Int64, j: Int64) : Float =
    let ij = i + j in
    let div = ij * (ij + 1) / 2 + i + 1 in
    1.0 / Stdlib.Int64.toFloat(div)

def av(n: Int64, v: Array<Float>, out: Array<Float>) : Unit =
    def inner(i: Int64) : Unit =
        if i >= n then ()
        else
            def sumLoop(j: Int64, s: Float) : Float =
                if j >= n then s
                else sumLoop(j + 1, s + matA(i, j) * Stdlib.Array.get(v, j))
            in
            let _ = Stdlib.Array.set(out, i, sumLoop(0, 0.0)) in
            inner(i + 1)
    in
    inner(0)

def atv(n: Int64, v: Array<Float>, out: Array<Float>) : Unit =
    // Similar but with matA(j, i) instead of matA(i, j)
    ...

def spectralNorm(n: Int64) : Float =
    let u = Stdlib.Array.make(n, 1.0) in
    let v = Stdlib.Array.make(n, 0.0) in
    let tmp = Stdlib.Array.make(n, 0.0) in

    def iterate(count: Int64) : Unit =
        if count >= 10 then ()
        else
            let _ = av(n, u, tmp) in
            let _ = atv(n, tmp, v) in
            let _ = av(n, v, tmp) in
            let _ = atv(n, tmp, u) in
            iterate(count + 1)
    in
    let _ = iterate(0) in

    // Compute final norm
    def dotProduct(i: Int64, vbv: Float, vv: Float) : (Float, Float) =
        if i >= n then (vbv, vv)
        else
            let ui = Stdlib.Array.get(u, i) in
            let vi = Stdlib.Array.get(v, i) in
            dotProduct(i + 1, vbv + ui * vi, vv + vi * vi)
    in
    let (vbv, vv) = dotProduct(0, 0.0, 0.0) in
    Stdlib.Float.sqrt(vbv / vv)
```

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity | Blocks Benchmark |
|--------------|-----------------|------------|------------------|
| Mutable Arrays | 10-100x vs Dict | High | Yes |
| Loop Constructs | Code clarity + 10-20% | Medium | No (can use recursion) |
| Aggressive Inlining | 2-4x for numeric code | Medium | No |
| Complete Implementation | Required | Medium | Yes |

**With arrays, Dark spectral_norm could achieve:**
- ~2-4x of Rust performance (typical for a non-SIMD implementation)
- Comparable to OCaml's ~4.4x overhead

## Recommended Implementation Order

1. **Mutable Arrays** - Essential prerequisite for the algorithm
2. **Complete the Implementation** - Make it produce correct output
3. **Aggressive Inlining** - Optimize the hot numeric loops
4. **Loop Constructs** - Improve ergonomics (optional)

## Appendix: Full IR Dumps

### Dark ANF (before optimization)
```
Function matA:
let TempId 2 = t0 + t1
let TempId 3 = t2
let TempId 4 = t3 + 1
let TempId 5 = t3 * t4
let TempId 6 = t5 / 2
let TempId 7 = t6 + t0
let TempId 8 = t7 + 1
let TempId 9 = t8
let TempId 10 = IntToFloat(t9)
let TempId 11 = 1 / t10
return t11

Main:
let TempId 12 = matA(0, 0)
let TempId 13 = t12
let TempId 14 = t13 * 1000000000
let TempId 15 = FloatToInt(t14)
let TempId 16 = t15
return t16
```

### Dark ANF (after optimization)
```
Function matA:
let TempId 2 = t0 + t1
let TempId 4 = t2 + 1
let TempId 5 = t2 * t4
let TempId 6 = t5 >> 1          ; Division by 2 -> shift right
let TempId 7 = t6 + t0
let TempId 8 = t7 + 1
let TempId 10 = IntToFloat(t8)
let TempId 11 = 1 / t10
return t11

Main:
let TempId 12 = matA(0, 0)
let TempId 14 = t12 * 1000000000
let TempId 15 = FloatToInt(t14)
return t15
```

### Dark MIR (Control Flow Graph)
```
Function matA:
  matA_entry:
    jump matA_body
  matA_body:
    v2 <- v0 + v1 : TInt64
    v4 <- v2 + 1 : TInt64
    v5 <- v2 * v4 : TInt64
    v6 <- v5 >> 1 : TInt64
    v7 <- v6 + v0 : TInt64
    v8 <- v7 + 1 : TInt64
    v10 <- IntToFloat(v8)
    v11 <- float[1] / v10 : TFloat64
    ret v11

Function _start:
  _start_body:
    v12 <- Call(matA, [0, 0])
    v14 <- v12 * float[1000000000] : TFloat64
    v15 <- FloatToInt(v14)
    Print(v15, type=TInt64)
    ret v15
```

### Dark LIR (After Register Allocation)
```
matA:
  Label "matA_body":
    X2 <- Add(X3, Reg X1)
    X1 <- Add(X2, Imm 1)
    X1 <- Mul(X2, Reg X1)
    X1 <- Lsr_imm(X1, #1)
    X1 <- Add(X1, Reg X3)
    X1 <- Add(X1, Imm 1)
    D0 <- IntToFloat(X1)
    fv1000 <- FLoad(float[1])
    D0 <- FDiv(fv1000, D0)
    D0 <- FMov(D0)
    Ret
  Label "matA_entry":
    X3 <- Mov(Reg X0)
    Jump(Label "matA_body")

_start:
  Label "_start_body":
    SaveRegs([], [])
    ArgMoves(X0 <- Imm 0, X1 <- Imm 0)
    X19 <- Call(matA, [Imm 0, Imm 0])
    D8 <- FMov(D0)
    RestoreRegs([], [])
    D0 <- FMov(D8)
    fv1001 <- FLoad(float[1000000000])
    D0 <- FMul(D0, fv1001)
    X19 <- FloatToInt(D0)
    X0 <- Mov(Reg X19)
    PrintInt(X0)
    X0 <- Mov(Reg X19)
    Ret
```

### Rust Inner Loop (Assembly) - Highly Optimized
```asm
; av() inner loop - computes s += a(i,j) * v[j]
; Only 11 instructions including branch!
9440: 8b0b012c    add x12, x9, x11        ; i + j
9444: fc6b7a63    ldr d3, [x19, x11, lsl #3]  ; v[j]
9448: 9100056b    add x11, x11, #0x1      ; j++
944c: 9b0c318c    madd x12, x12, x12, x12  ; (i+j)*(i+j+1)
9450: f101917f    cmp x11, #0x64          ; j < 100
9454: 8b4c054c    add x12, x10, x12, lsr #1  ; /2 + i + 1
9458: 9e630182    ucvtf d2, x12           ; int to float
945c: 1e621802    fdiv d2, d0, d2         ; 1.0 / div
9460: 1e620862    fmul d2, d3, d2         ; * v[j]
9464: 1e622821    fadd d1, d1, d2         ; s += ...
9468: 54fffec1    b.ne 9440               ; loop back
```

### OCaml Inner Loop (Assembly)
```asm
; av() inner loop - with function call to a(i,j)
547f8: f9400ff8    ldr x24, [sp, #24]      ; load v base
547fc: f85f8309    ldur x9, [x24, #-8]     ; array bounds
54800: eb49243f    cmp x1, x9, lsr #9      ; bounds check
54804: 54000602    b.cs 548c4              ; error handler
54808: 8b010b0a    add x10, x24, x1, lsl #2
5480c: fc5fc142    ldur d2, [x10, #-4]     ; v[j]
54810: fd001fe2    str d2, [sp, #56]       ; spill to stack
54814: f94013e0    ldr x0, [sp, #32]       ; load i
54818: 97ffffc4    bl 54728                ; call a(i,j)
5481c: fd400003    ldr d3, [x0]            ; get result
54820: fd401fe6    ldr d6, [sp, #56]       ; reload v[j]
54824: 1e660864    fmul d4, d3, d6         ; a(i,j) * v[j]
54828: fd4023e0    ldr d0, [sp, #64]       ; load s
5482c: 1e642800    fadd d0, d0, d4         ; s += ...
54830: fd0023e0    str d0, [sp, #64]       ; store s
; ... loop bookkeeping ...
```
