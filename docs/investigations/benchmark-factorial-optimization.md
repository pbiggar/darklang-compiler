# Factorial Benchmark Optimization Investigation

## Summary

The factorial benchmark computes `factorial(20)` 10,000 times using a `repeat` helper function. The factorial function is a simple recursive multiplication.

**Performance Results (instruction counts):**
- Rust: 256,121 instructions (baseline)
- Dark: 4,420,203 instructions (17.3x slower)
- OCaml: 7,937,785 instructions (31.0x slower)

**Key insight**: Rust achieves its extreme performance through **complete compile-time evaluation**. LLVM constant-folds `factorial(20)` and `repeat(10000, 0)` at compile time, resulting in a program that simply prints a pre-computed constant. Dark is actually faster than OCaml on this benchmark.

Current Dark evidence still shows one runtime `factorial(20)` call on every
tail-recursive `repeat` iteration. Register allocation now removes the older
redundant self-moves in the `repeat` loop, so the remaining benchmark gap is
not caused by those moves.

Current focused Cachegrind evidence confirms the gap is stable: Dark still
runs at 4,420,203 instructions, or 17.3x the cached Rust baseline. The same
run reports 1,600,034 data references and 210,025 branches for Dark, consistent
with repeated unboxed recursive integer work rather than heap allocation or
cache behavior. The Rust, OCaml, F#, Python, and Node rows in that focused run
used cached baselines from `benchmarks/BASELINES.md`.

## Benchmark Source Code

### Dark (`benchmarks/problems/factorial/dark/main.dark`)
```dark
def factorial(n: Int64) : Int64 =
    if n <= 1 then 1
    else n * factorial(n - 1)

def repeat(n: Int64, acc: Int64) : Int64 =
    if n <= 0 then acc
    else repeat(n - 1, factorial(20))

repeat(10000, 0)
```

### Rust (`benchmarks/problems/factorial/rust/main.rs`)
```rust
fn factorial(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn repeat(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        repeat(n - 1, factorial(20))
    }
}

fn main() {
    println!("{}", repeat(10000, 0));
}
```

### OCaml (`benchmarks/problems/factorial/ocaml/main.ml`)
```ocaml
let rec factorial n =
  if n <= 1L then 1L
  else Int64.mul n (factorial (Int64.sub n 1L))

let rec repeat n acc =
  if n <= 0 then acc
  else repeat (n - 1) (factorial 20L)

let () = Printf.printf "%Ld\n" (repeat 10000 0L)
```

## Analysis

### Rust: Complete Compile-Time Evaluation

Rust/LLVM performs aggressive constant propagation. Since `factorial(20)` is called with a compile-time constant, and the function is pure (no side effects), LLVM evaluates it at compile time:

```asm
; Rust main function - just loads a constant!
_ZN4main4main17h02cbda4dfa830c12E:
    mov     x8, #2192834560           ; Load low bits of factorial(20)
    movk    x8, #26492, lsl #32       ; Load more bits
    movk    x8, #8643, lsl #48        ; Load high bits = 2432902008176640000
    ; ... print the constant
```

The entire `repeat(10000, 0)` loop is eliminated because:
1. `factorial(20)` is constant = 2432902008176640000
2. `repeat` always returns `factorial(20)` regardless of iterations
3. Therefore `repeat(10000, 0)` = 2432902008176640000

### OCaml: Boxed Int64 Overhead

OCaml uses boxed Int64 values, requiring heap allocation for each intermediate result:

```asm
camlMain__factorial_267:
    ; Function prologue
    sub     sp, sp, #16
    str     x30, [sp, #8]
.L101:
    ldr     x1, [x0, #8]              ; Load boxed int64 value
    cmp     x1, #1
    b.gt    .L100
    ; Base case: return boxed 1
    adrp    x0, :got:camlMain__1
    ldr     x0, [x0, #:got_lo12:camlMain__1]
    ret
.L100:
    str     x0, [sp, #0]              ; Save n on stack
    ; Allocate boxed int64 for n-1
    sub     x27, x27, #24             ; Bump allocator
    cmp     x27, x16                  ; GC check
    b.lo    .L104
    ; ... setup boxed value ...
    sub     x6, x1, #1                ; n - 1
    str     x6, [x0, #8]              ; Store in box
    bl      camlMain__factorial_267   ; Recursive call
    ; ... multiply and box result ...
    mul     x11, x10, x9              ; Actual multiplication
    ; Allocate another boxed int64 for result
    sub     x27, x27, #24             ; More allocation!
```

**OCaml overhead per factorial call:**
- 2 heap allocations (24 bytes each) for boxed Int64
- GC checks on each allocation
- Indirect loads through boxed values
- No tail call optimization (result must be multiplied)

### Dark: Efficient Unboxed Int64 with Tail Call Optimization

Dark uses unboxed Int64 and recognizes the tail call in `repeat`:

**Dark ANF (after Tail Call Detection):**
```
Function factorial:
let TempId 1 = t0 <= 1
if t1 then
return 1
else
let TempId 2 = t0 - 1
let TempId 3 = factorial(t2)
let TempId 4 = t0 * t3
return t4

Function repeat:
let TempId 7 = t5 <= 0
if t7 then
return t6
else
let TempId 8 = t5 - 1
let TempId 9 = factorial(20)
let TempId 10 = TailCall(repeat, [t8, t9])  ; <-- Tail call detected!
return t10
```

**Dark LIR (After Register Allocation):**
```
factorial:
  Label "factorial_L0":             ; Base case: n <= 1
    X0 <- Mov(Imm 1)                ; return 1
    Ret
  Label "factorial_L1":             ; Recursive case
    X19 <- Sub(X20, Imm 1)          ; n - 1
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19)
    X19 <- Call(factorial, [Reg X19])
    RestoreRegs([], [])
    X19 <- Mov(Reg X0)
    X19 <- Mul(X20, Reg X19)        ; n * factorial(n-1)
    X0 <- Mov(Reg X19)
    Ret

repeat:
  Label "repeat_L1":                ; Tail recursive case
    X19 <- Sub(X19, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Imm 20)
    X20 <- Call(factorial, [Imm 20])
    RestoreRegs([], [])
    X20 <- Mov(Reg X0)
    Jump(Label "repeat_body")       ; Loop back (tail call!)
```

**Dark advantages over OCaml:**
1. **Unboxed Int64**: No heap allocation for integers
2. **Tail call optimization**: `repeat` loops instead of recursing
3. **Direct register operations**: No boxing/unboxing

## Identified Optimization Opportunities

### 1. Hoist Loop-Invariant Pure Calls from Tail-Recursive Loops

**Impact: Large for this benchmark**

**Root Cause:**
`repeat` is compiled as a loop, but the pure call `factorial(20)` remains in
the loop body even though it does not depend on the loop-carried values. The
current LIR therefore executes the same factorial computation 10,000 times:

```
repeat:
  Label "repeat_L1":
    X19 <- Sub(X19, Imm 1)
    ArgMoves(X0 <- Imm 20)
    X20 <- Call(factorial, [Imm 20])
    X20 <- Mov(Reg X0)
    Jump(Label "repeat_body")
```

The generated ARM64 has the same shape: the repeat loop decrements `x19`,
loads `20` into `x0`, calls `factorial`, stores the result in `x20`, and jumps
back to the loop condition.

```asm
21c: sub x19, x19, #0x1
220: mov x0, #0x14
224: bl  0x190
228: mov x20, x0
22c: b   0x238
```

Because `factorial(20)` is loop-invariant, a purity-aware LICM pass for
tail-recursive loops could compute it once before entering `repeat` and carry
the value through the loop. That would not match Rust's full compile-time
result, but it would remove the dominant repeated computation without needing
full partial evaluation.

This remains the best factorial-specific runtime opportunity after the current
register allocator: the post-allocation LIR has no spill slots and no saved
registers around the `factorial(20)` call, so there is little local calling
sequence cleanup left in `repeat`.

**Implementation Approach:**
1. Reuse or introduce purity information for calls in ANF or MIR.
2. Detect values inside lowered tail-recursive loops that do not depend on
   loop-carried parameters.
3. Hoist safe pure calls before the loop entry while preserving call ordering
   for any effectful operation.

**Files to Modify:**
- `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` or MIR lowering, depending on
  where tail-recursive loops are easiest to analyze.
- Add purity metadata only if existing call classification is insufficient.

---

### 2. Compile-Time Constant Folding for Pure Functions

**Impact: Could match Rust performance (~17x improvement)**

**Root Cause:**
Dark does not evaluate constant expressions at compile time. When `factorial(20)` is called, it's always computed at runtime even though:
1. The argument `20` is a compile-time constant
2. The function `factorial` is pure (no side effects)
3. The result is always the same

**Evidence from Dark IR:**
```
Function repeat:
  ...
  v9 <- Call(factorial, [20])    ; Runtime call with constant argument!
```

**Evidence from Rust:**
```asm
mov     x8, #2192834560          ; Compile-time computed constant
movk    x8, #26492, lsl #32
movk    x8, #8643, lsl #48       ; = 2432902008176640000
```

**Implementation Approach:**
1. **Purity analysis**: Track which functions have no side effects (no I/O, no mutable state)
2. **Constant propagation**: When a pure function is called with all constant arguments, evaluate at compile time
3. **Partial evaluation**: For loops/recursion with constant bounds, unroll or fully evaluate

**Files to Modify:**
- `src/DarkCompiler/passes/2.3_ANF_Optimize.fs` - Add constant folding for function calls
- Add new file: `src/DarkCompiler/passes/2.2_PurityAnalysis.fs` - Track pure functions
- `src/DarkCompiler/ANF.fs` - Add purity annotations

---

### 3. Inline Small Pure Functions

**Impact: ~10-15% performance improvement**

**Root Cause:**
The `factorial` function is called 10,000 * 20 = 200,000 times. Each call has overhead:
- Function prologue/epilogue
- Argument passing
- Return value handling

**Evidence:**
```
repeat_L1:
    X20 <- Sub(X20, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Imm 20)
    X19 <- Call(factorial, [Imm 20])   ; Full call overhead
    RestoreRegs([], [])
```

**Implementation Approach:**
1. Detect small pure functions (< threshold instructions)
2. Inline at call sites with constant arguments
3. After inlining, loops may become eligible for further optimization

**Files to Modify:**
- `src/DarkCompiler/passes/2.4_ANF_Inlining.fs` - Enhance inlining heuristics

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity |
|--------------|-----------------|------------|
| Hoist Loop-Invariant Pure Calls | Large for this benchmark | Medium |
| Compile-Time Constant Folding | 90%+ (match Rust) | High |
| Inline Small Pure Functions | 10-15% | Medium |

**Note:** Compile-time constant folding would eliminate the entire benchmark computation, matching Rust. Without that optimization, loop-invariant pure-call hoisting is the main remaining factorial-specific runtime opportunity documented here.

## Recommended Implementation Order

1. **Hoist Loop-Invariant Pure Calls** - Medium complexity, removes the dominant repeated `factorial(20)` work
2. **Inline Small Pure Functions** - Medium complexity, smaller improvement
3. **Compile-Time Constant Folding** - Major impact, high complexity

## Why Dark Beats OCaml

Despite being slower than Rust, Dark outperforms OCaml on this benchmark because:

1. **Unboxed integers**: Dark uses native Int64 in registers; OCaml boxes Int64 on heap
2. **No GC pressure**: Dark has zero heap allocations for this benchmark
3. **Tail call optimization**: Dark's `repeat` function loops; both handle `factorial` similarly
4. **No boxing overhead**: Each OCaml multiplication requires unboxing, computing, and re-boxing

## Appendix: Full IR Dumps

### Dark ANF (after Tail Call Detection)
```
Function factorial:
let TempId 1 = t0 <= 1
if t1 then
return 1
else
let TempId 2 = t0 - 1
let TempId 3 = factorial(t2)
let TempId 4 = t0 * t3
return t4

Function repeat:
let TempId 7 = t5 <= 0
if t7 then
return t6
else
let TempId 8 = t5 - 1
let TempId 9 = factorial(20)
let TempId 10 = TailCall(repeat, [t8, t9])
return t10

Main:
let TempId 11 = repeat(10000, 0)
let TempId 2000 = print(t11, type=TInt64)
return t11
```

### Dark MIR (Control Flow Graph)
```
Function factorial:
  factorial_entry:
    jump factorial_body
  factorial_L0:
    v5 <- 1 : TInt64
    jump factorial_L2
  factorial_L1:
    v2 <- v0 - 1 : TInt64
    v3 <- Call(factorial, [v2])
    v4 <- v0 * v3 : TInt64
    v5 <- v4 : TInt64
    jump factorial_L2
  factorial_L2:
    ret v5
  factorial_body:
    v1 <- v0 <= 1 : TInt64
    branch v1 ? factorial_L0 : factorial_L1

Function repeat:
  repeat_entry:
    jump repeat_body
  repeat_L0:
    v11 <- v6 : TInt64
    jump repeat_L2
  repeat_L1:
    v8 <- v5 - 1 : TInt64
    v9 <- Call(factorial, [20])
    v5 <- v8 : TInt64
    v6 <- v9 : TInt64
    jump repeat_body
  repeat_L2:
    ret v11
  repeat_body:
    v7 <- v5 <= 0 : TInt64
    branch v7 ? repeat_L0 : repeat_L1
```

### Dark LIR (After Register Allocation)
```
factorial:
  Label "factorial_L0":
    X0 <- Mov(Imm 1)
    Ret
  Label "factorial_L1":
    X19 <- Sub(X20, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19)
    X19 <- Call(factorial, [Reg X19])
    RestoreRegs([], [])
    X19 <- Mov(Reg X0)
    X19 <- Mul(X20, Reg X19)
    X0 <- Mov(Reg X19)
    Ret
  Label "factorial_body":
    Cmp(X20, Imm 1)
    CondBranch(LE, Label "factorial_L0", Label "factorial_L1")
  Label "factorial_entry":
    X20 <- Mov(Reg X0)
    Jump(Label "factorial_body")

repeat:
  Label "repeat_L1":
    X19 <- Sub(X19, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Imm 20)
    X20 <- Call(factorial, [Imm 20])
    RestoreRegs([], [])
    X20 <- Mov(Reg X0)
    Jump(Label "repeat_body")
  Label "repeat_L2":
    X0 <- Mov(Reg X20)
    Ret
  Label "repeat_body":
    Cmp(X19, Imm 0)
    CondBranch(LE, Label "repeat_L2", Label "repeat_L1")
  Label "repeat_entry":
    X19 <- Mov(Reg X0)
    X20 <- Mov(Reg X1)
    Jump(Label "repeat_body")
```

### OCaml Assembly (factorial function)
```asm
camlMain__factorial_267:
    sub     sp, sp, #16
    str     x30, [sp, #8]
.L101:
    ldr     x1, [x0, #8]          ; Unbox: load int64 value
    cmp     x1, #1
    b.gt    .L100
    ; Base case: return boxed 1
    adrp    x0, :got:camlMain__1
    ldr     x0, [x0, #:got_lo12:camlMain__1]
    ldr     x30, [sp, #8]
    add     sp, sp, #16
    ret
.L100:
    str     x0, [sp, #0]          ; Save boxed n
    ldr     x16, [x28, #0]
    sub     x27, x27, #24         ; Allocate 24 bytes
    cmp     x27, x16              ; GC check
    b.lo    .L104
.L103:
    add     x0, x27, #8           ; Setup boxed value
    movz    x3, #2303, lsl #0
    str     x3, [x0, #-8]         ; Header
    adrp    x4, :got:caml_int64_ops
    ldr     x4, [x4, #:got_lo12:caml_int64_ops]
    str     x4, [x0, #0]          ; Ops table
    sub     x6, x1, #1            ; n - 1
    str     x6, [x0, #8]          ; Store value
    bl      camlMain__factorial_267
.L105:
    add     x8, x0, #8
    ldr     x9, [x8, #0]          ; Unbox result
    ldr     x19, [sp, #0]
    ldr     x10, [x19, #8]        ; Unbox n
    mul     x11, x10, x9          ; n * factorial(n-1)
    ; Allocate box for result
    sub     x27, x27, #24
    ...
```
