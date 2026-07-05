# Fibonacci Benchmark Optimization Investigation

## Summary

The fib benchmark computes the 35th Fibonacci number using naive recursive implementation: `fib(n) = fib(n-1) + fib(n-2)`.

**Performance Results:**
- Rust: ~21ms per run
- Dark: ~41ms per run
- **Dark is ~2x slower than Rust**

## Benchmark Source Code

### Dark (`benchmarks/problems/fib/dark/main.dark`)
```dark
def fib(n: Int64) : Int64 =
    if n <= 1 then n
    else fib(n - 1) + fib(n - 2)

fib(35)
```

### Rust (`benchmarks/problems/fib/rust/main.rs`)
```rust
fn fib(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    println!("{}", fib(35));
}
```

## Analysis

### Rust Optimized Code (21 instructions total)

LLVM performs a remarkable optimization called **"sibling call optimization"** or **"tail recursion modulo cons"**. Instead of making two recursive calls per invocation, it converts the second call into a loop:

```asm
; Function prologue (8 instructions)
83d8: stp x29, x30, [sp, #-32]!    ; Save frame pointer and return address
83dc: stp x20, x19, [sp, #16]      ; Save callee-saved registers
83e0: mov x29, sp                   ; Set frame pointer
83e4: cmp x0, #2                    ; Compare n with 2
83e8: b.ge 83fc                     ; If n >= 2, jump to recursive case

; Base case: n <= 1, return n (5 instructions)
83ec: add x0, x0, xzr               ; Return n (identity move)
83f0: ldp x20, x19, [sp, #16]       ; Restore registers
83f4: ldp x29, x30, [sp], #32       ; Restore fp/lr
83f8: ret                           ; Return

; Recursive case - LOOP instead of double recursion (8 instructions)
83fc: mov x19, xzr                  ; accumulator = 0
8400: mov x20, x0                   ; x20 = n (loop counter)
; LOOP START:
8404: sub x0, x20, #1               ; x0 = n - 1
8408: bl 83d8                       ; fib(n-1) - only ONE recursive call
840c: mov x8, x0                    ; x8 = fib(n-1)
8410: sub x0, x20, #2               ; x0 = n - 2
8414: cmp x20, #3                   ; Compare n with 3
8418: add x19, x8, x19              ; accumulator += fib(n-1)
841c: mov x20, x0                   ; x20 = n - 2 (update counter)
8420: b.hi 8404                     ; If n > 3, continue loop
; LOOP END

8424: add x0, x0, x19               ; result = (n-2) + accumulator
8428: ldp x20, x19, [sp, #16]       ; Restore
842c: ldp x29, x30, [sp], #32
8430: ret
```

**Key insight**: LLVM recognizes that `fib(n-2)` is in "tail position modulo addition" and transforms it into iteration. This reduces the recursive depth by half and eliminates stack operations for the second call.

### Dark Generated Code (LIR after Register Allocation)

```
fib:
  Label "fib_entry":
    X21 <- Mov(Reg X0)              ; Save n to callee-saved register
    Jump(Label "fib_body")

  Label "fib_body":
    Cmp(X21, Imm 1)                 ; Compare n with 1
    CondBranch(LE, Label "fib_L0", Label "fib_L1")

  Label "fib_L0":                   ; Base case: n <= 1
    X19 <- Mov(Reg X21)             ; Result = n
    Jump(Label "fib_L2")

  Label "fib_L1":                   ; Recursive case
    X19 <- Sub(X21, Imm 1)          ; n - 1
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19)
    X20 <- Call(fib, [Reg X19])     ; FIRST recursive call
    RestoreRegs([], [])
    X20 <- Mov(Reg X0)

    X19 <- Sub(X21, Imm 2)          ; n - 2
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19)
    X19 <- Call(fib, [Reg X19])     ; SECOND recursive call
    RestoreRegs([], [])
    X19 <- Mov(Reg X0)

    X19 <- Add(X20, Reg X19)        ; fib(n-1) + fib(n-2)
    X19 <- Mov(Reg X19)             ; REDUNDANT self-move
    Jump(Label "fib_L2")

  Label "fib_L2":
    X0 <- Mov(Reg X19)
    Ret
```

**Dark makes TWO recursive calls per invocation**, while Rust makes only ONE (plus a loop). This is the primary source of the 2x slowdown.

## Identified Optimization Opportunities

### 1. Sibling Call Optimization / Tail Recursion Modulo Cons

**Impact: ~50% performance improvement (estimated)**

**Root Cause:**
The pattern `fib(n-1) + fib(n-2)` can be transformed. The second recursive call `fib(n-2)` is in "tail position modulo the addition". This means we can:
1. Make the first call `fib(n-1)`
2. Instead of a second recursive call, loop back with `n-2` and accumulate

**Evidence from Rust:**
```asm
; Rust loops instead of making second call:
8404: sub x0, x20, #1               ; n - 1
8408: bl 83d8                       ; fib(n-1)
8418: add x19, x8, x19              ; accumulator += result
841c: mov x20, x0                   ; n = n - 2
8420: b.hi 8404                     ; loop back
```

**Evidence from Dark (no such optimization):**
```
X20 <- Call(fib, [Reg X19])     ; First call
...
X19 <- Call(fib, [Reg X19])     ; Second call - could be a loop!
```

**Implementation Approach:**
This is an advanced optimization known as "tail recursion modulo cons" (TRMC) or more generally "accumulator passing style transformation". Implementation options:

1. **ANF-level transformation**: Detect pattern `let t1 = f(a) in let t2 = f(b) in t1 + t2` where the second call would be tail position without the addition. Transform to accumulator-passing style.

2. **MIR-level loop formation**: After detecting that a recursive call's result is only used in a commutative operation before returning, convert to a loop.

3. **LIR-level sibling call**: When two calls to the same function exist in a block, and the second call's result feeds into a simple operation with the first call's result, convert the second call to a branch back.

**Files to Modify:**
- `src/DarkCompiler/passes/2.7_TailCallDetection.fs` - Extend to detect "tail modulo cons" pattern
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs` - Generate loop structure for TRMC calls
- Or add new pass `src/DarkCompiler/passes/2.8_SiblingCallOptimization.fs`

---

### 2. Phi Node Resolution Optimization

**Impact: ~5% performance improvement (estimated)**

**Root Cause:**
The phi node in `fib_L2` requires a move to unify values from different paths:
```
Label "fib_L2":
    ; Phi resolution: result is in X19 from either path
    X0 <- Mov(Reg X19)
    Ret
```

In the current implementation, both paths set X19, so the phi resolution is optimal. However, the move from X19 to X0 before return could potentially be avoided if the result were computed directly in X0.

**Implementation Approach:**
Register allocation coalescing could assign X19 to X0 directly since they have the same value at the return point, avoiding the final move.

**Files to Modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Improve coalescing for return values

---

## Summary of Expected Improvements

| Optimization | Estimated Impact | Complexity |
|--------------|-----------------|------------|
| Sibling Call / TRMC | 40-50% | High |
| Phi/Return Coalescing | 5% | Medium |

**Total estimated improvement: ~2x faster** (bringing Dark to parity with Rust)

## Recommended Implementation Order

1. **Sibling Call Optimization** - Major impact, high complexity, requires careful design
2. **Phi/Return Coalescing** - Medium complexity, good follow-up

## Appendix: Full IR Dumps

### Dark ANF (before optimization)
```
Function fib:
let TempId 1 = t0 <= 1
if t1 then
return t0
else
let TempId 2 = t0 - 1
let TempId 3 = fib(t2)
let TempId 4 = t0 - 2
let TempId 5 = fib(t4)
let TempId 6 = t3 + t5
return t6

Main:
let TempId 7 = fib(35)
return t7
```

### Dark MIR (Control Flow Graph)
```
Function fib:
  fib_entry:
    jump fib_body
  fib_L0:
    v7 <- v0 : TInt64
    jump fib_L2
  fib_L1:
    v2 <- v0 - 1 : TInt64
    v3 <- Call(fib, [v2])
    v4 <- v0 - 2 : TInt64
    v5 <- Call(fib, [v4])
    v6 <- v3 + v5 : TFunction ([TInt64], TInt64)
    v7 <- v6 : TInt64
    jump fib_L2
  fib_L2:
    ret v7
  fib_body:
    v1 <- v0 <= 1 : TInt64
    branch v1 ? fib_L0 : fib_L1
```

### Dark LIR (After Register Allocation)
```
fib:
  Label "fib_L0":
    X19 <- Mov(Reg X21)
    Jump(Label "fib_L2")
  Label "fib_L1":
    X19 <- Sub(X21, Imm 1)
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19)
    X20 <- Call(fib, [Reg X19])
    RestoreRegs([], [])
    X20 <- Mov(Reg X0)
    X19 <- Sub(X21, Imm 2)
    SaveRegs([], [])
    ArgMoves(X0 <- Reg X19)
    X19 <- Call(fib, [Reg X19])
    RestoreRegs([], [])
    X19 <- Mov(Reg X0)
    X19 <- Add(X20, Reg X19)
    X19 <- Mov(Reg X19)
    Jump(Label "fib_L2")
  Label "fib_L2":
    X0 <- Mov(Reg X19)
    Ret
  Label "fib_body":
    Cmp(X21, Imm 1)
    CondBranch(LE, Label "fib_L0", Label "fib_L1")
  Label "fib_entry":
    X21 <- Mov(Reg X0)
    Jump(Label "fib_body")
```
