# Benchmark Investigation: sum_to_n

> The original Rust reference remains unchanged, including its invariant
> `sum_to(10000)` workload. Dark mirrors that source-level optimization
> opportunity, so the constant-folding comparison below remains relevant.

## Overview

This investigation analyzes the generated work for the `sum_to_n` benchmark,
which computes the sum 1+2+...+10000 repeatedly 100 times.

### Benchmark Results

Current routine-profile evidence:

| Compiler | Instructions | Relative |
|----------|--------------|----------|
| Rust     | 256,081      | 1.0x     |
| Dark     | 70,747       | 0.28x    |
| OCaml    | 9,421,844    | 36.8x    |

**Key Finding**: Rust's extreme speed is due to complete constant folding. It
computes the result (`50005000`) at compile time and stores it as an immediate
constant in the binary.

These instruction counts are the current `benchmarks/RESULTS.md` baselines.

## Source Code

```dark
def sumTo(n: Int64, acc: Int64) : Int64 =
    if n <= 0 then acc
    else sumTo(n - 1, acc + n)

def repeat(n: Int64, acc: Int64) : Int64 =
    if n <= 0 then acc
    else repeat(n - 1, sumTo(10000, 0))

repeat(100, 0)
```

## Analysis

### Rust Optimization: Complete Constant Folding

Rust/LLVM performs aggressive interprocedural constant propagation and folding.
Looking at the Rust assembly for `main`:

```asm
mov	w8, #0x408           ; Load lower 16 bits of 50005000
movk	w8, #0x2fb, lsl #16  ; Load upper 16 bits
; ... print w8 ...
```

The entire computation is performed at compile time:
`0x2fb << 16 | 0x408 = 50005000`.

### OCaml Analysis

OCaml generates runtime code for `sum_to` and `repeat`:

```asm
; camlMain__sum_to_267
4f9cc: cmp	x0, #0x1
4f9d0: b.gt	4f9e4
4f9d4: mov	x0, x1              ; return acc
4f9d8: ldr	x30, [sp, #8]
4f9dc: add	sp, sp, #0x10
4f9e0: ret
4f9e4: add	x2, x1, x0          ; acc + n
4f9e8: sub	x1, x2, #0x1        ; adjust for tagged integers
4f9ec: sub	x0, x0, #0x2        ; n - 1 (tagged)
4f9f0: b	4f9c0               ; loop back

; camlMain__repeat_270
4fa30: orr	x1, xzr, #0x1       ; acc = 0 (tagged)
4fa34: mov	x0, #0x4e21         ; n = 10000 (tagged: 20001)
4fa38: bl	4f9b8               ; call sum_to
```

OCaml's hot loop is tight but has overhead from tagged integer representation
and GC safepoint checks in the loop.

### Dark Analysis

Current ANF keeps both constant-argument calls as runtime calls:

```text
Function _start:
let TempId 12 = repeat(100, 0)
return t12

Function repeat:
let TempId 9 = t6 - 1
let TempId 10 = sumTo(10000, 0)
let TempId 11 = TailCall(repeat, [t9, t10])
return t11
```

Current register-allocated LIR for the hot `sumTo` loop:

```text
sumTo_L1:
    X3 <- Sub(X1, Imm 1)
    X2 <- Add(X2, Reg X1)
    X1 <- Mov(Reg X3)
    Jump(Label "sumTo_body")
sumTo_body:
    Cmp(X1, Imm 0)
    CondBranch(LE, Label "sumTo_L2", Label "sumTo_L1")
```

The loop is compact after register allocation, and the previous `Cset` plus
`Branch` form has been fused to `CondBranch`.

Current Dark binary disassembly confirms the same shape. The inner `sumTo`
loop is a compact subtract/add/move loop plus compare/branch:

```asm
1b4: sub x3, x1, #0x1
1b8: add x2, x2, x1
1bc: mov x1, x3
1c0: b 0x1cc
1cc: cmp x1, #0x0
1d0: b.le 0x1c4
1d4: b 0x1b4
```

## Identified Optimization Opportunities

### 1. Constant Folding for Pure Functions (High Impact)

**Issue**: Dark doesn't evaluate pure functions with constant arguments at compile time.

**Evidence**: In the ANF/MIR, the call `repeat(100, 0)` and `sumTo(10000, 0)` are preserved as runtime calls despite both arguments being constants and both functions being pure.

```
Main:
let TempId 12 = repeat(100, 0)  ; Could be folded to 50005000
return t12
```

**Rust Comparison**: LLVM evaluates the entire computation at compile time.

**Impact Estimate**: Eliminate the remaining runtime computation by evaluating
the full constant expression at compile time.

**Implementation Approach**:
1. Add purity analysis to mark functions as pure (no side effects)
2. In ANF optimization, detect calls to pure functions with all-constant arguments
3. Interpret/evaluate the pure function at compile time
4. Replace the call with the computed constant

**Files to Modify**:
- `src/DarkCompiler/passes/2.3_ANF_Optimization.fs` - Add constant folding
- `src/DarkCompiler/passes/2_AST_to_ANF.fs` - Track purity annotations

## Instruction Count Comparison

### Hot Loop (sumTo inner loop)

| Compiler | Instructions per iteration |
|----------|----------------------------|
| Rust     | 0 (constant folded)        |
| OCaml    | ~6 (with GC check)         |
| Dark     | ~5 after branch fusion     |

### Dark sumTo loop body:
1. `sub` - n - 1
2. `add` - acc + n
3. `mov` - update n
4. `b sumTo_body` - jump
5. `cmp` plus conditional branch in `sumTo_body`

### OCaml sum_to loop body:
1. `add x2, x1, x0` - acc + n
2. `sub x1, x2, #1` - tagged adjust
3. `sub x0, x0, #2` - n - 1 (tagged)
4. `cmp x0, #1` - compare with 0 (tagged)
5. `b.gt loop` - conditional branch
6. GC safepoint check (occasional)

## Recommendations Summary

| Optimization | Impact | Effort | Priority |
|--------------|--------|--------|----------|
| Constant folding for pure functions | High (eliminate runtime computation) | Medium | P1 |

## Conclusion

Rust's generated code still performs less runtime work because it evaluates the
whole computation at compile time. Compile-time evaluation of effect-free
functions with constant arguments is the remaining opportunity for Dark.
