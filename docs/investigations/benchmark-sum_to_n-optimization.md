# Benchmark Investigation: sum_to_n

## Overview

This investigation analyzes why the Dark compiler performs worse than Rust on the
`sum_to_n` benchmark, which computes the sum 1+2+...+10000 repeatedly
100 times.

### Benchmark Results

Current `benchmarks/RESULTS.md` instruction-count context:

| Compiler | Instructions | Relative |
|----------|--------------|----------|
| Rust     | 256,081      | 1.0x     |
| Dark     | 7,002,526    | 27.3x    |
| OCaml    | 9,421,844    | 36.8x    |

**Key Finding**: Rust's extreme speed is due to complete constant folding. It
computes the result (`50005000`) at compile time and stores it as an immediate
constant in the binary.

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

Current MIR still calls `sumTo(10000, 0)` inside the `repeat` loop:

```text
repeat_L1:
    v9 <- v6 - 1 : TInt64
    v10 <- Call(sumTo, [10000, 0])
    v6 <- v9 : TInt64
    v7 <- v10 : TInt64
    jump repeat_body
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

**Impact Estimate**: Up to the current 27.3x Rust-relative gap for this
benchmark if compile-time evaluation eliminates the full computation.

**Implementation Approach**:
1. Add purity analysis to mark functions as pure (no side effects)
2. In ANF optimization, detect calls to pure functions with all-constant arguments
3. Interpret/evaluate the pure function at compile time
4. Replace the call with the computed constant

**Files to Modify**:
- `src/DarkCompiler/passes/2.3_ANF_Optimization.fs` - Add constant folding
- `src/DarkCompiler/passes/2_AST_to_ANF.fs` - Track purity annotations

### 2. Loop-Invariant Code Motion for sumTo Call (Medium Impact)

**Issue**: In `repeat`, the call `sumTo(10000, 0)` always returns the same value but is called 100 times.

**Evidence**: From the MIR:
```
repeat_L1:
    v9 <- v6 - 1 : TInt64
    v10 <- Call(sumTo, [10000, 0])  ; Same call every iteration!
    v6 <- v9 : TInt64
    v7 <- v10 : TFunction ...
    jump repeat_body
```

**Impact Estimate**: Up to ~100x for the repeated inner work in this specific
pattern by reducing 100 identical `sumTo` calls to 1.

**Implementation Approach**:
1. Detect pure function calls with constant arguments inside loops
2. Hoist such calls outside the loop
3. Replace loop body references with the hoisted value

**Files to Modify**:
- `src/DarkCompiler/passes/3.5_MIR_Optimizations.fs` - Add LICM pass

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
| Constant folding for pure functions | High (up to 27.3x) | Medium | P1 |
| Loop-invariant code motion | High (~100x for repeated inner work) | Medium | P1 |

Direct conditional-branch fusion has already been implemented for this loop
shape and is no longer an open recommendation for `sum_to_n`.

## Conclusion

The primary performance gap between Dark and Rust on this benchmark is due to Rust's aggressive constant folding. While implementing full interprocedural constant folding is complex, significant gains can be achieved through:

1. **Loop-invariant code motion** - Hoist `sumTo(10000, 0)` outside the repeat loop
2. **Compile-time evaluation** - Evaluate pure functions with constant args at compile time

These two optimizations together would reduce Dark's runtime from computing 1,000,000 additions to computing just 10,000, matching the fundamental work that needs to be done at runtime if full constant folding isn't available.
