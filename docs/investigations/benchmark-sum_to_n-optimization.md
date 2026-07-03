# Benchmark Investigation: sum_to_n

## Overview

This investigation analyzes why the Dark compiler performs worse than Rust on the `sum_to_n` benchmark, which computes the sum 1+2+...+10000 repeatedly 100 times.

### Benchmark Results

| Compiler | Time (mean) | Relative |
|----------|-------------|----------|
| Rust     | 97.5 µs     | 1.0x     |
| Dark     | 314.7 µs    | 3.2x     |
| OCaml    | 589.1 µs    | 6.0x     |

**Key Finding**: Rust's extreme speed is due to complete constant folding - it computes the result (50005000) at compile time, storing it as an immediate constant in the binary.

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

Rust/LLVM performs aggressive interprocedural constant propagation and folding. Looking at the Rust assembly for `main`:

```asm
mov	w8, #0x408           ; Load lower 16 bits of 50005000
movk	w8, #0x2fb, lsl #16  ; Load upper 16 bits
; ... print w8 ...
```

The entire computation is performed at compile time! `0x2fb << 16 | 0x408 = 50005000`

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

OCaml's hot loop is tight but has overhead from:
1. Tagged integer representation (odd numbers = integers)
2. GC safepoint checks in the loop

### Dark Analysis

Dark's generated code for `sumTo`:

```asm
; sumTo function at 0x190
190: stp	x29, x30, [sp, #-16]!  ; frame setup
194: mov	x29, sp
198: mov	x9, x0                 ; x9 = n
19c: mov	x10, x1                ; x10 = acc
...
1b8: sub	x2, x3, #0x1           ; n - 1
1bc: add	x1, x1, x3             ; acc + n
1c0: mov	x3, x2                 ; update n
1c4: b	0x1d0                  ; loop
1c8: mov	x0, x1                 ; return acc
1d0: cmp	x3, #0x0               ; n <= 0?
1d4: b.le	0x1b4
1d8: b	0x1b8
1dc: ldp	x29, x30, [sp], #16    ; frame cleanup
1e0: ret
```

Dark's current loop shape is:

```
sumTo_L1:
    X2 <- Sub(X3, Imm 1)
    X1 <- Add(X1, Reg X3)
    X3 <- Mov(Reg X2)
    Jump(Label "sumTo_body")
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

**Impact Estimate**: 3x speedup for this benchmark (matching Rust).

**Implementation Approach**:
1. Add purity analysis to mark functions as pure (no side effects)
2. In ANF optimization, detect calls to pure functions with all-constant arguments
3. Interpret/evaluate the pure function at compile time
4. Replace the call with the computed constant

**Files to Modify**:
- `src/DarkCompiler/passes/2.3_ANF_Optimization.fs` - Add constant folding
- `src/DarkCompiler/passes/2_AST_to_ANF.fs` - Track purity annotations

### 2. Conditional Branch Optimization (Low Impact)

**Issue**: Dark uses a two-instruction sequence for conditional branches.

**Evidence**: Dark's code uses `cset` followed by `branch`:

```
v10011 <- Cset(LE)
Branch(v10011, Label "sumTo_L0", Label "sumTo_L1")
```

Generated as:
```asm
cmp	x3, #0x0
cset	w?, le      ; Set register to 1 if LE
b.?? label         ; Branch based on register
```

OCaml uses a single conditional branch:
```asm
cmp	x0, #0x1
b.gt	4f9e4
```

**Impact Estimate**: ~5% (saves one instruction in the hot loop).

**Implementation Approach**:
1. Modify code generation to directly emit conditional branches after comparisons
2. Remove the intermediate `cset` instruction when immediately followed by a branch

**Files to Modify**:
- `src/DarkCompiler/passes/6_CodeGen.fs` - Optimize branch generation

### 4. Loop-Invariant Code Motion for sumTo Call (Medium Impact)

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

**Impact Estimate**: ~99x speedup for this specific pattern (reduce 100 calls to 1).

**Implementation Approach**:
1. Detect pure function calls with constant arguments inside loops
2. Hoist such calls outside the loop
3. Replace loop body references with the hoisted value

**Files to Modify**:
- `src/DarkCompiler/passes/3.5_MIR_Optimizations.fs` - Add LICM pass

## Instruction Count Comparison

### Hot Loop (sumTo inner loop)

| Compiler | Instructions per iteration |
|----------|---------------------------|
| Rust     | 0 (constant folded)       |
| OCaml    | ~6 (with GC check)        |
| Dark     | ~6                        |

### Dark sumTo loop body:
1. `sub x2, x3, #1` - n - 1
2. `add x1, x1, x3` - acc + n
3. `mov x3, x2` - update n
4. `b sumTo_body` - jump
5. `cmp x3, #0` - compare
6. `b.le/b.gt` - conditional branch

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
| Constant folding for pure functions | High (3x) | Medium | P1 |
| Loop-invariant code motion | High (99x for pattern) | Medium | P1 |
| Direct conditional branches | Low (5%) | Low | P3 |

## Conclusion

The primary performance gap between Dark and Rust on this benchmark is due to Rust's aggressive constant folding. While implementing full interprocedural constant folding is complex, significant gains can be achieved through:

1. **Loop-invariant code motion** - Hoist `sumTo(10000, 0)` outside the repeat loop
2. **Compile-time evaluation** - Evaluate pure functions with constant args at compile time

These two optimizations together would reduce Dark's runtime from computing 1,000,000 additions to computing just 10,000, matching the fundamental work that needs to be done at runtime if full constant folding isn't available.
