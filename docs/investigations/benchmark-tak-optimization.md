# Benchmark Investigation: tak (Takeuchi Function)

**Date:** 2026-01-15
**Last checked:** 2026-07-05
**Benchmark:** tak
**Randomly selected:** Yes

## Executive Summary

The Dark implementation of the Takeuchi function still performs worse than Rust,
but the original entry-block PHI-copy evidence is stale. Current ARM64 LIR has
only three entry argument moves for `tak`, not the six-move chain previously
recorded. The remaining evidence points to:

1. **Conservative argument lifetime handling**: Dark moves all three arguments to
   callee-saved registers before the first comparison, while Rust compares `x0`
   and `x1` first and only saves `x2` on the early-exit path.
2. **Higher instruction count despite fixed entry PHI copies**: current benchmark
   results show Dark at 635,804,177 instructions versus Rust at 39,336,450
   instructions for `tak` (16.2x).

## Benchmark Overview

The Takeuchi function (tak) is a classic benchmark testing recursion and function call overhead:

```dark
def tak(x: Int64, y: Int64, z: Int64) : Int64 =
    if x <= y then z
    else tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
```

This creates deeply nested recursive calls - tak(24, 16, 8) makes millions of function invocations.

## Performance Comparison

| Language | Current instruction count |
|----------|---------------------------|
| Rust     | 39,336,450 |
| Dark     | 635,804,177 (16.2x) |

## Code Comparison

### Rust tak Function Entry (lines 110-130 of main.s)

```asm
_ZN4main3tak17hc54be1d56ea100feE:
    stp    x29, x30, [sp, #-64]!    ; Stack frame setup
    str    x23, [sp, #16]            ; Save callee-saved
    stp    x22, x21, [sp, #32]
    stp    x20, x19, [sp, #48]
    mov    x29, sp
    cmp    x0, x1                    ; DIRECT comparison using args
    mov    x19, x2                   ; Only save z to callee-saved
    b.ls   .LBB4_3                   ; Early exit path
    mov    x20, x1                   ; Save y (only in non-exit path)
    mov    x21, x0                   ; Save x
```

Key optimizations in Rust:
- **4 register saves** total (X19-X23, frame pointer)
- **Direct use of argument registers** for the comparison
- **Conditional saves**: X20, X21 only saved if continuing

### Dark tak Function Entry (current LIR after register allocation)

```
Label "tak_entry":
    X21 <- Mov(Reg X0)    ; Save x to callee-saved
    X22 <- Mov(Reg X1)    ; Save y to callee-saved
    X20 <- Mov(Reg X2)    ; Save z to callee-saved
    Jump(Label "tak_body")
```

Current status:
- The previous three redundant PHI-entry copies are no longer present.
- Post-register-allocation self-moves are removed by `LIR_Peephole.removeSelfMovesFromFunction`.
- `tak` still uses five callee-saved registers (`X19` through `X23`) across the
  recursive calls.
- Dark still saves all arguments before the first comparison, unlike Rust.

The loop body still contains non-self PHI-result moves after recursive calls:

```
X21 <- Mov(Reg X23)
X22 <- Mov(Reg X19)
```

### Root Cause: PHI Node Lowering

The MIR shows the PHI nodes:
```
tak_body:
    v10018 <- Phi([(Reg v2, tak_entry), (Reg v10030, tak_L1)])      ; z
    v10019 <- Phi([(Reg v1, tak_entry), (Reg v10029, tak_L1)])      ; y
    v10020 <- Phi([(Reg v0, tak_entry), (Reg v10028, tak_L1)])      ; x
```

Current register allocation coalesces the entry-block PHI sources directly into
the loop-carried registers for `tak`, so the old `X19 <- X2; X22 <- X19`
evidence no longer applies. The remaining PHI cost appears after recursive
calls, where two call results are assigned back to the loop-carried registers.

## Optimization Opportunities

### 1. Argument Register Reuse Before Calls (Medium Impact)

**Problem:** Dark saves all `tak` arguments to callee-saved registers before the
initial comparison.

**Evidence:**
Rust emits `cmp x0, x1` before moving `x2` to `x19`, while current Dark LIR
emits the three entry moves and then compares `X21` with `X22` in `tak_body`.

**Solution:**
- Prefer argument registers for values used before the first possible call.
- Move values to callee-saved registers only when they must survive recursive
  calls or loop backedges.

**Files to modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Add argument register
  preference/lifetime handling

### 2. Improved PHI Coalescing (Implemented for Entry Path, Still Relevant for Loop Backedge)

**Problem:** PHI nodes with entry block operands don't share registers with their sources.

**Evidence:**
```
v10018 <- Phi([(Reg v2, tak_entry), ...])
```
The stale evidence for an entry-block copy chain no longer appears in current
LIR. The current loop-carried PHI lowering still materializes moves from the
three recursive call results back into the loop-carried registers.

**Solution:**
- Keep the entry-path improvement.
- Investigate whether loop-backedge PHI moves can be reduced without increasing
  call-clobber pressure.

**Files to modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Enhance `collectPhiPreferences`

## Quantified Performance Impact

| Optimization | Estimated Speedup | Implementation Effort |
|--------------|------------------|----------------------|
| Argument register reuse before calls | 1-2% | Medium |
| Loop-backedge PHI coalescing | Unknown | Medium |

The previous combined estimate of **7-13%** depended on stale entry-copy
evidence and should not be used as current guidance.

## Detailed IR Analysis

### ANF Stage
The ANF is clean and shows correct tail call detection:
```
let TempId 10 = TailCall(tak, [t5, t7, t9])
```

### MIR Stage
The MIR correctly implements the tail recursion as a loop:
```
tak_L1:
    v4 <- v0 - 1 : TInt64
    v5 <- Call(tak, [v4, v1, v2])
    ...
    v0 <- v5 : TFunction ([TInt64; TInt64; TInt64], TInt64)
    v1 <- v7 : TFunction ([TInt64; TInt64; TInt64], TInt64)
    v2 <- v9 : TFunction ([TInt64; TInt64; TInt64], TInt64)
    jump tak_body
```

The loop structure is correct, but the variable reassignments create PHI nodes that don't coalesce well.

### LIR Stage (Before Register Allocation)
Shows PHI nodes that will cause register pressure:
```
v10018 <- Phi([(Reg v2, tak_entry), (Reg v10030, tak_L1)])
v10019 <- Phi([(Reg v1, tak_entry), (Reg v10029, tak_L1)])
v10020 <- Phi([(Reg v0, tak_entry), (Reg v10028, tak_L1)])
```

### LIR Stage (After Register Allocation)
The old six-move entry chain is gone. Current LIR shows three entry moves plus
two non-self PHI-result moves in the recursive loop:
```
Label "tak_entry":
    X21 <- Mov(Reg X0)
    X22 <- Mov(Reg X1)
    X20 <- Mov(Reg X2)

Label "tak_L1":
    ...
    X21 <- Mov(Reg X23)
    X22 <- Mov(Reg X19)
```

## Conclusion

The tak benchmark still reveals inefficiencies in Dark's register allocation and
PHI lowering phases, but current evidence is narrower than the original
investigation. Tail-call detection works and entry-block PHI copies are now
coalesced. Post-register-allocation self-move elimination is also complete for
the current `tak` LIR. The durable current findings are the conservative
argument-save placement before the first comparison and the large 16.2x
instruction-count gap versus Rust.
