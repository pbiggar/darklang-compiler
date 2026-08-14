# Benchmark Investigation: tak (Takeuchi Function)

**Date:** 2026-01-15
**Last checked:** 2026-07-15
**Benchmark:** tak
**Randomly selected:** Yes

## Executive Summary

The Dark implementation of the Takeuchi function still performs worse than Rust,
with current evidence pointing to:

1. **Conservative argument lifetime handling**: Dark moves all three arguments to
   callee-saved registers before the first comparison, while Rust compares `x0`
   and `x1` first and only saves `x2` on the early-exit path.
2. **Higher instruction count**: current benchmark
   results show Dark at 635,804,177 instructions versus Rust at 39,336,450
   instructions for `tak` (16.2x).
3. **Redundant entry argument copies in emitted assembly**: current Dark ARM64
   assembly copies arguments through temporary registers before moving them into
   the long-lived callee-saved registers used by the recursive loop.

## Benchmark Overview

The Takeuchi function (tak) is a classic benchmark testing recursion and function call overhead:

```dark
let tak(x: Int64, y: Int64, z: Int64) : Int64 =
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
- Post-register-allocation self-moves are removed by `LIR_Peephole.removeSelfMovesFromFunction`.
- `tak` still uses five callee-saved registers (`X19` through `X23`) across the
  recursive calls.
- Dark still saves all arguments before the first comparison, unlike Rust.
- Current generated ARM64 has an additional entry copy chain before the compare:

```asm
mov x9, x0
mov x10, x1
mov x11, x2
mov x0, x9
mov x1, x10
mov x2, x11
mov x21, x0
mov x22, x1
mov x20, x2
```

The LIR after register allocation only contains the final three moves into
`X21`, `X22`, and `X20`, so the extra `x9`/`x10`/`x11` round trip is introduced
after LIR, during final code generation or encoding.

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

Current register allocation coalesces entry-block PHI sources directly into the
loop-carried registers for `tak`. The remaining PHI cost appears after
recursive calls, where two call results are assigned back to the loop-carried
registers.

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

### 2. Improved PHI Coalescing for Loop Backedge (Medium Impact)

**Problem:** Loop-backedge PHI results still require moves after recursive calls.

**Evidence:**
```
v10018 <- Phi([(Reg v2, tak_entry), ...])
```
Current loop-carried PHI lowering materializes moves from recursive call results
back into the loop-carried registers:

```
X21 <- Mov(Reg X23)
X22 <- Mov(Reg X19)
```

**Solution:**
- Investigate whether loop-backedge PHI moves can be reduced without increasing
  call-clobber pressure.

**Files to modify:**
- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Enhance `collectPhiPreferences`

### 3. Remove Redundant Function-Entry Argument Copy Chains (Low Impact)

**Problem:** The emitted ARM64 for `tak` copies `x0`, `x1`, and `x2` through
`x9`, `x10`, and `x11`, restores them to the argument registers, and only then
moves them to `x21`, `x22`, and `x20`.

**Evidence:**
Current LIR does not contain the temporary-register round trip, but
`aarch64-linux-gnu-objdump -b binary -m aarch64 -D dark.out` shows it at the
start of the `tak` body before the first comparison.

**Solution:**
- Skip argument-preservation temporaries when the destination argument register
  is not overwritten before its final assigned register move.
- Alternatively, teach the late peephole/codegen path to collapse
  `arg -> temp -> arg -> callee-saved` into `arg -> callee-saved`.

**Files to inspect:**
- `src/DarkCompiler/passes/arm64/6_CodeGen.fs`
- `src/DarkCompiler/passes/arm64/7_Emit.fs`
- `src/DarkCompiler/passes/arm64/7_Encoding.fs`

## Quantified Performance Impact

| Optimization | Estimated Speedup | Implementation Effort |
|--------------|------------------|----------------------|
| Argument register reuse before calls | 1-2% | Medium |
| Loop-backedge PHI coalescing | Unknown | Medium |
| Remove redundant function-entry argument copy chains | <1% | Low |

The direct speedup from PHI coalescing is not quantified by current local
evidence.

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
Current LIR shows three entry moves plus two non-self PHI-result moves in the
recursive loop:
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

### Current Assembly and Cachegrind Evidence

Current Dark output for `tak(24, 16, 8)` repeated ten times is `9`.

Targeted local Cachegrind after rebuilding only the Dark `tak` benchmark reports
635,804,177 instructions, 261,801,696 data refs, 24,933,507 branches, and
4,331,831 branch mispredicts. This matches the current benchmark-results table
for Dark instruction count and keeps the Rust comparison at 16.2x.

Dark's emitted ARM64 starts the `tak` function with a full frame and five
callee-saved spills, then performs the redundant argument-copy chain before
entering the recursive loop comparison. The recursive loop still performs three
recursive `bl` instructions and two loop-carried result moves before jumping
back to the comparison.

## Conclusion

The tak benchmark still reveals inefficiencies in Dark's register allocation and
PHI lowering phases. Tail-call detection works, and current LIR keeps the
entry path compact while still using conservative argument-save placement before
the first comparison. Current assembly adds a small late-stage argument-copy
inefficiency not visible in LIR. The durable current findings are that
argument-save placement, loop-backedge PHI-result moves, redundant entry
argument copies, and the large 16.2x instruction-count gap versus Rust.
