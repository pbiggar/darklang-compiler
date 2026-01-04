# Tail Call Optimization (TCO)

This document explains how the Dark compiler handles tail call optimization.

## Overview

Tail call optimization converts tail-recursive calls into jumps, avoiding stack growth. This allows unbounded recursion for tail-recursive functions.

**Current Status**: TCO is **ENABLED**. The DCE bug that caused 197 test failures has been fixed (DeadCodeElimination.fs was not recognizing TailCall as a function call, causing stdlib functions called via tail call to be removed).

## What is a Tail Call?

A call is in **tail position** if its result is directly returned without further computation:

```dark
// Tail call - result of sumTo is directly returned
def sumTo(n: Int64, acc: Int64) : Int64 =
    if n <= 0 then acc else sumTo(n - 1, acc + n)

// NOT a tail call - result is multiplied before returning
def factorial(n: Int64) : Int64 =
    if n <= 1 then 1 else n * factorial(n - 1)
```

## Implementation

### Pass 2.7: Tail Call Detection

The `2.7_TailCallDetection.fs` pass identifies tail calls in ANF:

**Detection Pattern:**
```fsharp
Let (tempId, Call(funcName, args), Return (Var tempId))
```

If a `Let` binds a call result and the body immediately returns that same variable, it's a tail call.

**Transformation:**
```fsharp
Call(funcName, args)     →  TailCall(funcName, args)
IndirectCall(func, args) →  IndirectTailCall(func, args)
ClosureCall(clos, args)  →  ClosureTailCall(clos, args)
```

### Code Generation Difference

**Regular Call (BL instruction):**
1. Save caller-saved registers (144 bytes)
2. Set up arguments in X0-X7
3. BL (Branch and Link) - saves return address in X30
4. Restore caller-saved registers
5. Copy return value

**Tail Call (B instruction):**
1. Set up arguments in X0-X7 (parallel move)
2. B (Branch) - no link, no return
3. Callee uses caller's return address to return

The tail call skips SaveRegs/RestoreRegs because the callee will return directly to the caller's caller.

### ARM64 Instructions

| Call Type | Regular | Tail |
|-----------|---------|------|
| Direct | BL label | B label |
| Indirect | BLR Xn | BR Xn |

## The Parallel Move Problem

For tail calls, we need to set up new argument values in X0-X7 while those registers may contain values we need to read. This is a **parallel move** problem.

**Example:**
```
swap(x, y) calls swap(y, x)
// Need: X0 <- old_X1, X1 <- old_X0
// Problem: Can't do X0 <- X1 first, then X1 <- X0 (X0 already clobbered)
```

**Solution:** The compiler uses `TailArgMoves` with careful ordering:

1. **Phase 1**: Move non-register sources (immediates, memory) whose dest isn't used as source
2. **Phase 2**: Move registers that don't create cycles
3. **Phase 3**: Break cycles using temp register

### Historical Bugs (Now Fixed)

These bugs were fixed and TCO is now enabled:

1. **Phase 1 ordering** (fixed in `eb7cf84`): Non-register moves were emitted unconditionally, clobbering sources needed by other moves

2. **Phase 3 cycle handling** (fixed in `eb7cf84`): Cycle moves were emitted in arbitrary order, clobbering values

3. **DCE bug** (fixed): DeadCodeElimination.fs was not recognizing TailCall as a function call, causing stdlib functions called via tail call to be removed. This caused 197 test failures when TCO was enabled.

## Interaction with Reference Counting

TCO runs AFTER RefCountInsertion. The implementation looks through RefCountDec operations
between the call and return to detect tail calls. This is crucial because without TCO,
functions like `__reverseHelper` would use regular calls, causing intermediate cons cells
to be freed prematurely when the free list reuses those cells.

```dark
// CAN be optimized - looks through RefCountDec to find tail position
let result = tailCall() in
let _ = RefCountDec(someValue) in
return result
```

## Test Cases

From `src/Tests/e2e/tailcall.e2e`:

```dark
// Simple tail recursion
def sumTo(n: Int64, acc: Int64) : Int64 =
    if n <= 0 then acc else sumTo(n - 1, acc + n)
sumTo(10, 0)  // Expected: 55

// Large N (would overflow without TCO) - CURRENTLY DISABLED
def countDown(n: Int64) : Int64 =
    if n <= 0 then 0 else countDown(n - 1)
countDown(100000)  // Expected: 0, but test disabled
```

## Why TCO Matters

Without TCO, recursion is limited by stack size (~8MB = ~500K calls). With TCO:
- Tail-recursive functions use constant stack space
- Can iterate millions of times
- Enables functional iteration patterns (fold, map via recursion)

**Benchmark impact** (from Claude plan):
- `sum_to_n`: 152x slower than Rust without TCO
- `pisum`: Limited scale without TCO

## Git History Context

Key commits:
- `0cbed40` - Add tail call optimization (Phase 2/2 TCO)
- `6223fa0` - Add tail call IR variants and instruction encoding (Phase 1/7 TCO)
- `eb7cf84` - Fix TailArgMoves parallel move resolution bugs
- `57f0f20` - Fix TailCall float return type tracking
- `9dcb1f4` - Disable countDown(100000) test - TCO not fully working
- `9446a3d` - Disable TCO due to parallel move resolution bugs
- (later) - Re-enable TCO after fixing DCE bug (TailCall now recognized as function call)

## Design from Claude Plan

From `virtual-tumbling-bunny.md`:

**Implementation Phases:**
1. Add TailCall variants to ANF, MIR, LIR, ARM64
2. Create TailCallDetection pass
3. Update MIR optimization for side effects
4. Skip SaveRegs in MIR→LIR for tail calls
5. Emit B/BR instead of BL/BLR
6. Encode BR instruction

**V1 Simplifications:**
- Skip RefCountDec cases (safety first)
- Assume stack frame handled by jump target's prologue

## Related Files

- `src/DarkCompiler/passes/2.7_TailCallDetection.fs` - Tail position detection
- `src/DarkCompiler/passes/4_MIR_to_LIR.fs` - TailArgMoves generation
- `src/DarkCompiler/ANF.fs` - TailCall, IndirectTailCall, ClosureTailCall types
- `src/DarkCompiler/passes/6_CodeGen.fs` - B/BR emission
- `src/Tests/e2e/tailcall.e2e` - Test cases (minimal - 16 lines)
