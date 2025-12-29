# Register Allocation

This document describes the register allocation pass that maps virtual registers
to physical ARM64 registers.

## Overview

The Dark compiler uses a **linear scan** register allocator with liveness analysis.
Virtual registers from LIR are mapped to physical ARM64 registers, with spilling
to stack when registers are exhausted.

## ARM64 Register Conventions

### General-Purpose Registers

| Registers | Purpose |
|-----------|---------|
| X0 | Return value |
| X1-X8 | Caller-saved, preferred for allocation |
| X9-X10 | Excluded (used by StringHash/StringEq internally) |
| X11-X13 | Reserved as scratch registers for spill code |
| X19-X26 | Callee-saved, used when caller-saved exhausted |
| X27 | Reserved for free list base pointer (heap allocator) |
| X28 | Reserved for heap bump pointer |
| X29 | Frame pointer |
| X30 | Link register (return address) |

### Float Registers

| Registers | Purpose |
|-----------|---------|
| D0 | Float return value |
| D0-D7 | Caller-saved float registers |
| D8-D15 | Callee-saved float registers |

## Algorithm

Implemented in `5_RegisterAllocation.fs`:

### Phase 1: Liveness Analysis

Backward dataflow analysis computes which virtual registers are live at each point:

```
LiveIn(B) = GEN(B) ∪ (LiveOut(B) - KILL(B))
LiveOut(B) = ∪ LiveIn(S) for all successors S of B
```

Where:
- **GEN(B)**: Variables used before being defined in block B
- **KILL(B)**: Variables defined in block B

The analysis iterates until reaching a fixed point.

### Phase 2: Live Interval Construction

For each virtual register, compute its **live interval** (start, end):
- **Start**: First definition or use
- **End**: Last use

Intervals are sorted by start position for linear scan.

### Phase 3: Linear Scan Allocation

Process intervals in start-position order:

```
for each interval I in sorted order:
    expire_old_intervals(I.start)
    if no free registers:
        spill_at_interval(I)
    else:
        allocate_register(I)
```

**Register preference**: Caller-saved registers (X1-X8) are preferred over
callee-saved (X19-X26) to minimize prologue/epilogue overhead.

### Phase 4: Spill Code Generation

When a virtual register is spilled:
1. Store to stack slot after definition
2. Load from stack slot before each use
3. Uses scratch registers X11-X13 for spill loads

## Stack Frame Layout

```
Higher addresses
┌─────────────────────┐
│ Callee-saved regs   │ ← Saved X19-X26 if used
├─────────────────────┤
│ Spill slots         │ ← Spilled virtual registers
├─────────────────────┤
│ Frame pointer (X29) │
├─────────────────────┤
│ Link register (X30) │
└─────────────────────┘
Lower addresses (SP)
```

Stack is 16-byte aligned as required by ARM64 ABI.

## Caller-Saved Register Optimization

Around function calls, only live caller-saved registers are saved/restored:

```fsharp
let getLiveCallerSavedRegs (liveVRegs: Set<int>) (mapping: Map<int, Allocation>)
```

This generates `SaveRegs`/`RestoreRegs` instructions that save only the
registers that are actually live across the call.

## Float Register Allocation

Float values use a separate register file (D0-D15). The allocator tracks:
- Instructions that define float results (FloatSqrt, FloatAbs, IntToFloat, etc.)
- Instructions that use float operands (FloatToInt, comparison of floats)

Float registers D0-D7 are caller-saved and saved/restored around calls when live.

## Key Data Structures

```fsharp
type AllocationResult = {
    Mapping: Map<int, Allocation>     // VRegId -> register or stack slot
    StackSize: int                     // Total stack frame size
    UsedCalleeSaved: PhysReg list      // Callee-saved regs to save/restore
}

type Allocation =
    | PhysReg of PhysReg
    | StackSlot of int                 // Negative offset from frame pointer

type LiveInterval = {
    VRegId: int
    Start: int
    End: int
}
```

## Key Git History

- `a88cca3` - Implement liveness-based caller-saved register optimization
- `c0e1776` - Fix float register allocation for 8+ concurrent float values
- `58e0e7e` - Fix SSA destruction and register allocation for float values
- `1f36692` - Fix float register allocation and return type inference
- `32500a8` - Save/restore D0-D7 float registers around function calls

## Implementation Files

| File | Purpose |
|------|---------|
| `5_RegisterAllocation.fs:1-200` | Liveness analysis |
| `5_RegisterAllocation.fs:366-442` | Live interval construction |
| `5_RegisterAllocation.fs:443-553` | Linear scan algorithm |
| `5_RegisterAllocation.fs:555-700` | Apply allocation to LIR |

## Tests

See `src/Tests/e2e/register_allocation.e2e` (112 lines) for tests that stress
register pressure and spilling.
