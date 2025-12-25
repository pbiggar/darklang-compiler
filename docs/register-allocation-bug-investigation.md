# Register Allocation Bug Investigation (2025-12-25)

## Summary

A bug was found where list pattern matching fails when there are 5+ let bindings before the match, causing register spilling.

## Symptoms

| Test Case | 4 lets | 5 lets |
|-----------|--------|--------|
| `[a,b] -> a + b` | ✓ 15 | ✓ 15 |
| `[a,b] -> a` | ✓ 7 | ✗ 99 |
| `[a,b] -> 42` | ✓ 42 | ✗ 99 |

**Strange pattern**: Operations (`a+b`, `a+0`) work, but direct returns (`a`, `b`, `42`) fail.

## Test Cases

```dark
// Works (4 lets)
let x = 1 in let y = 2 in let z = 3 in let w = 4 in
match [7, 8] with | [a, b] -> a | _ -> 99
// Returns: 7 ✓

// Fails (5 lets)
let x = 1 in let y = 2 in let z = 3 in let w = 4 in let v = 5 in
match [7, 8] with | [a, b] -> a | _ -> 99
// Returns: 99 ✗ (should be 7)

// But this works with 5 lets!
let x = 1 in let y = 2 in let z = 3 in let w = 4 in let v = 5 in
match [7, 8] with | [a, b] -> a + b | _ -> 99
// Returns: 15 ✓
```

## Key Difference Between 4-let and 5-let

In L0, the 5-let case has an extra Store instruction that spills the CSET result:

```
// 4-let L0 (no spill):
L0:
  HeapLoad (Physical X8, Physical X6, 8)
  HeapLoad (Physical X9, Physical X6, 16)
  Cmp (Physical X9, Imm 0L)
  Cset (Physical X10, NE)
  Branch (Physical X10, "L3", "L4")

// 5-let L0 (with spill):
L0:
  HeapLoad (Physical X9, Physical X7, 8)
  HeapLoad (Physical X10, Physical X7, 16)
  Cmp (Physical X10, Imm 0L)
  Cset (Physical X11, NE)
  Store (-8, Physical X11)           ← Extra spill!
  Branch (Physical X11, "L3", "L4")
```

## What Was Verified (All Looked Correct)

- MIR control flow structure
- LIR before and after register allocation
- ARM64 instruction encoding
- Branch offsets (CBNZ correctly computed)
- Stack slot addresses (FP-8 through FP-32)
- Heap allocations and structure
- STUR/LDUR encoding for stack operations

## Hypothesis

The bug is in register allocation or live range analysis when register pressure causes spilling:

1. Live ranges may not be correctly computed across block boundaries
2. Spilled values may not be correctly reloaded
3. The same virtual register used for different purposes in different blocks may cause conflicts

## Files to Investigate

- `src/DarkCompiler/passes/5_RegisterAllocation.fs` - Main register allocator
- Look at how live ranges are computed across blocks
- Check if spilled values are correctly handled when crossing block boundaries

## The Operations-vs-Direct-Returns Mystery

The strangest aspect: `a+b` works but `a` alone fails. This suggests the issue is not just about the branch decision, but something about how L6 (the success branch) is structured:

```
// Works - L6 uses stack slot:
L6:
  Add (Physical X11, Physical X9, StackSlot -16)
  Store (-40, Physical X11)
  Mov (Physical X3, StackSlot -40)

// Fails - L6 uses register directly:
L6:
  Mov (Physical X3, Reg (Physical X9))
```

When L6 references a stack slot, it works. When it references a register directly, it fails. This points to register values not being preserved correctly across block transitions.
