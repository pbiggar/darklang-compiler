# ARM64 Code Generation

This document describes how the Dark compiler generates ARM64 machine code.

## Overview

The compiler generates native ARM64 binaries directly, without an assembler.
Two passes handle code generation:
- Pass 6: LIR → ARM64 instructions
- Pass 7: ARM64 instructions → machine code bytes

## ARM64 Instruction Types

Defined in `ARM64.fs`:

```fsharp
type Instr =
    | MOVZ of dest * imm * shift      // Move with zero
    | MOVK of dest * imm * shift      // Move with keep
    | ADD_imm of dest * src * imm     // Add immediate
    | ADD_reg of dest * src1 * src2   // Add register
    | SUB_imm / SUB_reg               // Subtract
    | MUL / SDIV / MSUB               // Multiply, divide, mod
    | CMP / CSET                      // Compare and set
    | B / BL                          // Branch, branch-link
    | BR / BLR                        // Branch register
    | CBZ / CBNZ                      // Compare and branch
    | LDR / STR / LDRB / STRB         // Load/store
    | SVC                             // System call
    // Floating-point
    | FADD / FSUB / FMUL / FDIV
    | FSQRT / FABS / FNEG
    | FCMP / SCVTF / FCVTZS
    // ...
```

## Two-Pass Encoding

### Pass 1: Label Collection

First pass computes byte offsets for all labels:

```fsharp
let computeLabelPositions (instrs: Instr list) : Map<string, int>
```

### Pass 2: Encoding

Second pass encodes instructions with resolved branch offsets:

```fsharp
let encode (instr: Instr) : uint32 list
```

## Instruction Encoding

Each ARM64 instruction is 32 bits. Examples:

### MOVZ (Move with Zero)
```
sf(31) opc(30-29) 100101(28-23) hw(22-21) imm16(20-5) Rd(4-0)
```

### ADD Immediate
```
sf(31) 0 0 10001(28-24) shift(23-22) imm12(21-10) Rn(9-5) Rd(4-0)
```

### Branch
```
000101(31-26) imm26(25-0)  // Signed offset in words
```

## Calling Convention

ARM64 AAPCS64 calling convention:

### Arguments
- X0-X7: First 8 integer arguments
- D0-D7: First 8 float arguments
- Stack: Additional arguments

### Return Values
- X0: Integer return value
- D0: Float return value

### Saved Registers
- X0-X18: Caller-saved
- X19-X28: Callee-saved
- X29: Frame pointer
- X30: Link register
- SP: Stack pointer

### Reserved Registers
- X27: Free list base (heap allocator)
- X28: Heap bump pointer

## Stack Frame

Function prologue:
```assembly
STP X29, X30, [SP, #-16]!  // Save FP and LR
MOV X29, SP                 // Set new FP
SUB SP, SP, #frame_size     // Allocate locals
// Save callee-saved registers if used
```

Function epilogue:
```assembly
// Restore callee-saved registers
ADD SP, SP, #frame_size
LDP X29, X30, [SP], #16
RET
```

## Syscalls

Platform-specific system calls:

### macOS
```assembly
MOVZ X16, #syscall_num     // X16 = syscall number
SVC #0x80                   // Trigger syscall
```

### Linux
```assembly
MOVZ X8, #syscall_num      // X8 = syscall number
SVC #0                      // Trigger syscall
```

Common syscalls:
- write: macOS=0x2000004, Linux=64
- exit: macOS=0x2000001, Linux=93
- read: macOS=0x2000003, Linux=63

## Heap Allocation

Bump allocator in generated code:

```assembly
// Allocate N bytes
ADD X0, X28, #N            // X0 = new pointer
MOV X28, X0                // Update bump pointer
SUB X0, X0, #N             // Return original pointer
```

## Code Generation Examples

### Function Call
```fsharp
Call (dest, "add", [Reg X1, Imm 5L])
```

Generates:
```assembly
MOV X0, X1        // First arg
MOV X1, #5        // Second arg
BL add            // Call function
MOV dest, X0      // Copy result
```

### Heap Allocation
```fsharp
HeapAlloc (dest, 24)
```

Generates:
```assembly
ADD X0, X28, #24   // Calculate new end
MOV X28, X0        // Update bump pointer
SUB dest, X0, #24  // Return start address
```

### Conditional Branch
```fsharp
Branch (cond, thenLabel, elseLabel)
```

Generates:
```assembly
CMP cond, #0
B.EQ elseLabel
// fall through to then
```

## Float Operations

Float instructions use D-registers:

```assembly
FADD D0, D1, D2    // D0 = D1 + D2
FSQRT D0, D1       // D0 = sqrt(D1)
SCVTF D0, X0       // D0 = (float)X0
FCVTZS X0, D0      // X0 = (int)D0
```

## Implementation Files

| File | Lines | Purpose |
|------|-------|---------|
| `6_CodeGen.fs` | 2872 | LIR → ARM64 |
| `7_ARM64_Encoding.fs` | 968 | ARM64 → bytes |
| `ARM64.fs` | ~200 | ARM64 instruction types |
| `Runtime.fs` | ~300 | Runtime support (print, etc.) |

## Key Functions

| Function | Purpose |
|----------|---------|
| `generateFunction` | Generate code for a function |
| `generateInstr` | Generate code for one LIR instruction |
| `encode` | Encode ARM64 instruction to bytes |
| `computeLabelPositions` | First pass label resolution |
