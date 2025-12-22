# Compilation Pipeline

This document provides an overview of the Darklang compiler pipeline and a complete end-to-end example.

## Overview

The compiler transforms source code through eight distinct stages:

```
Source Code
    ↓
1. Parser (passes/1_Parser.fs)
    ↓
AST (AST.fs)
    ↓
2. AST to ANF (passes/2_AST_to_ANF.fs)
    ↓
ANF (ANF.fs)
    ↓
3. ANF to MIR (passes/3_ANF_to_MIR.fs)
    ↓
MIR (MIR.fs)
    ↓
4. MIR to LIR (passes/4_MIR_to_LIR.fs)
    ↓
LIR with virtual registers (LIR.fs)
    ↓
5. Register Allocation (passes/5_RegisterAllocation.fs)
    ↓
LIR with physical registers (LIR.fs)
    ↓
6. Code Generation (passes/6_CodeGen.fs)
    ↓
ARM64 Instructions (ARM64.fs)
    ↓
7. ARM64 Encoding (passes/7_ARM64_Encoding.fs)
    ↓
Machine Code (uint32 list)
    ↓
8. Binary Generation (passes/8_Binary_Generation.fs)
    ↓
Mach-O Executable
```

Each IR type and transformation pass is documented in its respective source file.

## Complete End-to-End Example

Here's how `2 + 3 * 4` transforms through the entire pipeline:

```
Source:     "2 + 3 * 4"

AST:        BinOp(Add, IntLiteral(2),
              BinOp(Mul, IntLiteral(3), IntLiteral(4)))

ANF:        let tmp0 = 3 * 4
            let tmp1 = 2 + tmp0
            return tmp1

MIR:        v0 <- 3
            v1 <- 4
            v2 <- v0 * v1
            v3 <- 2
            v4 <- v3 + v2
            ret v4

LIR:        v0 <- Mov(Imm 3)
            v1 <- Mov(Imm 4)
            v2 <- Mul(v0, v1)
            v3 <- Mov(Imm 2)
            v4 <- Add(v3, v2)
            ret v4

Allocated:  X0 <- Mov(Imm 3)
            X1 <- Mov(Imm 4)
            X2 <- Mul(X0, X1)
            X3 <- Mov(Imm 2)
            X4 <- Add(X3, X2)
            ret X4

ARM64:      MOVZ X0, #3, LSL #0
            MOVZ X1, #4, LSL #0
            MUL X2, X0, X1
            MOVZ X3, #2, LSL #0
            ADD X4, X3, X2
            MOV X0, X4
            RET

Machine     0xD2800060  // MOVZ X0, #3
Code:       0xD2800081  // MOVZ X1, #4
            0x9B017C02  // MUL X2, X0, X1
            0xD2800043  // MOVZ X3, #2
            0x8B020064  // ADD X4, X3, X2
            0xAA0403E0  // MOV X0, X4
            0xD65F03C0  // RET

Result:     Executable that exits with code 14
```

## Testing Strategy

Each stage has dedicated tests:

1. **Unit Tests**: Test each stage in isolation
2. **Round-trip Tests**: Verify transformations preserve semantics
3. **End-to-end Tests**: Compile and verify complete programs
4. **Binary Tests**: Validate Mach-O structure

All tests use FsUnit with NUnit for readable assertions.
