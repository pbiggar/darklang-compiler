# Compilation Pipeline

This document explains each stage of the Darklang compiler pipeline in detail.

## Overview

The compiler transforms source code through seven distinct stages, each with a specific purpose. Each stage produces an intermediate representation (IR) that is consumed by the next stage.

## Stage 1: Parsing

**Input:** Source code (string)
**Output:** AST (Abstract Syntax Tree)
**Module:** `Parser.fs`

The parser performs two tasks:

1. **Lexing**: Breaks source code into tokens (integers, operators, parentheses)
2. **Parsing**: Builds an abstract syntax tree using recursive descent

**Key Features:**
- Operator precedence: `*` and `/` bind tighter than `+` and `-`
- Left-associative operators: `1 + 2 + 3` parses as `(1 + 2) + 3`
- Parentheses for explicit grouping

**Example:**
```
Input:  "2 + 3 * 4"
Output: BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))
```

## Stage 2: ANF Transformation

**Input:** AST
**Output:** ANF (A-Normal Form)
**Module:** `ANF.fs`

ANF transformation makes evaluation order explicit by ensuring that:
- All intermediate computations are named with temporary variables
- Only simple values (variables or literals) appear as operands

**Purpose:**
- Simplifies subsequent compiler passes
- Makes control flow and evaluation order explicit
- Easier to generate efficient code

**Example:**
```
Input AST:  2 + (3 * 4)

Output ANF:
  let tmp0 = 3
  let tmp1 = 4
  let tmp2 = tmp0 * tmp1
  let tmp3 = 2
  let tmp4 = tmp3 + tmp2
  return tmp4
```

**Key Pattern:**
The `VarGen` pattern is used for generating fresh temporary variable names in a purely functional way (no mutable state).

## Stage 3: MIR (Mid-level IR)

**Input:** ANF
**Output:** MIR (Three-address code with virtual registers)
**Module:** `MIR.fs`

MIR is a platform-independent intermediate representation that uses:
- Virtual registers (unlimited supply)
- Three-address code (at most two operands per instruction)
- Simple instruction set (Mov, BinOp, Ret)

**Purpose:**
- Platform-independent optimization layer
- Prepares code for register allocation
- Simple enough for easy analysis

**Example:**
```
ANF:  let tmp0 = 2 + 3
      return tmp0

MIR:  v0 <- 2
      v1 <- 3
      v2 <- v0 + v1
      ret v2
```

**Key Pattern:**
The `RegGen` pattern generates fresh virtual register names functionally.

## Stage 4: LIR (Low-level IR)

**Input:** MIR
**Output:** LIR (ARM64-specific instructions)
**Module:** `LIR.fs`

LIR performs instruction selection, choosing appropriate ARM64 instructions for each MIR operation. It handles:
- Immediate value constraints (ARM64 ADD immediate is 12-bit)
- Instruction encoding details
- ARM64-specific addressing modes

**Example:**
```
MIR:  v0 <- 42
      v1 <- v0 + 100

LIR:  v0 <- Mov(Imm 42)
      v1 <- Add(v0, Imm 100)
```

The LIR still uses virtual registers - physical registers are assigned in the next stage.

## Stage 5: Register Allocation

**Input:** LIR with virtual registers
**Output:** LIR with physical registers
**Module:** `LIR.fs` (allocateRegisters function)

Register allocation maps virtual registers to physical ARM64 registers (X0-X15).

**Algorithm:**
Currently uses a simple greedy allocation:
1. Collect all virtual registers used in the function
2. Assign them to physical registers in order
3. Fail if we run out of registers (no spilling yet)

**Example:**
```
Before: v0 <- 42
        v1 <- v0 + 100

After:  X0 <- 42
        X1 <- X0 + 100
```

**Available Registers:**
- X0-X15: General purpose registers
- X29: Frame pointer (reserved)
- X30: Link register (reserved)
- SP: Stack pointer (reserved)

## Stage 6: Code Generation

**Input:** LIR with physical registers
**Output:** ARM64 instructions
**Module:** `CodeGen.fs`

Code generation converts LIR instructions to ARM64 assembly instructions. It handles:
- Register mapping (LIR.X0 → ARM64.X0)
- Immediate value loading (MOVZ for small values)
- Instruction selection variants (ADD_imm vs ADD_reg)

**Example:**
```
LIR:     X0 <- Mov(Imm 42)
         X1 <- Add(X0, Imm 5)

ARM64:   MOVZ X0, #42, LSL #0
         ADD X1, X0, #5
```

## Stage 7: ARM64 Encoding

**Input:** ARM64 instructions
**Output:** Machine code (32-bit words)
**Module:** `ARM64.fs`

Each ARM64 instruction is encoded to a 32-bit machine code value following the ARM64 instruction encoding specification.

**Example:**
```
MOVZ X0, #42, LSL #0  →  0xD2800540
ADD X1, X0, #5        →  0x91001401
RET                   →  0xD65F03C0
```

**Key Details:**
- Fixed 32-bit instruction width (RISC architecture)
- Register numbers encoded in 5-bit fields
- Immediate values encoded with various bit widths depending on instruction

## Stage 8: Binary Generation

**Input:** Machine code (list of 32-bit words)
**Output:** Mach-O executable file
**Module:** `Binary.fs`

The final stage packages the machine code into a valid Mach-O executable for ARM64 macOS.

**Mach-O Structure:**
1. **Header**: Identifies file as 64-bit ARM64 executable
2. **Load Commands**:
   - `__PAGEZERO`: Zero-mapped memory region (security feature)
   - `__TEXT` segment: Contains executable code
   - `LC_MAIN`: Specifies entry point
3. **Code Section**: The actual machine code
4. **Padding**: Aligned to 16KB page boundaries

**Memory Layout:**
```
Address           Content
0x0000000000000000  __PAGEZERO (4GB of unmapped memory)
0x0000000100000000  __TEXT segment header
0x0000000100004000  .text section (executable code)
```

**Entry Point:**
The program starts executing at the first instruction in the .text section. The return value of the computation becomes the process exit code.

## Data Flow Example

Let's trace the compilation of `2 + 3 * 4`:

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
            MOV X0, X4        // Move result to X0 for return
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
