# Compiler Passes

The Dark compiler transforms source code through a series of passes, each with a specific responsibility. This document explains each pass in detail.

## Pipeline Overview

```
Source Code
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 1: Parser (1_Parser.fs)        │
│ Source → AST                        │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 1.5: Type Checking             │
│ (1.5_TypeChecking.fs)               │
│ AST → Typed AST                     │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2: AST to ANF (2_AST_to_ANF.fs)│
│ AST → A-Normal Form                 │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 2.5: Ref Count Insertion       │
│ (2.5_RefCountInsertion.fs)          │
│ ANF → ANF with memory ops           │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 3: ANF to MIR (3_ANF_to_MIR.fs)│
│ ANF → Control Flow Graph            │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 4: MIR to LIR (4_MIR_to_LIR.fs)│
│ MIR → Low-level IR (virtual regs)   │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 5: Register Allocation         │
│ (5_RegisterAllocation.fs)           │
│ LIR (virtual) → LIR (physical)      │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 6: Code Generation             │
│ (6_CodeGen.fs)                      │
│ LIR → ARM64 Instructions            │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 7: ARM64 Encoding              │
│ (7_ARM64_Encoding.fs)               │
│ ARM64 → Machine Code Bytes          │
└─────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────┐
│ Pass 8: Binary Generation           │
│ (8_Binary_Generation_*.fs)          │
│ Machine Code → Executable           │
└─────────────────────────────────────┘
    │
    ▼
Executable Binary
```

---

## Pass 1: Parser (`1_Parser.fs`)

**Input**: Source code string
**Output**: Abstract Syntax Tree (AST)

### Responsibilities
- **Lexical analysis**: Convert character stream to tokens
- **Syntactic analysis**: Build AST using recursive descent parsing
- **Operator precedence**: Handle binary operators with Pratt parsing

### Key Algorithms
- **Recursive descent**: Each grammar production is a function
- **Pratt precedence parsing**: Handles operator precedence elegantly
- **Escape sequence processing**: Handle `\n`, `\t`, `\"`, etc. in strings

### Example Transformation
```
Input:  "let x = 1 + 2 in x * 3"
Output: Let("x", BinOp(Add, IntLiteral(1), IntLiteral(2)),
            BinOp(Mul, Var("x"), IntLiteral(3)))
```

---

## Pass 1.5: Type Checking (`1.5_TypeChecking.fs`)

**Input**: AST
**Output**: Type-checked AST (same structure, validated)

### Responsibilities
- **Type validation**: Ensure expressions have consistent types
- **Error reporting**: Clear messages with source locations
- **Free variable collection**: For closure analysis

### Key Algorithms
- **Top-down checking**: Push expected types down, validate bottom-up
- **Result-based errors**: No exceptions, explicit error propagation
- **Environment threading**: Track variable types through expressions

### Example Error
```
Input:  1 + "hello"
Error:  Type mismatch: expected Int64, got String in binary operator
```

---

## Pass 2: AST to ANF (`2_AST_to_ANF.fs`)

**Input**: AST
**Output**: A-Normal Form (ANF)

### Responsibilities
- **Flatten nested expressions**: All intermediate results get names
- **Make evaluation order explicit**: Left-to-right evaluation visible
- **Handle desugaring**: Convert high-level constructs to primitives
- **Monomorphization**: Generate specialized versions of generic functions
- **Lambda lifting**: Convert lambdas to top-level functions with closures

### Key Algorithms
- **Fresh variable generation**: VarGen creates unique temporaries
- **Let-binding normalization**: Every complex subexpression bound to temp

### Example Transformation
```
Input:  1 + 2 * 3
Output: let t0 = 2 * 3 in
        let t1 = 1 + t0 in
        return t1
```

### Why ANF?
- Makes evaluation order explicit (important for side effects)
- Simplifies code generation (no nested expressions to evaluate)
- Enables optimizations (common subexpression elimination)

---

## Pass 2.5: Reference Count Insertion (`2.5_RefCountInsertion.fs`)

**Input**: ANF
**Output**: ANF with RefCountInc/RefCountDec operations

### Responsibilities
- **Memory management**: Insert reference counting operations
- **Ownership tracking**: Determine when values need inc/dec

### Key Algorithms
- **Borrowed calling convention**: Callers retain ownership, no inc on call
- **Scope-based release**: Dec when value goes out of scope

---

## Pass 3: ANF to MIR (`3_ANF_to_MIR.fs`)

**Input**: ANF
**Output**: Mid-level IR as Control Flow Graph (CFG)

### Responsibilities
- **Build CFG**: Convert structured control flow to basic blocks
- **Handle branches**: If/else becomes conditional jumps
- **String/float pooling**: Collect constants into data sections

### Key Concepts
- **Basic block**: Sequence of instructions with single entry/exit
- **CFG**: Graph of basic blocks connected by jumps
- **Virtual registers**: Unlimited registers, allocation comes later

### Example Transformation
```
Input:  if x > 0 then 1 else 2

Output: block0:
          cmp x, 0
          ble block2
        block1:
          mov result, 1
          jmp block3
        block2:
          mov result, 2
        block3:
          return result
```

---

## Pass 4: MIR to LIR (`4_MIR_to_LIR.fs`)

**Input**: MIR (platform-independent)
**Output**: LIR (ARM64-specific, virtual registers)

### Responsibilities
- **Instruction selection**: Choose ARM64 instructions for MIR operations
- **Address constraints**: Handle ARM64 immediate value limits
- **Calling convention**: Set up function calls per ARM64 ABI

### Key Algorithms
- **Pattern matching**: Each MIR operation maps to ARM64 sequence
- **Immediate splitting**: Large constants may need multiple instructions

### Example Transformation
```
Input (MIR):  Add(v1, v2, v3)      // v1 = v2 + v3
Output (LIR): ADD(V1, V2, V3)      // ARM64 ADD instruction
```

---

## Pass 5: Register Allocation (`5_RegisterAllocation.fs`)

**Input**: LIR with virtual registers
**Output**: LIR with physical registers

### Responsibilities
- **Liveness analysis**: Determine when each virtual register is live
- **Register assignment**: Map virtual to physical registers
- **Spill handling**: Use stack when registers exhausted

### Key Algorithms
- **Backward dataflow**: Compute live ranges from uses to definitions
- **Linear scan**: Efficient allocation using sorted live intervals
- **Spill code generation**: Load/store for spilled values

### Register Classes
- **Caller-saved (preferred)**: X1-X10, X14-X15
- **Callee-saved**: X19-X26 (used under high pressure)
- **Reserved**: X0 (return), X11-X13 (spill temps), X27-X28 (memory), X29-X30 (ABI)

---

## Pass 6: Code Generation (`6_CodeGen.fs`)

**Input**: LIR with physical registers
**Output**: ARM64 instruction list

### Responsibilities
- **Final instruction generation**: Convert LIR to ARM64 types
- **Prologue/epilogue**: Function entry/exit code
- **Stack frame setup**: Allocate space for spills and locals

### Key Outputs
- ARM64 instruction sequence per function
- Entry point setup (main function handling)

---

## Pass 7: ARM64 Encoding (`7_ARM64_Encoding.fs`)

**Input**: ARM64 instruction list
**Output**: Machine code bytes

### Responsibilities
- **Instruction encoding**: Convert to binary per ARM64 spec
- **Branch offset computation**: Calculate relative jumps
- **Two-pass encoding**: First pass measures, second encodes

### Why Two Passes?
Forward branches need to know the target address, which depends on instruction sizes. First pass calculates sizes, second pass encodes with correct offsets.

---

## Pass 8: Binary Generation (`8_Binary_Generation_*.fs`)

**Input**: Machine code bytes + data sections
**Output**: Executable file (Mach-O or ELF)

### Responsibilities
- **Format headers**: Mach-O (macOS) or ELF (Linux) headers
- **Section layout**: Code, data, string constants
- **Entry point**: Configure OS to start at main

### Platform Detection
Automatically generates correct format based on runtime platform.

---

## Data Structure Files

| File | Purpose |
|------|---------|
| `AST.fs` | Abstract Syntax Tree types |
| `ANF.fs` | A-Normal Form types |
| `MIR.fs` | Mid-level IR types |
| `LIR.fs` | Low-level IR types |
| `ARM64.fs` | ARM64 instruction types |

---

## Testing Each Pass

Each pass can be tested in isolation:

- **Parser**: Test with source strings, check AST structure
- **Type Checker**: Test type errors are caught
- **ANF**: PassTestRunner validates ANF output
- **End-to-end**: `.e2e` files test full pipeline

Run all tests: `dotnet test`
