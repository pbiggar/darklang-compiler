# Intermediate Representations (MIR/LIR)

This document describes the mid-level and low-level intermediate representations
used in the Dark compiler pipeline.

## Overview

The compiler uses two IRs between ANF and machine code:

```
ANF → MIR → LIR → target ISA
```

- **MIR**: Platform-independent three-address code with CFG
- **LIR**: target-neutral instruction shapes with virtual registers

## Dumping IRs

Use the CLI to dump textual IRs while compiling:

```bash
./dark --dump-anf prog.dark
./dark --dump-mir prog.dark
./dark --dump-lir prog.dark
```

`-vvv` dumps all IRs in sequence.

## MIR (Mid-level IR)

### Purpose

MIR provides a clean three-address representation:
- Platform-independent operations
- Control flow graph structure
- Virtual registers (infinite supply)
- String/float constants stored by value (pool indices assigned later)

### Key Types

```fsharp
type VReg = VReg of int  // Virtual register

type Operand =
    | IntConst of int64
    | BoolConst of bool
    | FloatSymbol of float  // Float value (resolved to pool later)
    | StringSymbol of string // String value (resolved to pool later)
    | Register of VReg
    | FuncAddr of string   // Function address

type Instr =
    | Mov of dest:VReg * src:Operand
    | BinOp of dest:VReg * op:BinOp * left:Operand * right:Operand
    | Call of dest:VReg * funcName:string * args:Operand list
    | HeapAlloc of dest:VReg * sizeBytes:int
    | HeapStore of addr:VReg * offset:int * src:Operand
    | HeapLoad of dest:VReg * addr:VReg * offset:int
    // ... more instructions
```

### Basic Blocks

MIR organizes code into basic blocks:

```fsharp
type BasicBlock = {
    Label: Label
    Instructions: Instr list
    Terminator: Terminator
}

type Terminator =
    | Ret of Operand
    | Jump of Label
    | Branch of cond:VReg * thenLabel:Label * elseLabel:Label
```

### CFG Structure

```fsharp
type CFG = {
    Entry: Label
    Blocks: Map<Label, BasicBlock>
}
```

### ANF to MIR Transformation

Key transformations in `3_ANF_to_MIR.fs`:

1. **Let bindings** → MIR instructions
2. **If expressions** → Branch + multiple blocks
3. **Function calls** → Call instruction
4. **Heap operations** → HeapAlloc/HeapStore/HeapLoad

Example:
```
ANF: Let (t1, Add(Var t0, IntLiteral 5), Return (Var t1))
MIR: v1 <- v0 + 5
     ret v1
```

## LIR (Low-level IR)

### Purpose

LIR prepares code for architecture-specific lowering:
- Target-neutral instruction shapes consumed by both backends
- Operand constraints (registers vs immediates)
- Virtual → physical register transition

### Key Types

```fsharp
type PhysReg = X0 | X1 | ... | X30 | SP
type PhysFPReg = D0 | D1 | ... | D15

type Reg =
    | Physical of PhysReg
    | Virtual of int

type Operand =
    | Imm of int64
    | FloatImm of float
    | Reg of Reg
    | StackSlot of int
    | StringSymbol of string
    | FloatSymbol of float
    | FuncAddr of string
```

### Instructions

LIR instructions map closely to the backend instruction selectors:

```fsharp
type Instr =
    | Mov of dest:Reg * src:Operand
    | Phi of dest:Reg * sources:(Operand * Label) list * valueType:AST.Type option
    | Add of dest:Reg * left:Reg * right:Operand
    | Sub of dest:Reg * left:Reg * right:Operand
    | Mul of dest:Reg * left:Reg * right:Reg
    | Cmp of left:Reg * right:Operand
    | Cset of dest:Reg * cond:Condition
    | Call of dest:Reg * funcName:string * args:Operand list
    // Floating-point
    | FPhi of dest:FReg * sources:(FReg * Label) list
    | FAdd of dest:FReg * left:FReg * right:FReg
    | FLoad of dest:FReg * floatValue:float
    // ...
```

### MIR to LIR Transformation

Key transformations in `4_MIR_to_LIR.fs`:

1. **Operand constraints**: Backends require certain operands in registers
2. **Immediate limits**: 12-bit immediates for ADD/SUB
3. **Insert MOV**: Load large constants into registers first
4. **Float handling**: Separate FP register file

Example:
```
MIR: v2 <- v0 + 1000000
LIR: X12 <- Mov(Imm 1000000)  // Load large immediate
     v2 <- Add(v0, X12)       // Use register
```

## LIR Literal Handling

LIR stores string/float constants by value (`StringSymbol`/`FloatSymbol`)
instead of pool indices. This avoids remapping constants when merging
prebuilt functions (stdlib, preamble, user code). Pools are built during
backend emission once the full program layout is known.

Key differences from older indexed LIR:
- `StringSymbol "hello"` and `FloatSymbol 1.5` are used directly.
- `PrintString` carries the string value directly.

## Constant Pools

Literal pools are defined in `LiteralPool.fs` and built during backend
resolution (`passes/{arm64,x64}/7_Resolve.fs`).

### String Pool
```fsharp
type StringPool = {
    Strings: Map<int, string * int>  // index → (value, length)
    StringToId: Map<string, int>      // value → index
    NextId: int
}
```

### Float Pool
```fsharp
type FloatPool = {
    Floats: Map<int, float>
    FloatBitsToId: Map<int64, int>
    NextId: int
}
```

## CFG Optimizations

### SSA Construction (Pass 3.1)
Converts CFG to SSA form with phi nodes.

### Phi Resolution (Pass 5)
Register allocation resolves phi nodes by inserting parallel moves at
predecessor exits, then removes the phi nodes before code generation.

### Dead Code Elimination
Removes unused instructions and blocks.

## Virtual Register Allocation

Before register allocation, code uses virtual registers:
- Unlimited supply
- Each definition gets fresh register
- SSA form: single assignment

After register allocation (Pass 5):
- Physical registers
- Spill code for overflow
- Stack slots for spilled values

## Implementation Files

| File | Lines | Purpose |
|------|-------|---------|
| `MIR.fs` | ~200 | MIR types |
| `LIR.fs` | ~200 | LIR types |
| `3_ANF_to_MIR.fs` | 2318 | ANF → MIR |
| `4_MIR_to_LIR.fs` | 1901 | MIR → LIR |
| `3.1_SSA_Construction.fs` | 1090 | SSA form |
| `3.5_MIR_Optimize.fs` | 1401 | MIR optimizations |
| `5_RegisterAllocation.fs` | 3861 | Register allocation and phi resolution |

## Example Pipeline

Source:
```dark
let x = 5 in x + 10
```

ANF:
```
Let (t0, Atom (IntLiteral 5),
  Let (t1, Prim (Add, Var t0, IntLiteral 10),
    Return (Var t1)))
```

MIR:
```
entry:
  v0 <- 5
  v1 <- v0 + 10
  ret v1
```

LIR:
```
entry:
  v0 <- Mov(Imm 5)
  v1 <- Add(v0, Imm 10)
  ret v1
```

After register allocation:
```
entry:
  X1 <- Mov(Imm 5)
  X0 <- Add(X1, Imm 10)
  ret
```
