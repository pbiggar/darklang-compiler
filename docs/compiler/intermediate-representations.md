# Intermediate Representations (MIR/LIR)

This document describes the mid-level and low-level intermediate representations
used in the Dark compiler pipeline.

## Overview

The compiler uses two IRs between ANF and machine code:

```
ANF → MIR → LIR → target ISA → Binary
```

- **MIR**: Platform-independent three-address code with CFG
- **LIR**: Shared low-level instructions with virtual registers and abstract
  physical register names that each backend maps to its target ISA

Before ANF, `ir/hir/HIR.fs` supplies typed value identities and structured
control flow shared by semantic leaf dialects. Primitive contracts expose
ordered inputs and operands, execution effects, and result alias provenance.
Block parameters retain both declaration order and source names, so function
signatures and lexical operand environments do not depend on map iteration.
Normalized function definitions own one entry block and derive their typed
signature from that interface rather than storing a second copy. Checked
functions can now cross this boundary with ordered parameters and explicit
lexical `let`, sequence, and conditional edges. Ordinary resolved calls also
become ordered HIR calls when their typed signature and an independent
effect/alias contract are available. Uncontracted, generic, indirect, and other
checked expressions stay as opaque scalar evaluation.
Native immediate literals and scalar unary/binary operators instead become
source-primitive leaves that retain their literal or operator identity. Their
contracts name normalized value inputs, classify unmanaged outputs as
`NoManagedAlias`, and mark integer division and modulo as potentially failing.
Managed and call-lowered operations, including string concatenation,
arbitrary-precision arithmetic, power, and structural equality, remain opaque.
`passes/hir/VerifyHIR.fs` independently checks definitions, uses, types,
branch results, and alias sources.
`ir/owned/OwnedIR.fs` supplies ownership-bearing blocks, ordered
`Evaluate`/`Dup`/`Drop` steps, borrow/consume/produce contracts, and managed
block arguments, checked independently by
`passes/ownership/VerifyOwnership.fs`. Representation-independent liveness and
destruction proofs live in `analysis/`. The list dialect uses these interfaces
for closed collection regions, storage selection, and branch-aware ownership.
Opaque operands still contain checked AST evaluation payloads, but their local
inputs are normalized identities rather than lexical-name lookups. The
whole-function constructor is not yet scheduled in code generation, and calls
without explicit contracts plus unsupported or managed source primitives remain
opaque; this is not yet a whole-program storage pipeline or a general RC solver.
See
[compiler-selected list arrays](runtime/list-array-reuse.md) for its boundary
and the remaining general ownership work.

The distinction between source-semantic IDs and these IR-local value/register
IDs is documented in [compiler identities](identities.md).

Primitive effect sets distinguish conservative opaque-source evaluation,
allocation, failure, user-code invocation, and reads or writes of
compiler-owned storage. Empty sets are effect-free.
Alias results distinguish unmanaged values, fresh managed storage, possible
reuse of one input, and conservative aliasing among named inputs. Reuse is a
capability for later storage/ownership selection, never a source-visible
mutation guarantee. Opaque AST operands retain source order and are not
silently treated as effect-free.

Function ownership signatures are separate from primitive contracts. Managed
parameters are borrowed, consumed, or uniquely consumed; managed results are
borrowed, produced, or uniquely produced. The ownership verifier starts both
consumed modes with one transferable unit, but only a unique parameter starts
with exclusivity provenance. Borrowed parameters remain accessible but
non-consumable, and every owned unit must be released or transferred as the
produced result. A borrowed result must remain within the borrowed boundary.
Unmanaged parameters and results stay outside ownership accounting. Signatures
deliberately carry no HIR alias or effect facts.

An owned function pairs a typed HIR definition with one positional ownership
signature. Group verification first checks typed value flow and the independent
primitive effect/alias contracts, then derives ownership call signatures from
the paired definitions and verifies unit transfer. Direct and recursive calls
therefore share one function boundary without allowing ownership information to
stand in for type, effect, or alias facts.

The ownership verifier tracks a nonnegative unit count and separate
exclusivity provenance per identity. A value is certified unique only when it
has provenance and exactly one local unit; an ordinary consumed parameter with
one unit is not enough because aliases may remain outside the function. `Dup`
creates another unit without discarding provenance, so uniqueness is suspended
until a balancing `Drop` restores the count to one. Scalar escape contracts
revoke provenance. Fresh primitive results, verified unique calls, and unique
function parameters can establish it. Exact states agree at joins; a managed
join result is unique only when both incoming results are unique. This is a
static Perceus-style uniqueness certificate, not a runtime RC==1 test or a
whole-program reset/reuse implementation.

Call-site facts snapshot this state before argument transfer, retaining the
exact call and established ownership contract under a caller-scoped result
identity. A unique argument must also be the only position naming its ownership
identity in that call. An ordinary owned call result may alias an input still
owned by the caller, permanently revoking that input's exclusivity; a uniquely
owned result proves independence. Joint typed-HIR and ownership analysis returns
facts only when the entire supplied function group verifies. Selection and
materialization can consume these facts, but production whole-function
scheduling is still pending.

Normalized HIR can also represent a resolved direct call with ordered value
arguments and a fresh result value. `VerifyHIR` requires an explicit typed
signature and an independently supplied primitive effect/alias contract for
every external target. For a mutually visible HIR function group, it derives
internal typed signatures from the definitions, rejects duplicate function or
parameter names and conflicting registered signatures, and uses that registry
for direct and recursive calls. Primitive effect/alias contracts remain
independent and mandatory. `VerifyOwnership` separately instantiates unmanaged,
borrowed, consumed, and uniquely consumed parameter modes plus unmanaged,
borrowed, produced, or uniquely produced result ownership. Borrowed results
name their source parameter and therefore cannot silently introduce an
ownership unit. A recursive call is accepted only
when its target is present in the verified function group and its independent
effect/alias and ownership registries; unknown and indirect calls remain opaque
evaluation rather than receiving guessed facts. Checked-source whole-function
construction and the closed-list dialect still do not admit these general calls,
so this boundary does not change emitted code or list representation selection.

## ANF shared continuations

ANF has nonrecursive lexical `Join(parameter, continuation, entry)` blocks and
`Jump(target, value)` transfers. A target is visible only in its entry; its
parameter value is visible only in its continuation. Both may capture values
from the enclosing scope. Nested continuations can transfer to enclosing
targets. Source lowering uses a typed continuation when a scalar match
expression supplies an argument, operand, or aggregate element, so the
enclosing computation is not duplicated across its return paths. ListHIR
likewise emits one continuation per scalar branch without enumerating return
paths. Join arguments support the immediate scalar types accepted by the ANF
join-interface verifier; managed values retain their ownership-aware structured
lowering.

Inlining freshens block identities along with value identities. Liveness keeps
continuation captures live; reference counting releases branch-local owners at
a jump and defers enclosing cleanup to the continuation. The post-RC interface
verifier checks lexical operands/targets, scalar argument types, and that join
entries transfer rather than return. Managed arguments exist in semantic HIR
ownership: each alternative transfers its path-local identity to one fresh
join identity. Lowering those arguments through general ANF joins and RC
insertion remains future work.

## Dumping IRs

Use the CLI to dump textual IRs while compiling:

```bash
./dark --dump-anf prog.dark
./dark --dump-mir prog.dark
./dark --dump-lir prog.dark
```

`-vvv` dumps all IRs in sequence.

Large programs should normally use a scoped dump:

```bash
./dark --dump-anf --dump-function=List.map prog.dark
./dark --dump-mir --dump-function=List.map --dump-ir-summary prog.dark
./dark --dump-lir --dump-function=List.map --dump-ir-output=/tmp/list-map.lir prog.dark
```

Function matching is case-insensitive and happens before formatting, so
unrelated functions do not consume memory or terminal context. Summary mode
prints the selected function count and, for MIR and LIR, block and instruction
counts. File output is replaced for each compiler invocation.

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
    | FuncAddr of AST.FunctionId // Function address

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

Key transformations in `ANF_to_MIR.fs`:

1. **Let bindings** → MIR instructions
2. **If expressions** → Branch + multiple blocks
3. **Function calls** → Call instruction
4. **Heap operations** → HeapAlloc/HeapStore/HeapLoad

Expression lowering distinguishes a returned value and its exit block from a
terminal control transfer. Enclosing conditionals redirect only value exits;
if both alternatives transfer elsewhere, they have no value continuation.
Function and branch bodies use the same lowering path. Merge registers carry
the enclosing function's declared return type, rather than a type inferred
from a non-returning alternative. ANF joins become shared MIR blocks; jumps
move the scalar argument into the parameter register and terminate the edge.
They never fabricate a returned value or duplicate the continuation.

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
- Abstract physical registers that map to ARM64 or x86_64 registers in pass 6

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

LIR instructions are close to machine code, but remain target-neutral enough
for both ARM64 and x86_64 code generators:

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

Key transformations in `MIR_to_LIR.fs`:

1. **Operand constraints**: operations that require registers get explicit
   register operands before code generation
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
prebuilt functions (stdlib, preamble, user code). ARM64 builds literal pools
during resolution once the full program layout is known. x64 instead
materializes float bits as immediates and allocates string literals during code
generation, so it does not emit float or string pools.

Key differences from older indexed LIR:
- `StringSymbol "hello"` and `FloatSymbol 1.5` are used directly.
- `PrintString` carries the string value directly.

## Constant Pools

Literal pools are defined in `backend/binary/LiteralPool.fs` and built during ARM64 resolution
(`backend/arm64/Resolve.fs`). The x64 backend does not use them.

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
- Physical LIR registers mapped by the selected backend
- Spill code for overflow
- Stack slots for spilled values

The physical LIR register names use an ARM64-like `X0`-`X30` abstraction.
Pass 6 maps those names to ARM64 registers directly or to x86_64 registers
according to the backend's calling convention and reserved-register rules.

## Implementation Files

| File | Purpose |
|------|---------|
| `ir/mir/MIR.fs` | MIR types |
| `ir/lir/LIR.fs` | LIR types |
| `ANF_to_MIR.fs` | ANF → MIR |
| `MIR_to_LIR.fs` | MIR → LIR |
| `SSA_Construction.fs` | SSA form |
| `MIR_Optimize.fs` | MIR optimizations |
| `RegisterAllocation.fs` | Register allocation and phi resolution |

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
