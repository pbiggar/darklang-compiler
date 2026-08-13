# Compiler Passes

Before expression typing, pass 1.5 builds a canonical immutable symbol
inventory and resolves value, callable, constructor, pattern, and type names.
The checked AST contains canonical identity spellings; pass 2 performs exact
identity lookup and never inserts namespaces or retries suffixes. The complete
rule table is in [Name resolution parity](name-resolution.md).

The Dark compiler transforms source code through a series of passes, each with a specific responsibility. This document explains each pass in detail.

## Pipeline Overview

| #    | Pass                    | File                                                        | Transform                                     |
|------|-------------------------|-------------------------------------------------------------|-----------------------------------------------|
| 1    | Parser                  | `passes/1_Parser.fs`                                        | Source → AST                                  |
| 1.5  | Type checking           | `passes/1.5_TypeChecking.fs`                                | AST → Typed AST                               |
| 2    | AST → ANF               | `passes/2_AST_to_ANF.fs`                                    | AST → ANF                                     |
| 2.3  | ANF optimizations       | `passes/2.3_ANF_Optimize.fs`                                | ANF → ANF                                     |
| 2.4  | ANF inlining            | `passes/2.4_ANF_Inlining.fs`                                | ANF → ANF                                     |
| 2.5  | Ref count insertion     | `passes/2.5_RefCountInsertion.fs`                           | ANF + memory ops                              |
| 2.6  | Print insertion         | `passes/2.6_PrintInsertion.fs`                              | ANF → ANF                                     |
| 2.7  | Tail call detection     | `passes/2.7_TailCallDetection.fs`                           | ANF → ANF                                     |
| 3    | ANF → MIR               | `passes/3_ANF_to_MIR.fs`                                    | ANF → CFG                                     |
| 3.1  | SSA construction        | `passes/3.1_SSA_Construction.fs`                            | MIR → SSA-form MIR                            |
| 3.5  | MIR optimizations       | `passes/3.5_MIR_Optimize.fs`                                | MIR → MIR                                     |
| 4    | MIR → LIR               | `passes/4_MIR_to_LIR.fs`                                    | MIR → LIR (virtual regs)                      |
| 4.5  | LIR peephole            | `passes/4.5_LIR_Peephole.fs`                                | LIR → LIR                                     |
| 5    | Register allocation     | `passes/5_RegisterAllocation.fs`                            | LIR (virtual) → LIR (physical)                |
| 5.5  | Function tree shaking   | `passes/5.5_FunctionTreeShaking.fs`                         | LIR → pruned LIR                              |
| 6    | Code generation         | `passes/{arm64,x64}/6_CodeGen.fs`                           | LIR → ISA instructions                        |
| 7    | Encode & resolve        | `passes/{arm64,x64}/7_Encoding.fs` + `7_Resolve.fs`         | ISA → machine code bytes                      |
| 8    | Binary generation       | `passes/{arm64,x64}/8_Binary_Generation_*.fs`               | Bytes → Mach-O or ELF executable              |

Passes 1–5 are shared across targets. Passes 6–8 live under
`passes/arm64/` or `passes/x64/`. The host is validated once as a
`Platform.Target` before stdlib construction, and `CompilerLibrary.generateBinary`
selects the backend from that explicit target.

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
  - Unresolved type variables are preserved; if hashing/equality intrinsics are needed, lowering emits an explicit runtime error expression instead of a fallback intrinsic name.
  - Optimizations like `Dict.fromList([])` → `Dict.empty` only apply when type arguments are concrete.

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

## Pass 2.3: ANF Optimizations (`2.3_ANF_Optimize.fs`)

**Input**: ANF
**Output**: Optimized ANF

### Responsibilities
- **Constant folding**: Fold literals and algebraic identities
- **Constant propagation**: Substitute known literals
- **Copy propagation**: Remove trivial `let` bindings
- **Dead code elimination**: Drop unused bindings without side effects
- **Strength reduction**: `mul/div/mod` by powers of 2 → shifts/bitwise ops

### Sub-passes (grouped)
- `const_folding`, `const_prop`, `copy_prop`, `dce`, `strength_reduction`

---

## Pass 2.4: ANF Inlining (`2.4_ANF_Inlining.fs`)

**Input**: ANF
**Output**: ANF with selected calls inlined

### Responsibilities
- **Inline small functions**: Reduce call overhead when safe
- **Preserve semantics**: Respect evaluation order and side effects

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

## Pass 2.6: Print Insertion (`2.6_PrintInsertion.fs`)

**Input**: ANF
**Output**: ANF with explicit print operations

### Responsibilities
- **Ensure observable output**: Insert print calls for program results
- **Preserve types**: Use type information to select correct printers

---

## Pass 2.7: Tail Call Optimization (`2.7_TailCallDetection.fs`)

**Input**: ANF with refcounting
**Output**: ANF annotated for tail calls / self-recursion loops

### Responsibilities
- **Detect tail positions**: Identify safe tail calls
- **Self-recursion loop conversion**: Turn tail-recursive calls into jumps

---

## Pass 3: ANF to MIR (`3_ANF_to_MIR.fs`)

**Input**: ANF
**Output**: Mid-level IR as Control Flow Graph (CFG)

### Responsibilities
- **Build CFG**: Convert structured control flow to basic blocks
- **Handle branches**: If/else becomes conditional jumps
- **Literal lowering**: Keep string/float constants as symbolic values

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

## Pass 3.1: SSA Construction (`3.1_SSA_Construction.fs`)

**Input**: MIR CFG
**Output**: MIR CFG in SSA form

### Responsibilities
- **SSA form**: Insert phi nodes and rename variables
- **Dominance tracking**: Build dominators for SSA placement

---

## Pass 3.5: MIR Optimizations (`3.5_MIR_Optimize.fs`)

**Input**: MIR CFG in SSA
**Output**: Optimized MIR CFG

### Responsibilities
- **Constant folding**: Fold literal computations
- **CSE**: Eliminate duplicate pure expressions
- **Copy propagation**: Simplify moves and trivial phis
- **DCE**: Remove unused instructions
- **CFG simplification**: Remove empty blocks / redirect edges
- **LICM**: Hoist loop-invariant expressions

### Sub-passes (grouped)
- `const_folding`, `cse`, `copy_prop`, `dce`, `cfg_simplify`, `licm`

---

## Pass 4: MIR to LIR (`4_MIR_to_LIR.fs`)

**Input**: MIR (target-independent)
**Output**: LIR (virtual registers, target-neutral instruction shapes)

### Responsibilities
- **Instruction selection**: Lower MIR operations into LIR primitives
  that both backends can consume.
- **Calling convention**: Set up function calls via `Platform.Arch`-aware
  argument placement.
- **Symbolic constants**: Keep string/float constants by value until late
  pool resolution.

### Key Algorithms
- **Pattern matching**: Each MIR operation maps to an LIR sequence.
- **Immediate splitting**: Large constants may need multiple instructions.

### Example Transformation
```
Input (MIR):  Add(v1, v2, v3)      // v1 = v2 + v3
Output (LIR): Add(V1, V2, V3)      // three-operand LIR add
```

Implementation detail: LIR keeps string/float constants by value and defers
pool construction until ISA emission. This avoids per-function pool remapping
when mixing stdlib, preamble, and user functions.

---

## Pass 4.5: LIR Peephole (`4.5_LIR_Peephole.fs`)

**Input**: LIR (virtual regs)
**Output**: Optimized LIR (virtual regs)

### Responsibilities
- **Peephole rewrites**: Local instruction simplifications
- **Branch fusion**: Combine compare/set/branch sequences when safe

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

### Register Classes (LIR-level abstraction)

The LIR uses abstract `PhysReg` identifiers X0-X30; each backend maps
them to actual hardware registers in `passes/{arch}/6_CodeGen.fs`.

- **Caller-saved (preferred)**: X1-X7
- **Callee-saved**: X19-X26 on ARM64, X19-X21 on x86_64 (fewer because
  X22/X23 are reserved for the heap pointer and free list on x86_64).
- **Reserved**: X0 (return), X8-X10 (scratch), X27-X28 (runtime state),
  X29-X30 (ABI).

On x86_64 the LIR PhysRegs X8-X17 all collapse onto R11 (shared scratch);
the allocator is aware of this via `isX86_64 arch` checks.

---

## Pass 5.5: Function Tree Shaking (`5.5_FunctionTreeShaking.fs`)

**Input**: LIR (physical regs)
**Output**: LIR with only reachable functions

### Responsibilities
- **Prune unused functions**: Keep `_start` roots and reachable callees
- **Stdlib filtering**: Include only stdlib functions called by user code
- **Call graph helpers**: Uses `DeadCodeElimination.fs` for LIR reachability and
  `ANFDeadCodeElimination.fs` when computing reachable stdlib names from ANF

---

## Pass 6: Code Generation (`passes/{arm64,x64}/6_CodeGen.fs`)

**Input**: LIR with physical registers
**Output**: Target-specific symbolic instruction list

### Responsibilities
- **Final instruction selection**: Convert LIR to the target ISA
  (ARM64Symbolic for arm64, `X86_64.Instr` for x64).
- **Prologue/epilogue**: Function entry/exit code.
- **Stack frame setup**: Allocate space for spills and locals.
- **Two-operand conflict handling (x64 only)**: Swap operands or use
  XMM15/R11 temps when dest == right for commutative/non-commutative ops.

---

## Passes 7 and 8: Encode and Emit

**Input**: Target-specific symbolic instruction list
**Output**: Executable file (Mach-O or ELF)

### Responsibilities
- **Literal pool resolution** (arm64): resolve symbolic data labels into
  literal pools. x64 uses RIP-relative addressing instead, and has no
  literal pools.
- **Label resolution**: fix up branch offsets to real byte distances.
- **Instruction encoding**: convert symbolic instructions to bytes per
  the ISA spec (fixed 32-bit on arm64, variable 1–15 bytes on x64).
- **Binary generation**: emit Mach-O (macOS arm64) or ELF (Linux arm64
  and x64).

### Per-backend files

| Backend | Encoding                               | Resolve                               | Binary                                                             |
|---------|----------------------------------------|---------------------------------------|--------------------------------------------------------------------|
| arm64   | `passes/arm64/7_Encoding.fs`           | `passes/arm64/7_Resolve.fs`           | `passes/arm64/8_Binary_Generation_{MachO,ELF}.fs` via `7_Emit.fs` |
| x64     | `passes/x64/7_Encoding.fs`             | `passes/x64/7_Resolve.fs`             | `passes/x64/8_Binary_Generation_ELF.fs`                            |

---

## Data Structure Files

| File               | Purpose                               |
|--------------------|---------------------------------------|
| `AST.fs`           | Abstract Syntax Tree types            |
| `ANF.fs`           | A-Normal Form types                   |
| `MIR.fs`           | Mid-level IR types                    |
| `LIR.fs`           | Low-level IR types                    |
| `Platform.fs`      | OS/Arch DUs and per-target syscall tables |
| `ARM64.fs`         | ARM64 instruction and register types  |
| `ARM64Symbolic.fs` | Symbolic ARM64 instructions (pre-encoding) |
| `X86_64.fs`        | x86_64 instruction and register types |
| `Binary_ELF.fs`    | Shared ELF header/segment types       |

---

## Testing Each Pass

Each pass can be tested in isolation:

- **Parser**: Test with source strings, check AST structure
- **Type Checker**: Test type errors are caught
- **ANF**: PassTestRunner validates ANF output
- **End-to-end**: `.e2e` files test full pipeline

Run all tests: `./run-tests --ai`
