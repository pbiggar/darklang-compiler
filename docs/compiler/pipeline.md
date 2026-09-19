# Compiler Passes

See the [recursion compatibility ledger](../compatibility/language/recursion.md) for the recursive identity and
group invariants carried through parsing, typing, ANF lowering, and tail-call
detection.

Before expression typing, name resolution builds a canonical immutable symbol
inventory and resolves value, callable, constructor, pattern, and type names.
The checked AST contains canonical identity spellings; ANF lowering performs exact
identity lookup and never inserts namespaces or retries suffixes. The complete
rule table is in [Name resolution parity](../compatibility/language/name-resolution.md).

The Dark compiler transforms source code through a series of passes, each with a specific responsibility. This document explains each pass in detail.

## Pipeline Overview

| #    | Pass                    | File                                                        | Transform                                     |
|------|-------------------------|-------------------------------------------------------------|-----------------------------------------------|
| 1    | Parser                  | `frontend/Parser.fs`                             | Source → parsed AST                           |
| 1.5  | Type checking           | `frontend/TypeChecking.fs`                       | Parsed AST → checked AST                      |
| 1.9  | Function ownership analysis | `passes/ownership/AnalyzeFunctionOwnership.fs` | Checked AST → verified owned HIR (analysis artifact) |
| 2    | AST → ANF               | `passes/anf/AST_to_ANF.fs`                       | Checked AST → ANF                             |
| 2 (regions) | List representation and ownership | `passes/hir/`, `passes/storage/`, `passes/ownership/`, `passes/anf/LowerListRegions.fs` | Closed semantic lists → storage → owned arrays → ANF |
| 2.2  | Generated result output | `passes/anf/PrintInsertion.fs`                   | ANF → ANF                                     |
| 2.25 | Function reachability   | `passes/anf/ANFDeadCodeElimination.fs`           | ANF → reachable ANF                           |
| 2.3  | ANF optimizations       | `passes/anf/ANF_Optimize.fs`                                | ANF → ANF                                     |
| 2.4  | ANF inlining            | `passes/anf/ANF_Inlining.fs`                                | ANF → ANF                                     |
| 2.4.4 | Known closure specialization | `passes/anf/ANF_HigherOrderSpecialization.fs`       | ANF → ANF                                     |
| 2.4.5 | Direct-call specialization | `passes/anf/ANF_DirectCallSpecialization.fs`          | ANF → ANF                                     |
| 2.4.6 | Escape analysis        | `passes/anf/ANF_EscapeAnalysis.fs`                        | ANF → scalar-replaced ANF                     |
| 2.5  | Ref count insertion     | `passes/anf/RefCountInsertion.fs`                           | ANF + memory ops                              |
| 2.7  | Tail call detection     | `passes/anf/TailCallDetection.fs`                           | ANF → ANF                                     |
| 3    | ANF → MIR               | `passes/anf/ANF_to_MIR.fs`                                    | ANF → CFG                                     |
| 3.1  | SSA construction        | `passes/mir/SSA_Construction.fs`                            | MIR → SSA-form MIR                            |
| 3.5  | MIR optimizations       | `passes/mir/MIR_Optimize.fs`                                | MIR → MIR                                     |
| 4    | MIR → LIR               | `passes/mir/MIR_to_LIR.fs`                                    | MIR → LIR (virtual regs)                      |
| 4.5  | LIR peephole            | `passes/lir/LIR_Peephole.fs`                                | LIR → LIR                                     |
| 5    | Register allocation     | `passes/lir/RegisterAllocation.fs`                            | LIR (virtual) → LIR (physical)                |
| 5.5  | Function tree shaking   | `passes/lir/FunctionTreeShaking.fs`                         | LIR → pruned LIR                              |
| 6    | Code generation         | `backend/{arm64,x64}/CodeGen.fs`                           | LIR → ISA instructions                        |
| 7    | Encode & resolve        | `backend/{arm64,x64}/Encoding.fs` + `Resolve.fs`         | ISA → machine code bytes                      |
| 8    | Binary generation       | `backend/{arm64,x64}/Binary_Generation_*.fs`                 | Blob → Mach-O or ELF executable              |

Passes 1–5 are shared across targets. Passes 6–8 live under
`backend/arm64/` or `backend/x64/`. The host is validated once as a
`Platform.Target` before stdlib construction, and `BinaryOutput.generateBinary`
selects the backend from that explicit target.

---

## Pass 1: Parser (`Parser.fs`)

**Input**: Source code string
**Output**: Abstract Syntax Tree (AST)

### Responsibilities
- **Lexical analysis**: Convert character stream to tokens
- **Syntactic analysis**: Build AST using recursive descent parsing
- **Operator precedence**: Handle binary operators with Pratt parsing
- **Control-flow syntax**: Represent `elif` as nested conditionals and `;` statement blocks as explicit `Sequence` nodes

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

## Pass 1.5: Type Checking (`TypeChecking.fs`)

**Input**: Parsed AST
**Output**: Checked AST with phase invariants represented by node shape

### Responsibilities
- **Type validation**: Ensure expressions have consistent types
- **Error reporting**: Clear messages with source locations
- **Free variable collection**: For closure analysis
- **Nominal declaration validation**: Predeclare recursive ADT identities and reject duplicate/empty declarations, invalid generic parameters, and duplicate cases
- **Constructor resolution**: Resolve unqualified or type-qualified constructor references to a declaring module/type identity and reject ambiguity

### Key Algorithms
- **Top-down checking**: Push expected types down, validate bottom-up
- **Result-based errors**: No exceptions, explicit error propagation
- **Environment threading**: Track variable types through expressions
- **Control-flow checking**: Require Boolean conditions, unify conditional arms, and use a sequence's Unit head and final-result type
- **Phase boundary construction**: Require inferred lambda parameter types,
  typed recursive identities, resolved nominal references, and checked value
  bodies before producing `CheckedAST.Program`

### Example Error
```
Input:  1 + "hello"
Error:  Type mismatch: expected Int64, got String in binary operator
```

---

## Pass 2: AST to ANF (`AST_to_ANF.fs`)

**Input**: Checked AST
**Output**: A-Normal Form (ANF)

### Responsibilities
- **Schedule whole-function ownership analysis**: Construct normalized HIR,
  infer borrow/consume boundaries across internal and recursive calls, place
  explicit duplication and cleanup, and jointly verify typed HIR and ownership
  before ordinary lowering. The verified artifact is not yet consumed by ANF.
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
- **ADT construction**: Evaluate constructor fields once in source order, then emit one canonical-tag construction operation

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

## Pass 2.2: Generated Result Output (`PrintInsertion.fs`)

**Input**: ANF
**Output**: ANF with explicit result-printing effects

### Responsibilities
- **Ensure observable output**: Insert printing for expression-mode program results
- **Expose ownership**: Make the consuming output use visible before reference-count insertion
- **Preserve cleanup order**: Finalize unrelated ownership before the output effect consumes its root

Reference-count insertion recognizes the terminal consuming print boundary. It
finalizes every unrelated ownership obligation before the output effect, while
the rendered root remains live through printing. MIR-to-LIR lowers that
consumption to the shape-specific final release using the existing release
registries.

---

## Pass 2.25: Function Reachability (`ANFDeadCodeElimination.fs`)

Expression compilation removes functions that cannot be reached from the
generated program entry before ANF optimization, then repeats the query after
inlining and specialization. Function references and closure allocations are
call-graph edges, so indirect calls retain every statically named target. The
later LIR tree-shaking pass remains responsible for combining fresh and
prebuilt functions and for selecting reachable standard-library functions.

---

## Pass 2.3: ANF Optimizations (`ANF_Optimize.fs`)

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

## Pass 2.4: ANF Inlining (`ANF_Inlining.fs`)

**Input**: ANF
**Output**: ANF with selected calls inlined

### Responsibilities
- **Inline small functions**: Reduce call overhead when safe
- **Preserve semantics**: Respect evaluation order and side effects

---

## Pass 2.4.4: Known Closure Specialization (`ANF_HigherOrderSpecialization.fs`)

**Input**: Inlined ANF
**Output**: ANF with selected higher-order helpers specialized

### Responsibilities

- **Propagate callable facts**: Track known closures and static function references through aliases, branch values and joins, and function returns
- **Specialize complete call shapes**: Clone a helper once for all eligible known functional arguments while retaining the generic closure path for unknown values
- **Pass captures directly**: Replace closure arguments with ordinary capture parameters and lower `ClosureCall` to direct calls; static references need no target clone or heap closure
- **Cross compilation units**: Use pre-reference-count external ANF templates to create local helper and target clones without changing prebuilt functions
- **Bound transformation size**: Skip large helpers/targets and charge every specialized functional argument against the sixteen-pair program budget

---

## Pass 2.4.6: Escape Analysis (`ANF_EscapeAnalysis.fs`)

**Input**: Specialized ANF
**Output**: ANF with eligible local aggregates scalar-replaced or uniquely reused

The first escape-analysis scope covers fixed-layout tuple and record
allocations whose fields are all immediate scalar values, including Float64. An
allocation is removed only when its complete lexical use set consists of field
projections, local aliases, and representation-only record-clone sources.
Returns, calls, closure capture, storage, raw-pointer operations, managed
fields, and every unmodelled use preserve the heap representation. Escaping
clones retain their own allocation while eligible source and intermediate
aggregates are scalar-replaced. A remaining uniquely owned Float record can
still transfer its allocation to a sole escaping clone after scalar replacement.

Running before reference-count insertion ensures eliminated aggregates never
acquire root retain or release operations and allows reused records to be
treated as one ownership family. Stack allocation, managed-field reuse or
scalar replacement, and interprocedural representation changes are outside the
current scope.

## Pass 2.5: Reference Count Insertion (`RefCountInsertion.fs`)

**Input**: ANF
**Output**: ANF with RefCountInc/RefCountDec operations

### Responsibilities
- **Memory management**: Insert reference counting operations
- **Ownership tracking**: Determine when values need inc/dec

### Key Algorithms
- **Borrowed calling convention**: Callers retain ownership, no inc on call
- **Scope-based release**: Dec when value goes out of scope

---

## Pass 2.7: Tail Call Optimization (`TailCallDetection.fs`)

**Input**: ANF with refcounting
**Output**: ANF annotated for tail calls / self-recursion loops

### Responsibilities
- **Detect tail positions**: Identify safe tail calls
- **Self-recursion loop conversion**: Turn tail-recursive calls into jumps

---

## Pass 3: ANF to MIR (`ANF_to_MIR.fs`)

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

## Pass 3.1: SSA Construction (`SSA_Construction.fs`)

**Input**: MIR CFG
**Output**: MIR CFG in SSA form

### Responsibilities
- **SSA form**: Insert phi nodes and rename variables
- **Dominance tracking**: Build dominators for SSA placement

---

## Pass 3.5: MIR Optimizations (`MIR_Optimize.fs`)

**Input**: MIR CFG in SSA
**Output**: Optimized MIR CFG

### Responsibilities
- **Constant folding**: Fold literal computations
- **Sparse conditional constant propagation**: Jointly solve explicitly typed
  integer and Boolean SSA values with executable CFG edges, then prune
  unreachable blocks and phi inputs
- **CSE**: Eliminate duplicate pure expressions
- **Copy propagation**: Simplify moves and trivial phis
- **DCE**: Remove unused instructions
- **CFG simplification**: Remove empty blocks / redirect edges
- **LICM**: Hoist loop-invariant expressions

### Sub-passes (grouped)
- `sccp`, `const_folding`, `cse`, `copy_prop`, `dce`, `cfg_simplify`, `licm`

---

## Pass 4: MIR to LIR (`MIR_to_LIR.fs`)

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

## Pass 4.5: LIR Peephole (`LIR_Peephole.fs`)

**Input**: LIR (virtual regs)
**Output**: Optimized LIR (virtual regs)

### Responsibilities
- **Peephole rewrites**: Local instruction simplifications
- **Branch fusion**: Combine compare/set/branch sequences when safe

---

## Pass 5: Register Allocation (`RegisterAllocation.fs`)

**Input**: LIR with virtual registers
**Output**: LIR with physical registers

### Responsibilities
- **Liveness analysis**: Determine when each virtual register is live
- **Register assignment**: Map virtual to physical registers
- **Spill handling**: Use stack when registers exhausted

### Key Algorithms
- **Backward dataflow**: Compute live ranges from uses to definitions
- **Chordal coloring**: Optimal coloring of the SSA interference graph
- **Spill code generation**: Load/store for spilled values
- **Float pressure control**: Literal-load scheduling, rematerialization, and
  shared spill-slot assignment

### Register Classes (LIR-level abstraction)

The LIR uses abstract `PhysReg` identifiers X0-X30; each backend maps
them to actual hardware registers in `backend/{arch}/CodeGen.fs`.

- **Caller-saved (preferred)**: X1-X7
- **Callee-saved**: X19-X26 on ARM64, X19-X21 on x86_64 (fewer because
  X22/X23 are reserved for the heap pointer and free list on x86_64).
- **Reserved**: X0 (return), X8-X10 (scratch), X27-X28 (runtime state),
  X29-X30 (ABI).

On x86_64 the LIR PhysRegs X8-X17 all collapse onto R11 (shared scratch);
the allocator is aware of this via `isX86_64 arch` checks.

---

## Pass 5.5: Function Tree Shaking (`FunctionTreeShaking.fs`)

**Input**: LIR (physical regs)
**Output**: LIR with only reachable functions

### Responsibilities
- **Prune unused functions**: Keep `_start` roots and reachable callees
- **Stdlib filtering**: Include only stdlib functions called by user code
- **Call graph helpers**: Uses `DeadCodeElimination.fs` for LIR reachability and
  `ANFDeadCodeElimination.fs` when computing reachable stdlib names from ANF

---

## Pass 6: Code Generation (`backend/{arm64,x64}/CodeGen.fs`)

**Input**: LIR with physical registers
**Output**: Target-specific symbolic instruction list

### Responsibilities
- **Final instruction selection**: Convert LIR to the target ISA
  (ARM64Symbolic for arm64, `X86_64.Instr` for x64).
- **Prologue/epilogue**: Function entry/exit code.
- **Stack frame setup**: Allocate space for spills and locals.
- **CLI native effects**: Lower process, host, signal, and terminal operations;
  construct language-managed results with ordinary ownership.
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
| arm64   | `backend/arm64/Encoding.fs`           | `backend/arm64/Resolve.fs`           | `backend/arm64/Binary_Generation_{MachO,ELF}.fs` via `Emit.fs` |
| x64     | `backend/x64/Encoding.fs`             | `backend/x64/Resolve.fs`             | `backend/x64/Binary_Generation_ELF.fs`                            |

---

## Data Structure Files

| File               | Purpose                               |
|--------------------|---------------------------------------|
| `AST.fs`           | Abstract Syntax Tree types            |
| `ir/anf/ANF.fs`           | A-Normal Form types                   |
| `ir/mir/MIR.fs`           | Mid-level IR types                    |
| `ir/lir/LIR.fs`           | Low-level IR types                    |
| `Platform.fs`      | OS/Arch DUs and per-target syscall tables |
| `backend/arm64/ISA.fs`         | ARM64 instruction and register types  |
| `backend/arm64/Symbolic.fs` | Symbolic ARM64 instructions (pre-encoding) |
| `backend/x64/ISA.fs`        | x86_64 instruction and register types |
| `backend/binary/ELF.fs`    | Shared ELF header/segment types       |

---

## Testing Each Pass

Each pass can be tested in isolation:

- **Parser**: Test with source strings, check AST structure
- **Type Checker**: Test type errors are caught
- **ANF**: PassTestRunner validates ANF output
- **End-to-end**: `.e2e` files test full pipeline

Build and run all tests: `./build --ai && ./run-tests --ai`
