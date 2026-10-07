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
| 1    | Parser and validation   | `src/frontend/interpreter/Parser.ml`, `src/frontend/WrittenParsing.ml` | Source → validated `WrittenTypes` |
| 1.5  | Type checking           | `src/frontend/WrittenChecking.ml` | `WrittenTypes` → checked AST |
| 1.9  | Function ownership analysis | `src/passes/ownership/AnalyzeFunctionOwnership.ml` | Checked AST → verified owned HIR (analysis artifact) |
| 2    | AST → ANF               | `src/passes/anf/AST_to_ANF.ml`                       | Checked AST → ANF                             |
| 2 (regions) | List representation and ownership | `passes/hir/`, `passes/storage/`, `passes/ownership/`, `src/passes/anf/LowerListRegions.ml` | Closed semantic lists → storage → owned arrays → ANF |
| 2.2  | Generated result output | `src/passes/anf/PrintInsertion.ml`                   | ANF → ANF                                     |
| 2.25 | Function reachability   | `src/passes/anf/ANFDeadCodeElimination.ml`           | ANF → reachable ANF                           |
| 2.3  | Accumulator helper lowering | `src/passes/anf/ANFAccumulatorLowering.ml`                  | ANF → ANF with recursive helpers              |
| 2.4  | ANF → high-level SSA    | `src/ir/anf/SSAANF.ml`                                           | ANF → typed blocks                             |
| 2.4.1 | SSA optimizations     | `src/passes/anf/SSAOptimization.ml`                              | SSA → simplified SSA                           |
| 2.4.5 | SSA inlining           | `src/passes/anf/SSAInlining.ml`                                 | SSA → SSA                                     |
| 2.4.6 | Known closure specialization | `src/passes/anf/SSAHigherOrderSpecialization.ml`   | SSA → specialized SSA                         |
| 2.5  | Direct-call specialization | `src/passes/anf/SSADirectCallSpecialization.ml`             | SSA → specialized SSA                         |
| 2.6  | Escape analysis        | `src/passes/anf/SSAEscapeAnalysis.ml`                           | SSA → scalar-replaced SSA                     |
| 2.7  | Ref count insertion     | `src/passes/anf/ownership/RcSSARefCountInsertion.ml`              | SSA + memory ops                              |
| 2.8  | Tail call detection     | `src/passes/anf/SSATailCallDetection.ml`                        | SSA → SSA                                     |
| 3.1  | SSA → MIR               | `src/passes/anf/ANF_to_MIR.ml`                                   | Typed blocks → SSA-form MIR                   |
| 3.5  | MIR optimizations       | `src/passes/mir/MIR_Optimize.ml`                                | MIR → MIR                                     |
| 4    | MIR → LIR               | `src/passes/mir/MIR_to_LIR.ml`                                    | MIR → LIR (virtual regs)                      |
| 4.5  | LIR peephole            | `src/passes/lir/LIR_Peephole.ml`                                | LIR → LIR                                     |
| 5    | Register allocation     | `src/passes/lir/RegisterAllocation.ml`                            | LIR (virtual) → LIR (physical)                |
| 5.5  | Function tree shaking   | `src/passes/lir/FunctionTreeShaking.ml`                         | LIR → pruned LIR                              |
| 6    | Code generation         | `src/backend/arm64/Backend_Arm64_CodeGen.ml` and `src/backend/x64/CodeGen_X86_64.ml`                           | LIR → ISA instructions                        |
| 7    | Encode & resolve        | `src/backend/arm64/ARM64_Encoding.ml` and `src/backend/x64/X86_64_Encoding.ml` + `src/backend/arm64/ARM64_Resolve.ml` and `src/backend/x64/X86_64_Resolve.ml`         | ISA → machine code bytes                      |
| 8    | Binary generation       | `backend/{arm64,x64}/Binary_Generation_*.fs`                 | Blob → Mach-O or ELF executable              |

Passes 1–5 are shared across targets. Passes 6–8 live under
`backend/arm64/` or `backend/x64/`. The host is validated once as a
`Platform.Target` before stdlib construction, and `BinaryOutput.generateBinary`
selects the backend from that explicit target.

After whole-program ANF and ownership work, `NativePipeline` lowers the current
function inventory to MIR and schedules direct-call strongly connected
components callee first. Independent components at the same dependency depth
share a batch. Each batch proceeds through MIR optimization, LIR lowering, and
register allocation before callers at a later depth. Calls within a recursive
component use conservative clobber information during allocation.

Finalized functions publish a versioned summary of observable effects, mutable
reads, traps, possible divergence, typed constant returns, and target-specific
register writes. Later functions, including functions in another compilation
unit, consume the summaries of their direct callees. Missing, ambiguous, and
indirect callees remain unknown. MIR may substitute a constant result after a
returning call; it removes the call only when the callee is also proven safe to
omit. ARM64 and x64 allocation use finalized callee writes to choose registers
and reduce call saves. A session cache key includes the callee facts actually
consumed by dependency compilation.

---

## Pass 1: Parser (`src/frontend/interpreter/Parser.ml`)

**Input**: Source code string
**Output**: Validated `WrittenTypes`

The copied interpreter parser preserves source syntax in `WrittenTypes`.
Validation checks source-level invariants before type checking.

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

## Pass 1.5: Type Checking (`src/frontend/WrittenChecking.ml`)

**Input**: Validated `WrittenTypes`
**Output**: Checked AST with phase invariants represented by node shape

Source entry points resolve written type annotations and names directly into
`CheckedAST.Program`. Runtime failure expressions have semantic type `TNever`;
privileged compiler sources alone may introduce `TInternalRawPtr` signatures.

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

## Pass 2: AST to ANF (`src/passes/anf/AST_to_ANF.ml`)

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

## Pass 2.2: Generated Result Output (`src/passes/anf/PrintInsertion.ml`)

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

## Pass 2.25: Function Reachability (`src/passes/anf/ANFDeadCodeElimination.ml`)

Expression compilation removes functions that cannot be reached from the
generated program entry before ANF optimization, then repeats the query after
inlining and specialization. Function references and closure allocations are
call-graph edges, so indirect calls retain every statically named target. The
later LIR tree-shaking pass remains responsible for combining fresh and
prebuilt functions and for selecting reachable standard-library functions.

---

## Pass 2.3: Accumulator Helper Lowering (`src/passes/anf/ANFAccumulatorLowering.ml`)

**Input**: ANF
**Output**: ANF with eligible recursive helpers

The structural tail-recursion-modulo-operation rewrite creates helpers for
addition, subtraction, multiplication, fixed constructors, and lists. It
still consumes structured ANF and remains ahead of SSA construction until
the direct SSA-lowering step.

---

## Pass 2.4: ANF to high-level SSA (`src/ir/anf/SSAANF.ml`)

**Input**: ANF after accumulator helper lowering
**Output**: High-level SSA blocks with explicit edges and block parameters

### Responsibilities
- **Build CFG**: Convert structured ANF joins and branches to basic blocks
- **Freshen values**: Give reused ANF temporaries distinct SSA definitions
- **Carry joins**: Pass typed values on edges to block parameters

---

## Pass 2.4.1: SSA Optimizations (`src/passes/anf/SSAOptimization.ml`)

**Input**: Typed SSA blocks
**Output**: Simplified SSA blocks

The bounded fixed-point pass applies scalar folding and strength reduction,
literal and alias propagation, ownership-safe dead-definition removal, and
common-expression reuse across dominating blocks. It folds constant and
Boolean-return branches, removes unreachable blocks, merges simple jumps,
and devirtualizes capture-free local closures. The string-byte and list-index
rewrites preserve the corresponding checked-access behavior without temporary
converted indices or Option values. Block labels are compacted before
inlining so earlier eliminated branches do not change later block layout.

---

## Pass 2.4.5: SSA Inlining (`src/passes/anf/SSAInlining.ml`)

**Input**: Typed SSA blocks
**Output**: SSA with selected direct calls inlined

### Responsibilities
- **Clone callee blocks**: Freshen block labels and value identities at each call
- **Connect returns**: Route every callee return to one typed continuation

## Pass 2.4.6: Known Closure Specialization (`src/passes/anf/SSAHigherOrderSpecialization.ml`)

**Input**: Typed SSA blocks
**Output**: SSA with selected higher-order helpers specialized

### Responsibilities

- **Propagate callable facts**: Track known closures and static function references through aliases, branch values and joins, and function returns
- **Specialize complete call shapes**: Clone a helper once for all eligible known functional arguments while retaining the generic closure path for unknown values
- **Pass captures directly**: Replace closure arguments with ordinary capture parameters and lower `ClosureCall` to direct calls; static references need no target clone or heap closure
- **Cross compilation units**: Use pre-reference-count external templates converted to SSA to create local helper and target clones without changing prebuilt functions
- **Bound transformation size**: Skip large helpers/targets and charge every specialized functional argument against the sixteen-pair program budget

---

## Pass 2.5: Direct-Call Specialization (`src/passes/anf/SSADirectCallSpecialization.ml`)

**Input**: High-level SSA
**Output**: SSA with specialized direct calls and bounded literal clones

The pass removes parameters whose literal value is identical at every direct
call, and creates typed block clones for selected differing literal patterns.
Call sites and function signatures change together. An exact tuple, record, or
wide integer argument can be reconstructed in a clone's entry block; an unused
caller construction is then removed.

## Pass 2.6: Escape Analysis (`src/passes/anf/SSAEscapeAnalysis.ml`)

**Input**: High-level SSA
**Output**: SSA with eligible local aggregates scalar-replaced or uniquely reused

The first escape-analysis scope scalar-replaces fixed-layout tuple, record, and
boxed-sum allocations whose fields are all immediate scalar values, including
Float64.
An allocation is removed only when its complete SSA use set consists of
field projections, local aliases, and representation-only constructor sources.
A remaining uniquely owned record can transfer its allocation to a
sole compatible clone when every field has structurally non-observable
destruction: immediate values and `String`, `Blob`, or `Int` buffers, plus
tuples, lists, dictionaries, and nominal records built recursively from those
leaves. Nominal fields are instantiated with their concrete generic arguments
before eligibility is decided. Regular recursive records are represented by a
typed release-plan back-edge. Reference-count elaboration retains replacement
child edges, releases displaced children with their complete recursive release
plans in field order, and then overwrites the block.

Boxed-sum lowering preserves the instantiated nominal type and each variant's
payload layout in the same fixed-block descriptor. A uniquely local sum can
transfer its two-word `[tag, payload]` block to a later straight-line
constructor of the same instantiated type. Cleanup uses the source variant
descriptor while stores and the result type use the target descriptor, so
cross-variant payload types cannot be confused. Complete sum-shape metadata
proves nested and regular recursive payloads coinductively; recursive cleanup
uses a typed release-plan back-edge.

Returns, calls, closure capture, storage, raw-pointer operations, composite or
potentially observable managed fields without that proof, and every unmodelled
use preserve the ordinary allocation. Streams and containers holding them are
rejected recursively; closures fail closed. Missing registry entries, generic
arity mismatches, type-growing record or sum cycles, and sum candidates that
cross branches or joins also remain unchanged.
Escaping constructors retain their own allocation while eligible immediate
source and intermediate aggregates are scalar-replaced.

Running on SSA before reference-count insertion ensures eliminated aggregates never
acquire root retain or release operations and allows reused fixed blocks to be
treated as one ownership family. Stack allocation, scalar replacement of
managed fields, observable-destruction reuse, and interprocedural
representation changes are outside the current scope.

## Pass 2.7: Reference Count Insertion (`src/passes/anf/ownership/RcSSARefCountInsertion.ml`)

**Input**: High-level SSA
**Output**: SSA with RefCountInc/RefCountDec operations

### Responsibilities
- **Memory management**: Insert reference counting operations
- **Ownership tracking**: Determine when values need inc/dec

### Key Algorithms
- **Borrowed calling convention**: Callers retain ownership, no inc on call
- **Scope-based release**: Dec when value goes out of scope

---

## Pass 2.8: Tail Call Optimization (`src/passes/anf/SSATailCallDetection.ml`)

**Input**: SSA with refcounting
**Output**: SSA annotated for tail calls / self-recursion loops

### Responsibilities
- **Detect tail positions**: Identify safe tail calls
- **Self-recursion loop conversion**: Turn tail-recursive calls into jumps

---

## Pass 3.1: High-level SSA to MIR (`src/passes/anf/ANF_to_MIR.ml`)

**Input**: High-level SSA blocks
**Output**: Mid-level CFG already in SSA form

### Responsibilities
- **Lower operations**: Expand typed ANF operations into MIR instructions
- **Lower block arguments**: Emit typed phis with one source per incoming edge
- **Preserve loop inputs**: Put self-tail-call values on loop-header phi edges
- **Literal lowering**: Keep string and float constants symbolic until needed

`src/passes/mir/SSA_Construction.ml` remains available for MIR analysis and independent
validation. The production pipeline does not run MIR SSA reconstruction.

---

## Pass 3.5: MIR Optimizations (`src/passes/mir/MIR_Optimize.ml`)

**Input**: MIR CFG in SSA
**Output**: Optimized MIR CFG

### Responsibilities
- **Constant folding**: Fold literal computations
- **Sparse conditional constant propagation**: Jointly solve explicitly typed
  integer and Boolean SSA values with executable CFG edges, then prune
  unreachable blocks and phi inputs
- **CSE**: Eliminate duplicate pure expressions
- **Copy propagation inside SCCP**: Simplify moves and trivial phis
- **DCE**: Remove unused instructions
- **CFG simplification**: Remove empty blocks / redirect edges
- **LICM**: Hoist loop-invariant expressions

### Sub-passes (grouped)
- `sccp`, `cse`, `dce`, `cfg_simplify`, `licm`

The default path performs constant folding, copy substitution, constant branch
simplification, and unreachable-block pruning inside SCCP's analysis and
rewrite. The remaining passes follow it in the optimizer's fixed-point loop.

---

## Pass 4: MIR to LIR (`src/passes/mir/MIR_to_LIR.ml`)

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

## Pass 4.5: LIR Peephole (`src/passes/lir/LIR_Peephole.ml`)

**Input**: LIR (virtual regs)
**Output**: Optimized LIR (virtual regs)

### Responsibilities
- **Peephole rewrites**: Local instruction simplifications
- **Branch fusion**: Combine compare/set/branch sequences when safe

---

## Pass 5: Register Allocation (`src/passes/lir/RegisterAllocation.ml`)

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
them to actual hardware registers in `src/backend/arm64/Backend_Arm64_CodeGen.ml` and `src/backend/x64/CodeGen_X86_64.ml`.

- **Caller-saved (preferred)**: X1-X7
- **Callee-saved**: X19-X26 on ARM64, X19-X21 on x86_64 (fewer because
  X22/X23 are reserved for the heap pointer and free list on x86_64).
- **Reserved**: X0 (return), X8-X10 (scratch), X27-X28 (runtime state),
  X29-X30 (ABI).

On x86_64 the LIR PhysRegs X8-X17 all collapse onto R11 (shared scratch);
the allocator is aware of this via `isX86_64 arch` checks.

---

## Pass 5.5: Function Tree Shaking (`src/passes/lir/FunctionTreeShaking.ml`)

**Input**: LIR (physical regs)
**Output**: LIR with only reachable functions

### Responsibilities
- **Prune unused functions**: Keep `_start` roots and reachable callees
- **Stdlib filtering**: Include only stdlib functions called by user code
- **Call graph helpers**: Uses `src/passes/lir/DeadCodeElimination.ml` for LIR reachability and
  `src/passes/anf/ANFDeadCodeElimination.ml` when computing reachable stdlib names from ANF

---

## Pass 6: Code Generation (`src/backend/arm64/Backend_Arm64_CodeGen.ml` and `src/backend/x64/CodeGen_X86_64.ml`)

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
- **Literal pool resolution**: ARM64 resolves symbolic string and float data
  labels into literal pools. x64 collects string literals for RIP-relative
  addressing and materializes float bits as immediates.
- **Label resolution**: fix up branch offsets to real byte distances.
- **Instruction encoding**: convert symbolic instructions to bytes per
  the ISA spec (fixed 32-bit on arm64, variable 1–15 bytes on x64).
- **Binary generation**: emit Mach-O (macOS arm64) or ELF (Linux arm64
  and x64).

### Per-backend files

| Backend | Encoding                               | Resolve                               | Binary                                                             |
|---------|----------------------------------------|---------------------------------------|--------------------------------------------------------------------|
| arm64   | `src/backend/arm64/ARM64_Encoding.ml`           | `src/backend/arm64/ARM64_Resolve.ml`           | `src/backend/arm64/Binary_Generation_MachO.ml` and `src/backend/arm64/Backend_Arm64_Binary_Generation_ELF.ml` via `src/backend/arm64/Emit.ml` |
| x64     | `src/backend/x64/X86_64_Encoding.ml`             | `src/backend/x64/X86_64_Resolve.ml`             | `src/backend/x64/Binary_Generation_ELF_X86_64.ml`                            |

---

## Data Structure Files

| File               | Purpose                               |
|--------------------|---------------------------------------|
| `src/AST.ml`           | Abstract Syntax Tree types            |
| `src/ir/anf/ANF.ml`           | A-Normal Form types                   |
| `src/ir/mir/MIR.ml`           | Mid-level IR types                    |
| `src/ir/lir/LIR.ml`           | Low-level IR types                    |
| `src/Platform.ml`      | OS/Arch DUs and per-target syscall tables |
| `src/backend/arm64/ARM64.ml`         | ARM64 instruction and register types  |
| `src/backend/arm64/Symbolic.ml` | Symbolic ARM64 instructions (pre-encoding) |
| `src/backend/x64/X86_64.ml`        | x86_64 instruction and register types |
| `src/backend/binary/ELF.ml`    | Shared ELF header/segment types       |

---

## Testing Each Pass

Each pass can be tested in isolation:

- **Parser**: Test with source strings, check AST structure
- **Type Checker**: Test type errors are caught
- **ANF**: PassTestRunner validates ANF output
- **End-to-end**: `.e2e` files test full pipeline

Build and run all tests: `./build --ai && ./run-tests --ai`
