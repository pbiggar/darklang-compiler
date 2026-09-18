# Architecture

Recursive declaration identity, grouping, and interpreter parity are specified
in the [recursion compatibility ledger](../compatibility/language/recursion.md).

Name resolution is a semantic boundary between parsing and ordinary type
checking. See [Name resolution parity](../compatibility/language/name-resolution.md) for the identity
model, context precedence, diagnostics, and pinned interpreter evidence.

## Design Philosophy

- The compiler is designed to eventually be rewritten in Darklang itself
- Generate Mach-O/ELF binaries directly without using an assembler or linker
- Pure functional F# code (no mutation, no exceptions)

## IR Pipeline

```
Source -> Parsed AST -> Checked AST -> ANF -> MIR -> LIR -> RegAlloc -> CodeGen -> Encode -> Binary
```

Passes 1-5 run through parsing, type checking, target-independent IR lowering,
and register allocation. Passes 6-8 (CodeGen, encoding, binary output) live
under `backend/arm64/` or `backend/x64/` depending on the target.

### Why Multiple IRs?

Each IR is designed to make specific transformations easier:

| IR  | Purpose                | Key Transformations                                       |
|-----|------------------------|-----------------------------------------------------------|
| AST | Close to source syntax | Type checking, error messages with source locations      |
| ANF | Explicit eval order    | Monomorphization, lambda lifting, ref counting            |
| MIR | Control flow graph     | SSA construction, optimizations, target-independent       |
| LIR | Close to machine code  | Register constraints, instruction selection               |
| ISA | Architecture-specific  | Encoding, branch offset calculation (ARM64 or x86-64)     |

### Parsed and checked ASTs

- `AST.fs` closely mirrors source syntax and is used for name resolution,
  checking, and source diagnostics.
- Successful checking constructs the distinct recursive nodes in
  `CheckedAST.fs`. Required lambda types, typed recursion evidence, canonical
  nominal references, and checked value definitions are structural there,
  rather than optional phase flags.
- Compiler preparation and ANF lowering accept only `CheckedAST.Program`.

### ANF (A-Normal Form)

Closed scalar-list regions pass through typed collection/storage/ownership
stages before ordinary ANF lowering. See
[compiler-selected list arrays](runtime/list-array-reuse.md) for the supported
boundary, storage contract, and remaining general HIR migration.

- All intermediate values have names (no nested expressions)
- Evaluation order is explicit
- Enables:
  - **Monomorphization**: Generate specialized code for each generic instantiation
  - **Lambda lifting**: Convert closures to top-level functions
  - **Reference count insertion**: Add memory management operations
- Types defined in `ir/anf/ANF.fs`

### MIR (Mid-level IR)

- Control Flow Graph (CFG) representation
- Basic blocks with explicit jumps
- Platform-independent
- SSA form for optimizations
- Types defined in `ir/mir/MIR.fs`

### LIR (Low-level IR)

- Close to machine instructions but still target-independent
- Virtual registers (unlimited)
- Calling convention handling (via `Platform.Arch`)
- Types defined in `ir/lir/LIR.fs`

## Memory Management

Uses reference counting (not tracing GC):

1. `RefCountInsertion.fs` inserts inc/dec operations in ANF
2. Runtime functions handle actual ref counting
3. Borrowed calling convention: callers retain ownership

Why ref counting?
- Deterministic deallocation
- Simpler to implement correctly
- Works well with the functional style

## Type System

- **Monomorphization**: Generics are expanded at compile time
- No runtime type information for generics
- Type and layout metadata remain available where ownership and native lowering need them
- Supports: primitives, tuples, records, ADTs, lists, functions

## Platform Support

- ARM64 (macOS and Linux) and x86_64 (Linux).
- Direct binary generation — no external assembler or linker:
  - `backend/arm64/Binary_Generation_MachO.fs` — ARM64 macOS
  - `backend/arm64/Binary_Generation_ELF.fs`   — ARM64 Linux
  - `backend/x64/Binary_Generation_ELF.fs`     — x86_64 Linux
- The host OS/architecture pair is validated once as a `Platform.Target`
  before stdlib construction. Register allocation, backend selection, runtime
  generation, and binary emission receive that target explicitly.
- Adding a new architecture: add a case to `Platform.Arch`, create
  `backend/<arch>/` instruction selection, encoding, resolution, and binary output,
  and wire it into `BinaryOutput.generateBinary`.

## Compiler Library API

`CompilerLibrary.compile` accepts a `CompilationContexts.CompileRequest`. The
supporting APIs have explicit owners under `driver/`:

- `StdlibCompilation`: stdlib prebuilding and concrete specializations.
- `PreambleAnalysis` and `PreambleCompilation`: reusable source environments.
- `CompilationSession`: bounded dependency, lowering, and backend caches.
- `CompilerExecution`: running generated binaries with target and timing.
- `CompilerReachability`: stdlib function inventory and reachability queries.

The driver `execute` is distinct from the Dark `Stdlib.Cli.execute` effect.
CLI/process operations remain typed through ANF, MIR, and LIR and reach the
native syscall/ABI boundary only in the selected backend. See
[CLI/process/host/input parity](../compatibility/cli-process-host-input.md).

## Compiler Cache

The compiler has no process-global compile cache. Callers may instead provide
a bounded `CompilationSession` when compiling a related group of programs.
Within that explicit lifetime the compiler reuses generated-declaration ANF,
fully lowered compiler-generated dependencies, target-specific function
chunks, prepared emission chunks, and composable code-generation metadata.
Disposing the session releases all of those entries. One-shot CLI compilation
uses no session and therefore retains no compilation state.

ARM64 function chunks are reusable only inside the registry context that
produced them. LIR alone does not encode every record/sum layout consulted by
code generation, so structurally equal LIR from different contexts must not
share cached machine code. The source-independent `_start` trampoline is the
intentional exception.

The process entry point is a fixed `_start` trampoline which calls
`__dark_compiler_program_entry`. The changing source expression lives in the
latter function, allowing a session to reuse `_start` without giving tests a
different compiler path from production callers.

## Key Invariants

Each pass must maintain certain properties:

| Pass        | Input Invariant     | Output Guarantee                         |
|-------------|---------------------|------------------------------------------|
| Parser      | Valid source string | Well-formed parsed AST                   |
| TypeChecker | Well-formed parsed AST | Structurally valid checked AST        |
| AST->ANF    | Checked AST         | All expressions named, lambdas lifted    |
| ANF->MIR    | Named expressions   | Valid CFG with basic blocks              |
| MIR->LIR    | Valid CFG           | Target-compatible LIR instructions       |
| RegAlloc    | Virtual registers   | Physical registers assigned              |
| CodeGen     | Physical registers  | Valid ISA instruction sequence           |
