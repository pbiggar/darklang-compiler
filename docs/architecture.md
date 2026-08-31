# Architecture

Recursive declaration identity, grouping, and interpreter parity are specified
in [recursion-parity.md](recursion-parity.md).

Name resolution is a semantic boundary between parsing and ordinary type
checking. See [Name resolution parity](name-resolution.md) for the identity
model, context precedence, diagnostics, and pinned interpreter evidence.

## Design Philosophy

- The compiler is designed to eventually be rewritten in Darklang itself
- Generate Mach-O/ELF binaries directly without using an assembler or linker
- Pure functional F# code (no mutation, no exceptions)

## IR Pipeline

```
Source -> AST -> ANF -> MIR -> LIR -> RegAlloc -> CodeGen -> Encode -> Binary
```

Passes 1-5 run through parsing, type checking, target-independent IR lowering,
and register allocation. Passes 6-8 (CodeGen, encoding, binary output) live
under `passes/arm64/` or `passes/x64/` depending on the target.

### Why Multiple IRs?

Each IR is designed to make specific transformations easier:

| IR  | Purpose                | Key Transformations                                       |
|-----|------------------------|-----------------------------------------------------------|
| AST | Close to source syntax | Type checking, error messages with source locations      |
| ANF | Explicit eval order    | Monomorphization, lambda lifting, ref counting            |
| MIR | Control flow graph     | SSA construction, optimizations, target-independent       |
| LIR | Close to machine code  | Register constraints, instruction selection               |
| ISA | Architecture-specific  | Encoding, branch offset calculation (ARM64 or x86-64)     |

### AST (Abstract Syntax Tree)

- Closely mirrors source code structure
- Used for type checking and error reporting
- Types defined in `AST.fs`

### ANF (A-Normal Form)

- All intermediate values have names (no nested expressions)
- Evaluation order is explicit
- Enables:
  - **Monomorphization**: Generate specialized code for each generic instantiation
  - **Lambda lifting**: Convert closures to top-level functions
  - **Reference count insertion**: Add memory management operations
- Types defined in `ANF.fs`

### MIR (Mid-level IR)

- Control Flow Graph (CFG) representation
- Basic blocks with explicit jumps
- Platform-independent
- SSA form for optimizations
- Types defined in `MIR.fs`

### LIR (Low-level IR)

- Close to machine instructions but still target-independent
- Virtual registers (unlimited)
- Calling convention handling (via `Platform.Arch`)
- Types defined in `LIR.fs`

## Memory Management

Uses reference counting (not tracing GC):

1. `2.5_RefCountInsertion.fs` inserts inc/dec operations in ANF
2. Runtime functions handle actual ref counting
3. Borrowed calling convention: callers retain ownership

Why ref counting?
- Deterministic deallocation
- Simpler to implement correctly
- Works well with the functional style

## Type System

- **Monomorphization**: Generics are expanded at compile time
- No runtime type information for generics
- Types are fully erased after ANF pass
- Supports: primitives, tuples, records, ADTs, lists, functions

## Platform Support

- ARM64 (macOS and Linux) and x86_64 (Linux).
- Direct binary generation — no external assembler or linker:
  - `passes/arm64/8_Binary_Generation_MachO.fs` — ARM64 macOS
  - `passes/arm64/8_Binary_Generation_ELF.fs`   — ARM64 Linux
  - `passes/x64/8_Binary_Generation_ELF.fs`     — x86_64 Linux
- The host OS/architecture pair is validated once as a `Platform.Target`
  before stdlib construction. Register allocation, backend selection, runtime
  generation, and binary emission receive that target explicitly.
- Adding a new architecture: add a case to `Platform.Arch`, create
  `passes/<arch>/{6_CodeGen,7_Encoding,7_Resolve,8_Binary_Generation_*}.fs`,
  and wire it into `CompilerLibrary.generateBinary`.

## Compiler Library API

`CompilerLibrary.fs` exposes a narrow surface for tools/tests:

- `buildStdlib` for target-specific stdlib prebuilding in test harnesses and tooling
- `buildStdlibSpecializations` for suite-level stdlib specializations
- `buildPreambleContext` for ad-hoc preamble reuse
- `analyzePreamble` + `buildPreambleContextFromAnalysis` for suite-level preamble specialization
- `compile` for in-memory compilation via `CompileRequest`
- `execute` for running compiled binaries with their selected target and timing

This driver `execute` is distinct from the Dark `Stdlib.Cli.execute` effect.
CLI/process operations remain typed through ANF, MIR, and LIR and reach the
native syscall/ABI boundary only in the selected backend. See
[CLI/process/host/input parity](cli-process-host-input-parity.md).

## Compiler Cache

The compiler does not use a compile cache.
remains as a historical record of the removed cache design.

## Key Invariants

Each pass must maintain certain properties:

| Pass        | Input Invariant     | Output Guarantee                         |
|-------------|---------------------|------------------------------------------|
| Parser      | Valid source string | Well-formed AST                          |
| TypeChecker | Well-formed AST     | Type-consistent AST                      |
| AST->ANF    | Typed AST           | All expressions named, lambdas lifted    |
| ANF->MIR    | Named expressions   | Valid CFG with basic blocks              |
| MIR->LIR    | Valid CFG           | Target-compatible LIR instructions       |
| RegAlloc    | Virtual registers   | Physical registers assigned              |
| CodeGen     | Physical registers  | Valid ISA instruction sequence           |
