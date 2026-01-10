# Architecture

## Design Philosophy

- The compiler is designed to eventually be rewritten in Darklang itself
- Generate Mach-O/ELF binaries directly without using an assembler or linker
- Pure functional F# code (no mutation, no exceptions)

## IR Pipeline

```
Source -> AST -> ANF -> MIR -> LIR -> ARM64 -> Binary
```

### Why Multiple IRs?

Each IR is designed to make specific transformations easier:

| IR | Purpose | Key Transformations |
|----|---------|---------------------|
| **AST** | Close to source syntax | Type checking, error messages with source locations |
| **ANF** | Explicit evaluation order | Monomorphization, lambda lifting, ref counting |
| **MIR** | Control flow graph | SSA construction, optimizations, platform-independent |
| **LIR** | ARM64-specific | Register constraints, instruction selection |
| **ARM64** | Machine instructions | Encoding, branch offset calculation |

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

- ARM64-specific instruction selection
- Virtual registers (unlimited)
- Calling convention handling
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

- ARM64 only (macOS and Linux)
- Direct binary generation:
  - `8_Binary_Generation_MachO.fs` for macOS
  - `8_Binary_Generation_ELF.fs` for Linux
- No external assembler or linker required

## Compiler Library API

`CompilerLibrary.fs` exposes a narrow surface for tools/tests:

- `compileWithOptions` for in-memory compilation with explicit options
- `compileAndRunWithOptions` for compile+execute with explicit options
- `compileStdlib` for test harnesses and tooling that cache stdlib
- `compilePreamble` and `compileTestWithPreamble` for preamble reuse in tests
- `compileAndRunWithStdlibCachedTimed` for E2E tests with timing breakdown

## Key Invariants

Each pass must maintain certain properties:

| Pass | Input Invariant | Output Guarantee |
|------|-----------------|------------------|
| Parser | Valid source string | Well-formed AST |
| TypeChecker | Well-formed AST | Type-consistent AST |
| AST->ANF | Typed AST | All expressions named, lambdas lifted |
| ANF->MIR | Named expressions | Valid CFG with basic blocks |
| MIR->LIR | Valid CFG | ARM64-compatible instructions |
| RegAlloc | Virtual registers | Physical registers assigned |
| CodeGen | Physical registers | Valid ARM64 instruction sequence |
