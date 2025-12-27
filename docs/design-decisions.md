# Design Decisions

This document explains the key architectural and implementation decisions in the Dark compiler, including the rationale behind each choice.

## Multi-Stage IR Pipeline

**Decision**: Use a multi-stage pipeline with distinct intermediate representations:
```
Source → AST → ANF → MIR → LIR → ARM64 → Binary
```

**Rationale**:
- Each IR focuses on specific concerns, making transformations simpler
- **AST**: Preserves source structure for error messages and future tooling
- **ANF**: Makes evaluation order explicit, simplifies code generation
- **MIR**: Platform-independent 3-address code, enables future optimizations
- **LIR**: Target-specific (ARM64) with virtual registers, clean separation from register allocation
- Easier to test each pass in isolation
- Enables future backend targets (x86, WASM) by swapping post-MIR stages

## Desugaring Strategy: AST-level vs Lexer-level

**Decision**: Desugar complex syntax (like string interpolation) at the AST level, not in the lexer.

**Rationale**:
- **Better error messages**: Source location information preserved for errors
- **Separation of concerns**: Lexer handles tokenization, parser handles structure
- **Future optimization**: AST-level representation enables pattern-specific optimizations
- **Tooling support**: IDE features (hover, go-to-definition) can understand the structure

**Example**: String interpolation `$"Hello {name}"` is parsed to an `InterpolatedString` AST node with `StringPart` list, then desugared to `StringConcat` operations in the ANF pass.

## Monomorphization for Generics

**Decision**: Use monomorphization (generating specialized versions) for generic functions rather than type erasure or runtime dispatch.

**Rationale**:
- **No runtime overhead**: Specialized code runs at native speed
- **Simpler runtime**: No need for runtime type information or vtables
- **Better optimization**: Each specialization can be optimized independently
- **Matches Darklang semantics**: Darklang uses structural types, monomorphization fits naturally

**Trade-off**: Binary size increases with more specializations. Acceptable for current use case.

## Reference Counting for Memory Management

**Decision**: Use reference counting instead of tracing garbage collection.

**Rationale**:
- **Deterministic destruction**: Resources freed immediately when no longer used
- **Simpler implementation**: No need for GC roots, safe points, or stop-the-world pauses
- **Matches Darklang model**: Darklang uses immutable data structures where ref counting works well
- **Predictable performance**: No GC pauses, consistent latency

**Implementation details**:
- Borrowed calling convention: callers retain ownership of arguments
- Size-segregated free lists for memory reuse (256 bytes of free list heads)
- X27 = free list base pointer, X28 = bump allocator pointer

## Direct Binary Generation

**Decision**: Generate Mach-O (macOS) and ELF (Linux) executables directly, without external assembler or linker.

**Rationale**:
- **Self-contained toolchain**: No external dependencies (no LLVM, no system assembler)
- **Faster compilation**: No subprocess overhead, no serialization to text assembly
- **Full control**: Can implement custom features without tool constraints
- **Educational value**: Understand the full compilation pipeline

**Implementation**:
- Two-pass encoding for forward branch offset computation
- Platform detection for Mach-O vs ELF generation
- Direct syscall interface (write, exit) - no libc dependency

## Functional F# Style

**Decision**: Write compiler in purely functional F# with no mutable state.

**Rationale**:
- **Future Darklang self-hosting**: Code should be translatable to Darklang
- **Easier reasoning**: Immutable data structures prevent bugs from shared state
- **Result-based error handling**: Explicit error propagation, no exceptions
- **Testing**: Pure functions are trivially testable

**Guidelines** (from CLAUDE.md):
- Use `Result` types, not exceptions or `failwith`
- Avoid mutable values completely
- Use `Option` for missing values
- Prefer string interpolation over printf-style formatting

## Compiler Pass Numbering Convention

**Decision**: Prefix pass files with their order in the pipeline (1_, 1.5_, 2_, 2.5_, etc.).

**Rationale**:
- **Clear ordering**: Developers immediately see the compilation flow
- **Easy navigation**: Files sort in pipeline order in file explorers
- **Decimal numbering**: Allows inserting passes (1.5_TypeChecking) without renaming

## Type System Design

**Decision**: Use top-down type checking with explicit type annotations on function signatures.

**Rationale**:
- **Simpler implementation**: No complex type inference algorithm
- **Clear documentation**: Function types serve as documentation
- **Better errors**: Type mismatches have clear expected vs actual types
- **Matches Darklang**: Darklang requires explicit function signatures

**Features**:
- Generics via type parameters on functions and types
- Structural types: records and sum types (algebraic data types)
- Pattern matching with exhaustiveness guidance
- Type-directed field lookup for record access

## ARM64-First Target

**Decision**: Target ARM64 (Apple Silicon) as the primary architecture.

**Rationale**:
- **Development hardware**: Primary development on Apple Silicon Macs
- **Modern ISA**: Clean, regular instruction set easier to target
- **Performance**: Native performance on development machines
- **Linux support**: ARM64 Linux (including Docker) works with same backend

**Notable ARM64 considerations**:
- No native modulo instruction: implemented as `sdiv` + `msub` sequence
- Immediate value constraints require careful handling
- Caller-saved vs callee-saved register conventions affect allocation strategy

## Test-Driven Development

**Decision**: Write tests first, use end-to-end tests as primary validation.

**Rationale**:
- **Specification**: Tests define expected behavior before implementation
- **Regression prevention**: 1600+ tests catch regressions immediately
- **Refactoring safety**: Can restructure internals with confidence
- **Documentation**: Test cases show how features work

**Test infrastructure**:
- `.e2e` files with simple DSL: `expression = expected_output`
- Per-pass tests for intermediate representations
- Unit tests for encoding and utilities
