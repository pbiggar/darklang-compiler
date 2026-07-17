# Design Decisions

This document explains the key architectural and implementation decisions in the Dark compiler, including the rationale behind each choice.

## Multi-Stage IR Pipeline

**Decision**: Use a multi-stage pipeline with distinct intermediate representations:
```
Source → AST → ANF → MIR → LIR → target ISA → Binary
```

**Rationale**:
- Each IR focuses on specific concerns, making transformations simpler
- **AST**: Preserves source structure for error messages and future tooling
- **ANF**: Makes evaluation order explicit, simplifies code generation
- **MIR**: Platform-independent 3-address code, enables future optimizations
- **LIR**: Target-neutral instruction shapes with virtual registers, clean separation from register allocation
- Easier to test each pass in isolation
- Enables backend targets (ARM64 and x86-64 today, WASM in the future) by swapping post-MIR stages

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

**Policy**: The compiler does not default unresolved type parameters to arbitrary concrete types. If monomorphization needs hashing or equality on an unresolved type, lowering emits an explicit runtime error expression (`Builtin.testRuntimeError`), making unreachable paths explicit instead of silently picking a type.

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

## Native Backend Targets

**Decision**: Support ARM64 on macOS/Linux and x86-64 on Linux, with ARM64 remaining the primary development target.

**Rationale**:
- **Development hardware**: Primary development on Apple Silicon Macs
- **Modern ARM64 ISA**: Clean, regular instruction set easier to target
- **Performance**: Native performance on development machines
- **Linux support**: ARM64 and x86-64 Linux (including Docker) are selected at runtime by platform detection

**Notable backend considerations**:
- No native modulo instruction: implemented as `sdiv` + `msub` sequence
- Immediate value constraints require careful per-ISA handling
- Caller-saved vs callee-saved register conventions affect allocation strategy

## HAMT-Based Immutable Dictionary

**Decision**: Implement `Stdlib.Dict` using a Hash Array Mapped Trie (HAMT) data structure with raw memory primitives.

**What is HAMT?**

A HAMT is a persistent (immutable) hash table that provides near-O(1) lookups while supporting efficient structural sharing for updates. It's the data structure behind Clojure's maps, Scala's immutable maps, and many functional language implementations.

**Key concepts**:
- A 64-bit hash is divided into 6-bit chunks (11 levels max)
- Each node has a 64-bit bitmap indicating which of 64 possible children exist
- Children are stored in a compressed array (only present children occupy space)
- Updates create new path from root to modified leaf, sharing unchanged subtrees

**Why HAMT for Dark?**

1. **Immutability**: Darklang uses immutable values; HAMT enables efficient "updates" via structural sharing
2. **Performance**: O(log64 n) ≈ O(1) for practical sizes (7 levels handles billions of entries)
3. **Memory efficiency**: Compressed nodes avoid sparse array waste
4. **Proven design**: Battle-tested in Clojure, Scala, Haskell, etc.

**Implementation approach**:

The implementation is split into two parts:

1. **Compiler intrinsics** (low-level memory operations):
   - `__raw_alloc(size: Int64) -> RawPtr` - allocate unmanaged memory
   - `__raw_free(ptr: RawPtr) -> Unit` - free memory
   - `__raw_get<T>(ptr: RawPtr, offset: Int64) -> T` - read 8 bytes as a typed value
   - `__raw_write_word(ptr: RawPtr, offset: Int64, value: Int64) -> Unit` - write 8 unmanaged bytes
   - `__raw_write_byte(ptr: RawPtr, offset: Int64, value: Int64) -> Unit` - write 1 unmanaged byte
   - `__raw_slot_init<T>(ptr: RawPtr, offset: Int64, value: T) -> Unit` - initialize a typed 8-byte slot edge
   - `__dict_to_rawptr<k, v>(dict: Dict<k, v>) -> RawPtr` - borrow a raw pointer view with tag bits cleared
   - `__rawptr_to_dict<k, v>(ptr: RawPtr, tag: Int64) -> Dict<k, v>` - retag an initialized raw HAMT node as a managed dict

2. **Pure Dark stdlib** (HAMT algorithms):
   - `hashChunk(hash, level)` - extract 6-bit chunk at level
   - `hasBit(bitmap, bit)` - check if child exists
   - `childIndex(bitmap, bit)` - compute compressed array index using popcount
   - `setBit(bitmap, bit)` - set bit in bitmap

**Memory layout** (planned):

```
HAMT Node (variable size):
  [0..7]   bitmap    : Int64     - 64-bit bitmap indicating present children
  [8..15]  child[0]  : RawPtr    - first present child (if bitmap bit 0+ set)
  [16..23] child[1]  : RawPtr    - second present child
  ...

Leaf Node:
  [0..7]   hash      : Int64     - full hash of key
  [8..15]  key       : RawPtr    - pointer to key value
  [16..23] value     : RawPtr    - pointer to value
```

**Trade-offs**:

- **TRawPtr type**: Internal type not exposed to users, enables unsafe memory ops
- **Explicit RC integration**: Raw HAMT memory uses typed slot initialization
  and dict release helpers rather than implicit GC scanning
- **Complexity**: HAMT is more complex than simple hash tables, but necessary for immutability

**Current status**: Phase 4 in progress - bitwise operators and popcount implemented, raw memory intrinsics added, HAMT helper functions in stdlib.dark.

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
