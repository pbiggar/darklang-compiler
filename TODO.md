# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## Things to add

- require type name for record creation
- polymorphism in types, functions
- reference counting

### Phase 5b: String Concatenation (Later)

- [ ] Implement simple bump allocator
- [ ] Add Concat operator
- [ ] Add heap string allocation
- [ ] Write E2E tests for concatenation

## Testing Strategy

### E2E Test Files

5. [ ] strings.e2e (Phase 5)

## Code Quality

### Documentation

- [ ] Update README.md with new language features

## Current Status

- ✅ Integers with full arithmetic (+, -, \*, /)
- ✅ Booleans with comparisons (==, !=, <, >, <=, >=) and operations (&&, ||, !)
- ✅ Variables (let bindings with shadowing support)
- ✅ Let-pattern matching with tuples (e.g., `let (a, b) = tuple in ...`)
- ✅ Control flow (if/then/else expressions, including in atom position)
- ✅ Functions with type signatures, calls, and recursion
- ✅ String literals with escape sequences
- ✅ Lists (linked list implementation with [1, 2, 3] syntax and pattern matching)
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 8-pass compiler pipeline (Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ 664 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)

## Known Limitations & Partial Implementations

These are features that exist but have known limitations or incomplete implementations:

### 1. Boolean Printing (Quality of Life)

**Status**: ⚠️ Partial

**Current Behavior:**

- Boolean literals (`true`, `false`) print as "true" and "false"
- Computed booleans (e.g., `5 > 3`) still print as `1` and `0`

**Why Partial:**

- Type information is lost during compilation for computed values
- Full fix would require propagating type info through MIR/LIR

**Impact**: Low - literal booleans work correctly, computed ones show numeric representation

### 2. Reference Counting (Memory Management)

**Status**: ⏸️ Deferred

**Current Behavior:**

- Simple bump allocator for heap allocations
- No deallocation (memory is never freed)

**Investigation Summary:**

Attempted to add reference count headers to heap allocations with the layout:
`[ref_count: 8 bytes][payload: sizeBytes]`

The approach was to return a pointer offset by 8 bytes (to the payload) while reserving
space for the ref count. However, adding the 8-byte offset to the returned pointer
causes segfaults in list pattern matching tests, specifically with 5+ element lists
or lists bound to variables before matching.

The issue appears related to how pointer arithmetic interacts with the register
allocation spilling mechanism and/or list pattern matching code. Tests pass with
offsets 0 or 1, but fail with offsets >= 2. The exact cause remains unclear.

**Next Steps:**

1. Investigate how list pattern matching accesses memory
2. Consider alternative layouts (ref count after payload)
3. May need to modify ANF/MIR compilation to explicitly track heap pointers

**Impact**: Currently memory is never freed, but the bump allocator has 64KB of space which is sufficient for most test programs
