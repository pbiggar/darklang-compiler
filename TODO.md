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

**Status**: 🚧 Infrastructure Complete

**Current Behavior:**

- Bump allocator for heap allocations
- Ref count headers initialized to 1 for each allocation
- Memory layout: `[payload: sizeBytes][refcount: 8 bytes]`
- No deallocation yet (memory is never freed)

**What's Done:**

✅ Ref count field added to all heap allocations
✅ Ref count initialized to 1 at allocation time
✅ All 664 tests pass with ref count headers

**History:**

Initially attempted layout `[refcount: 8 bytes][payload]` where the returned pointer
would be offset by 8 bytes to the payload start. This caused mysterious segfaults
with register spilling in list pattern matching (5+ elements or let-bound lists).

**Solution:** Use layout `[payload][refcount: 8 bytes]` instead. The returned pointer
points directly to the payload (unchanged), and the ref count is stored after it
at offset `sizeBytes`. This avoids any pointer arithmetic on the returned value.

**Remaining Work:**

1. Add ref count increment when pointers are copied
2. Add ref count decrement when pointers go out of scope
3. Free memory when ref count reaches 0 (requires tracking free list or similar)
4. Track heap pointer types through ANF/MIR/LIR for proper insertion of inc/dec

**Impact**: Currently memory is never freed, but the bump allocator has 64KB of space which is sufficient for most test programs
