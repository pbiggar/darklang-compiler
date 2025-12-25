# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## Things to add

- let-matching with tuples
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
- ✅ Control flow (if/then/else expressions, including in atom position)
- ✅ Functions with type signatures, calls, and recursion
- ✅ String literals with escape sequences
- ✅ Lists (linked list implementation with [1, 2, 3] syntax and pattern matching)
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 8-pass compiler pipeline (Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ 646 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)

## Known Limitations & Partial Implementations

These are features that exist but have known limitations or incomplete implementations:

### 1. Boolean Printing (Quality of Life)

**Status**: ⚠️ Partial

**Current Behavior:**

- Booleans print as `1` (true) and `0` (false)

**Missing:**

- `generatePrintBool()` function in Runtime.fs
- Should print "true"/"false" strings instead

**Impact**: Low - values are correct, just not pretty
**Required For**: Phase 5 (will need string printing anyway)
