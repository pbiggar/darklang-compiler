# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## Things to add

- polymorphism in types, functions
- string concatenation

### Phase 5b: String Concatenation (Later)

- [ ] Implement simple bump allocator
- [ ] Add Concat operator
- [ ] Add heap string allocation
- [ ] Write E2E tests for concatenation

## Testing Strategy

### E2E Test Files

5. [ ] strings.e2e (Phase 5)

## Code Quality Issues

The following are known simplifications or potential issues in the compiler code.
**Principle**: The compiler should never guess - always error if something is unknown.

### HIGH Priority (Can cause crashes or wrong results)

(No current high-priority issues)

### MEDIUM Priority (Incomplete features)

(No current medium-priority issues)

### LOW Priority (Feature limitations, not bugs)

#### 3. Monomorphic lists
**Files:** `src/DarkCompiler/AST.fs` (line 31), `1.5_TypeChecking.fs` (line 530)
**Issue:** TList only supports int, not polymorphic types
**Future:** Add type parameter support for lists

#### 4. ARM64 register subset
**File:** `src/DarkCompiler/ARM64.fs` (line 26)
**Issue:** Only subset of ARM64 registers implemented
**Future:** Add more registers as needed

#### 5. Parser structure limitations
**File:** `src/DarkCompiler/passes/1_Parser.fs` (line 839)
**Issue:** Only function definitions allowed after expressions
**Future:** Expand module-level structure support

### Documentation

- [x] Update README.md with new language features

## Current Status

- ✅ Integers with full arithmetic (+, -, \*, /)
- ✅ Booleans with comparisons (==, !=, <, >, <=, >=) and operations (&&, ||, !)
- ✅ Variables (let bindings with shadowing support)
- ✅ Let-pattern matching with tuples (e.g., `let (a, b) = tuple in ...`)
- ✅ Control flow (if/then/else expressions, including in atom position)
- ✅ Functions with type signatures, calls, and recursion
- ✅ String literals with escape sequences
- ✅ Lists (linked list implementation with [1, 2, 3] syntax and exact-length pattern matching)
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 9-pass compiler pipeline (Parser → TypeCheck → ANF → RefCount → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ Register allocation with callee-saved registers (X19-X27) for high register pressure
- ✅ Reference counting with free list memory reuse
- ✅ 731 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)
- ✅ Type-directed record field lookup (no ambiguity when multiple record types have same field names)
- ✅ Function return type inference using function registry (enables type inference for let-bound function calls)
- ✅ Record literals require type name: `Point { x = 1, y = 2 }` syntax (no anonymous records)
- ✅ Sum types (enums with payloads, pattern matching with extraction)
- ✅ Boolean printing (both literals and computed expressions print as true/false)

## Implementation Notes

### Reference Counting (Memory Management)

**Implementation:**

- Ref count headers initialized to 1 at HeapAlloc
- Memory layout: `[payload: sizeBytes][refcount: 8 bytes]`
- RefCountInc inserted after TupleGet (when extracting heap values)
- RefCountDec inserted at end of scope (before Return) for owned values
- Borrowed values (TupleGet, aliases) don't get Dec (parent owns memory)
- Free list memory reuse: freed blocks added to size-segregated free lists
- HeapAlloc checks free list first, falls back to bump allocator

**Architecture:**

- X27 = free list heads base (32 size classes × 8 bytes = 256 bytes)
- X28 = bump allocator pointer
- Size class = payload size (8-byte aligned)

**Testing:**

- Stress test: 5000 allocations of 24-byte tuples (120KB) with 64KB heap
- Verified: passes with free list, crashes (exit 139) without (`--no-free-list` flag)
- All 731 tests pass
