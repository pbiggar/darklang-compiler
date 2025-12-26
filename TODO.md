# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## Code Quality Issues

The following are known simplifications or potential issues in the compiler code.
**Principle**: The compiler should never guess - always error if something is unknown.

### HIGH Priority (Can cause crashes or wrong results)

- Flesh out the language.
  - Add all tests from https://github.com/darklang/dark/tree/main/backend/testfiles/execution/language, adapting them for this implementation of the Darklang language.
  - Determine what features are missing to allow them all to be compiled

### MEDIUM Priority (Incomplete features)

(No current medium-priority issues)

### LOW Priority (Feature limitations, not bugs)

#### 1. ARM64 register subset

**File:** `src/DarkCompiler/ARM64.fs` (line 26)
**Issue:** Only subset of ARM64 registers implemented
**Future:** Add more registers as needed

#### 2. Parser structure limitations

**File:** `src/DarkCompiler/passes/1_Parser.fs` (line 839)
**Issue:** Only function definitions allowed after expressions
**Future:** Expand module-level structure support

### Documentation

- [x] Update README.md with new language features

## Not Planned

The following features are explicitly out of scope:

- **REPL** - interactive mode
- **Debugger support** - DWARF debug info

## Current Status

- ✅ Integers with full arithmetic (+, -, \*, /)
- ✅ Booleans with comparisons (==, !=, <, >, <=, >=) and operations (&&, ||, !)
- ✅ Variables (let bindings with shadowing support)
- ✅ Let-pattern matching with tuples (e.g., `let (a, b) = tuple in ...`)
- ✅ Control flow (if/then/else expressions, including in atom position)
- ✅ Functions with type signatures, calls, recursion, and implicit mutual recursion
- ✅ String literals with escape sequences and concatenation (`++` operator)
- ✅ Lists (linked list implementation with [1, 2, 3] syntax and exact-length pattern matching)
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 9-pass compiler pipeline (Parser → TypeCheck → ANF → RefCount → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ Register allocation with callee-saved registers (X19-X27) for high register pressure
- ✅ Reference counting with free list memory reuse
- ✅ 837 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)
- ✅ Type-directed record field lookup (no ambiguity when multiple record types have same field names)
- ✅ Function return type inference using function registry (enables type inference for let-bound function calls)
- ✅ Record literals require type name: `Point { x = 1, y = 2 }` syntax (no anonymous records)
- ✅ Sum types (enums with payloads, pattern matching with extraction)
- ✅ Boolean printing (both literals and computed expressions print as true/false)
- ✅ Generic functions with type parameters (`def id<T>(x: T): T = x`)
- ✅ Polymorphic lists (`List<T>` with type inference)
- ✅ Lambda expressions with immediate application (`(x: int) => x + 1`)
- ✅ First-class functions (store lambdas in variables, call later)
- ✅ Higher-order functions (pass named functions or lambdas as arguments)
- ✅ Closures (lambdas that capture variables from enclosing scope)

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
- All tests pass
