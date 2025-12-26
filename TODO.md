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

## Code Quality Issues

The following are known simplifications or potential issues in the compiler code.
**Principle**: The compiler should never guess - always error if something is unknown.

### HIGH Priority (Can cause crashes or wrong results)

(No current high-priority issues)

### MEDIUM Priority (Incomplete features)

#### 2. Callee-saved registers not implemented
**Files:** `src/DarkCompiler/passes/5_RegisterAllocation.fs` (line 111), `6_CodeGen.fs` (lines 147, 173)
**Issue:** `calleeSavedRegs` is empty list, X19-X28 not supported in LIR.PhysReg
**Impact:** Suboptimal register allocation, more spilling than necessary

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

#### 6. MachO UUID hardcoded to zeros
**File:** `src/DarkCompiler/passes/8_Binary_Generation_MachO.fs` (line 468)
**Issue:** All binaries have identical UUID (all zeros)
**Impact:** Cosmetic - doesn't affect functionality

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
- ✅ Lists (linked list implementation with [1, 2, 3] syntax and exact-length pattern matching)
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 8-pass compiler pipeline (Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ 711 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)
- ✅ Type-directed record field lookup (no ambiguity when multiple record types have same field names)
- ✅ Function return type inference using function registry (enables type inference for let-bound function calls)

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
✅ All 709 tests pass with ref count headers

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
