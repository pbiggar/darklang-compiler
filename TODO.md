# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## ✅ Phase 4: Functions (COMPLETE)

- [x] Add FunctionDef and Call to AST.fs
- [x] Parse function definitions with type signatures
- [x] Parse function calls
- [x] Update ANF and MIR for multiple functions
- [x] Add function calling instructions to LIR.fs
- [x] Register allocation with caller-saved registers (X1-X10)
- [x] Register spilling to stack (STUR/LDUR)
- [x] Implement ARM64 calling convention (AAPCS64)
- [x] Generate function prologue/epilogue
- [x] SaveRegs/RestoreRegs for caller-save around calls
- [x] Update Runtime.fs with entry point
- [x] Write E2E tests (functions.e2e - 30 tests)
- [x] Test recursion (factorial, fib, gcd, power, ack, sumTo, countdown, isEven)

**All function signatures require type annotations**

## ✅ Phase 5a: String Literals (COMPLETE)

- [x] Add StringLiteral to AST.fs
- [x] Parse string literals with escape sequences (\n, \t, \", \\, \r, \0, \xNN)
- [x] Add string pool to MIR.fs
- [x] Add __cstring section to Binary_Generation_MachO.fs
- [x] Add string data to Binary_Generation_ELF.fs
- [x] Add ADRP and ADD_label to ARM64.fs
- [x] Implement generatePrintString in Runtime.fs
- [x] Write E2E tests (strings.e2e - 14 tests)

### Phase 5b: String Concatenation (Later)

- [ ] Implement simple bump allocator
- [ ] Add Concat operator
- [ ] Add heap string allocation
- [ ] Write E2E tests for concatenation

**Memory Model**: Hybrid (literal pool + simple heap, no GC)

## Phase 6: Floats (NEXT)

- [ ] Add FloatLiteral to AST.fs
- [ ] Parse float literals (decimal, scientific notation)
- [ ] Add float types to ANF, MIR, LIR
- [ ] Add floating-point registers (D0-D31) to ARM64.fs
- [ ] Add FP instructions: FMOV, FADD, FSUB, FMUL, FDIV, FCMP
- [ ] Update 5_RegisterAllocation.fs for FP registers
- [ ] Implement generatePrintFloat in Runtime.fs
- [ ] Write E2E tests (floats.e2e)

## Testing Strategy

### E2E Test Files

1. ✅ variables.e2e (127 tests - Phase 1)
2. ✅ booleans.e2e (tests complete - Phase 2)
3. ✅ Control flow tested in existing E2E tests (Phase 3)
4. ✅ functions.e2e (30 tests - Phase 4)
5. [ ] strings.e2e (Phase 5)
6. [ ] floats.e2e (Phase 6)

### Pass Tests

- ✅ Type checking tests (51 DSL tests + 8 unit tests)
- ✅ MIR→LIR tests (7 tests)
- ✅ ARM64 encoding tests (13 tests)

## Code Quality

### Documentation

- [ ] Update README.md with new language features
- ✅ Document type checking (in TypeChecking.fs comments)
- [ ] Document ARM64 calling convention (Phase 4)
- [ ] Document string memory model (Phase 5)

### Test Suite Quality

- ✅ Created type-checking DSL test format (.typecheck files)
- ✅ Replaced all failwith with Result<> in tests
- ✅ Replaced printf/printfn with string interpolation
- ✅ Fixed FIXMEs in test suite (explicit parentheses, informative notes)

## Current Status

- ✅ Integers with full arithmetic (+, -, \*, /)
- ✅ Booleans with comparisons (==, !=, <, >, <=, >=) and operations (&&, ||, !)
- ✅ Variables (let bindings with shadowing support)
- ✅ Control flow (if/then/else expressions, including in atom position)
- ✅ Functions with type signatures, calls, and recursion
- ✅ String literals with escape sequences
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 8-pass compiler pipeline (Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ 471 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)

## Implementation Order

```
✅ Phase 0 → ✅ Phase 1 → ✅ Phase 2 → ✅ Phase 3 → ✅ Phase 4 → ✅ Phase 5a
                                                                    ↓
                                                             Phase 6 (NEXT)
                                                                    ↓
                                                              Phase 5b
```

**Original Estimate**: 8-11 weeks total
**Completed**: Phases 0-4, 5a
**Remaining**: Phase 6, Phase 5b (optional)

---

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

### 2. Register Allocation

**Status**: ✅ Working

**Current Implementation:**

- Uses X1-X10 for virtual registers (10 registers)
- X0 reserved for return values
- Caller-saved registers preserved with SaveRegs/RestoreRegs
- Stack spilling via STUR/LDUR for overflow

**Limitations:**

- No liveness analysis (greedy allocation)
- Spilling is simple (one slot per spilled vreg)

### 3. Control Flow Graph

**Status**: ✅ Working

**Current Implementation:**

- Basic CFG with basic blocks
- Supports if/else (including nested)
- Supports if-expressions in atom position
- Branch, Jump, and Ret terminators working
- Epilogue labels for proper return from multiple exit points

**Deferred:**

- Advanced CFG optimizations (dead code elimination, etc.)
- Loop constructs (by design - will use recursion instead)

### 4. Cross-Platform Testing

**Status**: ⚠️ Partial

**Current State:**

- ✅ macOS Mach-O binary generation working
- ✅ Linux ELF binary generation working
- ❌ Cannot run E2E tests in Docker (generates macOS binaries on Linux host)

**Impact**: Medium - Affects Docker development workflow
**Workaround**: Run tests on macOS host

---
