# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## Phase 4: Functions (3-4 weeks)

- [ ] Add FunctionDef and Call to AST.fs
- [ ] Parse function definitions with type signatures
- [ ] Parse function calls
- [ ] Update ANF and MIR for multiple functions
- [ ] Add function calling instructions to LIR.fs
- [ ] **MAJOR**: Rewrite 5_RegisterAllocation.fs
  - [ ] Liveness analysis across blocks
  - [ ] Register spilling to stack
  - [ ] Callee-saved register tracking
- [ ] Implement ARM64 calling convention (AAPCS64)
- [ ] Generate function prologue/epilogue
- [ ] Update Runtime.fs with entry point
- [ ] Write E2E tests (functions.e2e)
- [ ] Test recursion, mutual recursion

**Critical**: All function signatures require type annotations

## Phase 5: Strings (1-2 weeks)

### Phase 5a: String Literals Only

- [ ] Add StringLiteral to AST.fs
- [ ] Parse string literals with escape sequences
- [ ] Add string pool to MIR.fs
- [ ] Add .rodata section to Binary_Generation_ELF.fs
- [ ] Add .rodata section to Binary_Generation_MachO.fs
- [ ] Add ADRP and ADD_pcrel to ARM64.fs
- [ ] Implement generatePrintString in Runtime.fs
- [ ] Write E2E tests (strings.e2e - literals only)

### Phase 5b: String Concatenation (Later)

- [ ] Implement simple bump allocator
- [ ] Add Concat operator
- [ ] Add heap string allocation
- [ ] Write E2E tests for concatenation

**Memory Model**: Hybrid (literal pool + simple heap, no GC)

## Phase 6: Floats (1 week)

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
4. [ ] functions.e2e (Phase 4)
5. [ ] strings.e2e (Phase 5)
6. [ ] floats.e2e (Phase 6)

### Pass Tests

- ✅ Type checking tests (51 DSL tests + 8 unit tests)
- [ ] Add liveness analysis tests (Phase 4)
- [ ] Add calling convention tests (Phase 4)

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
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 8-pass compiler pipeline (Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ 430 passing tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)

## Implementation Order

```
✅ Phase 0 → ✅ Phase 1 → ✅ Phase 2 → ✅ Phase 3 → Phase 4 (NEXT)
                                                      ↓
                                               Phase 5 ← → Phase 6
```

**Original Estimate**: 8-11 weeks total
**Completed**: Phases 0-3 (~2-3 weeks of work)
**Remaining**: Phases 4-6 (~6-8 weeks estimated)

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
**Required For**: Better user experience

### 2. Register Allocation (Critical for Functions)

**Status**: ⚠️ Limited - Works for current features

**Current Implementation:**

- Simple greedy allocation: X1-X15 (15 registers)
- No liveness analysis
- No register spilling

**Limitations:**

- **Fails if >15 virtual registers needed**
- No support for caller/callee-saved register conventions
- Located in: `src/DarkCompiler/passes/5_RegisterAllocation.fs:11`

**Impact**: High - **Required for Phase 4 (Functions)**
**Missing:**

- Liveness analysis across blocks
- Register spilling to stack
- Callee-saved register tracking
- Caller-saved register preservation across calls

### 3. Stack Slot Support (Critical for Functions)

**Status**: ⚠️ Defined but Not Implemented

**Current State:**

- Stack slots defined in LIR data structures
- Code generation returns error: "Stack slots not yet supported"
- Located in: `src/DarkCompiler/passes/6_CodeGen.fs:85,104,121,152`

**Impact**: High - **Required for Phase 4 (Functions)**
**Missing:**

- Stack slot code generation
- Stack frame management
- Local variable storage
- Spilled register storage

### 4. Control Flow Graph (Acceptable)

**Status**: ⚠️ Simple Implementation

**Current Implementation:**

- Basic CFG with basic blocks
- Supports if/else (including nested)
- Supports if-expressions in atom position
- Branch, Jump, and Ret terminators working

**Deferred:**

- Advanced CFG optimizations (dead code elimination, etc.)
- Phi node optimization (currently uses simple register assignments)
- Loop constructs (by design - will use recursion instead)

**Impact**: Low - Current implementation sufficient for if/else
**Note**: No loops planned (language will use recursion)

### 5. Cross-Platform Testing (Development Workflow)

**Status**: ⚠️ Partial

**Current State:**

- ✅ macOS Mach-O binary generation working
- ✅ Linux ELF binary generation working
- ❌ Cannot run E2E tests in Docker (generates macOS binaries on Linux host)

**Impact**: Medium - Affects Docker development workflow
**Workaround**: Run tests on macOS host

### 6. Type System (Explicit Annotations Required)

**Status**: ⚠️ Simple - No Type Inference

**Current Implementation:**

- Type checking with explicit annotations
- Top-down type propagation
- Let bindings have optional type annotations

**Limitations:**

- No type inference (beyond simple cases)
- Function signatures **require** explicit type annotations (planned for Phase 4)
- Limited type error messages

**Impact**: Medium - Verbose but clear
**Philosophy**: Explicit over implicit (good for learning compiler)

---

## Summary: What Needs Completion for Phase 4 (Functions)

**Critical (Must Have):**

1. ✅ CFG structure - **DONE** (added in Phase 3)
2. ❌ Register allocation with spilling - **TODO**
3. ❌ Liveness analysis - **TODO**
4. ❌ Stack slot support - **TODO**
5. ❌ Calling convention (AAPCS64) - **TODO**
6. ❌ Function prologue/epilogue generation - **TODO**

**Nice to Have:**

1. Better boolean printing
2. Improved type error messages
3. Docker E2E test support

---
