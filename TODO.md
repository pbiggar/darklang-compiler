# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## ✅ Completed Phases

### Phase 0: Type System Foundation ✅
- ✅ Type definitions in AST.fs
- ✅ 1.5_TypeChecking.fs pass implemented
- ✅ Simple top-down type checker working
- ✅ Type annotations in ANF.fs and MIR.fs
- ✅ 51 DSL tests + 8 unit tests for type checking
- ✅ Integrated into compiler pipeline

### Phase 1: Variables (Let Bindings) ✅
- ✅ Let and Var nodes in AST.fs
- ✅ Parser supports let expressions
- ✅ Variable environment in AST_to_ANF.fs
- ✅ Shadowing handled correctly
- ✅ 127 E2E tests in variables.e2e
- ✅ Edge cases tested: shadowing, nested lets, complex expressions

### Phase 2: Booleans & Comparisons ✅
- ✅ BoolLiteral and all comparison operators in AST
- ✅ Boolean tokens and operator precedence in Parser
- ✅ Boolean types throughout ANF, MIR, LIR
- ✅ ARM64 instructions: CMP, CSET, AND, ORR, MVN
- ✅ generatePrintBool in Runtime.fs
- ✅ Comparison code generation (CMP + CSET)
- ✅ E2E tests in booleans.e2e
- ✅ Comparisons (==, !=, <, >, <=, >=) working
- ✅ Boolean operations (&&, ||, !) working

### Phase 3: Control Flow (if/else) ✅
- ✅ If expressions in AST.fs
- ✅ Parser supports if/then/else
- ✅ Basic control flow working
- ✅ Type checker validates branch types match
- ✅ If expressions tested in variables.e2e and booleans.e2e

**Note**: CFG redesign deferred - current simple implementation works for if/else

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

- ✅ Integers with full arithmetic (+, -, *, /)
- ✅ Booleans with comparisons (==, !=, <, >, <=, >=) and operations (&&, ||, !)
- ✅ Variables (let bindings with shadowing support)
- ✅ Control flow (if/then/else expressions)
- ✅ Type checking (51 DSL tests + 8 unit tests)
- ✅ 8-pass compiler pipeline (Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary)
- ✅ 410 passing tests (127 variables E2E, booleans E2E, 51 type checking DSL, 20 unit tests, 13 ARM64 encoding, integers E2E)
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
