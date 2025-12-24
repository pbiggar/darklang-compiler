# TODO - Compiler Implementation Plan

This TODO reflects the approved implementation plan for completing the Dark compiler.
See `/home/paulbiggar/.claude/plans/lovely-swinging-crab.md` for detailed design.

## Phase 0: Type System Foundation (2-3 days)

- [ ] Add Type definitions to AST.fs
- [ ] Create new pass: 1.5_TypeChecking.fs
- [ ] Implement simple top-down type checker
- [ ] Add type annotations to ANF.fs and MIR.fs
- [ ] Write unit tests for type checking
- [ ] Write E2E tests for type errors
- [ ] Update DarkCompiler.fsproj with new pass

**Design**: Explicit type annotations with simple checking

- Function params/return types REQUIRED
- Let bindings have optional type annotations
- No ambiguity - top-down checking only

## Phase 1: Variables (Let Bindings) (2-3 days)

- [ ] Add Let and Var nodes to AST.fs
- [ ] Add tokens to 1_Parser.fs (TLet, TIn, TIdent)
- [ ] Parse let expressions: `let IDENT = expr in expr`
- [ ] Update 2_AST_to_ANF.fs with variable environment
- [ ] Handle shadowing correctly
- [ ] Write E2E tests (variables.e2e)
- [ ] Test edge cases: shadowing, undefined vars

## Phase 2: Booleans & Comparisons (3-4 days)

- [ ] Add BoolLiteral and comparison operators to AST.fs
- [ ] Add boolean tokens to 1_Parser.fs
- [ ] Update operator precedence
- [ ] Add boolean types to ANF, MIR, LIR
- [ ] Add ARM64 instructions: CMP, CSET, AND, ORR, MVN
- [ ] Implement generatePrintBool in Runtime.fs
- [ ] Update CodeGen for comparisons (CMP + CSET)
- [ ] Write E2E tests (booleans.e2e)
- [ ] Test edge cases: comparison chaining

## Phase 3: Control Flow (if/else only) (1 week)

- [ ] Add If and Block nodes to AST.fs
- [ ] Parse if/then/else expressions
- [ ] **REDESIGN MIR.fs** with CFG and BasicBlocks
- [ ] Update 3_ANF_to_MIR.fs to build CFG
- [ ] Add branch instructions to ARM64.fs
- [ ] Implement two-pass branch encoding in 7_ARM64_Encoding.fs
- [ ] Update type checker for if expressions
- [ ] Write E2E tests (control_flow.e2e)
- [ ] Test recursion examples (factorial, sum)

**Note**: No while loops - iteration via recursion

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

### E2E Test Files (Priority Order)

1. [ ] variables.e2e (Phase 1)
2. [ ] booleans.e2e (Phase 2)
3. [ ] control_flow.e2e (Phase 3)
4. [ ] functions.e2e (Phase 4)
5. [ ] strings.e2e (Phase 5)
6. [ ] floats.e2e (Phase 6)

### Pass Tests

- [ ] Add type checking tests
- [ ] Add CFG construction tests (Phase 3)
- [ ] Add liveness analysis tests (Phase 4)
- [ ] Add calling convention tests (Phase 4)

## Code Quality

### Documentation

- [ ] Update README.md with new language features
- [ ] Document type checking algorithm
- [ ] Document CFG structure
- [ ] Document ARM64 calling convention
- [ ] Document string memory model

### Refactoring

- [ ] Clean up TODO.md after each phase
- [ ] Update architecture docs
- [ ] Fix compiler warnings

## Current Status

- ✅ Integers with full arithmetic
- ✅ 8-pass compiler pipeline
- ✅ 216 passing E2E tests
- ✅ Cross-platform (Linux ELF, macOS Mach-O)

## Implementation Order

```
Phase 0 → Phase 1 → Phase 2 → Phase 3 → Phase 4
                                           ↓
                                Phase 5 ← → Phase 6
```

**Estimated Total Time**: 8-11 weeks
**Minimal Viable (Phases 0-4)**: 5-7 weeks
