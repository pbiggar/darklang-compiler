# Architecture Decisions

This document explains the key architectural decisions made in the Darklang compiler and their rationale.

## Pure Functional F#

**Decision:** Write the compiler in pure functional F# without mutable state or imperative features.

**Rationale:**
- **Future Portability**: The compiler is designed to eventually be rewritten in Darklang itself. Using pure functional patterns makes this transition easier.
- **Easier to Reason About**: Pure functions with no side effects are easier to understand and test.
- **Composability**: Functional transformations compose naturally in a pipeline architecture.

**Implementation Patterns:**

### Fresh Name Generation

Instead of using mutable counters, we use the "generator pattern":

```fsharp
type VarGen = VarGen of int

let freshVar (VarGen n) : TempId * VarGen =
    (TempId n, VarGen (n + 1))
```

Each function that needs a fresh name:
1. Takes a generator as input
2. Returns the fresh name AND an updated generator
3. Threading the generator through the computation maintains the counter state functionally

This pattern is used for:
- `VarGen` in ANF (temporary variable names)
- `RegGen` in MIR (virtual register names)

## Multi-Stage IR Pipeline

**Decision:** Use multiple intermediate representations (AST → ANF → MIR → LIR → ARM64).

**Rationale:**

### Separation of Concerns
Each IR focuses on a specific aspect:
- **AST**: Represents program structure
- **ANF**: Makes evaluation order explicit
- **MIR**: Platform-independent three-address code
- **LIR**: ARM64-specific instruction selection
- **ARM64**: Concrete machine instructions

### Testability
Each transformation can be tested in isolation with clear inputs and outputs.

### Modularity
Adding a new target architecture only requires implementing the LIR → Target stage (and possibly a new LIR variant).

### Optimization Opportunities
Each IR level provides opportunities for different optimizations:
- High-level optimizations on ANF
- Platform-independent optimizations on MIR
- Architecture-specific optimizations on LIR

## A-Normal Form (ANF)

**Decision:** Use ANF as the first intermediate representation after parsing.

**Rationale:**

### Explicit Evaluation Order
ANF makes the order of operations completely explicit:
```fsharp
// Before ANF (nested):
2 + (3 * 4)

// After ANF (flat):
let tmp0 = 3 * 4
let tmp1 = 2 + tmp0
return tmp1
```

### Simplifies Later Passes
With ANF, all later passes can assume:
- No nested expressions
- All operands are simple (variables or literals)
- Evaluation order is obvious

### Standard Compiler Technique
ANF is a well-studied intermediate form used in many functional language compilers (OCaml, GHC Core).

## Three-Address Code (MIR)

**Decision:** Use three-address code for the mid-level IR.

**Rationale:**

### Platform Independence
MIR uses unlimited virtual registers, making it independent of any specific CPU's register set.

### Simplicity
Each instruction has at most two operands and one destination, making analysis straightforward.

### Industry Standard
Three-address code is used in many production compilers (LLVM, GCC's RTL).

## Direct Mach-O Generation

**Decision:** Generate Mach-O binaries directly without using an assembler or linker.

**Rationale:**

### Complete Control
We control every byte of the output, understanding exactly what we're producing.

### Educational Value
Implementing the full pipeline from source to executable provides deep understanding of systems programming.

### No External Dependencies
The compiler is self-contained; it doesn't rely on Xcode command-line tools or other external programs.

### Simplified Debugging
When something goes wrong, we can trace through our own code rather than debugging linker behavior.

**Trade-offs:**
- More complex than calling `as` and `ld`
- Must manually implement binary format details
- Need to update when macOS changes Mach-O requirements

## Simple Register Allocation

**Decision:** Use a simple greedy register allocator without spilling.

**Rationale:**

### Good Enough for Now
For simple arithmetic expressions, we have plenty of registers (X0-X15).

### Clear Limitations
The allocator fails with a clear error if we run out of registers, rather than producing incorrect code.

### Future Extension Point
We intentionally left register spilling as a future enhancement when needed.

**Current Algorithm:**
1. Collect all virtual registers
2. Sort them
3. Assign physical registers in order
4. Fail if we need more than 16 registers

## Test-First Development

**Decision:** Write tests before or alongside implementation for each phase.

**Rationale:**

### Regression Prevention
Tests catch bugs when refactoring or adding features.

### Documentation
Tests serve as executable documentation showing how each component works.

### Confidence
Comprehensive tests give confidence when making changes.

**Test Strategy:**
1. **Unit Tests**: Test individual functions and transformations
2. **Phase Tests**: Test complete phases (e.g., full ANF transformation)
3. **Integration Tests**: Test multiple phases together
4. **End-to-End Tests**: Compile complete programs and verify execution

## Small, Frequent Commits

**Decision:** Make small, self-contained commits throughout development.

**Rationale:**

### Clear History
Each commit represents a single logical change.

### Easier Review
Small commits are easier to review and understand.

### Better Debugging
`git bisect` becomes more effective with granular commits.

### Documentation
Commit messages explain why changes were made.

## Minimal Initial Feature Set

**Decision:** Start with only integer arithmetic (+, -, *, /).

**Rationale:**

### Complete Pipeline First
Build the entire pipeline (parse → binary) with minimal features before adding complexity.

### Solid Foundation
Ensure the basic architecture works before adding:
- Variables
- Functions
- Control flow
- Type system

### Iterative Development
Add features incrementally, testing thoroughly at each step.

## Project Structure

**Decision:** Organize code by compiler phase, not by type of file.

```
src/DarkCompiler/
├── AST.fs          # Types + Parser
├── Parser.fs
├── ANF.fs          # Types + Transformation
├── MIR.fs          # Types + Transformation
├── LIR.fs          # Types + Allocation
├── ARM64.fs        # Types + Encoding
├── CodeGen.fs      # LIR → ARM64
└── Binary.fs       # Mach-O generation
```

**Rationale:**

### Phase Cohesion
Each file contains everything related to one compiler phase.

### Dependency Order
F# requires files to be ordered by dependency. This structure naturally follows the compilation pipeline.

### Easy Navigation
Finding code related to a specific phase is straightforward.

## Centralized Build Output

**Decision:** Use root-level `obj/` and `bin/` directories instead of per-project directories.

**Rationale:**

### Cleaner Repository
Build artifacts are in one place, not scattered through source directories.

### Simpler `.gitignore`
Single patterns ignore all build output.

### Easier Cleanup
`rm -rf obj bin` cleans all build artifacts.

**Implementation:**
Use `Directory.Build.props` in `src/` to configure MSBuild for all projects.

## No Over-Engineering

**Decision:** Keep implementations simple and direct; avoid premature abstraction.

**Rationale:**

### YAGNI (You Aren't Gonna Need It)
Don't add features or abstractions until they're actually needed.

### Easier to Understand
Simple, straightforward code is easier for others (and future you) to understand.

### Easy to Extend
Simple code is easier to refactor when you do need more features.

**Examples:**
- Register allocator doesn't handle spilling (not needed yet)
- No optimization passes (premature)
- Direct Mach-O generation (only one target)
- Simple immediate value handling (complex cases handled later)

## Error Handling Strategy

**Current:** Use F# exceptions (`failwith`) for error conditions.

**Rationale:**

### Simplicity
For an educational/prototype compiler, exceptions are straightforward.

### Clear Failures
The compiler fails fast with clear error messages.

**Future Direction:**
As the compiler matures, we'll likely move to:
- Result types for expected errors (parse errors, type errors)
- Exceptions only for unexpected internal errors
- Structured error messages with source locations

## Why F#?

**Decision:** Use F# as the implementation language.

**Rationale:**

### Functional-First
F# is designed for functional programming, making pure functional patterns natural.

### .NET Ecosystem
Access to .NET libraries and tools when needed.

### ML Family
F# is similar to OCaml (which Darklang is related to), making patterns transferable.

### Type System
Strong static typing catches many errors at compile time.

### Pattern Matching
Discriminated unions and pattern matching are perfect for compiler data structures.

## Future Architectural Decisions

As the compiler evolves, we'll need to make decisions about:

1. **Optimization**: Where and how to optimize?
2. **Type System**: How to represent and check types?
3. **Module System**: How to organize larger programs?
4. **Standard Library**: What to provide built-in?
5. **Error Recovery**: How to continue after errors?
6. **Incremental Compilation**: How to avoid recompiling everything?
7. **Debugging Support**: How to generate debug information?
8. **Other Targets**: How to support x86-64, WASM, etc.?

These decisions will be documented as they're made.
