# Dark Compiler - AI Agent Guidelines

## Task tracking

We use 'bd' for task tracking. See WORKFLOW.md.

## Quick Overview

- **Language**: Pure functional F# (no mutable state, no exceptions)
- **Target**: ARM64 native binaries (macOS Mach-O, Linux ELF)
- **Pipeline**: 8 passes from source to executable (see `docs/compiler-passes.md`)
- **testing**: Run `./run-tests --no-build` to run tests.
- **Scripting**: Use `python3` for scripts (not `python`).

## Architecture Overview

```
Source -> Parser -> TypeChecker -> ANF -> MIR -> LIR -> RegAlloc -> CodeGen -> Binary
```

Each pass has its own numbered file (e.g., `1_Parser.fs`, `2_AST_to_ANF.fs`).
See `docs/compiler-passes.md` for detailed pass documentation.

## F# Conventions

Write in F#, with the intention to translate to Darklang later. Thus:

- Use only functional constructs, no imperative ones
- Use `Option`s and `Result`s - don't use success flags or imperative patterns
- Don't use exceptions or `failwith` (or similar) or `exit`. (Use `crash` for impossible conditions`)
- Don't use `find` methods which throw exceptions
- Use the principle "Impossible states should be unrepresentable"
- Don't use dotnet libraries unless you can't avoid them.
- Completely avoid all mutable values, ESPECIALLY global variables"
- Use string interpolation instead of printf-style calls
- If a function we need throws exceptions, create a wrapper that returns `Result`

## Result Handling

The codebase uses standard F# Result extensively. See `docs/result-patterns.md` for comprehensive examples.

## Benchmarking

// The compiled code needs to be fast. DO NOT commit regressions.

Benchmarks are currently broken, allow committing without validating benchmarks.

## Compiler Structure

- Compiler passes start with a numbered prefix in pipeline order
- One compiler pass per file
- The dotnet compiler often hangs - don't use long timeouts

## IR Dumps

Use these CLI flags to dump specific IRs when debugging:

- `--dump-anf` (ANF stages)
- `--dump-mir` (MIR CFG)
- `--dump-lir` (LIR before and after register allocation)
- `-vvv` dumps all IRs

This is extremely useful for understanding what the compiler does - use it extensively.

Examples:

```bash
./dark --dump-anf prog.dark
./dark --dump-mir prog.dark
./dark --dump-lir prog.dark
```

## Comments

- Every file should have a comment at the top explaining what it's for
- Write comments for an experienced senior compiler engineer

## Testing - TDD Approach

**ALWAYS create a failing test first before implementing any feature or fixing any bug.**

1. Write a test that exposes the bug or demonstrates the desired behavior
2. Run the test to confirm it fails
3. Implement the fix/feature
4. Run the test to confirm it passes
5. Add the smallest test that covers the case, in the right place

Focus largely on end-to-end language tests in `src/Tests/e2e/`.

**Things to never do:**

- NEVER disable failing tests
- NEVER change test cases to avoid hitting bugs
- NEVER implement features without a test demonstrating them first

**Running tests:**

```bash
./run-tests                      # Run all tests
./run-tests --filter=PATTERN     # Run matching tests (case-insensitive)
./run-tests --help               # Full usage information
```

Common filter patterns: tuple, record, list, string, float, closure, match, adt, generic, stdlib

## Best Practices

- Keep README.md, TODO.md, and docs/ files updated
- Always fix dotnet compiler or runtime warnings/errors before committing
- For dumb warnings, ask the developer if you want to disable them
- Never make assumptions, fall-backs, or potentially incorrect defaults - use `TODO()` instead

## Critical Conventions

1. **No exceptions** - Use `Result<T, E>` for all error handling
2. **No mutability** - Pure functional code only
3. **Pass files numbered** - e.g., `1_Parser.fs`, `2_AST_to_ANF.fs`
4. **Tests first** - Write/identify failing test before fixing bugs

## Before Making Changes

- Read `docs/adding-features.md` for step-by-step guides
- Check `docs/result-patterns.md` for error handling patterns
- Use F# compiler exhaustiveness warnings to find all locations needing updates

## Documentation References

| Need to...                           | Read...                                         |
| ------------------------------------ | ----------------------------------------------- |
| Understand compiler passes           | `docs/compiler-passes.md`                       |
| Add a new feature                    | `docs/adding-features.md`                       |
| Understand architecture decisions    | `docs/architecture.md`                          |
| Handle errors properly               | `docs/result-patterns.md`                       |
| See design rationale                 | `docs/design-decisions.md`                      |
| Understand generics/monomorphization | `docs/features/generics.md`                     |
| Understand memory management         | `docs/features/reference-counting.md`           |
| Understand tail call optimization    | `docs/features/tail-call-optimization.md`       |
| Understand closures/lambda lifting   | `docs/features/closures.md`                     |
| Understand register allocation       | `docs/features/register-allocation.md`          |
| Understand binary generation         | `docs/features/binary-generation.md`            |
| Understand pattern matching          | `docs/features/pattern-matching.md`             |
| Understand type checking             | `docs/features/type-checking.md`                |
| Understand sum types (ADTs)          | `docs/features/sum-types.md`                    |
| Understand records                   | `docs/features/records.md`                      |
| Understand lists                     | `docs/features/lists.md`                        |
| Understand strings                   | `docs/features/strings.md`                      |
| Understand MIR/LIR IRs               | `docs/features/intermediate-representations.md` |
| Understand ARM64 code generation     | `docs/features/arm64-codegen.md`                |
| Understand stdlib                    | `docs/features/stdlib.md`                       |
| Understand Dict (HAMT)               | `docs/features/dict-hamt.md`                    |

## Key Files

| File                                          | Purpose                          |
| --------------------------------------------- | -------------------------------- |
| `src/DarkCompiler/CompilerLibrary.fs`         | Main compilation orchestration   |
| `src/DarkCompiler/passes/1_Parser.fs`         | Lexing and parsing               |
| `src/DarkCompiler/passes/1.5_TypeChecking.fs` | Type validation                  |
| `src/DarkCompiler/passes/2_AST_to_ANF.fs`     | ANF conversion, monomorphization |
| `src/DarkCompiler/Runtime.fs`                 | Runtime support, builtins        |
| `src/DarkCompiler/Stdlib.fs`                  | Standard library definitions     |

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up. Include a description in the issue.
2. **Run quality gates** (if code changed) - Ensure it build, all tests pass, benchmarks do not regress.
3. **Update issue status** - Close finished work, update in-progress items
4. **Clean up** - Clear stashes
5. **Verify** - All changes committed
6. **Hand off** - Provide context for next session
