# Dark Compiler - AI Agent Guidelines

## Quick Overview

- **Language**: Pure functional F# (no mutable state, no exceptions)
- **Target**: native binaries (macOS Mach-O, Linux ELF)
- **Pipeline**: 8 passes from source to executable (see `docs/compiler-passes.md`)
- **testing**: Run `./run-tests --ai` to run tests. Tests are very fast, don't use filters, just run the whole test suite.
- **Scripting**: Use `python3` for scripts (not `python`).

## Architecture Overview

```
Source -> AST -> ANF -> MIR -> LIR -> RegAlloc -> CodeGen -> Encode -> Binary
```

Passes 1-5 are shared across targets and live under `passes/` with a
numbered prefix (e.g., `1_Parser.fs`). Passes 6-8 are per-architecture
and live under `passes/arm64/` or `passes/x64/`. The backend is
selected at runtime by `Platform.detectArch ()`.

See `docs/architecture.md` for the design rationale and
`docs/compiler-passes.md` for per-pass details.

## F# Conventions

Write in F#, with the intention to translate to Darklang later. Thus:

- Use only functional constructs, no imperative ones
- Use `Option`s and `Result`s - don't use success flags or imperative patterns
- Don't use exceptions or `exit`
- Don't use `find` methods which throw exceptions
- Use the principle "Impossible states should be unrepresentable"
- Don't use dotnet libraries unless you can't avoid them.
- Completely avoid all mutable values, ESPECIALLY global variables"
- Use string interpolation instead of printf-style calls
- If a function we need throws exceptions, create a wrapper that returns `Result`
- NEVER EVER assume a default value, type, etc, when we don't know something. Instead, use `Crash.crash` to crash and document the error.

## Result Handling

The codebase uses standard F# Result extensively. See `docs/result-patterns.md` for how to handle this.

## Benchmarking

// The compiled code needs to be fast. DO NOT commit regressions.

Benchmarks are effective (ignore quicksort error). There is a `--quick` mode to speed up development, but the full benchmarks should be used to validate performance.

After running full benchmarks, always report the performance ratio from the table header in `RESULTS.md` (for example, "Performance ratio: X.XX") in your response.

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

## Executable / script conventions

- Always use command-line flags, and never use ENV_VARS

## Testing - TDD Approach

**ALWAYS create a failing test first before implementing any feature or fixing any bug.**

1. Write a test that exposes the bug or demonstrates the desired behavior
2. Run the test to confirm it fails
3. Implement the fix/feature
4. Run the test to confirm it passes
5. Add the smallest test that covers the case, in the right place
6. Exception: don't create tests for the test harness, or any test that takes more than 50ms

Focus near-exclusively on end-to-end language tests in `src/Tests/e2e/`. If writing a different test, you must have an EXCELLENT reason why it can't be accomplished as an E2E test.

**Never:**

- Disable failing tests
- Change test cases to avoid hitting bugs
- Use workarounds to allow failing tests to pass without fixing the issue.
- Implement features without a test demonstrating them first
- NEVER add tests that compile the entire stdlib

**Running tests:**

```bash
./run-tests --ai                 # Run all tests with AI-friendly progress output
./run-tests --ai --filter=PATTERN  # Run matching tests (case-insensitive)
./run-tests --quiet              # Much less output
./run-tests --help               # Full usage information
```

Common filter patterns: tuple, record, list, string, float, closure, match, adt, generic, stdlib

## Devcontainer

Build and test commands require .NET 10, which lives in the Docker
devcontainer. Use `scripts/run-in-container` — it auto-detects whether
you're inside the container or on the host and dispatches correctly:

```bash
scripts/run-in-container ./run-tests --ai
scripts/run-in-container ./dark -r -e "2 + 3"
scripts/run-in-container dotnet build --verbosity quiet
```

If the container isn't running: `docker compose up -d` in the repo root.

See `docs/docker.md` for devcontainer details, worktree setup, and
Codex/Claude integration.

**Do not install tooling on the host.** If something is needed persistently,
add it to the Dockerfile.

## Best Practices

- Keep README.md, and docs/ files updated
- Always fix dotnet compiler or runtime warnings/errors before committing
- Never make assumptions, fall-backs, or potentially incorrect defaults - use `Crash.TODO()` instead

## Critical Conventions

1. **No exceptions** - Use `Result<T, E>` for all error handling
2. **No mutability** - Pure functional code only
3. **Pass files numbered** - e.g., `1_Parser.fs`, `2_AST_to_ANF.fs`
4. **Tests first** - Write/identify failing test before fixing bugs
5. **No sentinels in compiler passes** - Model special states with explicit discriminated unions/enums and typed cases
6. **No string sentinels anywhere** - Never encode control flow or unknown states in magic strings (for example `"__some_internal_state"`)

## Before Making Changes

- Read `docs/adding-features.md` for step-by-step guides
- Check `docs/result-patterns.md` for error handling patterns
- Use F# compiler exhaustiveness warnings to find all locations needing updates

## Documentation References

| Need to...                           | Read...                                         |
| ------------------------------------ | ----------------------------------------------- |
| Get started with the CLI             | `docs/quick-start.md`                           |
| Work in the devcontainer             | `docs/docker.md`                                |
| Understand compiler passes           | `docs/compiler-passes.md`                       |
| Understand architecture decisions    | `docs/architecture.md`                          |
| Add a new feature                    | `docs/adding-features.md`                       |
| Handle errors properly               | `docs/result-patterns.md`                       |
| See design rationale                 | `docs/design-decisions.md`                      |
| Understand x64 refcounting status    | `docs/x64-refcounting.md`                       |
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

| File                                                | Purpose                              |
| ---------------------------------------------------- | ------------------------------------ |
| `src/DarkCompiler/CompilerLibrary.fs`                | Main compilation orchestration       |
| `src/DarkCompiler/Platform.fs`                       | OS/Arch detection + syscall tables   |
| `src/DarkCompiler/passes/1_Parser.fs`                | Lexing and parsing                   |
| `src/DarkCompiler/passes/1.5_TypeChecking.fs`        | Type validation                      |
| `src/DarkCompiler/passes/2_AST_to_ANF.fs`            | ANF conversion, monomorphization     |
| `src/DarkCompiler/passes/5_RegisterAllocation.fs`    | Register allocation (arch-aware)     |
| `src/DarkCompiler/passes/arm64/6_CodeGen.fs`         | LIR → ARM64 instruction selection    |
| `src/DarkCompiler/passes/x64/6_CodeGen.fs`           | LIR → x86_64 instruction selection   |
| `src/DarkCompiler/ARM64.fs`, `X86_64.fs`             | ISA instruction + register types     |
| `src/DarkCompiler/Runtime.fs`                        | Runtime support, builtins            |
| `src/DarkCompiler/Stdlib.fs`                         | Standard library definitions         |

## Git Workflow

- **Start by rebasing** - Begin new work by running `git rebase main` to ensure you're working on top of the latest main.
- **Never push** - Do not run `git push`. The user handles pushing.
- **"main" means the local branch** - References to `main` mean the local `main` branch, not `origin/main`.
- **Landing** - NEVER EVER land without explicit user permission. Before asking to land, commit any uncommitted changes and provide a detailed summary of changes, explicitly answering: "Did all tests pass?" and "Did benchmarks complete with no failures?" with an emoji tick or X. Only land after the user grants permission, and only if all tests and full benchmarks are run and pass with no regressions and no test failures. When the user says "land", first commit any uncommitted changes, then run `./scripts/land-on-main.sh`. This script rebases onto main, runs tests and benchmarks, then fast-forward merges into main. Do NOT push to remote - just run the land script.
