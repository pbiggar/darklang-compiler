# Dark Compiler - Claude Code Context

This is the Darklang compiler, written in F# with the intention to eventually translate to Darklang itself.

## Quick Overview

- **Language**: Pure functional F# (no mutable state, no exceptions)
- **Target**: ARM64 native binaries (macOS Mach-O, Linux ELF)
- **Pipeline**: 8 passes from source to executable (see `docs/compiler-passes.md`)

## Key Documentation

| Topic | File |
|-------|------|
| Coding conventions | `claude.md` |
| Compiler passes | `docs/compiler-passes.md` |
| Adding features | `docs/adding-features.md` |
| Architecture rationale | `docs/architecture.md` |
| Result handling patterns | `docs/result-patterns.md` |
| Design decisions | `docs/design-decisions.md` |

## Feature Documentation

| Feature | File |
|---------|------|
| Generics & monomorphization | `docs/features/generics.md` |
| Reference counting memory | `docs/features/reference-counting.md` |
| Tail call optimization | `docs/features/tail-call-optimization.md` |
| Closures & lambda lifting | `docs/features/closures.md` |
| Register allocation | `docs/features/register-allocation.md` |
| Binary generation (Mach-O/ELF) | `docs/features/binary-generation.md` |

## Critical Conventions

1. **No exceptions** - Use `Result<T, E>` for all error handling
2. **No mutability** - Pure functional code only
3. **Pass files numbered** - e.g., `1_Parser.fs`, `2_AST_to_ANF.fs`
4. **Tests first** - Write/identify failing test before fixing bugs

## Before Making Changes

- Read `docs/adding-features.md` for step-by-step guides
- Check `docs/result-patterns.md` for error handling patterns
- Use F# compiler exhaustiveness warnings to find all locations needing updates
