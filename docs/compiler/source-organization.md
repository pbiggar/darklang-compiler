# Compiler Source Organization

The repository root is the Dune project root. Compiler modules under `src/` have a
matching `.ml` implementation and `.mli` interface. The Dune graph preserves the
original pipeline responsibilities while distinguishing modules that formerly
had the same filename in different F# namespaces.

| Location | Responsibility |
|---|---|
| `bin/dark.ml` | Native command-line entry point |
| `src/Program.ml` | CLI commands, options and presentation |
| `src/frontend/interpreter/` | Written syntax, lexer, parser and validation |
| `src/frontend/checking/` | Checked expressions, inference, declarations and diagnostics |
| `src/frontend/` | Written/checked integration and JSON planning |
| `src/passes/preparation/` | Specialization, lifting and preparation |
| `src/passes/hir/` | HIR construction and verification |
| `src/passes/ownership/` | Ownership inference and elaboration |
| `src/passes/anf/` | ANF lowering, SSA transforms and reference counts |
| `src/passes/mir/` | MIR lowering and optimization |
| `src/passes/lir/` | LIR lowering, liveness and register allocation |
| `src/ir/` | IR definitions and printers |
| `src/backend/arm64/` | ARM64 selection, encoding, runtime and ELF/Mach-O output |
| `src/backend/x64/` | Linux x64 selection, encoding and runtime |
| `src/backend/binary/` | Shared binary structures and literal pools |
| `src/driver/` | Compilation contexts, caches, sessions and pipeline orchestration |
| `share/` | Installed Dark standard library sources and Unicode tables |
| `test/` | Production unit checks, DSL tooling and suite runner |
| `test/fixtures/` | Language and DSL input fixtures |
| `test/regression/` | Native allocation, cache, graph and bitset regression checks |
| `test/runtime-execution/` | Explicit native backend execution checks |
| `test/runtime-support/` | Private test-only backend helpers |

The completed F# migration's observation harness, source inventory and
comparison scripts remain in Git history. They are no longer part of the
build graph. `dune runtest` executes the native regression checks; the complete
production host suite runs through `./run-tests --ai` after `./build --ai`.
