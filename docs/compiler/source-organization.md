# Compiler Source Organization

The repository root is the Dune project root. Compiler modules under `lib/` have a
matching `.ml` implementation and `.mli` interface. The Dune graph preserves the
original pipeline responsibilities while distinguishing modules that formerly
had the same filename in different F# namespaces.

| Location | Responsibility |
|---|---|
| `bin/dark.ml` | Native command-line entry point |
| `lib/Program.ml` | CLI commands, options and presentation |
| `lib/frontend/interpreter/` | Written syntax, lexer, parser and validation |
| `lib/frontend/checking/` | Checked expressions, inference, declarations and diagnostics |
| `lib/frontend/` | Written/checked integration and JSON planning |
| `lib/passes/preparation/` | Specialization, lifting and preparation |
| `lib/passes/hir/` | HIR construction and verification |
| `lib/passes/ownership/` | Ownership inference and elaboration |
| `lib/passes/anf/` | ANF lowering, SSA transforms and reference counts |
| `lib/passes/mir/` | MIR lowering and optimization |
| `lib/passes/lir/` | LIR lowering, liveness and register allocation |
| `lib/ir/` | IR definitions and printers |
| `lib/backend/arm64/` | ARM64 selection, encoding, runtime and ELF/Mach-O output |
| `lib/backend/x64/` | Linux x64 selection, encoding and runtime |
| `lib/backend/binary/` | Shared binary structures and literal pools |
| `lib/driver/` | Compilation contexts, caches, sessions and pipeline orchestration |
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
