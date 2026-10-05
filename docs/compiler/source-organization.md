# Compiler Source Organization

The production implementation is under `ocaml/`. Each compiler module has a
matching `.ml` implementation and `.mli` interface. The Dune graph preserves the
original pipeline responsibilities while distinguishing modules that formerly
had the same filename in different F# namespaces.

| Location | Responsibility |
|---|---|
| `ocaml/bin/dark.ml` | Native command-line entry point |
| `ocaml/lib/Program.ml` | CLI commands, options and presentation |
| `ocaml/lib/frontend/interpreter/` | Written syntax, lexer, parser and validation |
| `ocaml/lib/frontend/checking/` | Checked expressions, inference, declarations and diagnostics |
| `ocaml/lib/frontend/` | Written/checked integration and JSON planning |
| `ocaml/lib/passes/preparation/` | Specialization, lifting and preparation |
| `ocaml/lib/passes/hir/` | HIR construction and verification |
| `ocaml/lib/passes/ownership/` | Ownership inference and elaboration |
| `ocaml/lib/passes/anf/` | ANF lowering, SSA transforms and reference counts |
| `ocaml/lib/passes/mir/` | MIR lowering and optimization |
| `ocaml/lib/passes/lir/` | LIR lowering, liveness and register allocation |
| `ocaml/lib/ir/` | IR definitions and printers |
| `ocaml/lib/backend/arm64/` | ARM64 selection, encoding, runtime and ELF/Mach-O output |
| `ocaml/lib/backend/x64/` | Linux x64 selection, encoding and runtime |
| `ocaml/lib/backend/binary/` | Shared binary structures and literal pools |
| `ocaml/lib/driver/` | Compilation contexts, caches, sessions and pipeline orchestration |
| `ocaml/share/` | Unchanged Dark standard library and Unicode tables |
| `ocaml/tests/` | Production unit checks, DSL tooling and suite runner |
| `src/Tests/` | Language and DSL input fixtures |
| `ocaml/validation/` | Additional port equivalence and allocation checks |

`ocaml/inventory.json` records the frozen F# source-to-native-owner mapping.
The reference revision remains in Git history. Migration-only observation
adapters do not form a public compiler API or a runtime bridge.
