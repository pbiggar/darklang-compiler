# Compiler Source Organization

The repository root is the Dune project root. Compiler modules under `src/` have a
matching `.ml` implementation and `.mli` interface. The Dune graph groups modules by pipeline stage and responsibility.

| Location | Responsibility |
|---|---|
| `bin/dark.ml` | Native command-line entry point |
| `src/Program.ml` | CLI commands, options and presentation |
| `src/util/` | Shared Unicode operations, text decoding, file input and float literal formatting |
| `src/packages/` | Package schema decoding, resolution, fetching and response cache |
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
| `stdlib/` | Dark standard library sources and Unicode tables embedded at build time |
| `tools/fuzzer/` | Typed generation, oracle comparison and syntax reduction |
| `tools/process/` | Captured process execution shared by tests and fuzzing |
| `test/` | Production unit checks, DSL tooling and suite runner |
| `test/fixtures/` | Language and DSL input fixtures |
| `test/regression/` | Native allocation, cache, graph and bitset regression checks |
| `test/runtime-execution/` | Explicit native backend execution checks |
| `test/runtime-support/` | Private test-only backend helpers |

`dune runtest` executes additional regression checks; the complete host suite
runs through `./run-tests --ai` after `./build --ai`.

Compiler services use OCaml libraries directly: Yojson for JSON, Digestif for
SHA-256 (its OCaml backend), Mtime for monotonic elapsed time, Uri for URL
resolution, and Uuidm for Mach-O UUIDs. Package transport uses Cohttp with
OCaml TLS and system trust anchors; its persistent response cache uses the
OCaml SQLite library and retains the existing `packages.sqlite3` schema.
Signal exit codes use `Sys.signal_to_int`; test process groups use Spawn.
Native platform metadata comes from the OCaml build configuration. The compiler
and test runner have no repository-owned C stubs. Timing fields store nanoseconds. Shared utilities retain
compiler policies such as Unicode segmentation, BOM decoding and exact float
literal round trips.
AST and IR diagnostics use the standard `Format` layout engine.
