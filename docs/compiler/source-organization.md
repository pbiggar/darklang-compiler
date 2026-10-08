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
| `StdLib/`, `packages/` | Dark library packages and compiler support embedded at build time |
| `tools/differential-testing/` | Typed generation, oracle comparison and syntax reduction |
| `tools/process/` | Captured process execution shared by tests and differential testing |
| `test/` | Production unit checks, DSL tooling and suite runner |
| `test/fixtures/` | Language and DSL input fixtures |
| `test/regression/` | Native allocation, cache, graph and bitset regression checks |
| `test/runtime-execution/` | Explicit native backend execution checks |
| `test/runtime-support/` | Private test-only backend helpers |

`dune runtest` executes additional regression checks; the complete host suite
runs through `./run-tests --ai` after `./build --ai`.

## Standard-library layout

`StdLib/` implicitly represents the `Darklang.Stdlib` package prefix. For
example, `Darklang.Stdlib.Cli.UI.Colors` lives in `StdLib/Cli/UI/Colors.dark`.
`Builtin` is the standard-library bridge exception: it lives at
`StdLib/Builtin.dark` and retains its interpreter `Builtin` namespace.
Other packages keep their full names under `packages/`, such as
`packages/Darklang/LanguageTools/ProgramTypes.dark`. The few `module Stdlib.*`
declarations also use the implicit `Darklang` owner. Placement does not change
language names.

| Repository path | Contents |
|---|---|
| `StdLib/` | Darklang.Stdlib packages and their implementation helpers |
| `StdLib/Root.dark`, `StdLib/Print.dark` | Functions directly in Darklang.Stdlib |
| `StdLib/__Types.dark`, `StdLib/__Hash.dark` | Compiler root type representation and hashing helpers |
| `packages/Darklang/LanguageTools/` | Program types, runtime types and package-manager APIs |
| `packages/Darklang/PrettyPrinter/` | Runtime type and error rendering |
| `packages/Darklang/SCM/` | Branch identity |
| `StdLib/Builtin.dark` | Compiler bridges for interpreter builtins; retains the `Builtin` namespace |
| `StdLib/String/__Unicode.dark`, `StdLib/String/__Unicode/` | Compiler Unicode operations and generated data |

A package implemented in several files keeps its main file at the package
path and its fragments in the matching directory. For example,
`StdLib/List.dark` contains the public List API, while
`StdLib/List/__SkewList.dark` and `__ListArray.dark` extend that same package.
`StdLib/List/SortByComparatorHelpers.dark` is a genuine nested interpreter
module, so it retains a normal filename.

Use `__` at the start of a filename for compiler-only support or private
implementation fragments, including generated tables. This is a source
organization convention; filename prefixes do not enforce language visibility.
Compiler-only module declarations also use `__` names, such as
`Darklang.Stdlib.__Network` and `Darklang.Stdlib.String.__Unicode`.
Private declarations require `--allow-internal`; public code cannot access them
through function calls, type annotations, record literals or enum constructors.
Fragments of a public package keep that package's declaration name.
Keep interpreter packages and public fragments unprefixed,
including nested modules extracted from an upstream file. The
[complete source inventory](library-sources.md) lists all 209 files, their
modules, and interpreter origins or compiler roles against the pinned revision.
Update it when adding, moving, or reclassifying sources.

`library-sources.list` contains repository-relative paths across both trees in
declaration order. Preserve that order when moving files: it follows declaration
dependencies rather than alphabetical directory order. `scripts/embed-stdlib.py`
checks that every Dark source in both trees appears exactly once, rejects paths
outside them, and generates immutable OCaml strings. Dune tracks the manifest
and both complete source trees, including nested directories. The standalone
binary does not load source files from disk or need a share installation.

Regenerate the pinned Unicode tables with
`python3 scripts/generate_unicode_tables.py`; `--check` compares generated output
without writing it. Output paths are `StdLib/String/__Unicode/__Data.dark`,
`StdLib/String/__Unicode/__Data/__Index*.dark`, and `__Table*.dark`.

## Compiler dependencies

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
