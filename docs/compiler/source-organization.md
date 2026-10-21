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
| `packages/` | Dark library packages and compiler support embedded at build time |
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

All embedded Dark sources live under `packages/`, matching the interpreter's
owner directories and lower-camel-case source paths. The standard library lives
under `packages/darklang/stdlib/`; other Darklang packages include
`languageTools/`, `prettyPrinter/` and `scm/`. For example,
`Darklang.Stdlib.Cli.UI.Colors` lives in `packages/darklang/stdlib/cli/ui/colors.dark`.
`Builtin` is the standard-library bridge exception: it lives at
`packages/darklang/stdlib/__builtin.dark` and retains its interpreter `Builtin` namespace.
Other packages use the interpreter's paths under `packages/`, such as
`packages/darklang/languageTools/programTypes.dark`. The few `module Stdlib.*`
declarations also use the implicit `Darklang` owner. Placement does not change
language names.

| Repository path | Contents |
|---|---|
| `packages/darklang/stdlib/` | Darklang.Stdlib packages and their implementation helpers |
| `packages/darklang/stdlib/noModule.dark`, `packages/darklang/stdlib/__print.dark` | Functions directly in Darklang.Stdlib |
| `packages/darklang/stdlib/__types.dark`, `packages/darklang/stdlib/__hash.dark` | Compiler root type representation and hashing helpers |
| `packages/darklang/languageTools/` | Program types, runtime types and package-manager APIs |
| `packages/darklang/prettyPrinter/` | Runtime type and error rendering |
| `packages/darklang/scm/` | Branch identity |
| `packages/darklang/stdlib/__builtin.dark` | Compiler bridges for interpreter builtins; retains the `Builtin` namespace |
| `packages/darklang/stdlib/string/__unicode.dark`, `packages/darklang/stdlib/string/__unicode/` | Compiler Unicode operations and generated data |

A package implemented in several files keeps its main file at the package
path and its fragments in the matching directory. For example,
`packages/darklang/stdlib/list.dark` contains the public List API, while
`packages/darklang/stdlib/list/__skewList.dark` and `__listArray.dark` extend that same package.
`packages/darklang/stdlib/list/sortByComparatorHelpers.dark` is a genuine nested interpreter
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
[complete source inventory](library-sources.md) lists all embedded files, their
modules, and interpreter origins or compiler roles against the pinned revision.
Update it when adding, moving, or reclassifying sources.

Directory placement does not imply source or behavior equivalence. Existing
split modules and compiler implementations keep their contents; matching the
interpreter's file boundaries is separate work. Env, File, Process, Sys and root
Cli execution retain byte-identical sources from the porting pin. The
[porting ledger](../porting/README.md) records declaration-level differences.

`library-sources.list` contains repository-relative package paths in
declaration order. Preserve that order when moving files: it follows declaration
dependencies rather than alphabetical directory order. `scripts/embed-stdlib.py`
checks that every Dark source under `packages/` appears exactly once, rejects
paths outside it, and generates immutable OCaml strings. Dune tracks the manifest
and the complete package source tree, including nested directories. The standalone
binary does not load source files from disk or need a share installation.

Regenerate the pinned Unicode tables with
`python3 scripts/generate_unicode_tables.py`; `--check` compares generated output
without writing it. Output paths are `packages/darklang/stdlib/string/__unicode/__data.dark`,
`packages/darklang/stdlib/string/__unicode/__data/__index*.dark`, and `__table*.dark`.

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
