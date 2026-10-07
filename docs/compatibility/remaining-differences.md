# Remaining non-AOT compatibility differences

Source audit: 2026-10-07, compiler
`7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`. Public-library baseline remains
darklang/dark `v0.0.35`, revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`; older per-surface interpreter
comparisons retain their own explicitly named revision. The compiler now uses
the copied interpreter parser and checks `WrittenTypes` directly.
[Current audit](current-audit.md) distinguishes source evidence from execution
results; [upstream inventory](upstream-test-inventory.md) records live gates.

This ledger excludes earlier AOT diagnostics, underconstrained dead-code
rejection, single executable entry selection, monomorphization, native
representations, performance, and diagnostic wording. An implemented API with
focused tests is not automatically proven equivalent across the full corpus.

## Language differences

| Area | Current compiler boundary | Evidence and validation limit |
| --- | --- | --- |
| Effect ceilings | `:{}` and `:{Clock}` parse into `WrittenTypes.fnDecl.effects`; the direct checker does not enforce the ceiling or propagate source effect rows through calls and callbacks | `Parser.ml`, `DeclarationSupport.ml`, `WrittenTypes.ml`, `WrittenDeclarations.checkFunction`; upstream `effect-ceiling.dark` remains gated. Syntax is implemented; enforcement is the gap |
| Declaration overlays | Duplicate functions and types within one source batch are rejected; values enter the inventory sequentially and can overwrite the same qualified value name | `WrittenDeclarations.predeclareTypes`, `predeclareFunctions`, and `checkItems`. The old blanket last-declaration-wins claim is no longer accurate |
| Module-open environments | Modules retain scope during checking, but no general module-open environment is implemented | `WrittenSource.ml`, `WrittenTypeSupport.ml`; distinguish this from explicit qualification and package loading |
| Runtime reflection and language tooling | No registered `Builtin.reflect`, `getAllBuiltinFns`, or `parserParseToWrittenTypes`; no embedded `RuntimeTypesToProgramTypes.dvalToExpr` implementation | The public runtime `Dval` and related type declarations, runtime pretty-printer, and error-segment renderer **do** exist in `packages/Darklang/`. Native values are not automatically promoted into Dval; compiler use of the copied parser does not expose a Dark parser builtin |
| Transparent async | Native I/O and sleep block; generated programs have no transparent yield-at-use suspension/resumption machinery | Native HTTP and sleep implementations; compile-time OCaml Lwt package transport is separate from the generated program runtime |

Qualified module values are supported by the direct checker:
`WrittenDeclarations` registers `path @ [valueName]`, and
`WrittenTypeSupport.resolveValue` searches scoped qualified candidates. The
upstream values fixture remains gated; its old `19/72` result is historical,
not a current limitation count. The fresh probe passed 19 cases and failed 53 with missing interpreter
test-package values such as `UserDefined.stringValue`; that invocation did not
supply the upstream package server. It does not disprove qualified-value or
optional hosted-loader support.

Fresh unchanged-source probes passed Crypto 9/9, Stream 25/25, and SSE 8/8,
making their whole-file gates removal candidates. Pretty passed 34/35; L186's
multiline nested application still fails with “Expected TUnit, got TFunction”.
See the [probe results](current-audit.md#fresh-probes-of-whole-file-gates) for
other dependency, preamble, and API failures. No gate was changed here.

Hosted package loading is implemented **at compile time**, enabled explicitly
with `--package-server URL`. `UserCompilation.compileUserWithPlan` calls
`PackageManager.resolveWritten`; the loader resolves names/hashes, traverses
dependencies, caches responses through SQLite, and adds fetched declarations
as package source units before checking. `package_manager.e2e` covers cached
declaration resolution/compilation;
`test/regression/package_io_regression.ml` covers cache/encoding and
`test/regression/package_http_regression.ml` covers HTTP/TLS transport against
a local server. This does not claim the whole upstream package tree compiles,
automatic loading without the flag, or a live runtime package-manager service. `ValueSearch` continues to use its explicit
compile-time value catalog.

## Standard-library and host differences

| Surface | Current support and remaining boundary |
| --- | --- |
| HTTP client | Buffered HTTP/HTTPS requests, verb wrappers, and pull-based response streaming exist. External-network upstream cases remain whole-file gated. Transport/TLS profile and restrictions are in the [HTTP ledger](stdlib/html-and-http.md) |
| HTTP server | Sequential IPv4 HTTP/1.1 serving, routing, configuration, body/framing limits, deadlines, signals, and resource cleanup exist. Concurrency, IPv6 listeners, persistent connections, compression, server TLS, and broader platform validation remain follow-up work. Current main has no HTTP/2 or HTTP/3 implementation claim |
| SQLite | No embedded public `Stdlib.Sqlite` query/execution/conversion API. Compiler package-cache use of SQLite does not supply that API to Dark programs |
| Host APIs | Filesystem, environment, process, presentation, input, and documented POSIX subsets exist. Broader descriptor, download, watch, lock, daemon, cloud database, SCM service, and application-service surfaces need individual implementation/dependency/host audits |
| Float presentation | Shortest-roundtrip finite formatting intentionally differs from the pinned release's lossy G12 formatting. The newer interpreter revision named in the [float ledger](stdlib/floats-and-math.md) uses the same shortest-roundtrip notation boundary |
| Runtime package queries | `ValueSearch` operates on an explicit catalog snapshot with frozen branch visibility and supplied ordering; compile-time hosted declaration loading does not make those queries live |

Pretty, terminal text, SSE, pure HTTP helpers, JSON, Option, Result, Base64,
Crypto, Streams, and String have source implementations and focused coverage.
A disabled upstream fixture may expose a real parser/checker/runtime defect,
a dependency, a test-only builtin, a host requirement, or an oracle difference;
its whole public API must not be labelled absent solely because of that gate.

## Upstream-test audit

At the audited compiler revision, the repository imports **105 `.dark` files**.
The runner has **44 whole-file exclusions** and **260 line-number entries in
24 files**. These are exclusion entries, not skipped-test or missing-feature
counts. Blank/comment/declaration lines can occur in the line lists, and E2E
assertion locations are assigned by the fixture parser. See the complete
[inventory](upstream-test-inventory.md) before interpreting a line entry.

Aliases, enums, bytes, Char, Dict, Dict literals, and terminal text are enabled
with any individual exclusions recorded in that inventory. HTTP server is
partially enabled. Crypto, Http, HttpClient, Json, Pretty, runtime PrettyPrinter,
SSE, Stream, and String remain whole-file gated despite existing implementations.
The HttpClient gate includes local `NoInternet` assertions as well as external
network calls.

The imported directory matches the newer parser-pin file inventory, but eight
files have source adaptations beyond local metadata/error spelling. See the
[current provenance audit](current-audit.md#imported-corpus-provenance). Do not
call every imported fixture unchanged-source evidence.

The previous compiler audit at `19d53b29db939f2404669e5cd29347857f5ad93d`
recorded 432 language passes/29 failures and 3,136 stdlib passes/137 failures
with individual gates removed. Those pre-port results are historical and are
not used as current failure counts or current causes. Gate changes and source
implementation changes must be audited independently.

To close a coverage gap, execute the unchanged upstream fixture, classify each
failure, and enable the passing cases. Do not change a gate or compiler behavior
as part of this documentation audit merely to obtain a parity claim.
