# Remaining non-AOT compatibility differences

This ledger records observable language and standard-library differences from
darklang/dark release `v0.0.35`, revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`. The compiler audit revision is
`19d53b29db939f2404669e5cd29347857f5ad93d`.

This ledger deliberately excludes differences that follow from ahead-of-time
compilation: earlier type and name errors, rejection of underconstrained dead
code, whole-program entry selection, concrete monomorphization, and native
resource representation. It also excludes performance and diagnostic wording.

## Language differences

| Area | Interpreter behavior | Current compiler boundary | Evidence |
| --- | --- | --- | --- |
| Effect ceilings | Function return annotations may contain permission ceilings such as `:{}` and `:{Clock}` | Effect-row syntax and permission-ceiling enforcement are absent | `upstream/language/effect-ceiling.dark` fails while parsing the return annotation |
| Qualified user values | A `val` declared in a module is available both bare inside the module and by its qualified module name | Unqualified top-level values work, but the imported nested-module value corpus cannot resolve names such as `UserDefined.stringValue`; package values exist only when explicitly catalogued | `upstream/language/custom-data/values.dark`: 19/72 cases pass when enabled, with qualified user and package values accounting for the failures |
| Application grouping | Space application remains left-associative when a bare value argument is followed by a parenthesized argument: `f value (g x)` | Some such calls are grouped as though `value` were applied to the parenthesized expression | The unchanged Crypto AWS chain reports `signing is not a function`; Stream transforms report `s is not a function` |
| Module environments | Opened modules and content-addressed packages participate in interpreter name resolution | Module-open environments and live content-addressed package loading are absent; compilation uses explicit units and snapshots | The identifier and name-resolution ledgers document this boundary |
| Runtime reflection | Interpreter runtime values, builtin metadata, parser services, and runtime-value-to-expression conversion are available to language tooling | `Builtin.getAllBuiltinFns`, `Builtin.parserParseToWrittenTypes`, `RuntimeTypes.Dval`, and `RuntimeTypesToProgramTypes.dvalToExpr` are absent | The builtin-introspection, parsed-file-shape, semantic-tokenization, and runtime-to-program-types upstream files fail on those names or types |

The application-grouping row is a frontend gap, not a Crypto or Stream
algorithm difference. Focused compiler-authored tests that use unambiguous
parentheses exercise those implementations successfully.

## Standard-library differences

| Surface | Remaining difference |
| --- | --- |
| HTTP client | The pure response/error types, content-type and authorization-header helpers are present. Operations that perform requests (`request`, `get`, `post`, `put`, `options`, `delete`, `head`, and `stream`) remain absent because there is no HTTP client host. |
| HTTP server | Pure `getMethod`, `get`, and `post` route construction is present. `serve` remains absent because there is no HTTP server host. |
| SQLite | The upstream `Stdlib.Sqlite` value, query, execution, column, and conversion API is absent. |
| Language tooling | Parsed-file shape, semantic tokenization, builtin introspection, runtime-value pretty printing, and runtime-value promotion are incomplete or absent. The snapshot-backed `ValueSearch` subset does not provide the interpreter's live package service. |
| Host APIs | Only the explicitly documented CLI/POSIX subset is implemented. The broader interpreter filesystem, environment, descriptor, download, watch, lock, and daemon surfaces have no parity claim. |
| Float presentation | Finite `Float.toString` intentionally emits the shortest round-tripping decimal, while the pinned interpreter uses lossy `G12` formatting. This is a deliberate observable improvement, not an AOT requirement. |

Pretty layout/rendering, SGR-aware CLI text measurement and marked clipping,
lazy SSE parsing, pure HTTP construction helpers, HTTP response helpers, JSON,
Option, Result, Base64, Crypto, Streams, and String have focused parity
coverage. Their disabled upstream files or lines do not by themselves
establish an API difference: several depend on interpreter test-only values,
dynamic error propagation, Blob-handle expectations, an unavailable host, or a
shared frontend gap listed above.

## Upstream-test audit

The imported corpus matches the pinned upstream sources except for local
`#compileerror` metadata and insignificant trailing-newline differences. At the
compiler revision above it contains 105 files. The default runner disables 44
whole files and individual cases in 24 more files (261 source lines). Those
denysets are an enablement queue, not a count of independent semantic gaps.

A diagnostic run removed only the individual-line denyset and used
`--e2e-batch-size=1` so one compile error could not invalidate neighboring
cases:

- language: 432 passed and 29 failed among the normally enabled files;
- stdlib: 3,136 passed and 137 failed among the normally enabled files.

The remaining failures in those runs were classified into the gaps above,
intentional AOT boundaries, interpreter-only test infrastructure, or expected
presentation differences. Whole disabled files were then enabled one at a time
to avoid cross-file preamble failures. The aliases, enums, bytes, Char, Dict,
Dict-literal smoke, and CLI TUI text files are now enabled. The pure
HTTP-server cases are enabled while three fixture expressions whose response
literals use the interpreter's former `Int64` status-code shape remain
line-gated. Pretty and SSE remain whole-file gated because their shared fixture
preambles trigger the application-grouping frontend gap; focused tests exercise
the copied implementations. The HTTP-client fixture also calls the
intentionally absent network host from its shared preamble. A disabled file
must not be treated as proof that its entire feature is missing.

The authoritative live denysets remain in
`src/Tests/test-suite-tooling/TestRunner.fs`. When a gap closes, enable its
unchanged upstream case before removing it from this ledger.
