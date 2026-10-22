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
| Dictionary keys | `Dict<key, value>` and `Dict { expression: value }` admit supported non-String key types | The public type is `Dict<value>`, APIs take `String` keys, and literals store String keys | `upstream/stdlib/dict.dark` fails at `val valDict = Dict { 1L: "a"; 2L: "b" }`; `interop.syntax` explicitly rejects two Dict type arguments |
| Effect ceilings | Function return annotations may contain permission ceilings such as `:{}` and `:{Clock}` | Effect-row syntax and permission-ceiling enforcement are absent | `upstream/language/effect-ceiling.dark` fails while parsing the return annotation |
| Qualified user values | A `val` declared in a module is available both bare inside the module and by its qualified module name | Unqualified top-level values work, but the imported nested-module value corpus cannot resolve names such as `UserDefined.stringValue`; package values exist only when explicitly catalogued | `upstream/language/custom-data/values.dark`: 19/72 cases pass when enabled, with qualified user and package values accounting for the failures |
| Generic aliases | Aliases preserve the structure and type arguments of the aliased record, tuple, list, or sum | A generic record alias is not recognized as a record during field access | `upstream/language/custom-data/aliases.dark` fails on `Inner<'a, 'b>.b` while building its preamble |
| Application grouping | Space application remains left-associative when a bare value argument is followed by a parenthesized argument: `f value (g x)` | Some such calls are grouped as though `value` were applied to the parenthesized expression | The unchanged Crypto AWS chain reports `signing is not a function`; Stream transforms report `s is not a function` |
| Layout-separated list elements | Newline-separated list elements do not require commas | Some adjacent applications in a newline-separated list are consumed as additional arguments | The Blob concat case in `upstream/stdlib/bytes.dark` reports that `Blob.fromString` received five arguments; the other 17 cases pass |
| Module environments | Opened modules and content-addressed packages participate in interpreter name resolution | Module-open environments and live content-addressed package loading are absent; compilation uses explicit units and snapshots | The identifier and name-resolution ledgers document this boundary |
| Runtime reflection | Interpreter runtime values, builtin metadata, parser services, and runtime-value-to-expression conversion are available to language tooling | `Builtin.getAllBuiltinFns`, `Builtin.parserParseToWrittenTypes`, `RuntimeTypes.Dval`, and `RuntimeTypesToProgramTypes.dvalToExpr` are absent | The builtin-introspection, parsed-file-shape, semantic-tokenization, and runtime-to-program-types upstream files fail on those names or types |

The application-grouping and layout-list rows are frontend gaps, not Crypto,
Stream, or Blob algorithm differences. Focused compiler-authored tests that use
unambiguous parentheses exercise those implementations successfully.

The recursive generic sum entry from the pinned audit was closed on
2026-10-22. Its apparently identical `RBTree<a>` types came from a misleading
return-type diagnostic: a generic nullary constructor rejected the still-unknown
fold accumulator type. Nullary constructors now unify with that context. Stdlib
specializations also retain user sum ownership layouts, fixing the subsequent
ownership-planning crash. The unchanged red-black-tree case in
`upstream/language/custom-data/enums.dark` now runs by default, alongside focused
integer/String tree construction, folding, recursive traversal, and rejection
coverage in `recursive_generic_sums.e2e`. Other enum cases remain individually
disabled for independent arity, match, rendering, and helper-linking gaps; this
closure does not claim full enum parity.

## Standard-library differences

| Surface | Remaining difference |
| --- | --- |
| `List` | `Stdlib.List.dedup` is absent. Unlike `unique`, it must retain the first occurrence and preserve input order. All five pinned upstream `dedup` cases fail name resolution. |
| `Char` | `isLetter`, `isAlphanumeric`, `isWhitespace`, and `toCodepoint` are absent. The other enabled Char operations pass their upstream cases. |
| `Pretty` | Only the `Doc`/`Mode` types and `empty`, `line`, `hardLine`, and `softLine` values are present. `text`, `concat`, `nest`, `group`, `styled`, `join`, `hsep`, `vsep`, and `render` are absent. |
| HTTP client | Content-type constants exist, but the request model and `basicAuth`, `bearerToken`, `get`, and `request` operations are absent. |
| HTTP server | The default body-size value exists, but `getMethod`, `get`, and `post` route helpers are absent. |
| Server-sent events | `Stdlib.HttpClient.Sse.Event` and `parse` are absent. |
| SQLite | The upstream `Stdlib.Sqlite` value, query, execution, column, and conversion API is absent. |
| CLI TUI text | `Stdlib.Cli.Tui.Text.clipMarked` and `styledWidth` are absent. |
| Language tooling | Parsed-file shape, semantic tokenization, builtin introspection, runtime-value pretty printing, and runtime-value promotion are incomplete or absent. The snapshot-backed `ValueSearch` subset does not provide the interpreter's live package service. |
| Host APIs | Only the explicitly documented CLI/POSIX subset is implemented. The broader interpreter filesystem, environment, descriptor, download, watch, lock, and daemon surfaces have no parity claim. |
| Float presentation | Finite `Float.toString` intentionally emits the shortest round-tripping decimal, while the pinned interpreter uses lossy `G12` formatting. This is a deliberate observable improvement, not an AOT requirement. |

Pure HTTP response helpers, JSON, Option, Result, Base64, Crypto, Streams, and
String have focused parity coverage. Their disabled upstream files or lines do
not by themselves establish an API difference: several depend on interpreter
test-only values, dynamic error propagation, Blob-handle expectations, or a
shared frontend gap listed above.

## Upstream-test audit

The imported corpus matches the pinned upstream sources except for local
`#compileerror` metadata and insignificant trailing-newline differences. At the
compiler revision above it contains 105 files. The default runner disables 53
whole files and individual cases in 22 more files (202 source lines). Those
denysets are an enablement queue, not a list of 255 independent semantic gaps.

A diagnostic run removed only the individual-line denyset and used
`--e2e-batch-size=1` so one compile error could not invalidate neighboring
cases:

- language: 432 passed and 29 failed among the normally enabled files;
- stdlib: 3,136 passed and 137 failed among the normally enabled files.

The remaining failures in those runs were classified into the gaps above,
intentional AOT boundaries, interpreter-only test infrastructure, or expected
presentation differences. Whole disabled files were then enabled one at a time
to avoid cross-file preamble failures. For example, the formerly disabled Dict
literal smoke file passed 3/3 unchanged, demonstrating why a disabled file must
not be treated as proof that its entire feature is missing.

The authoritative live denysets remain in
`src/Tests/test-suite-tooling/TestRunner.fs`. When a gap closes, enable its
unchanged upstream case before removing it from this ledger.
