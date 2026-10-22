# Remaining non-AOT compatibility differences

This ledger compares observable language and standard-library behavior with
darklang/dark `main` at revision
[`e76430be5cd9f6b77ed255430dc34a1b58dd8fc4`](https://github.com/darklang/dark/tree/e76430be5cd9f6b77ed255430dc34a1b58dd8fc4) (2026-10-19), the latest interpreter
revision checked on 2026-10-21. The source-audit compiler comparison revision is
[`73ae3461f8e0b009b072b0d26cda7e4e4e4dd121`](https://github.com/darklang/compiler/tree/73ae3461f8e0b009b072b0d26cda7e4e4e4dd121).

This refresh checks interpreter source and package-test witnesses, compiler
source, and compiler parser/type-checker probes. It is not a new interpreter
execution-suite run. The imported execution tests and compatibility validator
still target the older release; their historical results are separated below.
Other compatibility ledgers retain their own explicitly named revision pairs.

This ledger deliberately excludes differences that follow from ahead-of-time
compilation: earlier type and name errors, rejection of underconstrained dead
code, whole-program entry selection, concrete monomorphization, and native
resource representation. It also excludes performance and diagnostic wording.

## Language differences

| Area | Interpreter behavior | Current compiler boundary | Evidence |
| --- | --- | --- | --- |
| Effect ceilings | Function return annotations may contain permission ceilings such as `:{}` and `:{Clock}`; trait methods may also declare ceilings | Effect-row syntax and permission-ceiling enforcement are absent | Current interpreter `packages/darklang/tests/language/effectCeiling.dark` and `backend/src/LibParser/GRAMMAR.md`; compiler rejects `let pure (x: Int64) :{} Int64 = x + 1L` |
| Qualified user values | A `val` declared in a module is available both bare inside the module and by its qualified module name | Simple qualified module values work, but the imported nested-module value corpus still cannot resolve names such as `UserDefined.stringValue`; package values exist only when explicitly catalogued | Current interpreter `packages/darklang/tests/language/values.dark`; rechecking the imported `custom-data/values.dark` witnesses still reports those unresolved names |
| Recursive generic sums | Recursive generic ADTs retain one resolved identity through construction, folds, and recursion | Some recursive generic uses acquire incompatible identities that render identically | Current interpreter `packages/darklang/tests/language/enums.dark`; rechecked imported `custom-data/enums.dark` still reports incompatible `RBTree<a>` types in `ofList` |
| Application grouping | Space application remains left-associative when a bare value argument is followed by a parenthesized argument: `f value (g x)` | Some such calls are grouped as though `value` were applied to the parenthesized expression | The unchanged Crypto AWS chain reports `signing is not a function`; Stream transforms report `s is not a function` |
| Layout-separated list elements | Newline-separated list elements do not require commas | Some adjacent applications in a newline-separated list are consumed as additional arguments | The Blob concat case in `upstream/stdlib/bytes.dark` reports that `Blob.fromString` received five arguments; the other 17 cases pass parsing/type checking |
| Package environments | Interpreter names resolve through current/enclosing module candidates and branch-aware content-addressed package lookups | Compilation uses explicit units and snapshots, without the interpreter's live branch/package service | Current interpreter `backend/src/LibDB/NameLookup.fs` (`namesToTry`) and `backend/src/LibParser/NameResolver.fs`; this is not a claim that the current grammar has an `open` declaration |
| Runtime reflection | Interpreter runtime values, builtin metadata, parser services, and runtime-value-to-expression conversion are available to language tooling | `Builtin.getAllBuiltinFns`, `Builtin.parserParseToWrittenTypes`, `RuntimeTypes.Dval`, and `RuntimeTypesToProgramTypes.dvalToExpr` are absent | Current interpreter `packages/darklang/languageTools/` and `packages/darklang/tests/languageTools/`, plus `tests/language/runtimeToProgramTypes.dark`; compiler has no corresponding types/services |
| Traits and implementations | `trait`, `impl`, conditional implementations, and generic bounds such as `'a: Show + Compare` support user-defined dispatch; arithmetic and ordering operators use standard-library traits | No trait/implementation declarations, trait bounds, or user-defined operator dispatch | Current interpreter `packages/darklang/tests/language/traits.dark` and `packages/darklang/stdlib/traits.dark`; compiler declaration/bound probes fail parsing |
| Receiver calls | `value.method` dispatches a trait method; remaining arguments can be supplied afterward, and a real record field wins | Dot access only resolves record fields; it does not dispatch methods | Current interpreter trait tests cover `(9).show` and receiver partial application; compiler rejects `(1L).show` as access on a non-record |
| Content-addressed type identity | Separately named types with identical content can share identity | Separately declared records retain distinct nominal identities even with identical fields | Interpreter `packages/darklang/tests/stdlib/dict.dark` compares `Point` and `SameShape`; the equivalent two-record compiler probe rejects equality as incompatible types |
| Postfix unwrap | `value?` extracts `Some`/`Ok`, or returns `None`/`Error` from the enclosing function or lambda, skipping later work | No postfix `?` syntax or corresponding early-return lowering | Current interpreter `packages/darklang/tests/language/unwrap.dark`; compiler rejects `?` lexically. This differs from `Builtin.unwrap`, which raises on failure |
| Operator spellings and meaning | `**` is right-associative power; `^` is bitwise XOR; `&`, `\|`, `<<`, `>>`, prefix `~`, and prefix `!` are supported | `^` still means power; `**` and the listed bitwise/not spellings are unsupported expressions | Current interpreter grammar's precedence table; compiler parses `2L ^ 3L` as `Pow` and rejects probes for the other spellings |
| Collection separators | Lists, dictionaries, records, record updates, record-type fields, and list patterns use commas or newlines; semicolons are errors in these positions | Semicolons are still accepted as collection/field separators | Current interpreter `GRAMMAR.md` and `errCollectionSemicolon` in `LibParser/Parser.fs`; compiler accepts `[1L; 2L]`, a semicolon-separated Dict, and semicolon-separated record fields |
| Deferred reads | Read-only host operations can return pending values that flow through functions and containers; observation forces them, writes retain program order, and `Stdlib.await` can force early | No pending-value representation or matching read/force behavior | Current interpreter `docs/processes.md` ("Reads are concurrent"), `LibExecution/Pending.fs`, and `packages/darklang/stdlib/await.dark`; compiler AST/runtime has no promise representation |
| Package test declarations | `test name = body` and `test name = body => raises "message"` define isolated package tests returning `Stdlib.Test.Result` or expecting an exact error | No package-test declarations or package-test execution model; the compiler's E2E harness is a separate facility | Current interpreter `GRAMMAR.md` and `packages/darklang/stdlib/test.dark`; compiler rejects `test simple = true` |

The application-grouping and layout-list rows are frontend gaps, not Crypto,
Stream, or Blob algorithm differences. Focused compiler-authored tests that use
unambiguous parentheses exercise those implementations successfully.
Simple `f value (g x)` and newline-list probes also succeed; the retained rows
refer to the failing imported witnesses, not every use of those constructs.

Integer `/` is no longer a compiler-only operator: current interpreter numeric
types implement `Stdlib.Divide`. Its user-defined dispatch is part of the trait
gap above, not an AOT consequence.

## Closed differences: dictionary keys and generic aliases

The 2026-10-22 compiler changes add expression-key literals and the public
`Dict<key, value>` API. Hashing and canonical key comparison cover primitive
values, tuples, lists, records, sums, and dictionaries. NaN keys compare equal
and both signed zeros identify one key; ordinary dictionary-value equality
retains the language's Float equality. Unsupported keys raise errors, including
when nested in a compound value. Typed release plans cover managed keys.
The older String-key shorthand remains accepted. The public key restriction is
separate from private HAMT storage, which retains Blob identity keys for compiler
ownership operations. Empty construction and `fromList` retain their Dict and
Option result shapes, respectively.

Aliases now substitute parameters simultaneously, retain fixed arguments on
aliased sum constructors, and preserve concrete field-inferred type arguments.
Implicit signature variables bind generic functions for specialization.
The imported alias fixture also required its module preambles to stop retaining
sibling-module definitions: that contamination caused the apparent non-record
and arity errors recorded by the earlier source audit.

Regression coverage lives in `generic-dictionary-keys.e2e` and
`generic-alias-regressions.e2e`. The unchanged alias expressions and dictionary
literal smoke cases are enabled upstream. The alias suite has 42 active cases;
the dictionary suite has 127 active cases. Invalid programs with different
expected diagnostic wording have compile-time overrides. The dictionary denyset
retains 13 cases for diagnostic wording, frontend limitations, historical NaN
assertions, unconstrained lambdas, and the separate content-identity gap.

Host Linux x64 validation passes all 9,350 active tests with
`./run-tests --ai --e2e-batch-size=128`, following `./build --ai`. This includes
85 focused key/alias regressions and 223 ownership cases. Full benchmark
verification remains unavailable because the host x64 full snapshot and
Valgrind are absent; no performance or cross-architecture execution claim is
made by this closure evidence.

## Standard-library differences

| Surface | Remaining difference |
| --- | --- |
| `List` | `Stdlib.List.dedup` is absent. It retains first occurrences in input order using dictionary-key equality, including its NaN and unsupported-key rules. `List.sort` also supports a user `Compare` implementation in the interpreter; the compiler has no such dispatch. |
| Generic numeric and presentation APIs | The interpreter exposes trait-based `Stdlib.min`, `max`, `abs`, `clamp`, seeded `sum`/`product`, and `toString`, with numeric operations implemented as trait methods. The compiler retains the older type-specific functions and has no equivalent trait-based surface. |
| `Char` | `isLetter`, `isAlphanumeric`, `isWhitespace`, and `toCodepoint` are absent. The other enabled Char operations pass their upstream cases. |
| `Pretty` | Only the `Doc`/`Mode` types and `empty`, `line`, `hardLine`, and `softLine` values are present. `text`, `concat`, `nest`, `group`, `styled`, `join`, `hsep`, `vsep`, and `render` are absent. |
| HTTP client | Content-type constants exist, but the client's `Response`, `StreamResponse`, and `RequestError` types and `basicAuth`, `bearerToken`, `get`, and `request` operations are absent. |
| HTTP server | The default body-size value exists, but `getMethod`, `get`, and `post` route helpers are absent. |
| Server-sent events | `Stdlib.HttpClient.Sse.Event` and `parse` are absent. |
| SQLite | The upstream `Stdlib.Sqlite` value, query, execution, column, and conversion API is absent. |
| CLI TUI text | `Stdlib.Cli.Tui.Text.clipMarked` and `styledWidth` are absent. |
| Language tooling | Parsed-file shape, semantic tokenization, builtin introspection, runtime-value pretty printing, and runtime-value promotion are incomplete or absent. The snapshot-backed `ValueSearch` subset does not provide the interpreter's live package service. |
| Host APIs | Only the explicitly documented CLI/POSIX subset is implemented. The broader interpreter filesystem, environment, descriptor, download, watch, lock, and daemon surfaces have no parity claim. |
| Explicit concurrency | The interpreter's `Stdlib.Exec` provides typed handles, `spawn`, `spawnDetached`, `await`, `awaitWithin`, `select`, `cancel`, and process inspection. These APIs and their scheduler/lifetime semantics are absent from the compiler. |
| Float bits | Current interpreter `Stdlib.Float.toBits` returns `UInt64`; the compiler version returns arbitrary-precision `Int`. |

The standard-library rows were checked against current interpreter sources
under `packages/darklang/stdlib/` and compiler sources under
`src/DarkCompiler/stdlib/` and `Stdlib.fs`. Assertions about enabled upstream
test passes refer to the historical audit below, not a new interpreter run.

### Closed difference: finite float presentation

The interpreter no longer uses lossy `G12` formatting. Its
`backend/src/Prelude/Prelude.fs` helper `floatToShortestString` explicitly
matches the compiler's shortest-roundtrip digits and notation: fixed exponents
-4 through 15, otherwise lowercase scientific notation without exponent zero
padding, with `.0` on fixed whole numbers. `Builtin.floatToString` now uses
that helper. The former finite-formatting difference is therefore removed;
this source check does not assert parity for every non-finite rendering site.

Pure HTTP response helpers, JSON, Option, Result, Base64, Crypto, Streams, and
String have focused parity coverage. Their disabled upstream files or lines do
not by themselves establish an API difference: several depend on interpreter
test-only values, dynamic error propagation, Blob-handle expectations, or a
shared frontend gap listed above.

## Current audit evidence

The compiler build passed with `./build --ai`. Isolated parser/type-checker
probes confirmed the new syntax and identity boundaries above. Rechecking six
imported files through `TestDSL.E2EFormat.parseE2ETestFile`,
`PreambleAnalysis.analyzePreamble`, and
`TypeChecking.checkProgramWithBaseEnv` confirmed these retained witnesses:

| Imported witness | Compiler result at the comparison revision |
| --- | --- |
| `language/custom-data/values.dark` | Qualified nested values and uncatalogued package values remain unresolved |
| `language/custom-data/aliases.dark` | Historical probe failure, closed by the changes described above |
| `language/custom-data/enums.dark` | `ofList` still reports incompatible identically rendered `RBTree<a>` types |
| `stdlib/crypto.dark` | AWS chain still reports `signing is not a function` |
| `stdlib/stream.dark` | Some transforms still report `s is not a function` or equivalent |
| `stdlib/bytes.dark` | Concat witness still reports five arguments to `Blob.fromString`; the other 17 cases pass parsing/type checking |

These are compiler frontend checks, not native execution results. Some corpus
cases deliberately expect errors, so a count of accepted sources is not a
passing-test count. The full `./run-tests --ai` attempt terminated with a stack
overflow in ANF lowering. That earlier default-batch attempt did not produce a
whole-suite result; the closure validation above uses bounded batches.

The latest interpreter's language and standard-library tests live under
`packages/darklang/tests/`, using package test declarations. The compiler's
imported `src/Tests/e2e/upstream/` corpus is still the older execution-test
snapshot, not an unchanged copy of the latest interpreter tests. Updating its
format/importer and execution integration is separate work; neither that
corpus nor the validator pin is changed by this documentation refresh.

## Historical upstream-test audit

The following results belong to interpreter release `v0.0.35`, revision
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`, and compiler audit revision
`19d53b29db939f2404669e5cd29347857f5ad93d`. They are retained as historical
evidence, not results against the current interpreter.

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
`src/Tests/test-suite-tooling/TestRunner.fs`. When a gap with an imported witness
closes, enable that unchanged case before removing it from this ledger. Newer
package-test features also need compiler regression coverage; changes in the
test format alone do not establish a language gap or prove one has closed.
