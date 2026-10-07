# Current compatibility audit

Reviewed 2026-10-07 against compiler main
`7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`, after the OCaml port and the
StdLib source reorganization. This page owns the current review scope and
validation status; [remaining differences](remaining-differences.md) owns the
open-gap summary and [upstream inventory](upstream-test-inventory.md) owns
file/line enablement. Every detailed ledger links here.

## Baselines and evidence rules

The embedded public-library inventory targets darklang/dark `v0.0.35`,
`0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`. The copied parser and effect
syntax instead derive from `1cc4bb7f63acdf29dc66458f3c401ed91d444775`, as
recorded in `src/frontend/interpreter/README.md`. Older per-surface comparisons
also name `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. These are distinct
baselines: accepting newer syntax does not prove parity with every old package
or fixture. Historical performance ratios and test totals are not current
compatibility measurements.

Current production source parsing goes through `WrittenParsing` and the copied
interpreter parser. `WrittenSource` retains module paths;
`WrittenChecking`/`WrittenDeclarations` produce `CheckedAST` directly. The old
parsed-AST normalization, resolver, and type-checker line references in detailed
implementation histories do not describe this production boundary. Consult
[the current type-checking architecture](../compiler/frontend/type-checking.md)
and [library inventory](../compiler/library-sources.md) for current structure
and embedded-source ordering.

The tables below record **source implementations and existing test evidence**.
“Enabled” describes the default runner gate, not an execution result.
“Focused” means a compiler-authored fixture exists; it does not mean every
unchanged upstream case passed. Previously diagnosed failures whose code path
was replaced are recorded as needing revalidation rather than repeated as
verified current bugs. No test gates or compiler behavior were changed here.

## Imported-corpus provenance

The full imported execution directory was compared with darklang/dark
`1cc4bb7f63acdf29dc66458f3c401ed91d444775` using:

```bash
python3 scripts/diff-upstream-execution-tests.py --upstream-dir /workspace/scratch/b5eb11cc95e0/darklang-interpreter/backend/testfiles/execution --ignore-expected-diff --output /workspace/scratch/b5eb11cc95e0/upstream-diff.md
```

That diagnostic comparison found no upstream-only or local-only files, and
**eight files with normalized content differences**. The comparator strips
local `#compileerror` metadata and treats the supported test-error spelling
adaptations as equivalent. Its “meaningful” category can still contain a
trailing blank line. The corpus is therefore **imported with adaptations**,
not universally unchanged. This is a source comparison, not a parity run.

| Imported file | Remaining source adaptation |
| --- | --- |
| `language/apply/eapply.dark` | An invalid return declaration is scoped into its own assertion; trailing blank line differs |
| `language/basic/elet.dark` | Exhaustive fallback match arms added for AOT checking |
| `language/custom-data/enums.dark` | Exhaustive fallback match arms added |
| `language/flow-control/ematch.dark` | Exhaustive arms added; one guarded Result test is expressed through an annotated function |
| `stdlib/dict.dark` | NaN/infinity key ordering checked through integer classifications, avoiding NaN equality in the oracle |
| `stdlib/html.dark` | Multiline application/list layout compacted, with explicit list separators |
| `stdlib/list.dark` | Explicit Int64 type arguments added to empty tail/splitLast calls |
| `stdlib/option.dark` | Explicit Int64 type argument added to combine an all-None list |

These adaptations can hide unsupported original source shapes. Restoring or
retesting those original shapes belongs in the enablement queue; adapted
fixture success must not be reported as unchanged-source success. The current
comparison revision is the parser pin, not an assertion that every fixture
matches the older public-library release.

## Language ledgers

| Ledger | Current source/test evidence | Current qualification |
| --- | --- | --- |
| [Bindings](language/bindings.md) | `WrittenLetSupport.ml`, `WrittenLambdaSupport.ml`; `interpreter/bindings.e2e`, `closures.e2e` | Lexical scope and captures exist; binding diagnostics occur before code generation. Historical AST node descriptions are superseded by direct checking |
| [Conditionals and sequences](language/conditionals-and-sequences.md) | `WrittenExpressions.ml`, checked If/Sequence, ANF control flow; `conditional_sequence_parity.e2e` | Bool conditions, unified arm types, and Unit sequence heads remain static requirements |
| [Identifiers](language/identifiers.md) | Copied `Lexer.ml`, `NameSyntax.ml`; `syntax/names.syntax`, `ParserTests.ml` | Unicode/quoted qualification exists; general module-open environments remain absent; hosted compile-time package lookup exists |
| [Name resolution](language/name-resolution.md) | `WrittenTypeSupport.ml`, `WrittenExpressions.ml`, `WrittenDeclarations.ml`; `name-resolution.e2e`, `first_class_values.e2e`, `package_manager.e2e` | Qualified values have support. Duplicate function/type declarations now fail rather than overlay. Full upstream values coverage remains gated |
| [Primitive literals](language/primitive-literals.md) | Copied Lexer and expression parser; `literal_parity.e2e`, `syntax/literals.syntax` | Arbitrary Int and sized literals exist; Int128/UInt128 use fixed limb blocks. Apostrophe graphemes in package sources were repaired at the audited main revision |
| [Program structure](language/program-structure.md) | `WrittenSource.validateSourceUnits`, `WrittenDeclarations.checkItems`; `ProgramStructureTests.ml` | One entry across executable units; dependency units are declarations only. Functions/types predeclare; values check sequentially. Optional hosted packages join compilation before checking |
| [Records](language/records.md) | `WrittenRecordSupport.ml`, `WrittenTypeSupport.structuralEqualityCompatible`; `records.e2e`, `record_alias_construction.e2e`, upstream record files | Assignment remains nominal; equality additionally accepts separately named compatible record layouts |
| [Recursion](language/recursion.md) | `WrittenDeclarations.attachRecursiveGroups`, `WrittenLetSupport.ml`, specialization; `interpreter/recursion_parity.e2e`, `tailcall.e2e` | Self/mutual recursion and lexical closures exist; top-level values are a supported facility, checked sequentially |
| [Tuples](language/tuples.md) | Copied parser, `WrittenCollectionSupport.ml`, ANF aggregate lowering; `tuple-parity.e2e` | Ordered heterogeneous tuples, destructuring, and structural equality exist; internal projection is not public syntax |

Effect-ceiling parsing is separately implemented in the copied declarations
parser; source ceiling enforcement is absent in the direct checker. Operator
syntax now uses `**` for exponentiation, `^` for XOR, `&`/`|` for bitwise
and/or, `<<`/`>>` for shifts, `~` for integer complement, and `!` for Boolean
negation. `ParserTests.ml` and `operator_parity.e2e` record the changed power/XOR
contract. The old reserved-but-unsupported bitwise claim has been removed.

## Standard-library ledgers

| Ledger | Current implementation and focused evidence | Default upstream enablement and limits |
| --- | --- | --- |
| [Binary and Crypto](stdlib/binary-and-crypto.md) | `StdLib/{Blob,Base64,Crypto,X509}.dark`; `blob.e2e`, `x509.e2e`, local stdlib fixtures | Bytes and X509 enabled; Base64 partially enabled; Crypto whole-file gated, but fresh probe passes 9/9. Blob equality is handle identity |
| [Comparison](stdlib/comparison.md) | `WrittenOperatorSupport.ml`, comparison planning/helpers; `comparison-parity.e2e`, `dval_recursive_equality.e2e` | Nomodule partially enabled. Streams support identity equality; compatible records support field equality. Database-reference host semantics are not claimed |
| [Dict](stdlib/dicts.md) | `StdLib/Dict.dark`, HAMT, `WrittenCollectionSupport.ml`; `dict_parity.e2e`, `dval_dict_lookup.e2e` | Dict partially enabled; edict enabled. Two-argument Dict types and structural keys exist. Public numeric ordering operators do not accept Dict |
| [Diff/ValueSearch](stdlib/diff-and-value-search.md) | `StdLib/{Diff,ValueSearch}.dark`, `PackageCatalog.ml`; `interpreter/diff.e2e`, `ValueSearchCatalogTests.ml` | PickLocation remains whole-file gated. ValueSearch is catalog-backed; optional hosted source loading does not supply live runtime queries |
| [Float/Math](stdlib/floats-and-math.md) | `StdLib/{Float,Math}.dark`; `stdlib/{float,math}.e2e` | Both upstream files partially enabled. Shortest-roundtrip presentation intentionally differs from the old release, agrees with the newer named interpreter source |
| [Html/HTTP](stdlib/html-and-http.md) | `StdLib/Html.dark`, Http modules and native transport; `html_http.e2e`, `http_client_wrappers.e2e`, `stdlib-internal/http_server.e2e` | Html and HTTP server partially enabled; Http and HttpClient whole-file gated. HTTP/1.1 client/server support exists with the documented platform/TLS limits |
| [Integers](stdlib/integers.md) | All eleven integer modules; `integer-family.e2e`, `int128-wrapping.e2e` | All eleven upstream files enabled; three have individual line gates. Arbitrary Int and fixed limb Int128/UInt128 are distinct managed representations |
| [JSON](stdlib/json.md) | `StdLib/{AltJson,Json}.dark`, `JsonPlanning.ml`; `json-parity.e2e` | AltJson enabled, typed Json whole-file gated. Native typed codecs exist; unsupported runtime shapes fail at compile time |
| [Lists](stdlib/lists.md) | `StdLib/List.dark`, private list support, typed lowering; `list_parity.e2e`, `list_language_parity.e2e` | List partially enabled; dlist enabled. Generic callbacks, skew-list representation, typed equality and rendering exist |
| [Option/Result/Retry](stdlib/option-result-retry.md) | `StdLib/{Option,Result,Retry}.dark`; `control_combinators_retry.e2e` | Option and Result partially enabled. Retry delay blocks; no transparent async claim |
| [Pretty](stdlib/pretty.md) | `StdLib/Pretty.dark`; `stdlib/copied_pure_surfaces.e2e` | Pretty whole-file gated; fresh probe passes 34 cases and fails one multiline application at L186 |
| [Streams](stdlib/streams.md) | `StdLib/Stream.dark`, native handle lifecycle; `stream.e2e`, `stdlib-internal/stream.e2e` | Stream whole-file gated, but fresh probe passes 25/25. Lazy pulls and deterministic close exist; public streaming is not transparent async scheduling |
| [Temporal](stdlib/temporal.md) | `StdLib/{DateTime,Duration}.dark`, native clock; `temporal-parity.e2e` | Date and Duration enabled without line gates; historical pass totals are retained as historical results |
| [Text](stdlib/text.md) | `StdLib/{Char,String,Regex}.dark`, Unicode runtime data; `stdlib/text_parity.e2e` | Char, Regex, and terminal text enabled; String whole-file gated. Compiler Unicode library and generated runtime Unicode version are separate pins |

SSE also exists at `StdLib/HttpClient/Sse.dark`, with focused cases in
`stdlib/copied_pure_surfaces.e2e`; its upstream file is whole-file gated, but the fresh probe passes 8/8.
`packages/Darklang/LanguageTools/RuntimeTypes/` contains public Dval/type trees.
`packages/Darklang/PrettyPrinter/RuntimeTypes.dark` and its RuntimeError module
implement rendering; `dval_recursive_equality.e2e`, `dval_dict_lookup.e2e`, and
`runtime_error_segments.e2e` provide focused evidence. Missing reflection,
builtin enumeration, parser builtin access, and Dval-to-expression promotion
must be distinguished from those existing types and renderers.

## CLI ledgers

| Ledger | Current implementation/test evidence | Current boundary |
| --- | --- | --- |
| [CLI](cli.md) | `StdLib/Cli/Path.dark`, File and FileSystem, Env; `cli_filesystem.e2e`, `filesystem_env_parity.e2e` | Path/glob, process, and color upstream fixtures enabled; broad CLI/service fixtures remain gated |
| [Presentation](cli-presentation.md) | `StdLib/Print.dark`, `StdLib/Cli/Log.dark`, `StdLib/Cli/UI/`; `interpreter/cli_presentation.e2e` | Print/input/log/color/progress/prompt/spinner/table exist; old source paths and numbered registry anchors are historical |
| [Process/host/input](cli-process-host-input.md) | `StdLib/Cli.dark`, `StdLib/Cli/{Process,Host,Stdin,Sys,Env,Posix}.dark`; `cli_process_host_input.e2e` | Documented host subset implemented; larger descriptor/watch/lock/daemon APIs have no blanket parity claim |

## Fresh probes of whole-file gates

On 2026-10-07, fifteen gated files were copied without source edits to a
temporary directory under the imported corpus and run individually with
`./run-tests --ai --filter=__audit_20261007/<copied-name> --e2e-batch-size=1`.
The alternate paths bypass the exact whole-file denyset. All temporary copies
were removed; the runner and committed gates remain unchanged. Counts below
are parsed assertions, including repeated preamble failures, not independent
missing features. Imported package-dependent fixtures were not supplied their
interpreter test package server; these failures do not establish a hosted-loader
defect or absence.

| Fixture | Passed / failed | Observed boundary |
| --- | --- | --- |
| `language/custom-data/values.dark` | 19 / 53 | Missing `UserDefined.*` test-package declarations, including `stringValue`; source support for qualified values exists |
| `language/effect-ceiling.dark` | 0 / 6 | Missing `Ceiling.*` declarations in this invocation; source inspection separately confirms absent ceiling enforcement |
| `stdlib/crypto.dark` | 9 / 0 | All parsed cases pass; whole-file gate is stale for this host/run |
| `stdlib/http.dark` | 21 / 26 | Response/query value mismatches and missing `Stdlib.Http.urlDecode`; needs case-level diagnosis |
| `stdlib/json.dark` | 0 / 540 | Shared preamble needs `UserDefinedEnums.PrettyLikely`; typed codec parity was not exercised |
| `stdlib/pretty.dark` | 34 / 1 | L186 multiline nested `concat` produces “Expected TUnit, got TFunction”; application-layout failure remains in this source shape |
| `stdlib/sse.dark` | 8 / 0 | All parsed cases pass; whole-file gate is stale for this host/run |
| `stdlib/stream.dark` | 25 / 0 | All parsed cases pass; whole-file gate is stale for this host/run |
| `stdlib/string.dark` | 0 / 640 | Shared preamble uses missing test-only `Builtin.testToChar`; public String parity was not exercised |
| `stdlib/prettyPrinter.dark` | 0 / 57 | Shared preamble uses missing `Builtin.reflect` |
| `stdlib/language-tools/pickLocation.dark` | 0 / 30 | Shared preamble expects absent `PickContext.preferredLocation` field |
| `language/builtin-introspection.dark` | 0 / 2 | Missing `Builtin.getAllBuiltinFns` |
| `language/runtime-to-programtypes.dark` | 0 / 19 | Shared preamble needs absent `ProgramTypes.Expr` |
| `stdlib/language-tools/parsedFileShape.dark` | 0 / 13 | Shared preamble uses missing `Builtin.parserParseToWrittenTypes` |
| `stdlib/language-tools/semanticTokenization.dark` | 0 / 102 | Shared preamble needs absent `SemanticTokens.TokenType` |

Crypto, SSE, and Stream are concrete gate-removal candidates. Pretty has a
single reproduced failure rather than a whole API absence. Dependency and
shared-preamble failures require narrower probes before assigning public
semantic gaps. These results cover only the listed fixtures on this host.

## Validation for this audit

- The pinned native toolchain environment check passed.
- `env -u LD_PRELOAD ./build --ai` passed.
- `./run-tests --ai` passed **10,869 / 10,869** assertions (82.8 seconds).
- `dune runtest` passed, including package cache and HTTP/TLS regressions.
- `python3 scripts/audit-upstream-gates.py --check` verifies the inventory against
  every imported file, both runner denysets, duplicate paths/lines, and bounds.
- Python compilation, local document links, and `git diff --check` passed.
- `./benchmarks/run_benchmarks.sh --verify-parent full` was attempted and blocked
  before measurement: “snapshot workload contract digest is incompatible”. No
  baseline was reset and no performance result is claimed.
- No compiler behavior or committed test gate changed. Full disabled-corpus
  execution and cross-platform parity are not implied by this audit.
