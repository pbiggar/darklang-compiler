# Compatibility test failure ledger

Audited on 2026-10-08 (UTC), against compiler revision `4a61318019fb8dea7f34a0bfec4df33f1fa18f5c` on Linux x86-64.

**Every currently excluded assertion was rerun: 2,327 assertions across 55 files.**
**13 assertions have been enabled since the initial audit; 2,327 still fail.**

Latest results for the audited files, including enabled neighbours: 3,599 assertions, 1,272 passed and 2,327 failed.
The remaining gates cover these exact failures: 34 whole files and 258 assertion lines across 21 mixed files.

The assertion harness now compares original parsed expressions. Multiline applications and literal contents are preserved in individual runs and batches. A follow-up retest of `stdlib/dict.dark` passed all 140 assertions after L21 was changed to expect its existing compile-time key type error.

A follow-up against `36002414730437e871b7d457e76b79f859935fb1` recovered `Builtin.unwrap` payload types during lambda lifting. Individual ungated runs passed 191/220 List assertions and 263/264 Int64 assertions. List L161 and Int64 L90 are now enabled; List L162 initially still failed before lambda lifting on an empty list.

Numeric inference follow-up: checking now carries immutable constraints into lambda parameter and body types. The ungated List file passed 192/220 assertions, so L162 is now enabled. Nine focused regressions cover operand order, compound expressions, comparisons, let-bound offsets, and conflicting types. All 55 disabled fixtures were rerun individually against the final compiler; every failing assertion identity matches the updated ledger. The separately retested Dict file passed all 140 assertions.

Explicit numeric types follow-up: List L201 now uses `map2<Int64, Int64, Int64>` because both input lists are empty and subtraction has no numeric type evidence. The explicit Int64 form is now enabled, and the fresh individual List run passed **193/193** enabled assertions. No unused-callback elimination or numeric defaulting was added.

Mixed-list type errors follow-up: List L22–L24 now expect their existing compile-time type errors instead of the interpreter's runtime messages. All three are enabled, and the individual List run passed **196/196** enabled assertions. Compiler behavior is unchanged.

Non-exhaustive callback follow-up: List L93 and L218 retain their original callbacks and now expect `compileerror="Non-exhaustive match expression"`. Both are enabled, and the individual List run passed **198/198** enabled assertions. No fallback match arms or compiler changes were added.

Invalid predicate follow-up: List L53 and L302 now require `compileerror="Expected TBool, got TInt64"` for their unchanged Int64-returning callbacks. Both are enabled, and the individual List run passed **200/200** enabled assertions. Compiler behavior is unchanged.

Verification including the invalid predicate follow-up on main `60f23c91974b3cbc9df18264077afa0339e77bbe`: native build passed; the host suite passed **11,394/11,394** tests; `dune runtest` passed; the canonical compiled leak gate passed **58/58** workloads. The preceding compiler regressions passed individually and batched. The parent benchmark check stopped before measurement because the stored workload digest is incompatible; no baseline was reset.


## Failing test files

Each file links to the individual failing assertions and their observed diagnostics.

| File | Executed | Passed | Failed |
| --- | ---: | ---: | ---: | ---: |
| [cli/app-service-safety.dark](failures/cli/app-service-safety.md) | 9 | 0 | 9 |
| [cli/command-completions.dark](failures/cli/command-completions.md) | 10 | 0 | 10 |
| [cli/deprecation-kinds.dark](failures/cli/deprecation-kinds.md) | 8 | 0 | 8 |
| [cli/include-parsing.dark](failures/cli/include-parsing.md) | 8 | 0 | 8 |
| [cli/outliner.dark](failures/cli/outliner.md) | 70 | 0 | 70 |
| [cli/permissions-display.dark](failures/cli/permissions-display.md) | 2 | 0 | 2 |
| [cli/permissions-grammar.dark](failures/cli/permissions-grammar.md) | 53 | 0 | 53 |
| [cli/tailscale.dark](failures/cli/tailscale.md) | 5 | 0 | 5 |
| [cli/workbench-repl.dark](failures/cli/workbench-repl.md) | 36 | 0 | 36 |
| [cloud/db.dark](failures/cloud/db.md) | 151 | 0 | 151 |
| [language/basic/eor.dark](failures/language/basic/eor.md) | 13 | 12 | 1 |
| [language/basic/evariable.dark](failures/language/basic/evariable.md) | 2 | 1 | 1 |
| [language/builtin-introspection.dark](failures/language/builtin-introspection.md) | 2 | 0 | 2 |
| [language/custom-data/enums.dark](failures/language/custom-data/enums.md) | 38 | 22 | 16 |
| [language/custom-data/values.dark](failures/language/custom-data/values.md) | 72 | 19 | 53 |
| [language/derror.dark](failures/language/derror.md) | 15 | 10 | 5 |
| [language/effect-ceiling.dark](failures/language/effect-ceiling.md) | 6 | 0 | 6 |
| [language/error-type-names.dark](failures/language/error-type-names.md) | 13 | 0 | 13 |
| [language/flow-control/eif.dark](failures/language/flow-control/eif.md) | 16 | 14 | 2 |
| [language/nested-fns.dark](failures/language/nested-fns.md) | 12 | 10 | 2 |
| [language/runtime-to-programtypes.dark](failures/language/runtime-to-programtypes.md) | 19 | 0 | 19 |
| [scm/branch-identity.dark](failures/scm/branch-identity.md) | 8 | 2 | 6 |
| [scm/commit-hash.dark](failures/scm/commit-hash.md) | 9 | 0 | 9 |
| [scm/conflicts.dark](failures/scm/conflicts.md) | 40 | 3 | 37 |
| [scm/constraint-kinds.dark](failures/scm/constraint-kinds.md) | 10 | 0 | 10 |
| [scm/lww.dark](failures/scm/lww.md) | 9 | 0 | 9 |
| [scm/matter-routes.dark](failures/scm/matter-routes.md) | 35 | 0 | 35 |
| [scm/propagation-policy.dark](failures/scm/propagation-policy.md) | 19 | 0 | 19 |
| [scm/removal-conflicts.dark](failures/scm/removal-conflicts.md) | 5 | 0 | 5 |
| [scm/sync-seen-everything.dark](failures/scm/sync-seen-everything.md) | 6 | 0 | 6 |
| [scm/sync-wire.dark](failures/scm/sync-wire.md) | 19 | 0 | 19 |
| [stachu/darklangParser.dark](failures/stachu/darklangParser.md) | 31 | 0 | 31 |
| [stachu/parser.dark](failures/stachu/parser.md) | 56 | 0 | 56 |
| [stachu/tinyLang.dark](failures/stachu/tinyLang.md) | 34 | 0 | 34 |
| [stdlib/base64.dark](failures/stdlib/base64.md) | 41 | 15 | 26 |
| [stdlib/earg.dark](failures/stdlib/earg.md) | 15 | 0 | 15 |
| [stdlib/eself.dark](failures/stdlib/eself.md) | 39 | 0 | 39 |
| [stdlib/float.dark](failures/stdlib/float.md) | 167 | 145 | 22 |
| [stdlib/html.dark](failures/stdlib/html.md) | 99 | 92 | 7 |
| [stdlib/http.dark](failures/stdlib/http.md) | 47 | 21 | 26 |
| [stdlib/httpclient.dark](failures/stdlib/httpclient.md) | 61 | 54 | 7 |
| [stdlib/httpserver.dark](failures/stdlib/httpserver.md) | 7 | 4 | 3 |
| [stdlib/ints/int64.dark](failures/stdlib/ints/int64.md) | 264 | 263 | 1 |
| [stdlib/ints/int8.dark](failures/stdlib/ints/int8.md) | 236 | 235 | 1 |
| [stdlib/json.dark](failures/stdlib/json.md) | 540 | 0 | 540 |
| [stdlib/language-tools/parsedFileShape.dark](failures/stdlib/language-tools/parsedFileShape.md) | 13 | 0 | 13 |
| [stdlib/language-tools/pickLocation.dark](failures/stdlib/language-tools/pickLocation.md) | 30 | 0 | 30 |
| [stdlib/language-tools/semanticTokenization.dark](failures/stdlib/language-tools/semanticTokenization.md) | 102 | 0 | 102 |
| [stdlib/list.dark](failures/stdlib/list.md) | 220 | 200 | 20 |
| [stdlib/math.dark](failures/stdlib/math.md) | 32 | 30 | 2 |
| [stdlib/option.dark](failures/stdlib/option.md) | 73 | 61 | 12 |
| [stdlib/prettyPrinter.dark](failures/stdlib/prettyPrinter.md) | 57 | 0 | 57 |
| [stdlib/result.dark](failures/stdlib/result.md) | 67 | 59 | 8 |
| [stdlib/sqlite.dark](failures/stdlib/sqlite.md) | 8 | 0 | 8 |
| [stdlib/string.dark](failures/stdlib/string.md) | 640 | 0 | 640 |

## Assertions enabled

- [stdlib/pretty.dark:L186](../../test/fixtures/e2e/upstream/stdlib/pretty.dark#L186)
- [stdlib/dict.dark:L21](../../test/fixtures/e2e/upstream/stdlib/dict.dark#L21) — expects a compile-time type error

## Run conditions

Fixtures were copied byte-for-byte to temporary paths within the upstream fixture directory, bypassing exact-path gates. Each file ran individually with `--ai --e2e-batch-size=1`; failures were extracted from complete per-test output and checked against file totals. Original fixture contents were unchanged.

A shared-preamble failure records a failed invocation rather than a measurement of public semantics. Missing test packages, test-only builtins, network services, and compiler failures remain listed by their observed diagnostics. Results describe this host and configuration.
