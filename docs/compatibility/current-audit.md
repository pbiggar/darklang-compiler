# Compatibility test failure ledger

Run on 2026-10-08 (Europe/Rome), against compiler and fixture revision
`7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce` on Linux x86-64.

**Every disabled test was run: 2,589 assertions across 68 files.**
**249 passed; 2,340 failed.** The file links below list each failing test
by its original source line and observed diagnostic.

Before this audit the runner had 44 whole-file gates and 260 line entries in 24 other files.
The fixture parser matched 214 of those line entries to assertions; the other
**46 entries identify no assertion** and are listed separately below.

All assertions in affected files were run, including enabled neighbours:
4,406 executed, 2,066 passed, 2,340 failed.
All failures were among the previously excluded tests.

## Test files

Enablement now matches these results: all 249 passing exclusions are enabled.
The 2,340 failing tests remain excluded through 34 whole-file gates (files with
no passing assertions) and 271 assertion-line gates across 23 mixed files.
The 46 entries that identified no assertion have been removed. The table below
retains the original exclusion counts to show what was tested.

Verification after updating the gates: native rebuild passed; the default host
suite passed **11,118/11,118** tests (249 more than the previous 10,869);
`dune runtest` passed. The live gate set was checked against every measured
failure and matches all 2,340 failing assertion identities exactly.
The parent benchmark check was attempted but stopped before measurement because
the baseline workload digest is incompatible; no baseline was reset.

| File | Disabled tests run | Passed | Failed |
| --- | ---: | ---: | ---: |
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
| [language/apply/eapply.dark](../../test/fixtures/e2e/upstream/language/apply/eapply.dark) | 0 | 0 | 0 |
| [language/basic/eand.dark](../../test/fixtures/e2e/upstream/language/basic/eand.dark) | 2 | 2 | 0 |
| [language/basic/elet.dark](../../test/fixtures/e2e/upstream/language/basic/elet.dark) | 0 | 0 | 0 |
| [language/basic/eor.dark](failures/language/basic/eor.md) | 1 | 0 | 1 |
| [language/basic/estring.dark](../../test/fixtures/e2e/upstream/language/basic/estring.dark) | 2 | 2 | 0 |
| [language/basic/evariable.dark](failures/language/basic/evariable.md) | 1 | 0 | 1 |
| [language/big.dark](../../test/fixtures/e2e/upstream/language/big.dark) | 1 | 1 | 0 |
| [language/builtin-introspection.dark](failures/language/builtin-introspection.md) | 2 | 0 | 2 |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | 1 | 1 | 0 |
| [language/custom-data/enums.dark](failures/language/custom-data/enums.md) | 18 | 2 | 16 |
| [language/custom-data/values.dark](failures/language/custom-data/values.md) | 72 | 19 | 53 |
| [language/derror.dark](failures/language/derror.md) | 10 | 5 | 5 |
| [language/effect-ceiling.dark](failures/language/effect-ceiling.md) | 6 | 0 | 6 |
| [language/error-type-names.dark](failures/language/error-type-names.md) | 13 | 0 | 13 |
| [language/flow-control/eif.dark](failures/language/flow-control/eif.md) | 4 | 2 | 2 |
| [language/nested-fns.dark](failures/language/nested-fns.md) | 2 | 0 | 2 |
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
| [stdlib/base64.dark](failures/stdlib/base64.md) | 26 | 0 | 26 |
| [stdlib/crypto.dark](../../test/fixtures/e2e/upstream/stdlib/crypto.dark) | 9 | 9 | 0 |
| [stdlib/dict.dark](failures/stdlib/dict.md) | 16 | 15 | 1 |
| [stdlib/earg.dark](failures/stdlib/earg.md) | 15 | 0 | 15 |
| [stdlib/eself.dark](failures/stdlib/eself.md) | 39 | 0 | 39 |
| [stdlib/float.dark](failures/stdlib/float.md) | 34 | 12 | 22 |
| [stdlib/html.dark](failures/stdlib/html.md) | 7 | 0 | 7 |
| [stdlib/http.dark](failures/stdlib/http.md) | 47 | 21 | 26 |
| [stdlib/httpclient.dark](failures/stdlib/httpclient.md) | 61 | 54 | 7 |
| [stdlib/httpserver.dark](failures/stdlib/httpserver.md) | 3 | 0 | 3 |
| [stdlib/ints/int32.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int32.dark) | 1 | 1 | 0 |
| [stdlib/ints/int64.dark](failures/stdlib/ints/int64.md) | 5 | 3 | 2 |
| [stdlib/ints/int8.dark](failures/stdlib/ints/int8.md) | 1 | 0 | 1 |
| [stdlib/json.dark](failures/stdlib/json.md) | 540 | 0 | 540 |
| [stdlib/language-tools/parsedFileShape.dark](failures/stdlib/language-tools/parsedFileShape.md) | 13 | 0 | 13 |
| [stdlib/language-tools/pickLocation.dark](failures/stdlib/language-tools/pickLocation.md) | 30 | 0 | 30 |
| [stdlib/language-tools/semanticTokenization.dark](failures/stdlib/language-tools/semanticTokenization.md) | 102 | 0 | 102 |
| [stdlib/list.dark](failures/stdlib/list.md) | 35 | 5 | 30 |
| [stdlib/math.dark](failures/stdlib/math.md) | 2 | 0 | 2 |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | 8 | 8 | 0 |
| [stdlib/option.dark](failures/stdlib/option.md) | 16 | 4 | 12 |
| [stdlib/pretty.dark](failures/stdlib/pretty.md) | 35 | 34 | 1 |
| [stdlib/prettyPrinter.dark](failures/stdlib/prettyPrinter.md) | 57 | 0 | 57 |
| [stdlib/result.dark](failures/stdlib/result.md) | 19 | 11 | 8 |
| [stdlib/sqlite.dark](failures/stdlib/sqlite.md) | 8 | 0 | 8 |
| [stdlib/sse.dark](../../test/fixtures/e2e/upstream/stdlib/sse.dark) | 8 | 8 | 0 |
| [stdlib/stream.dark](../../test/fixtures/e2e/upstream/stdlib/stream.dark) | 25 | 25 | 0 |
| [stdlib/string.dark](failures/stdlib/string.md) | 640 | 0 | 640 |

## Stale individual gate entries

These line entries do not correspond to a parsed assertion. They are neither
passing nor failing tests. The fixture parser, rather than a text heuristic,
was used to check assertion identities.

| File | Entries with no assertion |
| --- | --- |
| [language/apply/eapply.dark](../../test/fixtures/e2e/upstream/language/apply/eapply.dark) | L120 |
| [language/basic/eand.dark](../../test/fixtures/e2e/upstream/language/basic/eand.dark) | L11 |
| [language/basic/elet.dark](../../test/fixtures/e2e/upstream/language/basic/elet.dark) | L67 |
| [language/basic/eor.dark](../../test/fixtures/e2e/upstream/language/basic/eor.dark) | L16, L17 |
| [language/basic/estring.dark](../../test/fixtures/e2e/upstream/language/basic/estring.dark) | L11, L17 |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | L39, L148, L149, L159, L175, L177 |
| [language/custom-data/enums.dark](../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) | L109 |
| [language/flow-control/eif.dark](../../test/fixtures/e2e/upstream/language/flow-control/eif.dark) | L20 |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | L61, L74, L144, L145, L146, L227, L237, L242, L245, L273, L311, L315, L327, L332, L387, L389, L391, L394, L398, L400, L402, L406, L408 |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | L302, L304, L370, L372, L373, L374, L418, L419 |

## Run conditions

Fixtures were copied byte-for-byte to temporary paths within the upstream
fixture directory, bypassing the runner’s exact-path whole-file and line
gates. Each file ran separately with `--ai --e2e-batch-size=1` and a filter
for its copied path. Original fixture contents and committed gates were not
changed. Assertion names and line numbers were read with the runner’s own
`E2EFormat` parser. Every reported failure was extracted from complete
per-test runner output and checked against file totals.

A shared-preamble failure is a failed test invocation, not a measurement
of that test’s public semantics. Missing interpreter test-package names,
test-only builtins, network services, and genuine compiler failures remain
visible as observed failures; no source-level support claim replaces a
test result. This run used the repository’s imported/adapted fixtures and
the compiler’s existing default package configuration.

No compiler fixes are included. Test enablement was updated to these results.
Results describe this host and configuration.
