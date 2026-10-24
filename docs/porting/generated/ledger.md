# Current compiler port status

[How to read and update this ledger](../README.md).

Interpreter revision: `06be3203f31820bcaa99db7bca6756cda3ae5c8d`.

The complete corpus contains 433 source files. Items are counted directly from parsed source annotations.

A run disposition means selected, not a passing execution receipt. Replacement contracts are checked independently.

| Area | Source dispositions |
| --- | --- |
| packages | 3,235 blocked, 2 excluded, 35 replace, 431 run |
| stdlib | 848 blocked, 3 excluded, 1,142 replace, 5 run |
| tests | 1,519 run, 840 xfail |

## Expected test failures

Verified AOT rejection contracts cover programs rejected under the compiler’s static rules; they are not unfinished support.
Missing support includes compiler bugs, unavailable dependencies and test-runner gaps. Counts describe source contracts; execution receipts remain separate.

Upstream bugs record interpreter behavior or test expectations that the compiler deliberately rejects as incorrect; they are separate from missing support.

| Class | Compile | Runtime | Total |
| --- | ---: | ---: | ---: |
| `aot-rejection` | 216 | 0 | 216 |
| `missing-support` | 617 | 6 | 623 |
| `upstream-bug` | 0 | 1 | 1 |

| Class | Reason | Tests |
| --- | --- | ---: |
| `aot-rejection` | `aot-arity-error` | 12 |
| `aot-rejection` | `aot-invalid-field` | 17 |
| `aot-rejection` | `aot-invalid-key` | 8 |
| `aot-rejection` | `aot-match-error` | 26 |
| `aot-rejection` | `aot-name-error` | 11 |
| `aot-rejection` | `aot-type-error` | 142 |
| `missing-support` | `compiler-gap` | 29 |
| `missing-support` | `test-counter` | 7 |
| `missing-support` | `upstream-dependency` | 587 |
| `upstream-bug` | `upstream-bug` | 1 |

### Remaining support by source

Each row links to the authoritative per-test annotations. A named test remains one case even when it contains several assertions.

| Source | Tests |
| --- | ---: |
| [packages/darklang/tests/cli/builtinGroups.dark](../../../upstream/packages/darklang/tests/cli/builtinGroups.dark) | 4 |
| [packages/darklang/tests/cli/commandCompletions.dark](../../../upstream/packages/darklang/tests/cli/commandCompletions.dark) | 8 |
| [packages/darklang/tests/cli/commandGroups.dark](../../../upstream/packages/darklang/tests/cli/commandGroups.dark) | 5 |
| [packages/darklang/tests/cli/deprecationKinds.dark](../../../upstream/packages/darklang/tests/cli/deprecationKinds.dark) | 4 |
| [packages/darklang/tests/cli/golden.dark](../../../upstream/packages/darklang/tests/cli/golden.dark) | 2 |
| [packages/darklang/tests/cli/includeParsing.dark](../../../upstream/packages/darklang/tests/cli/includeParsing.dark) | 8 |
| [packages/darklang/tests/cli/instance-tests.dark](../../../upstream/packages/darklang/tests/cli/instance-tests.dark) | 4 |
| [packages/darklang/tests/cli/interactiveTests.dark](../../../upstream/packages/darklang/tests/cli/interactiveTests.dark) | 4 |
| [packages/darklang/tests/cli/isolation.dark](../../../upstream/packages/darklang/tests/cli/isolation.dark) | 11 |
| [packages/darklang/tests/cli/listing.dark](../../../upstream/packages/darklang/tests/cli/listing.dark) | 1 |
| [packages/darklang/tests/cli/outliner.dark](../../../upstream/packages/darklang/tests/cli/outliner.dark) | 46 |
| [packages/darklang/tests/cli/permissionsDisplay.dark](../../../upstream/packages/darklang/tests/cli/permissionsDisplay.dark) | 2 |
| [packages/darklang/tests/cli/permissionsGrammar.dark](../../../upstream/packages/darklang/tests/cli/permissionsGrammar.dark) | 15 |
| [packages/darklang/tests/cli/propagate.dark](../../../upstream/packages/darklang/tests/cli/propagate.dark) | 1 |
| [packages/darklang/tests/cli/releaseAssets.dark](../../../upstream/packages/darklang/tests/cli/releaseAssets.dark) | 6 |
| [packages/darklang/tests/cli/searchRanking.dark](../../../upstream/packages/darklang/tests/cli/searchRanking.dark) | 5 |
| [packages/darklang/tests/cli/tests-picker.dark](../../../upstream/packages/darklang/tests/cli/tests-picker.dark) | 4 |
| [packages/darklang/tests/cli/tests-tui.dark](../../../upstream/packages/darklang/tests/cli/tests-tui.dark) | 37 |
| [packages/darklang/tests/cli/tests-workbench.dark](../../../upstream/packages/darklang/tests/cli/tests-workbench.dark) | 2 |
| [packages/darklang/tests/cli/tests.dark](../../../upstream/packages/darklang/tests/cli/tests.dark) | 45 |
| [packages/darklang/tests/cli/tracesRendering.dark](../../../upstream/packages/darklang/tests/cli/tracesRendering.dark) | 25 |
| [packages/darklang/tests/cli/workbenchRepl.dark](../../../upstream/packages/darklang/tests/cli/workbenchRepl.dark) | 16 |
| [packages/darklang/tests/language/aliases.dark](../../../upstream/packages/darklang/tests/language/aliases.dark) | 2 |
| [packages/darklang/tests/language/apply.dark](../../../upstream/packages/darklang/tests/language/apply.dark) | 2 |
| [packages/darklang/tests/language/archivedSelf.dark](../../../upstream/packages/darklang/tests/language/archivedSelf.dark) | 1 |
| [packages/darklang/tests/language/equality.dark](../../../upstream/packages/darklang/tests/language/equality.dark) | 2 |
| [packages/darklang/tests/language/errorTypeNames.dark](../../../upstream/packages/darklang/tests/language/errorTypeNames.dark) | 5 |
| [packages/darklang/tests/language/eself.dark](../../../upstream/packages/darklang/tests/language/eself.dark) | 2 |
| [packages/darklang/tests/language/fastOps.dark](../../../upstream/packages/darklang/tests/language/fastOps.dark) | 4 |
| [packages/darklang/tests/language/lambdas.dark](../../../upstream/packages/darklang/tests/language/lambdas.dark) | 1 |
| [packages/darklang/tests/language/letBindings.dark](../../../upstream/packages/darklang/tests/language/letBindings.dark) | 1 |
| [packages/darklang/tests/language/operators.dark](../../../upstream/packages/darklang/tests/language/operators.dark) | 1 |
| [packages/darklang/tests/language/records.dark](../../../upstream/packages/darklang/tests/language/records.dark) | 2 |
| [packages/darklang/tests/language/runtimeToProgramTypes.dark](../../../upstream/packages/darklang/tests/language/runtimeToProgramTypes.dark) | 5 |
| [packages/darklang/tests/language/traits.dark](../../../upstream/packages/darklang/tests/language/traits.dark) | 17 |
| [packages/darklang/tests/language/tupleLiterals.dark](../../../upstream/packages/darklang/tests/language/tupleLiterals.dark) | 1 |
| [packages/darklang/tests/language/unwrap.dark](../../../upstream/packages/darklang/tests/language/unwrap.dark) | 5 |
| [packages/darklang/tests/language/values.dark](../../../upstream/packages/darklang/tests/language/values.dark) | 1 |
| [packages/darklang/tests/languageTools/atRestTypeChecker.dark](../../../upstream/packages/darklang/tests/languageTools/atRestTypeChecker.dark) | 172 |
| [packages/darklang/tests/languageTools/builtinIntrospection.dark](../../../upstream/packages/darklang/tests/languageTools/builtinIntrospection.dark) | 3 |
| [packages/darklang/tests/languageTools/parsedFileShape.dark](../../../upstream/packages/darklang/tests/languageTools/parsedFileShape.dark) | 5 |
| [packages/darklang/tests/languageTools/perf.dark](../../../upstream/packages/darklang/tests/languageTools/perf.dark) | 5 |
| [packages/darklang/tests/languageTools/pickLocation.dark](../../../upstream/packages/darklang/tests/languageTools/pickLocation.dark) | 8 |
| [packages/darklang/tests/languageTools/semanticTokenization.dark](../../../upstream/packages/darklang/tests/languageTools/semanticTokenization.dark) | 9 |
| [packages/darklang/tests/prettyPrinter/runtimeTypes.dark](../../../upstream/packages/darklang/tests/prettyPrinter/runtimeTypes.dark) | 11 |
| [packages/darklang/tests/scm/branchIdentity.dark](../../../upstream/packages/darklang/tests/scm/branchIdentity.dark) | 5 |
| [packages/darklang/tests/scm/commitHash.dark](../../../upstream/packages/darklang/tests/scm/commitHash.dark) | 5 |
| [packages/darklang/tests/scm/conflicts.dark](../../../upstream/packages/darklang/tests/scm/conflicts.dark) | 30 |
| [packages/darklang/tests/scm/constraintKinds.dark](../../../upstream/packages/darklang/tests/scm/constraintKinds.dark) | 4 |
| [packages/darklang/tests/scm/lww.dark](../../../upstream/packages/darklang/tests/scm/lww.dark) | 6 |
| [packages/darklang/tests/scm/removalConflicts.dark](../../../upstream/packages/darklang/tests/scm/removalConflicts.dark) | 5 |
| [packages/darklang/tests/stdlib/altJson.dark](../../../upstream/packages/darklang/tests/stdlib/altJson.dark) | 7 |
| [packages/darklang/tests/stdlib/cli/sgr.dark](../../../upstream/packages/darklang/tests/stdlib/cli/sgr.dark) | 3 |
| [packages/darklang/tests/stdlib/dict.dark](../../../upstream/packages/darklang/tests/stdlib/dict.dark) | 8 |
| [packages/darklang/tests/stdlib/discovery.dark](../../../upstream/packages/darklang/tests/stdlib/discovery.dark) | 2 |
| [packages/darklang/tests/stdlib/httpclient.dark](../../../upstream/packages/darklang/tests/stdlib/httpclient.dark) | 3 |
| [packages/darklang/tests/stdlib/ints/int8.dark](../../../upstream/packages/darklang/tests/stdlib/ints/int8.dark) | 1 |
| [packages/darklang/tests/stdlib/json.dark](../../../upstream/packages/darklang/tests/stdlib/json.dark) | 3 |
| [packages/darklang/tests/stdlib/list.dark](../../../upstream/packages/darklang/tests/stdlib/list.dark) | 9 |
| [packages/darklang/tests/stdlib/live.dark](../../../upstream/packages/darklang/tests/stdlib/live.dark) | 2 |
| [packages/darklang/tests/stdlib/pretty.dark](../../../upstream/packages/darklang/tests/stdlib/pretty.dark) | 1 |
| [packages/darklang/tests/stdlib/sqlite.dark](../../../upstream/packages/darklang/tests/stdlib/sqlite.dark) | 7 |
| [packages/darklang/tests/stdlib/string.dark](../../../upstream/packages/darklang/tests/stdlib/string.dark) | 2 |

| Kind | Items |
| --- | --- |
| assertion | 239 |
| expression | 2 |
| function | 4,460 |
| implementation | 214 |
| named-test | 2,120 |
| trait | 21 |
| trait-method | 25 |
| type | 707 |
| val | 272 |

## Reasons

Tags are defined in [the glossary](../../../porting/compiler-reasons.json).

| Area | Tag | Items |
| --- | --- | --- |
| packages | `compiler-contract` | 12 |
| packages | `compiler-gap` | 7 |
| packages | `compiler-implementation` | 34 |
| packages | `constructor-identity` | 1 |
| packages | `container-adaptation` | 28 |
| packages | `missing-implementation` | 3,023 |
| packages | `runtime-reflection` | 2 |
| packages | `test-counter` | 1 |
| packages | `test-diagnostics` | 1 |
| packages | `upstream-dependency` | 161 |
| packages | `value-specialization` | 2 |
| stdlib | `compiler-contract` | 6 |
| stdlib | `compiler-implementation` | 1,139 |
| stdlib | `container-adaptation` | 490 |
| stdlib | `interpreter-intrinsic` | 1 |
| stdlib | `missing-implementation` | 350 |
| stdlib | `runtime-reflection` | 3 |
| stdlib | `test-counter` | 2 |
| stdlib | `test-diagnostics` | 2 |
| tests | `aot-arity-error` | 12 |
| tests | `aot-invalid-field` | 17 |
| tests | `aot-invalid-key` | 8 |
| tests | `aot-match-error` | 26 |
| tests | `aot-name-error` | 11 |
| tests | `aot-type-error` | 142 |
| tests | `compiler-gap` | 29 |
| tests | `diagnostic-difference` | 2 |
| tests | `test-counter` | 7 |
| tests | `upstream-bug` | 1 |
| tests | `upstream-dependency` | 587 |

## Separate capability assessments

| Area | Assessments |
| --- | --- |
| features | 6 excluded, 9 missing, 1 partial, 199 supported |
| parser | 1 partial, 30 supported |
| builtins | 169 missing, 79 present, 533 unassessed |
