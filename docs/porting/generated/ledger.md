# Repeatable compatibility ledger

Generated from pinned sources and maintained decisions. No compatibility probes or new tests are generated.

Upstream: `a9d93efc853486244630dad693fe79f6f4882cf6`; target: `linux-x64`.

## Language features

| Feature | Status | Review needed |
| --- | --- | --- |
| Async | missing | no |
| Bindings, functions and closures | implemented | no |
| Effects | partial | no |
| Generic functions and algebraic types | implemented | no |
| Lists, dictionaries and tuples | implemented | no |
| Modules and name resolution | implemented | no |
| Named package tests | partial | no |
| Pattern matching | implemented | no |
| Primitive literals and numeric types | implemented | no |
| Recursion | implemented | no |
| Runtime reflection and dynamic evaluation | missing | no |
| Traits | missing | no |

## Source equivalency

| Ledger | Identical | Different | Missing | Compiler-only |
| --- | ---: | ---: | ---: | ---: |
| stdlib | 5 | 59 | 34 | 198 |
| packages | 5 | 67 | 352 | 4 |

## Existing test references

Lexical references are coverage hints, not evidence of execution. One deterministic witness (the shortest indirect path) and the total reference count per declaration are in `porting/generated/ledger.json`.

| Direct | Indirect | Maintained | Not identified |
| ---: | ---: | ---: | ---: |
| 1524 | 3144 | 170 | 683 |

Package compilation state: **not-run**. Original declarations are parsed, checked and lowered through the native preamble pipeline; no entry program is synthesized or linked.

| Compiled | Fail | Blocked | Stale | Not run |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 0 | 424 |

## Parser and builtins

Builtin mappings distinguish callable Builtin declarations from source implementation counterparts. A counterpart is not a callable alias or a behavior pass. Computed registrations remain explicit review items.

- parser: compiler-only: 17, maintained: 9
- builtins: declared-builtin: 75, maintained: 4, missing: 235, source-counterpart: 466
- Computed builtin constructor templates: 7 (reviewed literal call sites expanded; not an assembled registry)

## Existing test execution

State: **stale**.

Imported legacy cases with current execution receipts:

| Pass | Fail | Skipped | Blocked |
| ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 0 |

Named package tests: 2107 declarations, execution blocked (not silently skipped).

## Review queue

- Missing source counterpart: packages:packages/darklang/cli/accepts.dark
- Missing source counterpart: packages:packages/darklang/cli/backups.dark
- Missing source counterpart: packages:packages/darklang/cli/builtins.dark
- Missing source counterpart: packages:packages/darklang/cli/clear.dark
- Missing source counterpart: packages:packages/darklang/cli/completionPicker.dark
- Missing source counterpart: packages:packages/darklang/cli/component.dark
- Missing source counterpart: packages:packages/darklang/cli/config.dark
- Missing source counterpart: packages:packages/darklang/cli/constraints.dark
- Missing source counterpart: packages:packages/darklang/cli/core.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/cli.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/command.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/deprecation.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/enums.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/errors.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/for-ai-internal.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/for-ai.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/functions.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/http-server.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/interactive-testing.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/live.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/operators.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/packages.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/pattern-matching.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/processes.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/records.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/scm.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/stdlib.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/stdlibOverview.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/syntax.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/terminalUi.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/traits.dark
- Missing source counterpart: packages:packages/darklang/cli/docs/types.dark
- Missing source counterpart: packages:packages/darklang/cli/entry.dark
- Missing source counterpart: packages:packages/darklang/cli/execution/eval.dark
- Missing source counterpart: packages:packages/darklang/cli/execution/repl.dark
- Missing source counterpart: packages:packages/darklang/cli/execution/run.dark
- Missing source counterpart: packages:packages/darklang/cli/exportSeed.dark
- Missing source counterpart: packages:packages/darklang/cli/help.dark
- Missing source counterpart: packages:packages/darklang/cli/http/serve.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/config.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/download.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/helpers.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/install.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/status.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/system.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/uninstall.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/update.dark
- Missing source counterpart: packages:packages/darklang/cli/installation/version.dark
- Missing source counterpart: packages:packages/darklang/cli/liveview/host.dark
- Missing source counterpart: packages:packages/darklang/cli/liveview/model.dark
- Missing source counterpart: packages:packages/darklang/cli/liveview/runner.dark
- Missing source counterpart: packages:packages/darklang/cli/loop.dark
- Missing source counterpart: packages:packages/darklang/cli/outliner/app.dark
- Missing source counterpart: packages:packages/darklang/cli/outliner/commands.dark
- Missing source counterpart: packages:packages/darklang/cli/outliner/core.dark
- Missing source counterpart: packages:packages/darklang/cli/outliner/editor.dark
- Missing source counterpart: packages:packages/darklang/cli/outliner/main.dark
- Missing source counterpart: packages:packages/darklang/cli/outliner/store.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/atRestCheck.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/back.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/core.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/delete.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/deprecate.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/display.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/edit.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/errors.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/findValues.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/fn.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/hash.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/impl.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/impls.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/listing.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/location.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/module.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/nav.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/navInteractive.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/propagate.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/query.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/rename.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/search.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/signatures.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/test.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/trait.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/traversal.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/tree.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/type.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/typecheck.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/value.dark
- Missing source counterpart: packages:packages/darklang/cli/packages/view.dark
- Missing source counterpart: packages:packages/darklang/cli/permissions/command.dark
- Missing source counterpart: packages:packages/darklang/cli/permissions/profiles.dark
- Missing source counterpart: packages:packages/darklang/cli/prompt.dark
- Missing source counterpart: packages:packages/darklang/cli/ps.dark
- Missing source counterpart: packages:packages/darklang/cli/quit.dark
- Missing source counterpart: packages:packages/darklang/cli/registry.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/branch.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/branchHistory.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/branchMerge.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/commit.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/commits.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/conflicts.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/deps.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/discard.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/ops.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/propagate.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/scmjson.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/status.dark
- Missing source counterpart: packages:packages/darklang/cli/scm/undo.dark
- Missing source counterpart: packages:packages/darklang/cli/telemetry.dark
- Missing source counterpart: packages:packages/darklang/cli/traceVerbs.dark
- Missing source counterpart: packages:packages/darklang/cli/traceWatch.dark
- Missing source counterpart: packages:packages/darklang/cli/tracing.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/completion.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/flagParser.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/glyphs.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/listing.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/logo.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/relativeTime.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/syntaxHighlighting.dark
- Missing source counterpart: packages:packages/darklang/cli/utils/text.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/app.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/authoring.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/branches.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/chrome.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/detail.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/frame-assembly.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/frame.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/home.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/input.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/keys-view.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/keys.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/loaders-misc.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/loaders.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/nav.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/navbar.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/render.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/scm-status.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/types.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/viewdefs.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/views-misc.dark
- Missing source counterpart: packages:packages/darklang/cli/workbench/views-scm-repl.dark
- Missing source counterpart: packages:packages/darklang/dark-packages.dark
- Missing source counterpart: packages:packages/darklang/demo-data/httpserver-test.dark
- Missing source counterpart: packages:packages/darklang/github.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/coverage.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/declarations.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/environment.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/generate.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/impls.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/items.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/model.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/nodeIds.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/run.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/solve.dark
- Missing source counterpart: packages:packages/darklang/languageTools/atRestTypeChecker/types.dark
- Missing source counterpart: packages:packages/darklang/languageTools/interpreterStats.dark
- Missing source counterpart: packages:packages/darklang/languageTools/nameResolver.dark
- Missing source counterpart: packages:packages/darklang/languageTools/parser/cliScript.dark
- Missing source counterpart: packages:packages/darklang/languageTools/parser/core.dark
- Missing source counterpart: packages:packages/darklang/languageTools/parser/parse.dark
- Missing source counterpart: packages:packages/darklang/languageTools/perf.dark
- Missing source counterpart: packages:packages/darklang/languageTools/permissions.dark
- Missing source counterpart: packages:packages/darklang/languageTools/runtimeErrors.dark
- Missing source counterpart: packages:packages/darklang/languageTools/runtimeTypesToProgramTypes.dark
- Missing source counterpart: packages:packages/darklang/languageTools/semanticTokens.dark
- Missing source counterpart: packages:packages/darklang/languageTools/testWorker.dark
- Missing source counterpart: packages:packages/darklang/languageTools/writtenTypes.dark
- Missing source counterpart: packages:packages/darklang/languageTools/writtenTypesToProgramTypes.dark
- Missing source counterpart: packages:packages/darklang/prettyPrinter/ast.dark
- Missing source counterpart: packages:packages/darklang/prettyPrinter/cliScript.dark
- Missing source counterpart: packages:packages/darklang/prettyPrinter/common.dark
- Missing source counterpart: packages:packages/darklang/prettyPrinter/instructions.dark
- Missing source counterpart: packages:packages/darklang/prettyPrinter/moduleDeclaration.dark
- Missing source counterpart: packages:packages/darklang/prettyPrinter/programTypes.dark
- Missing source counterpart: packages:packages/darklang/scm/branches.dark
- Missing source counterpart: packages:packages/darklang/scm/commits.dark
- Missing source counterpart: packages:packages/darklang/scm/conflicts.dark
- Missing source counterpart: packages:packages/darklang/scm/constraints.dark
- Missing source counterpart: packages:packages/darklang/scm/deps.dark
- Missing source counterpart: packages:packages/darklang/scm/docs.dark
- Missing source counterpart: packages:packages/darklang/scm/draft.dark
- Missing source counterpart: packages:packages/darklang/scm/draftRewrite.dark
- Missing source counterpart: packages:packages/darklang/scm/lineage.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOps.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOpsBindings.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOpsBranch.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOpsCommits.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOpsDraft.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOpsMerge.dark
- Missing source counterpart: packages:packages/darklang/scm/packageOpsRepoints.dark
- Missing source counterpart: packages:packages/darklang/scm/partialCommit.dark
- Missing source counterpart: packages:packages/darklang/scm/propagation.dark
- Missing source counterpart: packages:packages/darklang/scm/scm.dark
- Missing source counterpart: packages:packages/darklang/scm/storeHealth.dark
- Missing source counterpart: packages:packages/darklang/stdlib/await.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/bash.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/curl.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/fish.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/powershell.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/tui/core.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/tui/frame.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/tui/inlineFrame.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/tui/layout.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/tui/terminalSession.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/box.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/canvas.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/chrome.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/confirm.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/editor.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/html.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/layout.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/listview.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/node.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/picker.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/splitpane.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/text.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/ui/widget.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/unix.dark
- Missing source counterpart: packages:packages/darklang/stdlib/cli/zsh.dark
- Missing source counterpart: packages:packages/darklang/stdlib/exec.dark
- Missing source counterpart: packages:packages/darklang/stdlib/execPolicy.dark
- Missing source counterpart: packages:packages/darklang/stdlib/hostAwait.dark
- Missing source counterpart: packages:packages/darklang/stdlib/live.dark
- Missing source counterpart: packages:packages/darklang/stdlib/localStore.dark
- Missing source counterpart: packages:packages/darklang/stdlib/sqlite.dark
- Missing source counterpart: packages:packages/darklang/stdlib/test.dark
- Missing source counterpart: packages:packages/darklang/stdlib/testProcess.dark
- Missing source counterpart: packages:packages/darklang/stdlib/traits.dark
- Missing source counterpart: packages:packages/darklang/test/test.dark
- Missing source counterpart: packages:packages/darklang/test/test_earg.dark
- Missing source counterpart: packages:packages/darklang/test/test_eself.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/commandCompletions.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/deprecationKinds.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/file.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/golden.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/includeParsing.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/instance-tests.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/instance.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/interactive.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/interactiveTests.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/isolation.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/listing.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/outliner.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/permissionsDisplay.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/permissionsGrammar.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/propagate.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/releaseAssets.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/searchRanking.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/tests-picker.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/tests-tui.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/tests-workbench.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/tests.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/tracesRendering.dark
- Missing source counterpart: packages:packages/darklang/tests/cli/workbenchRepl.dark
- Missing source counterpart: packages:packages/darklang/tests/language/aliases.dark
- Missing source counterpart: packages:packages/darklang/tests/language/apply.dark
- Missing source counterpart: packages:packages/darklang/tests/language/archivedArgs.dark
- Missing source counterpart: packages:packages/darklang/tests/language/archivedSelf.dark
- Missing source counterpart: packages:packages/darklang/tests/language/basic.dark
- Missing source counterpart: packages:packages/darklang/tests/language/big.dark
- Missing source counterpart: packages:packages/darklang/tests/language/collections.dark
- Missing source counterpart: packages:packages/darklang/tests/language/conditionals.dark
- Missing source counterpart: packages:packages/darklang/tests/language/earg.dark
- Missing source counterpart: packages:packages/darklang/tests/language/effectCeiling.dark
- Missing source counterpart: packages:packages/darklang/tests/language/enums.dark
- Missing source counterpart: packages:packages/darklang/tests/language/equality.dark
- Missing source counterpart: packages:packages/darklang/tests/language/errorPropagation.dark
- Missing source counterpart: packages:packages/darklang/tests/language/errorTypeNames.dark
- Missing source counterpart: packages:packages/darklang/tests/language/eself.dark
- Missing source counterpart: packages:packages/darklang/tests/language/fastOps.dark
- Missing source counterpart: packages:packages/darklang/tests/language/interpreter.dark
- Missing source counterpart: packages:packages/darklang/tests/language/lambdas.dark
- Missing source counterpart: packages:packages/darklang/tests/language/letBindings.dark
- Missing source counterpart: packages:packages/darklang/tests/language/matchExpressions.dark
- Missing source counterpart: packages:packages/darklang/tests/language/nestedFunctions.dark
- Missing source counterpart: packages:packages/darklang/tests/language/operators.dark
- Missing source counterpart: packages:packages/darklang/tests/language/pipe.dark
- Missing source counterpart: packages:packages/darklang/tests/language/records.dark
- Missing source counterpart: packages:packages/darklang/tests/language/runtimeToProgramTypes.dark
- Missing source counterpart: packages:packages/darklang/tests/language/semantics.dark
- Missing source counterpart: packages:packages/darklang/tests/language/stringInterpolation.dark
- Missing source counterpart: packages:packages/darklang/tests/language/traits.dark
- Missing source counterpart: packages:packages/darklang/tests/language/tupleLiterals.dark
- Missing source counterpart: packages:packages/darklang/tests/language/unwrap.dark
- Missing source counterpart: packages:packages/darklang/tests/language/values.dark
- Missing source counterpart: packages:packages/darklang/tests/languageTools/atRestTypeChecker.dark
- Missing source counterpart: packages:packages/darklang/tests/languageTools/builtinIntrospection.dark
- Missing source counterpart: packages:packages/darklang/tests/languageTools/parsedFileShape.dark
- Missing source counterpart: packages:packages/darklang/tests/languageTools/perf.dark
- Missing source counterpart: packages:packages/darklang/tests/languageTools/pickLocation.dark
- Missing source counterpart: packages:packages/darklang/tests/languageTools/semanticTokenization.dark
- Missing source counterpart: packages:packages/darklang/tests/prettyPrinter/runtimeTypes.dark
- Missing source counterpart: packages:packages/darklang/tests/scm/branchIdentity.dark
- Missing source counterpart: packages:packages/darklang/tests/scm/commitHash.dark
- Missing source counterpart: packages:packages/darklang/tests/scm/conflicts.dark
- Missing source counterpart: packages:packages/darklang/tests/scm/constraintKinds.dark
- Missing source counterpart: packages:packages/darklang/tests/scm/lww.dark
- Missing source counterpart: packages:packages/darklang/tests/scm/removalConflicts.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/altJson.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/base64.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/bool.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/bytes.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/char.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/cli/color.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/cli/glob.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/cli/path.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/cli/process.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/cli/sgr.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/cli/tuiText.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/crypto.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/dateTime.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/dict.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/diff.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/discovery.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/duration.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/float.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/html.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/http.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/httpclient.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/httpserver.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/int.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/int128.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/int16.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/int32.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/int64.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/int8.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/uint128.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/uint16.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/uint32.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/uint64.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/ints/uint8.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/json.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/list.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/live.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/math.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/option.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/pretty.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/regex.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/result.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/sqlite.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/sse.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/stream.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/string.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/test.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/tuple.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/uuid.dark
- Missing source counterpart: packages:packages/darklang/tests/stdlib/x509.dark
- Missing source counterpart: packages:packages/darklang/tracing/format.dark
- Missing source counterpart: packages:packages/darklang/tracing/store.dark
- Missing source counterpart: packages:packages/darklang/tracing/types.dark
- Missing source counterpart: packages:packages/internal/tests.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/await.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/bash.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/curl.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/fish.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/powershell.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/tui/core.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/tui/frame.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/tui/inlineFrame.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/tui/layout.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/tui/terminalSession.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/box.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/canvas.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/chrome.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/confirm.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/editor.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/html.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/layout.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/listview.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/node.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/picker.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/splitpane.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/text.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/ui/widget.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/unix.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/cli/zsh.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/exec.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/execPolicy.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/hostAwait.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/live.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/localStore.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/sqlite.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/test.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/testProcess.dark
- Missing source counterpart: stdlib:packages/darklang/stdlib/traits.dark
- Source difference: packages:packages/darklang/cli/utils/terminal.dark
- Source difference: packages:packages/darklang/languageTools/common.dark
- Source difference: packages:packages/darklang/languageTools/packageManager.dark
- Source difference: packages:packages/darklang/languageTools/programTypes.dark
- Source difference: packages:packages/darklang/languageTools/runtimeTypes.dark
- Source difference: packages:packages/darklang/prettyPrinter/runtimeError.dark
- Source difference: packages:packages/darklang/prettyPrinter/runtimeTypes.dark
- Source difference: packages:packages/darklang/scm/branch.dark
- Source difference: packages:packages/darklang/stdlib/alt-json.dark
- Source difference: packages:packages/darklang/stdlib/base64.dark
- Source difference: packages:packages/darklang/stdlib/blob.dark
- Source difference: packages:packages/darklang/stdlib/bool.dark
- Source difference: packages:packages/darklang/stdlib/char.dark
- Source difference: packages:packages/darklang/stdlib/cli/dir.dark
- Source difference: packages:packages/darklang/stdlib/cli/fileSystem.dark
- Source difference: packages:packages/darklang/stdlib/cli/host.dark
- Source difference: packages:packages/darklang/stdlib/cli/log.dark
- Source difference: packages:packages/darklang/stdlib/cli/path.dark
- Source difference: packages:packages/darklang/stdlib/cli/posix.dark
- Source difference: packages:packages/darklang/stdlib/cli/stdin.dark
- Source difference: packages:packages/darklang/stdlib/cli/tui/terminalSupport.dark
- Source difference: packages:packages/darklang/stdlib/cli/tui/text.dark
- Source difference: packages:packages/darklang/stdlib/cli/ui/colors.dark
- Source difference: packages:packages/darklang/stdlib/cli/ui/progress.dark
- Source difference: packages:packages/darklang/stdlib/cli/ui/prompt.dark
- Source difference: packages:packages/darklang/stdlib/cli/ui/table.dark
- Source difference: packages:packages/darklang/stdlib/cli/ui/textfield.dark
- Source difference: packages:packages/darklang/stdlib/crypto.dark
- Source difference: packages:packages/darklang/stdlib/dateTime.dark
- Source difference: packages:packages/darklang/stdlib/dict.dark
- Source difference: packages:packages/darklang/stdlib/diff.dark
- Source difference: packages:packages/darklang/stdlib/duration.dark
- Source difference: packages:packages/darklang/stdlib/env.dark
- Source difference: packages:packages/darklang/stdlib/float.dark
- Source difference: packages:packages/darklang/stdlib/fun.dark
- Source difference: packages:packages/darklang/stdlib/html.dark
- Source difference: packages:packages/darklang/stdlib/http.dark
- Source difference: packages:packages/darklang/stdlib/httpclient.dark
- Source difference: packages:packages/darklang/stdlib/httpserver.dark
- Source difference: packages:packages/darklang/stdlib/int.dark
- Source difference: packages:packages/darklang/stdlib/int128.dark
- Source difference: packages:packages/darklang/stdlib/int16.dark
- Source difference: packages:packages/darklang/stdlib/int32.dark
- Source difference: packages:packages/darklang/stdlib/int64.dark
- Source difference: packages:packages/darklang/stdlib/int8.dark
- Source difference: packages:packages/darklang/stdlib/json.dark
- Source difference: packages:packages/darklang/stdlib/list.dark
- Source difference: packages:packages/darklang/stdlib/math.dark
- Source difference: packages:packages/darklang/stdlib/noModule.dark
- Source difference: packages:packages/darklang/stdlib/option.dark
- Source difference: packages:packages/darklang/stdlib/pretty.dark
- Source difference: packages:packages/darklang/stdlib/print.dark
- Source difference: packages:packages/darklang/stdlib/regex.dark
- Source difference: packages:packages/darklang/stdlib/result.dark
- Source difference: packages:packages/darklang/stdlib/retry.dark
- Source difference: packages:packages/darklang/stdlib/stream.dark
- Source difference: packages:packages/darklang/stdlib/string.dark
- Source difference: packages:packages/darklang/stdlib/tuple2.dark
- Source difference: packages:packages/darklang/stdlib/tuple3.dark
- Source difference: packages:packages/darklang/stdlib/uint128.dark
- Source difference: packages:packages/darklang/stdlib/uint16.dark
- Source difference: packages:packages/darklang/stdlib/uint32.dark
- Source difference: packages:packages/darklang/stdlib/uint64.dark
- Source difference: packages:packages/darklang/stdlib/uint8.dark
- Source difference: packages:packages/darklang/stdlib/uuid.dark
- Source difference: packages:packages/darklang/stdlib/valueSearch.dark
- Source difference: packages:packages/darklang/stdlib/x509.dark
- Source difference: stdlib:packages/darklang/stdlib/alt-json.dark
- Source difference: stdlib:packages/darklang/stdlib/base64.dark
- Source difference: stdlib:packages/darklang/stdlib/blob.dark
- Source difference: stdlib:packages/darklang/stdlib/bool.dark
- Source difference: stdlib:packages/darklang/stdlib/char.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/dir.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/fileSystem.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/host.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/log.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/path.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/posix.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/stdin.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/tui/terminalSupport.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/tui/text.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/ui/colors.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/ui/progress.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/ui/prompt.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/ui/table.dark
- Source difference: stdlib:packages/darklang/stdlib/cli/ui/textfield.dark
- Source difference: stdlib:packages/darklang/stdlib/crypto.dark
- Source difference: stdlib:packages/darklang/stdlib/dateTime.dark
- Source difference: stdlib:packages/darklang/stdlib/dict.dark
- Source difference: stdlib:packages/darklang/stdlib/diff.dark
- Source difference: stdlib:packages/darklang/stdlib/duration.dark
- Source difference: stdlib:packages/darklang/stdlib/env.dark
- Source difference: stdlib:packages/darklang/stdlib/float.dark
- Source difference: stdlib:packages/darklang/stdlib/fun.dark
- Source difference: stdlib:packages/darklang/stdlib/html.dark
- Source difference: stdlib:packages/darklang/stdlib/http.dark
- Source difference: stdlib:packages/darklang/stdlib/httpclient.dark
- Source difference: stdlib:packages/darklang/stdlib/httpserver.dark
- Source difference: stdlib:packages/darklang/stdlib/int.dark
- Source difference: stdlib:packages/darklang/stdlib/int128.dark
- Source difference: stdlib:packages/darklang/stdlib/int16.dark
- Source difference: stdlib:packages/darklang/stdlib/int32.dark
- Source difference: stdlib:packages/darklang/stdlib/int64.dark
- Source difference: stdlib:packages/darklang/stdlib/int8.dark
- Source difference: stdlib:packages/darklang/stdlib/json.dark
- Source difference: stdlib:packages/darklang/stdlib/list.dark
- Source difference: stdlib:packages/darklang/stdlib/math.dark
- Source difference: stdlib:packages/darklang/stdlib/noModule.dark
- Source difference: stdlib:packages/darklang/stdlib/option.dark
- Source difference: stdlib:packages/darklang/stdlib/pretty.dark
- Source difference: stdlib:packages/darklang/stdlib/print.dark
- Source difference: stdlib:packages/darklang/stdlib/regex.dark
- Source difference: stdlib:packages/darklang/stdlib/result.dark
- Source difference: stdlib:packages/darklang/stdlib/retry.dark
- Source difference: stdlib:packages/darklang/stdlib/stream.dark
- Source difference: stdlib:packages/darklang/stdlib/string.dark
- Source difference: stdlib:packages/darklang/stdlib/tuple2.dark
- Source difference: stdlib:packages/darklang/stdlib/tuple3.dark
- Source difference: stdlib:packages/darklang/stdlib/uint128.dark
- Source difference: stdlib:packages/darklang/stdlib/uint16.dark
- Source difference: stdlib:packages/darklang/stdlib/uint32.dark
- Source difference: stdlib:packages/darklang/stdlib/uint64.dark
- Source difference: stdlib:packages/darklang/stdlib/uint8.dark
- Source difference: stdlib:packages/darklang/stdlib/uuid.dark
- Source difference: stdlib:packages/darklang/stdlib/valueSearch.dark
- Source difference: stdlib:packages/darklang/stdlib/x509.dark
- ambiguous declaration: Darklang.Language.Tests.Enums.Polymorphism.insert
- ambiguous declaration: Darklang.Language.Tests.Enums.Polymorphism.toList
- ambiguous declaration: Darklang.ShadowingTest.valueShadowing
- ambiguous declaration: test1
- ambiguous declaration: test10
- ambiguous declaration: test11
- ambiguous declaration: test12
- ambiguous declaration: test13
- ambiguous declaration: test14
- ambiguous declaration: test2
- ambiguous declaration: test3
- ambiguous declaration: test4
- ambiguous declaration: test5
- ambiguous declaration: test6
- ambiguous declaration: test7
- ambiguous declaration: test8
- ambiguous declaration: test9
- ambiguous declaration: testList
