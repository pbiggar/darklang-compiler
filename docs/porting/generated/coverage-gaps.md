# Existing-test reference gaps

Regenerated from the pinned source index. These are not failing tests or proof that no test exists.

Review found three recurring cases: numeric trait methods reached implicitly by operators (maintained references record the existing typed suites); archived `darklang/test/test_*` helpers whose newer tests redeclare their own functions; and CLI/host APIs without a discovered path from an existing test. None is silently exempted or supplied with a new test.

Remaining implicit dispatch, reflection, and unfamiliar syntax can require maintained references. References do not establish runtime execution.

## packages/darklang/cli/completionPicker.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.CompletionPicker.KeyResult | type | 194 |
| Darklang.Cli.CompletionPicker.buildCompletedCommand | function | 225 |
| Darklang.Cli.CompletionPicker.getSelectedValue | function | 53 |
| Darklang.Cli.CompletionPicker.handleKeyAtSize | function | 201 |
| Darklang.Cli.CompletionPicker.moveDownWithCapacity | function | 175 |
| Darklang.Cli.CompletionPicker.moveUp | function | 161 |
| Darklang.Cli.CompletionPicker.presentAtSize | function | 248 |
| Darklang.Cli.CompletionPicker.startAtSize | function | 234 |
| Darklang.Cli.CompletionPicker.stop | function | 264 |
| Darklang.Cli.CompletionPicker.unsupportedSizeMessage | value | 25 |
| Darklang.Cli.CompletionPicker.visibleCapacity | function | 85 |

## packages/darklang/cli/config.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Config.getInt | function | 39 |

## packages/darklang/cli/core.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.View.formatWelcome | function | 270 |
| Darklang.Cli.refuseDashLedName | function | 107 |

## packages/darklang/cli/entry.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.executeCliCommand | function | 48 |
| Darklang.Cli.parseBranchFlag | function | 43 |
| Darklang.Cli.takeFlagValue | function | 12 |

## packages/darklang/cli/installation/config.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Installation.Config.readLocallyInstalledDarklangVersion | function | 51 |

## packages/darklang/cli/installation/download.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Installation.Download.basename | function | 11 |

## packages/darklang/cli/installation/system.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Installation.System.installationModeToString | function | 16 |

## packages/darklang/cli/liveview/host.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.LiveView.Host.plainRows | function | 583 |
| Darklang.Cli.LiveView.Host.prepare | function | 560 |

## packages/darklang/cli/liveview/runner.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.LiveView.Runner.loop | function | 24 |
| Darklang.Cli.LiveView.Runner.run | function | 66 |
| Darklang.Cli.LiveView.Runner.timerMs | function | 20 |

## packages/darklang/cli/loop.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Update.processInput | function | 329 |
| Darklang.Cli.Update.updateAppState | function | 320 |
| Darklang.Cli.applyKeyRead | function | 449 |
| Darklang.Cli.burstTimes | function | 433 |
| Darklang.Cli.computeCompletionHint | function | 341 |
| Darklang.Cli.handleKeyInput | function | 67 |
| Darklang.Cli.maxKeyRepeat | value | 317 |
| Darklang.Cli.pageName | function | 424 |
| Darklang.Cli.releasePromptRegion | function | 33 |
| Darklang.Cli.renderMainPrompt | function | 386 |
| Darklang.Cli.runInteractiveLoop | function | 513 |
| Darklang.Cli.withModeHint | function | 15 |

## packages/darklang/cli/outliner/app.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Outliner.App.flag | function | 32 |

## packages/darklang/cli/outliner/core.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Outliner.indexOf | function | 265 |

## packages/darklang/cli/packages/atRestCheck.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Packages.AtRestCheck.staticType | function | 31 |

## packages/darklang/cli/packages/navInteractive.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Packages.NavInteractive.exitToMainPrompt | function | 700 |
| Darklang.Cli.Packages.NavInteractive.handleKey | function | 900 |
| Darklang.Cli.Packages.NavInteractive.handleNavKeys | function | 807 |
| Darklang.Cli.Packages.NavInteractive.handleNavModeKeys | function | 872 |
| Darklang.Cli.Packages.NavInteractive.handleSearchKeys | function | 826 |
| Darklang.Cli.Packages.NavInteractive.loadFocusRows | function | 217 |
| Darklang.Cli.Packages.NavInteractive.moveDown | function | 677 |
| Darklang.Cli.Packages.NavInteractive.moveUp | function | 654 |
| Darklang.Cli.Packages.NavInteractive.navigateIntoModule | function | 733 |
| Darklang.Cli.Packages.NavInteractive.navigateToParent | function | 753 |
| Darklang.Cli.Packages.NavInteractive.performSearch | function | 767 |
| Darklang.Cli.Packages.NavInteractive.presentAtSize | function | 576 |
| Darklang.Cli.Packages.NavInteractive.presentState | function | 591 |
| Darklang.Cli.Packages.NavInteractive.refreshFocusRows | function | 287 |
| Darklang.Cli.Packages.NavInteractive.scrollOffsetForSelection | function | 618 |
| Darklang.Cli.Packages.NavInteractive.selectAndExit | function | 711 |
| Darklang.Cli.Packages.NavInteractive.show | function | 597 |
| Darklang.Cli.Packages.NavInteractive.showAtSize | function | 607 |
| Darklang.Cli.Packages.NavInteractive.toggleDisplay | function | 817 |
| Darklang.Cli.Packages.NavInteractive.withFocusScrollOffset | function | 639 |

## packages/darklang/cli/packages/signatures.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Packages.Signatures.complete | function | 333 |

## packages/darklang/cli/prompt.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Prompt.Display.formatPromptWithInput | function | 210 |
| Darklang.Cli.Prompt.Display.plainPrefix | function | 197 |
| Darklang.Cli.Prompt.Display.styledPrefix | function | 203 |
| Darklang.Cli.Prompt.Display.viewAtSize | function | 221 |
| Darklang.Cli.Prompt.Editing.deleteAtCursor | function | 82 |
| Darklang.Cli.Prompt.Editing.deleteBeforeCursor | function | 69 |
| Darklang.Cli.Prompt.Editing.insertAtCursor | function | 58 |
| Darklang.Cli.Prompt.Editing.moveCursorEnd | function | 109 |
| Darklang.Cli.Prompt.Editing.moveCursorHome | function | 105 |
| Darklang.Cli.Prompt.Editing.moveCursorLeft | function | 94 |
| Darklang.Cli.Prompt.Editing.moveCursorRight | function | 99 |
| Darklang.Cli.Prompt.Editing.setText | function | 121 |
| Darklang.Cli.Prompt.History.addCommand | function | 128 |
| Darklang.Cli.Prompt.History.navigateNext | function | 169 |
| Darklang.Cli.Prompt.History.navigatePrevious | function | 139 |
| Darklang.Cli.Prompt.confirm | function | 16 |

## packages/darklang/cli/ps.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Ps.Watch.Model | type | 433 |
| Darklang.Cli.Ps.Watch.Msg | type | 439 |
| Darklang.Cli.Ps.Watch.Row | type | 427 |
| Darklang.Cli.Ps.Watch.act | function | 467 |
| Darklang.Cli.Ps.Watch.clamp | function | 455 |
| Darklang.Cli.Ps.Watch.gather | function | 441 |
| Darklang.Cli.Ps.Watch.init | function | 453 |
| Darklang.Cli.Ps.Watch.noteOf | function | 462 |
| Darklang.Cli.Ps.Watch.render | function | 487 |
| Darklang.Cli.Ps.Watch.update | function | 474 |

## packages/darklang/cli/registry.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Commands.parseAndExecute | function | 587 |
| Darklang.Cli.Registry.completionContextKey | function | 370 |
| Darklang.Cli.Registry.formatCompactGroup | function | 293 |
| Darklang.Cli.Registry.getCompactCommandList | function | 298 |
| Darklang.Cli.Registry.getCompletionHintFromContext | function | 478 |
| Darklang.Cli.Registry.getCompletionsForContext | function | 390 |

## packages/darklang/cli/scm/conflicts.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Conflicts.previewLine | function | 89 |

## packages/darklang/cli/scm/deps.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Deps.HashedItem | type | 17 |

## packages/darklang/cli/scm/propagate.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.PropagateCmd.resolve | function | 11 |

## packages/darklang/cli/telemetry.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Telemetry.elapsedMs | function | 21 |
| Darklang.Cli.Telemetry.log | function | 52 |
| Darklang.Cli.Telemetry.logFilePath | value | 14 |
| Darklang.Cli.Telemetry.logMessage | function | 56 |
| Darklang.Cli.Telemetry.logSessionStart | function | 65 |
| Darklang.Cli.Telemetry.logWithContext | function | 26 |
| Darklang.Cli.Telemetry.now | function | 18 |

## packages/darklang/cli/traceWatch.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.TraceWatch.Model | type | 23 |
| Darklang.Cli.TraceWatch.Msg | type | 52 |
| Darklang.Cli.TraceWatch.annotate | function | 59 |
| Darklang.Cli.TraceWatch.clockFreeTime | function | 130 |
| Darklang.Cli.TraceWatch.init | function | 177 |
| Darklang.Cli.TraceWatch.reload | function | 143 |
| Darklang.Cli.TraceWatch.render | function | 205 |
| Darklang.Cli.TraceWatch.update | function | 186 |

## packages/darklang/cli/tracing.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Tracing.TraceData | type | 11 |
| Darklang.Cli.Tracing.jsonEscape | function | 1120 |

## packages/darklang/cli/utils/completion.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Completion.buildCompletedCommand | function | 63 |

## packages/darklang/cli/utils/listing.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Listing.footer | function | 33 |

## packages/darklang/cli/utils/logo.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Logo.combineLogoAndText | function | 40 |
| Darklang.Cli.Logo.logo | value | 4 |
| Darklang.Cli.Logo.logoBlock | value | 22 |

## packages/darklang/cli/utils/relativeTime.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.RelativeTime.stamp | function | 38 |

## packages/darklang/cli/utils/terminal.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Terminal.displayOptions | function | 22 |
| Darklang.Cli.Terminal.palette | function | 32 |
| Darklang.Cli.Terminal.renderValue | function | 46 |

## packages/darklang/cli/workbench/chrome.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Workbench.viewBlurb | function | 113 |

## packages/darklang/cli/workbench/loaders-misc.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Workbench.branchListText | function | 80 |
| Darklang.Cli.Workbench.loadBranchItems | function | 45 |

## packages/darklang/cli/workbench/loaders.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Workbench.wipItemCount | function | 141 |

## packages/darklang/dark-packages.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.DarkPackages.Stats | type | 4 |

## packages/darklang/demo-data/httpserver-test.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.DemoData.HttpServerTest.echoHandler | value | 19 |
| Darklang.DemoData.HttpServerTest.echoHandlerFn | function | 15 |
| Darklang.DemoData.HttpServerTest.handlers | value | 38 |
| Darklang.DemoData.HttpServerTest.helloHandler | value | 8 |
| Darklang.DemoData.HttpServerTest.helloHandlerFn | function | 5 |
| Darklang.DemoData.HttpServerTest.rootHandler | value | 29 |
| Darklang.DemoData.HttpServerTest.rootHandlerFn | function | 26 |
| Darklang.DemoData.HttpServerTest.router | function | 44 |

## packages/darklang/languageTools/atRestTypeChecker/environment.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.AtRestTypeChecker.Environment.loadClosure | function | 748 |
| Darklang.LanguageTools.AtRestTypeChecker.Environment.pipeRefs | function | 706 |

## packages/darklang/languageTools/atRestTypeChecker/solve.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.AtRestTypeChecker.Solve.isNumeric | function | 2038 |
| Darklang.LanguageTools.AtRestTypeChecker.Solve.isSignedNumeric | function | 2055 |

## packages/darklang/languageTools/interpreterStats.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.InterpreterStats.enableDetailedTiming | function | 23 |

## packages/darklang/languageTools/packageManager.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.PackageManager.Function.getWithLocation | function | 263 |
| Darklang.LanguageTools.PackageManager.Value.getWithLocation | function | 221 |
| Darklang.LanguageTools.PackageManager.ownerHasItems | function | 486 |

## packages/darklang/languageTools/parser/cliScript.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.Parser.CliScript.PTCliScriptModule | type | 31 |
| Darklang.LanguageTools.Parser.CliScript.WTCliScriptModule | type | 18 |
| Darklang.LanguageTools.Parser.CliScript.parse | function | 400 |
| Darklang.LanguageTools.Parser.CliScript.parseDecls | function | 42 |
| Darklang.LanguageTools.Parser.CliScript.parseForCli | function | 414 |
| Darklang.LanguageTools.Parser.CliScript.parseFromWT | function | 343 |
| Darklang.LanguageTools.Parser.CliScript.pmWithExtras | function | 434 |
| Darklang.LanguageTools.Parser.CliScript.processDecls | function | 119 |
| Darklang.LanguageTools.Parser.CliScript.restamp | function | 327 |
| Darklang.LanguageTools.Parser.CliScript.stabilizedHash | function | 277 |
| Darklang.LanguageTools.Parser.CliScript.toPT | function | 132 |

## packages/darklang/languageTools/parser/parse.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.Parser.Parse.initialParse | function | 15 |
| Darklang.LanguageTools.Parser.Parse.parsePTExpr | function | 28 |
| Darklang.LanguageTools.Parser.Parse.parsePTExprInContext | function | 43 |
| Darklang.LanguageTools.Parser.Parse.parsePTSourceFileWithOps | function | 92 |

## packages/darklang/languageTools/programTypes.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.ProgramTypes.DB | type | 1087 |
| Darklang.LanguageTools.ProgramTypes.Deprecation | type | 382 |
| Darklang.LanguageTools.ProgramTypes.PackageManager.withExtras | function | 878 |
| Darklang.LanguageTools.ProgramTypes.Reference.fromHashAndKind | function | 657 |
| Darklang.LanguageTools.ProgramTypes.SourceFile.Declaration | type | 1095 |
| Darklang.LanguageTools.ProgramTypes.SourceFile.SourceFile | type | 1106 |

## packages/darklang/languageTools/runtimeErrors.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.RuntimeTypes.RuntimeError.TypeChecking.ReverseTypeCheckPath | type | 11 |
| Darklang.LanguageTools.RuntimeTypes.RuntimeError.TypeChecking.TypeCheckPathPart | type | 4 |

## packages/darklang/languageTools/runtimeTypes.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.RuntimeTypes.BuiltInParam | type | 331 |

## packages/darklang/languageTools/semanticTokens.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.SemanticTokens.VariableIdentifier.tokenize | function | 82 |

## packages/darklang/languageTools/writtenTypes.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.WrittenTypes.Expr.range | function | 411 |

## packages/darklang/languageTools/writtenTypesToProgramTypes.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.Identifiers.Fn.toPT | function | 186 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.Identifiers.Value.toPT | function | 183 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.declarationsToPT | function | 2139 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.fnHashBoundAt | function | 62 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.gtlid | function | 29 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.parsedFileAsSourceFile | function | 2245 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.referenceBoundAt | function | 48 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.typeHashBoundAt | function | 71 |
| Darklang.LanguageTools.WrittenTypesToProgramTypes.valueHashBoundAt | function | 80 |

## packages/darklang/prettyPrinter/cliScript.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.ProgramTypes.sourceFile | function | 54 |
| Darklang.PrettyPrinter.ProgramTypes.sourceFileAtWidth | function | 3 |

## packages/darklang/prettyPrinter/common.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.DisplayOptions.inModule | function | 281 |
| Darklang.PrettyPrinter.DisplayOptions.painted | function | 285 |
| Darklang.PrettyPrinter.Palette.plain | value | 220 |
| Darklang.PrettyPrinter.indentByLevel | function | 17 |

## packages/darklang/prettyPrinter/instructions.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.RuntimeTypes.Instructions.listing | function | 240 |

## packages/darklang/prettyPrinter/moduleDeclaration.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.ModuleDeclaration.Module | type | 4 |
| Darklang.PrettyPrinter.ModuleDeclaration.emptyModule | value | 15 |
| Darklang.PrettyPrinter.ModuleDeclaration.implMethodFnHashes | function | 144 |
| Darklang.PrettyPrinter.ModuleDeclaration.intoRootless | function | 29 |
| Darklang.PrettyPrinter.ModuleDeclaration.placeAt | function | 43 |
| Darklang.PrettyPrinter.ModuleDeclaration.toModules | function | 206 |
| Darklang.PrettyPrinter.ModuleDeclaration.withExpr | function | 160 |
| Darklang.PrettyPrinter.ModuleDeclaration.withFn | function | 95 |
| Darklang.PrettyPrinter.ModuleDeclaration.withImpl | function | 123 |
| Darklang.PrettyPrinter.ModuleDeclaration.withItem | function | 77 |
| Darklang.PrettyPrinter.ModuleDeclaration.withTrait | function | 109 |
| Darklang.PrettyPrinter.ModuleDeclaration.withType | function | 88 |
| Darklang.PrettyPrinter.ModuleDeclaration.withValue | function | 102 |
| Darklang.PrettyPrinter.definitions | function | 287 |
| Darklang.PrettyPrinter.moduleDeclaration | function | 255 |

## packages/darklang/prettyPrinter/programTypes.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.ProgramTypes.BuiltinFn.signature | function | 1827 |
| Darklang.PrettyPrinter.ProgramTypes.Context.forMain | value | 122 |
| Darklang.PrettyPrinter.ProgramTypes.FQFnName.Package.fullForReference | function | 283 |
| Darklang.PrettyPrinter.ProgramTypes.FQFnName.fullForReference | function | 293 |
| Darklang.PrettyPrinter.ProgramTypes.FQTraitName.Package.fullForReference | function | 202 |
| Darklang.PrettyPrinter.ProgramTypes.FQTypeName.Package.fullForReference | function | 173 |
| Darklang.PrettyPrinter.ProgramTypes.FQTypeName.fullForReference | function | 183 |
| Darklang.PrettyPrinter.ProgramTypes.FQValueName.Package.fullForReference | function | 236 |
| Darklang.PrettyPrinter.ProgramTypes.FQValueName.fullForReference | function | 247 |
| Darklang.PrettyPrinter.ProgramTypes.NameResolutionError.error | function | 63 |
| Darklang.PrettyPrinter.ProgramTypes.TypeDeclaration.enumCase | function | 1438 |
| Darklang.PrettyPrinter.ProgramTypes.TypeDeclaration.enumField | function | 1409 |
| Darklang.PrettyPrinter.ProgramTypes.TypeDeclaration.recordField | function | 1389 |
| Darklang.PrettyPrinter.ProgramTypes.customType | function | 1472 |
| Darklang.PrettyPrinter.ProgramTypes.db | function | 1479 |
| Darklang.PrettyPrinter.ProgramTypes.packageName | function | 155 |
| Darklang.PrettyPrinter.ProgramTypes.sourceOf | function | 1800 |

## packages/darklang/prettyPrinter/runtimeError.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.RuntimeTypes.RuntimeError.ErrorMessage | type | 679 |
| Darklang.PrettyPrinter.RuntimeTypes.RuntimeError.toErrorMessage | function | 681 |

## packages/darklang/prettyPrinter/runtimeTypes.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.PrettyPrinter.RuntimeTypes.Dval.declaredFieldOrder | function | 632 |
| Darklang.PrettyPrinter.RuntimeTypes.Dval.inDeclaredOrder | function | 651 |
| Darklang.PrettyPrinter.RuntimeTypes.Dval.makeSpaces | function | 671 |
| Darklang.PrettyPrinter.RuntimeTypes.caseNameStandsAlone | function | 105 |
| Darklang.PrettyPrinter.RuntimeTypes.nameResolution | function | 22 |
| Darklang.PrettyPrinter.RuntimeTypes.typeNameWith | function | 80 |

## packages/darklang/scm/branches.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.SCM.Branches.ensure | function | 112 |
| Darklang.SCM.Branches.nameConflicts | function | 374 |
| Darklang.SCM.Branches.rebase | function | 394 |

## packages/darklang/scm/commits.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.SCM.Commits.children | function | 117 |
| Darklang.SCM.Commits.opIdsIn | function | 128 |

## packages/darklang/scm/packageOpsBindings.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.SCM.PackageOps.opBindsAName | function | 27 |

## packages/darklang/scm/packageOpsCommits.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.SCM.PackageOps.count | function | 70 |
| Darklang.SCM.PackageOps.getCommitNamedOps | function | 52 |
| Darklang.SCM.PackageOps.getRecent | function | 9 |

## packages/darklang/stdlib/alt-json.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.AltJson.Builder.addArray | function | 232 |
| Darklang.Stdlib.AltJson.Builder.addBool | function | 224 |
| Darklang.Stdlib.AltJson.Builder.addFloat | function | 216 |
| Darklang.Stdlib.AltJson.Builder.addJson | function | 228 |
| Darklang.Stdlib.AltJson.Builder.addOptionalArray | function | 286 |
| Darklang.Stdlib.AltJson.Builder.addOptionalBool | function | 266 |
| Darklang.Stdlib.AltJson.Builder.addOptionalFloat | function | 246 |
| Darklang.Stdlib.AltJson.Builder.addOptionalJson | function | 276 |
| Darklang.Stdlib.AltJson.Builder.addOptionalString | function | 236 |
| Darklang.Stdlib.AltJson.Builder.addString | function | 212 |
| Darklang.Stdlib.AltJson.Helpers.getArray | function | 173 |
| Darklang.Stdlib.AltJson.Helpers.getBool | function | 74 |
| Darklang.Stdlib.AltJson.Helpers.getObject | function | 185 |
| Darklang.Stdlib.AltJson.Helpers.getString | function | 59 |
| Darklang.Stdlib.AltJson.ParseError.toString | function | 19 |

## packages/darklang/stdlib/await.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.await | function | 7 |

## packages/darklang/stdlib/bool.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Bool.ToString.toString | function | 19 |
| Darklang.Stdlib.Bool.impl ToString for Bool | impl | 17 |

## packages/darklang/stdlib/char.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Char.Compare.greaterThan | function | 84 |
| Darklang.Stdlib.Char.Compare.greaterThanOrEqualTo | function | 85 |
| Darklang.Stdlib.Char.Compare.lessThan | function | 82 |
| Darklang.Stdlib.Char.Compare.lessThanOrEqualTo | function | 83 |
| Darklang.Stdlib.Char.ToString.toString | function | 90 |
| Darklang.Stdlib.Char.impl Compare for Char | impl | 81 |
| Darklang.Stdlib.Char.impl ToString for Char | impl | 88 |

## packages/darklang/stdlib/cli/bash.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Bash.addAlias | function | 36 |

## packages/darklang/stdlib/cli/dir.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Dir.create | function | 5 |
| Darklang.Stdlib.Cli.Dir.current | function | 15 |
| Darklang.Stdlib.Cli.Dir.delete | function | 39 |
| Darklang.Stdlib.Cli.Dir.home | function | 44 |
| Darklang.Stdlib.Cli.Dir.listFullPaths | function | 30 |
| Darklang.Stdlib.Cli.Dir.listRecursive | function | 50 |
| Darklang.Stdlib.Cli.Dir.setCurrent | function | 20 |
| Darklang.Stdlib.Cli.Dir.size | function | 151 |
| Darklang.Stdlib.Cli.Dir.walk | function | 119 |
| Darklang.Stdlib.Cli.Dir.withTemp | function | 166 |

## packages/darklang/stdlib/cli/env.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Env.getOr | function | 10 |
| Darklang.Stdlib.Cli.Env.home | function | 19 |

## packages/darklang/stdlib/cli/file.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.File.__copyBuffered | function | 483 |
| Darklang.Stdlib.Cli.File.__copyCloneDestination | function | 550 |
| Darklang.Stdlib.Cli.File.__copyCopy | function | 563 |
| Darklang.Stdlib.Cli.File.__copyDescriptors | function | 524 |
| Darklang.Stdlib.Cli.File.__copyFinish | function | 518 |
| Darklang.Stdlib.Cli.File.__copyKernelCopy | function | 496 |
| Darklang.Stdlib.Cli.File.__copyOpenDestination | function | 538 |
| Darklang.Stdlib.Cli.File.__copyTransfer | function | 510 |
| Darklang.Stdlib.Cli.File.__copyUnsupported | function | 461 |
| Darklang.Stdlib.Cli.File.__copyWriteAt | function | 467 |
| Darklang.Stdlib.Cli.File.__watchCloseBoth | function | 676 |
| Darklang.Stdlib.Cli.File.__watchDarwin | function | 683 |
| Darklang.Stdlib.Cli.File.__watchDrain | function | 603 |
| Darklang.Stdlib.Cli.File.__watchEvents | function | 589 |
| Darklang.Stdlib.Cli.File.__watchHas | function | 585 |
| Darklang.Stdlib.Cli.File.__watchLinux | function | 629 |
| Darklang.Stdlib.Cli.File.__watchLoop | function | 610 |
| Darklang.Stdlib.Cli.File.__watchVnodeEvents | function | 647 |
| Darklang.Stdlib.Cli.File.__watchVnodeLoop | function | 662 |
| Darklang.Stdlib.Cli.File.appendText | function | 219 |
| Darklang.Stdlib.Cli.File.areIdentical | function | 285 |
| Darklang.Stdlib.Cli.File.copy | function | 32 |
| Darklang.Stdlib.Cli.File.countNewlines | function | 79 |
| Darklang.Stdlib.Cli.File.createSymlink | function | 272 |
| Darklang.Stdlib.Cli.File.dropThroughFirstNewline | function | 86 |
| Darklang.Stdlib.Cli.File.getModifiedTime | function | 25 |
| Darklang.Stdlib.Cli.File.getOwner | function | 302 |
| Darklang.Stdlib.Cli.File.glob | function | 390 |
| Darklang.Stdlib.Cli.File.move | function | 37 |
| Darklang.Stdlib.Cli.File.readFully | function | 100 |
| Darklang.Stdlib.Cli.File.readLastLines | function | 146 |
| Darklang.Stdlib.Cli.File.readLines | function | 74 |
| Darklang.Stdlib.Cli.File.readSymlink | function | 280 |
| Darklang.Stdlib.Cli.File.readTailChunks | function | 118 |
| Darklang.Stdlib.Cli.File.setPermissions | function | 254 |
| Darklang.Stdlib.Cli.File.touch | function | 239 |
| Darklang.Stdlib.Cli.File.transform | function | 293 |
| Darklang.Stdlib.Cli.File.watch | function | 430 |
| Darklang.Stdlib.Cli.File.watchLoop | function | 406 |
| Darklang.Stdlib.Cli.File.withLock | function | 340 |

## packages/darklang/stdlib/cli/fileSystem.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.FileSystem.appendToFile | function | 75 |
| Darklang.Stdlib.Cli.FileSystem.deleteFile | function | 69 |
| Darklang.Stdlib.Cli.FileSystem.isDirectory | function | 64 |
| Darklang.Stdlib.Cli.FileSystem.readFileAsString | function | 137 |

## packages/darklang/stdlib/cli/fish.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Fish.addAlias | function | 31 |

## packages/darklang/stdlib/cli/log.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Log.debug | function | 19 |
| Darklang.Stdlib.Cli.Log.error | function | 15 |
| Darklang.Stdlib.Cli.Log.info | function | 7 |
| Darklang.Stdlib.Cli.Log.success | function | 23 |

## packages/darklang/stdlib/cli/posix.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Posix.Errno.eperm | value | 211 |
| Darklang.Stdlib.Cli.Posix.Errno.esrch | value | 208 |
| Darklang.Stdlib.Cli.Posix.OpenFlags.append | function | 33 |
| Darklang.Stdlib.Cli.Posix.OpenFlags.rdwr | function | 30 |
| Darklang.Stdlib.Cli.Posix.__abiOpenAppend | function | 301 |
| Darklang.Stdlib.Cli.Posix.__accountFieldFromPasswd | function | 774 |
| Darklang.Stdlib.Cli.Posix.__accountLookup | function | 786 |
| Darklang.Stdlib.Cli.Posix.chdir | function | 94 |
| Darklang.Stdlib.Cli.Posix.chmod | function | 119 |
| Darklang.Stdlib.Cli.Posix.fdSeekFromStart | function | 75 |
| Darklang.Stdlib.Cli.Posix.fileOwner | function | 169 |
| Darklang.Stdlib.Cli.Posix.flock | function | 164 |
| Darklang.Stdlib.Cli.Posix.isProcessRunning | function | 245 |
| Darklang.Stdlib.Cli.Posix.sigterm | value | 198 |
| Darklang.Stdlib.Cli.Posix.symlink | function | 133 |
| Darklang.Stdlib.Cli.Posix.utimesNow | function | 124 |

## packages/darklang/stdlib/cli/powershell.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.PowerShell.addAlias | function | 53 |
| Darklang.Stdlib.Cli.PowerShell.grantExecutePermission | function | 45 |

## packages/darklang/stdlib/cli/process.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Process.exec | function | 89 |
| Darklang.Stdlib.Cli.Process.forceKill | function | 111 |
| Darklang.Stdlib.Cli.Process.isRunning | function | 227 |
| Darklang.Stdlib.Cli.Process.kill | function | 106 |
| Darklang.Stdlib.Cli.Process.pipe | function | 200 |
| Darklang.Stdlib.Cli.Process.runIn | function | 119 |
| Darklang.Stdlib.Cli.Process.runSilent | function | 134 |
| Darklang.Stdlib.Cli.Process.shellPipeline | function | 221 |
| Darklang.Stdlib.Cli.Process.waitFor | function | 232 |

## packages/darklang/stdlib/cli/stdin.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Stdin.Key.isControl | function | 505 |
| Darklang.Stdlib.Cli.Stdin.Key.isTextual | function | 495 |
| Darklang.Stdlib.Cli.Stdin.Key.toString | function | 515 |
| Darklang.Stdlib.Cli.Stdin.KeyRead.toString | function | 564 |
| Darklang.Stdlib.Cli.Stdin.Modifiers.toString | function | 527 |
| Darklang.Stdlib.Cli.Stdin.readKey | function | 567 |
| Darklang.Stdlib.Cli.Stdin.readSecret | function | 26 |

## packages/darklang/stdlib/cli/sys.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Sys.__affinityWith | function | 78 |
| Darklang.Stdlib.Cli.Sys.__countBits | function | 74 |
| Darklang.Stdlib.Cli.Sys.__cpuCount | function | 88 |
| Darklang.Stdlib.Cli.Sys.arch | function | 28 |
| Darklang.Stdlib.Cli.Sys.cpuCount | function | 39 |
| Darklang.Stdlib.Cli.Sys.currentUser | function | 6 |
| Darklang.Stdlib.Cli.Sys.isRoot | function | 49 |
| Darklang.Stdlib.Cli.Sys.os | function | 22 |

## packages/darklang/stdlib/cli/tui/layout.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Tui.Layout.viewport | function | 24 |

## packages/darklang/stdlib/cli/tui/terminalSession.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Tui.TerminalSession.present | function | 290 |
| Darklang.Stdlib.Cli.Tui.TerminalSession.start | function | 313 |

## packages/darklang/stdlib/cli/ui/colors.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Colors.accent | value | 84 |
| Darklang.Stdlib.Cli.UI.Colors.argument | function | 99 |
| Darklang.Stdlib.Cli.UI.Colors.argumentColor | value | 68 |
| Darklang.Stdlib.Cli.UI.Colors.bgBlack | value | 32 |
| Darklang.Stdlib.Cli.UI.Colors.bgCyan | value | 38 |
| Darklang.Stdlib.Cli.UI.Colors.bgMagenta | value | 37 |
| Darklang.Stdlib.Cli.UI.Colors.bgWhite | value | 39 |
| Darklang.Stdlib.Cli.UI.Colors.brightCyan | value | 28 |
| Darklang.Stdlib.Cli.UI.Colors.brightGreen | value | 24 |
| Darklang.Stdlib.Cli.UI.Colors.brightRed | value | 23 |
| Darklang.Stdlib.Cli.UI.Colors.brightYellow | value | 25 |
| Darklang.Stdlib.Cli.UI.Colors.comment | function | 103 |
| Darklang.Stdlib.Cli.UI.Colors.logo | function | 110 |
| Darklang.Stdlib.Cli.UI.Colors.logoColor | value | 79 |
| Darklang.Stdlib.Cli.UI.Colors.mutedText | function | 97 |
| Darklang.Stdlib.Cli.UI.Colors.number | function | 102 |
| Darklang.Stdlib.Cli.UI.Colors.primary | value | 82 |
| Darklang.Stdlib.Cli.UI.Colors.prompt | function | 109 |
| Darklang.Stdlib.Cli.UI.Colors.promptColor | value | 78 |
| Darklang.Stdlib.Cli.UI.Colors.string | function | 101 |

## packages/darklang/stdlib/cli/ui/editor.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Editor.emptyBuf | function | 10 |

## packages/darklang/stdlib/cli/ui/html.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Html.className | function | 9 |
| Darklang.Stdlib.Cli.UI.Html.escape | function | 28 |
| Darklang.Stdlib.Cli.UI.Html.page | function | 94 |
| Darklang.Stdlib.Cli.UI.Html.render | function | 32 |
| Darklang.Stdlib.Cli.UI.Html.renderStatic | function | 91 |
| Darklang.Stdlib.Cli.UI.Html.severityClass | function | 22 |

## packages/darklang/stdlib/cli/ui/layout.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Layout.distributeCols | function | 161 |
| Darklang.Stdlib.Cli.UI.Layout.fixedWidth | function | 211 |
| Darklang.Stdlib.Cli.UI.Layout.flex | function | 143 |
| Darklang.Stdlib.Cli.UI.Layout.flexWidth | function | 218 |
| Darklang.Stdlib.Cli.UI.Layout.greedyWidth | function | 225 |
| Darklang.Stdlib.Cli.UI.Layout.hstack | function | 188 |

## packages/darklang/stdlib/cli/ui/node.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Node.bold | function | 83 |
| Darklang.Stdlib.Cli.UI.Node.row | function | 79 |
| Darklang.Stdlib.Cli.UI.Node.styled | function | 81 |
| Darklang.Stdlib.Cli.UI.Node.toLines | function | 342 |

## packages/darklang/stdlib/cli/ui/progress.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Progress.bar | function | 8 |

## packages/darklang/stdlib/cli/ui/prompt.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Prompt.ask | function | 5 |

## packages/darklang/stdlib/cli/ui/splitpane.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.SplitPane.Orientation | type | 7 |
| Darklang.Stdlib.Cli.UI.SplitPane.render | function | 20 |

## packages/darklang/stdlib/cli/ui/text.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.Text.lines | function | 11 |
| Darklang.Stdlib.Cli.UI.Text.render | function | 65 |

## packages/darklang/stdlib/cli/ui/textfield.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.UI.TextField.clampCursor | function | 44 |
| Darklang.Stdlib.Cli.UI.TextField.renderMaybeFocused | function | 177 |
| Darklang.Stdlib.Cli.UI.TextField.view | function | 209 |

## packages/darklang/stdlib/cli/unix.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Unix.addAlias | function | 66 |
| Darklang.Stdlib.Cli.Unix.areFilesIdentical | function | 31 |
| Darklang.Stdlib.Cli.Unix.executeInDirectoryAndRedirectToFile | function | 19 |
| Darklang.Stdlib.Cli.Unix.findCommandPath | function | 47 |
| Darklang.Stdlib.Cli.Unix.getAbsolutePath | function | 61 |
| Darklang.Stdlib.Cli.Unix.removeFile | function | 4 |
| Darklang.Stdlib.Cli.Unix.showDiff | function | 37 |

## packages/darklang/stdlib/cli/zsh.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Cli.Zsh.addAlias | function | 37 |

## packages/darklang/stdlib/crypto.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Crypto.constantTimeEquals | function | 28 |

## packages/darklang/stdlib/dateTime.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.DateTime.Compare.greaterThan | function | 66 |
| Darklang.Stdlib.DateTime.Compare.greaterThanOrEqualTo | function | 67 |
| Darklang.Stdlib.DateTime.Compare.lessThan | function | 64 |
| Darklang.Stdlib.DateTime.Compare.lessThanOrEqualTo | function | 65 |
| Darklang.Stdlib.DateTime.ToString.toString | function | 12 |
| Darklang.Stdlib.DateTime.impl Compare for DateTime | impl | 63 |
| Darklang.Stdlib.DateTime.impl ToString for DateTime | impl | 10 |

## packages/darklang/stdlib/dict.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Dict.Zero.zero | function | 9 |
| Darklang.Stdlib.Dict.impl Stdlib.Zero for Dict<'k, 'v> | impl | 8 |
| Darklang.Stdlib.Dict.iter | function | 104 |

## packages/darklang/stdlib/env.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Env.getAll | function | 9 |

## packages/darklang/stdlib/exec.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Exec.awaitWithin | function | 134 |
| Darklang.Stdlib.Exec.spawnDetached | function | 122 |

## packages/darklang/stdlib/execPolicy.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Exec.Policy.leastRunFirst | function | 30 |
| Darklang.Stdlib.Exec.Policy.roundRobin | function | 19 |
| Darklang.Stdlib.Exec.Policy.youngestFirst | function | 25 |

## packages/darklang/stdlib/float.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Float.Add.add | function | 50 |
| Darklang.Stdlib.Float.Compare.greaterThan | function | 81 |
| Darklang.Stdlib.Float.Compare.greaterThanOrEqualTo | function | 83 |
| Darklang.Stdlib.Float.Compare.lessThan | function | 76 |
| Darklang.Stdlib.Float.Compare.lessThanOrEqualTo | function | 78 |
| Darklang.Stdlib.Float.Divide.divide | function | 71 |
| Darklang.Stdlib.Float.Multiply.multiply | function | 60 |
| Darklang.Stdlib.Float.Negate.negate | function | 41 |
| Darklang.Stdlib.Float.One.one | function | 152 |
| Darklang.Stdlib.Float.Power.power | function | 65 |
| Darklang.Stdlib.Float.Subtract.subtract | function | 55 |
| Darklang.Stdlib.Float.ToString.toString | function | 146 |
| Darklang.Stdlib.Float.Zero.zero | function | 149 |
| Darklang.Stdlib.Float.impl Add for Float | impl | 48 |
| Darklang.Stdlib.Float.impl Compare for Float | impl | 74 |
| Darklang.Stdlib.Float.impl Divide for Float | impl | 69 |
| Darklang.Stdlib.Float.impl Multiply for Float | impl | 58 |
| Darklang.Stdlib.Float.impl Negate for Float | impl | 39 |
| Darklang.Stdlib.Float.impl One for Float | impl | 151 |
| Darklang.Stdlib.Float.impl Power for Float | impl | 63 |
| Darklang.Stdlib.Float.impl Subtract for Float | impl | 53 |
| Darklang.Stdlib.Float.impl ToString for Float | impl | 143 |
| Darklang.Stdlib.Float.impl Zero for Float | impl | 148 |

## packages/darklang/stdlib/fun.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Fun.identity | function | 4 |

## packages/darklang/stdlib/hostAwait.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Host.RawEvent | type | 34 |

## packages/darklang/stdlib/html.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Html.Attribute | type | 47 |
| Darklang.Stdlib.Html.a | function | 238 |
| Darklang.Stdlib.Html.button | function | 308 |

## packages/darklang/stdlib/http.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Http.Cookie | type | 278 |
| Darklang.Stdlib.Http.serverError | function | 272 |

## packages/darklang/stdlib/httpclient.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.HttpClient.StreamResponse | type | 58 |
| Darklang.Stdlib.HttpClient.delete | function | 122 |
| Darklang.Stdlib.HttpClient.head | function | 126 |
| Darklang.Stdlib.HttpClient.options | function | 118 |
| Darklang.Stdlib.HttpClient.post | function | 102 |
| Darklang.Stdlib.HttpClient.put | function | 110 |
| Darklang.Stdlib.HttpClient.stream | function | 137 |
| Darklang.Stdlib.HttpClient.toString | function | 30 |

## packages/darklang/stdlib/httpserver.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.HttpServer.RouteMatch | type | 22 |
| Darklang.Stdlib.HttpServer.RouteSegment | type | 28 |
| Darklang.Stdlib.HttpServer.findHandler | function | 115 |
| Darklang.Stdlib.HttpServer.getPath | function | 148 |
| Darklang.Stdlib.HttpServer.getPathParam | function | 186 |
| Darklang.Stdlib.HttpServer.matchRoute | function | 110 |
| Darklang.Stdlib.HttpServer.matchRouteSegments | function | 79 |
| Darklang.Stdlib.HttpServer.matchSegment | function | 51 |
| Darklang.Stdlib.HttpServer.parseRouteSegments | function | 35 |
| Darklang.Stdlib.HttpServer.routeRequest | function | 172 |
| Darklang.Stdlib.HttpServer.splitPath | function | 70 |

## packages/darklang/stdlib/int.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Int.Add.add | function | 34 |
| Darklang.Stdlib.Int.BitwiseAnd.bitwiseAnd | function | 163 |
| Darklang.Stdlib.Int.BitwiseNot.bitwiseNot | function | 172 |
| Darklang.Stdlib.Int.BitwiseOr.bitwiseOr | function | 166 |
| Darklang.Stdlib.Int.BitwiseXor.bitwiseXor | function | 169 |
| Darklang.Stdlib.Int.Compare.greaterThan | function | 70 |
| Darklang.Stdlib.Int.Compare.greaterThanOrEqualTo | function | 72 |
| Darklang.Stdlib.Int.Compare.lessThan | function | 66 |
| Darklang.Stdlib.Int.Compare.lessThanOrEqualTo | function | 68 |
| Darklang.Stdlib.Int.Divide.divide | function | 49 |
| Darklang.Stdlib.Int.Modulo.modulo | function | 19 |
| Darklang.Stdlib.Int.Multiply.multiply | function | 44 |
| Darklang.Stdlib.Int.Negate.negate | function | 61 |
| Darklang.Stdlib.Int.One.one | function | 193 |
| Darklang.Stdlib.Int.Power.power | function | 55 |
| Darklang.Stdlib.Int.ShiftLeft.shiftLeft | function | 175 |
| Darklang.Stdlib.Int.ShiftRight.shiftRight | function | 178 |
| Darklang.Stdlib.Int.Subtract.subtract | function | 39 |
| Darklang.Stdlib.Int.ToString.toString | function | 187 |
| Darklang.Stdlib.Int.Zero.zero | function | 190 |
| Darklang.Stdlib.Int.fromInt128 | function | 121 |
| Darklang.Stdlib.Int.fromUInt16 | function | 106 |
| Darklang.Stdlib.Int.impl Add for Int | impl | 32 |
| Darklang.Stdlib.Int.impl BitwiseAnd for Int | impl | 162 |
| Darklang.Stdlib.Int.impl BitwiseNot for Int | impl | 171 |
| Darklang.Stdlib.Int.impl BitwiseOr for Int | impl | 165 |
| Darklang.Stdlib.Int.impl BitwiseXor for Int | impl | 168 |
| Darklang.Stdlib.Int.impl Compare for Int | impl | 64 |
| Darklang.Stdlib.Int.impl Divide for Int | impl | 47 |
| Darklang.Stdlib.Int.impl Modulo for Int | impl | 14 |
| Darklang.Stdlib.Int.impl Multiply for Int | impl | 42 |
| Darklang.Stdlib.Int.impl Negate for Int | impl | 59 |
| Darklang.Stdlib.Int.impl One for Int | impl | 192 |
| Darklang.Stdlib.Int.impl Power for Int | impl | 52 |
| Darklang.Stdlib.Int.impl ShiftLeft for Int | impl | 174 |
| Darklang.Stdlib.Int.impl ShiftRight for Int | impl | 177 |
| Darklang.Stdlib.Int.impl Subtract for Int | impl | 37 |
| Darklang.Stdlib.Int.impl ToString for Int | impl | 185 |
| Darklang.Stdlib.Int.impl Zero for Int | impl | 189 |
| Darklang.Stdlib.Int.sqrt | function | 77 |
| Darklang.Stdlib.Int.toInt16 | function | 137 |
| Darklang.Stdlib.Int.toUInt16 | function | 140 |
| Darklang.Stdlib.Int.toUInt32 | function | 146 |

## packages/darklang/stdlib/int128.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Int128.One.one | function | 163 |
| Darklang.Stdlib.Int128.Power.power | function | 54 |
| Darklang.Stdlib.Int128.Zero.zero | function | 160 |
| Darklang.Stdlib.Int128.impl Add for Int128 | impl | 30 |
| Darklang.Stdlib.Int128.impl BitwiseAnd for Int128 | impl | 126 |
| Darklang.Stdlib.Int128.impl BitwiseNot for Int128 | impl | 141 |
| Darklang.Stdlib.Int128.impl BitwiseOr for Int128 | impl | 131 |
| Darklang.Stdlib.Int128.impl BitwiseXor for Int128 | impl | 136 |
| Darklang.Stdlib.Int128.impl Compare for Int128 | impl | 63 |
| Darklang.Stdlib.Int128.impl Divide for Int128 | impl | 45 |
| Darklang.Stdlib.Int128.impl Modulo for Int128 | impl | 9 |
| Darklang.Stdlib.Int128.impl Multiply for Int128 | impl | 40 |
| Darklang.Stdlib.Int128.impl Negate for Int128 | impl | 58 |
| Darklang.Stdlib.Int128.impl One for Int128 | impl | 162 |
| Darklang.Stdlib.Int128.impl Power for Int128 | impl | 50 |
| Darklang.Stdlib.Int128.impl ShiftLeft for Int128 | impl | 146 |
| Darklang.Stdlib.Int128.impl ShiftRight for Int128 | impl | 151 |
| Darklang.Stdlib.Int128.impl Subtract for Int128 | impl | 35 |
| Darklang.Stdlib.Int128.impl ToString for Int128 | impl | 155 |
| Darklang.Stdlib.Int128.impl Zero for Int128 | impl | 159 |

## packages/darklang/stdlib/int16.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Int16.Modulo.modulo | function | 14 |
| Darklang.Stdlib.Int16.One.one | function | 177 |
| Darklang.Stdlib.Int16.Zero.zero | function | 174 |
| Darklang.Stdlib.Int16.impl Add for Int16 | impl | 30 |
| Darklang.Stdlib.Int16.impl BitwiseAnd for Int16 | impl | 140 |
| Darklang.Stdlib.Int16.impl BitwiseNot for Int16 | impl | 155 |
| Darklang.Stdlib.Int16.impl BitwiseOr for Int16 | impl | 145 |
| Darklang.Stdlib.Int16.impl BitwiseXor for Int16 | impl | 150 |
| Darklang.Stdlib.Int16.impl Compare for Int16 | impl | 63 |
| Darklang.Stdlib.Int16.impl Divide for Int16 | impl | 53 |
| Darklang.Stdlib.Int16.impl Modulo for Int16 | impl | 9 |
| Darklang.Stdlib.Int16.impl Multiply for Int16 | impl | 40 |
| Darklang.Stdlib.Int16.impl Negate for Int16 | impl | 58 |
| Darklang.Stdlib.Int16.impl One for Int16 | impl | 176 |
| Darklang.Stdlib.Int16.impl Power for Int16 | impl | 45 |
| Darklang.Stdlib.Int16.impl ShiftLeft for Int16 | impl | 160 |
| Darklang.Stdlib.Int16.impl ShiftRight for Int16 | impl | 165 |
| Darklang.Stdlib.Int16.impl Subtract for Int16 | impl | 35 |
| Darklang.Stdlib.Int16.impl ToString for Int16 | impl | 169 |
| Darklang.Stdlib.Int16.impl Zero for Int16 | impl | 173 |
| Darklang.Stdlib.Int16.sqrt | function | 82 |

## packages/darklang/stdlib/int32.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Int32.One.one | function | 175 |
| Darklang.Stdlib.Int32.Zero.zero | function | 172 |
| Darklang.Stdlib.Int32.impl Add for Int32 | impl | 30 |
| Darklang.Stdlib.Int32.impl BitwiseAnd for Int32 | impl | 138 |
| Darklang.Stdlib.Int32.impl BitwiseNot for Int32 | impl | 153 |
| Darklang.Stdlib.Int32.impl BitwiseOr for Int32 | impl | 143 |
| Darklang.Stdlib.Int32.impl BitwiseXor for Int32 | impl | 148 |
| Darklang.Stdlib.Int32.impl Compare for Int32 | impl | 63 |
| Darklang.Stdlib.Int32.impl Divide for Int32 | impl | 45 |
| Darklang.Stdlib.Int32.impl Modulo for Int32 | impl | 9 |
| Darklang.Stdlib.Int32.impl Multiply for Int32 | impl | 40 |
| Darklang.Stdlib.Int32.impl Negate for Int32 | impl | 58 |
| Darklang.Stdlib.Int32.impl One for Int32 | impl | 174 |
| Darklang.Stdlib.Int32.impl Power for Int32 | impl | 50 |
| Darklang.Stdlib.Int32.impl ShiftLeft for Int32 | impl | 158 |
| Darklang.Stdlib.Int32.impl ShiftRight for Int32 | impl | 163 |
| Darklang.Stdlib.Int32.impl Subtract for Int32 | impl | 35 |
| Darklang.Stdlib.Int32.impl ToString for Int32 | impl | 167 |
| Darklang.Stdlib.Int32.impl Zero for Int32 | impl | 171 |

## packages/darklang/stdlib/int64.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Int64.One.one | function | 193 |
| Darklang.Stdlib.Int64.Zero.zero | function | 190 |
| Darklang.Stdlib.Int64.impl Add for Int64 | impl | 30 |
| Darklang.Stdlib.Int64.impl BitwiseAnd for Int64 | impl | 156 |
| Darklang.Stdlib.Int64.impl BitwiseNot for Int64 | impl | 171 |
| Darklang.Stdlib.Int64.impl BitwiseOr for Int64 | impl | 161 |
| Darklang.Stdlib.Int64.impl BitwiseXor for Int64 | impl | 166 |
| Darklang.Stdlib.Int64.impl Compare for Int64 | impl | 63 |
| Darklang.Stdlib.Int64.impl Divide for Int64 | impl | 45 |
| Darklang.Stdlib.Int64.impl Modulo for Int64 | impl | 9 |
| Darklang.Stdlib.Int64.impl Multiply for Int64 | impl | 40 |
| Darklang.Stdlib.Int64.impl Negate for Int64 | impl | 58 |
| Darklang.Stdlib.Int64.impl One for Int64 | impl | 192 |
| Darklang.Stdlib.Int64.impl Power for Int64 | impl | 50 |
| Darklang.Stdlib.Int64.impl ShiftLeft for Int64 | impl | 176 |
| Darklang.Stdlib.Int64.impl ShiftRight for Int64 | impl | 181 |
| Darklang.Stdlib.Int64.impl Subtract for Int64 | impl | 35 |
| Darklang.Stdlib.Int64.impl ToString for Int64 | impl | 185 |
| Darklang.Stdlib.Int64.impl Zero for Int64 | impl | 189 |

## packages/darklang/stdlib/int8.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Int8.One.one | function | 175 |
| Darklang.Stdlib.Int8.Zero.zero | function | 172 |
| Darklang.Stdlib.Int8.impl Add for Int8 | impl | 30 |
| Darklang.Stdlib.Int8.impl BitwiseAnd for Int8 | impl | 138 |
| Darklang.Stdlib.Int8.impl BitwiseNot for Int8 | impl | 153 |
| Darklang.Stdlib.Int8.impl BitwiseOr for Int8 | impl | 143 |
| Darklang.Stdlib.Int8.impl BitwiseXor for Int8 | impl | 148 |
| Darklang.Stdlib.Int8.impl Compare for Int8 | impl | 63 |
| Darklang.Stdlib.Int8.impl Divide for Int8 | impl | 53 |
| Darklang.Stdlib.Int8.impl Modulo for Int8 | impl | 9 |
| Darklang.Stdlib.Int8.impl Multiply for Int8 | impl | 40 |
| Darklang.Stdlib.Int8.impl Negate for Int8 | impl | 58 |
| Darklang.Stdlib.Int8.impl One for Int8 | impl | 174 |
| Darklang.Stdlib.Int8.impl Power for Int8 | impl | 45 |
| Darklang.Stdlib.Int8.impl ShiftLeft for Int8 | impl | 158 |
| Darklang.Stdlib.Int8.impl ShiftRight for Int8 | impl | 163 |
| Darklang.Stdlib.Int8.impl Subtract for Int8 | impl | 35 |
| Darklang.Stdlib.Int8.impl ToString for Int8 | impl | 167 |
| Darklang.Stdlib.Int8.impl Zero for Int8 | impl | 171 |

## packages/darklang/stdlib/list.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.List.Zero.zero | function | 9 |
| Darklang.Stdlib.List.impl Stdlib.Zero for List<'a> | impl | 8 |

## packages/darklang/stdlib/live.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Live.Observation | type | 583 |
| Darklang.Stdlib.Live.atRestFailure | function | 611 |
| Darklang.Stdlib.Live.diagnose | function | 558 |
| Darklang.Stdlib.Live.observe | function | 630 |
| Darklang.Stdlib.Live.show | function | 74 |
| Darklang.Stdlib.Live.touchingOnly | function | 549 |
| Darklang.Stdlib.Live.versionsNewestFirst | function | 596 |

## packages/darklang/stdlib/retry.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Retry.withBackoff | function | 23 |
| Darklang.Stdlib.Retry.withBackoffLoop | function | 4 |
| Darklang.Stdlib.Retry.withFixedDelay | function | 47 |
| Darklang.Stdlib.Retry.withFixedDelayLoop | function | 30 |

## packages/darklang/stdlib/string.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.String.Add.add | function | 97 |
| Darklang.Stdlib.String.Compare.greaterThan | function | 590 |
| Darklang.Stdlib.String.Compare.greaterThanOrEqualTo | function | 592 |
| Darklang.Stdlib.String.Compare.lessThan | function | 585 |
| Darklang.Stdlib.String.Compare.lessThanOrEqualTo | function | 587 |
| Darklang.Stdlib.String.ToString.toString | function | 598 |
| Darklang.Stdlib.String.Zero.zero | function | 101 |
| Darklang.Stdlib.String.charAt | function | 573 |
| Darklang.Stdlib.String.compare | function | 580 |
| Darklang.Stdlib.String.fromBlob | function | 450 |
| Darklang.Stdlib.String.impl Add for String | impl | 96 |
| Darklang.Stdlib.String.impl Compare for String | impl | 584 |
| Darklang.Stdlib.String.impl ToString for String | impl | 597 |
| Darklang.Stdlib.String.impl Zero for String | impl | 100 |

## packages/darklang/stdlib/traits.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Add | trait | 12 |
| Darklang.Stdlib.BitwiseAnd | trait | 58 |
| Darklang.Stdlib.BitwiseAnd.bitwiseAnd | function | 59 |
| Darklang.Stdlib.BitwiseNot | trait | 73 |
| Darklang.Stdlib.BitwiseNot.bitwiseNot | function | 74 |
| Darklang.Stdlib.BitwiseOr | trait | 63 |
| Darklang.Stdlib.BitwiseOr.bitwiseOr | function | 64 |
| Darklang.Stdlib.BitwiseXor | trait | 68 |
| Darklang.Stdlib.BitwiseXor.bitwiseXor | function | 69 |
| Darklang.Stdlib.Compare | trait | 88 |
| Darklang.Stdlib.Compare.greaterThan | function | 91 |
| Darklang.Stdlib.Compare.greaterThanOrEqualTo | function | 92 |
| Darklang.Stdlib.Compare.lessThan | function | 89 |
| Darklang.Stdlib.Compare.lessThanOrEqualTo | function | 90 |
| Darklang.Stdlib.Divide | trait | 27 |
| Darklang.Stdlib.Divide.divide | function | 28 |
| Darklang.Stdlib.Modulo | trait | 32 |
| Darklang.Stdlib.Modulo.modulo | function | 33 |
| Darklang.Stdlib.Multiply | trait | 22 |
| Darklang.Stdlib.Multiply.multiply | function | 23 |
| Darklang.Stdlib.Negate | trait | 42 |
| Darklang.Stdlib.Negate.negate | function | 43 |
| Darklang.Stdlib.One | trait | 53 |
| Darklang.Stdlib.Power | trait | 37 |
| Darklang.Stdlib.Power.power | function | 38 |
| Darklang.Stdlib.ShiftLeft | trait | 78 |
| Darklang.Stdlib.ShiftLeft.shiftLeft | function | 79 |
| Darklang.Stdlib.ShiftRight | trait | 83 |
| Darklang.Stdlib.ShiftRight.shiftRight | function | 84 |
| Darklang.Stdlib.Subtract | trait | 17 |
| Darklang.Stdlib.Subtract.subtract | function | 18 |
| Darklang.Stdlib.ToString | trait | 100 |
| Darklang.Stdlib.Zero | trait | 48 |

## packages/darklang/stdlib/uint128.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.UInt128.One.one | function | 134 |
| Darklang.Stdlib.UInt128.Power.power | function | 40 |
| Darklang.Stdlib.UInt128.Zero.zero | function | 131 |
| Darklang.Stdlib.UInt128.impl Add for UInt128 | impl | 17 |
| Darklang.Stdlib.UInt128.impl BitwiseAnd for UInt128 | impl | 92 |
| Darklang.Stdlib.UInt128.impl BitwiseNot for UInt128 | impl | 110 |
| Darklang.Stdlib.UInt128.impl BitwiseOr for UInt128 | impl | 98 |
| Darklang.Stdlib.UInt128.impl BitwiseXor for UInt128 | impl | 104 |
| Darklang.Stdlib.UInt128.impl Compare for UInt128 | impl | 44 |
| Darklang.Stdlib.UInt128.impl Divide for UInt128 | impl | 32 |
| Darklang.Stdlib.UInt128.impl Modulo for UInt128 | impl | 9 |
| Darklang.Stdlib.UInt128.impl Multiply for UInt128 | impl | 27 |
| Darklang.Stdlib.UInt128.impl One for UInt128 | impl | 133 |
| Darklang.Stdlib.UInt128.impl Power for UInt128 | impl | 37 |
| Darklang.Stdlib.UInt128.impl ShiftLeft for UInt128 | impl | 115 |
| Darklang.Stdlib.UInt128.impl ShiftRight for UInt128 | impl | 121 |
| Darklang.Stdlib.UInt128.impl Subtract for UInt128 | impl | 22 |
| Darklang.Stdlib.UInt128.impl ToString for UInt128 | impl | 126 |
| Darklang.Stdlib.UInt128.impl Zero for UInt128 | impl | 130 |

## packages/darklang/stdlib/uint16.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.UInt16.One.one | function | 166 |
| Darklang.Stdlib.UInt16.Zero.zero | function | 163 |
| Darklang.Stdlib.UInt16.impl Add for UInt16 | impl | 17 |
| Darklang.Stdlib.UInt16.impl BitwiseAnd for UInt16 | impl | 129 |
| Darklang.Stdlib.UInt16.impl BitwiseNot for UInt16 | impl | 144 |
| Darklang.Stdlib.UInt16.impl BitwiseOr for UInt16 | impl | 134 |
| Darklang.Stdlib.UInt16.impl BitwiseXor for UInt16 | impl | 139 |
| Darklang.Stdlib.UInt16.impl Compare for UInt16 | impl | 49 |
| Darklang.Stdlib.UInt16.impl Divide for UInt16 | impl | 44 |
| Darklang.Stdlib.UInt16.impl Modulo for UInt16 | impl | 9 |
| Darklang.Stdlib.UInt16.impl Multiply for UInt16 | impl | 27 |
| Darklang.Stdlib.UInt16.impl One for UInt16 | impl | 165 |
| Darklang.Stdlib.UInt16.impl Power for UInt16 | impl | 32 |
| Darklang.Stdlib.UInt16.impl ShiftLeft for UInt16 | impl | 149 |
| Darklang.Stdlib.UInt16.impl ShiftRight for UInt16 | impl | 154 |
| Darklang.Stdlib.UInt16.impl Subtract for UInt16 | impl | 22 |
| Darklang.Stdlib.UInt16.impl ToString for UInt16 | impl | 158 |
| Darklang.Stdlib.UInt16.impl Zero for UInt16 | impl | 162 |

## packages/darklang/stdlib/uint32.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.UInt32.One.one | function | 165 |
| Darklang.Stdlib.UInt32.Zero.zero | function | 162 |
| Darklang.Stdlib.UInt32.impl Add for UInt32 | impl | 17 |
| Darklang.Stdlib.UInt32.impl BitwiseAnd for UInt32 | impl | 128 |
| Darklang.Stdlib.UInt32.impl BitwiseNot for UInt32 | impl | 143 |
| Darklang.Stdlib.UInt32.impl BitwiseOr for UInt32 | impl | 133 |
| Darklang.Stdlib.UInt32.impl BitwiseXor for UInt32 | impl | 138 |
| Darklang.Stdlib.UInt32.impl Compare for UInt32 | impl | 49 |
| Darklang.Stdlib.UInt32.impl Divide for UInt32 | impl | 44 |
| Darklang.Stdlib.UInt32.impl Modulo for UInt32 | impl | 9 |
| Darklang.Stdlib.UInt32.impl Multiply for UInt32 | impl | 27 |
| Darklang.Stdlib.UInt32.impl One for UInt32 | impl | 164 |
| Darklang.Stdlib.UInt32.impl Power for UInt32 | impl | 32 |
| Darklang.Stdlib.UInt32.impl ShiftLeft for UInt32 | impl | 148 |
| Darklang.Stdlib.UInt32.impl ShiftRight for UInt32 | impl | 153 |
| Darklang.Stdlib.UInt32.impl Subtract for UInt32 | impl | 22 |
| Darklang.Stdlib.UInt32.impl ToString for UInt32 | impl | 157 |
| Darklang.Stdlib.UInt32.impl Zero for UInt32 | impl | 161 |

## packages/darklang/stdlib/uint64.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.UInt64.One.one | function | 181 |
| Darklang.Stdlib.UInt64.Zero.zero | function | 178 |
| Darklang.Stdlib.UInt64.impl Add for UInt64 | impl | 15 |
| Darklang.Stdlib.UInt64.impl BitwiseAnd for UInt64 | impl | 144 |
| Darklang.Stdlib.UInt64.impl BitwiseNot for UInt64 | impl | 159 |
| Darklang.Stdlib.UInt64.impl BitwiseOr for UInt64 | impl | 149 |
| Darklang.Stdlib.UInt64.impl BitwiseXor for UInt64 | impl | 154 |
| Darklang.Stdlib.UInt64.impl Compare for UInt64 | impl | 47 |
| Darklang.Stdlib.UInt64.impl Divide for UInt64 | impl | 30 |
| Darklang.Stdlib.UInt64.impl Modulo for UInt64 | impl | 9 |
| Darklang.Stdlib.UInt64.impl Multiply for UInt64 | impl | 25 |
| Darklang.Stdlib.UInt64.impl One for UInt64 | impl | 180 |
| Darklang.Stdlib.UInt64.impl Power for UInt64 | impl | 35 |
| Darklang.Stdlib.UInt64.impl ShiftLeft for UInt64 | impl | 164 |
| Darklang.Stdlib.UInt64.impl ShiftRight for UInt64 | impl | 169 |
| Darklang.Stdlib.UInt64.impl Subtract for UInt64 | impl | 20 |
| Darklang.Stdlib.UInt64.impl ToString for UInt64 | impl | 173 |
| Darklang.Stdlib.UInt64.impl Zero for UInt64 | impl | 177 |

## packages/darklang/stdlib/uint8.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.UInt8.One.one | function | 164 |
| Darklang.Stdlib.UInt8.Zero.zero | function | 161 |
| Darklang.Stdlib.UInt8.impl Add for UInt8 | impl | 17 |
| Darklang.Stdlib.UInt8.impl BitwiseAnd for UInt8 | impl | 127 |
| Darklang.Stdlib.UInt8.impl BitwiseNot for UInt8 | impl | 142 |
| Darklang.Stdlib.UInt8.impl BitwiseOr for UInt8 | impl | 132 |
| Darklang.Stdlib.UInt8.impl BitwiseXor for UInt8 | impl | 137 |
| Darklang.Stdlib.UInt8.impl Compare for UInt8 | impl | 49 |
| Darklang.Stdlib.UInt8.impl Divide for UInt8 | impl | 44 |
| Darklang.Stdlib.UInt8.impl Modulo for UInt8 | impl | 9 |
| Darklang.Stdlib.UInt8.impl Multiply for UInt8 | impl | 27 |
| Darklang.Stdlib.UInt8.impl One for UInt8 | impl | 163 |
| Darklang.Stdlib.UInt8.impl Power for UInt8 | impl | 32 |
| Darklang.Stdlib.UInt8.impl ShiftLeft for UInt8 | impl | 147 |
| Darklang.Stdlib.UInt8.impl ShiftRight for UInt8 | impl | 152 |
| Darklang.Stdlib.UInt8.impl Subtract for UInt8 | impl | 22 |
| Darklang.Stdlib.UInt8.impl ToString for UInt8 | impl | 156 |
| Darklang.Stdlib.UInt8.impl Zero for UInt8 | impl | 160 |

## packages/darklang/stdlib/uuid.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Uuid.ToString.toString | function | 17 |
| Darklang.Stdlib.Uuid.impl ToString for Uuid | impl | 15 |

## packages/darklang/test/test.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Test.Values.dictValue | value | 60 |
| Darklang.Test.Values.listValue | value | 59 |
| Darklang.Test.Values.recordValue | value | 61 |
| Darklang.Test.X | type | 32 |
| Darklang.Test.definedPolyFn | function | 38 |
| Darklang.Test.derrorFn | function | 29 |
| Darklang.Test.fnWithTypeArgAndOneParam | function | 6 |
| Darklang.Test.polyFn | function | 36 |
| Darklang.Test.returnsInt | function | 11 |
| Darklang.Test.returnsOptionNone | function | 18 |
| Darklang.Test.returnsOptionSome | function | 15 |
| Darklang.Test.returnsResultError | function | 23 |
| Darklang.Test.returnsResultOk | function | 20 |
| Darklang.Test.returnsString | function | 13 |
| Darklang.Test.strFn | function | 37 |
| Darklang.Test.stringFn | function | 4 |
| Darklang.Test.throwsException | function | 26 |
| Darklang.Test.twoStringFn | function | 8 |

## packages/darklang/test/test_earg.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Test.EArgTests.addTwo | function | 4 |
| Darklang.Test.EArgTests.argAndLocalVar | function | 11 |
| Darklang.Test.EArgTests.lambdaTest | function | 21 |
| Darklang.Test.EArgTests.matchArgName | function | 26 |
| Darklang.Test.EArgTests.matchWithNestedShadowing | function | 31 |
| Darklang.Test.EArgTests.matchWithNestedShadowing2 | function | 40 |
| Darklang.Test.EArgTests.nestedShadowing | function | 15 |
| Darklang.Test.EArgTests.shadowTest | function | 7 |
| Darklang.Test.EArgTests.tupleMatchWithArgShadowing | function | 46 |
| test1 | function | 55 |
| test10 | function | 64 |
| test11 | function | 65 |
| test12 | function | 66 |
| test13 | function | 67 |
| test14 | function | 68 |
| test2 | function | 56 |
| test3 | function | 57 |
| test4 | function | 58 |
| test5 | function | 59 |
| test6 | function | 60 |
| test7 | function | 61 |
| test8 | function | 62 |
| test9 | function | 63 |
| testList | function | 70 |

## packages/darklang/test/test_eself.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.ShadowingTest.conditionalShadowing | function | 74 |
| Darklang.ShadowingTest.factorialWithShadowing | function | 38 |
| Darklang.ShadowingTest.incrementWithShadowing | function | 25 |
| Darklang.ShadowingTest.letBindingShadowing | function | 67 |
| Darklang.ShadowingTest.matchWithShadowing | function | 47 |
| Darklang.ShadowingTest.pipeWithShadowing | function | 87 |
| Darklang.ShadowingTest.tupleMatchWithShadowing | function | 57 |
| Darklang.ShadowingTest.valueShadowing | value | 97 |
| Darklang.ShadowingTest.valueShadowing | function | 98 |
| Darklang.Test.factorial | function | 3 |
| Darklang.Test.fibonacci | function | 10 |
| Darklang.Test.sumUpTo | function | 17 |
| test1 | function | 108 |
| test10 | function | 121 |
| test11 | function | 122 |
| test12 | function | 123 |
| test13 | function | 126 |
| test14 | function | 127 |
| test15 | function | 128 |
| test16 | function | 129 |
| test17 | function | 130 |
| test18 | function | 131 |
| test19 | function | 133 |
| test2 | function | 109 |
| test20 | function | 134 |
| test21 | function | 135 |
| test22 | function | 136 |
| test23 | function | 138 |
| test24 | function | 139 |
| test25 | function | 141 |
| test26 | function | 142 |
| test27 | function | 143 |
| test28 | function | 144 |
| test29 | function | 145 |
| test3 | function | 110 |
| test30 | function | 146 |
| test31 | function | 147 |
| test32 | function | 149 |
| test33 | function | 150 |
| test34 | function | 151 |
| test35 | function | 152 |
| test36 | function | 154 |
| test37 | function | 155 |
| test38 | function | 156 |
| test4 | function | 111 |
| test5 | function | 114 |
| test6 | function | 115 |
| test7 | function | 116 |
| test8 | function | 117 |
| test9 | function | 120 |
| testList | function | 158 |

## packages/darklang/tests/cli/instance.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Cli.Tests.Instance.expectContains | function | 127 |

## packages/darklang/tests/language/aliases.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Language.Tests.Aliases.NestedAlias.EmployeeCredential | type | 45 |

## packages/darklang/tests/language/enums.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Language.Tests.Enums.Polymorphism.insert | function | 211 |
| Darklang.Language.Tests.Enums.Polymorphism.insert | function | 230 |
| Darklang.Language.Tests.Enums.Polymorphism.singleton | function | 208 |
| Darklang.Language.Tests.Enums.Polymorphism.toList | function | 240 |
| Darklang.Language.Tests.Enums.Polymorphism.toList | function | 246 |

## packages/darklang/tests/language/equality.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Language.Tests.Equality.ERec2UInt8 | type | 10 |

## packages/darklang/tests/language/nestedFunctions.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Language.Tests.NestedFunctions.sumDown | function | 61 |

## packages/darklang/tests/language/traits.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Language.Tests.Traits.Ambiguity.Describe | trait | 143 |
| Darklang.Language.Tests.Traits.Ambiguity.Describe.short | function | 145 |
| Darklang.Language.Tests.Traits.Ambiguity.Int.Describe.describe | function | 151 |
| Darklang.Language.Tests.Traits.Ambiguity.Int.Describe.short | function | 152 |
| Darklang.Language.Tests.Traits.Ambiguity.Int.Label.describe | function | 155 |
| Darklang.Language.Tests.Traits.Ambiguity.Label | trait | 147 |
| Darklang.Language.Tests.Traits.Ambiguity.impl Describe for Int | impl | 150 |
| Darklang.Language.Tests.Traits.Ambiguity.impl Label for Int | impl | 154 |
| Darklang.Language.Tests.Traits.MethodBounds.Box.Pair.pair | function | 179 |
| Darklang.Language.Tests.Traits.MethodBounds.Pair | trait | 175 |
| Darklang.Language.Tests.Traits.MethodBounds.impl Pair for Box | impl | 178 |
| Darklang.Language.Tests.Traits.Multi.Vec.Add.add | function | 101 |
| Darklang.Language.Tests.Traits.Multi.impl Stdlib.Add for Vec | impl | 100 |
| Darklang.Language.Tests.Traits.Rendering.Point.ToString.toString | function | 374 |
| Darklang.Language.Tests.Traits.Rendering.impl Stdlib.ToString for Point | impl | 373 |
| Darklang.Language.Tests.Traits.Shapes.Bool.Show.show | function | 21 |
| Darklang.Language.Tests.Traits.Shapes.Int.Show.show | function | 18 |
| Darklang.Language.Tests.Traits.Shapes.List.Show.show | function | 24 |
| Darklang.Language.Tests.Traits.Shapes.Show | trait | 10 |
| Darklang.Language.Tests.Traits.Shapes.impl Show for Bool | impl | 20 |
| Darklang.Language.Tests.Traits.Shapes.impl Show for Int | impl | 17 |
| Darklang.Language.Tests.Traits.Shapes.impl Show for Point | impl | 13 |
| Darklang.Language.Tests.Traits.Shapes.impl<'a: Show> Show for List<'a> | impl | 23 |
| Darklang.Language.Tests.Traits.SortOrder.Desc.Compare.greaterThan | function | 214 |
| Darklang.Language.Tests.Traits.SortOrder.Desc.Compare.greaterThanOrEqualTo | function | 215 |
| Darklang.Language.Tests.Traits.SortOrder.Desc.Compare.lessThan | function | 212 |
| Darklang.Language.Tests.Traits.SortOrder.Desc.Compare.lessThanOrEqualTo | function | 213 |
| Darklang.Language.Tests.Traits.SortOrder.impl Compare for Desc | impl | 211 |

## packages/darklang/tests/scm/conflicts.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.SCM.Tests.Conflicts.Helpers.base | function | 39 |

## packages/darklang/tests/stdlib/html.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Stdlib.Html.Tests.HtmlTag | type | 4 |

## packages/darklang/tracing/format.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Tracing.Format.jsonEscape | function | 15 |

## packages/darklang/tracing/types.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Tracing.Call | type | 83 |

## packages/internal/tests.dark

| Declaration | Kind | Line |
| --- | --- | ---: |
| Darklang.Internal.Test.PTTest | type | 9 |
| Darklang.Internal.Test.WTTest | type | 3 |
| Darklang.Internal.Test.parseSingleTestFromFile | function | 38 |
| Darklang.Internal.Test.parseTest | function | 15 |

