# Compatibility test failure ledger

Audited on 2026-10-08 (UTC), against compiler revision `4a61318019fb8dea7f34a0bfec4df33f1fa18f5c` on Linux x86-64.

**The previous complete exclusion audit reran 2,274 assertions across 50 files.**
**66 assertions were enabled between the initial audit and the forward arithmetic follow-up.**

Results from that complete audit, including enabled neighbours: 3,599 assertions, 1,325 passed and 2,274 failed.
That audit used 34 whole-file gates and 205 assertion lines across 16 mixed files.

Current configured exclusions: **1,184 assertions across 65 files**, including
**69 unsupported interpreter runtime-error assertions**. There are 31 whole-file
gates and 308 assertion lines across 34 mixed files. The earlier retirement audit reran the 19 affected files
individually without gates: **1,006 passed
and 110 failed out of 1,116 assertions**. The ungated application fixture contributes 33 cascading preamble failures;
disabling only its dependent L118 assertion lets the existing fallback prune the
helper, and **33/33** remaining assertions pass. At that point, the other failures comprised
69 dependent assertions and eight existing exclusions. `derror.dark:L10` passes and remains enabled.

Constructor arity follow-up: named enum cases and payload counts are validated before generic argument inference, including aliases and explicit arguments. Option `None 5` and `Some(5, 6)` now report field-count compile errors; upstream enum L16 and L19 are enabled with phase-specific overrides. The focused regression passed **8/8** (the original checker failed four); the enum fixture passed **33/33** enabled assertions individually. Native build, the full host suite (**11,910/11,910**, after integrating main `38dceec47`), `dune runtest`, and **58/58** leak workloads passed. Parent benchmark verification stopped before measurement because the stored workload digest is incompatible; no baseline was reset.

The assertion harness now compares original parsed expressions. Multiline applications and literal contents are preserved in individual runs and batches. A follow-up retest of `stdlib/dict.dark` passed all 140 assertions after L21 was changed to expect its existing compile-time key type error.

A follow-up against `36002414730437e871b7d457e76b79f859935fb1` recovered `Builtin.unwrap` payload types during lambda lifting. Individual ungated runs passed 191/220 List assertions and 263/264 Int64 assertions. List L161 and Int64 L90 are now enabled; List L162 initially still failed before lambda lifting on an empty list.

Numeric inference follow-up: checking now carries immutable constraints into lambda parameter and body types. The ungated List file passed 192/220 assertions, so L162 is now enabled. Nine focused regressions cover operand order, compound expressions, comparisons, let-bound offsets, and conflicting types. All 55 disabled fixtures were rerun individually against the final compiler; every failing assertion identity matches the updated ledger. The separately retested Dict file passed all 140 assertions.

Explicit numeric types follow-up: List L201 now uses `map2<Int64, Int64, Int64>` because both input lists are empty and subtraction has no numeric type evidence. The explicit Int64 form is now enabled, and the fresh individual List run passed **193/193** enabled assertions. No unused-callback elimination or numeric defaulting was added.

Mixed-list type errors follow-up: List L22–L24 now expect their existing compile-time type errors instead of the interpreter's runtime messages. All three are enabled, and the individual List run passed **196/196** enabled assertions. Compiler behavior is unchanged.

Non-exhaustive callback follow-up: List L93 and L218 retain their original callbacks and now expect `compileerror="Non-exhaustive match expression"`. Both are enabled, and the individual List run passed **198/198** enabled assertions. No fallback match arms or compiler changes were added.

Invalid predicate follow-up: List L53 and L302 now require `compileerror="Expected TBool, got TInt64"` for their unchanged Int64-returning callbacks. Both are enabled, and the individual List run passed **200/200** enabled assertions. Compiler behavior is unchanged.

String predicate follow-up: List L92 and L223 now require `compileerror="Expected TBool, got TString"` for their unchanged String-returning callbacks. Both are enabled, and the individual List run passed **202/202** enabled assertions. Compiler behavior is unchanged.

Verification including the String predicate follow-up on main `dd91bfa7ec01607d5408cfd3b58e55e1f3747847`: native build passed; the host suite passed **11,396/11,396** tests; `dune runtest` passed; the canonical compiled leak gate passed **58/58** workloads. The preceding compiler regressions passed individually and batched. The parent benchmark check stopped before measurement because the stored workload digest is incompatible; no baseline was reset.


Compile-error sweep: 34 assertions across eight fixtures now require compile-time diagnostics for invalid types, names, constructor fields, patterns, and non-exhaustive matches. Their expressions remain unchanged; compiler behavior is unchanged. These assertions are enabled and removed from the failure ledger. The three basic variable, Boolean-or, and if-expression fixtures have no remaining disabled assertions.

Verification of the sweep based on main `5497ec091927098f88c6d0195fe2b0e7d2382385`: native build passed; the complete host suite passed **11,430/11,430** tests; `dune runtest` passed; compiled leaks passed **58/58** workloads. Parent benchmark verification stopped before measurement because the stored workload digest is incompatible; no baseline was reset.

Forward arithmetic follow-up (2026-10-09): the checker retains unresolved operator requirements until later arguments or specialization supply types. Provably unused callbacks are removed before representation selection. Ungated individual runs passed **73/73 Option** and **67/67 Result** assertions; all 17 remaining gates in those files are removed. No numeric default is chosen. The retired AST checker and its support modules are removed, and package catalog validation uses the current Written checker.

Verification with current main `a2771f0cbe82d728de618e3eacd7a1c870e1d330`: native build passed; the complete host suite passed **11,501/11,501** tests; `dune runtest --build-dir _build-forward-verification` passed after avoiding stale action-cache files; compiled leaks passed **58/58** workloads. Parent benchmark verification stopped before measurement because the stored workload digest is incompatible; no baseline was reset.

Runtime failure builtin follow-up (2026-10-09), based on main `2fad7ee2655fecabfa2ef6b199fe2a4fca548e6d`: compiler-owned uses now call `Builtin.crash`, and the compiler no longer registers `Builtin.testRuntimeError`. The public crash regression passed **5/5**; its rejection case fails on the parent compiler because the retired builtin still compiles. The complete host suite passed **11,862/11,862**, `dune runtest` passed, and compiled leaks passed **58/58**. Individual ungated runs of the 19 affected upstream files identified dependencies; the application fixture was subsequently checked with only L118 gated, recovering all 33 neighbours. The 29 benchmark source edits are exact builtin-name substitutions; their parity hashes are refreshed and the parity check passes. Parent benchmark verification stopped before measurement because the stored workload contract digest is incompatible; no baseline was reset.

Application preamble gate correction (2026-10-09), after merge `2021f6b74`: only `eapply.dark:L118` depends on the retired helper. Disabling it lets the existing reduced-preamble fallback omit that helper. All **33/33** neighbours remain enabled and pass; the complete host suite passes **11,895/11,895**, the native build passes, and `dune runtest` passes. No compiler or benchmark source changes are involved in this correction.

Compile-error expectation follow-up (2026-10-09), based on main `bf9cd3ad4`: 16 assertions now require their correct compile-time diagnostics: enum L107, all 13 type-name assertions, and JSON L934/L983. Expressions and interpreter expectations remain unchanged. The upstream reduced-declaration retry now handles phase-specific compile-error messages as well as legacy error messages, preserving the ahead-of-time requirement. Before that retry change, seven type-name cases failed on pruned helper names; the complete type-name fixture now passes **13/13**, and enums passes **34/34** enabled assertions.

The real parser enumerated all 6,734 imported assertions; all 127 remaining legacy error expectations were probed individually before the JSON changes. Missing interpreter helpers and unrelated preamble errors are not accepted as replacement diagnostics. JSON was additionally run in full individually: **511/540** passed before expectation updates; the two genuine static type errors bring it to **513/540**. All **513/513** enabled JSON assertions then passed through the normal runner individually. The whole-file JSON gate is replaced with exact gates for its 27 remaining failures, recovering 511 passing neighbours. In total, **527 assertions** are enabled and removed from the failure ledger.

Verification: on the task's original parent `bf9cd3ad4`, native build, the complete host suite (**12,440/12,440**), `dune runtest`, **58/58** leak workloads, and the generated inventory check passed. After integrating main `2b8f02e06`, native build succeeds, **12,440** host tests pass and the three newly added executable-path assertions fail; the new native executable-path regression also fails. This VM has no `/proc/self/exe`; both failures are reproduced on unmodified main. All 58 leak workloads still pass. Parent benchmark verification stops before measurement on the incompatible stored workload digest; no baseline was reset.

Char/String type follow-up (2026-10-10), based on main `f2e57deb5`: Char and String no longer unify or reconcile with each other. Private typed intrinsics preserve their shared managed-buffer representation for explicit conversions; list rendering, Regex, and terminal width helpers now convert explicitly. The upstream `String.fromList ["a"]` assertion at L418 requires its correct compile error. A sweep of imported negative expectations found no other Char/String mismatch assertion. All 640 String assertions were run individually: 592 passed before the expectation override; the corrected override enables the 593rd. The whole-file gate is replaced with exact gates for the 47 remaining failures, recovering 592 working neighbours. The focused regression passes **26/26**, covering both conversion directions, container and branch types, generic calls, interpolation, grapheme conversions, sharing, and private visibility.

Verification of the Char/String follow-up: native build passes; the complete host suite has **13,067 passing tests and three executable-path failures** caused by this VM lacking `/proc/self/exe`. The 593 enabled String assertions also pass individually through the normal runner, and all **58/58** leak workloads are clean. `dune runtest` encounters the same inherited executable-path regression failure. Parent benchmark verification stops before measurement on the incompatible stored workload digest; no baseline was reset. The generated inventory and parsed assertion counts agree with the updated gates.

Display-width padding follow-up (2026-10-10), based on main `14b3040d8`: `String.padStartToWidth` and `padEndToWidth` copy the pinned interpreter’s display-cell padding logic using `displayWidth`, `repeat`, and ordinary concatenation. Negative targets and text already at least as wide as the target return the original text. All 15 upstream padding assertions are enabled, leaving 32 String exclusions. Eight focused boundary regressions failed on missing function names before implementation and now pass; the enabled upstream String assertions pass **608/608** individually.

Verification: native build passes; the complete host suite has **13,093 passing tests and three inherited executable-path failures** because this VM lacks `/proc/self/exe`. `dune runtest` encounters the same inherited executable-path regression failure. Compiled leaks pass **58/58** workloads. Inventory and parsed gate counts agree (**1,202 excluded assertions**, 31 whole-file gates and 326 line entries across 34 mixed files). Parent benchmark verification stops before measurement on the incompatible stored workload digest; no baseline was reset.

Portable slugify follow-up (2026-10-10), based on main `00ec4fc85`: `Builtin.stringSlugify` is implemented entirely in Darklang. It lowercases, filters codepoints to ASCII letters/digits and separators, discards vertical tab, trims Unicode whitespace, and collapses separator runs to hyphens while preserving existing edge hyphens. Codepoint filtering preserves ASCII bases when unsupported combining marks are removed. Slugify trims its filtered codepoint list directly: reusing the current String.trim Unicode fallback left a leading CRLF grapheme in a mixed-whitespace regression. No general String.trim change is included.

All 18 upstream slugify assertions are enabled; all **626/626** enabled String assertions pass individually, leaving 14 unsupported test-helper assertions. The ten new focused tests fail on missing names before implementation and pass afterwards. Native build passes; the complete host suite has **13,121 passing tests and three inherited executable-path failures** caused by this VM’s missing `/proc/self/exe`. `dune runtest` encounters the same inherited executable-path regression. Compiled leaks pass **58/58** workloads; an additional leak-accounted stress program performs 200 slugify calls on shared dynamic input and completes cleanly. Inventory, parsed gate counts, and diff checks pass (**1,184 exclusions**, 31 whole-file gates and 308 line entries across 34 mixed files). Parent benchmark verification stops before measurement on the incompatible stored workload digest; no baseline was reset.

## Unsupported interpreter test infrastructure

The compiler no longer exposes `Builtin.testRuntimeError`. Its compiler-owned
uses have migrated to `Builtin.crash : String -> Never`. The
[exact 69 assertions in 19 upstream files](failures/interpreter-test-runtime-error.md)
remain unchanged and are disabled as unsupported interpreter infrastructure.
Four overlap the previous failure list; this adds 65 exclusions.

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
| [language/apply/eapply.dark](failures/interpreter-test-runtime-error.md) | 34 | 33 | 1 |
| [language/apply/einfix.dark](failures/interpreter-test-runtime-error.md) | 104 | 101 | 3 |
| [language/basic/eand.dark](failures/interpreter-test-runtime-error.md) | 13 | 11 | 2 |
| [language/basic/elet.dark](failures/interpreter-test-runtime-error.md) | 38 | 28 | 10 |
| [language/basic/eor.dark](failures/interpreter-test-runtime-error.md) | 13 | 11 | 2 |
| [language/builtin-introspection.dark](failures/language/builtin-introspection.md) | 2 | 0 | 2 |
| [language/collections/dlist.dark](failures/interpreter-test-runtime-error.md) | 6 | 4 | 2 |
| [language/collections/dtuple.dark](failures/interpreter-test-runtime-error.md) | 6 | 4 | 2 |
| [language/custom-data/enums.dark](failures/language/custom-data/enums.md) | 38 | 34 | 4 |
| [language/custom-data/record-field-acess.dark](failures/interpreter-test-runtime-error.md) | 5 | 4 | 1 |
| [language/custom-data/records.dark](failures/interpreter-test-runtime-error.md) | 24 | 23 | 1 |
| [language/custom-data/values.dark](failures/language/custom-data/values.md) | 72 | 19 | 53 |
| [language/derror.dark](failures/language/derror.md) | 15 | 2 | 13 |
| [language/effect-ceiling.dark](failures/language/effect-ceiling.md) | 6 | 0 | 6 |
| [language/elambda.dark](failures/interpreter-test-runtime-error.md) | 23 | 18 | 5 |
| [language/error-syntax.dark](failures/interpreter-test-runtime-error.md) | 2 | 1 | 1 |
| [language/flow-control/eif.dark](failures/interpreter-test-runtime-error.md) | 16 | 9 | 7 |
| [language/flow-control/ematch.dark](failures/interpreter-test-runtime-error.md) | 161 | 154 | 7 |
| [language/flow-control/epipe.dark](failures/interpreter-test-runtime-error.md) | 30 | 29 | 1 |
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
| [stdlib/dict.dark](failures/interpreter-test-runtime-error.md) | 140 | 138 | 2 |
| [stdlib/earg.dark](failures/stdlib/earg.md) | 15 | 0 | 15 |
| [stdlib/eself.dark](failures/stdlib/eself.md) | 39 | 0 | 39 |
| [stdlib/float.dark](failures/stdlib/float.md) | 167 | 145 | 22 |
| [stdlib/html.dark](failures/stdlib/html.md) | 99 | 92 | 7 |
| [stdlib/http.dark](failures/stdlib/http.md) | 47 | 21 | 26 |
| [stdlib/httpclient.dark](failures/stdlib/httpclient.md) | 61 | 54 | 7 |
| [stdlib/httpserver.dark](failures/stdlib/httpserver.md) | 7 | 4 | 3 |
| [stdlib/ints/int64.dark](failures/stdlib/ints/int64.md) | 264 | 263 | 1 |
| [stdlib/ints/int8.dark](failures/stdlib/ints/int8.md) | 236 | 235 | 1 |
| [stdlib/json.dark](failures/stdlib/json.md) | 540 | 513 | 27 |
| [stdlib/language-tools/parsedFileShape.dark](failures/stdlib/language-tools/parsedFileShape.md) | 13 | 0 | 13 |
| [stdlib/language-tools/pickLocation.dark](failures/stdlib/language-tools/pickLocation.md) | 30 | 0 | 30 |
| [stdlib/language-tools/semanticTokenization.dark](failures/stdlib/language-tools/semanticTokenization.md) | 102 | 0 | 102 |
| [stdlib/list.dark](failures/stdlib/list.md) | 220 | 211 | 9 |
| [stdlib/math.dark](failures/stdlib/math.md) | 32 | 30 | 2 |
| [stdlib/nomodule.dark](failures/interpreter-test-runtime-error.md) | 228 | 227 | 1 |
| [stdlib/prettyPrinter.dark](failures/stdlib/prettyPrinter.md) | 57 | 0 | 57 |
| [stdlib/sqlite.dark](failures/stdlib/sqlite.md) | 8 | 0 | 8 |
| [stdlib/string.dark](failures/stdlib/string.md) | 640 | 626 | 14 |

## Assertions enabled

- [stdlib/pretty.dark:L186](../../test/fixtures/e2e/upstream/stdlib/pretty.dark#L186)
- [stdlib/dict.dark:L21](../../test/fixtures/e2e/upstream/stdlib/dict.dark#L21) — expects a compile-time type error

## Run conditions

Fixtures were copied byte-for-byte to temporary paths within the upstream fixture directory, bypassing exact-path gates. Each file ran individually with `--ai --e2e-batch-size=1`; failures were extracted from complete per-test output and checked against file totals. Original fixture contents were unchanged.

A shared-preamble failure records a failed invocation rather than a measurement of public semantics. Missing test packages, test-only builtins, network services, and compiler failures remain listed by their observed diagnostics. Results describe this host and configuration.
