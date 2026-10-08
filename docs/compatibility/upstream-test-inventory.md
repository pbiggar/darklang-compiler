# Imported upstream test inventory

Generated from `test/test-suite-tooling/TestRunner.ml` and the imported
`test/fixtures/e2e/upstream/**/*.dark` files. Regenerate with
`python3 scripts/audit-upstream-gates.py`; verify with `--check`.

**105 files; 34 whole-file exclusions; 258 line-number entries
across 21 files.** A line entry is not a skipped-test count.
The runner matches the `L<number>:` assertion name produced by the fixture
parser. Declarations and multiline assertions require parser inspection;
the source line alone does not establish whether a gate suppresses a test.
Other skip metadata and runtime filters are outside this inventory.

| Imported file | Whole-file gate | Individual line entries |
| --- | --- | --- |
| [cli/app-service-safety.dark](../../test/fixtures/e2e/upstream/cli/app-service-safety.dark) | disabled | — |
| [cli/command-completions.dark](../../test/fixtures/e2e/upstream/cli/command-completions.dark) | disabled | — |
| [cli/deprecation-kinds.dark](../../test/fixtures/e2e/upstream/cli/deprecation-kinds.dark) | disabled | — |
| [cli/file.dark](../../test/fixtures/e2e/upstream/cli/file.dark) | enabled | — |
| [cli/include-parsing.dark](../../test/fixtures/e2e/upstream/cli/include-parsing.dark) | disabled | — |
| [cli/outliner.dark](../../test/fixtures/e2e/upstream/cli/outliner.dark) | disabled | — |
| [cli/permissions-display.dark](../../test/fixtures/e2e/upstream/cli/permissions-display.dark) | disabled | — |
| [cli/permissions-grammar.dark](../../test/fixtures/e2e/upstream/cli/permissions-grammar.dark) | disabled | — |
| [cli/tailscale.dark](../../test/fixtures/e2e/upstream/cli/tailscale.dark) | disabled | — |
| [cli/workbench-repl.dark](../../test/fixtures/e2e/upstream/cli/workbench-repl.dark) | disabled | — |
| [cloud/db.dark](../../test/fixtures/e2e/upstream/cloud/db.dark) | disabled | — |
| [language/apply/eapply.dark](../../test/fixtures/e2e/upstream/language/apply/eapply.dark) | enabled | — |
| [language/apply/einfix.dark](../../test/fixtures/e2e/upstream/language/apply/einfix.dark) | enabled | — |
| [language/basic/dfloat.dark](../../test/fixtures/e2e/upstream/language/basic/dfloat.dark) | enabled | — |
| [language/basic/eand.dark](../../test/fixtures/e2e/upstream/language/basic/eand.dark) | enabled | — |
| [language/basic/elet.dark](../../test/fixtures/e2e/upstream/language/basic/elet.dark) | enabled | — |
| [language/basic/eor.dark](../../test/fixtures/e2e/upstream/language/basic/eor.dark) | enabled | 6 |
| [language/basic/estring.dark](../../test/fixtures/e2e/upstream/language/basic/estring.dark) | enabled | — |
| [language/basic/evariable.dark](../../test/fixtures/e2e/upstream/language/basic/evariable.dark) | enabled | 3 |
| [language/big.dark](../../test/fixtures/e2e/upstream/language/big.dark) | enabled | — |
| [language/builtin-introspection.dark](../../test/fixtures/e2e/upstream/language/builtin-introspection.dark) | disabled | — |
| [language/collections/dlist.dark](../../test/fixtures/e2e/upstream/language/collections/dlist.dark) | enabled | — |
| [language/collections/dtuple.dark](../../test/fixtures/e2e/upstream/language/collections/dtuple.dark) | enabled | — |
| [language/collections/edict.dark](../../test/fixtures/e2e/upstream/language/collections/edict.dark) | enabled | — |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | enabled | — |
| [language/custom-data/enums.dark](../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) | enabled | 7, 11, 15, 17, 22, 23, 24, 26, 27, 28, 30, 65, 67, 69, 101, 104 |
| [language/custom-data/record-field-acess.dark](../../test/fixtures/e2e/upstream/language/custom-data/record-field-acess.dark) | enabled | — |
| [language/custom-data/records.dark](../../test/fixtures/e2e/upstream/language/custom-data/records.dark) | enabled | — |
| [language/custom-data/values.dark](../../test/fixtures/e2e/upstream/language/custom-data/values.dark) | enabled | 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 76, 77, 78, 80, 81, 83, 84, 86, 87, 89, 90, 92, 93, 95, 96, 98, 99, 101, 102, 104, 105, 107, 108, 110, 111, 113, 114, 116, 117, 119, 120, 122, 123, 125, 126, 128, 129 |
| [language/derror.dark](../../test/fixtures/e2e/upstream/language/derror.dark) | enabled | 2, 10, 15, 22, 23 |
| [language/effect-ceiling.dark](../../test/fixtures/e2e/upstream/language/effect-ceiling.dark) | disabled | — |
| [language/elambda.dark](../../test/fixtures/e2e/upstream/language/elambda.dark) | enabled | — |
| [language/error-syntax.dark](../../test/fixtures/e2e/upstream/language/error-syntax.dark) | enabled | — |
| [language/error-type-names.dark](../../test/fixtures/e2e/upstream/language/error-type-names.dark) | disabled | — |
| [language/flow-control/eif.dark](../../test/fixtures/e2e/upstream/language/flow-control/eif.dark) | enabled | 1, 14 |
| [language/flow-control/ematch.dark](../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark) | enabled | — |
| [language/flow-control/epipe.dark](../../test/fixtures/e2e/upstream/language/flow-control/epipe.dark) | enabled | — |
| [language/interpreter.dark](../../test/fixtures/e2e/upstream/language/interpreter.dark) | enabled | — |
| [language/nested-fns.dark](../../test/fixtures/e2e/upstream/language/nested-fns.dark) | enabled | 55, 60 |
| [language/runtime-to-programtypes.dark](../../test/fixtures/e2e/upstream/language/runtime-to-programtypes.dark) | disabled | — |
| [scm/branch-identity.dark](../../test/fixtures/e2e/upstream/scm/branch-identity.dark) | enabled | 9, 20, 23, 27, 31, 32 |
| [scm/commit-hash.dark](../../test/fixtures/e2e/upstream/scm/commit-hash.dark) | disabled | — |
| [scm/conflicts.dark](../../test/fixtures/e2e/upstream/scm/conflicts.dark) | enabled | 51, 58, 64, 70, 79, 86, 92, 99, 105, 111, 129, 136, 144, 155, 161, 169, 175, 184, 194, 204, 220, 224, 228, 232, 233, 234, 236, 242, 243, 246, 251, 257, 258, 259, 263, 265, 266 |
| [scm/constraint-kinds.dark](../../test/fixtures/e2e/upstream/scm/constraint-kinds.dark) | disabled | — |
| [scm/lww.dark](../../test/fixtures/e2e/upstream/scm/lww.dark) | disabled | — |
| [scm/matter-routes.dark](../../test/fixtures/e2e/upstream/scm/matter-routes.dark) | disabled | — |
| [scm/propagation-policy.dark](../../test/fixtures/e2e/upstream/scm/propagation-policy.dark) | disabled | — |
| [scm/removal-conflicts.dark](../../test/fixtures/e2e/upstream/scm/removal-conflicts.dark) | disabled | — |
| [scm/sync-seen-everything.dark](../../test/fixtures/e2e/upstream/scm/sync-seen-everything.dark) | disabled | — |
| [scm/sync-wire.dark](../../test/fixtures/e2e/upstream/scm/sync-wire.dark) | disabled | — |
| [stachu/darklangParser.dark](../../test/fixtures/e2e/upstream/stachu/darklangParser.dark) | disabled | — |
| [stachu/parser.dark](../../test/fixtures/e2e/upstream/stachu/parser.dark) | disabled | — |
| [stachu/tinyLang.dark](../../test/fixtures/e2e/upstream/stachu/tinyLang.dark) | disabled | — |
| [stdlib/alt-json.dark](../../test/fixtures/e2e/upstream/stdlib/alt-json.dark) | enabled | — |
| [stdlib/base64.dark](../../test/fixtures/e2e/upstream/stdlib/base64.dark) | enabled | 7, 9, 10, 11, 12, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 33, 34, 35, 36, 39, 40, 43, 44, 45, 46, 47 |
| [stdlib/bool.dark](../../test/fixtures/e2e/upstream/stdlib/bool.dark) | enabled | — |
| [stdlib/bytes.dark](../../test/fixtures/e2e/upstream/stdlib/bytes.dark) | enabled | — |
| [stdlib/char.dark](../../test/fixtures/e2e/upstream/stdlib/char.dark) | enabled | — |
| [stdlib/cli-color.dark](../../test/fixtures/e2e/upstream/stdlib/cli-color.dark) | enabled | — |
| [stdlib/cli-glob.dark](../../test/fixtures/e2e/upstream/stdlib/cli-glob.dark) | enabled | — |
| [stdlib/cli-path.dark](../../test/fixtures/e2e/upstream/stdlib/cli-path.dark) | enabled | — |
| [stdlib/cli-process.dark](../../test/fixtures/e2e/upstream/stdlib/cli-process.dark) | enabled | — |
| [stdlib/cli-tui-text.dark](../../test/fixtures/e2e/upstream/stdlib/cli-tui-text.dark) | enabled | — |
| [stdlib/crypto.dark](../../test/fixtures/e2e/upstream/stdlib/crypto.dark) | enabled | — |
| [stdlib/date.dark](../../test/fixtures/e2e/upstream/stdlib/date.dark) | enabled | — |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | enabled | — |
| [stdlib/discovery.dark](../../test/fixtures/e2e/upstream/stdlib/discovery.dark) | enabled | — |
| [stdlib/duration.dark](../../test/fixtures/e2e/upstream/stdlib/duration.dark) | enabled | — |
| [stdlib/earg.dark](../../test/fixtures/e2e/upstream/stdlib/earg.dark) | disabled | — |
| [stdlib/eself.dark](../../test/fixtures/e2e/upstream/stdlib/eself.dark) | disabled | — |
| [stdlib/float.dark](../../test/fixtures/e2e/upstream/stdlib/float.dark) | enabled | 47, 55, 58, 59, 64, 65, 73, 76, 81, 89, 107, 110, 111, 125, 127, 134, 136, 160, 173, 176, 244, 255 |
| [stdlib/html.dark](../../test/fixtures/e2e/upstream/stdlib/html.dark) | enabled | 42, 44, 66, 69, 72, 75, 83 |
| [stdlib/http.dark](../../test/fixtures/e2e/upstream/stdlib/http.dark) | enabled | 21, 27, 33, 48, 51, 57, 63, 102, 108, 110, 111, 114, 117, 122, 123, 124, 128, 129, 130, 131, 133, 137, 144, 149, 155, 161 |
| [stdlib/httpclient.dark](../../test/fixtures/e2e/upstream/stdlib/httpclient.dark) | enabled | 71, 108, 111, 131, 132, 133, 134 |
| [stdlib/httpserver.dark](../../test/fixtures/e2e/upstream/stdlib/httpserver.dark) | enabled | 29, 33, 37 |
| [stdlib/ints/int.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int.dark) | enabled | — |
| [stdlib/ints/int128.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int128.dark) | enabled | — |
| [stdlib/ints/int16.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int16.dark) | enabled | — |
| [stdlib/ints/int32.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int32.dark) | enabled | — |
| [stdlib/ints/int64.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int64.dark) | enabled | 368 |
| [stdlib/ints/int8.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int8.dark) | enabled | 47 |
| [stdlib/ints/uint128.dark](../../test/fixtures/e2e/upstream/stdlib/ints/uint128.dark) | enabled | — |
| [stdlib/ints/uint16.dark](../../test/fixtures/e2e/upstream/stdlib/ints/uint16.dark) | enabled | — |
| [stdlib/ints/uint32.dark](../../test/fixtures/e2e/upstream/stdlib/ints/uint32.dark) | enabled | — |
| [stdlib/ints/uint64.dark](../../test/fixtures/e2e/upstream/stdlib/ints/uint64.dark) | enabled | — |
| [stdlib/ints/uint8.dark](../../test/fixtures/e2e/upstream/stdlib/ints/uint8.dark) | enabled | — |
| [stdlib/json.dark](../../test/fixtures/e2e/upstream/stdlib/json.dark) | disabled | — |
| [stdlib/language-tools/parsedFileShape.dark](../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark) | disabled | — |
| [stdlib/language-tools/pickLocation.dark](../../test/fixtures/e2e/upstream/stdlib/language-tools/pickLocation.dark) | disabled | — |
| [stdlib/language-tools/semanticTokenization.dark](../../test/fixtures/e2e/upstream/stdlib/language-tools/semanticTokenization.dark) | disabled | — |
| [stdlib/list.dark](../../test/fixtures/e2e/upstream/stdlib/list.dark) | enabled | 61, 65, 71, 75, 81, 87, 92, 101, 109, 129, 130, 136, 174, 180, 216, 223, 224, 264, 269, 314 |
| [stdlib/math.dark](../../test/fixtures/e2e/upstream/stdlib/math.dark) | enabled | 27, 30 |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | enabled | — |
| [stdlib/option.dark](../../test/fixtures/e2e/upstream/stdlib/option.dark) | enabled | 44, 75, 119, 148, 170, 176, 204, 211, 218, 242, 255, 260 |
| [stdlib/pretty.dark](../../test/fixtures/e2e/upstream/stdlib/pretty.dark) | enabled | — |
| [stdlib/prettyPrinter.dark](../../test/fixtures/e2e/upstream/stdlib/prettyPrinter.dark) | disabled | — |
| [stdlib/regex.dark](../../test/fixtures/e2e/upstream/stdlib/regex.dark) | enabled | — |
| [stdlib/result.dark](../../test/fixtures/e2e/upstream/stdlib/result.dark) | enabled | 57, 79, 85, 110, 117, 139, 147, 178 |
| [stdlib/sqlite.dark](../../test/fixtures/e2e/upstream/stdlib/sqlite.dark) | disabled | — |
| [stdlib/sse.dark](../../test/fixtures/e2e/upstream/stdlib/sse.dark) | enabled | — |
| [stdlib/stream.dark](../../test/fixtures/e2e/upstream/stdlib/stream.dark) | enabled | — |
| [stdlib/string.dark](../../test/fixtures/e2e/upstream/stdlib/string.dark) | disabled | — |
| [stdlib/tuple.dark](../../test/fixtures/e2e/upstream/stdlib/tuple.dark) | enabled | — |
| [stdlib/uuid.dark](../../test/fixtures/e2e/upstream/stdlib/uuid.dark) | enabled | — |
| [stdlib/x509.dark](../../test/fixtures/e2e/upstream/stdlib/x509.dark) | enabled | — |

## Blank and comment line entries

These stale-looking entries need assertion-location revalidation before
editing the runner. They must not be counted as unsupported expressions.

| File | Line | Source line |
| --- | --- | --- |

0 of the 258 entries point to blank or comment lines.
