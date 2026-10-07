# Imported upstream test inventory

Generated from `test/test-suite-tooling/TestRunner.ml` and the imported
`test/fixtures/e2e/upstream/**/*.dark` files. Regenerate with
`python3 scripts/audit-upstream-gates.py`; verify with `--check`.

**105 files; 44 whole-file exclusions; 260 line-number entries
across 24 files.** A line entry is not a skipped-test count.
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
| [language/apply/eapply.dark](../../test/fixtures/e2e/upstream/language/apply/eapply.dark) | enabled | 120 |
| [language/apply/einfix.dark](../../test/fixtures/e2e/upstream/language/apply/einfix.dark) | enabled | — |
| [language/basic/dfloat.dark](../../test/fixtures/e2e/upstream/language/basic/dfloat.dark) | enabled | — |
| [language/basic/eand.dark](../../test/fixtures/e2e/upstream/language/basic/eand.dark) | enabled | 5, 7, 11 |
| [language/basic/elet.dark](../../test/fixtures/e2e/upstream/language/basic/elet.dark) | enabled | 67 |
| [language/basic/eor.dark](../../test/fixtures/e2e/upstream/language/basic/eor.dark) | enabled | 6, 16, 17 |
| [language/basic/estring.dark](../../test/fixtures/e2e/upstream/language/basic/estring.dark) | enabled | 11, 17, 21, 28 |
| [language/basic/evariable.dark](../../test/fixtures/e2e/upstream/language/basic/evariable.dark) | enabled | 3 |
| [language/big.dark](../../test/fixtures/e2e/upstream/language/big.dark) | disabled | — |
| [language/builtin-introspection.dark](../../test/fixtures/e2e/upstream/language/builtin-introspection.dark) | disabled | — |
| [language/collections/dlist.dark](../../test/fixtures/e2e/upstream/language/collections/dlist.dark) | enabled | — |
| [language/collections/dtuple.dark](../../test/fixtures/e2e/upstream/language/collections/dtuple.dark) | enabled | — |
| [language/collections/edict.dark](../../test/fixtures/e2e/upstream/language/collections/edict.dark) | enabled | — |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | enabled | 39, 148, 149, 157, 159, 175, 177 |
| [language/custom-data/enums.dark](../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) | enabled | 7, 11, 15, 17, 22, 23, 24, 26, 27, 28, 30, 45, 65, 67, 69, 74, 101, 104, 109 |
| [language/custom-data/record-field-acess.dark](../../test/fixtures/e2e/upstream/language/custom-data/record-field-acess.dark) | enabled | — |
| [language/custom-data/records.dark](../../test/fixtures/e2e/upstream/language/custom-data/records.dark) | enabled | — |
| [language/custom-data/values.dark](../../test/fixtures/e2e/upstream/language/custom-data/values.dark) | disabled | — |
| [language/derror.dark](../../test/fixtures/e2e/upstream/language/derror.dark) | enabled | 2, 10, 13, 15, 16, 18, 19, 22, 23, 32 |
| [language/effect-ceiling.dark](../../test/fixtures/e2e/upstream/language/effect-ceiling.dark) | disabled | — |
| [language/elambda.dark](../../test/fixtures/e2e/upstream/language/elambda.dark) | enabled | — |
| [language/error-syntax.dark](../../test/fixtures/e2e/upstream/language/error-syntax.dark) | enabled | — |
| [language/error-type-names.dark](../../test/fixtures/e2e/upstream/language/error-type-names.dark) | disabled | — |
| [language/flow-control/eif.dark](../../test/fixtures/e2e/upstream/language/flow-control/eif.dark) | enabled | 1, 12, 13, 14, 20 |
| [language/flow-control/ematch.dark](../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark) | enabled | — |
| [language/flow-control/epipe.dark](../../test/fixtures/e2e/upstream/language/flow-control/epipe.dark) | enabled | — |
| [language/interpreter.dark](../../test/fixtures/e2e/upstream/language/interpreter.dark) | enabled | — |
| [language/nested-fns.dark](../../test/fixtures/e2e/upstream/language/nested-fns.dark) | enabled | 55, 60 |
| [language/runtime-to-programtypes.dark](../../test/fixtures/e2e/upstream/language/runtime-to-programtypes.dark) | disabled | — |
| [scm/branch-identity.dark](../../test/fixtures/e2e/upstream/scm/branch-identity.dark) | disabled | — |
| [scm/commit-hash.dark](../../test/fixtures/e2e/upstream/scm/commit-hash.dark) | disabled | — |
| [scm/conflicts.dark](../../test/fixtures/e2e/upstream/scm/conflicts.dark) | disabled | — |
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
| [stdlib/crypto.dark](../../test/fixtures/e2e/upstream/stdlib/crypto.dark) | disabled | — |
| [stdlib/date.dark](../../test/fixtures/e2e/upstream/stdlib/date.dark) | enabled | — |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | enabled | 21, 30, 32, 59, 61, 74, 144, 145, 146, 147, 159, 227, 237, 242, 245, 251, 273, 279, 282, 311, 315, 322, 327, 332, 334, 351, 353, 357, 387, 389, 391, 394, 396, 398, 400, 402, 404, 406, 408 |
| [stdlib/discovery.dark](../../test/fixtures/e2e/upstream/stdlib/discovery.dark) | enabled | — |
| [stdlib/duration.dark](../../test/fixtures/e2e/upstream/stdlib/duration.dark) | enabled | — |
| [stdlib/earg.dark](../../test/fixtures/e2e/upstream/stdlib/earg.dark) | disabled | — |
| [stdlib/eself.dark](../../test/fixtures/e2e/upstream/stdlib/eself.dark) | disabled | — |
| [stdlib/float.dark](../../test/fixtures/e2e/upstream/stdlib/float.dark) | enabled | 45, 47, 51, 55, 58, 59, 64, 65, 71, 73, 76, 79, 81, 87, 89, 106, 107, 110, 111, 113, 124, 125, 127, 133, 134, 136, 160, 173, 176, 179, 242, 244, 253, 255 |
| [stdlib/html.dark](../../test/fixtures/e2e/upstream/stdlib/html.dark) | enabled | 42, 44, 66, 69, 72, 75, 83 |
| [stdlib/http.dark](../../test/fixtures/e2e/upstream/stdlib/http.dark) | disabled | — |
| [stdlib/httpclient.dark](../../test/fixtures/e2e/upstream/stdlib/httpclient.dark) | disabled | — |
| [stdlib/httpserver.dark](../../test/fixtures/e2e/upstream/stdlib/httpserver.dark) | enabled | 29, 33, 37 |
| [stdlib/ints/int.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int.dark) | enabled | — |
| [stdlib/ints/int128.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int128.dark) | enabled | — |
| [stdlib/ints/int16.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int16.dark) | enabled | — |
| [stdlib/ints/int32.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int32.dark) | enabled | 126 |
| [stdlib/ints/int64.dark](../../test/fixtures/e2e/upstream/stdlib/ints/int64.dark) | enabled | 45, 60, 90, 210, 368 |
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
| [stdlib/list.dark](../../test/fixtures/e2e/upstream/stdlib/list.dark) | enabled | 22, 23, 24, 53, 61, 65, 71, 75, 81, 87, 92, 93, 101, 109, 129, 130, 136, 140, 161, 162, 174, 180, 201, 206, 216, 218, 223, 224, 250, 264, 269, 302, 314, 346, 354 |
| [stdlib/math.dark](../../test/fixtures/e2e/upstream/stdlib/math.dark) | enabled | 27, 30 |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | enabled | 302, 304, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 377, 418, 419 |
| [stdlib/option.dark](../../test/fixtures/e2e/upstream/stdlib/option.dark) | enabled | 44, 75, 119, 138, 148, 158, 170, 176, 190, 204, 211, 218, 234, 242, 255, 260 |
| [stdlib/pretty.dark](../../test/fixtures/e2e/upstream/stdlib/pretty.dark) | disabled | — |
| [stdlib/prettyPrinter.dark](../../test/fixtures/e2e/upstream/stdlib/prettyPrinter.dark) | disabled | — |
| [stdlib/regex.dark](../../test/fixtures/e2e/upstream/stdlib/regex.dark) | enabled | — |
| [stdlib/result.dark](../../test/fixtures/e2e/upstream/stdlib/result.dark) | enabled | 19, 24, 57, 67, 79, 85, 91, 97, 110, 117, 124, 139, 147, 155, 178, 185, 188, 277, 294 |
| [stdlib/sqlite.dark](../../test/fixtures/e2e/upstream/stdlib/sqlite.dark) | disabled | — |
| [stdlib/sse.dark](../../test/fixtures/e2e/upstream/stdlib/sse.dark) | disabled | — |
| [stdlib/stream.dark](../../test/fixtures/e2e/upstream/stdlib/stream.dark) | disabled | — |
| [stdlib/string.dark](../../test/fixtures/e2e/upstream/stdlib/string.dark) | disabled | — |
| [stdlib/tuple.dark](../../test/fixtures/e2e/upstream/stdlib/tuple.dark) | enabled | — |
| [stdlib/uuid.dark](../../test/fixtures/e2e/upstream/stdlib/uuid.dark) | enabled | — |
| [stdlib/x509.dark](../../test/fixtures/e2e/upstream/stdlib/x509.dark) | enabled | — |

## Blank and comment line entries

These stale-looking entries need assertion-location revalidation before
editing the runner. They must not be counted as unsupported expressions.

| File | Line | Source line |
| --- | --- | --- |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | 39 | blank |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | 149 | blank |
| [language/custom-data/aliases.dark](../../test/fixtures/e2e/upstream/language/custom-data/aliases.dark) | 159 | blank |
| [language/custom-data/enums.dark](../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) | 109 | blank |
| [language/apply/eapply.dark](../../test/fixtures/e2e/upstream/language/apply/eapply.dark) | 120 | comment |
| [language/basic/eand.dark](../../test/fixtures/e2e/upstream/language/basic/eand.dark) | 11 | comment |
| [language/basic/eor.dark](../../test/fixtures/e2e/upstream/language/basic/eor.dark) | 16 | comment |
| [language/basic/eor.dark](../../test/fixtures/e2e/upstream/language/basic/eor.dark) | 17 | comment |
| [language/basic/estring.dark](../../test/fixtures/e2e/upstream/language/basic/estring.dark) | 17 | comment |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 61 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 144 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 145 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 237 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 273 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 391 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 398 | blank |
| [stdlib/dict.dark](../../test/fixtures/e2e/upstream/stdlib/dict.dark) | 408 | blank |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | 370 | blank |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | 374 | blank |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | 418 | comment |
| [stdlib/nomodule.dark](../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | 419 | blank |

21 of the 260 entries point to blank or comment lines.
