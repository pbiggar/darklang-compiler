# Repeatable compatibility ledger

Generated from pinned sources and maintained decisions. No compatibility probes or new tests are generated.

Upstream: `a9d93efc853486244630dad693fe79f6f4882cf6`; target: `linux-x64`.

## Language features

| Feature | Status | Review needed |
| --- | --- | --- |
| Async | missing | yes |
| Bindings, functions and closures | implemented | yes |
| Effects | partial | yes |
| Generic functions and algebraic types | implemented | yes |
| Lists, dictionaries and tuples | implemented | yes |
| Modules and name resolution | implemented | yes |
| Named package tests | partial | no |
| Pattern matching | implemented | yes |
| Primitive literals and numeric types | implemented | yes |
| Recursion | implemented | yes |
| Runtime reflection and dynamic evaluation | missing | no |
| Traits | implemented | no |

## Source equivalency by declaration

Counts are functions, values, types, traits and implementations. Named tests are in the test inventory. Packages include StdLib; file splits do not determine declaration matches. Identical means exact declaration source bytes, excluding surrounding comments/blank lines.

| Ledger | Identical | Different | Missing | Ambiguous | Compiler-only |
| --- | ---: | ---: | ---: | ---: | ---: |
| stdlib | 388 | 1010 | 609 | 0 | 3442 |
| packages | 393 | 1051 | 4312 | 0 | 3445 |

## Existing test references

Lexical references are coverage hints, not evidence of execution. One deterministic witness (the shortest indirect path) and the total reference count per declaration are in `porting/generated/ledger.json`.

| Direct | Indirect | Maintained | Not identified |
| ---: | ---: | ---: | ---: |
| 1524 | 3144 | 170 | 918 |

Package compilation state: **not-run**. Each declaration links to its containing source-unit receipt. A failed unit does not establish that every declaration fails independently. Original units are parsed, checked and lowered through the native preamble pipeline.

| Declarations in compiled units | In failed units | In blocked units | Stale | Not run |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 0 | 5756 |

## Parser and builtins

Builtin mappings distinguish callable Builtin declarations from source implementation counterparts. A counterpart is not a callable alias or a behavior pass. Computed registrations remain explicit review items.

- parser: compiler-only: 19, maintained: 9
- builtins: declared-builtin: 75, maintained: 4, missing: 174, source-counterpart: 527
- Computed builtin constructor templates: 7 (reviewed literal call sites expanded; not an assembled registry)

## Interpreter tests versus compiler tests

Source inventory only; enabled tests are assumed to pass. Compiler counterparts include existing upstream fixtures and named declarations. Recognizable named assertions and Test.all groups match legacy assertions by content; partial groups stay explicit. Helper-driven migrations require maintained mappings. Helper/preamble equivalence is not inferred from assertion content.

| Upstream format | Total | Identical | Adapted | Different | Partial | Missing | Enabled | Disabled | Ambiguous |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| legacy | 239 | 173 | 0 | 0 | 0 | 57 | 173 | 0 | 9 |
| named | 2107 | 88 | 369 | 0 | 125 | 1524 | 374 | 83 | 1 |

Unmatched compiler tests: 9616 (8748 enabled). Older imported fixtures absent from the current pin remain visible here; unmatched does not prove compiler authorship.

Whole-file gates, individual line gates, fixture skip metadata and compiler expectation overrides are recorded per counterpart. Overrides are never counted as identical. Source forms that could not be indexed: 2.

## Review queue

- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L13
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L130
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L137
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L17
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L6
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-posix-fnmatch.dark#L67
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-syscall.dark#L7
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-syscall.dark#L8
- Ambiguous test counterpart: backend/testfiles/execution/stdlib/cli-system-syscall.dark#L4
- Ambiguous test counterpart: packages/darklang/tests/language/basic.dark#Darklang.Language.Tests.Basic.and
- Ambiguous test counterpart: packages/darklang/tests/language/basic.dark#Darklang.Language.Tests.Basic.or
- Ambiguous test counterpart: packages/darklang/tests/language/values.dark#Darklang.Language.Tests.Values.Package.values
- Ambiguous test counterpart: packages/darklang/tests/stdlib/bool.dark#Darklang.Stdlib.Bool.Tests.andTruthTable
- Ambiguous test counterpart: packages/darklang/tests/stdlib/bool.dark#Darklang.Stdlib.Bool.Tests.orTruthTable
- Ambiguous test counterpart: packages/darklang/tests/stdlib/char.dark#Darklang.Stdlib.Char.Tests.isWhitespace
- Ambiguous test counterpart: packages/darklang/tests/stdlib/dateTime.dark#Darklang.Stdlib.DateTime.Tests.fieldsIn1970
- Ambiguous test counterpart: packages/darklang/tests/stdlib/float.dark#Darklang.Stdlib.Float.Tests.toIntTruncatesWhereFloorRoundsDown
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.Option.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.Package.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.Result.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt128.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt128.UInt128Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt16.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt16.UInt16Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt32.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt32.UInt32Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt64.Basic.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/json.dark#Darklang.Stdlib.Json.Tests.UInt64.UInt64Limits.serialize
- Ambiguous test counterpart: packages/darklang/tests/stdlib/list.dark#Darklang.Stdlib.List.Tests.map
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.dropFirst
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.length
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.lengthFastPath
- Ambiguous test counterpart: packages/darklang/tests/stdlib/string.dark#Darklang.Stdlib.String.Tests.toBytesLength
- Repeated declaration identity: packages/darklang/test/test_eself.dark#Darklang.ShadowingTest.valueShadowing
- Repeated declaration identity: packages/darklang/tests/language/enums.dark#Darklang.Language.Tests.Enums.Polymorphism.insert
- Repeated declaration identity: packages/darklang/tests/language/enums.dark#Darklang.Language.Tests.Enums.Polymorphism.toList
- Review feature status: Async
- Review feature status: Bindings, functions and closures
- Review feature status: Effects
- Review feature status: Generic functions and algebraic types
- Review feature status: Lists, dictionaries and tuples
- Review feature status: Modules and name resolution
- Review feature status: Pattern matching
- Review feature status: Primitive literals and numeric types
- Review feature status: Recursion
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
- test/fixtures/e2e/upstream/cloud/db.dark:728: unindexed legacy test statement
- test/fixtures/e2e/upstream/language/basic/estring.dark:21: unindexed legacy test statement
