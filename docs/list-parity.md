# List parity

This matrix was revalidated from compiler HEAD
`ba8d319f3ea663bce5f4f0c559dc04fb51d1791d` and darklang/dark
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit
`8a402797` and the prior module inventory were used only to locate candidates;
the retained findings were checked against both revisions and current probes.
Performance differences are outside this comparison unless observable behavior
changes.

## Evidence

The interpreter API and algorithms are in
`packages/darklang/stdlib/list.dark`, especially lines 4-117, 161-281,
390-428, 509-600. Its runtime sortable-value ordering and primitive
`sort`/`unique` behavior are in `backend/src/Builtins/Builtins.Pure/Libs/List.fs`
lines 13-171 and 304-348. Expected values and errors are in
`backend/testfiles/execution/stdlib/list.dark` lines 1-378.

The compiler implementation is in `src/DarkCompiler/stdlib/List.dark`: the
public Int contracts and sorting functions are at lines 13-188, indexing and
bounds operations at 221-326, and the newly ported collection functions at
352-502. Comparator helpers are at
`src/DarkCompiler/stdlib/ListSortByComparatorHelpers.dark:7-85`. The polymorphic
`empty` identity is registered at `src/DarkCompiler/Stdlib.fs:220` and lowered
at `src/DarkCompiler/passes/2_AST_to_ANF.fs:4802` and `:8972`.

Canonical comparison is selected from concrete AOT types in
`src/DarkCompiler/passes/1.5_TypeChecking.fs:1255-1324`, synthesized at
`:5583-6012`, and materialized at `:6232-6358`. The type-directed plan covers
the compiler representations of sortable scalar values and recursively covers
lists, tuples, string-keyed dictionaries, records, and enums. Unsupported
concrete types are rejected during type checking.

Focused compiler coverage is in `src/Tests/e2e/list_parity.e2e`. It covers the
changed Int contracts, inclusive range, bounds, repeat error, structured chunk
error, collection additions, stable sorting, canonical compound ordering,
invalid comparators, and retained extensions. The upstream same-source List
corpus enabled in `src/Tests/test-suite-tooling/TestRunner.fs` supplies 147
enabled passing cases, including Char, Bool, and tuple group keys. A direct
same-source dictionary ordering probe also passes:

```dark
Stdlib.List.sort([Dict { b = 2L }, Dict { a = 2L }, Dict { a = 1L }])
```

It produces `[Dict { a = 1 }, Dict { a = 2 }, Dict { b = 2 }]`.

## Public behavior matrix

| Area | Compiler behavior | Revalidated source/test evidence | Classification |
| --- | --- | --- | --- |
| `empty` | bare polymorphic `Stdlib.List.empty`, version-zero identity | C `Stdlib.fs:220-221`, `2_AST_to_ANF.fs:4802,8972`, probe `list_parity.e2e:1-2`; I `list.dark:4-5`, test `list.dark:1` | parity |
| `length` | returns `Int` | C `List.dark:16-18`, probe `list_parity.e2e:3`; I `list.dark:106-108`, tests `list.dark:5-6` | parity |
| `findFirstIndex`, `indexedMap` | indices are `Int`; callbacks retain canonical currying/order | C `List.dark:321-329,353-359`, probes `list_parity.e2e:4-5`; I `list.dark:72-78,390-398`, tests `list.dark:118-124,160-163` | parity |
| `getAt` | accepts `Int`; negative, out-of-range, and non-`Int64` values return `None` | C `List.dark:225-229`, probes `list_parity.e2e:6-8`; I `list.dark:509-512`, tests `list.dark:143-153` | parity |
| `range` | inclusive `Int` bounds; descending bounds return `[]` | C `List.dark:293-299`, probes `list_parity.e2e:10-11`; I `list.dark:111-117`, tests `list.dark:188,239-241` | parity |
| `repeatUnsafe`, `repeat` | times-first `Int`; validated form returns `Result` with canonical error text | C `List.dark:301-318`, probes `list_parity.e2e:12-14`; I `list.dark:86-103`, tests `list.dark:242-250` | parity |
| `take`, `drop` | `Int` counts and canonical non-positive/oversized behavior | C `List.dark:245-263`, probes `list_parity.e2e:16-19`; I `list.dark:333-340,360-368`, tests `list.dark:39-51,292-299` | parity |
| `ChunkBySizeError`, `chunkBySize`, helper | structured error, ordered chunks, short final chunk | C `List.dark:10,491-502`, probes `list_parity.e2e:31-35`; I `list.dark:568-600`, tests `list.dark:368-374` | parity |
| `map2shortestHelper` | list/list/function/result order; stops at the shorter list | C `List.dark:398-410`, probes `list_parity.e2e:21-22`; I `list.dark:401-428`, tests `list.dark:199-206` | parity |
| `groupByWithKey` | structural key equality with first-key and element order | C `List.dark:434-472`, probes `list_parity.e2e:23-24`; I `list.dark:522-541`, tests `list.dark:339-361` | parity |
| `iter` | ordered, exactly-once Unit sequencing | C `List.dark:474-479`, failing-callback probes `list_parity.e2e:26-27`; I `list.dark:559-565`, tests `list.dark:61-84` | parity |
| `randomElement` | no draw for empty; unbiased bounded member selection otherwise | C `List.dark:481-489`, probes `list_parity.e2e:28-29`; I `list.dark:515-519`, tests `list.dark:235-238` | parity |
| `sort`, `sortBy`, `unique`, `uniqueBy` | canonical type-directed ordering and canonical retained-value behavior | C `List.dark:139-188`, `1.5_TypeChecking.fs:1255-1324,5583-6012`, probes `list_parity.e2e:37-45`; I `list.dark:161-204`, runtime `List.fs:13-171,304-348`, tests `list.dark:256-261,307-318` | parity |
| comparator helpers and `sortByComparator` | `Int` comparator, alternating-split merge sort, left-before-right equality, only `-1`, `0`, `1` accepted, exact error text | C `ListSortByComparatorHelpers.dark:7-85`, probes `list_parity.e2e:47-55`; I `list.dark:207-281`, tests `list.dark:263-280` | parity |

## Extensions and intentional AOT divergences

- `equals`, `flatMap`, `setAt`, and `forAll` remain compiler extensions
  (`List.dark:20-37,83-85,231-233,265-273`; focused probes
  `list_parity.e2e:57-60`). They are absent from the pinned interpreter module;
  `all` remains its parity spelling. These extensions are not used to redefine
  interpreter contracts.
- `toDisplayString_*`, skew-list accessors, canonical-comparison helpers, and
  ownership-rooting helpers are compiler display or implementation details,
  not additional interpreter API claims.
- Invalid comparator return types or arity are rejected at AOT type checking.
  The interpreter reaches runtime errors for those dynamically typed calls.
- `empty` is a deliberately narrow module-value intrinsic lowered directly to
  the empty skew-list representation. This does not add general source-level
  top-level constants; an otherwise unconstrained bare `empty` remains
  uninferrable in the AOT compiler.
- Sortability is decided from the concrete monomorphized type. Unsupported
  representations fail compilation instead of introducing interpreter-style
  tagged runtime dispatch.
