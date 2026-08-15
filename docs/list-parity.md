# List parity

This is the revision-stamped record for list syntax, typing, lowering, runtime
behavior, and the public `Stdlib.List` contract. Every comparison in this file
uses this evidence pair:

- compiler comparison HEAD: `14dc38f7964ad6a8fc6e383b43fb44f424c0f064`
- darklang/dark interpreter: `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`

The approved compiler evidence revision
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899`, DCB1 report commit
`8a402797ccccda0ca47b516b356ae1de4d670038`, and the other parity documents
were starting evidence only. Every finding retained below was checked again at
the comparison HEAD and interpreter revision. Performance is excluded unless
it changes observable behavior.

## Source anchors and probes

The interpreter baseline is `LibParser/Parser.fs` for list, cons, and append
syntax; `LibExecution/TypeChecker.fs` for runtime element-type merging;
`LibExecution/Interpreter.fs` for list construction and matching;
`Builtins.Pure/Libs/NoModule.fs` for structural equality;
`Builtins.Pure/Libs/List.fs` for sorting and uniqueness; and
`packages/darklang/stdlib/list.dark` for the public contract. Expected behavior
is exercised by `backend/testfiles/execution/language/collections/dlist.dark`
and `backend/testfiles/execution/stdlib/list.dark` at the stamped revision.

Compiler evidence is anchored in:

- `src/DarkCompiler/passes/1_Parser.fs` and `1_InterpreterParser.fs` for the two
  accepted source modes and their common AST normalization;
- `AST.fs`, `passes/1.5_TypeChecking.fs`, and `passes/2_AST_to_ANF.fs` for the
  canonical list form, homogeneous typing, private typed equality, native
  construction, and pattern lowering;
- `Runtime.fs`, `Stdlib.fs`, `stdlib/List.dark`, and
  `stdlib/ListSortByComparatorHelpers.dark` for representation, `List.empty`,
  random selection, and the callable contract;
- `passes/1.6_ValueRendering.fs` and private `List.__toDisplayString_*` helpers
  for typed recursive rendering; and
- `src/Tests/e2e/list_language_parity.e2e`, `list_parity.e2e`, `lists.e2e`,
  `stdlib/list.e2e`, `pattern_matching.e2e`, and
  `compiler-passes/SyntaxInteropTests.fs` for focused same-source probes.

## Language and runtime matrix

| Area | Interpreter contract and compiler result | Focused evidence | Classification |
| --- | --- | --- | --- |
| Literals | `[]` and populated literals accept comma, semicolon, or newline separators, including trailing separators, and normalize to `ListLiteral`. | Both parser implementations; parser-mode tests and `list_language_parity.e2e`. | parity |
| Removed spread | Expression and pattern forms using `...` are rejected. The expression-level `ListCons` AST case is deleted. | Parser rejection probes in both modes; no `ListCons` expression remains in `AST.fs`. | removed compiler extension |
| Cons patterns | `head :: tail` is pattern syntax, associates right, and chained heads normalize to one internal `PListCons` without reordering bindings. | Both parsers and `SyntaxInteropTests.fs`; nested/list-tuple cases in `lists.e2e`. | parity; `PListCons` is internal only |
| Append | `@` associates right at interpreter precedence and normalizes to `Stdlib.List.append`. Both operands are homogeneous lists. | Both parsers, AST-shape test, type and value probes in `list_language_parity.e2e`. | parity |
| Inference | `[]` uses contextual polymorphic inference. A nonempty literal establishes one element type; heterogeneous elements and non-list cons tails are rejected. | `1.5_TypeChecking.fs`; focused positive and compile-error probes. | parity result, intentional AOT phase difference |
| Construction | Elements and append operands evaluate once, left to right, before native structural assembly. The skew-list builder and ownership rules remain native. | `2_AST_to_ANF.fs`; first-failure probes in `list_language_parity.e2e`; skew-list/refcount suites. | retained native equivalence |
| Representation | Empty is the zero root; populated values use direct-payload skew-binary trees with persistent reference-counted edges. | `stdlib/__SkewList.dark`, `Runtime.fs`, refcount insertion, and both code generators. | intentional compiler architecture; behavior-equivalent |
| Matching | Exact patterns require exact length; cons patterns require a nonempty list; nested patterns bind in source order and tails are canonical list values. | All `PList`/`PListCons` paths in `2_AST_to_ANF.fs`; `lists.e2e` and `pattern_matching.e2e`. | parity |
| Match failure | Exhausted alternatives use the standard nonexhaustive-match failure and recursively render literal list/tuple values instead of a list-specific fallback. | `2_AST_to_ANF.fs`; exact failure probes in `pattern_matching.e2e` and `lists.e2e`. | parity text for shared representable values |
| Equality | `==`/`!=` synthesize private typed structural equality: lengths and elements are compared recursively in order. No public `List.equals` call is generated. | `1.5_TypeChecking.fs`; scalar, nested, tuple, record, enum, and list equality probes. | retained native equivalence |
| Rendering | Boundary rendering is synthesized recursively for scalar, tuple, nested-list, record, and enum element types; legacy concrete helpers have private `__` identities. | `1.6_ValueRendering.fs`, `ListDisplay.fs`, value-rendering and refcount tests. | retained native equivalence |

The interpreter discovers a heterogeneous list while merging runtime
`ValueType`s and reports the first mismatched index. The AOT compiler rejects
the same program during type checking because its native list representation
must be monomorphic before lowering. Callback arity/type errors and unsupported
concrete equality/sort categories are rejected at the same AOT boundary. These
are intentional failure-phase differences, not different accepted results.
Interpreter-only dynamic values outside the compiler type universe are not
claimed as supported compiler element categories.

## Public `List` contract

The F#-backed interpreter module defines the behavior; equivalent compiler Dark
or native implementations are retained. Type variables below are polymorphic.

| Contract group | Public signatures and behavior |
| --- | --- |
| Values and construction | `empty : List<a>` is a non-callable value; `singleton`, `push`, `pushBack`, and `append` retain order. `head`, `tail`, `last`, and `splitLast` return `Option` on empty input. |
| Size and lookup | `length : List<a> -> Int`; `getAt : List<a> * Int -> Option<a>` returns `None` for negative, out-of-range, or non-native-sized indices; `isEmpty`, `member`, and `findFirst` match interpreter edge cases. |
| Traversal | `fold`, `map`, `indexedMap`, `filter`, `filterMap`, `flatten`, `reverse`, `interpose`, `interleave`, `drop`, `dropWhile`, `dropLast`, `take`, and `takeWhile` preserve interpreter results and callback order. Counts and indices are public `Int`. |
| Predicates | `all` and `any` invoke callbacks left to right, once per visited element, and stop at the first decisive result. `findFirst` and `findFirstIndex` stop at the first match. |
| Pairwise | `map2shortest` and `zipShortest` truncate; `map2` and `zip` return `None` on unequal lengths and `Some` on equal lengths; `unzip` preserves order. |
| Range and repetition | `range(start, end)` is inclusive and descending bounds return `[]`. `repeatUnsafe(times, value)` returns a list. `repeat(times, value)` returns `Result<List<a>, String>` and uses the canonical negative-count message. Bounds are `Int`. |
| Sorting and uniqueness | `sort`, `sortBy`, `unique`, and `uniqueBy` use canonical typed ordering. `sortByComparator` is stable, accepts an `Int` comparator, and returns the exact error unless each result is `-1`, `0`, or `1`. |
| Grouping and callbacks | `groupByWithKey` preserves first-key and member order and evaluates its key callback in source order. `partition` preserves output order, calls its predicate once per element, and observes the interpreter's tail-to-head callback order. `iter` calls a Unit callback left to right and returns Unit. |
| Random | `randomElement : List<a> -> Option<a>` does not request entropy for `[]`; nonempty input uses rejection sampling and only returns a valid member. |
| Chunking | Public `ChunkBySizeError.SizeMustBeGreaterThanZero`; `chunkBySize` rejects nonpositive `Int` sizes and otherwise returns ordered chunks with a possibly short final chunk. |

Callback failures propagate from the first callback reached in the specified
order. The focused suites cover fold/map/indexed-map, pairwise map, filters,
find/all/any, drop/take-while, sorting, uniqueness, grouping, partition, and
iteration, including short-circuit and failure cases.

## Removed extensions and private implementation surface

The former public compiler aliases `List.equals`, `List.flatMap`,
`List.forAll`, and `List.setAt` are absent from callable lookup; focused probes
assert unresolved-callable failures. Compiler-owned sources use canonical
`==`, `List.flatten`/`map`, `List.all`, and private
`Stdlib.Internal.SkewList.setAt` as appropriate. The misplaced truncating
`List.zip` in `Float.dark` is removed; canonical `List.zip` returns `Option`.

The compiler-only list spread grammar and expression representation are gone.
Compiler stdlib, benchmarks, and tests use `::` patterns, `@`, `List.push`,
`List.append`, or explicitly private skew-list operations. Helpers prefixed
`__`, `Stdlib.Internal.SkewList`, generated comparison/rendering functions, and
ownership-rooting helpers are implementation details and are not additions to
the public interpreter contract. The existing `getAtOrDefault` machine-sized
helper remains a documented compiler implementation extension because removing
it was outside the approved public-helper removal set; parity callers use
`getAt`.
