# x86_64 Reference Counting Implementation

## Status

The x64 backend has active reference-counting support for the core root
operations:

- fixed-block allocation initializes trailing refcounts
- fixed-block allocation participates in leak accounting
- generic fixed-block `RefCountInc` and `RefCountDec` are enabled
- generic tuple fixed-block `RefCountDec` releases dynamic string/bytes fields
  before freeing the enclosing block
- tagged-list root `RefCountInc` and recursive node `RefCountDec` are enabled
- tagged-list edge ownership is retained by typed `RawSet`
- dict root `RefCountInc` and `RefCountDec` helpers are enabled
- dynamic string and bytes RC lower through the dynamic-buffer path

The remaining x64 work is no longer "turn refcounting on". The backend has
broad focused coverage for root helpers, recursive fixed-block payloads,
tagged-list payloads, boxed sums, closures, and selected dict-value shapes.
The remaining risk is that there is not yet a complete dual-backend E2E memory
matrix, and arbitrary recursive payloads still depend on helper-family
specializations instead of one shared shape-plan executor.

## Covered By Tests

`src/Tests/compiler-passes/X86_64CodeGenTests.fs` directly covers:

- fixed-block refcount initialization
- fixed-block leak accounting on allocation
- generic fixed-block refcount increment
- generic fixed-block refcount decrement for 8-, 16-, and 24-byte payloads
- dynamic string decrement after `StringConcat`
- materialized string literal `RefCountDec` skips release through the immutable
  refcount sentinel
- generic fixed-block dynamic string field release
- generic fixed-block literal string field release skips release through the
  immutable refcount sentinel
- generic fixed-block dynamic bytes field release
- generic fixed-block nested tuple field release for dynamic string payloads
- generic fixed-block tuple string/list/dict field release
- generic fixed-block record string field release
- generic fixed-block record dict root field release
- generic fixed-block record closure root field release
- generic fixed-block record string/list/dict field release
- generic fixed-block boxed sum string payload release
- generic fixed-block boxed sum bytes payload release
- generic fixed-block nested boxed sum field release for dynamic string payloads
- generic fixed-block boxed sum list payload release
- generic fixed-block boxed sum dict payload release
- generic fixed-block boxed sum closure payload release
- generic fixed-block boxed sum tuple string/list/dict payload release
- generic fixed-block boxed sum record string/list/dict payload release
- generic boxed-sum mixed-payload cleanup dispatch by active variant tag when
  sum-aware `RcReleasePlan` metadata is present
- nested generic boxed-sum child mixed-payload cleanup dispatch by active
  variant tag when sum-aware `RcReleasePlan` metadata is present
- closure capture mixed boxed-sum cleanup dispatch by active variant tag when
  sum-aware `RcReleasePlan` metadata is present
- tagged-list boxed-sum mixed no-payload/dynamic-payload cleanup dispatch by
  active variant tag when sum-aware `RcReleasePlan` metadata is present
- generic fixed-block dict root field release
- generic fixed-block dynamic-buffer, nested fixed-block, list, dict, and
  closure field release preserving live `RAX` across cleanup
- zero-capture closure allocation plus explicit closure `RefCountDec` leak
  accounting
- generic fixed-block zero-capture closure field release
- direct closure dynamic string/bytes capture release on closure `RefCountDec`
- direct closure list/dict/closure root capture release on closure
  `RefCountDec`
- direct closure tuple/record/sum fixed-block capture release on closure
  `RefCountDec`, including dynamic string fields
- direct closure tuple string/list/dict capture release on closure
  `RefCountDec`
- direct closure tuple string/bytes/list/dict-list capture release on closure
  `RefCountDec`
- direct closure record string/list/dict capture release on closure
  `RefCountDec`
- direct closure record string/bytes/list/dict-list capture release on closure
  `RefCountDec`
- direct closure sum tuple string/list/dict capture release on closure
  `RefCountDec`
- direct closure sum record string/list/dict capture release on closure
  `RefCountDec`
- direct closure release with multiple managed captures
- tagged-list closure leaf payload release
- tagged-list dict leaf payload release
- tagged-list dynamic string leaf payload release
- tagged-list tuple2 dynamic-buffer field release
- tagged-list tuple3 dynamic-buffer field release for all non-empty dynamic
  field combinations
- tagged-list tuple3 string/list/dict payload release
- tagged-list tuple3 closure/list/dict payload release
- tagged-list tuple4 string/bytes/list/dict payload release
- tagged-list tuple4 closure/bytes/list/dict payload release
- tagged-list tuple2 payload release with nested tuple dynamic string/bytes
  field combinations
- tagged-list tuple2 payload release with nested tuple list/dict fields
- tagged-list tuple2 payload release with nested tuple dict fields
- tagged-list tuple2 payload release with nested tuple closure fields
- tagged-list tuple2 payload release with nested tuple string/list/dict fields
- tagged-list tuple2 payload release with nested tuple string/bytes/list/dict
  fields
- tagged-list tuple4 payload release with nested tuple dynamic-buffer fields
- tagged-list tuple4 payload release with nested tuple string/list/dict fields
- tagged-list tuple4 payload release with nested tuple closure/bytes/list/dict
  fields
- tagged-list tuple4 payload release with a nested record in the middle field
  containing string/list/dict fields, using a planned fixed-block
  `RcReleasePlan` leaf helper instead of another handwritten release program
- tagged-list tuple4 payload release with a nested record in the middle field
  containing string/list/dict-list fields, also using a planned fixed-block
  `RcReleasePlan` leaf helper
- tagged-list record4 payload release with nested tuple dynamic-buffer fields
- tagged-list record4 payload release with nested tuple string/list/dict fields
- tagged-list record4 payload release with nested tuple closure/bytes/list/dict
  fields
- tagged-list one-field record dynamic-buffer field release
- tagged-list three-field record dynamic-buffer field release for all non-empty
  dynamic field combinations
- tagged-list three-field record string/bytes/list/dict payload release
- tagged-list three-field record closure/list/dict payload release
- tagged-list four-field record string/bytes/list/dict payload release
- tagged-list four-field record closure/bytes/list/dict payload release
- tagged-list boxed sum dynamic-buffer payload release
- tagged-list boxed sum mixed no-payload/dynamic-payload release dispatch
- tagged-list boxed sum list payload release
- tagged-list boxed sum dict payload release
- tagged-list boxed sum closure payload release
- tagged-list boxed sum tuple2 dynamic-buffer payload release for all non-empty
  dynamic field combinations
- tagged-list boxed sum tuple3 dynamic-buffer payload release for all non-empty
  dynamic field combinations
- tagged-list boxed sum tuple3 string/list/dict payload release
- tagged-list boxed sum tuple4 string/bytes/list/dict payload release
- tagged-list boxed sum tuple4 nested tuple string/list/dict payload release
  through a planned `RcReleasePlan` leaf helper
- tagged-list boxed sum tuple4 string/bytes/list/dict-list payload release
- tagged-list boxed sum tuple4 closure/bytes/list/dict payload release
- tagged-list boxed sum tuple4 closure/string/list/dict-list payload release
- tagged-list boxed sum record dynamic-buffer payload release, covering
  one-field records and all non-empty three-field dynamic-buffer combinations
- tagged-list boxed sum record3 string/list/dict payload release
- tagged-list boxed sum record4 string/bytes/list/dict payload release
- tagged-list boxed sum record4 string/bytes/list/dict-list payload release
- tagged-list boxed sum record4 closure/bytes/list/dict payload release
- tagged-list nested boxed sum dynamic-buffer payload release

The x64 tests run generated x64 ELF binaries directly on x64 hosts and through
`qemu-x86_64-static` on non-x64 hosts.

## Tagged Lists

`generateListRefCountDecHelper` in `passes/x64/6_CodeGen.fs` performs an
iterative DFS over FingerTree nodes using the process stack as a work stack. It
handles all five list tags:

- `SINGLE`
- `DEEP`
- `NODE2`
- `NODE3`
- `LEAF`

The generic list helper reclaims list nodes. Specialized helpers currently
exist for:

- tuple2 leaf payload roots
- tuple2 leaf payload dynamic string/bytes fields when their offsets are known
- tuple3 leaf payload dynamic string/bytes field combinations
- tuple3 leaf payloads with string/list/dict fields
- tuple3 leaf payloads with closure/list/dict fields
- tuple4 leaf payloads with string/bytes/list/dict fields
- tuple4 leaf payloads with closure/bytes/list/dict fields
- tuple2 leaf payloads with nested tuple dynamic string/bytes field
  combinations
- tuple2 leaf payloads with nested tuple list/dict fields
- tuple2 leaf payloads with nested tuple dict fields
- tuple2 leaf payloads with nested tuple closure fields
- tuple2 leaf payloads with nested tuple string/list/dict fields
- tuple2 leaf payloads with nested tuple string/bytes/list/dict fields
- tuple4 leaf payloads with nested tuple dynamic-buffer fields
- tuple4 leaf payloads with nested tuple string/list/dict fields
- tuple4 leaf payloads with nested tuple closure/bytes/list/dict fields
- tuple4 leaf payloads with a nested record in the middle field containing
  string/list/dict fields through a planned `RcReleasePlan` fixed-block helper
- tuple4 leaf payloads with a nested record in the middle field containing
  string/list/dict-list fields through a planned `RcReleasePlan` fixed-block
  helper, with ARM64 helper-selection parity pinned by a matching symbolic test
- record4 leaf payloads with nested tuple dynamic-buffer fields
- record4 leaf payloads with nested tuple string/list/dict fields
- record4 leaf payloads with nested tuple closure/bytes/list/dict fields
- one-field record leaf payload roots with dynamic string/bytes fields
- three-field record leaf payload dynamic string/bytes field combinations
- three-field record leaf payloads with string/bytes/list/dict fields
- three-field record leaf payloads with closure/list/dict fields
- four-field record leaf payloads with string/bytes/list/dict fields
- four-field record leaf payloads with closure/bytes/list/dict fields
- boxed sum leaf payload roots with dynamic string/bytes payload fields
- boxed sum tuple2 and tuple3 dynamic string/bytes field combinations
- boxed sum tuple3 leaf payloads with string/list/dict fields
- boxed sum tuple4 leaf payloads with string/bytes/list/dict fields
- boxed sum tuple4 leaf payloads with nested tuple string/list/dict fields
  through a planned `RcReleasePlan` helper
- boxed sum tuple4 leaf payloads with closure/bytes/list/dict fields
- boxed sum record leaf payloads covering one-field dynamic string/bytes,
  three-field dynamic string/bytes combinations, record3 string/list/dict, and
  record4 string/bytes/list/dict and closure/bytes/list/dict shapes
- nested boxed sum dynamic string/bytes payload fields
- nested list leaf payload roots
- closure leaf payload roots
- dict leaf payload roots
- dynamic string leaf payload roots

This is still narrower than ARM64 for broader nested tuple/record payloads, but
the common dynamic-buffer, list, dict, closure, tuple3, tuple4, nested-tuple,
record3, record4, and boxed-sum list payload families now have targeted x64
probes. The tuple4 nested-record middle-field string/list/dict and
string/list/dict-list cases route through the generic `RcReleasePlan`
fixed-block executor, which is the intended replacement direction for further
helper-family specializations.

## Dicts

x64 has dict root increment and decrement helpers. These helpers operate on the
tagged HAMT root and raw HAMT nodes, but recursive key/value shape coverage is
not yet proven to the same depth as ARM64.

The next dict parity work should add architecture-targeted tests for:

- persistent sharing across update/remove
- string keys and values
- list, record, tuple, closure, bytes, and nested dict values

## Dynamic Buffers

String and bytes `RefCountInc`/`RefCountDec` use the dynamic-buffer layout:

```text
[length:8][data:N][padding to 8][refcount:8]
```

Literal strings are skipped by dynamic-buffer RC. Heap strings produced by
`StringConcat` are now covered by an x64 leak-check unit test.

Dynamic-buffer zero-refcount paths balance leak accounting. Variable-size
buffer reuse is still a broader memory-policy question, shared with ARM64.

## Remaining Parity Work

The main x64 gaps are:

- fixed-block and tagged-list field release for boxed-sum payload shapes beyond
  the current top-level, nested-child, closure-capture, and tagged-list
  no-payload/dynamic-payload variant-dispatched cases and the string, list,
  dict, closure, tuple dynamic-buffer, tuple string/list/dict, tuple4
  string/bytes/list/dict, tuple4 nested tuple string/list/dict, tuple4
  closure/bytes/list/dict, record
  dynamic-buffer, record string/list/dict, record4 string/bytes/list/dict,
  record4 closure/bytes/list/dict, sum tuple4 string/bytes/list/dict, sum
  tuple4 closure/bytes/list/dict, sum record4 string/bytes/list/dict, and sum
  record4 closure/bytes/list/dict payload paths, plus nested sum
  dynamic-buffer payload paths; closure payloads beyond direct dynamic-buffer,
  direct list/dict/closure/fixed-block captures, and the covered tuple and
  record string/bytes/list/dict-list capture shapes
- broader record field coverage beyond the current string/bytes/nested
  fixed-block/string-list-dict/string-bytes-list-dict release paths
- closure capture recursive release coverage beyond the current direct dynamic,
  root, tuple-string-list-dict, record-string-list-dict,
  mixed-sum, sum-tuple-string-list-dict, sum-record-string-list-dict, and
  fixed-block capture probes
- list helper variants for deeper nested tuple/record payloads beyond the
  covered tuple2 nested tuple dynamic-buffer, list/dict, dict, closure,
  string/list/dict, and string/bytes/list/dict shapes, covered tuple3
  closure/list/dict and string/list/dict shapes, covered tuple4 nested tuple
  dynamic-buffer, string/list/dict, and closure/bytes/list/dict shapes,
  covered tuple4 nested-record middle-field string/list/dict and
  string/list/dict-list shapes through planned `RcReleasePlan` helpers,
  covered record4 nested tuple dynamic-buffer, string/list/dict, and
  closure/bytes/list/dict shapes, covered record3
  string/bytes/list/dict and closure/list/dict shapes, covered record4
  string/bytes/list/dict and closure/bytes/list/dict shapes, broader
  multi-field records and higher-arity tuples, and arbitrary
  non-dynamic-buffer sum payloads beyond the current list/dict/closure, sum
  tuple3, sum tuple4, sum record3, sum record4, and fixed-block mixed shapes
- dict/HAMT key and value recursive retain/release coverage beyond direct
  dynamic string leaf key/value release and the current nested value-helper
  probes
- helper register preservation for values live across cleanup beyond the
  covered generic fixed-block dynamic-buffer, nested fixed-block, list, dict,
  and closure field `RAX` cases
- documentation and tests that distinguish leak-counter balance from allocator
  reuse

## Recommended Next Steps

1. Keep adding x64 probes when ARM64 gains a new recursive helper family.
   `List<TFunction>` release from a `Stdlib.List.__mapHelper*` context is
   already covered and uses the closure-list helper without a stdlib-specific
   exclusion. Direct `Dict<String, Int64>` dynamic leaf key release and
   `Dict<Int64, String>` dynamic leaf value release are covered, as is the
   combined `Dict<String, String>` leaf case. The same dynamic-buffer path is
   pinned for `Dict<Bytes, Bytes>`, and mixed `Dict<String, List<Int64>>`
   `Dict<String, Dict<Int64, Int64>>`, and
   `Dict<String, Dict<Int64, List<Int64>>>` release are covered. Dynamic
   string keys paired with tuple leaf values containing
   `String/List<Int64>/Dict<Int64, Int64>` fields are also covered.
2. Replace x64 helper-family selection with a shared shape-driven release plan
   executor instead of continuing helper explosion.
3. Add x64 dict key/value shape matrix tests for typed recursive values.
4. Add a real dual-backend memory matrix if the test harness grows support for
   forcing the backend independent of the host architecture.
5. Update this file after each parity or shape-plan slice lands.
