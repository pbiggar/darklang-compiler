# x86_64 Reference Counting Implementation

## Status

The x64 backend has active reference-counting support for the core root
operations:

- fixed-block allocation initializes trailing refcounts
- fixed-block allocation participates in leak accounting
- generic fixed-block `RefCountInc` and `RefCountDec` are enabled
- generic tuple fixed-block `RefCountDec` releases dynamic string/Blob fields
  before freeing the enclosing block
- tagged-list root `RefCountInc` and recursive node `RefCountDec` are enabled
- tagged-list edge ownership is retained by `RawSlotInit<T>`
- dict root `RefCountInc` and `RefCountDec` helpers are enabled
- dynamic string and bytes RC lower through the dynamic-buffer path

The remaining x64 work is no longer "turn refcounting on". The backend has
broad focused coverage for root helpers, recursive fixed-block payloads,
tagged-list payloads, boxed sums, closures, and selected dict-value shapes.
Tagged-list fixed-block leaf payloads now route through planned
`RcReleasePlan` helpers instead of a static tuple/record/sum helper matrix.
The remaining risk is that there is not yet a complete dual-backend E2E memory
matrix, dict/HAMT recursive payload ownership still needs a clearer
shape-driven story, and raw-memory policy remains deferred.

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
- generic fixed-block dynamic Blob field release
- generic fixed-block nested tuple field release for dynamic string payloads
- generic fixed-block tuple string/list/dict field release
- generic fixed-block record string field release
- generic fixed-block record dict root field release
- generic fixed-block record closure root field release
- generic fixed-block record string/list/dict field release
- generic fixed-block boxed sum string payload release
- generic fixed-block boxed sum Blob payload release
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
- direct closure dynamic string/Blob capture release on closure `RefCountDec`
- direct closure list/dict/closure root capture release on closure
  `RefCountDec`
- direct closure tuple/record/sum fixed-block capture release on closure
  `RefCountDec`, including dynamic string fields
- direct closure tuple string/list/dict capture release on closure
  `RefCountDec`
- direct closure tuple string/Blob/list/dict-list capture release on closure
  `RefCountDec`
- direct closure record string/list/dict capture release on closure
  `RefCountDec`
- direct closure record string/Blob/list/dict-list capture release on closure
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
- tagged-list tuple4 string/Blob/list/dict payload release
- tagged-list tuple4 closure/bytes/list/dict payload release
- tagged-list tuple2 payload release with nested tuple dynamic string/Blob
  field combinations, list/dict fields, dict fields, closure fields,
  string/list/dict fields, and string/Blob/list/dict fields through planned
  fixed-block `RcReleasePlan` leaf helpers
- tagged-list tuple4 payload release with nested tuple dynamic-buffer fields,
  string/list/dict fields, string/list/dict-list fields, and
  closure/bytes/list/dict fields through planned fixed-block `RcReleasePlan`
  leaf helpers
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
- tagged-list three-field record string/Blob/list/dict payload release
- tagged-list three-field record closure/list/dict payload release
- tagged-list four-field record string/Blob/list/dict payload release
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
- tagged-list boxed sum tuple4 string/Blob/list/dict payload release
- tagged-list boxed sum tuple4 nested tuple string/list/dict payload release
  through a planned `RcReleasePlan` leaf helper
- tagged-list boxed sum tuple4 string/Blob/list/dict-list payload release
- tagged-list boxed sum tuple4 closure/bytes/list/dict payload release
- tagged-list boxed sum tuple4 closure/string/list/dict-list payload release
- tagged-list boxed sum record dynamic-buffer payload release, covering
  one-field records and all non-empty three-field dynamic-buffer combinations
  through planned fixed-block `RcReleasePlan` helpers
- tagged-list boxed sum record3 string/list/dict payload release through a
  planned fixed-block `RcReleasePlan` helper
- tagged-list boxed sum record4 string/Blob/list/dict payload release,
  string/Blob/list/dict-list payload release, and closure/bytes/list/dict
  payload release through planned fixed-block `RcReleasePlan` helpers
- tagged-list nested boxed sum dynamic-buffer payload release

The x64 tests run generated x64 ELF binaries directly on x64 hosts and through
`qemu-x86_64-static` on non-x64 hosts.

## Tagged Lists

`generateListRefCountDecHelper` in `passes/x64/6_CodeGen.fs` performs an
iterative DFS over skew RAL nodes using the process stack as a work stack. It
handles all three allocated list tags:

- `DIGIT`, which owns a complete tree and the remaining digit spine
- `LEAF`, which owns one direct element payload
- `NODE`, which owns one direct element payload and two child trees

The generic list helper reclaims structural nodes without element payload
cleanup. Payload-aware variants release direct payloads in both leaf and
internal nodes. Direct static payload helpers remain only for root families:

- nested list roots
- closure roots
- dict roots
- dict roots whose values are lists
- direct dynamic string/Blob buffers

Fixed-block and boxed-sum element payloads no longer have a static x64
tuple/record/sum helper matrix. When a list element is represented as
`GenericHeap`, helper selection creates a stable planned-list helper label from
the element `RcReleasePlan`, and `generateListRefCountDecHelper` delegates the
element payload cleanup to the shared generic release-plan executor. Planned list
helpers discover recursive list/dict/closure helper dependencies by walking
their `RcReleasePlan`, not by enumerating helper-label special cases.

This means new tuple, record, and boxed-sum list payload shapes should be
covered by extending `RcShape`/`RcReleasePlan` metadata and the shared generic
executor, not by adding another tuple/record/sum list helper label.
Focused x64 and ARM64 codegen tests now guard this selection rule for tuple and
record list payloads by checking that generated code uses the planned-list
helper label prefixes.

## Dicts

x64 has dict root increment and decrement helpers. These helpers operate on the
tagged HAMT root and raw HAMT nodes, but recursive key/value shape coverage is
not yet proven to the same depth as ARM64.

The next dict parity work should add architecture-targeted tests for:

- persistent sharing across update/remove
- collision release for dynamic string keys paired with recursive fixed-block
  values
- remaining record, closure, bytes, and nested dict key/value shapes not already
  covered by the current leaf and planned-helper probes

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

- dict/HAMT key and value recursive retain/release coverage beyond the current
  dynamic-buffer key/value, list-value, nested-dict-value, dict-list-value, and
  tuple-value probes. `Dict<String, (String, List<Int64>)>` is now covered for
  leaf runtime release, planned-helper selection, and collision-node runtime
  release.
- a complete dual-backend E2E memory matrix that can force ARM64 and x64
  independently of the host architecture
- helper register-preservation probes for values live across cleanup beyond the
  covered generic fixed-block dynamic-buffer, nested fixed-block, list, dict,
  and closure field `RAX` cases
- documentation and tests that distinguish leak-counter balance from allocator
  reuse
- the deferred raw-memory policy shared with list nodes, HAMT nodes, dynamic
  buffers, and user-visible raw pointers

## Recommended Next Steps

1. Keep adding x64 probes when ARM64 gains a new recursive helper family.
   `List<TFunction>` release from a `Stdlib.List.__mapHelper*` context is
   already covered and uses the closure-list helper without a stdlib-specific
   exclusion. Direct `Dict<String, Int64>` dynamic leaf key release and
   `Dict<Int64, String>` dynamic leaf value release are covered, as is the
   combined `Dict<String, String>` leaf case. The same dynamic-buffer path is
   pinned for `Dict<Blob, Blob>`, and mixed `Dict<String, List<Int64>>`
   `Dict<String, Dict<Int64, Int64>>`, and
   `Dict<String, Dict<Int64, List<Int64>>>` release are covered. Dynamic
   string keys paired with tuple leaf values containing
   `String/List<Int64>/Dict<Int64, Int64>` fields are also covered, and the
   narrower `String/List<Int64>` tuple shape now has both leaf runtime coverage
   and a planned-helper selection guard. x64 also has collision-node runtime
   coverage for that narrower shape. ARM64 has the matching helper-selection
   guard plus collision generic-payload loop coverage for that shape.
2. Keep fixed-block and boxed-sum list payload cleanup on the planned
   `RcReleasePlan` path; do not reintroduce tuple/record/sum helper matrices.
   Tuple and record payload guard tests now pin this on both backends.
3. Add x64 dict collision tests for additional typed recursive key/value
   shapes.
4. Add a real dual-backend memory matrix if the test harness grows support for
   forcing the backend independent of the host architecture.
5. Update this file after each parity or shape-plan slice lands.
