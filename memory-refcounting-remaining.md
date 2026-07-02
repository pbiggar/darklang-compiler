<!--
memory-refcounting-remaining.md - Current remaining work for Dark compiler
reference-counted memory management.

This file supersedes the old investigation and implementation-plan documents:
`memory-findings.md` and `memory-refcounting-plan.md`. Those files described
the initial state from 2026-05-16. A substantial part of that work has since
landed. This document records only the remaining work and the context future
changes need in order to avoid re-opening completed problems.
-->

# Refcounting Remaining Work

Status date: 2026-07-02.

Latest update:

- ARM64 now releases nested fixed-block and boxed-sum child roots even when the
  child root has only primitive fields. This removed the intentional guard that
  skipped primitive-only child roots during generic fixed-block cleanup. The
  regression is pinned by
  `ARM64CodeGenTests.testGenericFixedBlockNestedImmediateFieldReleasesChildRoot`.
- Borrowed tuple projections that flow into self-recursive or self-tail calls
  are now retained before the owned parent result is released, including the
  lowered pattern-match shape used by `Stdlib.Crypto.__sha1Rounds`: an owned
  call result may be aliased, projected, re-typed through one or more aliases,
  and then passed as the next recursive state. `Crypto.sha1` no longer crashes
  when primitive-only nested child roots are released, and the exact ownership
  insertion shape is pinned by borrowed-projection tests in
  `RefCountInsertionTests`.
- RC insertion now processes each function with a fresh local temp-type
  environment before merging the resulting type map, avoiding stale
  cross-function `TempId` type inference during ownership insertion.
- ARM64 generic fixed-block and boxed-sum field cleanup now routes each field
  through one recursive `RcReleasePlan` dispatcher instead of a separate
  direct-field pass plus a nested fixed-block pass. This is a step toward a
  shared shape-plan executor; list, dict, closure, dynamic-buffer, fixed-block,
  and boxed-sum field releases now enter through the same local dispatcher in
  the ARM64 generic-root path.
- ARM64 generic fixed-block child cleanup now dispatches nested boxed-sum child
  payload releases by variant tag before freeing the child root. The mixed
  no-payload/bytes-payload case is pinned so a primitive variant no longer
  risks running payload cleanup from the boxed-sum field-release summary.
- ARM64 top-level generic boxed-sum payload cleanup now uses the same
  match-and-branch-past-remaining-cases dispatch shape. Mixed sums with multiple
  managed payload variants no longer fall through from one matched payload
  cleanup into later variant cases after helper calls.
- ARM64 closure capture cleanup now also recurses through nested fixed-block
  and boxed-sum child release plans before freeing the child root. The first
  pinned case is a captured tuple containing a nested tuple with a bytes field;
  before the fix, the closure helper decremented the nested tuple root but did
  not release the bytes field inside it.
- ARM64 closure capture cleanup now dispatches boxed-sum variant payload
  release plans as well. A captured boxed sum with a bytes payload now releases
  that payload before the captured sum root is freed.
- x64 top-level generic boxed-sum cleanup now uses sum-aware
  `RcReleasePlan` variant metadata when it is present: the generated release
  code loads the active tag, runs only the matching variant's field releases,
  and branches past the remaining cases. Field-list-only boxed-sum plans still
  use their explicit payload field releases, which preserves the existing
  single-payload/generic-shape release contract. This is pinned by
  `X86_64CodeGenTests.testGenericRefCountDecMixedSumPayloadUsesVariantDispatch`.
- x64 `Dict<String, Int64>` leaf release now decrements dynamic string keys
  using a release-plan-selected dict helper variant. The new
  `X86_64CodeGenTests.testDictRefCountDecStringKey` first exposed the old
  x64 value-only helper behavior as `leaks: 1`, then passed after adding the
  dynamic-key helper path. x64 `Dict<Int64, String>` leaf release now also
  decrements direct dynamic string values via
  `X86_64CodeGenTests.testDictRefCountDecStringValue`, and
  `Dict<String, String>` now uses a combined dynamic key/value helper pinned by
  `X86_64CodeGenTests.testDictRefCountDecStringKeyValue`. The same
  dynamic-buffer helper path is pinned for `Dict<Bytes, Bytes>` by
  `X86_64CodeGenTests.testDictRefCountDecBytesKeyValue`. x64 also now covers
  mixed dynamic string keys with list leaf values via
  `X86_64CodeGenTests.testDictRefCountDecStringKeyListValue`, and mixed
  dynamic string keys with nested dict leaf values via
  `X86_64CodeGenTests.testDictRefCountDecStringKeyDictValue`. Nested
  dict-list values under dynamic string keys are covered by
  `X86_64CodeGenTests.testDictRefCountDecStringKeyDictListValue`. Broader x64
  dict key/value matrix work remains open.
- x64 now has an explicit stdlib-context closure-list release probe. The
  ARM64 bug was an architecture-local helper-selection guard that excluded
  `Stdlib.*` functions; x64 did not have that guard, and
  `X86_64CodeGenTests.testTaggedListRefCountDecClosurePayloadInStdlibFunction`
  now pins that `List<TFunction>` releases through the closure-list helper even
  from a `Stdlib.List.__mapHelper*`-named function.
- Closure lists produced by `Stdlib.List.map` are now covered by leak-check
  tests. `List<TFunction>` roots route through tagged-list release helpers
  rather than generic fixed-block release, ARM64 closure-list helper selection
  no longer excludes stdlib functions, and RC insertion models the map helper's
  closure-producing specializations explicitly: callers retain the borrowed
  source before entering those helpers, helper recursion releases replaced
  source and accumulator roots before self tail calls, and closure values
  returned directly into closure-list `pushBack` are treated as ownership
  transfers instead of receiving an immediate local decrement.
- ARM64 dict-list value release parity was extended after this document was
  first written. ARM64 now has focused symbolic coverage for direct list
  elements, nested tuple/list payloads, boxed-sum tuple payloads, and nested
  dict values that contain `Dict<Int64, List<Int64>>`. The recent commits
  ending at `cbbe933` added typed helper selection for tuple3, tuple4,
  nested tuple, boxed-sum, and nested dict-list payload shapes. Raw memory
  policy remains explicitly deferred.

Current head reviewed: includes x64 fixed-block dynamic string/bytes field
release, tuple-only nested fixed-block field release, record-registry-based
record field release, boxed sum string/bytes/list/dict/closure payload release,
dict root field release, record dict/closure root field release, zero-capture
closure field release, tagged-list closure leaf payload release, tagged-list
dict leaf payload release, and tagged-list dynamic string leaf payload release,
shape ownership helper coverage, initial RC insertion scope/retain/root-dispatch
decisions using `RcShape` where full metadata is available, list literal
fixed/tagged-root element retain decisions using `RcShape`,
and tagged-list tuple dynamic
string field release, and tagged-list one-field record dynamic string field
release, tagged-list boxed sum dynamic string payload release, tagged-list
boxed sum list payload release, tagged-list boxed sum dict payload release,
tagged-list boxed sum closure payload release, tagged-list boxed sum tuple
dynamic string payload release, tagged-list boxed sum tuple3 dynamic
string/bytes payload release for all non-empty dynamic-buffer field
combinations, tagged-list boxed sum bytes payload release, tagged-list boxed
sum list/dict payload release through generic sum payload metadata,
tagged-list boxed sum record dynamic string payload release,
tagged-list boxed sum record3 dynamic string/bytes payload release for all
non-empty dynamic-buffer field combinations, tagged-list nested boxed sum
dynamic string payload release, tagged-list three-field record
string/bytes/list/dict payload release, tagged-list boxed sum record3
string/list/dict payload release, tagged-list tuple3 string/list/dict payload
release on ARM64, tagged-list boxed sum tuple3 string/list/dict payload
release, and
direct x64 closure dynamic
string/bytes/list/dict/closure/tuple/tuple-string-list-dict/record/
record-string-list-dict/sum/sum-tuple-string-list-dict capture release,
including a
multiple-managed-capture closure probe, ARM64 string literal materialization via
sentinel literal-pool entries for `Mov`, `ArgMoves`, and `TailArgMoves`,
dynamic-string retains for returned borrowed parameters, scoped
`Float.toString` results, branch-selected literal strings, list display string
generation for int/string/bool/float lists, and the ARM64 `FloatToString`
runtime helper's aligned refcount slot, ARM64 file intrinsic literal operands
using sentinel literal-pool entries, file read success/error result string
payload leak accounting, unaligned file read string refcount layout, file write
success root accounting, file write/append error string payload leak
accounting, file delete and set-executable success/error result leak
accounting, plus multiple dynamic bytes list payloads, repeated immutable
bytes updates, dynamic bytes dict keys/values including overwrite, persistent
dict update/remove/overwrite old-root sharing with managed string values,
persistent multi-branch dict sharing from a common base with managed string
values, dict lookup `Option<String>` payload reclamation, dict string-to-string
key/value reclamation, persistent int-to-bytes dict values with old and new
roots live, dict record values with nested string fields, E2E suite-level
stdlib specialization carrying test/preamble record type registries, ARM64
fixed-block `Option` payload release for tuple/record values, ARM64 dict
leaf-value release for `List` values, dict closure value coverage,
ARM64 dict leaf-value release for nested dict values, ARM64 dict fixed-block
leaf-value release for tuple values containing string/list fields, ARM64 dict
fixed-block leaf-value release for tuple3 values containing string/list/dict
fields, isolated ARM64 dict helper-local labels so multiple typed dict release
helpers can coexist safely, dict record values with nested string/list/dict
fields covered, dynamic string keys paired with tuple3 string/list/dict values
covered, direct ARM64 tagged-list concrete non-generic sum payload release for
`Bytes`, `List`, `Dict`, and closure payload variants through LIR-carried variant
metadata, and `RcShape`
retain/release operation helper coverage and RC insertion retain/release
emission, plus ARM64 tagged-list tuple3 and record3 closure/list/dict payload
release, plus ARM64 tagged-list tuple3 string/bytes/list payload release, plus
ARM64 tagged-list tuple3 string/bytes/dict payload release, plus
ARM64 tagged-list tuple3 string/bytes/closure payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-record payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-record-dict payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-record-closure payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-list payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-dict payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-closure payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-dynamic payload release, plus
ARM64 tagged-list tuple4 and record4 string/bytes/list/dict payload release,
plus ARM64 tagged-list tuple4 and record4
closure/dynamic-buffer/list/dict payload release, plus ARM64 tagged-list
tuple4 nested tuple dynamic-buffer payload release, plus ARM64 tagged-list
tuple4 nested tuple list/dynamic-buffer payload release, plus ARM64 tagged-list
tuple4 nested tuple dict/dynamic-buffer payload release, plus ARM64 tagged-list
tuple4 nested tuple closure/dynamic-buffer payload release, plus ARM64
tagged-list tuple4 nested record list/dynamic-buffer payload release, plus
ARM64 tagged-list tuple4 nested record dict/dynamic-buffer payload release,
plus ARM64 tagged-list tuple4 nested record closure/dynamic-buffer payload
release, plus ARM64 tagged-list concrete boxed-sum tuple4 and record4
string/bytes/list/dict payload release, plus ARM64 tagged-list concrete
boxed-sum tuple4 nested tuple/record payload release, plus ARM64 tagged-list
concrete boxed-sum tuple4 nested tuple dict payload release, plus ARM64
tagged-list concrete boxed-sum tuple4 nested tuple closure payload release,
plus ARM64 tagged-list concrete boxed-sum record4 nested tuple payload release,
plus ARM64 tagged-list concrete boxed-sum record4 nested tuple dict/closure
payload release, plus x64 tagged-list record4 string/bytes/list/dict payload
release, plus x64 tagged-list tuple4 string/bytes/list/dict payload release,
plus x64 tagged-list boxed-sum tuple4 string/bytes/list/dict payload release,
plus x64 tagged-list boxed-sum record4 string/bytes/list/dict payload release,
plus x64 tagged-list boxed-sum tuple4 closure/bytes/list/dict payload release,
plus x64 tagged-list boxed-sum record4 closure/bytes/list/dict payload release,
plus x64 tagged-list tuple2 nested tuple dynamic string/bytes payload release
for all non-empty dynamic-buffer field combinations, plus x64 tagged-list
tuple2 nested tuple list/dict payload release, plus x64 tagged-list tuple2
nested tuple dict payload release, plus x64 tagged-list tuple2 nested tuple
closure payload release, plus x64 tagged-list tuple2 nested tuple
string/list/dict payload release, plus x64 tagged-list tuple2 nested tuple
string/bytes/list/dict payload release, plus x64 tagged-list tuple3
closure/list/dict payload release, plus x64 tagged-list record3
closure/list/dict payload release, plus x64 tagged-list tuple4
closure/bytes/list/dict payload release, plus x64 tagged-list record4
closure/bytes/list/dict payload release, plus x64 tagged-list tuple4
nested tuple dynamic string payload release, plus x64 tagged-list tuple4
nested tuple string/list/dict payload release, plus x64 tagged-list tuple4
nested tuple closure/bytes/list/dict payload release, plus x64 tagged-list
record4 nested tuple dynamic string payload release, plus x64 tagged-list
record4 nested tuple string/list/dict payload release, plus x64 tagged-list
record4 nested tuple closure/bytes/list/dict payload release, plus initial
`RcShape` storage-class classification used by RC insertion's legacy fixed-root
compatibility predicate, plus shared `RcReleasePlan` metadata and x64 generic
fixed-block dynamic string/bytes, dict-root, and closure-root field release
consumption through `RcReleasePlan` for tuples, records, and boxed-sum payloads,
plus boxed-sum release plans that carry source-type payload field cleanup when
variant payload metadata is available, plus x64 top-level generic boxed-sum
variant dispatch for mixed payload cleanup.

Last full-suite verification after the x64 fixed-block dynamic string/bytes
field coverage, nested fixed-block release, record string field release, boxed
sum string/bytes/list/dict/closure payload release, dict root field release,
record dict/closure root field release, and zero-capture closure field release,
including tagged-list
closure/dict/dynamic-string leaf payload release, plus direct x64 closure
dynamic string/bytes/list/dict/closure/tuple/record/sum capture release,
including multiple managed captures in the same closure, and x64 tagged-list
tuple3 and record3 dynamic-buffer payload release, plus tagged-list boxed sum
list, dict, closure, bytes, generic sum-list and sum-dict payloads, tuple
dynamic string, tuple3 dynamic string/bytes, and record dynamic string payload
release, plus
tagged-list boxed sum record3
dynamic string/bytes payload release for all non-empty dynamic-buffer field
combinations, plus tagged-list nested boxed sum dynamic string payload release,
plus tagged-list three-field record string/bytes/list/dict payload release,
plus tagged-list boxed sum record3 string/list/dict payload release, plus
tagged-list tuple3 string/list/dict payload release on ARM64, plus
tagged-list boxed sum tuple3 string/list/dict payload release, plus direct x64 closure tuple
string/list/dict capture release, plus direct x64 closure record
string/list/dict capture release, plus direct x64 closure sum tuple
string/list/dict capture release, plus direct x64 closure sum record
string/list/dict capture release, plus x64 generic fixed-block dynamic-buffer,
nested fixed-block, list, dict, and closure field release preserving a live
`RAX` value across cleanup,
plus x64 materialized string literals using the immutable refcount sentinel,
plus ARM64 materialized string literals using the immutable refcount sentinel,
plus x64 generic fixed-block boxed sum tuple and record string/list/dict
payload release, plus x64 generic fixed-block tuple and record
string/list/dict field release, plus returned borrowed sum string payload
projection retention, plus ARM64 returned borrowed deep nested tuple string
and record string projection cleanup, plus returned branch-selected borrowed
string projection
retention, plus returned borrowed sum bytes, dict, and closure payload projection
retention, plus returned borrowed sum list, tuple, and record payload
projection retention, plus returned borrowed nested sum payload projection
retention, plus returned branch-selected borrowed bytes, list, dict, closure,
tuple, and record projection retention, plus sum record payload release, plus mixed sum
no-payload and payload variant release, plus record-contained sum payload
release, plus dict-contained sum value payload release, plus pure enum sum
no-heap-ownership coverage, plus scoped `Float.toString`, branch-selected
literal string, list display string reclamation, multiple dynamic bytes list
payloads, repeated immutable bytes update reclamation, dynamic bytes dict
key/value reclamation including overwrite, persistent dict
update/remove/overwrite sharing with old roots still live, persistent
multi-branch dict sharing from a common base, dict lookup `Option<String>`
payload reclamation, dict string-to-string key/value reclamation, tagged-list
boxed sum bytes payload release, tagged-list boxed sum list/dict payload
release, tagged-list tuple3 string/list/dict payload release on ARM64,
persistent
dict int-to-bytes values with old and new roots live, dict record values with
nested string fields, dict list values with leaf payload release, dict closure
value reclamation, nested dict value leaf payload release, dict tuple value
nested string/list field reclamation, dict tuple3 value nested string/list/dict
field reclamation, dict record value nested string/list/dict field coverage,
dynamic string key plus tuple3 string/list/dict value coverage, direct
tagged-list closure payload capture release for a dynamic string capture, and `RcShape`
retain/release operation helper tests plus RC insertion use of those helpers,
plus direct concrete non-generic sum payload release in tagged lists for
`Bytes`, `List`, `Dict`, and closure variants through variant metadata, plus
tagged-list tuple bytes/list/dict/closure field release, tagged-list tuple3
bytes/list/dict field release, ARM64 returned `Option<String>`/`Option<Bytes>`
payload release for values projected through `Dict.keys` and `List.head`, plus file read
success/error, unaligned file read, file write success/error, file append
error reclamation, and ARM64 tagged-list tuple3 and record3 closure/list/dict
payload release, plus ARM64 tagged-list tuple3 string/bytes/list payload
release, plus ARM64 tagged-list tuple3 string/bytes/dict payload release, plus
ARM64 tagged-list tuple3 string/bytes/closure payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-record payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-record-dict payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-record-closure payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-list payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-dict payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-closure payload release, plus
ARM64 tagged-list tuple3 string/bytes/nested-tuple-dynamic payload release, plus
ARM64 tagged-list tuple4 and record4
string/bytes/list/dict payload release, plus ARM64 tagged-list tuple4 and
record4 closure/dynamic-buffer/list/dict payload release, plus ARM64
tagged-list tuple4 nested tuple dynamic-buffer payload release, plus ARM64
tagged-list tuple4 nested tuple list/dynamic-buffer payload release, plus
ARM64 tagged-list tuple4 nested tuple dict/dynamic-buffer payload release, plus
ARM64 tagged-list tuple4 nested tuple closure/dynamic-buffer payload release,
plus ARM64 tagged-list tuple4 nested record list/dynamic-buffer payload
release, plus ARM64 tagged-list tuple4 nested record dict/dynamic-buffer
payload release, plus ARM64 tagged-list tuple4 nested record
closure/dynamic-buffer payload release, plus ARM64 tagged-list concrete
boxed-sum tuple4 and record4 string/bytes/list/dict payload release, plus
ARM64 tagged-list concrete boxed-sum tuple4 nested tuple/record payload
release, plus ARM64 tagged-list concrete boxed-sum tuple4 nested tuple dict
payload release, plus ARM64 tagged-list concrete boxed-sum tuple4 nested tuple
closure payload release, plus ARM64 tagged-list concrete boxed-sum record4
nested tuple payload release, plus ARM64 tagged-list concrete boxed-sum record4
nested tuple dict/closure payload release, plus x64 tagged-list record4
string/bytes/list/dict payload release, plus x64 tagged-list tuple4
string/bytes/list/dict payload release, plus x64 tagged-list boxed-sum tuple4
string/bytes/list/dict payload release, plus x64 tagged-list boxed-sum record4
string/bytes/list/dict payload release, plus x64 tagged-list boxed-sum tuple4
closure/bytes/list/dict payload release, plus x64 tagged-list boxed-sum record4
closure/bytes/list/dict payload release, plus x64 tagged-list tuple2 nested
tuple dynamic string/bytes payload release for all non-empty dynamic-buffer
field combinations, plus x64 tagged-list tuple2 nested tuple string/list/dict
payload release, plus x64 tagged-list tuple2 nested tuple list/dict payload
release, plus x64 tagged-list tuple2 nested tuple dict payload release, plus
x64 tagged-list tuple2 nested tuple closure payload release, plus x64
tagged-list tuple2 nested tuple string/bytes/list/dict payload release, plus
x64 tagged-list tuple3 closure/list/dict payload release, plus x64
tagged-list record3 closure/list/dict payload release, plus x64 tagged-list
tuple4 closure/bytes/list/dict payload release, plus x64 tagged-list record4
closure/bytes/list/dict payload release, plus x64 tagged-list tuple4 nested
tuple dynamic string payload release, plus x64 tagged-list tuple4 nested tuple
string/list/dict payload release, plus x64 tagged-list tuple4 nested tuple
closure/bytes/list/dict payload release, plus x64 tagged-list record4 nested
tuple dynamic string payload release, plus x64 tagged-list record4 nested tuple
string/list/dict payload release, plus x64 tagged-list record4 nested tuple
closure/bytes/list/dict payload release, plus initial `RcShape` storage-class
classification used by RC insertion's legacy fixed-root compatibility
predicate:

- `scripts/run-in-container ./run-tests --filter=refcounting`: `205 passed`
- `scripts/run-in-container ./run-tests --filter="x64 codegen"`: `104 passed`
- Full-suite baseline: `scripts/run-in-container ./run-tests`:
  `4820 passed, 2 failed`
- The remaining failures were the known float baseline:
  - `floats.e2e:L494`
  - `floats.e2e:L495`

## Purpose

The original memory model investigation found that the compiler had several
overlapping allocation and ownership systems:

- fixed-size heap blocks with trailing refcounts for tuples, records, sums, and
  closures
- tagged FingerTree/list nodes allocated through raw memory
- tagged HAMT/dict nodes allocated through raw memory
- dynamically sized strings and bytes with length, data, padding, and trailing
  refcount
- raw pointers whose free operation is effectively unmanaged
- backend-specific implementations that had drifted apart

Since then, many of the high-priority gaps have been implemented and tested.
The remaining work is no longer "add refcounting everywhere" in the broad
sense. The remaining work is:

- finish replacing legacy type-level ownership checks with explicit runtime
  shape decisions
- finish backend parity, especially x64 recursive payload release
- finish edge coverage for bytes, strings, closures, and sum payloads
- defer the broader raw-memory policy and allocator redesign until the managed
  refcounting model is shape-driven and backend-parity work is complete
- update user-facing docs to match the implementation

## Completed Context To Preserve

This section is intentionally short. It is not a new history document; it
records the important state future work must not regress.

### Representation Metadata Exists

`src/DarkCompiler/ANF.fs` now defines:

```fsharp
type RcShape =
    | Immediate
    | FixedBlock of payloadSize:int * fieldShapes:RcShape list
    | BoxedSum of payloadSize:int
    | TaggedListShape of elementShape:RcShape
    | DictRoot of keyShape:RcShape * valueShape:RcShape
    | DynamicString
    | DynamicBytes
    | ClosureShape of captureShapes:RcShape list
    | StaticString
    | RawUnmanaged
```

`rcShapeOfType` classifies source types into this shape model. `ANF.fs` also
now exposes the first small ownership helpers and release-plan model:

- `rcShapeNeedsOwnedScopeRelease`
- `rcShapeRootKind`
- `rcShapePayloadSize`
- `rcShapeStorageClass`
- `rcShapeNeedsBorrowedRetain`
- `rcShapeIsOwnershipTransferRoot`
- `rcShapeNeedsAutomaticBindingDec`
- `rcShapeReleasePlan`
- `rcReleasePlanOfType`
- `RcReleasePlan`, `RcPayloadReleasePlan`, and `RcFieldRelease`

`2.5_RefCountInsertion.fs` uses those helpers for automatic scope release,
borrowed-retain checks, and root dispatch where full type metadata is available.
`2_AST_to_ANF.fs` also uses these helpers for list literal fixed/tagged-root
element retains, while preserving the existing dynamic string/bytes list-leaf
ownership contract: list leaves consume those freshly owned dynamic buffers
rather than retaining an additional reference. It deliberately preserves the
previous non-crashing classification for record-like names whose field metadata
is not present in the current `TypeReg`; probing a stricter
`needsAutomaticDec` path exposed both generated type-variable placeholders such
as `k$0` and monomorphized payload records such as `DictSumPayload`. Fixed root
operations still require concrete payload metadata before they are emitted.

`BoxedSumPayloadRelease` now carries field releases when `rcReleasePlanOfType`
has source-type payload metadata. Shape-only `BoxedSum` classification remains
conservative and records an empty boxed-sum payload field list.

`ANF.fs` now also exposes `RcOperation`, `RcStorageClass`,
`rcShapeStorageClass`, `rcShapeRetainOperation`, and
`rcShapeReleaseOperation`, `rcShapeNeedsBorrowedRetain`,
`rcShapeNeedsAutomaticBindingDec`, and `rcShapeIsOwnershipTransferRoot`. These
helpers separate unmanaged values, dynamic buffers, and fixed/tagged RC roots,
then combine dynamic-buffer operations with fixed-size root payload/kind
dispatch.
`2.5_RefCountInsertion.fs` uses them when emitting retain/release expressions,
when deciding whether a borrowed value must be retained before being returned
or otherwise materialized as owned, when deciding whether a normal owning
binding gets an automatic decref, and when checking whether a helper-call
parameter type transfers RC-root ownership. Its legacy fixed-root compatibility
predicate now routes through `rcShapeStorageClass`; other production paths
still consume older lower-level helper combinations directly.
`ANF.fs` also exposes `rcShapeIsRootManaged` and
`rcShapeNeedsRecursiveRelease` as early planner predicates for separating
root-managed values from dynamic buffers and for identifying shapes whose
release can require nested payload/capture/leaf traversal.

`RcShape` is still not the sole source of truth for ownership decisions. Several
active code paths still use legacy helpers such as `ANF.isHeapType`,
`payloadSize`, `rcKind`, and local type predicates.

Remaining work is therefore migration and enforcement, not initial creation.

### Dynamic Strings Are Mostly In The RC System

Recent commits added:

- string lifetime decrefs in RC insertion
- aligned string refcount layout in stdlib string builders
- literal string pool entries with `INT64_MAX` sentinel refcount slots
- string field retains when dynamic strings are stored in fixed blocks
- string field releases when fixed blocks die
- returned borrowed string retains, so a string projected from a container
  remains alive through cleanup and printing
- returned borrowed string parameter retains use the dynamic string retain path
  rather than the fixed-block path
- ARM64 `Mov`, `ArgMoves`, and `TailArgMoves` materialize literal strings as
  direct pointers to sentinel-refcount literal-pool entries
- ARM64 file intrinsics materialize literal string operands as direct pointers
  to sentinel-refcount literal-pool entries instead of temporary heap copies
- the ARM64 `FloatToString` runtime helper stores its refcount at
  `8 + aligned(length)`, matching the dynamic string layout
- ARM64 file read result roots and string payloads are accounted separately,
  including unaligned file sizes
- ARM64 file write and append result roots are accounted, and their error
  string payloads are accounted when the runtime returns an error

Covered by `src/Tests/e2e/stdlib-internal/refcounting.e2e`:

- string concat reclaimed
- string slice reclaimed, including unaligned data length
- string from codepoints reclaimed
- scoped `Stdlib.Float.toString` result reclaimed
- branch-selected literal string reclaimed
- list display string generation for int, string, bool, and float lists
- `Crypto.bytesToHex` display string generation and source bytes reclaimed
- file read success and error result strings reclaimed
- unaligned file read result strings reclaimed
- file write success result roots reclaimed
- file write and append error result strings reclaimed
- file delete success result roots and error result strings reclaimed
- file set-executable success result roots and error result strings reclaimed
- tuple string field release
- record string field release
- returned record string field release
- returned record string field use after return
- literal record string field skipped safely
- sum string payload release
- closure string capture release
- list dynamic string payload release
- dict dynamic string key/value release

Remaining string work is edge coverage and actual variable-size memory reuse,
not the basic field ownership model.

### Dynamic Bytes Have Initial RC Coverage

Recent commits added dynamic bytes reference counting and aligned bytes layout
in `src/DarkCompiler/stdlib/Bytes.dark`.

Covered by current tests:

- `Bytes.create(4)` reclaimed
- zero-length `Bytes.create(0)` reclaimed
- `Bytes.set` results reclaimed, including repeated immutable updates
- `Bytes.fromList` results reclaimed
- `Bytes.toList` releases both the result list and source bytes
- closure captures of dynamic bytes are released
- returned list releases dynamic bytes payloads
- list literals with multiple dynamic bytes payloads are released
- tuple and record dynamic bytes fields retain the byte buffer while both the
  original binding and fixed block are live
- returned record dynamic bytes fields remain usable after function cleanup
- returned borrowed bytes projections remain usable after parent cleanup
- dict roots release dynamic bytes keys and values
- dict overwrite with equal dynamic bytes keys releases replaced dynamic payloads
- dict update/remove keep old and new roots live with managed string values
- dict lookup `Option<String>` payloads are reclaimed on the dict path
- dict string-to-string lookup retains and releases both dynamic keys and values
- dict keys returned through `Dict.keys` and projected with `List.head` reclaim
  returned `Option<String>`/`Option<Bytes>` payloads
- persistent dict values of `Bytes` remain live across old and new roots
- dict record values release nested string fields after lookup; this required
  suite-level stdlib specialization to include test/preamble record type
  registries when a generic stdlib specialization mentions a user-defined
  record type
- dict values of `List<Int64>` release the leaf payload root when the dict root
  dies
- dict values of closures are reclaimed when the dict root dies
- dict values of nested dicts release the leaf payload root when the outer dict
  root dies
- dict values of `(String, List<Int64>)` release nested string/list fields when
  the tuple leaf payload root dies

Remaining bytes work is parity with strings beyond the first fixed-block cases:
deeper nested payloads, broader dict structural-sharing cases,
constructor/transform audits, and backend parity.

### Dict Roots Have Typed RC

Recent commits added:

- dict dispatch kind in RC IR
- ARM64 dict refcount helpers
- x64 dict refcount helpers
- dict root refcount insertion
- list-of-dict payload release
- dynamic string key/value release through dict helpers

Covered by current tests:

- dict singleton reclaimed
- dict overwrite reclaimed
- dict remove reclaimed
- dict fromList and input tuple list reclaimed
- dict dynamic string keys and values reclaimed
- returned dict dynamic string keys and values reclaimed
- returned list releases dict payload roots
- returned dict releases sum value payload fields
- dict string-to-string key/value lookup reclaimed
- dict int-to-bytes values stay live across old and new roots
- dict int-to-record values release nested string fields after lookup
- dict int-to-list values release leaf payload roots
- dict int-to-closure values are reclaimed
- dict string-to-dict values release leaf payload roots
- dict int-to-tuple values release nested string/list payload fields

Remaining dict work is mostly deeper raw HAMT lifecycle and shape-driven
coverage, not initial root retention/release.

### Tagged Lists Have Recursive Payload Coverage For Common Shapes

Recent commits added:

- x64 tagged list root increments/decrements
- x64 list edge retains
- nested list payload release from list leaves
- tuple payload release from list leaves
- one-field and two-field record payload release from list leaves
- dict payload release from list leaves
- closure payload release from list literals
- returned list closure payload roots
- returned list closure dynamic string captures
- returned list bytes payloads

Covered by current tests:

- primitive list literal reclaimed
- list of closures reclaimed
- returned list of closures reclaimed
- list of dynamic strings reclaimed
- returned list of dynamic bytes reclaimed
- list of tuple payloads reclaimed
- list of tuple payloads with dynamic bytes fields reclaimed
- list of tuple payloads with list fields reclaimed
- list of tuple payloads with dict fields reclaimed
- list of tuple payloads with closure fields reclaimed
- list of three-field tuple payloads with bytes, list, and dict fields
  reclaimed on ARM64
- list of three-field tuple payloads with string, list, and dict fields
  reclaimed on ARM64
- nested list payloads reclaimed
- returned list of dicts reclaimed
- returned list of single-field records reclaimed
- returned list of two-field records reclaimed
- returned list of records carrying bytes fields reclaimed
- returned list of three-field records with bytes, list, and dict fields
  reclaimed on ARM64
- returned list of three-field records with string, list, and dict fields
  reclaimed
- returned list of nested records reclaimed
- returned list of sums carrying string payloads reclaimed
- returned list of sums carrying bytes payloads reclaimed when the payload type
  is carried as a generic type argument
- returned list of sums carrying list payloads reclaimed when the payload type
  is carried as a generic type argument
- returned list of sums carrying dict payloads reclaimed when the payload type
  is carried as a generic type argument

Remaining list work is generalized payload release, broad arity coverage,
backend parity for every helper variant, and replacing ad hoc specializations
with shape-driven traversal.

During the float-display string work, a non-memory lowering bug was observed:
`List<Float>` cons patterns using `...tail` lowered the tail operation through
the `tail_i64` specialization. `Stdlib.List.toDisplayString_f64` avoids that
path by using `Stdlib.__FingerTree.head<Float>` and
`Stdlib.__FingerTree.tail<Float>` directly. Future list work should fix this
typed list-pattern lowering issue rather than copying the workaround.

### Fixed Blocks And Closures Have Important ARM64 Coverage

Recent commits added:

- fixed-block heap field reclamation coverage
- returned closure root release
- retain of fixed-block closure captures
- release of fixed-block fields inside closure captures
- release of fixed-block list fields
- release of fixed-block string fields

Covered by current tests:

- tuple root reclaimed
- large tuple root reclaimed without free-list overflow
- tuple releases dynamic string field
- record releases string and list fields
- record releases a closure field alongside string/list heap fields
- returned record releases list field
- returned tuple releases list field
- returned record releases closure field
- record releases dict field
- tuple releases closure field
- record releases nested tuple fields containing string, bytes, list, and dict
- tuple releases nested record fields
- tuple releases nested sum payload fields
- record releases sum payload field
- sum releases dynamic string payload
- sum releases list payload
- returned closure root reclaimed
- closure releases dynamic string capture
- returned closure retains fixed-block capture
- returned closure releases fixed-block capture fields
- returned closure releases nested fixed-block capture fields

Remaining fixed-block work is generalization beyond the covered ARM64 shapes,
x64 parity, deeper sum payload recursion, and removing remaining legacy
ownership shortcuts.

## Current Architecture Constraints

Future memory work should keep these constraints in mind.

### Source Types Are Not Runtime Ownership

`AST.Type` is still not enough to decide ownership:

- `TString` may be static literal pool data or an owned dynamic string.
- `TSum` may be an immediate enum value or a boxed payload value.
- `TFunction` may be a closure heap object or a compile-time function reference.
- `TDict` is a tagged root whose internal HAMT nodes are raw memory.
- `TList` is a tagged FingerTree pointer whose payload element shape matters.
- `TRawPtr` is intentionally unmanaged unless wrapped by a managed structure.

This is why `RcShape` should keep replacing source-type-only decisions.

### RC Insertion Is Still Type-Driven In Several Places

Important current path:

- `src/DarkCompiler/passes/2.5_RefCountInsertion.fs`

Key legacy functions:

- `isRcManagedHeapType`
- `needsAutomaticDec`
- `rcInfoForType`
- `retainExprForType`
- `bindingNeedsAutomaticDec`

These work for current tests, but they are still a transitional ownership
model. They mix source types with representation decisions. A shape-driven
model should decide:

- whether a value is immediate, static, dynamically owned, borrowed, raw, or
  managed through a helper
- how to retain a value
- how to release a value
- whether release is recursive or root-only
- whether a release can update leak accounting without actual reuse

### Backend Helpers Are Shape-Specific But Not Yet Unified

Important files:

- `src/DarkCompiler/passes/arm64/6_CodeGen.fs`
- `src/DarkCompiler/passes/x64/6_CodeGen.fs`

ARM64 has the most complete fixed-block field release support. It currently
has specialized paths for lists, dicts, closures, and strings. x64 has generic
RC, tagged-list helpers, and dict helpers, but its recursive payload release
coverage is narrower and not clearly documented/tested to the same depth.

### Leak-Check Passing Does Not Always Mean Memory Is Reusable

Dynamic strings and bytes can decrement refcounts and leak counters, but
zero-refcount variable-size blocks are not yet routed through a complete
variable-size reuse/free path. Raw-backed structures also use specialized
helper behavior. Passing leak-check means "the ownership and leak accounting
model balanced for this test", not necessarily "the allocator can reuse every
byte of memory produced by this program".

This distinction matters for long-running programs and benchmarks.

## Remaining Work By Area

## 1. Complete Backend `RcReleasePlan` Consumption

### Problem

The shared `RcShape` classifier is now the production source for optimizer
ownership-sensitive DCE and RC insertion. The old metadata-free
`ANF.isHeapType` helper has been removed, ANF optimization requires an explicit
record/sum metadata context, and RC insertion builds retains, releases, binding
decrefs, borrowed retains, alias preservation, and ownership-transfer checks
through `RcShape` operations.

The remaining managed-shape work is in backend helper selection and recursive
payload cleanup. Both backends consume `RcReleasePlan` in several paths, but
there are still large helper tables and helper-selection scans that encode
shape-specific cases directly.

### Why It Matters

Backend helper tables are currently correct for covered cases, but they are
still expensive to extend. A new recursive shape should be represented once in
`RcShape`/`RcReleasePlan`, then consumed by both backends. It should not require
another matrix of source-type predicates and per-architecture helper labels.

### Completed Since This File Was Written

- `ANF.isHeapType` was removed.
- ANF DCE uses `rcShapeOfTypeWithSums` through an explicit
  `ANF_Optimize.OptimizeContext`.
- Compiler and optimization test-runner calls pass real type and sum metadata
  into ANF optimization.
- RC insertion uses `rcShapeForType`/`RcShape` helpers for managed-alias
  preservation, automatic binding decrefs, borrowed retains, retain/release
  operation selection, ownership-transfer roots, and RC metadata.
- RC insertion reuses the shared `AST_to_ANF.rcSumShapeRegistryFromVariantLookup`
  helper rather than maintaining a separate copy.
- Raw-set retain decisions on ARM64 and x64 use sum-aware `RcShape`.
- x64 and ARM64 reject missing sum metadata in sum-aware shape classification.

### Remaining Tasks

1. Continue replacing backend helper selection based on helper-table pattern
   matrices with direct `RcReleasePlan` consumption where practical.
2. Keep x64 and ARM64 parity as new recursive payload-release cases are added.
3. Continue adding classifier-consumer tests as backend paths migrate. Direct
   planner tests now prove pure enums do not heap-release, boxed sums release
   through generic heap roots, closure values use closure release, strings and
   bytes use dynamic-buffer release, dict values use dict-root release, and raw
   pointers do not release.
4. Keep raw allocation policy out of this section; that remains explicitly
   deferred in section 8.

### Suggested Commit Breakdown

1. Add one backend regression test for a missing recursive payload case.
2. Replace the smallest matching helper-table predicate with a release-plan
   predicate or direct release-plan traversal.
3. Run the full suite and commit.
4. Repeat per helper family, keeping x64 and ARM64 changes separate unless the
   shared planner API itself changes.

## 2. Complete Bytes Ownership To Match Strings

### Current State

`TBytes` is now treated as needing automatic decrement in
`2.5_RefCountInsertion.fs`. The IR has:

- `RefCountIncBytes`
- `RefCountDecBytes`

ARM64 and x64 lower bytes RC through the same dynamic-buffer logic as strings.
`src/DarkCompiler/stdlib/Bytes.dark` now uses:

```dark
[length:8][data:N][padding to 8][refcount:8]
```

Current tests prove:

- a directly scoped `Bytes.create(4)` is reclaimed
- zero-length `Bytes.create(0)` is reclaimed
- `Bytes.set` results are reclaimed
- repeated immutable `Bytes.set` results are reclaimed
- `Bytes.fromList` results are reclaimed
- `Bytes.toList` releases both the result list and source bytes
- closures release captured dynamic bytes
- returned list payloads of `Bytes` are released
- list literals with multiple dynamic `Bytes` payloads are released
- tuple and record fields retain dynamic bytes while both owners are live
- returned record bytes fields remain usable and release cleanly
- returned borrowed bytes field projections remain usable after the parent is
  released
- dict roots release scoped dynamic bytes keys and values
- dict overwrite with equal dynamic bytes keys uses byte-wise hash/equality and
  releases replaced dynamic payloads
- persistent dict bytes keys stay live across old and new roots
- persistent dict bytes values stay live across old and new roots
- sum payloads containing bytes are released directly and under list payloads
- nested bytes combinations under tuples, records, lists, sums, dicts, and
  closure-containing payloads are covered by leak-check tests
- persistent dict update/remove keep old and new roots live with managed string
  values
- persistent dict overwrite keeps old and new roots live with managed string
  values
- multiple persistent dict branches derived from the same base keep the base and
  both derived roots live with managed string values
- `Dict.getOrDefault` over `String` values reclaims its lookup `Option`
  payload

### Remaining Gaps

Bytes ownership coverage is now close to string coverage for managed
refcounting behavior. The stdlib bytes layout audit found:

- `Bytes.create`, `Bytes.set`, and `Bytes.fromList` all compute
  `dataSize = ((length + 7) / 8) * 8`, allocate `16 + dataSize`, and write the
  refcount at `8 + dataSize`
- zero-length bytes allocate 16 bytes and write the refcount at offset 8
- crypto and base64 bytes-producing paths build results through
  `Bytes.fromList`, so they share the aligned layout

The remaining gap is lower-level:

- deciding whether zero-refcount bytes blocks only balance leak counters or can
  enter a reusable variable-size free path

### Risks

Dynamic-buffer code treats strings and bytes similarly, but user-visible bytes
operations are written in Dark using raw memory. This means bytes bugs can come
from either:

- RC insertion and codegen
- stdlib raw allocation layout
- raw-set edge metadata
- list/dict/fixed-block recursive release plans

### Remaining Tasks

1. Decide whether zero-refcount bytes blocks should only balance leak counters
   or also be reusable.

### Suggested Commit Breakdown

1. Defer variable-size bytes reuse to the raw/dynamic-buffer policy work.

## 3. Finish Dynamic String Edge Coverage And Reuse Semantics

### Current State

String ownership is now substantially better than in the original findings:

- ordinary dynamic strings get scoped decrefs
- dynamic strings stored in fixed blocks are retained
- fixed-block string fields are released
- returned borrowed strings are retained
- literal string pool entries have sentinel refcounts
- x64 materialized string literals now use the same sentinel behavior, including
  when stored in fixed-block fields
- ARM64 materialized string literals now use the same sentinel behavior for
  ordinary moves, argument moves, and tail-call argument moves
- string layout is aligned in stdlib builders and literal pools
- the ARM64 `FloatToString` runtime helper writes its trailing refcount at the
  aligned dynamic-string offset
- list display strings for int, string, bool, and float lists are covered by
  leak-check tests; float list display now builds the display string directly
  instead of first building a temporary `List<String>`
- `Crypto.bytesToHex` is covered as a dynamic display-string allocator over a
  bytes source

### Remaining Gaps

Still under-proven or not fully implemented:

- direct intrinsic coverage for `FloatToString`; the ARM64 helper has been
  patched, but current user-level `Stdlib.Float.toString` does not exercise
  that backend intrinsic directly
- display/toString paths beyond the covered list display and
  `Stdlib.Float.toString` cases
- success and error strings for file operations beyond the currently covered
  read success, read error, unaligned read, write success/error, and append
  error cases
- `Stdlib.File.delete` and `Stdlib.File.setExecutable` currently return `Ok`
  for nonexistent paths on the active backend, so their error string ownership
  cannot be validly probed until that semantic bug is fixed
- strings inside deeply nested fixed blocks beyond currently covered examples
- strings inside dict/list combinations beyond current string key/value and
  list payload tests
- actual variable-size block reuse for zero-refcount dynamic strings

### Runtime Paths To Audit

Important files:

- `src/DarkCompiler/Runtime.fs`
- `src/DarkCompiler/passes/arm64/6_CodeGen.fs`
- `src/DarkCompiler/passes/x64/6_CodeGen.fs`
- `src/DarkCompiler/stdlib/String.dark`

Known string-producing operations include:

- `StringConcat`
- `Stdlib.String.slice`
- `Stdlib.String.fromCodepoints`
- `FloatToString` backend intrinsic path
- remaining file operation success/error strings not yet covered by leak tests
  outside the currently covered read/write/append/delete/set-executable paths
- display strings for records, sums, bytes, and display paths not covered by
  the current list-display tests
- runtime error message strings

### Remaining Tasks

1. Add direct leak-check coverage for the backend `FloatToString` intrinsic if
   or when it is exposed through source-level code generation.
2. Add leak-check tests for remaining display paths that allocate strings,
   especially records, sums, bytes, and nested display values.
3. Add leak-check tests for any future file-operation string result payloads;
   read, write, append, delete, and set-executable paths are now covered.
4. Audit remaining backend runtime helpers so every heap string initializes
   refcount at `8 + aligned(length)`.
5. Separate "leak counter balanced" from "memory is reusable" in docs/tests.
6. Design variable-size reuse for strings, or explicitly document that raw
   bump allocations for dynamic strings are not yet reused.

### Suggested Commit Breakdown

1. Cover direct backend-intrinsic `FloatToString` dynamic string reclamation.
2. Cover remaining display-generated dynamic strings.
3. Cover remaining file-operation success/error strings after delete and
   setExecutable report missing-path errors correctly.
4. Continue auditing file-operation runtime string refcount alignment.
5. Audit and patch any unaligned runtime string builders.
6. Add a dedicated design note for variable-size dynamic-buffer reuse.

## 4. Generalize Fixed-Block Recursive Release

### Current State

ARM64 fixed-block release can release several field shapes:

- lists
- dicts
- closures
- strings
- fixed-block child fields in closure captures

Current tests cover tuple, record, sum, closure capture, nested fixed-block
capture, returned record/tuple, and several list/dict/string combinations.
ARM64 generic fixed-block cleanup now consumes release plans for nested
fixed-block and boxed-sum child roots even when those child roots have no
managed fields of their own, and it now dispatches nested boxed-sum child
payload cleanup by active variant tag for mixed sums. ARM64 top-level generic
boxed-sum payload cleanup also branches past remaining cases after a match.
ARM64 closure capture cleanup now consumes nested fixed-block child release
plans before freeing captured child roots, and it dispatches captured boxed-sum
variant payload release plans.

### Remaining Gaps

The implementation is still specialized and partial:

- bytes fields are still not covered to the same level as strings across every
  backend path, though ARM64 generic fixed-block and closure nested fixed-block
  cleanup now have explicit bytes-field coverage
- dict fields in arbitrary fixed blocks need explicit tests
- closure fields in arbitrary fixed blocks need broader tests
- nested fixed blocks outside closure captures need broader matrix tests, but
  the primitive-only child-root case is covered and implemented
- sum payload recursive release is not generalized across all backend paths,
  though ARM64 generic fixed-block child cleanup and top-level generic boxed-sum
  cleanup, plus x64 top-level generic boxed-sum cleanup, now handle mixed
  boxed-sum payload dispatch
- fixed-block arities beyond one and two are sparsely tested for heap fields
- x64 fixed-block field release parity is not clearly complete

### Remaining Tasks

1. Add tests for record/tuple fields of:

   - `Bytes`
   - `Dict`
   - closure
   - nested tuple
   - nested record
   - sum with heap payload

2. Add returned-value variants for each test shape.
3. Add use-after-return variants where a borrowed child is returned and must be
   retained before cleanup.
4. Continue moving field release selection from ad hoc backend matches to
   `RcReleasePlan`; ARM64 generic fixed-block field dispatch now has a single
   recursive release-plan entry point, but list/dict helper selection and x64
   still have specialized tables.
5. Implement the same release plan on x64.

### Suggested Commit Breakdown

1. Cover bytes fields in fixed blocks.
2. Cover dict fields in fixed blocks.
3. Cover closure fields in tuples and multi-field records.
4. Cover broader nested fixed-block field matrices outside closures.
5. Cover sum payloads nested in fixed blocks.
6. Continue converting ARM64 fixed-block release to shape plans.
7. Port shape-plan release to x64.

## 5. Generalize Tagged List Payload Release

### Current State

The list helper ecosystem has grown from root-only retain/release to multiple
payload-specialized helpers. Current tests cover primitive lists, dynamic
strings, dynamic bytes, nested lists, dicts, closures, tuple payloads, and
one/two-field records. The current ARM64 coverage also includes narrow helpers
for records shaped as `String`, `List<Int64>`, and `Dict<Int64, Int64>`, records
shaped as `List<Dict<Int64, Int64>>`, plus boxed-sum payload helpers.
LIR now carries sum variant metadata from MIR into codegen, so ARM64 direct
tagged-list release can distinguish concrete non-generic sum payloads whose
source type appears as `TSum(name, [])` at the list site. The direct concrete
sum-list coverage now includes dynamic buffers, list roots, dict roots, and
closure roots.

### Remaining Gaps

The list release path is still organized around special helper variants rather
than a general element release plan. That leaves holes:

- records with more than two fields outside the currently covered
  `String/List<Int64>/Dict<Int64, Int64>` shape
- tuples with more than two fields
- records/tuples with mixed heap fields beyond currently specialized cases
- additional sum payload shapes in lists beyond currently covered dynamic
  buffers, list roots, dict roots, and closure roots
- dict/list/closure combinations nested more than one level deep outside the
  currently covered `List<Record { List<Dict<Int64, Int64>> }>` shape
- x64 helper parity for all ARM64 helper variants
- avoiding helper explosion as more shapes are added

### Remaining Tasks

1. Define a shape-driven list leaf payload release plan.
2. Add tests for:

   - list of additional three-field records with heap fields
   - list of three-element tuples with heap fields beyond the currently covered
     string/list/dict shape
   - nested list of record/list/dict combinations

3. Replace per-shape list helper dispatch with a plan or a small set of
   composable helpers.
4. Ensure x64 and ARM64 helpers have equivalent semantics.

### Suggested Commit Breakdown

1. Add coverage for higher-arity tuple/list payloads.
2. Add coverage for additional higher-field record/list payloads.
3. Add coverage for additional sum payloads in lists.
4. Introduce a generic payload release plan.
5. Convert ARM64 list helpers to the plan.
6. Convert x64 list helpers to the plan.

## 6. Complete Dict/HAMT Lifecycle Semantics

### Current State

Dict root RC and helper paths exist. Current tests demonstrate that many common
dict programs balance leak accounting:

- singleton
- overwrite
- remove
- fromList
- string keys
- string values
- returned dicts with string keys/values
- list payloads containing dicts
- update/remove/overwrite cases where old and new roots remain live with
  managed string values
- multiple derived branches from one base root with managed string values

### Remaining Gaps

Dicts are still backed by HAMT nodes allocated through raw memory. The root is
managed, but the raw node lifecycle needs a clear correctness story:

- whether every old persistent node is released exactly once
- how structural sharing is retained
- how overwrite/remove preserve shared subtrees
- how keys and values with heap ownership are retained/released recursively
- whether raw HAMT nodes become reusable or only leak-counter balanced
- whether x64 and ARM64 dict helpers are semantically equivalent

Recent probes showed that scope-only dict leaf values can leak when their
payload shape is not one of the typed helper cases. The immediate cause is that
the generic dict decrement helper can balance HAMT raw nodes, but it is not
typed and therefore cannot recursively release arbitrary typed key/value
payloads stored in leaf or collision nodes. Narrow ARM64 helpers now handle
leaf `List` values, closure values, nested dict values, tuple values of
`(String, List<Int64>)`, tuple values of
`(String, List<Int64>, Dict<Int64, Int64>)`, and the current
`Dict<Int64, List<Int64>>` matrix used by list, nested tuple, boxed-sum, and
nested dict payload tests. Future fixes should add a serialized shape-plan path
or more complete typed dict release helpers for the remaining arbitrary leaf and
collision payload shapes. This is separate from the later raw-allocation policy
decision.

### Remaining Tasks

1. Add key/value shape matrix tests:

   - collision/leaf cases where managed keys and values both require recursive
     release

2. Audit `Stdlib.__HAMT` raw node allocations and helper-generated releases.
3. Define whether HAMT raw nodes are refcounted, copied, or uniquely owned.
4. If structural sharing is real, add node-level retain/release or prove copies
   break sharing safely.
5. Port any missing semantics to x64.

### Suggested Commit Breakdown

1. Extend dict value shape matrix tests beyond the current ARM64 dict-list
   coverage.
2. Fix any key/value recursive retain gaps.
3. Fix any key/value recursive release gaps.
4. Add x64 parity tests or architecture-specific probes.
5. Document HAMT node ownership explicitly.

## 7. Resolve x64 Backend Parity

### Current State

The old findings said x64 generic RC was disabled. That is stale. Recent x64
commits enabled:

- fixed heap allocation leak accounting
- fixed heap refcount initialization
- generic refcount increments
- generic refcount decrements
- tagged-list refcount increments
- tagged-list reclamation
- list edge retains
- dict refcount helpers
- dynamic string decref after `StringConcat`
- generic fixed-block tuple field release for dynamic strings
- generic fixed-block tuple field release for dynamic bytes
- tuple-only nested fixed-block field release
- record-registry-based fixed-block record string field release
- record-registry-based fixed-block record dict root field release
- record-registry-based fixed-block record closure root field release
- boxed sum string payload release
- boxed sum bytes payload release
- nested boxed sum string field release
- boxed sum list payload release
- boxed sum dict payload release
- boxed sum closure payload release
- dict root field release
- zero-capture closure allocation plus explicit closure `RefCountDec` leak
  accounting
- generic fixed-block zero-capture closure field release
- tagged-list closure leaf payload release
- tagged-list closure leaf payload release from a `Stdlib.List.__mapHelper*`
  x64 function context
- tagged-list dict leaf payload release
- tagged-list dynamic string leaf payload release
- tagged-list tuple dynamic string field release
- tagged-list tuple3 dynamic string/bytes field release for all non-empty
  dynamic-buffer field combinations
- tagged-list tuple3 string/list/dict payload release
- tagged-list tuple3 closure/list/dict payload release
- tagged-list tuple4 closure/bytes/list/dict payload release
- dict dynamic string leaf key release for `Dict<String, Int64>`
- dict dynamic string leaf value release for `Dict<Int64, String>`
- dict dynamic string leaf key/value release for `Dict<String, String>`
- dict dynamic bytes leaf key/value release for `Dict<Bytes, Bytes>`
- dict dynamic string leaf key plus list value release for
  `Dict<String, List<Int64>>`
- dict dynamic string leaf key plus nested dict value release for
  `Dict<String, Dict<Int64, Int64>>`
- dict dynamic string leaf key plus nested dict-list value release for
  `Dict<String, Dict<Int64, List<Int64>>>`
- tagged-list tuple4 nested tuple dynamic string payload release
- tagged-list tuple4 nested tuple string/list/dict payload release
- tagged-list tuple4 nested tuple closure/bytes/list/dict payload release
- tagged-list record4 nested tuple dynamic string payload release
- tagged-list record4 nested tuple string/list/dict payload release
- tagged-list record4 nested tuple closure/bytes/list/dict payload release
- tagged-list one-field record dynamic string field release
- tagged-list three-field record dynamic string/bytes field release for all
  non-empty dynamic-buffer field combinations
- tagged-list three-field record string/bytes/list/dict payload release
- tagged-list three-field record closure/list/dict payload release
- tagged-list four-field record closure/bytes/list/dict payload release
- tagged-list boxed sum dynamic string payload release
- tagged-list boxed sum list payload release
- tagged-list boxed sum dict payload release
- tagged-list boxed sum closure payload release
- tagged-list boxed sum tuple2 dynamic string/bytes payload release for all
  non-empty dynamic-buffer field combinations
- tagged-list boxed sum tuple3 dynamic string/bytes payload release for all
  non-empty dynamic-buffer field combinations
- tagged-list boxed sum tuple3 string/list/dict payload release
- tagged-list boxed sum tuple4 closure/bytes/list/dict payload release
- tagged-list boxed sum tuple4 closure/string/list/dict-list payload release
- tagged-list boxed sum record4 string/bytes/list/dict-list payload release
- tagged-list boxed sum record4 closure/bytes/list/dict payload release
- tagged-list boxed sum record dynamic string payload release
- tagged-list boxed sum record3 dynamic string/bytes payload release for all
  non-empty dynamic-buffer field combinations
- tagged-list boxed sum record3 string/list/dict payload release
- tagged-list nested boxed sum dynamic string payload release
- direct closure dynamic string/bytes/list/dict/closure/tuple/record/sum
  capture release
- direct closure tuple string/list/dict capture release
- direct closure tuple string/bytes/list/dict-list capture release
- direct closure record string/list/dict capture release
- direct closure record string/bytes/list/dict-list capture release
- direct closure sum tuple string/list/dict capture release
- direct closure sum record string/list/dict capture release
- direct closure release with multiple managed captures
- generic fixed-block dynamic-buffer, nested fixed-block, list, dict, and
  closure field release preserves a live `RAX` value across cleanup
- generic fixed-block tuple string/list/dict field release
- generic fixed-block record string/list/dict field release
- generic fixed-block boxed sum tuple string/list/dict payload release
- generic fixed-block boxed sum record string/list/dict payload release
- generic boxed-sum mixed-payload release dispatch by active variant tag when
  sum-aware `RcReleasePlan` metadata is present

The current focused x64 suite covers the major root, fixed-block, list,
boxed-sum, closure-capture, and selected dict-list value families. It does not
prove full parity with ARM64 because the test harness still does not run every
E2E memory case through both backends, and the backend still relies on
helper-family specializations rather than a single shared shape-plan executor.
Dict/HAMT reclamation remains separately constrained by raw-node lifecycle
semantics.

### Remaining Gaps

The important question is no longer "is x64 RC off?" The question is "does x64
match ARM64 for every recursive payload shape?"

Likely gaps:

- fixed-block field release for boxed-sum payload shapes beyond the current
  top-level variant-dispatched mixed-payload case and the
  string/list/dict/closure/tuple-string/tuple3-string-list-dict/
  tuple4-string-bytes-list-dict/record-string/record3-string-list-dict/
  record4-string-bytes-list-dict/nested-sum-string cases, and
  untested record field combinations beyond string/list/dict/bytes/nested
  fixed blocks
- closure capture recursive release coverage beyond the current direct
  dynamic-buffer, managed-root, tuple-string-list-dict,
  record-string-list-dict, sum-tuple-string-list-dict,
  sum-record-string-list-dict, and fixed-block capture probes
- list payload helper variants beyond the currently covered tuple2, exhaustive
  tuple3 dynamic-buffer combinations, mixed tuple3 string/list/dict and
  closure/list/dict shapes,
  mixed tuple4 string/bytes/list/dict, string/bytes/list/dict-list,
  closure/bytes/list/dict, and closure/string/list/dict-list shapes,
  tuple4 nested tuple dynamic string, string/list/dict, and
  closure/bytes/list/dict shapes, record4 nested tuple dynamic string,
  string/list/dict, and closure/bytes/list/dict shapes, record1, exhaustive
  record3 dynamic-buffer combinations, mixed record3 string/bytes/list/dict
  and closure/list/dict shapes, mixed record4
  string/bytes/list/dict and closure/bytes/list/dict shapes, sum dynamic-buffer,
  sum-list/sum-dict,
  sum-closure, mixed sum-tuple3 string/list/dict variants, mixed sum-tuple4
  string/bytes/list/dict, string/bytes/list/dict-list,
  closure/bytes/list/dict, and closure/string/list/dict-list variants,
  sum-record3 string/list/dict variants, mixed sum-record4
  string/bytes/list/dict and closure/bytes/list/dict variants,
  list/closure/dict/string
- dict helper key/value recursion parity
- dynamic bytes literal sentinel/aligned layout parity if a separate bytes
  literal materialization path is introduced
- register preservation around helper calls and inline releases beyond the
  covered x64 generic fixed-block dynamic-buffer, nested fixed-block, list,
  dict, and closure field release cases
- free-list indexing consistency for payload sizes and raw sizes

### Remaining Tasks

1. Add or run architecture-specific leak-check probes on x64 whenever ARM64
   gains a new recursive helper family.
2. Keep `docs/x64-refcounting.md` current after every parity or shape-plan
   slice.
3. Replace backend helper-family matching with shared `RcReleasePlan`
   execution where practical.
4. Port ARM64 closure capture release semantics beyond the current direct
   dynamic-buffer, managed-root, and fixed-block probes to x64.
5. Port any missing list helper variants to x64 until the shared shape-plan
   executor replaces the need for per-family helpers.
6. Confirm dynamic bytes literal layout if bytes gains a separate literal
   materialization path. x64 materialized string literals now carry the
   immutable sentinel and skip dynamic RC.
7. Continue auditing helper register preservation using focused tests that
   return values live across cleanup. The x64 generic fixed-block
   dynamic-buffer, nested fixed-block, list, dict, and closure field release
   paths now preserve live `RAX`; broader helper/backend combinations remain.

### Suggested Commit Breakdown

1. Add x64-specific expected-pass leak probes for fixed blocks.
2. Add x64-specific expected-pass leak probes for lists.
3. Add x64-specific expected-pass leak probes for dicts.
4. Port fixed-block dict/closure releases to x64.
5. Port closure capture recursive releases beyond the current direct
   dynamic-buffer, managed-root, and fixed-block probes to x64.
6. Update x64 docs after tests pass.

## 8. Deferred: Define Raw Memory Policy

This area should stay later than the managed refcounting work above. The next
implementation slices should not attempt a broad `RawFree` or raw allocator
redesign. First finish bytes parity, fixed-block and list generalization, dict
root semantics, and x64 parity. Raw-memory policy
should only be tackled once those managed-shape rules are stable.

### Current State

Raw memory remains central:

- FingerTree/list nodes use `RawAlloc` and typed `RawSet`.
- HAMT/dict nodes use `RawAlloc` and typed `RawSet`.
- strings and bytes constructors in stdlib use `RawAlloc`.
- `RawFree` is still a no-op.
- Raw allocation has free-list reuse in backend allocation paths for supported
  size classes.
- Managed structures often reclaim raw-backed memory through specialized RC
  helper paths rather than through `RawFree`.

### Remaining Design Decision

There are two possible directions:

1. Keep raw memory unmanaged and reserve it for true low-level internal buffers.
   Managed structures must not expose raw ownership directly; every managed
   raw-backed structure needs a typed retain/release helper.

2. Make raw memory itself manually managed with real `RawFree`, explicit
   ownership rules, and tests.

The current implementation is closer to option 1. `RawFree` remains a no-op,
and the safe paths are specialized helpers.

### Remaining Tasks

These tasks are intentionally deferred.

1. Decide and document raw memory policy.
2. If option 1:

   - document that `RawPtr` is unmanaged
   - keep `RawFree` no-op or remove user-facing assumptions around it
   - require every managed raw-backed structure to have typed RC helpers
   - ensure `RawSet` with `valueType=None` cannot silently store managed values

3. If option 2:

   - implement real `RawFree`
   - define double-free and use-after-free behavior
   - add tests for manual raw allocation and freeing
   - separate manual raw memory from compiler-managed raw-backed structures

4. Decide how variable-size string/bytes memory enters reuse:

   - specialized dynamic-buffer free list
   - general raw free
   - leak-accounting only for now

### Later Commit Breakdown

1. Add a design doc section for raw memory policy.
2. Add tests documenting `RawFree` current behavior or intended behavior.
3. Add diagnostics or assertions for typed `RawSet` omissions.
4. Implement chosen policy in small, shape-specific increments.

## 9. Strengthen Borrowed Return And Projection Ownership

### Current State

RC insertion treats several expressions as borrowing:

- `IfValue`
- `TupleGet`
- `RawGet`
- `Atom(Var _)`
- `TypedAtom(Var _, _)`

Recent projection work extended returned borrowed value retention beyond
generic heap shapes. Covered projections now include strings, bytes, lists,
dict roots, closure roots, nested fixed-block tuples and records containing
dynamic strings, branch-selected string projections, and one sum payload projection
containing a dynamic string, bytes, list, dict, closure, tuple, record, or
nested sum payload.
Branch-selected borrowed returns are now covered for strings, bytes, lists,
dicts, closures, tuples, and records.
Self-recursive uses of borrowed tuple projections from an owned local parent
are also covered, including source aliases and typed alias chains before the
recursive call. Projections from parameters remain borrowed unless another
rule materializes ownership.

### Remaining Gaps

More projected shapes still need the same confidence:

- broader sum payload shapes beyond the covered string, bytes, list, dict,
  closure, tuple, record, and nested sum payload projections
- deeper nested projections through `RawGet` beyond the covered tuple and
  record projection paths
- branch-selected borrowed values beyond the covered string, bytes, list, dict,
  closure, tuple, and record projection cases
- x64 backend parity for each retained projection family

### Remaining Tasks

1. Add tests that return/use borrowed projections after parent cleanup for:

   - broader sum payload shapes
   - deeper nested tuple/record fields beyond the covered recursive tuple
     projection path
   - branch-selected borrowed values beyond the covered string projection case

2. Ensure print insertion and cleanup ordering is safe for every retained
   borrowed value.
3. Audit helper register preservation for values live across cleanup on both
   backends. The x64 generic fixed-block dynamic-buffer, nested fixed-block,
   list, dict, and closure field cleanup paths have coverage for live `RAX`;
   other helpers and live registers remain to be proved.

### Suggested Commit Breakdown

1. Add broader borrowed-return sum payload projection coverage.
2. Add deeper borrowed-return tuple/record and raw-backed projection coverage.
3. Add backend register-preservation regression tests.

## 10. Sum Type Representation And Recursive Payloads

### Current State

`RcShape` has `BoxedSum`, and the sum-aware classifier now distinguishes pure
enum sums from boxed payload sums when variant metadata is available. The
metadata-free classifier also treats bare no-argument sums as immediate and
single-argument sums as boxed payloads:

```fsharp
| AST.TSum (_, []) -> Immediate
| AST.TSum (_, [payloadType]) -> BoxedSum (16, [(8, rcShapeOfType typeReg payloadType)], [])
```

Current tests cover:

- sum releases dynamic string payload
- sum releases list payload
- sum releases bytes payload
- sum releases dict payload
- sum releases closure payload
- sum releases tuple payload containing dynamic string field
- sum releases record payload containing dynamic string field
- returned borrowed nested sum payload projection
- mixed sum releases no-payload variant
- mixed sum releases payload variant
- pure enum sum is reclaimed without heap ownership
- `RcShape` classifies pure enum sums as immediate when sum metadata is present
- record releases sum field payload
- returned dict releases sum value payload fields

### Remaining Gaps

The compiler still needs precise handling for:

- mixed sums beyond direct payload and no-payload cleanup smoke coverage
- deeper fixed-block payload recursion beyond direct tuple/record payloads
- broader list/dict/record-contained sum shapes beyond the direct covered cases
- x64 parity beyond the current top-level generic boxed-sum variant-dispatch
  coverage

### Remaining Tasks

1. Continue applying boxed-sum payload release plans in backend consumers.
2. Cover sum payload matrix:

   - string
   - bytes
   - list
   - dict
   - record
   - tuple
   - closure
   - nested sum

3. Ensure pattern matching that extracts payloads retains returned borrowed
   payloads when necessary.

### Suggested Commit Breakdown

1. Add one mixed or nested boxed-sum leak-check probe for a currently uncovered
   payload shape.
2. Generalize the corresponding boxed-sum payload release through shape plans
   in both backends.
3. Repeat by payload family, keeping x64 and ARM64 parity visible.

## 11. Closure Ownership And Function-Typed Values

### Current State

Closure roots and several capture cases are covered:

- returned closure root reclaimed
- dynamic string capture released
- fixed-block capture retained when returned
- fixed-block capture fields released
- nested fixed-block capture fields released
- returned closure uses and releases a record string/bytes/list/dict-list
  capture
- list of closures released
- returned list releases closure payload roots

### Remaining Gaps

`TFunction` is still not simply equivalent to "heap closure":

- named function references may be static function addresses
- closures have heap payloads
- function lists had regressions around generic versus tagged list dispatch
- closure payload size is resolved from function metadata in helper code
- closure captures can contain any shape, not just currently tested shapes

### Remaining Tasks

1. Represent static function references and heap closures distinctly in shape
   planning.
2. Ensure list-of-function behavior does not regress into tagged-list helper
   mismatch.
3. Port recursive closure capture release parity to x64.

### Suggested Commit Breakdown

1. Add capture matrix tests.
2. Add shape distinction for static function reference versus heap closure.
3. Convert closure retain/release to shape plans.
4. Port x64 recursive capture release.

## 12. Leak-Check And Reuse Semantics

### Current State

Leak-check is the primary observable test mechanism for memory work. It is now
strong enough to catch many ownership mistakes on the active backend.

### Remaining Gaps

Leak-check currently conflates at least two questions:

1. Did the compiler balance ownership and leak counters?
2. Was the memory actually made reusable or returned to an allocator?

For fixed blocks and many raw-size-class blocks, helpers can push blocks into
free lists. For dynamic strings/bytes and some raw-backed structures, the
answer may only be leak-counter balance.

### Remaining Tasks

1. Document the semantic meaning of leak-check:

   - ownership balanced
   - leak counter balanced
   - memory reusable

2. Add allocator reuse tests where feasible:

   - allocate/free/allocate same fixed-block size
   - allocate/free/allocate list node sizes
   - allocate/free/allocate bytes/string sizes if reuse is implemented

3. Separate "expected no leak" from "expected reuse" in tests.
4. Ensure both ARM64 and x64 leak accounting count the same allocation classes.

### Suggested Commit Breakdown

1. Update leak-check docs.
2. Add fixed-block reuse tests.
3. Add list/dict raw-node reuse tests where applicable.
4. Add dynamic-buffer reuse tests after the design is implemented.

## 13. Documentation Debt

### Current State

The old `memory-findings.md` and `memory-refcounting-plan.md` were stale. This
file replaces them.

Other docs still contain stale statements and should be updated after the next
implementation steps:

- `docs/features/reference-counting.md`
- `docs/features/strings.md`
- `docs/features/lists.md`
- `docs/features/dict-hamt.md`
- `docs/features/records.md`
- `docs/features/sum-types.md`
- `docs/features/closures.md`
- `docs/x64-refcounting.md`

Examples of stale statements likely present:

- strings are not broadly decref'd
- bytes have no automatic lifetime decref
- x64 generic RC is disabled
- dict roots are excluded from RC
- list payload recursion is not implemented

### Remaining Tasks

1. Update docs after each remaining implementation slice.
2. Keep one current memory-status document, not multiple divergent plans.
3. Link from feature docs to this remaining-work doc where appropriate.
4. After raw memory policy is decided, document it in both design and feature
   docs.

## Recommended Next Work Sequence

This sequence excludes a full raw-allocation redesign until later. Each unit is
small enough to test independently.

### Step 1: x64 Parity Audit

Goal:

- Establish which memory tests pass on x64 and close parity gaps. The
  closure-list/stdlib-helper slice is now accounted for by a focused x64
  codegen probe.

Tests first:

- run or add architecture-targeted probes equivalent to
  `stdlib-internal/refcounting.e2e`

Implementation:

- port ARM64 release plans to x64
- fix register preservation around helper calls
- update `docs/x64-refcounting.md`

Done when:

- x64 docs match code
- x64 targeted memory tests pass or are explicitly accounted for

### Step 2: Documentation Reconciliation

Goal:

- Bring the feature docs in line with the completed refcounting implementation
  and the remaining-work sequence.

Tests first:

- No runtime test is needed for documentation-only changes.

Implementation:

- Update `docs/features/reference-counting.md`.
- Update `docs/x64-refcounting.md`.
- Update focused feature docs for strings, bytes, lists, dicts, sums, records,
  and closures where they currently contradict the implementation.
- Keep raw-memory policy explicitly marked as deferred.

Done when:

- docs no longer claim that already-landed RC paths are missing
- docs point to this file for remaining work

### Later Step: Raw Memory Policy

Goal:

- Decide whether `RawFree` remains intentionally no-op or becomes real manual
  memory management.

Tests first:

- tests that document intended raw-free behavior
- tests for managed raw-backed structures after their roots die

Implementation:

- either document and enforce unmanaged `RawPtr`
- or implement real `RawFree` with a separate discipline from compiler-managed
  structures

Done when:

- raw-backed managed structures and unmanaged raw pointers have distinct,
  documented ownership rules

## Open Questions

1. Should `RcShape.StaticString` be represented in the type/IR path, or should
   static strings remain a backend/literal-pool detail with sentinel refcounts?

2. Should dynamic strings and bytes be reused through:

   - a variable-size free list,
   - raw memory free lists,
   - or leak-counter balancing only?

3. Should pure enum sums be represented as immediate in `RcShape` now, or wait
   until sum lowering exposes enough variant metadata everywhere?

4. Should x64 parity be required for every memory commit, or can ARM64 remain
   the active backend while x64 is brought up in a dedicated phase?

5. Should `RawSet(valueType=None)` be allowed to store values whose source type
   might be managed, or should that become a compiler crash/assertion in
   internal lowering?

6. How should closure shape distinguish static function references from heap
   closures after lambda lifting and closure conversion?

## Test Inventory To Add

Add these as e2e leak-check tests unless a lower-level unit test is clearly more
appropriate.

### Bytes

- `let t = (Bytes.create(4), 1) in 0`
- record with `Bytes` field
- returned record with `Bytes` field
- returned record using `Bytes.length(r.field)`
- nested bytes combinations under list or sum payloads
- persistent dict sharing cases with bytes keys/values

### Strings

- direct backend-intrinsic `FloatToString`, if it becomes source-reachable
- remaining file-operation success/error string payloads beyond covered read
  success/error, unaligned read, write success/error, and append error cases
- display string generation for record/sum/bytes and nested display values
- nested record/list/dict string combinations

### Fixed Blocks

- record with dict field
- record with bytes/list/dict/string mixed fields
- returned borrowed projection for each field kind

### Lists

- list of three-element tuples with heap fields beyond string/list/dict
- list of three-field records with heap fields beyond the currently covered
  string/bytes/list/dict shapes
- nested list of record containing list/dict

### Dicts

- keep old root and new root live after update
- keep old root and removed root live after remove
- dict key/value leaf and collision release where both sides have managed
  payloads

### x64

- architecture-targeted versions of the current refcounting suite
- helper register preservation tests where returned values remain live across
  cleanup
- fixed-block field release matrix
- list payload release matrix
- dict key/value release matrix

## Done Criteria For The Whole Memory Project

The memory model is unified enough when:

- ownership insertion uses `RcShape` or a successor representation model instead
  of source-type heap guesses
- every managed runtime shape has a documented retain and release operation
- every managed container has recursive release behavior derived from field or
  payload shapes
- strings and bytes have one layout and one dynamic-buffer ownership story
- dict/HAMT structural sharing has a documented and tested retain/release model
- raw pointers have a documented policy separate from managed data structures
- ARM64 and x64 either pass the same memory tests or have explicit, documented
  architecture-specific gaps
- leak-check docs distinguish leak-counter balance from actual allocator reuse
- no stale memory docs contradict the implementation
