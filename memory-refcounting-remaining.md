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

Status date: 2026-05-17.

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
combinations, tagged-list boxed sum record dynamic string payload release,
tagged-list boxed sum record3 dynamic string/bytes payload release for all
non-empty dynamic-buffer field combinations, tagged-list nested boxed sum
dynamic string payload release, tagged-list three-field record
string/bytes/list/dict payload release, tagged-list boxed sum record3
string/list/dict payload release, tagged-list tuple3 string/list/dict payload
release, tagged-list boxed sum tuple3 string/list/dict payload release, and
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
accounting, plus multiple dynamic bytes list payloads, repeated immutable
bytes updates, dynamic bytes dict keys/values including overwrite, and
`RcShape` retain/release operation helper coverage and RC insertion
retain/release emission.

Last full-suite verification after the x64 fixed-block dynamic string/bytes
field coverage, nested fixed-block release, record string field release, boxed
sum string/bytes/list/dict/closure payload release, dict root field release,
record dict/closure root field release, and zero-capture closure field release,
including tagged-list
closure/dict/dynamic-string leaf payload release, plus direct x64 closure
dynamic string/bytes/list/dict/closure/tuple/record/sum capture release,
including multiple managed captures in the same closure, and x64 tagged-list
tuple3 and record3 dynamic-buffer payload release, plus tagged-list boxed sum
list, dict, closure, tuple dynamic string, tuple3 dynamic string/bytes, and
record dynamic string payload release, plus tagged-list boxed sum record3
dynamic string/bytes payload release for all non-empty dynamic-buffer field
combinations, plus tagged-list nested boxed sum dynamic string payload release,
plus tagged-list three-field record string/bytes/list/dict payload release,
plus tagged-list boxed sum record3 string/list/dict payload release, plus
tagged-list tuple3 string/list/dict payload release, plus tagged-list boxed sum
tuple3 string/list/dict payload release, plus direct x64 closure tuple
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
key/value reclamation including overwrite, and `RcShape` retain/release
operation helper tests plus RC insertion use of those helpers, plus file read
success/error, unaligned file read, file write success/error, and file append
error reclamation:

- `scripts/run-in-container ./run-tests`: `4713 passed, 2 failed`
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
now exposes the first small ownership helpers:

- `rcShapeNeedsOwnedScopeRelease`
- `rcShapeRootKind`
- `rcShapePayloadSize`

`2.5_RefCountInsertion.fs` uses those helpers for automatic scope release,
borrowed-retain checks, and root dispatch where full type metadata is available.
`2_AST_to_ANF.fs` also uses these helpers for list literal fixed/tagged-root
element retains, while preserving the existing dynamic string/bytes list-leaf
ownership contract: list leaves consume those freshly owned dynamic buffers
rather than retaining an additional reference. It deliberately preserves the
previous non-crashing classification for generated record names whose field
metadata is not present in the current `TypeReg`; fixed root operations still
require concrete payload metadata before they are emitted.

`ANF.fs` now also exposes `RcOperation`, `rcShapeRetainOperation`, and
`rcShapeReleaseOperation`. These helpers combine dynamic-buffer operations with
fixed-size root payload/kind dispatch. `2.5_RefCountInsertion.fs` uses them
when emitting retain/release expressions; other production paths still consume
older lower-level helper combinations directly.

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
- file read success and error result strings reclaimed
- unaligned file read result strings reclaimed
- file write success result roots reclaimed
- file write and append error result strings reclaimed
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
- returned list bytes payloads

Covered by current tests:

- primitive list literal reclaimed
- list of closures reclaimed
- returned list of closures reclaimed
- list of dynamic strings reclaimed
- returned list of dynamic bytes reclaimed
- list of tuple payloads reclaimed
- nested list payloads reclaimed
- returned list of dicts reclaimed
- returned list of single-field records reclaimed
- returned list of two-field records reclaimed
- returned list of three-field records with string, list, and dict fields
  reclaimed
- returned list of nested records reclaimed
- returned list of sums carrying string payloads reclaimed

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
- returned record releases list field
- returned tuple releases list field
- returned record releases closure field
- record releases dict field
- tuple releases closure field
- record releases nested tuple fields containing string, bytes, list, and dict
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

## 1. Replace Legacy Heap Classification With `RcShape`

### Problem

`RcShape` exists, but production ownership decisions still use:

- `ANF.isHeapType`
- `payloadSize`
- `rcKind`
- local type checks such as `t = AST.TString || isRcManagedHeapType t`
- backend-specific pattern matching on `sourceType`

This keeps representation knowledge spread across ANF, RC insertion, list
lowering, raw-set codegen, and backend destructors.

### Why It Matters

The current tests pass because many special cases have been patched. The risk
is that the next data shape or language feature will need another local patch
instead of flowing through one consistent ownership classifier.

Concrete examples:

- `TFunction` is not generally heap-managed in `ANF.isHeapType`, but closures
  are heap objects.
- `TString` is not a fixed block, but dynamic strings need scoped decrefs and
  field retains/releases.
- `TBytes` now has dynamic RC, but container ownership is not as complete as
  strings.
- `TSum` is conservatively heap-like even though pure enums can be immediate.
- `TDict` is a managed root with raw internals, not a generic fixed block.

### Remaining Tasks

1. Extend the small ownership API over `RcShape`.

   Initial operations exist for owned scope release, root dispatch kind, root
   payload size, and retain/release operations. Remaining suggested shape
   operations:

   - `fieldReleasePlan : RcShape -> FieldReleasePlan list`
   - `containerPayloadPlan : RcShape -> PayloadReleasePlan`
   - `isRootManaged : RcShape -> bool`
   - `isRecursiveReleaseNeeded : RcShape -> bool`

2. Finish replacing `isRcManagedHeapType` and `needsAutomaticDec` in
   `2.5_RefCountInsertion.fs` with shape-operation decisions. Retain/release
   emission now uses `rcShapeRetainOperation` and `rcShapeReleaseOperation`,
   but the current adapter still has a metadata-gap fallback for generated
   record names and some classification checks still use legacy names.

3. Replace backend dispatch based on `payloadSize` and partial `sourceType`
   pattern matching with a serialized release plan.

4. Add tests that prove the classifier controls behavior:

   - pure enum values are not heap-released
   - boxed sums are heap-released
   - closure values use closure release
   - string and bytes values use dynamic-buffer release
   - dict values use dict-root release
   - raw pointers do not get automatic release

### Suggested Commit Breakdown

1. Add pure tests for `rcShapeOfType` behavior that currently lacks direct
   coverage.
2. Extend the ownership helpers around `RcShape` without changing codegen.
3. Finish converting RC insertion for strings and bytes to use the planner.
4. Finish converting RC insertion for fixed blocks and lists to use the planner.
5. Convert backend fixed-block field release selection to consume a plan.

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

### Remaining Gaps

Bytes coverage is much thinner than string coverage. Missing or under-proven
cases include:

- broader nested bytes combinations, especially bytes inside records/tuples
  nested under list or sum payloads
- persistent dict sharing cases involving bytes keys/values when old and new
  roots are both live

### Risks

Dynamic-buffer code treats strings and bytes similarly, but user-visible bytes
operations are written in Dark using raw memory. This means bytes bugs can come
from either:

- RC insertion and codegen
- stdlib raw allocation layout
- raw-set edge metadata
- list/dict/fixed-block recursive release plans

### Remaining Tasks

1. Add failing-then-passing e2e leak-check tests for the missing cases above.
2. Add equivalent x64 support if missing.
3. Audit bytes constructors and transforms for aligned refcount layout.
4. Decide whether zero-refcount bytes blocks should only balance leak counters
   or also be reusable.

### Suggested Commit Breakdown

1. Cover sum bytes payloads.
2. Cover nested bytes combinations.
3. Audit bytes constructor and transform layout.

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
  after fixing delete/setExecutable error semantics
- display strings for records, sums, bytes, and display paths not covered by
  the current list-display tests
- runtime error message strings

### Remaining Tasks

1. Add direct leak-check coverage for the backend `FloatToString` intrinsic if
   or when it is exposed through source-level code generation.
2. Add leak-check tests for remaining display paths that allocate strings,
   especially records, sums, bytes, and nested display values.
3. Add leak-check tests for remaining file-operation string result payloads
   after fixing delete/setExecutable error behavior; append error is now
   covered.
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

### Remaining Gaps

The implementation is still specialized and partial:

- bytes fields are not covered to the same level as strings
- dict fields in arbitrary fixed blocks need explicit tests
- closure fields in arbitrary fixed blocks need broader tests
- nested fixed blocks outside closure captures need broader tests
- sum payload recursive release is not generalized
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
4. Move field release selection from ad hoc backend matches to a shared release
   plan.
5. Implement the same release plan on x64.

### Suggested Commit Breakdown

1. Cover bytes fields in fixed blocks.
2. Cover dict fields in fixed blocks.
3. Cover closure fields in tuples and multi-field records.
4. Cover nested fixed-block fields outside closures.
5. Cover sum payloads nested in fixed blocks.
6. Convert ARM64 fixed-block release to shape plans.
7. Port shape-plan release to x64.

## 5. Generalize Tagged List Payload Release

### Current State

The list helper ecosystem has grown from root-only retain/release to multiple
payload-specialized helpers. Current tests cover primitive lists, dynamic
strings, dynamic bytes, nested lists, dicts, closures, tuple payloads, and
one/two-field records. The current ARM64 coverage also includes a narrow
three-field record helper for records shaped as `String`, `List<Int64>`, and
`Dict<Int64, Int64>`, plus a narrow boxed-sum string payload helper.

### Remaining Gaps

The list release path is still organized around special helper variants rather
than a general element release plan. That leaves holes:

- records with more than two fields outside the currently covered
  `String/List<Int64>/Dict<Int64, Int64>` shape
- tuples with more than two fields
- records/tuples with mixed heap fields beyond currently specialized cases
- sums in list payloads beyond the currently covered string payload shape
- bytes in list payloads beyond one returned-list case
- dict/list/closure combinations nested more than one level deep
- x64 helper parity for all ARM64 helper variants
- avoiding helper explosion as more shapes are added

### Remaining Tasks

1. Define a shape-driven list leaf payload release plan.
2. Add tests for:

   - list of additional three-field records with heap fields
   - list of three-element tuples with heap fields
   - list of sums carrying list/dict/bytes payloads
   - list of records carrying bytes
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

### Remaining Gaps

Dicts are still backed by HAMT nodes allocated through raw memory. The root is
managed, but the raw node lifecycle needs a clear correctness story:

- whether every old persistent node is released exactly once
- how structural sharing is retained
- how overwrite/remove preserve shared subtrees
- how keys and values with heap ownership are retained/released recursively
- whether raw HAMT nodes become reusable or only leak-counter balanced
- whether x64 and ARM64 dict helpers are semantically equivalent

### Remaining Tasks

1. Add targeted tests for persistent sharing:

   - create `d1`, derive `d2`, keep both alive, then use both
   - overwrite while both old and new roots remain live
   - remove while old root remains live
   - branch sharing across multiple updates

2. Add key/value shape matrix tests:

   - dict of string to string
   - dict of int to list
   - dict of int to record
   - dict of int to tuple
   - dict of int to closure
   - dict of int to bytes with old and new roots both live
   - dict of string to dict

3. Audit `Stdlib.__HAMT` raw node allocations and helper-generated releases.
4. Define whether HAMT raw nodes are refcounted, copied, or uniquely owned.
5. If structural sharing is real, add node-level retain/release or prove copies
   break sharing safely.
6. Port any missing semantics to x64.

### Suggested Commit Breakdown

1. Add persistent-sharing tests for dict update/remove.
2. Add dict value shape matrix tests for ARM64.
3. Fix any key/value recursive retain gaps.
4. Fix any key/value recursive release gaps.
5. Add x64 parity tests or architecture-specific probes.
6. Document HAMT node ownership explicitly.

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
- tagged-list dict leaf payload release
- tagged-list dynamic string leaf payload release
- tagged-list tuple dynamic string field release
- tagged-list tuple3 dynamic string/bytes field release for all non-empty
  dynamic-buffer field combinations
- tagged-list tuple3 string/list/dict payload release
- tagged-list one-field record dynamic string field release
- tagged-list three-field record dynamic string/bytes field release for all
  non-empty dynamic-buffer field combinations
- tagged-list three-field record string/bytes/list/dict payload release
- tagged-list boxed sum dynamic string payload release
- tagged-list boxed sum list payload release
- tagged-list boxed sum dict payload release
- tagged-list boxed sum closure payload release
- tagged-list boxed sum tuple2 dynamic string/bytes payload release for all
  non-empty dynamic-buffer field combinations
- tagged-list boxed sum tuple3 dynamic string/bytes payload release for all
  non-empty dynamic-buffer field combinations
- tagged-list boxed sum tuple3 string/list/dict payload release
- tagged-list boxed sum record dynamic string payload release
- tagged-list boxed sum record3 dynamic string/bytes payload release for all
  non-empty dynamic-buffer field combinations
- tagged-list boxed sum record3 string/list/dict payload release
- tagged-list nested boxed sum dynamic string payload release
- direct closure dynamic string/bytes/list/dict/closure/tuple/record/sum
  capture release
- direct closure tuple string/list/dict capture release
- direct closure record string/list/dict capture release
- direct closure sum tuple string/list/dict capture release
- direct closure sum record string/list/dict capture release
- direct closure release with multiple managed captures
- generic fixed-block dynamic-buffer, nested fixed-block, list, dict, and
  closure field release preserves a live `RAX` value across cleanup
- generic fixed-block tuple string/list/dict field release
- generic fixed-block record string/list/dict field release
- generic fixed-block boxed sum tuple string/list/dict payload release
- generic fixed-block boxed sum record string/list/dict payload release

However, x64 is still not as well covered as ARM64 in the memory tests run in
this environment, and docs still say recursive fixed-block/list payload release
and dict/HAMT reclamation are remaining work.

### Remaining Gaps

The important question is no longer "is x64 RC off?" The question is "does x64
match ARM64 for every recursive payload shape?"

Likely gaps:

- fixed-block field release for boxed sum payloads beyond the current
  string/list/dict/closure/tuple-string/tuple3-string-list-dict/
  record-string/record3-string-list-dict/nested-sum-string cases, and
  untested record field combinations beyond string/list/dict/bytes/nested
  fixed blocks
- closure capture recursive release coverage beyond the current direct
  dynamic-buffer, managed-root, tuple-string-list-dict,
  record-string-list-dict, sum-tuple-string-list-dict,
  sum-record-string-list-dict, and fixed-block capture probes
- list payload helper variants beyond the currently covered tuple2, exhaustive
  tuple3 dynamic-buffer combinations, one mixed tuple3 string/list/dict shape,
  record1, exhaustive record3
  dynamic-buffer combinations, one mixed record3 string/bytes/list/dict shape,
  sum dynamic-buffer, sum-list/sum-dict, sum-closure, one mixed
  sum-tuple3-string-list-dict shape, sum-record3-string-list-dict,
  list/closure/dict/string
- dict helper key/value recursion parity
- dynamic bytes literal sentinel/aligned layout parity if a separate bytes
  literal materialization path is introduced
- register preservation around helper calls and inline releases beyond the
  covered x64 generic fixed-block dynamic-buffer, nested fixed-block, list,
  dict, and closure field release cases
- free-list indexing consistency for payload sizes and raw sizes

### Remaining Tasks

1. Add or run architecture-specific leak-check probes on x64 for every shape
   currently covered by `stdlib-internal/refcounting.e2e`.
2. Update `docs/x64-refcounting.md` with the actual current state.
3. Port ARM64 fixed-block field release semantics to x64.
4. Port ARM64 closure capture release semantics beyond the current direct
   dynamic-buffer, managed-root, and fixed-block probes to x64.
5. Port any missing list helper variants to x64.
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
redesign. First finish bytes parity, borrowed projection ownership, fixed-block
and list generalization, dict root semantics, and x64 parity. Raw-memory policy
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

### Remaining Gaps

More projected shapes still need the same confidence:

- broader sum payload shapes beyond the covered string, bytes, list, dict,
  closure, tuple, record, and nested sum payload projections
- deeper nested projections through `RawGet` or typed aliases beyond the
  covered tuple and record projection paths
- branch-selected borrowed values beyond the covered string, bytes, list, dict,
  closure, tuple, and record projection cases
- x64 backend parity for each retained projection family

### Remaining Tasks

1. Add tests that return/use borrowed projections after parent cleanup for:

   - broader sum payload shapes
   - deeper nested tuple/record fields beyond the covered tuple projection path
   - branch-selected borrowed values beyond the covered string projection case

2. Make borrowed-return retention shape-driven.
3. Ensure print insertion and cleanup ordering is safe for every retained
   borrowed value.
4. Audit helper register preservation for values live across cleanup on both
   backends. The x64 generic fixed-block dynamic-buffer, nested fixed-block,
   list, dict, and closure field cleanup paths have coverage for live `RAX`;
   other helpers and live registers remain to be proved.

### Suggested Commit Breakdown

1. Add broader borrowed-return sum payload projection coverage.
2. Add deeper borrowed-return tuple/record projection coverage.
3. Convert return-retain logic to `RcShape`.
4. Add backend register-preservation regression tests.

## 10. Sum Type Representation And Recursive Payloads

### Current State

`RcShape` has `BoxedSum`, but `rcShapeOfType` currently treats all `TSum` as
boxed:

```fsharp
| AST.TSum _ -> BoxedSum 16
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
- record releases sum field payload
- returned dict releases sum value payload fields

### Remaining Gaps

The compiler still needs precise handling for:

- `RcShape`-level pure enum classification is still conservative even though
  direct runtime leak coverage exists for a pure no-payload enum binding
- mixed sums beyond direct payload and no-payload cleanup smoke coverage
- deeper fixed-block payload recursion beyond direct tuple/record payloads
- broader list/dict/record-contained sum shapes beyond the direct covered cases
- x64 parity

### Remaining Tasks

1. Teach `rcShapeOfType` or a companion classifier to distinguish pure enum
   sums from boxed sums using variant metadata.
2. Extend pure enum allocation/leak coverage if sum representation changes.
3. Add recursive payload release plans for boxed sums.
4. Cover sum payload matrix:

   - string
   - bytes
   - list
   - dict
   - record
   - tuple
   - closure
   - nested sum

5. Ensure pattern matching that extracts payloads retains returned borrowed
   payloads when necessary.

### Suggested Commit Breakdown

1. Classify pure enum sums as immediate in `RcShape` once variant metadata is
   available.
2. Extend pure enum leak-check coverage beyond the direct no-payload binding if
   representation changes.
3. Add boxed sum bytes/dict/closure payload coverage.
4. Generalize boxed sum payload release through shape plans.

## 11. Closure Ownership And Function-Typed Values

### Current State

Closure roots and several capture cases are covered:

- returned closure root reclaimed
- dynamic string capture released
- fixed-block capture retained when returned
- fixed-block capture fields released
- nested fixed-block capture fields released
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
2. Add closure capture tests for recursive capture payloads beyond the current
   direct dynamic-buffer, managed-root, and fixed-block probes:

   - record with multiple heap fields
   - tuple with multiple heap fields

3. Add returned closure tests that use captured values after cleanup.
4. Ensure list-of-function behavior does not regress into tagged-list helper
   mismatch.
5. Port recursive closure capture release parity to x64.

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

- Establish which memory tests pass on x64 and close parity gaps.

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
- record with closure field and other heap fields
- tuple with nested record field
- tuple with sum payload field
- record with bytes/list/dict/string mixed fields
- returned borrowed projection for each field kind

### Lists

- list of three-element tuples with heap fields
- list of three-field records with heap fields
- list of sums carrying strings/lists/dicts/bytes
- nested list of record containing list/dict
- list of closures capturing heap values

### Dicts

- keep old root and new root live after update
- keep old root and removed root live after remove
- dict string to string
- dict int to bytes with old and new roots both live
- dict int to list
- dict int to record
- dict int to tuple
- dict int to closure
- dict int to dict

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
