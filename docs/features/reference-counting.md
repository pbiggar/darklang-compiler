# Reference Counting

Dark uses reference counting for compiler-managed heap values. The
implementation is still being unified around representation shapes, but the old
"only tuples and records are managed" model is no longer accurate.

For the current remaining work list, see
[`memory-refcounting-remaining.md`](../../memory-refcounting-remaining.md).

## Runtime Shapes

The compiler currently has these managed or partially managed runtime shapes:

| Shape | Representation | Current ownership behavior |
|---|---|---|
| Fixed blocks | `[payload fields][refcount:8]` | Generic root retain/release; managed field release for many tuple, record, sum, and closure-capture shapes |
| Boxed sums | fixed block with tag/payload | Root RC; payload release for strings, Blobs, lists, dicts, closures, tuples, records, and selected nested sums |
| Tagged lists | Direct-payload skew RAL nodes allocated through raw memory | Root and iterative node RC helpers with shape-driven `RcReleasePlan` cleanup for direct generic fixed-block and boxed-sum payloads |
| Dicts | tagged HAMT root with refcounted raw HAMT nodes | Path-copy structural sharing; `RawSlotInit<T>` edge retains; recursive node/key/value release when node RC reaches zero |
| Dynamic strings | `[length:8][data][padding][refcount:8]` | Scoped RC, field retain/release, borrowed projection retain, literal sentinel skip |
| Dynamic Blob | `[length:8][data][padding][refcount:8]` | Scoped RC, constructor/transform coverage, container retains/releases, and initial parity with strings |
| Closures | `[func_ptr][captures...][refcount:8]` | Closure root RC and recursive capture release for the covered capture shapes |
| Raw pointers | raw addresses | Unmanaged; `RawFree` policy remains deferred |

Primitive scalars are immediate and do not participate in RC.

## Ownership Insertion

Pass 2.5, `src/DarkCompiler/passes/2.5_RefCountInsertion.fs`, inserts retains
and releases after ANF lowering.

Important current rules:

- calls are treated as returning owned values
- owned temporaries are released at scope exit unless returned or otherwise
  retained
- local aliases and projections are treated as borrowed
- returned borrowed managed values, including container fields and covered sum
  payload projections, are retained before cleanup
- borrowed tuple projections from owned local parents are retained before
  self-recursive or self-tail calls when the parent will be cleaned up,
  including alias chains introduced by pattern lowering
- closure-producing `Stdlib.List.__mapHelper` specializations treat the source
  and accumulator parameters as owned helper state; callers retain the borrowed
  source before entering those helpers, helper recursion releases replaced
  owned roots before self tail calls, typed raw-slot initialization retains closure payloads
  written into list leaves, and callers release immediate closure-call results
  after the retaining store
- RC operations are side-effecting and are preserved by optimization passes
- cleanup is preserved before tail calls

The pass now routes its ownership-sensitive decisions through `RcShape` helpers:
automatic binding release, borrowed-return retain, alias root preservation,
ownership-transfer root detection, and retain/release operation selection all
use representation shape rather than ad hoc source-type predicates. Local
expression predicates still decide whether a binding is owned or borrowed.

## Backend Lowering

ARM64 has the most complete memory support today:

- fixed-block root RC
- dynamic string/Blob RC
- tagged-list root and recursive node helpers
- list leaf payload helpers for direct roots and planned generic payloads
- dict root helpers
- closure root helpers
- recursive field release for selected fixed-block, sum, and closure-capture
  shapes, including primitive-only nested fixed-block child roots and returned
  closures with record string/Blob/list/dict-list captures

x64 has active root RC support and focused unit coverage for:

- fixed-block refcount initialization
- fixed-block allocation leak accounting
- generic fixed-block retain/release
- tagged-list root and recursive node release
- dict root helpers
- dynamic string decrement after `StringConcat`
- literal string sentinel behavior
- recursive release for covered fixed-block, list, boxed-sum, and closure
  capture shapes

x64 no longer has generic RC disabled or an obvious root-helper gap. Its
remaining risk is narrower: focused helper coverage is broad, but the project
does not yet run a complete dual-backend E2E memory matrix, and some recursive
dict/HAMT key-family expansion and arbitrary shape-plan cases remain documented in
[`../x64-refcounting.md`](../x64-refcounting.md).

## Leak Checking

`--leak-check` reports whether leak accounting balanced by program exit. A
passing leak-check test means ownership accounting balanced for that program. It
does not always mean the memory is reusable:

- fixed-size blocks and many list/dict node sizes can be routed through free
  lists
- dynamic strings and Blobs currently balance leak accounting, while
  variable-size reuse remains a design decision
- raw pointers are not automatically reclaimed
- `String`/`Blob`/`Dict`/`List -> RawPtr` intrinsics expose borrowed raw views;
  `RawPtr -> String`/`Blob` and `RawPtr -> Dict`/`List` retag initialized raw
  allocations as managed values with normal RC ownership rules

Tests that need to prove allocator reuse should test reuse explicitly instead
of relying only on leak-check silence.

## Known Gaps

The major remaining work is:

- keep future backend helper selection on direct `RcReleasePlan` consumption
  rather than rebuilding tuple/record/sum helper matrices
- extend dict/HAMT key-release helpers if new managed hashable key families are
  added; current managed hashable keys are dynamic strings and Blobs
- keep x64 and ARM64 recursive release semantics in parity as shape-plan work
  replaces helper-family special cases
- add focused coverage for any new Blob/string runtime allocation paths
- distinguish static function references from heap closures
- document or implement the deferred raw memory policy

The detailed task breakdown lives in
[`memory-refcounting-remaining.md`](../../memory-refcounting-remaining.md).
