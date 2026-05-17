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
| Boxed sums | fixed block with tag/payload | Root RC; payload release for strings, bytes, lists, dicts, closures, tuples, records, and selected nested sums |
| Tagged lists | FingerTree nodes allocated through raw memory | Root and node RC helpers; selected leaf payload helpers for dynamic buffers, fixed blocks, lists, dicts, and closures |
| Dicts | tagged HAMT root with raw HAMT nodes | Dict root RC helpers; raw HAMT lifecycle still needs a complete sharing story |
| Dynamic strings | `[length:8][data][padding][refcount:8]` | Scoped RC, field retain/release, borrowed projection retain, literal sentinel skip |
| Dynamic bytes | `[length:8][data][padding][refcount:8]` | Scoped RC, constructor/transform coverage, container retains/releases, and initial parity with strings |
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
- RC operations are side-effecting and are preserved by optimization passes
- cleanup is preserved before tail calls

The pass still uses transitional type-driven helpers in several places. The
target model is for ownership decisions to come from `RcShape` or a successor
representation plan rather than ad hoc source-type predicates.

## Backend Lowering

ARM64 has the most complete memory support today:

- fixed-block root RC
- dynamic string/bytes RC
- tagged-list root and recursive node helpers
- list leaf payload helpers for several common shapes
- dict root helpers
- closure root helpers
- recursive field release for selected fixed-block, sum, and closure-capture
  shapes

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

x64 still trails ARM64 for recursive payload release. See
[`../x64-refcounting.md`](../x64-refcounting.md).

## Leak Checking

`--leak-check` reports whether leak accounting balanced by program exit. A
passing leak-check test means ownership accounting balanced for that program. It
does not always mean the memory is reusable:

- fixed-size blocks and many list/dict node sizes can be routed through free
  lists
- dynamic strings and bytes currently balance leak accounting, while
  variable-size reuse remains a design decision
- raw pointers are not automatically reclaimed

Tests that need to prove allocator reuse should test reuse explicitly instead
of relying only on leak-check silence.

## Known Gaps

The major remaining work is:

- replace legacy heap classification with shape-driven ownership planning
- complete deeper bytes coverage and constructor/layout audits
- generalize fixed-block recursive release
- generalize tagged-list payload release without helper explosion
- define dict/HAMT structural sharing and raw-node lifecycle semantics
- bring x64 recursive release parity up to ARM64
- distinguish pure enum sums from boxed sums in ownership planning
- distinguish static function references from heap closures
- document or implement the deferred raw memory policy

The detailed task breakdown lives in
[`memory-refcounting-remaining.md`](../../memory-refcounting-remaining.md).
