<!--
memory-refcounting-plan.md - Small-step implementation plan for unifying Dark
compiler memory management around reference counting.

Each unit is intended to be independently testable and small enough to land as
one focused commit.
-->

# Refcounting Unification Plan

Status date: 2026-05-16.

This plan turns `memory-findings.md` into a sequence of small, low-risk commits.
The ordering is deliberate: first make memory behavior observable and
representation-aware, then fix one runtime shape at a time, then enable backend
reclamation consistently.

## Working Rules

- Every behavior change starts with a failing E2E leak-check test.
- Prefer one data shape per commit.
- Do not enable a reclaim path until allocation layout, refcount initialization,
  retain, release, and leak accounting are all tested for that shape.
- Keep ARM64 and x64 behavior explicit. A test may document a current x64 gap
  before enabling the fix.
- Avoid broad refactors until a shape classifier exists and has tests.
- Run `./run-tests` for each commit. For memory-heavy milestones, run targeted
  leak probes manually in addition to the full suite.

## Commit Unit Template

Each unit should have:

- Goal: the one behavior this commit improves.
- Scope: files expected to change.
- Test first: the failing test added before implementation.
- Implementation: the smallest code change to pass the test.
- Done when: objective completion criteria.

## Phase 0: Lock In Current Observability

### 0.1 Add Leak-Check Test Harness Cases For Known Gaps

Goal: record current memory gaps as failing or expected-failing tests, without
changing runtime behavior.

Scope:

- `src/Tests/e2e/`

Test first:

- Add tiny leak-check cases for string concat, bytes create, dict singleton,
  list literal, tuple discard, and closure discard.

Implementation:

- If the current harness cannot assert leak output cleanly, add the smallest
  E2E convention for leak-check expectations.

Done when:

- Tests demonstrate which shapes leak today.
- No runtime code changes.

Suggested commit:

- `Add leak-check coverage for current memory model gaps`

### 0.2 Add Backend-Specific Leak Accounting Tests

Goal: make x64 false negatives visible.

Scope:

- `src/Tests/e2e/`
- test harness only if needed

Test first:

- Add a tuple allocation leak-check test that should report one dynamic
  allocation once generic heap allocation accounting is fixed.

Implementation:

- Mark behavior as current known gap if the harness supports known failures, or
  add a diagnostic-only test group.

Done when:

- The suite records that generic x64 heap allocation is not reliably counted.

Suggested commit:

- `Document generic heap leak accounting gap in tests`

## Phase 1: Introduce Representation Metadata Without Behavior Change

### 1.1 Add `RcShape` Type

Goal: create a central representation type without using it yet.

Scope:

- likely `src/DarkCompiler/ANF.fs` or a new small ownership module

Test first:

- Add unit tests for constructing/printing/equality of the new shape type if
  local test patterns exist.

Implementation:

- Add `RcShape` with cases for immediate, fixed block, boxed sum, tagged list,
  dict root, dynamic string, dynamic bytes, closure, static string, raw
  unmanaged.

Done when:

- Code compiles.
- No compiler behavior changes.

Suggested commit:

- `Add runtime RC shape model`

### 1.2 Add A Pure Shape Classifier For Primitive And Fixed-Block Types

Goal: classify primitives, tuples, and records without changing RC insertion.

Scope:

- ownership/ANF module
- unit tests

Test first:

- Test that primitives classify as immediate.
- Test tuple and record payload sizes.

Implementation:

- Add `tryRcShape` or equivalent for primitives, tuple, and record.
- Keep existing `isHeapType` call sites unchanged.

Done when:

- New classifier is tested but unused by production lowering.

Suggested commit:

- `Classify primitive tuple and record RC shapes`

### 1.3 Add Shape Classification For Boxed Versus Immediate Sums

Goal: make pure enum versus boxed sum explicit.

Scope:

- shape classifier
- type/variant registry tests

Test first:

- Pure enum classifies as immediate.
- Sum with payload classifies as boxed sum.
- No-payload variant in a mixed sum classifies as boxed.

Implementation:

- Use variant metadata to determine whether any variant has payload.

Done when:

- Sum representation no longer has to be guessed from `TSum` alone in new code.

Suggested commit:

- `Classify immediate and boxed sum RC shapes`

### 1.4 Add Shape Classification For Strings, Bytes, Lists, Dicts, Closures

Goal: classify all remaining heap-capable source values.

Scope:

- shape classifier
- tests

Test first:

- `TString` -> dynamic-or-static string representation marker as appropriate.
- `TBytes` -> dynamic bytes.
- `TList a` -> tagged list with element shape.
- `TDict k v` -> dict root with key/value shapes.
- closure allocation metadata -> closure shape.

Implementation:

- Use conservative shapes where runtime distinction is not available yet.

Done when:

- Every source type has an explicit shape answer.

Suggested commit:

- `Classify remaining runtime RC shapes`

### 1.5 Replace `ANF.isHeapType` In One Read-Only Path

Goal: prove the shape classifier can replace legacy heap classification in a
safe, low-impact place.

Scope:

- a diagnostic or helper path only

Test first:

- Existing tests should pass.

Implementation:

- Replace a non-behavioral use of `isHeapType` or add an adapter:
  `isRcManagedShape`.

Done when:

- No runtime behavior changes.
- The migration path is clear.

Suggested commit:

- `Introduce RC shape helper adapters`

## Phase 2: Make Fixed-Block Allocation Correct On x64

### 2.1 Add Failing x64 Tuple Refcount Layout Test

Goal: expose that generic `HeapAlloc` on x64 does not initialize the trailing
refcount.

Scope:

- E2E leak-check or low-level codegen test

Test first:

- A discarded tuple should be counted as allocated and then reclaimable once
  generic dec works.

Implementation:

- Test only. Do not alter code yet.

Done when:

- The failing test captures the layout/accounting gap.

Suggested commit:

- `Add failing x64 tuple heap layout test`

### 2.2 Fix x64 `HeapAlloc` Size And Refcount Initialization

Goal: make x64 fixed heap blocks match ARM64 layout.

Scope:

- `src/DarkCompiler/passes/x64/6_CodeGen.fs`

Test first:

- Use the test from 2.1.

Implementation:

- Allocate `payloadSize + 8`, aligned.
- Store refcount `1` at `[block + payloadSize]`.
- Do not enable `RefCountDec` yet.

Done when:

- x64 generated blocks have the expected layout.
- Existing tests still pass or only known unrelated failures remain.

Suggested commit:

- `Initialize x64 fixed heap refcounts`

### 2.3 Add x64 Generic Heap Leak Counter Increment

Goal: make leak-check count fixed heap allocations.

Scope:

- x64 codegen
- leak-check tests

Test first:

- Discarded tuple with no decref should report a leak.

Implementation:

- Increment leak counter on generic heap bump allocation.
- Do not increment on free-list reuse.

Done when:

- Leak-check reports generic heap allocation leaks on x64.

Suggested commit:

- `Count x64 fixed heap allocations in leak checker`

### 2.4 Enable x64 Generic `RefCountInc` Only

Goal: make retain operations update fixed-block refcounts.

Scope:

- x64 codegen

Test first:

- Tuple alias/return case that requires retain should keep working.

Implementation:

- Wire `LIR.RefCountInc(_, _, GenericHeap)` to existing
  `genRefCountIncGeneric`.
- Keep dec disabled.

Done when:

- Retain emits code and does not regress existing tests.

Suggested commit:

- `Enable x64 generic refcount increments`

### 2.5 Enable x64 Generic `RefCountDec` For One Safe Size

Goal: enable reclaim for a narrow fixed-block size before widening.

Scope:

- x64 codegen
- tests

Test first:

- Tuple2 discarded should allocate and then reclaim with no leak.

Implementation:

- Enable `GenericHeap` dec only for payload size 16.
- Use existing `genRefCountDecGeneric`.

Done when:

- Tuple2 leak-check passes.
- No broad generic dec exposure yet.

Suggested commit:

- `Enable x64 generic refcount decrement for tuple2`

### 2.6 Expand x64 Generic Dec To All Valid Fixed Size Classes

Goal: reclaim normal tuples/records/sums/closures by payload size.

Scope:

- x64 codegen
- E2E tests

Test first:

- tuple1, tuple3, record2, boxed option/result leak-check cases.

Implementation:

- Remove the payload-size restriction once layout tests pass.
- Guard free-list use by valid size class.

Done when:

- Fixed-block root reclaim works on x64.

Suggested commit:

- `Enable x64 generic fixed-block reclamation`

## Phase 3: Add Recursive Release For Fixed Blocks

### 3.1 Add Test For Tuple Owning A String

Goal: show that releasing a tuple root does not release child heap fields.

Scope:

- E2E leak-check tests

Test first:

- Tuple containing a dynamic string should leave no leaks after both tuple and
  string release are implemented.

Implementation:

- Test only.

Done when:

- Current failure documents missing destructors.

Suggested commit:

- `Add fixed-block child release leak test`

### 3.2 Add Field Shape Metadata To Fixed-Block RC Operations

Goal: carry enough information for release-to-zero destructors.

Scope:

- ANF/MIR/LIR RC instruction representation, or a side metadata table

Test first:

- IR printer or unit test showing field shapes are preserved.

Implementation:

- Add metadata without changing backend release behavior.

Done when:

- Pipeline preserves field shape metadata.

Suggested commit:

- `Carry fixed-block field shapes through RC IR`

### 3.3 Implement Tuple Child Release On ARM64

Goal: release child heap fields when tuple refcount reaches zero on ARM64.

Scope:

- ARM64 codegen
- tests

Test first:

- Tuple containing dynamic string/list/tuple.

Implementation:

- In generic release-to-zero, call release for each RC-managed field before
  adding the root block to the free list.

Done when:

- Tuple child leak tests pass on ARM64.

Suggested commit:

- `Release tuple children on ARM64`

### 3.4 Implement Tuple Child Release On x64

Goal: match ARM64 tuple child release.

Scope:

- x64 codegen

Test first:

- Same tests as ARM64.

Implementation:

- Add x64 field release loop/helper for fixed blocks.

Done when:

- Tuple child leak tests pass on x64.

Suggested commit:

- `Release tuple children on x64`

### 3.5 Extend Fixed-Block Child Release To Records

Goal: apply tuple child release machinery to records.

Scope:

- RC metadata generation
- tests

Test first:

- Record with dynamic string/list field.

Implementation:

- Emit record field shapes.

Done when:

- Record child leak tests pass.

Suggested commit:

- `Release record children through fixed-block destructors`

### 3.6 Extend Fixed-Block Child Release To Boxed Sums

Goal: release boxed sum payloads correctly.

Scope:

- variant metadata
- backend release

Test first:

- `Some(dynamicString)` and `Ok(dynamicString)` leak tests.

Implementation:

- On release-to-zero, inspect tag and release payload based on variant shape.

Done when:

- Boxed sum payloads are reclaimed.

Suggested commit:

- `Release boxed sum payloads`

## Phase 4: Normalize String And Bytes Memory

### 4.1 Add String Layout Tests For Unaligned Lengths

Goal: catch the `8 + length` versus `8 + aligned(length)` mismatch.

Scope:

- E2E tests

Test first:

- `String.slice` or `String.fromCodepoints` with lengths 1, 3, 7, 8, 9.

Implementation:

- Test only.

Done when:

- Current mismatch is visible.

Suggested commit:

- `Add unaligned string refcount layout tests`

### 4.2 Align Stdlib String Refcount Writes

Goal: make stdlib-created strings match backend string RC offset.

Scope:

- `src/DarkCompiler/stdlib/String.dark`

Test first:

- Tests from 4.1.

Implementation:

- Add/use an internal align helper.
- Write refcount at `8 + aligned(length)`.

Done when:

- Unaligned string layout tests pass.

Suggested commit:

- `Align stdlib string refcount layout`

### 4.3 Add Automatic String Scope Decrefs

Goal: insert `RefCountDecString` for owned dynamic strings.

Scope:

- `2.5_RefCountInsertion.fs`
- tests

Test first:

- Dynamic string concat discarded should not leak.

Implementation:

- Add string ownership handling separately from generic fixed heap.
- Keep `StringSymbol` static literals non-owned.

Done when:

- String concat leak-check passes.

Suggested commit:

- `Insert string lifetime decrefs`

### 4.4 Implement String Free-List Reuse Or Explicit Non-Reuse

Goal: make string release-to-zero reclaim memory, not only decrement leak count.

Scope:

- ARM64 and x64 string dec codegen

Test first:

- Repeated string allocation loop under leak-check and heap pressure.

Implementation:

- Either push variable-size string blocks to a size-class free list or document
  and test leak-count-only release as an interim step.

Done when:

- String release-to-zero has clear memory behavior.

Suggested commit:

- `Reclaim zero-refcount dynamic strings`

### 4.5 Align Stdlib Bytes Refcount Layout

Goal: make bytes layout match dynamic buffer convention.

Scope:

- `src/DarkCompiler/stdlib/Bytes.dark`

Test first:

- `Bytes.create(1)`, `Bytes.create(7)`, `Bytes.create(9)`.

Implementation:

- Use aligned refcount offset.

Done when:

- Bytes layout tests pass.

Suggested commit:

- `Align stdlib bytes refcount layout`

### 4.6 Add Bytes Retain/Release Operations

Goal: make bytes a first-class RC-managed dynamic buffer.

Scope:

- ANF/MIR/LIR ops or shared dynamic-buffer ops
- ARM64 and x64 codegen

Test first:

- `Bytes.create(4)` discarded should not leak.

Implementation:

- Reuse string dynamic-offset logic with bytes-specific shape.

Done when:

- Bytes allocations are released at scope exit.

Suggested commit:

- `Add dynamic bytes reference counting`

## Phase 5: Complete List RC

### 5.1 Add List Of Heap Payload Leak Tests

Goal: show leaf payloads are not recursively released.

Scope:

- E2E tests

Test first:

- list of strings
- list of tuples
- nested list

Implementation:

- Test only.

Done when:

- Current failures are documented.

Suggested commit:

- `Add list payload leak tests`

### 5.2 Remove `TList<TFunction>` GenericHeap Special Case

Goal: make list representation consistently tagged.

Scope:

- `ANF.rcKind`
- tests for list of closures/functions

Test first:

- list of closures compiles and leak-check behavior is explicit.

Implementation:

- Route all `TList _` through `TaggedList`.

Done when:

- No generic fixed-block RC is attempted on tagged list pointers.

Suggested commit:

- `Use tagged-list RC for all list element types`

### 5.3 Enable x64 RawSet Ownership Inc For List Edges

Goal: retain child list nodes when storing them into parent nodes.

Scope:

- x64 codegen `RawSet`

Test first:

- List construction case that previously freed children too early when dec was
  enabled.

Implementation:

- Enable only `Some (TList _)` ownership increment.
- Keep list dec disabled until next unit.

Done when:

- Retain code emits without regressions.

Suggested commit:

- `Enable x64 list edge retains`

### 5.4 Enable x64 TaggedList RefCountInc

Goal: root list retains work on x64.

Scope:

- x64 codegen

Test first:

- Returning borrowed list alias needs retain.

Implementation:

- Wire `TaggedList` inc to helper.

Done when:

- List retain tests pass.

Suggested commit:

- `Enable x64 tagged-list refcount increments`

### 5.5 Enable x64 TaggedList RefCountDec

Goal: reclaim list nodes on x64.

Scope:

- x64 codegen

Test first:

- Primitive list literal discarded should not leak.

Implementation:

- Wire dec helper and leak accounting together.

Done when:

- Primitive list leak-check passes without crypto/TCO regressions.

Suggested commit:

- `Enable x64 tagged-list reclamation`

### 5.6 Release List Leaf Payloads

Goal: reclaim heap values stored inside list leaves.

Scope:

- ARM64 and x64 list dec helpers
- shape metadata for element type

Test first:

- list of strings and list of tuples leak-check tests.

Implementation:

- On LEAF zero, release contained element according to element shape.

Done when:

- Heap payload list tests pass.

Suggested commit:

- `Release tagged-list leaf payloads`

## Phase 6: Make Dict/HAMT Refcounted

### 6.1 Add Dict Leak Tests For Primitive Keys And Values

Goal: lock in current dict leakage.

Scope:

- E2E tests

Test first:

- singleton, set overwrite, remove, fromList.

Implementation:

- Test only.

Done when:

- Dict raw node leaks are visible.

Suggested commit:

- `Add dict leak-check coverage`

### 6.2 Add Refcount Slot To HAMT Leaf Nodes

Goal: introduce leaf node RC layout without full recursive release.

Scope:

- `__HAMT.dark`
- codegen release helper later

Test first:

- Inspect or behavior test for leaf allocation layout.

Implementation:

- Allocate leaf as `[key][value][refcount]`.
- Initialize refcount to 1.

Done when:

- Existing dict behavior still works.

Suggested commit:

- `Add HAMT leaf refcount field`

### 6.3 Add Refcount Slot To HAMT Internal Nodes

Goal: make internal node layout ready for RC.

Scope:

- `__HAMT.dark`

Test first:

- Dict with multiple children still works.

Implementation:

- Allocate `[bitmap][children...][refcount]`.

Done when:

- Existing dict tests pass.

Suggested commit:

- `Add HAMT internal node refcount field`

### 6.4 Add Refcount Slot To HAMT Collision Nodes

Goal: complete HAMT node layout changes.

Scope:

- `__HAMT.dark`

Test first:

- Collision-specific tests if available, otherwise add one with forced hash
  collision if practical.

Implementation:

- Allocate `[count][entries...][refcount]`.

Done when:

- Collision path still works or is explicitly covered as not implemented.

Suggested commit:

- `Add HAMT collision node refcount field`

### 6.5 Add Dict Retain Helper

Goal: increment HAMT node refcounts recursively only at roots/edges as needed.

Scope:

- backend helper or compiler intrinsic

Test first:

- Copy/share dict child and keep both roots alive.

Implementation:

- Retain root node by tag.
- Do not recursively retain children for immutable shared subtrees; the node
  refcount owns the subtree edge set.

Done when:

- Shared dict roots do not free live subtrees.

Suggested commit:

- `Add dict root retain helper`

### 6.6 Add Dict Release Helper For Primitive Keys And Values

Goal: reclaim HAMT nodes that contain only primitive payloads.

Scope:

- backend helpers
- RC insertion shape dispatch

Test first:

- primitive dict singleton discarded should not leak.

Implementation:

- Release root; when node refcount reaches zero, release children and free node.

Done when:

- Primitive dict leak-check tests pass.

Suggested commit:

- `Reclaim primitive HAMT nodes`

### 6.7 Release Dict Keys And Values By Shape

Goal: handle heap-managed keys/values.

Scope:

- dict release helper
- shape metadata

Test first:

- dict string-to-string
- dict int-to-list
- dict int-to-record

Implementation:

- Release keys and values in leaf/collision nodes based on shape.

Done when:

- Heap payload dict leak tests pass.

Suggested commit:

- `Release HAMT keys and values by RC shape`

## Phase 7: Make Closures First-Class RC Values

### 7.1 Add Closure Leak And Capture Tests

Goal: expose closure root and capture leaks.

Scope:

- E2E tests

Test first:

- discarded closure with no captures
- discarded closure capturing string
- closure stored in tuple/list

Implementation:

- Test only.

Done when:

- Current gaps are visible.

Suggested commit:

- `Add closure memory leak tests`

### 7.2 Classify Static Function References Separately

Goal: avoid RC on non-capturing function addresses.

Scope:

- shape classifier
- ANF lowering

Test first:

- function reference value does not allocate or leak.

Implementation:

- Mark `FuncRef` as static function ref.

Done when:

- Static function values remain immediate/static.

Suggested commit:

- `Classify static function references as non-owned`

### 7.3 Treat ClosureAlloc As `Closure` Shape

Goal: stop pretending closures are tuples for ownership.

Scope:

- RC insertion
- type map or shape map

Test first:

- closure allocation gets closure shape in IR diagnostics.

Implementation:

- Add closure shape with capture metadata.

Done when:

- Closure RC no longer depends on `TFunction` or fake tuple inference.

Suggested commit:

- `Represent closure allocations with closure RC shape`

### 7.4 Retain Captures On Closure Allocation

Goal: closure fields own captured heap values.

Scope:

- RC insertion or closure lowering

Test first:

- closure captures string/list and outlives original binding.

Implementation:

- Insert retains for RC-managed captures.

Done when:

- Captured heap values remain live while closure is live.

Suggested commit:

- `Retain heap captures in closures`

### 7.5 Release Captures On Closure Release

Goal: reclaim captured values when closure dies.

Scope:

- fixed-block/closure release helper

Test first:

- discarded closure capturing string/list has no leak.

Implementation:

- Closure release-to-zero releases each capture by shape.

Done when:

- Closure leak tests pass.

Suggested commit:

- `Release closure captures on refcount zero`

## Phase 8: Replace Legacy Heap Checks

### 8.1 Replace `ANF.isHeapType` In RC Insertion

Goal: drive ownership insertion from `RcShape`.

Scope:

- `2.5_RefCountInsertion.fs`

Test first:

- Existing leak-check matrix.

Implementation:

- Use central shape classifier for all retain/decref decisions.

Done when:

- `TString`, `TBytes`, closures, lists, dicts, sums are no longer decided by
  ad hoc source-type checks.

Suggested commit:

- `Use RC shapes in ownership insertion`

### 8.2 Replace RawSet Ownership Decisions With Shapes

Goal: remove duplicated backend type checks for stored edges.

Scope:

- MIR/LIR/codegen metadata

Test first:

- RawSet of list, dict, string, tuple payloads.

Implementation:

- Attach value shape to edge stores.

Done when:

- Backends no longer independently guess which raw stores need retain.

Suggested commit:

- `Use RC shapes for raw edge stores`

### 8.3 Delete Or Deprecate Legacy `isHeapType`

Goal: prevent new memory bugs from using the old classifier.

Scope:

- `ANF.fs`
- call sites

Test first:

- Full suite.

Implementation:

- Remove the function or rename it to a narrow non-ownership helper.

Done when:

- Ownership code cannot call source-type heap classification.

Suggested commit:

- `Remove legacy heap-type ownership checks`

## Phase 9: Backend Parity And Hardening

### 9.1 Add Backend Parity Leak Matrix

Goal: run the same leak cases on ARM64 and x64 where CI supports it.

Scope:

- tests/scripts

Test first:

- Matrix includes tuple, record, sum, list, string, bytes, dict, closure.

Implementation:

- Add target-aware test metadata if needed.

Done when:

- Backend differences are explicit and tracked.

Suggested commit:

- `Add backend parity leak-check matrix`

### 9.2 Harden Free-List Size-Class Bounds

Goal: prevent out-of-range free-list accesses.

Scope:

- ARM64 and x64 codegen

Test first:

- Heap object payload >248 bytes.

Implementation:

- Check size class before reading/writing free-list table.

Done when:

- Large objects do not index outside free-list heads.

Suggested commit:

- `Harden free-list size-class bounds`

### 9.3 Add Heap Pressure Reuse Tests

Goal: verify reclaimed blocks are actually reusable.

Scope:

- E2E or benchmark-like tests under 50ms

Test first:

- Repeated allocate/drop loop for each size class.

Implementation:

- Tests only if runtime already supports reuse.

Done when:

- Reuse behavior is covered without relying only on leak count.

Suggested commit:

- `Add heap pressure tests for reclaimed blocks`

## Phase 10: Optimization After Correctness

### 10.1 Add No-Op Retain/Release Pair Elision

Goal: reduce obvious RC overhead after correctness is locked.

Scope:

- ANF or MIR optimization

Test first:

- IR-level test showing adjacent retain/release pair.

Implementation:

- Remove only trivially adjacent pairs on same value with no intervening use
  that can observe refcount.

Done when:

- No behavior changes, fewer RC ops.

Suggested commit:

- `Elide adjacent refcount retain release pairs`

### 10.2 Add Unique Temporary Allocation Fast Path

Goal: avoid retain when a freshly allocated value is stored exactly once.

Scope:

- ownership pass

Test first:

- IR test for tuple/list construction.

Implementation:

- Conservative local-only uniqueness check.

Done when:

- Generated IR has fewer redundant retains without changing release behavior.

Suggested commit:

- `Avoid redundant retains for unique temporaries`

### 10.3 Benchmark Memory Workloads

Goal: ensure correctness work did not regress performance badly.

Scope:

- benchmarks
- `RESULTS.md`

Test first:

- Not applicable; benchmark validation.

Implementation:

- Run full benchmarks.
- Record performance ratio from `RESULTS.md`.

Done when:

- Benchmark result is recorded and regressions are understood.

Suggested commit:

- `Record refcounting benchmark baseline`

## Suggested Landing Order

1. Phase 0: observability tests.
2. Phase 1: representation metadata.
3. Phase 2: x64 fixed-block allocation and root reclaim.
4. Phase 3: recursive fixed-block release.
5. Phase 4: strings and bytes.
6. Phase 5: lists.
7. Phase 6: dicts.
8. Phase 7: closures.
9. Phase 8: remove legacy ownership checks.
10. Phase 9: backend parity and allocator hardening.
11. Phase 10: optimization and benchmark validation.

## Milestone Definitions

Milestone A: Fixed blocks are sound.

- tuples, records, boxed sums, and closures have correct root allocation,
  retain, release, child release, and leak accounting on both backends.

Milestone B: Dynamic buffers are sound.

- strings and bytes have one layout, automatic decrefs, and real release paths.

Milestone C: Persistent collections are sound.

- lists and dicts retain shared children and release dead subgraphs.

Milestone D: Ownership is shape-driven.

- `RcShape` drives all ownership decisions.
- No compiler pass relies on source-type-only heap checks for memory behavior.

Milestone E: RC is optimizable.

- correctness tests pass.
- full benchmarks complete.
- RC elision work can proceed safely.
