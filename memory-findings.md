<!--
memory-findings.md - Investigation report for Dark compiler runtime memory management.

This document audits heap representations, ownership insertion, backend reclaim
paths, and a unification plan for moving the compiler to one reference-counted
memory model.
-->

# Memory Management Findings

Status date: 2026-05-16.

This report investigates whether each runtime data structure and language
feature uses reference counting, whether that reference counting reaches real
memory reclamation, and what must change to unify all heap-managed values under
one model.

## Executive Summary

The compiler does not currently have one memory model. It has a reference
count insertion pass, several backend-specific RC implementations, stdlib
structures that manually allocate raw memory, and x64 paths where the RC
instructions are present in IR but disabled in codegen.

The intended model appears to be:

- heap values carry a refcount
- owned values are decremented at scope exit
- borrowed aliases and projections are not decremented
- returned borrowed values are retained before return
- freed blocks are recycled through size-segregated free lists

The implemented model is mixed:

- tuples, records, boxed sums, and closures are represented as fixed-size heap
  blocks with a trailing refcount
- lists are tagged FingerTree nodes allocated through raw allocation, with
  special list retain/release helpers
- strings and bytes are variable-size heap buffers with length, data, and a
  trailing refcount
- dicts are tagged HAMT nodes allocated through raw allocation with no node
  refcount field
- raw pointers are explicitly unmanaged, and `RawFree` is currently a no-op
- ARM64 implements active generic RC and active list RC helpers
- x64 has much of the RC machinery in the file, but `LIR.RefCountInc` and
  `LIR.RefCountDec` currently emit no code

The highest-risk findings are:

1. x64 does not actually reclaim generic RC-managed heap values, because generic
   `RefCountInc`/`RefCountDec` codegen is disabled.
2. x64 `LIR.HeapAlloc` does not initialize the trailing refcount and does not
   consistently bump by payload plus refcount for generic tuple/record/sum
   allocations. This matches `docs/x64-refcounting.md`, which says generic
   `RefCountDec` causes many failures when enabled.
3. Strings and bytes have a layout split: backend string RC uses
   `8 + aligned(length)`, while many stdlib builders write refcount at
   `8 + length`.
4. Strings and bytes are not included in the generic ownership pass, so normal
   scopes do not automatically emit string/bytes decrefs.
5. Dict/HAMT does not use real reference counting. It allocates raw tagged nodes
   and never frees old persistent nodes.
6. `RawFree` is a no-op in both backends.
7. Closure values are heap objects, but ownership tracking is inconsistent
   because closure allocation is inferred as tuple-like while many source-level
   function values are typed as `TFunction`, which is not generally RC-managed.
8. List RC is split across ANF list literal lowering, raw-memory edge stores,
   and backend helper routines. ARM64 has active behavior; x64 has disabled
   behavior.
9. The leak checker is not a complete truth source. On x64, some heap
   allocations are not counted, so a missing leak report can be a false
   negative.

## Investigation Notes

Primary files inspected:

- `src/DarkCompiler/ANF.fs`
- `src/DarkCompiler/passes/2_AST_to_ANF.fs`
- `src/DarkCompiler/passes/2.5_RefCountInsertion.fs`
- `src/DarkCompiler/passes/3_ANF_to_MIR.fs`
- `src/DarkCompiler/passes/4_MIR_to_LIR.fs`
- `src/DarkCompiler/passes/arm64/6_CodeGen.fs`
- `src/DarkCompiler/passes/x64/6_CodeGen.fs`
- `src/DarkCompiler/stdlib/String.dark`
- `src/DarkCompiler/stdlib/Bytes.dark`
- `src/DarkCompiler/stdlib/List.dark`
- `src/DarkCompiler/stdlib/__FingerTree.dark`
- `src/DarkCompiler/stdlib/Dict.dark`
- `src/DarkCompiler/stdlib/__HAMT.dark`
- `docs/features/reference-counting.md`
- `docs/x64-refcounting.md`

Small leak-check probes were also run through `scripts/run-in-container` on the
current workspace target, which is Linux x86_64. These probes are useful for
spot-checking the active backend, but not sufficient as proof for generic heap
objects because x64 leak accounting is incomplete.

Observed x64 leak-check probe results:

| Probe | Result | Interpretation |
|---|---:|---|
| tuple2 discarded | no leak report | False-negative risk: generic x64 heap alloc is not counted and generic dec is disabled. |
| tuple3 discarded | no leak report | Same false-negative risk. |
| string concat returned | `leaks: 1` | String concat increments leak counter and is not reclaimed automatically. |
| `Bytes.create(4)` returned | `leaks: 1` | Bytes raw allocation is counted and not reclaimed. |
| `Dict.singleton<Int64, Int64>(1, 2)` returned | `leaks: 1` | Dict/HAMT raw node allocation is not reclaimed. |
| `[1, 2, 3]` returned | `leaks: 14` | FingerTree/list raw nodes are counted and not reclaimed on x64. |
| discarded closure | no leak report | False-negative risk: generic x64 heap alloc is not counted and closure RC is inconsistent. |

## Current Ownership Pipeline

### Source Types

`AST.Type` models language-level types:

- primitive integers
- `Bool`
- `Float64`
- `String`
- `Bytes`
- `Char`
- `Unit`
- `Function`
- `Tuple`
- `Record`
- `Sum`
- `List`
- `RawPtr`
- `Dict`

It does not encode ownership or runtime representation. This is the root
reason several values with the same source shape require special handling later.
For example, `TSum` can be an immediate tag or a boxed `[tag, payload]` object,
and `TString` can be a literal/static string or a heap buffer.

### ANF Heap Classification

`ANF.isHeapType` returns true for:

- `TTuple`
- `TRecord`
- `TList`
- `TSum`
- `TDict`

It returns false for:

- `TString`
- `TBytes`
- `TFunction`
- `TRawPtr`
- primitives

This already diverges from the actual runtime, where strings, bytes, and
closures can be heap objects.

### RC Insertion

`2.5_RefCountInsertion.fs` inserts `RefCountInc` and `RefCountDec` based on:

- inferred source type
- whether the expression is borrowing/aliasing
- whether the value is returned
- function parameter ownership on return

Borrowing expressions are:

- `IfValue`
- `TupleGet`
- `RawGet`
- `Atom(Var _)`
- `TypedAtom(Var _, _)`

This means projections from heap containers are treated as borrowed and are not
independently decremented. Returning a borrowed heap value inserts a retain to
materialize ownership.

The pass excludes `TDict` from `isRcManagedHeapType` even though `ANF.isHeapType`
includes it. That split is deliberate enough to avoid generic RC on tagged HAMT
pointers, but it leaves a global inconsistency: any code using `ANF.isHeapType`
directly can still treat dicts as RC-managed.

### IR Preservation

`RefCountInc`, `RefCountDec`, `RefCountIncString`, and `RefCountDecString` are
preserved through ANF, MIR, and LIR. Optimizers classify them as side-effecting.

This means the IR can contain a correct-looking ownership program even when the
backend emits no reclaiming code for it.

### Backend Divergence

ARM64:

- initializes trailing refcounts for `HeapAlloc`
- increments and decrements generic fixed-size heap blocks
- pushes zero-refcount generic blocks into free lists
- has list RC helpers for tagged FingerTree nodes
- has string RC intrinsics, but string free only updates the leak counter and
  does not push the variable-size block to a reusable free list
- implements raw allocation with free-list reuse for blocks whose aligned size
  maps to supported classes
- implements `RawFree` as no-op

x64:

- maps reserved heap pointer to `R14` and free-list base to `R15`
- contains helper routines for generic RC and list RC
- leaves `LIR.RefCountInc` as no-op for both generic heap and tagged lists
- leaves `LIR.RefCountDec` as no-op for both generic heap and tagged lists
- has active string-specific RC intrinsics, but automatic insertion is missing
- has raw allocation free-list reuse
- implements `RawFree` as no-op
- `docs/x64-refcounting.md` states that enabling generic `RefCountDec` causes
  extensive failures and that list RC wiring is written but disabled

## Data Structure Findings

### Primitive Scalars

Types:

- all signed and unsigned integer types
- `Bool`
- `Float64`
- `Unit`

Runtime representation:

- immediate register/stack values
- no heap allocation
- no refcount

Does it use RC:

- No, correctly.

Does reclamation work:

- Not applicable.

Risks:

- Primitive fields stored in heap objects are fine.
- `Float64` needs special heap-store handling because it may live in FP
  registers, but it is not itself heap-managed.

Plan:

- Keep primitives as non-RC values.
- Ensure representation metadata explicitly marks them `Immediate`.

### Tuples

Runtime representation:

- fixed-size heap block
- payload is `8 * arity` bytes
- refcount is stored immediately after payload
- examples: tuple2 payload 16, tuple3 payload 24

Allocation path:

- ANF emits `TupleAlloc`
- MIR emits `HeapAlloc(dest, payloadSize)` followed by `HeapStore` for fields
- ARM64 `HeapAlloc` allocates payload plus refcount and initializes refcount
- x64 `HeapAlloc` currently bumps by `sizeBytes` only and does not initialize
  the trailing refcount in the generic path

Does it use RC:

- RC insertion: Yes.
- ARM64 backend: Yes.
- x64 backend: No effective reclaim; `RefCountInc`/`RefCountDec` are no-op.

Does reclamation work:

- ARM64: partially. Fixed-size block decrement frees the root block to a size
  class free list, but generic release does not recursively release heap fields.
- x64: no. The generic RC ops are disabled, and generic allocation does not
  maintain the refcount layout needed by those ops.

Important correctness gap:

- Generic tuple release only frees the tuple block. It does not walk tuple
  fields and release owned child heap values. The current model compensates
  partly with `TupleAlloc` element retains and scope decs, but a tuple that is
  the final owner of nested heap values needs a destructor-like release path to
  decrement children when the tuple itself reaches zero.

Plan:

- Define tuple representation as `FixedBlock { payloadSize; fields }`.
- Make allocation, retain, and release derive from that representation.
- On release-to-zero, recursively release all RC-managed fields before placing
  the tuple block on a free list.
- Fix x64 `HeapAlloc` before enabling generic dec: allocate payload plus
  refcount, initialize refcount to 1, and increment leak counter on bump
  allocation.
- Add leak-check tests for tuple payload sizes 8, 16, 24, 32, and >248.

### Records

Runtime representation:

- same physical representation as tuples: fixed-size heap block
- payload is one 8-byte slot per field
- trailing refcount

Allocation path:

- records lower to tuple-like allocation and field stores
- `ANF.payloadSize` derives record payload size from the type registry

Does it use RC:

- RC insertion: Yes.
- ARM64 backend: Yes for root block.
- x64 backend: No effective reclaim.

Does reclamation work:

- Same as tuples: root block may be reclaimed on ARM64, but child fields are not
  recursively released; x64 does not reclaim.

Risks:

- Record type metadata is needed for field count. If type registry lookup is
  missing, `payloadSize` crashes.
- Generic release lacks field descriptors, so it cannot know which fields need
  recursive release.

Plan:

- Treat records as `FixedBlock` with named field metadata.
- Generate or preserve enough field type metadata for backend release helpers.
- Share tuple and record release logic.

### Sum Types / ADTs

Runtime representation:

- pure enum with no payload variants can be represented as an immediate tag
- payload variants and no-payload variants in mixed sum types are heap boxed as
  `[tag, payload]`
- boxed sum payload size is 16 bytes, with trailing refcount at offset 16

Allocation path:

- `2_AST_to_ANF.fs` returns immediate tag when the entire sum type has no
  payload variants
- otherwise it emits `TupleAlloc [tag; payloadOrZero]`

Does it use RC:

- RC insertion: partially.
- `ANF.isHeapType` treats all `TSum` as heap-like, conservatively.
- Actual representation is mixed, so immediate enum values can be treated as
  heap by ownership logic unless inference/lowering keeps them distinct.

Does reclamation work:

- ARM64: boxed sum root block can be reclaimed, but payload child release is not
  recursively handled by generic release.
- x64: no effective reclaim.
- Pure immediate enum has no heap to reclaim.

Risks:

- The source type `TSum` is not enough to know whether a particular value is
  immediate or boxed.
- Payload type inference for generic/multi-parameter sums is fragile. The RC
  pass has recovery logic for typed aliases and `RawGet`, but unresolved payload
  types can suppress needed RC.
- Options/results that carry strings, bytes, lists, records, dicts, or closures
  need recursive payload release when the boxed sum is released.

Plan:

- Split sum representation into `ImmediateEnum` and `BoxedSum`.
- Add variant metadata to release helpers.
- On release-to-zero for a boxed sum, release payload according to the active
  variant's payload type.
- Ensure pattern matching over sums preserves borrowed payload semantics and
  retains when a payload escapes.

### Lists / FingerTree

Runtime representation:

- tagged pointer in low 3 bits
- tag 0 is empty/null
- tag 1 `SINGLE`: raw block `[node:8][refcount:8]`
- tag 2 `DEEP`: raw block `[measure, prefix_count, p0..p3, middle,
  suffix_count, s0..s3][refcount]`
- tag 3 `NODE2`: raw block `[child0, child1, measure][refcount]`
- tag 4 `NODE3`: raw block `[child0, child1, child2, measure][refcount]`
- tag 5 `LEAF`: raw block `[value][refcount]`

Allocation path:

- list literals are lowered directly in `2_AST_to_ANF.fs` with `RawAlloc`,
  `RawSet`, manual refcount initialization, and pointer tagging
- stdlib list operations allocate nodes in `__FingerTree.dark`, also through
  `__raw_alloc` and manual refcount initialization

Does it use RC:

- Conceptually yes, but through special list helpers rather than generic fixed
  block RC.
- RC insertion uses `ANF.rcKind` to mark normal `TList` as `TaggedList`.
- `TList<TFunction>` is special-cased to `GenericHeap`, which is suspicious
  because list representation is still a tagged FingerTree pointer.
- ARM64 `RefCountInc/Dec` dispatches `TaggedList` to list helper routines.
- x64 `TaggedList` inc/dec codegen is disabled.

Does reclamation work:

- ARM64: partially. The list dec helper recursively walks list tree nodes when
  a node refcount reaches zero and pushes nodes to the free list. It is the most
  complete specialized reclaim path in the compiler.
- x64: no. The helper exists, but calls are disabled.
- Element values inside LEAF nodes are not generally released recursively when a
  leaf dies. The helper knows list-node topology but not element type ownership.

Risks:

- List ownership is split across several places:
  - pass 2 list literal lowering increments heap elements stored in leaves
  - backend `RawSet` may increment stored list child edges
  - backend list dec helper recursively releases list child nodes
  - pass 2.5 inserts `TaggedList` root decrefs
- ARM64 `RawSet` increments list children and other RC-managed heap values when
  `valueType` is provided. x64 has this written as a comment but disabled.
- `RawSet` with `valueType=None` cannot know it is storing an owned heap edge.
- The list dec helper releases list child nodes but not arbitrary heap payloads
  in leaf values.
- `TList<TFunction>` choosing `GenericHeap` is inconsistent with the tagged list
  runtime representation.

Plan:

- Represent list as `TaggedList elemShape`, not as generic heap.
- Make all list node constructors use typed edge writes.
- Implement release-to-zero for LEAF nodes that releases the contained element
  according to `elemShape`.
- Remove or justify the `TList<TFunction> -> GenericHeap` special case.
- Enable x64 list retain/release only after raw edge ownership increments and
  leak counter accounting are correct.
- Add tests for lists of:
  - primitives
  - tuples/records
  - strings
  - bytes
  - dicts
  - closures
  - nested lists

### Dictionaries / HAMT

Runtime representation:

- tagged pointer using low 2 bits
- tag 0: empty/null
- tag 1: internal node `[bitmap][children...]`
- tag 2: leaf node `[key][value]`
- tag 3: collision node `[count][key1][value1]...`
- nodes are raw allocations with no refcount field

Allocation path:

- `Dict.dark` is public API
- `__HAMT.dark` allocates all nodes with `__raw_alloc`
- persistent updates allocate new path nodes and share unchanged subtrees

Does it use RC:

- No coherent RC model.
- `ANF.isHeapType` says `TDict` is heap-like.
- `RefCountInsertion.isRcManagedHeapType` excludes `TDict`.
- `RawSet` edge increments explicitly exclude `TDict`.
- HAMT nodes do not store refcounts.

Does reclamation work:

- No. Old persistent HAMT nodes are never released, and `RawFree` is no-op.
- Returning/removing/updating dictionaries allocates raw nodes that remain live
  until process exit.

Risks:

- Persistent data structure sharing requires exact ownership semantics. Dict
  currently shares old children without incrementing child ownership and
  discards replaced paths without decrementing old ownership.
- Keys and values can themselves be heap-managed. HAMT leaf/collision/internal
  release would need to release key/value fields and child nodes.
- Because dict roots are tagged pointers, generic fixed-offset RC cannot be
  applied directly.

Plan:

- Choose one model. The recommended model is RC-managed HAMT nodes.
- Add refcount to every HAMT node layout:
  - leaf: `[key][value][refcount]`
  - internal: `[bitmap][children...][refcount]`
  - collision: `[count][entries...][refcount]`
- Add `DictRoot keyShape valueShape` RC shape.
- Retain copied children and copied keys/values when constructing shared nodes.
- Release old nodes recursively on refcount zero:
  - internal nodes release each child
  - leaf nodes release key and value when their shapes require RC
  - collision nodes release all keys and values
- Remove `TDict` from generic `ANF.isHeapType` or replace that API with one
  representation-aware classifier.

### Strings

Runtime representation:

- intended heap layout: `[length:8][data:N][padding][refcount:8]`
- literal/static strings can appear as `StringSymbol` and are sometimes treated
  as not owning heap memory
- backend string RC checks for an `INT64_MAX` sentinel, but ordinary literal
  operands often bypass RC directly as `StringSymbol`

Allocation paths:

- string literals can be emitted as static pool data or converted to heap
  strings when stored in heap structures or passed to file operations
- `StringConcat` allocates a new heap string and initializes refcount
- `FloatToString` returns a heap string
- file I/O can allocate heap strings for read/error results
- stdlib `String.slice` and `String.fromCodepoints` allocate with `__raw_alloc`

Does it use RC:

- Not generally.
- `RefCountIncString` and `RefCountDecString` exist in IR and both backends.
- Pass 2.5 does not automatically insert string lifetime decrefs for normal
  `TString` values.
- `ANF.isHeapType` excludes `TString`.

Does reclamation work:

- Automatic reclamation: no.
- ARM64/x64 string-specific decref can decrement the string refcount and update
  the leak counter on zero, but neither backend pushes variable-size string
  blocks into a reusable free list.
- Many strings never receive a `RefCountDecString`.

Critical layout mismatch:

- Backend string RC uses refcount offset `8 + aligned(length)`.
- Several stdlib string builders write refcount at `8 + length`.
- When `length` is not 8-byte aligned, the backend will read and write the
  wrong word.

Risks:

- `String.slice("abcdef", 0, 3)` writes refcount at offset 11, while backend RC
  expects offset 16.
- File operation helper conversions may allocate temporary heap strings that are
  never released.
- Literal handling is inconsistent: some paths allocate literal copies on the
  heap and count leaks, while others treat `StringSymbol` as non-owned.

Plan:

- Canonicalize string layout as `[len][data][padding][refcount]`.
- Update all stdlib raw string builders to write at `8 + aligned(length)`.
- Add `DynamicString` RC shape.
- Insert automatic string decrefs at scope exit.
- Define literal strings as `StaticString` with no refcount and no leak counter
  increment, or materialize them explicitly into owned heap strings with normal
  RC. Do not mix the two without representation tags.
- Implement variable-size free/reuse for strings, or route strings through a
  size-class allocator using total allocated size.

### Bytes

Runtime representation:

- intended layout is the same as string:
  `[length:8][data:N][padding][refcount:8]`

Allocation path:

- `Bytes.create`, `Bytes.set`, and `Bytes.fromList` allocate with `__raw_alloc`
- they manually write length and refcount

Does it use RC:

- No automatic RC.
- `TBytes` is excluded from `ANF.isHeapType`.
- There are no bytes-specific retain/release IR operations.

Does reclamation work:

- No. Raw allocation is counted by leak check but `RawFree` is no-op and no
  automatic decrement is emitted.

Critical layout mismatch:

- stdlib bytes builders write refcount at `8 + length`, not
  `8 + aligned(length)`.
- There is no backend bytes RC today, but if bytes reuse string RC logic later,
  this will break for non-8-byte-aligned lengths.

Plan:

- Canonicalize bytes layout with aligned trailing refcount.
- Add `DynamicBytes` RC shape.
- Insert automatic bytes decrefs.
- Implement retain/release logic parallel to strings.
- Consider sharing one variable-size buffer implementation between `String` and
  `Bytes`, with distinct type-level constructors.

### Closures And Function Values

Runtime representation:

- closure heap object: `[func_ptr][capture0][capture1]...[refcount]`
- closure calls pass the closure as hidden first argument
- lifted closure functions receive a tuple-like closure parameter

Allocation path:

- lambdas and partial applications can lower to `ClosureAlloc`
- `ClosureAlloc` is inferred in pass 2.5 as tuple-like for ownership purposes
- MIR and LIR lower it to heap allocation and field stores

Does it use RC:

- Inconsistent.
- Fresh `ClosureAlloc` can be treated as tuple-like by inference.
- Values typed as `TFunction` are not generally considered RC-managed.
- Captured heap values are stored in closure fields, but release-to-zero does
  not recursively release captures.

Does reclamation work:

- ARM64: closure root block may be reclaimed when it is typed/inferred as a
  tuple-like heap value and receives a generic `RefCountDec`.
- x64: no effective generic reclaim.
- Captures are not recursively released.

Risks:

- Function values can be represented as plain function references or heap
  closures. The source type `TFunction` does not distinguish those cases.
- Non-capturing function references should be static and not RC-managed.
- Capturing closures should be RC-managed.
- Closure capture fields need typed ownership increments on allocation and
  decrements on closure release.

Plan:

- Split function representation into:
  - `StaticFunctionRef`
  - `Closure { captureShapes }`
- Treat closure allocation as a first-class RC shape, not as a fake tuple.
- Retain captured heap values when installing them into a closure.
- Release captures when closure refcount reaches zero.
- Ensure indirect calls and closure calls borrow the closure during the call.

### Raw Pointers

Runtime representation:

- untyped raw pointer
- used internally for HAMT, FingerTree, string/bytes builders, and casts

Allocation path:

- `RawAlloc` in LIR/codegen
- ARM64 and x64 align the requested allocation size
- both can use free-list reuse for eligible classes
- both increment leak counter on raw bump allocation in the active code paths
  inspected, except x64 comments indicate raw leak increment is deliberately
  omitted in one path until list dec is enabled

Does it use RC:

- No. `TRawPtr` is explicitly unmanaged.

Does reclamation work:

- No. `RawFree` emits no instructions.
- Some raw blocks can be recycled only if a specialized RC helper places them
  on a free list. `RawFree` itself does not.

Risks:

- Raw memory is being used not just for low-level buffers, but for high-level
  persistent structures. That makes “raw is manually managed” untrue in
  practice because no manual free exists.
- `RawSet` cannot generally know whether it stores an owned edge unless
  `valueType` is supplied.

Plan:

- Keep `RawPtr` only for true unmanaged interop/internal buffers.
- For compiler-owned data structures, replace raw-pointer allocation APIs with
  typed constructors that carry RC shape metadata.
- Implement `RawFree` only if there is a real manual-memory discipline; otherwise
  remove or isolate it from user-visible compiler-owned structures.

### File I/O Results

Runtime representation:

- file read returns `Result<String, String>`
- write/append/delete/setExecutable return `Result<Unit, String>`
- backend code allocates result sum blocks directly in some paths
- error strings are heap allocated in several backend paths

Does it use RC:

- Result root is a boxed sum and should be covered by generic RC.
- Contained strings are not automatically released.
- On x64 generic RC is disabled.

Does reclamation work:

- ARM64: result root can be reclaimed if it receives generic dec, but contained
  string payloads are not recursively released.
- x64: no effective generic reclaim.
- File path temporary heap strings made from literals are not automatically
  released.

Plan:

- Route file result allocations through normal boxed sum allocation helpers.
- Ensure result release recursively releases payload strings.
- Avoid heap-materializing literal path strings unless ownership is explicit.

### Pattern Matching And Projection

Runtime behavior:

- tuple, record, sum, and list patterns extract fields/payloads from existing
  heap structures
- extraction is generally modeled as borrowed by `TupleGet` or `RawGet`

Does it use RC:

- Borrowed projections do not get decrefs.
- If a borrowed projection is returned, pass 2.5 can insert a retain.

Does reclamation work:

- Partially for fixed heap/list values on ARM64.
- Not for strings/bytes/dicts/closures in general.
- Not on x64.

Risks:

- Borrowed payloads escaping through aliases require accurate type inference.
- `RawGet` without a `valueType` can become `TVar`, which suppresses required RC
  rather than guessing.
- Multi-argument sum payload recovery remains fragile.

Plan:

- Make borrow/own behavior explicit in IR.
- Require typed projection operations for every heap-shaped payload.
- Use representation-aware shapes to decide whether escaping borrowed values
  need retain operations.

### Equality, Printing, And Display

Runtime behavior:

- structural equality reads through compound values
- printing/display reads strings, lists, sums, records, and bytes

Does it use RC:

- These operations generally borrow inputs.
- They may allocate strings for display/float conversion.

Does reclamation work:

- Read-only traversal does not need reclamation.
- Any allocated temporary strings/lists/sums from display paths need normal RC,
  and currently strings are not automatically managed.

Plan:

- Annotate display/equality helpers as borrowed-input operations.
- Ensure any returned display strings are owned and receive automatic decref at
  the caller when not returned.

## Cross-Cutting Problems

### Source Type Is Not Runtime Shape

The compiler uses `AST.Type` as a proxy for memory behavior. That does not work:

- `TSum` can be immediate or boxed.
- `TString` can be static or dynamic.
- `TFunction` can be static function reference or heap closure.
- `TDict` is a tagged HAMT root, not a fixed heap block.
- `TList` is a tagged FingerTree root, not a fixed heap block.

Fix:

- Introduce a runtime representation/ownership type. Do not use source type
  directly for retain/release decisions.

### Generic Release Does Not Run Destructors

Fixed-size generic release decrements the root refcount and frees the block, but
does not release child heap fields. For immutable compound values, the root is
often the owner of references to nested values.

Fix:

- Every RC-managed shape needs a release-to-zero destructor that releases owned
  child fields before recycling the block.

### Raw Allocation Is Used For Managed Structures

Raw allocation is used by:

- list nodes
- dict nodes
- strings and bytes in stdlib
- internal buffers

But `RawFree` is no-op, and raw nodes only reclaim when a specialized helper
does so.

Fix:

- Stop using untyped raw allocation for managed data structures, or wrap it in
  typed constructors/release helpers with explicit shapes.

### Backend Behavior Is Not Equivalent

ARM64 and x64 are not just at different optimization levels; they have different
semantics:

- ARM64 emits active generic and list RC.
- x64 emits no generic/list RC.
- x64 generic heap allocation layout is not currently compatible with generic
  RC.

Fix:

- Treat x64 RC enablement as a correctness project, not a small switch flip.
- Add backend-independent IR tests and backend-specific leak tests.

### Leak Counter Is Not Complete Memory Accounting

The leak counter is useful, but currently measures only some allocation paths
and some release paths. It should not be used as proof that memory management is
correct unless allocation and release accounting are complete for that shape.

Fix:

- Define leak accounting per allocation shape.
- Count only owned dynamic heap allocations.
- Decrement only when real release-to-zero happens.
- Add tests that fail when generic x64 heap allocations are not counted.

## Unified Refcounting Plan

### Phase 1: Define Runtime Ownership Shapes

Add one central classifier that maps typed IR values to runtime memory shapes:

```fsharp
type RcShape =
    | Immediate
    | StaticString
    | FixedBlock of payloadSize:int * fields:FieldShape list
    | BoxedSum of variants:VariantShape list
    | TaggedList of elem:RcShape
    | DictRoot of key:RcShape * value:RcShape
    | DynamicString
    | DynamicBytes
    | Closure of captures:RcShape list
    | RawUnmanaged
```

Use this classifier everywhere:

- RC insertion
- tuple/list/raw edge stores
- payload-size calculation
- backend retain/release dispatch
- leak accounting

Delete or narrow old APIs that answer only “is heap type?” without shape.

### Phase 2: Stabilize Fixed-Block RC

Scope:

- tuples
- records
- boxed sums
- closures

Work:

- Make `HeapAlloc` layout identical on ARM64 and x64.
- Always allocate payload plus refcount.
- Always initialize refcount to 1.
- Always increment leak counter on bump allocation.
- Reuse free lists only for validated size classes.
- Implement release-to-zero destructors for child fields.
- Add typed field metadata to fixed block release.

Exit criteria:

- tuple/record/sum/closure leak-check tests pass on ARM64 and x64.
- payload sizes 8, 16, 24, 32, 96, and >248 are covered.

### Phase 3: Fix String And Bytes Layouts

Work:

- Canonicalize variable buffer layout as
  `[length][data][padding][refcount]`.
- Update stdlib `String.dark` and `Bytes.dark` builders to use aligned
  refcount offsets.
- Add `DynamicString` and `DynamicBytes` retain/release operations.
- Insert automatic decrefs for `TString` and `TBytes` dynamic values.
- Decide how literals are represented:
  - static non-owned `StringSymbol`, or
  - owned heap materialization at explicit boundaries

Exit criteria:

- string concat, slice, fromCodepoints, file read strings, float-to-string, and
  bytes create/set/fromList all reclaim or intentionally remain static.

### Phase 4: Make Lists Fully Typed-RC

Work:

- Keep tagged FingerTree nodes, but define them as managed shapes.
- Make all list edge stores typed.
- Ensure raw edge ownership increment is active on both backends.
- Enable x64 list retain/release after fixing raw edge increments and leak
  accounting.
- Release LEAF payloads according to element shape.
- Remove the `TList<TFunction> -> GenericHeap` special case unless a concrete
  representation reason exists.

Exit criteria:

- list of primitives, list of strings, list of bytes, list of records, list of
  closures, nested lists, and list of dicts are covered.
- list operations that rebuild/share nodes do not double-free and do not leak.

### Phase 5: Convert Dict/HAMT To Managed Nodes

Work:

- Add refcounts to HAMT leaf/internal/collision nodes.
- Retain shared children during persistent updates.
- Release old nodes recursively on zero.
- Release key/value payloads in leaves and collision nodes.
- Add typed dict raw edge operations or replace raw node construction with
  compiler intrinsics.

Exit criteria:

- dict set/remove/map/filter/merge/fromList reclaim unreferenced old nodes.
- dict keys/values of every RC-managed shape are covered.

### Phase 6: Make Closures First-Class Managed Values

Work:

- Represent static function references separately from closures.
- Track closure capture shapes.
- Retain captures when building closures.
- Release captures when closure dies.
- Ensure closure call borrows the closure and its captures.

Exit criteria:

- capturing and non-capturing lambdas behave differently only where their
  representations differ.
- closures stored in tuples/lists/dicts are retained and released correctly.

### Phase 7: Replace Ad Hoc Borrowing With Ownership Annotations

Work:

- Annotate IR operations with owned/borrowed result behavior.
- Annotate function signatures or lowered call sites with return ownership.
- Keep local alias borrowing, but do not infer ownership from source names or
  incomplete type guesses.

Exit criteria:

- borrowed projection return materialization is systematic.
- call results are consistently owned unless marked borrowed.
- unresolved types fail loudly where ownership cannot be determined.

### Phase 8: Make Leak Checking Trustworthy

Work:

- Count every dynamic allocation exactly once.
- Decrement only on release-to-zero.
- Include shape name and size class in optional verbose leak diagnostics.
- Add tests that intentionally leak each shape, and tests that release each
  shape.

Exit criteria:

- leak-check is reliable on both ARM64 and x64.
- “no leak report” means no counted dynamic heap allocation remains live.

### Phase 9: Optimize RC After Correctness

Only after all shapes are correct:

- elide retain/release pairs around local unique values
- batch RC operations in loops
- specialize common fixed-block destructors
- optimize string/bytes temporary ownership
- consider uniqueness analysis for persistent structure updates

## Recommended Test Matrix

Add end-to-end leak-check tests in `src/Tests/e2e/` for:

- tuple arity 1, 2, 3, 4, 12, and large >248 byte payload
- record with primitive fields
- record with string/list/dict/closure fields
- boxed sum with primitive payload
- boxed sum with heap payload
- pure enum sum with no payload
- list literal of primitive values
- list literal of strings
- list literal of records
- list operations: push, pushBack, append, tail, init, map, filter, flatten
- string concat, slice, fromCodepoints, float-to-string
- bytes create, set, fromList, toList
- dict singleton, set overwrite, remove, map, filter, merge, fromList
- closure discard, closure return, closure stored in tuple/list/dict
- file read/write result allocations
- pattern matching that returns borrowed payloads
- tail calls with RC cleanup and overlapping arguments

Each test should assert both functional output and leak-check output.

## Final Assessment

Reference counting exists in the compiler, but it is not yet the compiler's
universal memory management system.

The most complete path today is ARM64 fixed-block/list root reclamation. Even
there, recursive release of child fields and dynamic string/bytes/dict handling
are incomplete. On x64, generic and list RC operations are intentionally disabled
and should be treated as non-functional until allocation layout, edge ownership,
and leak accounting are fixed together.

The correct unification path is to stop asking “is this source type heap-like?”
and instead attach a runtime ownership shape to every value that can cross an
ownership boundary. Retain, release, allocation, edge stores, destructors, and
leak checking should all use that one shape system.
