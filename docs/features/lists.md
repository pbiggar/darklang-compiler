# Lists

Dark lists are immutable sequences represented as tagged FingerTree nodes, not
as simple cons cells.

## Surface Syntax

```dark
[]                    // empty list
[1, 2, 3]             // list literal
[head, ...tail]       // prepend
Stdlib.List.push(xs, x)
```

Pattern matching supports empty, exact-length, and head/tail forms:

```dark
match xs with
| [] -> "empty"
| [h, ...t] -> "non-empty"
| [a, b] -> "exactly two"
```

## Runtime Representation

The empty list is `0`.

Non-empty lists are tagged pointers. Low pointer bits encode the node kind:

| Tag | Node kind | Payload size |
|---|---|---|
| 1 | single | 8 bytes |
| 2 | deep | 96 bytes |
| 3 | node2 | 24 bytes |
| 4 | node3 | 32 bytes |
| 5 | leaf | 8 bytes |

Each raw node stores its payload followed by a refcount slot. List helper code
clears the low tag bits to find the raw node address, then uses the tag to find
the refcount offset and children.

The exact FingerTree layout is internal to list lowering and backend helpers.
The key ownership rule is that nodes own edges to their child nodes and leaves
own their element payload according to the element representation shape.

## Lowering

List construction happens in `src/DarkCompiler/passes/2_AST_to_ANF.fs`.
Lowering allocates FingerTree nodes with `RawAlloc` and writes fields with
`RawWriteWord` for metadata and `RawSlotInit<T>` for typed payload or child
slots. The type on `RawSlotInit<T>` is important because it lets backend code
retain managed edges, especially list and dict roots.

## Reference Counting

Lists have specialized backend helpers rather than using generic fixed-block
RC:

- root increments retain the tagged root node
- decrements traverse list nodes iteratively and free nodes whose refcount
  reaches zero
- `RawSlotInit<T>` retains child list and dict edges
- direct leaf payload helpers release root element payloads
- generic fixed-block and boxed-sum element payloads use `RcReleasePlan`
  helpers

Current leaf payload coverage includes:

- primitive payloads, which need no payload release
- dynamic strings and bytes
- nested lists
- dict roots
- closure roots
- tuple payloads through `RcReleasePlan` helpers
- record payloads through `RcReleasePlan` helpers
- boxed sums carrying dynamic buffers, lists, dicts, closures, tuples, records,
  and selected nested sums through `RcReleasePlan` helpers

ARM64 and x64 both route generic fixed-block list payload cleanup through
helpers derived from `RcReleasePlan`. x64 no longer has a static
tuple/record/boxed-sum helper matrix for list payload cleanup.

## Remaining Work

List memory management should stay on the shape-driven payload release path.
Remaining gaps are mostly around:

- adding focused tests when new recursive payload families are introduced
- extending the generic `RcReleasePlan` executor if a new tuple/record/sum
  shape exposes an unsupported release-plan case
- keeping ARM64 and x64 helper dependency discovery in parity
- avoiding new per-shape helper matrices

See [`memory-refcounting-remaining.md`](../../memory-refcounting-remaining.md)
for the current task breakdown.

## Implementation Files

| File | Purpose |
|---|---|
| `src/DarkCompiler/AST.fs` | `TList`, list literals, list patterns |
| `src/DarkCompiler/passes/2_AST_to_ANF.fs` | list lowering |
| `src/DarkCompiler/passes/2.5_RefCountInsertion.fs` | list lifetime insertion |
| `src/DarkCompiler/passes/arm64/6_CodeGen.fs` | ARM64 list helpers |
| `src/DarkCompiler/passes/x64/6_CodeGen.fs` | x64 list helpers |
| `src/DarkCompiler/stdlib/List.dark` | stdlib list functions |
