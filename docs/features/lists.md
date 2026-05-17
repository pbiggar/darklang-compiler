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
typed `RawSet`. The optional type on `RawSet` is important because it lets
backend code retain managed edges, especially list and dict roots.

## Reference Counting

Lists have specialized backend helpers rather than using generic fixed-block
RC:

- root increments retain the tagged root node
- decrements traverse list nodes iteratively and free nodes whose refcount
  reaches zero
- typed `RawSet` retains child list and dict edges
- selected leaf payload helpers release managed element payloads

Current ARM64 leaf payload coverage includes:

- primitive payloads, which need no payload release
- dynamic strings and bytes
- nested lists
- dict roots
- closure roots
- tuple payloads, including selected higher-arity managed-field shapes
- one-field, two-field, nested, and selected three-field records
- boxed sums carrying dynamic buffers, lists, dicts, closures, tuples, records,
  and selected nested sums

x64 has root/node reclamation plus focused helper coverage for dynamic buffers,
tuples, records, boxed sums, lists, dicts, and closures. It still has less
end-to-end coverage than ARM64 and should be checked against
[`docs/x64-refcounting.md`](../x64-refcounting.md) before assuming parity for a
new shape.

## Remaining Work

List memory management still needs to move from per-shape helper variants to a
shape-driven payload release plan. Remaining gaps include:

- three-element tuples with managed fields
- additional multi-field record shapes
- sum payloads beyond currently covered dynamic-buffer, root, tuple, and record
  payload shapes
- bytes/list/dict/closure combinations nested more deeply
- x64 parity for ARM64 helper variants
- avoiding unbounded helper growth as new shapes are covered

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
