# Lists

Dark lists are immutable sequences represented as direct-payload skew-binary
random-access lists. The representation is persistent and uses reference
counting for every shared root, structural edge, and managed element payload.

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

The empty list is `0`. A non-empty root is a short spine of digit nodes. Each
digit points to one complete skew-binary tree and the remaining digits.

| Tag | Node | Fields before refcount | Allocation |
|---|---|---|---|
| 1 | digit | weight, suffix length, tree, rest | 40 bytes |
| 2 | leaf | element | 16 bytes |
| 3 | internal tree | element, left, right | 32 bytes |

The low three pointer bits hold the tag. Every node has a trailing refcount.
Elements are stored directly in both leaves and internal tree nodes, avoiding a
separate wrapper allocation per internal value. The cached suffix length makes
`length` constant time.

The skew invariant permits at most two leading trees of the same weight. A
prepend combines two equal leading trees with the new value as their parent;
otherwise it adds a weight-one tree. Removing the head either drops a
weight-one digit or splits one complete tree into two child digits.

## Complexity

| Operation | Complexity |
|---|---|
| `push`, `head`, `tail`, `length`, `isEmpty` | O(1) worst case |
| `getAt`, `setAt`, `last` | O(log n) |
| `map`, `filter`, `reverse`, `append` | O(n) |
| `pushBack`, `dropLast` | O(n) |

`map` preserves the forest shape and allocates one output tree node per element
plus one digit per source digit. Traversal-oriented operations use repeated
head/tail with O(1) prepend accumulators and reverse once where order requires
it. Workloads that repeatedly append at the right should instead build in
reverse with `push`.

## Lowering and Reference Counting

List literals are built directly in
`src/DarkCompiler/passes/2_AST_to_ANF.fs`; they do not call a sequence of public
list functions. Metadata uses `RawWriteWord`, while element and child edges use
typed `RawSlotInit<T>` so the backends retain managed ownership.

ARM64 and x64 use specialized iterative list-release helpers. When a node's
refcount reaches zero, the helper releases its direct element payload according
to `RcReleasePlan`, schedules its structural children, and returns the raw block
to the allocator's size-class free list. This covers dynamic strings and bytes,
nested lists and dicts, closures, tuples, records, and boxed sums without a
garbage collector.

## Implementation Files

| File | Purpose |
|---|---|
| `src/DarkCompiler/stdlib/__SkewList.dark` | representation and primitive operations |
| `src/DarkCompiler/stdlib/List.dark` | public list functions |
| `src/DarkCompiler/passes/2_AST_to_ANF.fs` | literal and pattern lowering |
| `src/DarkCompiler/passes/2.5_RefCountInsertion.fs` | list lifetime insertion |
| `src/DarkCompiler/passes/arm64/6_CodeGen.fs` | ARM64 ownership helpers |
| `src/DarkCompiler/passes/x64/6_CodeGen.fs` | x64 ownership helpers |

See [`memory-refcounting-remaining.md`](../../memory-refcounting-remaining.md)
for the current memory-management task breakdown.
