# Dict (HAMT Implementation)

This document describes the Hash Array Mapped Trie (HAMT) implementation used
for Dark's immutable dictionaries.

## Overview

`Dict<K, V>` is an immutable hash map implemented as a HAMT. It provides
efficient O(log64 n) operations with structural sharing for immutability.

## What is a HAMT?

A Hash Array Mapped Trie (HAMT) is a trie where:
- Keys are hashed to 64-bit integers
- Hash is consumed 6 bits at a time (64-way branching)
- Bitmap compression reduces memory for sparse nodes
- Maximum depth: 11 levels (64 bits / 6 bits per level, rounded up)

## Node Types

Four tags are used. Three of them are allocated node layouts; empty is the
null tagged root.

### Empty (tag = 0)
```
0  (just the integer zero)
```

### Internal Node (tag = 1)
```
[bitmap, child0, child1, ..., childN, refcount]
```
- Bitmap: 64-bit, indicates which of 64 slots have children
- Children: only present slots are stored (bitmap compression)
- Refcount: trailing 64-bit node reference count

### Leaf Node (tag = 2)
```
[key, value, refcount]
```
- Stores actual key-value pair
- Refcount: trailing 64-bit node reference count

### Collision Node (tag = 3)
```
[count, key0, value0, key1, value1, ..., refcount]
```
- Stores entries whose hashes collide through the entire trie depth
- Refcount: trailing 64-bit node reference count

## Memory Layout

### Internal Node
```
Offset 0:  Bitmap (8 bytes)
Offset 8:  Child 0 pointer (8 bytes)
Offset 16: Child 1 pointer (8 bytes)
...
Offset 8 + childCount * 8: Refcount (8 bytes)
```

### Leaf Node
```
Offset 0:  Key (8 bytes)
Offset 8:  Value (8 bytes)
Offset 16: Refcount (8 bytes)
```

### Collision Node
```
Offset 0:  Count (8 bytes)
Offset 8:  Key 0 (8 bytes)
Offset 16: Value 0 (8 bytes)
Offset 24: Key 1 (8 bytes)
Offset 32: Value 1 (8 bytes)
...
Offset 8 + count * 16: Refcount (8 bytes)
```

## Bitmap Operations

6 bits of hash select position (0-63) in bitmap:

```dark
def hashChunk(hash: Int64, level: Int64) : Int64 =
    (hash >> (level * 6)) & 63

def hasBit(bitmap: Int64, bit: Int64) : Bool =
    (bitmap & (1 << bit)) != 0

def childIndex(bitmap: Int64, bit: Int64) : Int64 =
    Stdlib.Int64.popcount(bitmap & ((1 << bit) - 1))

def setBit(bitmap: Int64, bit: Int64) : Int64 =
    bitmap | (1 << bit)

def clearBit(bitmap: Int64, bit: Int64) : Int64 =
    bitmap & !(1 << bit)
```

## Key Operations

### Get

```dark
def get<k, v>(dict: Dict<k, v>, key: k) : Option<v>
```

Algorithm:
1. Compute hash of key
2. For each level (0-10):
   - Extract 6-bit chunk from hash
   - Check if bitmap has that bit set
   - If yes, follow child pointer
   - If no, key not present
3. At leaf: compare keys, return value if match

### Set

```dark
def set<k, v>(dict: Dict<k, v>, key: k, value: v) : Dict<k, v>
```

Algorithm:
1. Compute hash of key
2. Navigate to insertion point
3. If empty: create leaf
4. If leaf with same key: replace value
5. If leaf with different key: expand to internal node
6. If internal: recurse into appropriate child
7. Create new nodes along path (structural sharing)

### Remove

```dark
def remove<k, v>(dict: Dict<k, v>, key: k) : Dict<k, v>
```

Algorithm:
1. Navigate to key
2. If not found: return unchanged
3. Remove leaf, propagate changes up
4. Collapse single-child internals to leaves
5. Create new nodes along path

## Structural Sharing

HAMT modifications share unmodified subtrees:

```
Before set(dict, "x", 1):
     [root]
    /      \
  [A]      [B]
  / \      / \
[1] [2]  [3] [4]

After set (modified path):
     [root']    ← new
    /      \
  [A']     [B]  ← shared
  / \      / \
[1'] [2] [3] [4]  ← [1'] is new, [2] shared
```

## Collision Handling

When two keys hash to the same value:
1. Continue descending until hashes differ
2. If all 64 bits match (extremely rare), collision node needed
3. Collision nodes store the colliding key/value entries inline and are copied
   on update/remove like other HAMT nodes

## Raw Memory Intrinsics

HAMT uses low-level memory operations:

```dark
__raw_alloc(size: Int64) : RawPtr
__raw_get<T>(ptr: RawPtr, offset: Int64) : T
__raw_write_word(ptr: RawPtr, offset: Int64, value: Int64) : Unit
__raw_write_byte(ptr: RawPtr, offset: Int64, value: Int64) : Unit
__raw_slot_init<T>(ptr: RawPtr, offset: Int64, value: T) : Unit
```

These bypass the normal value allocator for precise layout control. Typed slots
must be initialized with `__raw_slot_init<T>` rather than `__raw_write_word` so
the compiler/backend can retain copied managed edges.

## Reference Counting

`Dict<K, V>` roots are compiler-managed values whose raw HAMT nodes are also
refcounted. Nodes are not uniquely owned and updates do not deep-copy the whole
tree. Instead, `set`, `remove`, `map`, and collision updates path-copy the
changed nodes and structurally share the untouched subtrees.

The ownership invariant is:

- every live dict root owns one reference to its tagged root node
- every internal-node child slot owns one reference to the child node stored in
  that slot
- every leaf or collision key/value slot owns one reference to each managed
  key or value stored in that slot
- `__raw_get<T>` returns a borrowed value
- `__raw_slot_init<T>` is the edge-creation operation and retains managed
  roots stored into raw nodes or other raw-backed containers
- `__raw_write_word` and `__raw_write_byte` write unmanaged metadata/bits only;
  they do not create ownership edges

Dict root retain increments only the root node's trailing node refcount. Dict
root release decrements the root node refcount; only when it reaches zero does
the helper recursively release child nodes and leaf/collision payload edges.
This is what makes structural sharing safe: when a new internal node copies an
unchanged child pointer from an old internal node,
`__raw_slot_init<Dict<k, v>>` retains that child. Releasing either old or new root then
removes only that root's edge. The shared child remains live until all parent
edges and root edges have been released.

For current Dark hashable key semantics, managed keys are dynamic strings and
bytes. Dict release helpers handle primitive/no-release keys, dynamic
string/bytes keys, and the covered managed value shapes through
`RcReleasePlan`-selected helpers: dynamic buffers, lists, nested dicts,
closures, tuples, records, and boxed sums. If the language later adds new
managed hashable key families, dict release helpers need symmetric key-release
support for those shapes.

Raw HAMT memory is reclaimed through dict refcount helpers and backend
allocation/free-list accounting for supported raw-node size classes. General
manual `RawFree` policy remains separate from this managed HAMT lifecycle.

## Tag Encoding

Tags are encoded in the pointer:

```dark
def __getTag<k, v>(dict: Dict<k, v>) : Int64 =
    __dict_get_tag<k, v>(dict)

def __clearTag<k, v>(dict: Dict<k, v>) : RawPtr =
    __dict_to_rawptr<k, v>(dict)

def __setTag<k, v>(ptr: RawPtr, tag: Int64) : Dict<k, v> =
    __rawptr_to_dict<k, v>(ptr, tag)
```

`__dict_to_rawptr` returns a borrowed raw view of the tagged dict pointer with
tag bits cleared. It does not create an `Int64` round trip and does not transfer
ownership. `__rawptr_to_dict` retags an initialized raw HAMT node as a managed
`Dict<k, v>` root; refcount insertion treats the resulting dict value as owned
when it is bound like any other managed value.

## Stdlib.Dict API

```dark
def empty<k, v>() : Dict<k, v>
def get<k, v>(dict: Dict<k, v>, key: k) : Option<v>
def set<k, v>(dict: Dict<k, v>, key: k, value: v) : Dict<k, v>
def remove<k, v>(dict: Dict<k, v>, key: k) : Dict<k, v>
def contains<k, v>(dict: Dict<k, v>, key: k) : Bool
def isEmpty<k, v>(dict: Dict<k, v>) : Bool
def size<k, v>(dict: Dict<k, v>) : Int64
def keys<k, v>(dict: Dict<k, v>) : List<k>
def values<k, v>(dict: Dict<k, v>) : List<v>
def entries<k, v>(dict: Dict<k, v>) : List<(k, v)>
def fold<k, v, a>(dict: Dict<k, v>, init: a, f: (a, k, v) -> a) : a
def map<k, v, w>(dict: Dict<k, v>, f: (k, v) -> w) : Dict<k, w>
def filter<k, v>(dict: Dict<k, v>, f: (k, v) -> Bool) : Dict<k, v>
def merge<k, v>(dict1: Dict<k, v>, dict2: Dict<k, v>) : Dict<k, v>
def fromList<k, v>(pairs: List<(k, v)>) : Dict<k, v>
def getOrDefault<k, v>(dict: Dict<k, v>, key: k, default: v) : v
def singleton<k, v>(key: k, value: v) : Dict<k, v>
def toList<k, v>(dict: Dict<k, v>) : List<(k, v)>
```

## Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| get | O(log64 n), effectively constant for normal sizes |
| set | O(log64 n) |
| remove | O(log64 n) |
| size | O(n) |
| keys/values | O(n) |

With 64-way branching, depth is small for normal program dictionaries.

## Hash Function

Keys are hashed using `__hash<k>(key)`. Current stdlib hash/equality
specializations cover:
- Int64: identity
- Bool: boolean identity
- String: FNV-1a over string data
- Bytes: FNV-1a over bytes data

## Implementation Details

Located in `src/DarkCompiler/stdlib/__HAMT.dark`:

| Function | Lines | Purpose |
|----------|-------|---------|
| Hash/bitmap helpers | top of file | Bit manipulation |
| Tag helpers | top of file | Pointer tagging |
| `__allocLeaf` | allocation section | Leaf allocation |
| `__allocInternal` | allocation section | Internal-node allocation |
| `__allocCollision` | collision section | Collision-node allocation |
| `__getHelper` | dict helpers | Recursive get |
| `__setHelper` | dict helpers | Recursive set |
| `__removeHelper` | dict helpers | Recursive remove |
| Iteration helpers | dict helpers | keys/values/entries/fold |

## Example

```dark
let d = Stdlib.Dict.empty<String, Int64>()
let d = Stdlib.Dict.set(d, "a", 1)
let d = Stdlib.Dict.set(d, "b", 2)

Stdlib.Dict.get(d, "a")  // Some(1)
Stdlib.Dict.get(d, "c")  // None
Stdlib.Dict.size(d)      // 2
```

## Why HAMT?

- **Immutable**: Safe for functional programming
- **Efficient sharing**: Modifications reuse most structure
- **Fast operations**: Near-constant time
- **Simple implementation**: Easier than red-black trees
