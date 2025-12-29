# Dict (HAMT Implementation)

This document describes the Hash Array Mapped Trie (HAMT) implementation used
for Dark's immutable dictionaries.

## Overview

`Dict<K, V>` is an immutable hash map implemented as a HAMT. It provides
efficient O(log32 n) operations with structural sharing for immutability.

## What is a HAMT?

A Hash Array Mapped Trie (HAMT) is a trie where:
- Keys are hashed to 64-bit integers
- Hash is consumed 5 bits at a time (32-way branching)
- Bitmap compression reduces memory for sparse nodes
- Maximum depth: 13 levels (64 bits / 5 bits per level)

## Node Types

Three node types (distinguished by tag):

### Empty (tag = 0)
```
0  (just the integer zero)
```

### Internal Node (tag = 1)
```
[bitmap, child0, child1, ..., childN]
```
- Bitmap: 64-bit, indicates which of 32 slots have children
- Children: only present slots are stored (bitmap compression)

### Leaf Node (tag = 2)
```
[key, value]
```
- Stores actual key-value pair

## Memory Layout

### Internal Node
```
Offset 0:  Bitmap (8 bytes)
Offset 8:  Child 0 pointer (8 bytes)
Offset 16: Child 1 pointer (8 bytes)
...
```

### Leaf Node
```
Offset 0:  Key (8 bytes)
Offset 8:  Value (8 bytes)
```

## Bitmap Operations

5 bits of hash select position (0-31) in bitmap:

```dark
def hashChunk(hash: Int64, level: Int64) : Int64 =
    (hash >> (level * 5)) & 31

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
2. For each level (0-12):
   - Extract 5-bit chunk from hash
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
3. Current implementation: keys must have distinct hashes

## Raw Memory Intrinsics

HAMT uses low-level memory operations:

```dark
__raw_alloc(size: Int64) : RawPtr
__raw_get<T>(ptr: RawPtr, offset: Int64) : T
__raw_set<T>(ptr: RawPtr, offset: Int64, value: T) : Unit
```

These bypass the normal heap allocator for precise control.

## Tag Encoding

Tags are encoded in the pointer:

```dark
def __getTag<k, v>(dict: Dict<k, v>) : Int64 =
    __dict_to_int64<k, v>(dict) & 3

def __clearTag<k, v>(dict: Dict<k, v>) : RawPtr =
    __int64_to_rawptr(__dict_to_int64<k, v>(dict) & !3)

def __setTag<k, v>(ptr: RawPtr, tag: Int64) : Dict<k, v> =
    __int64_to_dict<k, v>(__rawptr_to_int64(ptr) | tag)
```

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
```

## Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| get | O(log32 n) ≈ O(1) |
| set | O(log32 n) |
| remove | O(log32 n) |
| size | O(n) |
| keys/values | O(n) |

With 32-way branching, depth is ~6 for millions of entries.

## Hash Function

Keys are hashed using `__hash<k>(key)`:
- Int64: identity
- String: FNV-1a hash

## Implementation Details

Located in `stdlib.dark:837-1468` (~630 lines):

| Function | Lines | Purpose |
|----------|-------|---------|
| Hash/bitmap helpers | 845-870 | Bit manipulation |
| Tag helpers | 875-900 | Pointer tagging |
| `__getHelper` | 912-940 | Recursive get |
| `__setHelper` | 949-981 | Recursive set |
| `__expandLeaf` | 987-1017 | Split colliding leaves |
| `__removeHelper` | 1100-1157 | Recursive remove |
| Iteration helpers | 1200-1350 | keys/values/entries/fold |

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
