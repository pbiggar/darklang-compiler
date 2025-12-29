# Lists

This document describes how the Dark compiler handles list types.

## Overview

Lists are immutable, singly-linked sequences. They use cons-cell representation
and support pattern matching on head/tail.

## Syntax

### Construction
```dark
[]                    // Empty list
[1, 2, 3]             // List literal
[1, ...rest]          // Cons (prepend)
Stdlib.List.push(list, 0)  // Prepend element
```

### Pattern Matching
```dark
match list with
| [] -> "empty"
| [h, ...t] -> "has elements"
| [a, b] -> "exactly two"
```

## Runtime Representation

Lists use a cons-cell structure:

### Empty List (Nil)
```
0  (just the integer zero)
```

### Non-Empty List (Cons)
```
[tag=1, head, tail]  -- heap-allocated tuple
```

Memory layout for cons cell:
```
Offset 0:   Tag = 1 (8 bytes)
Offset 8:   Head element (8 bytes)
Offset 16:  Tail pointer (8 bytes)
Offset 24:  Refcount (8 bytes)
```

### Example
```dark
[1, 2, 3]
```

Represents as:
```
Cons(1, Cons(2, Cons(3, Nil)))
→ [1, 1, Cons2]  where Cons2 = [1, 2, Cons3]  where Cons3 = [1, 3, 0]
```

## Type System

```fsharp
TList of Type  // List<T> - polymorphic list type
```

Lists are generic; `List<Int64>` and `List<String>` are distinct types.

## Compilation

### List Literal

```dark
[1, 2, 3]
```

Compiles to nested cons allocations (right to left):
```
let t3 = TupleAlloc [1, 3, 0] in      // [1, 3, nil]
let t2 = TupleAlloc [1, 2, t3] in     // [1, 2, cons3]
let t1 = TupleAlloc [1, 1, t2] in     // [1, 1, cons2]
t1
```

### Cons Expression

```dark
[x, ...rest]
```

Compiles to:
```
TupleAlloc [1, x, rest]
```

### Empty Check

```dark
match list with
| [] -> ...
```

Compiles to:
```
if list == 0 then ...  // Nil is represented as 0
```

### Head/Tail Extraction

```dark
match list with
| [h, ...t] -> h + sumList(t)
```

Compiles to:
```
if list != 0 then
    let h = TupleGet(list, 1) in    // Head at offset 1
    let t = TupleGet(list, 2) in    // Tail at offset 2
    ...
```

## Stdlib.List Functions

Defined in `stdlib.dark:222-350`:

| Function | Type | Description |
|----------|------|-------------|
| `isEmpty` | `List<T> -> Bool` | Check if empty |
| `length` | `List<T> -> Int64` | Count elements |
| `push` | `(List<T>, T) -> List<T>` | Prepend element |
| `map` | `(List<A>, A -> B) -> List<B>` | Transform elements |
| `filter` | `(List<A>, A -> Bool) -> List<A>` | Keep matching |
| `fold` | `(List<A>, B, (B,A) -> B) -> B` | Left fold |
| `append` | `(List<A>, List<A>) -> List<A>` | Concatenate |
| `reverse` | `List<A> -> List<A>` | Reverse order |
| `head` | `List<A> -> Option<A>` | First element |
| `tail` | `List<A> -> Option<List<A>>` | Rest of list |
| `getAt` | `(List<A>, Int64) -> Option<A>` | Element at index |

## Pattern Matching Details

### Exact Length
```dark
| [a, b, c] -> ...  // Matches exactly 3 elements
```

Checks: `list != nil && list.tail != nil && list.tail.tail != nil && list.tail.tail.tail == nil`

### Variable Length
```dark
| [h, ...t] -> ...  // Matches 1+ elements
```

Checks: `list != nil`

### Empty
```dark
| [] -> ...  // Matches empty list
```

Checks: `list == nil`

## Reference Counting

Each cons cell is reference-counted:

- Creating cons increments refcount of tail
- List going out of scope decrements each cell
- Cells freed when refcount reaches zero
- Structural sharing: multiple lists can share tails

## Monomorphization

Generic list functions are monomorphized:

```dark
Stdlib.List.map<Int64, String>(nums, toString)
```

Creates specialized `Stdlib.List.map_i64_String`.

## Implementation Files

| File | Purpose |
|------|---------|
| `AST.fs:43` | TList type |
| `AST.fs:92-93` | PList, PListCons patterns |
| `AST.fs:130-131` | ListLiteral, ListCons expressions |
| `stdlib.dark:222-350` | List stdlib functions |
| `2_AST_to_ANF.fs` | List compilation |
