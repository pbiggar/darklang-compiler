# Sum Types (ADTs)

This document describes how the Dark compiler handles algebraic data types
(sum types / discriminated unions).

## Overview

Sum types allow defining types with multiple variants, each optionally
carrying a payload. They are the foundation for Option, Result, and
user-defined types.

## Syntax

### Definition
```dark
type Color = Red | Green | Blue

type Option<T> = Some of T | None

type Result<T, E> = Ok of T | Error of E
```

### Construction
```dark
let c = Red
let opt = Some(42)
let res = Ok("success")
```

### Pattern Matching
```dark
match opt with
| Some(x) -> x
| None -> 0
```

## Runtime Representation

Sum types use a tag-based representation:

### Variants Without Payload
```
Red    → 0  (just the tag value)
Green  → 1
Blue   → 2
```

### Variants With Payload
```
Some(42) → [tag=0, payload=42]  (heap-allocated tuple)
None     → 1                     (just the tag value)
```

Memory layout for variants with payloads:
```
Offset 0:  Tag (8 bytes)
Offset 8:  Payload (8+ bytes depending on type)
Offset N:  Refcount (8 bytes)
```

## Tag Assignment

Tags are assigned in definition order starting from 0:

```dark
type Traffic = Red | Yellow | Green
// Red = 0, Yellow = 1, Green = 2
```

The variant lookup maps names to tags:
```fsharp
type VariantLookup = Map<string, (typeName * typeParams * tagIndex * payloadType)>
```

## Compilation

### Constructor Compilation

In `2_AST_to_ANF.fs`, `AST.Constructor` compiles to:

**Without payload:**
```fsharp
| AST.Constructor (_, variantName, None) ->
    // Just return the tag value
    Let (tempId, Atom (IntLiteral tag), ...)
```

**With payload:**
```fsharp
| AST.Constructor (_, variantName, Some payload) ->
    // Allocate tuple: [tag, payload]
    Let (tupleId, TupleAlloc [IntLiteral tag, payloadAtom], ...)
```

### Pattern Matching Compilation

Matching on sum types:
1. Load tag from value (or use value directly if no payload)
2. Compare tag against variant's tag number
3. If match, extract payload and bind variables
4. Otherwise, try next pattern

## Polymorphic Sum Types

Generic sum types like `Option<T>` are monomorphized:

```dark
let x: Option<Int64> = Some(42)
let y: Option<String> = Some("hi")
```

Creates specialized types `Option_i64` and `Option_String` with
concrete payload types.

## Built-in Sum Types

### Option
```dark
type Stdlib.Option.Option<T> = Some of T | None
```
Used for values that may or may not exist.

### Result
```dark
type Stdlib.Result.Result<T, E> = Ok of T | Error of E
```
Used for computations that may fail.

## Code Generation

In `6_CodeGen.fs`:

**Checking tags:**
```assembly
LDR X1, [X0]        ; Load tag from tuple
CMP X1, #0          ; Compare to expected tag
B.NE else_label     ; Branch if not equal
```

**Extracting payloads:**
```assembly
LDR X1, [X0, #8]    ; Load payload from offset 8
```

## Reference Counting

Sum type values with payloads are heap-allocated and reference-counted:

- Increment when assigning to new variable
- Decrement when value goes out of scope
- Free when refcount reaches zero

Variants without payloads are just integers (no allocation).

## Implementation Files

| File | Purpose |
|------|---------|
| `AST.fs:42` | TSum type definition |
| `AST.fs:128` | Constructor expression |
| `AST.fs:85` | PConstructor pattern |
| `1.5_TypeChecking.fs` | SumTypeRegistry, VariantLookup |
| `2_AST_to_ANF.fs` | Constructor and match compilation |
| `6_CodeGen.fs` | Runtime representation |

## Tests

See `src/Tests/e2e/adt.e2e` for sum type tests.
