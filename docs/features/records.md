# Records

This document describes how the Dark compiler handles record types.

## Overview

Records are named product types with labeled fields. They provide structured
data with named access, compiled to positional tuples at runtime.

## Syntax

### Definition
```dark
type Point = { x: Int64, y: Int64 }

type Person = { name: String, age: Int64, active: Bool }
```

### Construction
```dark
let p = Point { x = 10, y = 20 }
let person = Person { name = "Alice", age = 30, active = true }
```

### Field Access
```dark
p.x        // 10
person.age // 30
```

### Record Update
```dark
let p2 = { p with x = 100 }  // Point { x = 100, y = 20 }
```

## Runtime Representation

Records are compiled to tuples with fields in definition order:

```dark
type Point = { x: Int64, y: Int64 }
Point { x = 10, y = 20 }
```

Becomes:
```
[10, 20, refcount]  -- heap-allocated tuple
```

Memory layout:
```
Offset 0:   Field 0 (x) - 8 bytes
Offset 8:   Field 1 (y) - 8 bytes
Offset 16:  Refcount - 8 bytes
```

## Type Registry

The type checker maintains a registry of record types:

```fsharp
type TypeRegistry = Map<string, (string * Type) list>
// "Point" -> [("x", TInt64); ("y", TInt64)]
```

This maps record names to ordered lists of (fieldName, fieldType) pairs.

## Compilation

### Record Literal

In `2_AST_to_ANF.fs`, `AST.RecordLiteral` compiles to:

```fsharp
| AST.RecordLiteral (typeName, fields) ->
    // Reorder fields to match definition order
    // Compile each field value
    // Create TupleAlloc with field values
```

Fields in the literal can be in any order; the compiler reorders them
to match the type definition.

### Field Access

`AST.RecordAccess` compiles to `TupleGet`:

```fsharp
| AST.RecordAccess (record, fieldName) ->
    // Look up field index in TypeRegistry
    // Generate TupleGet (record, index)
```

### Record Update

`AST.RecordUpdate` creates a new record with some fields changed:

```fsharp
| AST.RecordUpdate (record, updates) ->
    // Copy unchanged fields from original
    // Replace updated fields with new values
    // Create new TupleAlloc
```

## Pattern Matching

Records can be destructured in patterns:

```dark
match p with
| Point { x = a, y = b } -> a + b
```

Compiles to extracting each field by index:
```
let a = TupleGet(p, 0) in
let b = TupleGet(p, 1) in
a + b
```

## Nested Records

Records can contain other records:

```dark
type Line = { start: Point, end_: Point }

let line = Line { start = Point { x = 0, y = 0 }, end_ = Point { x = 10, y = 10 } }
line.start.x  // 0
```

Each nested record is a separate heap allocation.

## Reference Counting

Records are heap-allocated and reference-counted:

- Construction increments refcount to 1
- Assignment to variable increments refcount
- Going out of scope decrements refcount
- Free when refcount reaches zero

Record updates create new records (immutable semantics).

## Implementation Files

| File | Purpose |
|------|---------|
| `AST.fs:41` | TRecord type |
| `AST.fs:91` | PRecord pattern |
| `AST.fs:125-127` | RecordLiteral, RecordUpdate, RecordAccess |
| `1.5_TypeChecking.fs` | TypeRegistry |
| `2_AST_to_ANF.fs` | Record compilation |

## Differences from Tuples

| Aspect | Records | Tuples |
|--------|---------|--------|
| Access | Named (`.field`) | Positional (`.0`, `.1`) |
| Definition | Explicit type | Inline |
| Reordering | Any order in literal | Must match order |
| Runtime | Same (positional) | Same (positional) |
