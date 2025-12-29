# Pattern Matching

This document describes how the Dark compiler handles pattern matching in
`match` expressions.

## Overview

Pattern matching allows destructuring and conditional dispatch on values.
The compiler compiles `match` expressions to nested if-else chains with
variable bindings.

## Pattern Types

Defined in `AST.fs:80-93`:

```fsharp
type Pattern =
    | PUnit                    // () - matches unit value
    | PWildcard                // _ - matches anything
    | PVar of string           // x - binds value to variable
    | PConstructor of string * Pattern option  // Some(x), None
    | PLiteral of int64        // 42 - exact integer match
    | PBool of bool            // true, false
    | PString of string        // "hello"
    | PFloat of float          // 3.14
    | PTuple of Pattern list   // (a, b, c)
    | PRecord of string * (string * Pattern) list  // { x = a, y = b }
    | PList of Pattern list    // [a, b, c] - exact length
    | PListCons of Pattern list * Pattern  // [a, b, ...rest]
```

## Match Compilation

Implemented in `2_AST_to_ANF.fs:2303+`.

### Algorithm

1. **Evaluate scrutinee**: Compile the expression being matched
2. **For each case**: Generate condition check and body
3. **Pattern bindings**: Extract variables from pattern into scope
4. **Fall-through**: Try next pattern if current doesn't match

### Example Compilation

```dark
match x with
| 0 -> "zero"
| n -> "other"
```

Compiles to:
```
let scrutinee = x in
if scrutinee == 0 then
    "zero"
else
    let n = scrutinee in
    "other"
```

## Binding Extraction

The `extractAndCompileBody` function recursively extracts variable bindings:

### Variable Pattern
```dark
| x -> x + 1
```
Binds `x` to the scrutinee value.

### Tuple Pattern
```dark
| (a, b) -> a + b
```
Binds `a` to `scrutinee.0`, `b` to `scrutinee.1`.

### Constructor Pattern
```dark
| Some(x) -> x
| None -> 0
```
Checks tag, then extracts payload if present.

### List Pattern
```dark
| [] -> 0
| [h, ...t] -> h
```
Checks for nil (tag=0) or cons (tag=1), extracts head/tail.

## Exhaustiveness Checking

The compiler rejects non-exhaustive patterns at compile time.

### Exhaustive Patterns
- Wildcard `_` or variable `x` in final position
- All variants of a sum type covered
- All list cases covered (empty + cons)

### Non-Exhaustive Example
```dark
match opt with
| Some(x) -> x   // Error: non-exhaustive, missing None
```

The exhaustiveness check is implemented via `patternAlwaysMatches`:

```fsharp
let rec patternAlwaysMatches (pattern: AST.Pattern) : bool =
    match pattern with
    | PWildcard | PVar _ -> true
    | PTuple patterns -> List.forall patternAlwaysMatches patterns
    | _ -> false
```

## Guard Clauses

Patterns can have optional `when` guards:

```dark
match n with
| x when x > 0 -> "positive"
| x when x < 0 -> "negative"
| _ -> "zero"
```

Guards are evaluated after pattern match but before body execution.

## Tuple Pattern Compilation

```dark
match t with
| (a, b, c) -> a + b + c
```

1. Bind `a = TupleGet(t, 0)`
2. Bind `b = TupleGet(t, 1)`
3. Bind `c = TupleGet(t, 2)`
4. Compile body with extended environment

## List Pattern Compilation

### Exact List Pattern
```dark
match list with
| [a, b] -> a + b  // Matches exactly 2-element list
```

Checks length, then extracts each element.

### Cons Pattern
```dark
match list with
| [h, ...t] -> h   // Matches non-empty list
| [] -> 0
```

1. Check if list is cons (tag == 1)
2. Extract head: `HeapLoad(list, 8)`
3. Extract tail: `HeapLoad(list, 16)`

## Constructor Pattern Compilation

```dark
type Color = Red | Green | Blue of Int64

match c with
| Red -> 0
| Green -> 1
| Blue(n) -> n
```

1. Load tag from `HeapLoad(c, 0)`
2. Compare tag to variant's tag number
3. If match with payload, extract: `HeapLoad(c, 8)`

## Implementation Files

| File | Purpose |
|------|---------|
| `AST.fs:80-93` | Pattern type definitions |
| `2_AST_to_ANF.fs:2303-2600` | Match compilation |
| `2_AST_to_ANF.fs:2336-2347` | `patternAlwaysMatches` exhaustiveness |
| `2_AST_to_ANF.fs:2347-2600` | `extractAndCompileBody` binding extraction |

## Tests

- `src/Tests/e2e/adt.e2e` - Constructor patterns
- `src/Tests/e2e/tuple.e2e` - Tuple patterns
- `src/Tests/e2e/list.e2e` - List patterns
