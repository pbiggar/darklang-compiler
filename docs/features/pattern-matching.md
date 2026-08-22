# Pattern Matching

This document describes how the Dark compiler handles pattern matching in
`match` expressions.

## Overview

Pattern matching allows destructuring and conditional dispatch on values.
The compiler compiles `match` expressions to nested if-else chains with
variable bindings.

## Pattern Types

Defined in `AST.fs`:

```fsharp
type Pattern =
    | PUnit
    | PWildcard
    | PVar of string
    | PConstructor of variantName:string * payload:Pattern option
    | PInt64 of int64
    | PInt128Literal of System.Int128
    | PInt8Literal of sbyte
    | PInt16Literal of int16
    | PInt32Literal of int32
    | PUInt8Literal of byte
    | PUInt16Literal of uint16
    | PUInt32Literal of uint32
    | PUInt64Literal of uint64
    | PUInt128Literal of System.UInt128
    | PBool of bool
    | PString of string
    | PChar of string
    | PFloat of float
    | PTuple of Pattern list
    | PList of Pattern list
    | PListCons of head:Pattern list * tail:Pattern
```

## Match Compilation

Implemented in `2_AST_to_ANF.fs`.

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

### Record Pattern Type Preservation

When `inferType` analyzes a `match` expression, record field bindings must use
the concrete field types from the record definition. Do not default record
fields to `Int64`.

If a string field is inferred as `Int64`, later equality lowering will emit
primitive pointer equality (`==`) instead of `__string_eq`, producing incorrect
results for equal-but-distinct strings.

### Pattern Grouping Type Preservation

Grouped alternatives (`| pat1 | pat2 -> body`) must only contribute bindings
from alternatives that can structurally match the scrutinee type. If an
alternative is impossible (for example tuple arity mismatch), it must not add
fabricated bindings with default types such as `Int64`.

Grouped alternatives are desugared into sequential single-pattern cases during
ANF lowering. Statically impossible alternatives must be dropped before body
compilation; otherwise unreachable branches can still trigger spurious errors.

Record destructuring is not part of the public pattern grammar. Bind the whole
record with a variable pattern and use named access in the guard or body.

List alternatives in grouped patterns follow the same rule. For example,
`match 42 with | 0 | [_] -> ...` must not crash while extracting bindings for
`[_]`, and must treat the list alternative as a non-match for the `Int64`
scrutinee.

### List Pattern
```dark
| [] -> 0
| h :: t -> h
```
Checks the native list length, then extracts the head and canonical tail through
the private skew-list operations.

## Exhaustiveness Checking

The compiler lowers an exhausted match to the standard nonexhaustive-match
runtime failure. Statically invalid pattern/type combinations are rejected
during AOT type checking.

### Exhaustive Patterns
- Wildcard `_` or variable `x` in final position
- All variants of a sum type covered
- All list cases covered (empty + cons)

### Non-Exhaustive Example
```dark
match opt with
| Some(x) -> x   // Runtime failure when the value is None
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

## Interpreter Parity Boundary

The source comparison evidence is compiler
`b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899` and interpreter
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`; the implementation was
revalidated at compiler HEAD `e3d84235c70f01425e4a0f66104ea4a5851d6f6b`.

The public grammar accepts unit, literal, variable, wildcard, tuple, exact
list, cons, constructor, nested, guarded, and grouped-alternative patterns.
Alternatives are parsed as the patterns following one case bar and before its
`when` or `->`; each alternative must bind exactly the same usable names.
Repeated usable names in one pattern are rejected, while names beginning with
`_` are ignored bindings. Record matching binds the whole record (then uses
field access); record destructuring and ellipsis/rest record syntax are not
public patterns.

The scrutinee is evaluated once. Cases, alternatives, and nested structural
tests run left to right. A successful structural match introduces its bindings
only for that arm's guard and body. The Boolean guard runs once after those
bindings are available; a false guard falls through. Only the first eligible
body executes. If no arm succeeds, the runtime failure is
`Non-exhaustive match: No matching case found for value <value> in match expression`.

The compiler deliberately retains one AOT-only timing divergence: all arms are
type checked, including arms which the interpreter would not reach. Invalid
patterns, duplicate bindings, alternative binding-set differences, and
non-Boolean guards therefore diagnose during compilation rather than becoming
runtime decisions. This is an intentional compiler extension in diagnostic
timing, not an additional runtime pattern form.

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
| h :: t -> h   // Matches non-empty list
| [] -> 0
```

1. Check that the cached list length covers every normalized cons head.
2. Extract each head through the typed private skew-list accessor.
3. Advance with the private tail operation and bind or match the canonical
   remaining list.

List-cons lowering requires the scrutinee type to be `TList _`. It must not
silently default to `Int64` when list element type information is missing.
When the scrutinee is inferred from an `if` expression, branch type inference
must reconcile both branches so list element types are preserved for pattern
bindings.

For top-level list patterns, unresolved scrutinee types (`TVar`) must preserve
unresolved element types in ANF lowering (`__list_elem_*`) instead of
defaulting to `Int64`.

For top-level tuple patterns on unresolved scrutinee types (`TVar`), type
checking must preserve per-slot unresolved element types (`__tuple_elem_*`)
when extracting bindings. Dropping those bindings causes false `undefined
variable` errors in match bodies.

Tuple destructuring inside list-cons heads (for example, `(a, b) :: rest`)
uses the tuple element type from the list element type. If tuple arity does not
match (for example `(a, b, c) :: rest` against a list of 2-tuples), the
pattern is treated as a non-match and falls through to later cases. The ANF
lowering does not default missing tuple element types to `Int64`.

When list element type is still unresolved (for example `TVar "t"` from match
inference), tuple head bindings must still be created with unresolved per-slot
types. Dropping those bindings causes false `undefined variable` errors in
branch bodies.

The same rule applies to nested list/list-cons bindings in constructor payloads.
If the payload source type is unresolved (`TVar`) or runtime-error-typed, ANF
lowering must propagate unresolved list element types instead of defaulting to
`Int64`.

The same non-match behavior applies when tuple destructuring appears inside
guarded list patterns (`when ...`): mismatched tuple arity must fall through
instead of binding extra elements as `Int64`.

This also applies to constructor payloads inside list patterns. For example,
`Some((x, y)) :: rest` against `List<Option<Int64>>` is statically
impossible and must compile as a non-match, rather than matching `Some` and
binding tuple payload fields with a default `Int64` type.

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
| `AST.fs` | Pattern type definitions |
| `2_AST_to_ANF.fs` | Match compilation, exhaustiveness, and binding extraction |

## Tests

- `src/Tests/e2e/adts.e2e` - Constructor patterns
- `src/Tests/e2e/tuples.e2e` - Tuple patterns
- `src/Tests/e2e/lists.e2e` - List patterns
