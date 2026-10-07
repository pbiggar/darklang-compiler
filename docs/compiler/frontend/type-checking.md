# Type Checking

This document describes the type checking pass in the Dark compiler.

## Overview

The Dark compiler uses **top-down type checking** with targeted local inference
for generic call sites. Function parameters and return types require explicit
annotations; let bindings have optional annotations.

## Design Philosophy

- **Explicit over implicit**: Type annotations required at function boundaries
- **Simple implementation**: Local type-variable unification for generics, not
  global constraint solving
- **Fast compilation**: Single-pass, no iteration to fixed point for types

## Type representations

Source type syntax is defined in
`src/frontend/interpreter/WrittenTypes.ml`. Resolved semantic
types are defined in `src/AST.ml`:

```ocaml
type semanticType =
  | TInt8
  | TInt16
  | TInt32
  | TInt64
  | TInt128
  | TInt
  | TUInt8
  | TUInt16
  | TUInt32
  | TUInt64
  | TUInt128
  | TBool
  | TFloat64
  | TString
  | TBlob
  | TChar
  | TDateTime
  | TUnit
  | TNever
  | TFunction of semanticType list * semanticType
  | TTuple of semanticType list
  | TRecord of string * semanticType list
  | TSum of string * semanticType list
  | TList of semanticType
  | TStream of semanticType
  | TVar of string
  | TInferenceVar of string * string
  | TInternalRawPtr
  | TDict of semanticType * semanticType
```

The interpreter parser returns `WrittenTypes`; `WrittenChecking` resolves
source annotations and names directly into `CheckedAST.Program`. `TNever` is
a semantic bottom type and cannot occur in written source. `TInternalRawPtr` is an
internal-signature capability: public parsing rejects `RawPtr`, while
privileged compiler sources use it for the unsafe runtime-support layer.

`Blob` is the sole binary type. Blob equality is admitted as handle identity;
there is no `Bytes` type or function namespace.

`Stream<'a>` is opaque: it has no constructors or patterns, is never traversed
by equality, and renders without forcing. Equality and inequality compare
handle identity; ordering is rejected during AOT type checking.

## Type Registries

The type checker maintains several registries:

### TypeEnv
Maps variable names to types:
```ocaml
type typeEnv = AST.semanticType StringOrder.Map.t
```

### TypeRegistry
Maps record type names to field definitions:
```ocaml
type typeRegistry = (string * AST.semanticType) list StringOrder.Map.t
```

### SumTypeRegistry
Maps sum type names to variants:
```ocaml
type sumTypeRegistry = (string * int * AST.semanticType list) list StringOrder.Map.t
```

### VariantLookup
Maps variant names to their containing type:
```ocaml
type variantLookup = (string * string list * int * AST.semanticType list) StringOrder.Map.t
```

## Type Checking Algorithm

### Function Definitions
```dark
let add (a: Int64) (b: Int64) : Int64 = a + b
```
1. Add parameters to type environment
2. Check body expression
3. Verify return type matches declared type

### Let Bindings
```dark
let x = 5 in x + 1
```
1. Infer type of value expression
2. Add binding to environment
3. Check body with extended environment

### Binary Operations
```dark
a + b
```
1. Check left operand type
2. Check right operand type
3. Verify compatible types for operator
4. Return result type

### Unary Operations
```dark
Stdlib.Int64.bitwiseNot x
```
1. Check operand type
2. Verify the operator is valid for that type
3. Preserve the operand integer width for sized integer unary operators (for example `UInt8`)

### Function Calls
```dark
add 1 2
```
1. Look up function signature
2. Check argument types match parameter types
3. Return declared return type

## Partial Application

The type checker desugars partial application:

```dark
let addFive = add(5)  // Partial application
```

Desugars to:
```dark
let addFive = fun x -> add 5 x
```

This is handled by generating lambda wrappers with fresh parameter names.

## Generic Functions

Generic functions use type parameters:

```dark
let identity<'T> (x: T) : T = x
```

At call sites, type arguments can be explicit or inferred from argument types,
and in some contexts from the expected return type:
```dark
identity<Int64> 42  // Explicit
identity 42         // Inferred from argument
```

### Freshening

When instantiating generic functions, type parameters are freshened to avoid
capture:
```ocaml
val freshenTypeParams : string option -> string list -> string list * string StringOrder.Map.t
```

### Local Unification

Generic calls use local unification to match parameter and return type patterns
against concrete call-site types:

```ocaml
type substitution = AST.semanticType StringOrder.Map.t
val unifyTypes : AST.semanticType -> AST.semanticType -> (Types.substitution, string) result
val applySubst : substitution -> AST.semanticType -> AST.semanticType
```

This supports generic type argument inference without introducing whole-program
constraint solving.

## Error Types

```ocaml
type typeError =
  | TypeMismatch of AST.semanticType * AST.semanticType * string
  | IfBranchTypeMismatch of AST.semanticType * AST.semanticType
  | UndefinedVariable of string | UndefinedCallTarget of string | MissingTypeAnnotation of string
  | InvalidOperation of string * AST.semanticType list
  | IncompatibleEqualityOperands of AST.semanticType * AST.semanticType
  | IncompatibleOrderingOperands of AST.semanticType * AST.semanticType
  | PolymorphicRecursion of string | ResolutionFailure of NameResolution.resolutionError | GenericError of string
```

## Type Inference for Expressions

| Expression | Inferred Type |
|------------|---------------|
| `42` | TInt64 |
| `true` | TBool |
| `"hello"` | TString |
| `3.14` | TFloat64 |
| `()` | TUnit |
| `(a, b)` | TTuple [type(a), type(b)] |
| `[1, 2, 3]` | TList TInt64 |
| `a + b` | TInt64 (arithmetic) or TString (concat) |
| `a == b` | TBool |

## Implementation Files

| File | Purpose |
|------|---------|
| `src/frontend/TypeChecking.ml` | Main type checker |
| `src/AST.ml` | Type definitions |

## Key Functions

| Function | Purpose |
|----------|---------|
| `checkExpr` | Type check an expression |
| `checkFunctionDef` | Type check a function definition |
| `unifyTypes` | Check type compatibility and collect substitutions |
| `applySubst` | Apply type variable substitution |
| `freshenTypeParams` | Give each generic-call instantiation distinct inference identities without scanning the caller environment |
