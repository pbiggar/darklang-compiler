# Type Checking

This document describes the type checking pass in the Dark compiler.

## Overview

The Dark compiler uses **top-down type checking** rather than full type inference.
Function parameters and return types require explicit annotations; let bindings
have optional annotations.

## Design Philosophy

- **Explicit over implicit**: Type annotations required at function boundaries
- **Simple implementation**: No unification algorithm or constraint solving
- **Fast compilation**: Single-pass, no iteration to fixed point for types

## Type System

Defined in `AST.fs:20-46`:

```fsharp
type Type =
    | TInt64 | TInt32 | TInt16 | TInt8
    | TUInt64 | TUInt32 | TUInt16 | TUInt8
    | TBool | TFloat64 | TString | TChar | TUnit
    | TFunction of Type list * Type    // (params) -> return
    | TTuple of Type list              // (Int64, Bool)
    | TRecord of string                // record type name
    | TSum of string * Type list       // sum type with type args
    | TList of Type                    // List<T>
    | TDict of Type * Type             // Dict<K, V>
    | TVar of string                   // type variable (generics)
    | TRawPtr                          // raw pointer (internal)
```

## Type Registries

The type checker maintains several registries:

### TypeEnv
Maps variable names to types:
```fsharp
type TypeEnv = Map<string, Type>
```

### TypeRegistry
Maps record type names to field definitions:
```fsharp
type TypeRegistry = Map<string, (string * Type) list>
// "Point" -> [("x", TInt64); ("y", TInt64)]
```

### SumTypeRegistry
Maps sum type names to variants:
```fsharp
type SumTypeRegistry = Map<string, (string * int * Type option) list>
// "Option" -> [("Some", 0, Some TVar "t"); ("None", 1, None)]
```

### VariantLookup
Maps variant names to their containing type:
```fsharp
type VariantLookup = Map<string, (string * string list * int * Type option)>
// "Some" -> ("Option", ["t"], 0, Some TVar "t")
```

## Type Checking Algorithm

### Function Definitions
```dark
def add(a: Int64, b: Int64) : Int64 = a + b
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

### Function Calls
```dark
add(1, 2)
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
let addFive = (x: Int64) => add(5, x)
```

This is handled by generating lambda wrappers with fresh parameter names.

## Generic Functions

Generic functions use type parameters:

```dark
def identity<T>(x: T) : T = x
```

At call sites, type arguments can be explicit or inferred:
```dark
identity<Int64>(42)  // Explicit
identity(42)         // Inferred from argument
```

### Freshening

When instantiating generic functions, type parameters are freshened to avoid
capture:
```fsharp
let freshenTypeParams (typeParams: string list) : string list * Map<string, string>
```

## Error Types

```fsharp
type TypeError =
    | TypeMismatch of expected:Type * actual:Type * context:string
    | UndefinedVariable of name:string
    | MissingTypeAnnotation of context:string
    | InvalidOperation of op:string * types:Type list
    | GenericError of string
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
| `1.5_TypeChecking.fs` | Main type checker (2212 lines) |
| `AST.fs:20-46` | Type definitions |

## Key Functions

| Function | Purpose |
|----------|---------|
| `checkExpr` | Type check an expression |
| `checkFunctionDef` | Type check a function definition |
| `unify` | Check type compatibility |
| `applyTypeSubst` | Apply type variable substitution |
| `freshenTypeParams` | Generate fresh type variable names |
