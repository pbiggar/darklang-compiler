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

## Type System

Defined in `src/DarkCompiler/AST.fs`:

```fsharp
type Type =
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TBool | TFloat64 | TString | TBlob | TChar | TUnit
    | TRuntimeError
    | TFunction of Type list * Type
    | TTuple of Type list
    | TRecord of string * Type list
    | TSum of string * Type list
    | TList of Type
    | TVar of string
    | TRawPtr
    | TDict of keyType:Type * valueType:Type
```

`Blob` is the sole binary type. Blob equality is admitted as handle identity;
there is no `Bytes` type. `Stdlib.Bytes` is only a legacy function namespace
whose binary parameters and results are typed `Blob`.

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

### Unary Operations
```dark
~~~x
```
1. Check operand type
2. Verify the operator is valid for that type
3. Preserve the operand integer width for sized integer unary operators (for example `UInt8`)

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
let addFive = fun x -> add(5, x)
```

This is handled by generating lambda wrappers with fresh parameter names.

## Generic Functions

Generic functions use type parameters:

```dark
def identity<T>(x: T) : T = x
```

At call sites, type arguments can be explicit or inferred from argument types,
and in some contexts from the expected return type:
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

### Local Unification

Generic calls use local unification to match parameter and return type patterns
against concrete call-site types:

```fsharp
type Substitution = Map<string, Type>
let unifyTypes (pattern: Type) (actual: Type) : Result<Substitution, string>
let applySubst (subst: Substitution) (typ: Type) : Type
```

This supports generic type argument inference without introducing whole-program
constraint solving.

## Error Types

```fsharp
type TypeError =
    | TypeMismatch of expected:Type * actual:Type * context:string
    | IfBranchTypeMismatch of expected:Type * actual:Type
    | UndefinedVariable of name:string
    | UndefinedCallTarget of name:string
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
| `src/DarkCompiler/passes/1.5_TypeChecking.fs` | Main type checker |
| `src/DarkCompiler/AST.fs` | Type definitions |

## Key Functions

| Function | Purpose |
|----------|---------|
| `checkExpr` | Type check an expression |
| `checkFunctionDef` | Type check a function definition |
| `unifyTypes` | Check type compatibility and collect substitutions |
| `applySubst` | Apply type variable substitution |
| `freshenTypeParams` | Generate fresh type variable names |
