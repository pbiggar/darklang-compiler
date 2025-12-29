# Generics and Monomorphization

This document explains how generic functions work in the Dark compiler.

## Overview

The Dark compiler uses **monomorphization** - generics are fully expanded at compile time. There are no runtime generics or type erasure with boxing. Each generic function instantiation becomes a separate specialized function.

## How It Works

### 1. Type Parameters

Generic functions declare type parameters in angle brackets:

```dark
def identity<T>(x: T) : T = x
def swap<A, B>(pair: (A, B)) : (B, A) = (pair.1, pair.0)
```

### 2. Type Application (Call Sites)

When calling a generic function, type arguments are provided:

```dark
identity<Int64>(42)       // Calls identity specialized for Int64
swap<String, Bool>(("hello", true))
```

### 3. Monomorphization Process

The compiler performs monomorphization in `2_AST_to_ANF.fs`:

1. **Collect generic definitions**: Find all functions with type parameters
2. **Find instantiation sites**: Scan for `TypeApp` expressions (generic calls)
3. **Generate specializations**: For each unique `(funcName, [typeArgs])` pair:
   - Substitute type parameters with concrete types in the function body
   - Generate a new function with mangled name (e.g., `identity_i64`)
4. **Replace TypeApps with Calls**: Convert `TypeApp("identity", [Int64], [x])` to `Call("identity_i64", [x])`
5. **Iterate until fixed point**: New specializations may contain more TypeApps

### 4. Name Mangling

Specialized function names encode their type arguments:

| Generic Call | Specialized Name |
|--------------|------------------|
| `identity<Int64>(x)` | `identity_i64` |
| `identity<String>(s)` | `identity_str` |
| `map<Int64, Bool>(...)` | `map_i64_bool` |
| `Dict.get<String, Int64>(...)` | `Stdlib.Dict.get_str_i64` |

## Key Implementation Details

### Iterative Monomorphization

Specialization is iterative because a specialized function body may contain new TypeApps:

```dark
def wrap<T>(x: T) : List<T> = [x]
def doubleWrap<T>(x: T) : List<List<T>> = wrap<List<T>>(wrap<T>(x))

// Calling doubleWrap<Int64> requires:
// 1. doubleWrap_i64 (from initial call)
// 2. wrap_i64 (discovered in doubleWrap_i64 body)
// 3. wrap_list_i64 (discovered in doubleWrap_i64 body)
```

The algorithm uses a fixed-point iteration: keep specializing until no new TypeApps are found.

### External Generic Definitions

When compiling user code that calls stdlib generics (like `List.map<T>`), the compiler needs access to the generic function bodies from stdlib. The `monomorphizeWithExternalDefs` function handles this by:

1. Loading stdlib generic function definitions
2. Merging them with user-defined generics
3. Specializing both as needed

### Type Substitution

Type substitution walks the AST and replaces type variables with concrete types:

- `TVar "T"` → `TInt64` (when T=Int64)
- `List<T>` → `List<Int64>`
- `(T, T)` → `(Int64, Int64)`

## Design Decisions

### Why Monomorphization?

1. **No runtime type info needed**: Specialized functions work directly with concrete types
2. **Better optimization**: Each specialization can be optimized for its specific types
3. **Simpler code generation**: No boxing/unboxing or vtable dispatch
4. **Matches Darklang's functional style**: Pure functions with known types

### Limitations

1. **Code size**: Each instantiation generates a new function (code bloat for heavily generic code)
2. **No runtime polymorphism**: Can't have heterogeneous collections like `List<Any>`
3. **Compile-time only**: All type arguments must be known at compile time

## Git History Context

Key commits in the evolution of generics:

- `5a50953` - Add multiple type parameter support with validation
- `6f5ad49` - Fix iterative monomorphization (fixed-point approach)
- `74fcc65` - Add deferred type unification for generic data types
- `d782124` - Fix type inference for generic recursion
- `9a1960f` - Add partial application support for generic functions

## Related Files

- `src/DarkCompiler/passes/2_AST_to_ANF.fs` - Monomorphization implementation (lines 1162-1290)
- `src/DarkCompiler/passes/1.5_TypeChecking.fs` - Generic type validation
- `src/DarkCompiler/AST.fs` - `TVar`, `TypeApp` type definitions
- `src/Tests/e2e/generics.e2e` - Test cases
