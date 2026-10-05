# Generics and Monomorphization

This document explains how generic functions work in the Dark compiler.

## Overview

The Dark compiler uses **monomorphization** - generics are fully expanded at compile time. There are no runtime generics or type erasure with boxing. Each generic function instantiation becomes a separate specialized function.

## How It Works

### 1. Type Parameters

Generic functions declare type parameters in angle brackets:

```dark
let identity<'T> (x: T) : T = x
let swap<'A, 'B> (pair: (A * B)) : (B * A) = let (first, second) = pair in (second, first)
```

### 2. Type Application (Call Sites)

When calling a generic function, type arguments are provided:

```dark
identity<Int64>(42)       // Calls identity specialized for Int64
swap<String, Bool> ("hello", true)
```

### 3. Monomorphization Process

The compiler performs monomorphization in `passes/preparation/Monomorphization.fs`,
with identity, substitution, and program entry points in neighboring modules:

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
| `identity<Int64> x` | `identity_i64` |
| `identity<String> s` | `identity_str` |
| `map<Int64, Bool> ...` | `map_i64_bool` |
| `Dict.get<String, Int64> ...` | `Stdlib.Dict.get_str_i64` |

## Key Implementation Details

### Iterative Monomorphization

Specialization is iterative because a specialized function body may contain new TypeApps:

```dark
let wrap<'T> (x: T) : List<T> = [x]
let doubleWrap<'T> (x: T) : List<List<T>> = wrap<List<T>> (wrap<T> x)

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

## Related Files

- `ocaml/lib/passes/preparation/Monomorphization.ml` - Reachable specialization solving and type-application replacement.
- `ocaml/lib/passes/preparation/PrepareFunctions.ml` - Program specialization entry points.
- `ocaml/lib/frontend/TypeChecking.ml` - Generic type validation
- `ocaml/lib/AST.ml` - `TVar`, `TypeApp` type definitions
- `src/Tests/e2e/generics.e2e` - Test cases
