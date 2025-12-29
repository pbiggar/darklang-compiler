# Closures & Lambda Lifting

This document describes how the Dark compiler handles closures (anonymous functions
that capture variables from their enclosing scope).

## Overview

Closures are implemented through **lambda lifting**, a transformation that converts
nested lambdas into top-level functions with explicit capture parameters.

## Key Concepts

### Free Variables

A **free variable** in a lambda is a variable used but not defined within the lambda:

```dark
let y = 10 in
let f = (x: Int64) => x + y  // y is free, x is bound
in f(5)  // Returns 15
```

### Closure Representation

At runtime, a closure is a heap-allocated tuple containing:
1. A pointer to the lifted function
2. The captured values (in order they appear)

```
Closure memory layout: [func_ptr, cap1, cap2, ...]
```

## Lambda Lifting Algorithm

Implemented in `2_AST_to_ANF.fs`:

### Phase 1: Free Variable Collection

```fsharp
// freeVars collects variables used but not bound in scope
let rec freeVars (expr: AST.Expr) (bound: Set<string>) : Set<string>
```

Walk the expression tree, tracking bound variables. A variable is free if it's
used but not in the bound set.

### Phase 2: Lambda Transformation

When a lambda is encountered in argument position:

1. Collect free variables in the lambda body
2. Generate a unique lifted function name (e.g., `__closure_0`)
3. Add a hidden `__closure` parameter (first parameter)
4. Rewrite body to extract captures from the closure tuple
5. Replace the lambda with a `Closure` expression

**Before:**
```dark
let y = 10 in
((x: Int64) => x + y)(5)
```

**After lifting:**
```dark
// Generated function:
let __closure_0(__closure, x: Int64) =
    let y = __closure.1  // Extract captured y
    in x + y

// At call site:
let y = 10 in
let closure = ClosureAlloc(__closure_0, [y])
in ClosureCall(closure, [5])
```

### Uniform Closure Calling Convention

All function values (even non-capturing lambdas and function references) become
closures for uniform calling convention. This simplifies codegen and higher-order
function support:

```dark
// Even this creates a trivial closure:
let f = (x: Int64) => x + 1  // ClosureAlloc(__closure_1, [])
in f(5)
```

## ANF Representation

From `ANF.fs`:

```fsharp
type CExpr =
    | ClosureAlloc of funcName:string * captures:Atom list
    | ClosureCall of closure:Atom * args:Atom list
    | ClosureTailCall of closure:Atom * args:Atom list
```

## Code Generation

From `6_CodeGen.fs`:

1. **ClosureAlloc**: Allocates tuple on heap, stores function address and captures
2. **ClosureCall**: Loads function pointer from closure[0], passes closure as hidden first arg
3. **ClosureTailCall**: Same as ClosureCall but uses BR instead of BLR (no return)

## Supported Features

### Basic Closures
```dark
let y = 10 in ((x: Int64) => x + y)(5)  // 15
```

### First-Class Functions
```dark
let f = (x: Int64) => x + 1 in f(5)  // 6
```

### Multiple Captures
```dark
let a = 1 in let b = 2 in ((x: Int64) => a + b + x)(39)  // 42
```

### Nested Lambdas
```dark
let y = 10 in ((x: Int64) => ((z: Int64) => x + y + z)(2))(30)  // 42
```

### Higher-Order Functions
```dark
let apply = (f: Int64 -> Int64, x: Int64) => f(x)
in apply((x: Int64) => x * 2, 21)  // 42
```

### Partial Application
```dark
let add = (x: Int64) => (y: Int64) => x + y
in add(10)(32)  // 42
```

## Key Git History

- `66cb761` - Add closures: lambdas that capture variables from enclosing scope
- `6945c98` - Add higher-order functions: pass functions as arguments
- `772b0f9` - Add first-class functions: store lambdas in variables
- `92f6245` - Add lambda expressions with immediate application support
- `6d50977` - Add curried lambda support (x => y => x + y)
- `73d0cd2` - Add partial application support for functions and lambdas

## Implementation Files

| File | Purpose |
|------|---------|
| `2_AST_to_ANF.fs:696-1011` | Lambda lifting (freeVars, liftLambdasInExpr) |
| `ANF.fs:76-78` | ClosureAlloc, ClosureCall, ClosureTailCall types |
| `6_CodeGen.fs` | Closure runtime code generation |

## Tests

See `src/Tests/e2e/closures.e2e` (146 lines) and `src/Tests/e2e/higher_order.e2e` (50 lines).
