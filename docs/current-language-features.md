# Current Language Features

This document lists the language features currently supported by the Dark compiler.

## Types

- Integers: arbitrary-precision signed `Int`, Int8/Int16/Int32/Int64/Int128, and UInt8/UInt16/UInt32/UInt64/UInt128; Bool booleans, Float floating-point numbers, String strings, Unit `()`
- Tuples of any arity: `(Int64, Bool)`, `(Int64, Int64, Int64)`
- Records with named fields: `type Point = { x: Int64, y: Int64 }`
- Interpreter-compatible sum types (ADTs): `type Option<'a> = | None | Some of 'a`
- Lists as linked lists: `List<T>` with literals `[1, 2, 3]`
- Dict types: `Dict<K, V>`
- Type aliases: `type Id = Int64`, `type Pair<T> = (T, T)`
- Generic type parameters and type application
- Function types with arrow syntax: `Int64 -> Int64`
- Type-directed record field lookup (no ambiguity when record types share field names)
- Static type checking with exhaustiveness checking for pattern matches
- Function return type inference using function registry (enables type inference for let-bound function calls)

## Expressions

- Integer, float, boolean, string, and unit literals (with string escape sequences); unsuffixed integer literals are Int64 and an `I` suffix produces an arbitrary-precision `Int`
- Arithmetic and remainder: `+`, `-`, `*`, `/`, `%`
- Bitwise integer operators: `&`, `|||`, `^`, `~~~`, `<<`, `>>` (`^` is exponentiation for `Int`, matching Dark)
- Comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical operators: `&&`, `||`, `!`
- Parentheses for grouping
- String interpolation: `$"Hello {name}"`
- String concatenation: `++`
- Tuple construction and zero-based access: `(1, 2)` and `t.0`
- Record construction and field access: `Point { x = 1, y = 2 }` and `p.x`
- Record update: `{ p with x = 100 }`
- List literals and cons: `[]`, `[1, 2, 3]`, `[1, ...rest]`
- ADT constructors: `Option.Some 42L`, `Option.None`
- Function application and pipe operator: `f(x)` and `x |> f`
- Structural equality and inequality for tuples, records, and ADTs

## Control flow

- Let bindings with shadowing: `let x = 5 in x + 1`
- Let-pattern matching: `let (a, b) = tuple in ...`
- If expressions (else optional, returns unit when omitted)
- Match expressions with `when` guards

## Patterns

- Literal patterns: `42`, `true`, `"hello"`, `()`
- Variable and wildcard patterns: `x`, `_`
- Tuple patterns: `(a, b)`, `(x, _, z)`
- Record patterns: `Point { x = a, y = b }` (type name required)
- ADT constructor patterns: `Some(n)`, `None`
- List patterns with exact-length and cons matching: `[]`, `[a, b]`, `[h, ...t]`

## Functions and lambdas

- Function definitions with type signatures and calls
- Recursion and implicit mutual recursion
- Up to 8 integer or pointer parameters and up to 8 Float parameters in registers
- Lambda expressions with immediate application: `(x: Int64) => x + 1`
- First-class functions (store lambdas in variables, call later)
- Higher-order functions (pass named functions or lambdas as arguments)
- Closures (lambdas that capture variables from enclosing scope)
- Currying and partial application

## Examples

```dark
// Factorial
def factorial(n: Int64) : Int64 =
  if n <= 1 then 1
  else n * factorial(n - 1)

def main() : Int64 = factorial(5)

// Pattern matching on an ADT (interpreter syntax)
type Option<'a> = | None | Some of 'a

match Option.Some 42L with
| Some x -> x * 2L
| None -> 0L

// List processing (exact-length matching)
def main() : Int64 =
  match [1, 2, 3] with
  | [a, b, c] -> a + b + c  // matches exactly 3 elements
  | _ -> 0
```
