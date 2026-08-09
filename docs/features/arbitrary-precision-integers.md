# Arbitrary-Precision Integers

The `Int` type is a signed integer with no fixed-width overflow boundary. An
`I` suffix selects it: `9223372036854775808I`. Unsuffixed integer literals
remain `Int64` for compatibility with existing compiler programs.

`Int` supports arithmetic, truncating division, nonnegative-exponent power,
comparison, remainder and modulus, shifts, and infinite two's-complement
bitwise operations. `Stdlib.Int` also supplies parsing, formatting, square
root, Float conversion, and checked conversions to every fixed-width integer
type. NaN and infinities cannot convert to `Int` and produce an out-of-range
runtime error.

## Representation

The compiler represents each value as a reference-counted dynamic buffer
containing its canonical decimal form. The type system keeps `Int` distinct
from `String`, while representation-only intrinsics allow the target-neutral
stdlib implementation to share the existing buffer allocation and reference
counting machinery on ARM64 and x86-64.

Canonical storage has one optional leading minus sign, no leading zeroes, and
uses `0` rather than negative zero. This makes equality and printing stable
after every operation. Arithmetic is implemented in
`src/DarkCompiler/stdlib/Int.dark`, so both native backends use identical
semantics.
