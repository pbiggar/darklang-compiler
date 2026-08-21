# Arbitrary-Precision and 128-bit Integers

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
after every operation. Arithmetic is implemented in the internal target-neutral
`src/DarkCompiler/stdlib/__Integer.dark` layer, with the public surface in
`Int.dark`. Both native backends therefore use identical semantics.

## Int128 and UInt128

`Q` and `Z` literals select signed and unsigned 128-bit values. They use the
same canonical managed-buffer representation as `Int`, but normalize every
arithmetic and bitwise result modulo 2^128. Signed values reinterpret that
residue through the interval -2^127 through 2^127-1. Their modules provide the
interpreter-declared arithmetic, remainder/modulus, comparison, formatting,
parsing, Float conversion, aggregation, checked conversions, bitwise
operations, and masked shifts.

See [integer-parity.md](../integer-parity.md) for the revision-pinned public
surface, failure contracts, extensions, and intentional AOT differences.
