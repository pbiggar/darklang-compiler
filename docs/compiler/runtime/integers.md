# Arbitrary-Precision and 128-bit Integers

The `Int` type is a signed integer with no fixed-width overflow boundary.
Unsuffixed integer literals select it, so `9223372036854775808` is an `Int`.
The `L` suffix selects `Int64`.

`Int` supports arithmetic, truncating division, nonnegative-exponent power,
comparison, remainder and modulus, shifts, and infinite two's-complement
bitwise operations. `Stdlib.Int` also supplies parsing, formatting, square
root, Float conversion, and checked conversions to every fixed-width integer
type. NaN and infinities cannot convert to `Int` and produce an out-of-range
runtime error.

## Representation

Values in the signed 62-bit interval -2^62 through 2^62-1 are stored directly
in the machine word with a low-bit tag. Larger values use an immutable,
reference-counted buffer of little-endian base-2^31 limbs. Its signed used-limb
count records the sign without requiring two's-complement padding. Zero and
small arithmetic results use tagged values; operations accept either form.

The compiler tracks this representation as `DynamicInt`, separately from
strings and blobs. Dedicated retain/release instructions skip tagged values and
manage only limb buffers, including when an `Int` is captured or stored in a
list, tuple, record, sum, or dictionary.

Arithmetic, shifts, infinite two's-complement bitwise operations, decimal
parsing/formatting, and fixed-width conversions are implemented in the internal
target-neutral `src/DarkCompiler/stdlib/__Integer.dark` layer. Both native
backends therefore share semantics. Decimal strings are allocated only at text
boundaries; arithmetic does not parse or rebuild decimal text.

## Int128 and UInt128

`Q` and `Z` literals select signed and unsigned 128-bit values. They use the
same immutable fixed-block representation: a low `UInt64` limb, a high
`UInt64` limb, and a following reference count. Arithmetic and bitwise results
normalize modulo 2^128. Signed values reinterpret that residue through the
interval -2^127 through 2^127-1. Decimal buffers are created only for text and
arbitrary-precision conversion boundaries. Their modules provide the
interpreter-declared arithmetic, remainder/modulus, comparison, formatting,
parsing, Float conversion, aggregation, checked conversions, bitwise
operations, and masked shifts.

See [integer compatibility](../../compatibility/stdlib/integers.md) for the revision-pinned public
surface, failure contracts, extensions, and intentional AOT differences.
