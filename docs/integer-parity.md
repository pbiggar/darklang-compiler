# Integer parity

This implementation was revalidated from compiler baseline
`C@a78567efd773de86265e55a54445ddf5a5a8911c` and interpreter baseline
`I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. The historical DCB1 report at
`8a402797` and compiler evidence at
`C@51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` were used only to locate the
surface; every retained finding was checked again against
`packages/darklang/stdlib/int.dark` through `uint128.dark` and
`backend/testfiles/execution/stdlib/ints` at the pinned interpreter revision.

## Parity surface

`Int` provides arbitrary-width arithmetic, canonical `BadFormat` parsing,
Result-valued remainder, Euclidean modulus, power, absolute value, aggregation,
clamp, inclusive order-independent random values, Float conversion, checked
fixed-width conversions, and infinite two's-complement bitwise operations.

The signed and unsigned 8-, 16-, 32-, and 64-bit modules provide their declared
wrapping arithmetic, checked conversion matrix, parsing, Float conversion,
square root, aggregation, clamp, and inclusive randomness. Signed remainder is
truncating and Result-valued. Modulus requires a positive divisor. Division by
zero, invalid modulus, negative powers, failed Float conversion, and parse and
conversion bounds use the interpreter's values and failure text.

`Int128` and `UInt128` are operational managed values. Arithmetic and bitwise
results normalize through two's-complement residue modulo 2^128. The signed
boundary `170141183460469231731687303715884105726Q + 4Q` therefore produces
`-170141183460469231731687303715884105726Q`; unsigned maximum plus one produces
zero. Their public modules intentionally have no random function, matching the
pinned interpreter.

The target-neutral implementation is in
`src/DarkCompiler/stdlib/__Integer.dark`; public wrappers are in `Int.dark`, the
eight fixed-width module files, `Int128.dark`, and `UInt128.dark`. Typed
representation views are ownership-neutral, while newly computed canonical
buffers are owned. `ANF.fs` classifies Int, Int128, and UInt128 as managed
dynamic values, including when nested in closures and heap shapes. Fixed-width
shifts and arithmetic lower in `4_MIR_to_LIR.fs`; signed right shift is
arithmetic, unsigned right shift is logical, and counts use the interpreter's
machine-width masks on both ARM64 and x86-64.

## Compiler extensions and intentional differences

The following existing names remain compiler extensions and are not additions
to the parity API: `sub`, `mul`, and `div`; `Int.compare` and `Int.equals`;
`popcount`, `isEven`, and `isOdd`; unsigned `absoluteValue` and `negate`; and
representation helpers beginning with `__`. Compiler syntax retains its
integer operator spellings: `^` is `Int` power and fixed-width/128-bit XOR,
while `/` is integer division. The named functions remain the canonical parity
surface. Static operand and conversion type enforcement is the intentional AOT
divergence; the compiler does not reproduce interpreter
runtime dispatch errors for source that can be rejected during type checking.

The executable parity corpus is the eleven files under
`src/Tests/e2e/upstream/stdlib/ints`, plus `integer-family.e2e`. One upstream
package-value probe is represented by its identical typed Int8 value because
the AOT test environment does not import interpreter package globals. Fixed
Int64 power and one closure modulus probe use the equivalent named functions
because the compiler retains different operator spellings; two aggregate
expectations express equality explicitly so the native runner evaluates the
same lists inside the compiled program. These source adaptations are called
out inline and do not change the values under test.
