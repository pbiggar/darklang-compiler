# Integer parity

This implementation was revalidated from compiler baseline
`C@a78567efd773de86265e55a54445ddf5a5a8911c` and interpreter baseline
`I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. The historical DCB1 report at
`8a402797` and compiler evidence at
`C@51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` were used only to locate the
surface; every retained finding was checked again against
`packages/darklang/stdlib/int.dark` through `uint128.dark` and
`backend/testfiles/execution/stdlib/ints` at the pinned interpreter revision.

Integration was revalidated after rebasing at exact compiler revision
`C@f2f5f68e3e6c0a15c41a492caf2144b98356bc50` against the same exact
interpreter revision `I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The comparison used a fresh checkout of the interpreter revision, not the DCB1
report or only the copied fixtures. At that compiler revision, the public
implementations are `src/DarkCompiler/stdlib/Int.dark`, `Int8.dark` through
`UInt64.dark`, `Int128.dark`, and `UInt128.dark`; shared arbitrary-width
primitives and checked conversions are in `src/DarkCompiler/stdlib/__Integer.dark`.
The matching behavioral probes are the eleven files under
`src/Tests/e2e/upstream/stdlib/ints` and `src/Tests/e2e/int128-wrapping.e2e`.

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

The public modules expose only the interpreter-declared names. Historical
`sub`, `mul`, `div`, and `*_v0` compatibility aliases, and the historical
power operator spelling, are absent from this parity surface. Internal
representation helpers beginning with `__` remain implementation details.
Static operand and conversion type enforcement is the intentional AOT
divergence; the compiler does not reproduce interpreter runtime dispatch
errors for source that can be rejected during type checking.

## Package-value probe

The upstream expression `Stdlib.Int8.add Darklang.Test.Values.int8Value 5y`
obtains the package value `Darklang.Test.Values.int8Value`, whose evaluated
value is `5y`; the interpreter resolves that global through its package-value
environment before calling `Int8.add`, producing `10y`. AOT never performs a
live package lookup. Its compile request receives the immutable
`PackageValueCatalog` entry with hash `darklang-test-values-int8Value`, the
`Darklang.Test.Values.int8Value` location, concrete `Int8` result type, and
the available evaluator expression `5y`. Materialization generates the typed
`Builtin.pmEvaluateValue<Int8>` case from that entry. The focused parity test
in `ValueSearchCatalogTests.fs` calls that generated evaluator and supplies its
returned `5y` to `Stdlib.Int8.add(value, 5y)`, asserting the same `10y` result.

The executable parity corpus is the eleven files under
`src/Tests/e2e/upstream/stdlib/ints`, plus `integer-family.e2e`. Fixed Int
power and closure modulus probes use the declared named functions, and two
aggregate expectations express equality explicitly so the native runner
evaluates the same lists inside the compiled program.
