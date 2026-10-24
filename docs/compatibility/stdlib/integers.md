# Integer parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

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

`Int128` and `UInt128` are operational managed values backed by immutable
fixed blocks containing low and high `UInt64` limbs. Arithmetic and bitwise
results normalize through two's-complement residue modulo 2^128. The signed
boundary `170141183460469231731687303715884105726Q + 4Q` therefore produces
`-170141183460469231731687303715884105726Q`; unsigned maximum plus one produces
zero. Their public modules intentionally have no random function, matching the
pinned interpreter.

The arbitrary-width implementation is in
`packages/darklang/stdlib/int/__integer.dark`; public wrappers are in `Int.dark`, the
eight fixed-width module files, `Int128.dark`, and `UInt128.dark`. The 128-bit
modules perform limb arithmetic directly and use arbitrary-width values only
at operations and conversions that require them. Typed representation views
are ownership-neutral, while newly computed fixed blocks are owned. `src/ir/anf/ANF.ml`
classifies `Int` as a managed dynamic value and both 128-bit types as managed
16-byte fixed blocks, including when nested in closures and heap shapes. Fixed-width
shifts and arithmetic lower in `src/passes/mir/MIR_to_LIR.ml`; signed right shift is
arithmetic, unsigned right shift is logical, and counts use the interpreter's
machine-width masks on both ARM64 and x86-64.

## Intentional difference

The named parity API uses the interpreter-declared functions. Internal
representation helpers beginning with `__` are implementation details.
Static operand and conversion type enforcement is the intentional AOT
divergence; the compiler does not reproduce interpreter runtime dispatch
errors for source that can be rejected during type checking.

## Package-value probe

The upstream expression `Stdlib.Int8.add Darklang.Test.Values.int8Value 5y`
obtains the package value `Darklang.Test.Values.int8Value`, whose evaluated
value is `5y`; the interpreter resolves that global through its package-value
environment before calling `Int8.add`, producing `10y`. Without an explicit hosted package server, this focused probe uses an immutable
catalog rather than a live lookup. Its compile request receives the immutable
`PackageValueCatalog` entry with hash `darklang-test-values-int8Value`, the
`Darklang.Test.Values.int8Value` location, concrete `Int8` result type, and
the available evaluator expression `5y`. Materialization generates the typed
`Builtin.pmEvaluateValue<Int8>` case from that entry. The focused parity test
in `test/compiler-passes/ValueSearchCatalogTests.ml` calls that generated evaluator and supplies its
returned `5y` to `Stdlib.Int8.add(value, 5y)`, asserting the same `10y` result.

The executable parity corpus is the eleven files under
`test/fixtures/e2e/upstream/stdlib/ints`, plus `integer-family.e2e`. Fixed Int
power and closure modulus probes use the declared named functions, and two
aggregate expectations express equality explicitly so the native runner
evaluates the same lists inside the compiled program.
