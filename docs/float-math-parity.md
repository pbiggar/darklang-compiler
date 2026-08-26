# Float and Math compatibility

This document records the observable Float/Math contract implemented by the
native compiler. It is a behavior comparison, not a performance comparison.

## Pinned evidence

Every interpreter comparison in this change used:

- compiler baseline `C@fb61d714723f34d6c43e9bdc03dd96fb46f0c4ea`
  (the exact rebased HEAD before implementation);
- interpreter `I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`;
- historical inventory `DCB1@8a402797` only as starting evidence.

`git ls-remote https://github.com/darklang/dark.git refs/heads/main` was
rechecked on 2026-08-14 and returned the pinned interpreter revision. The
interpreter surface is anchored at
`packages/darklang/stdlib/float.dark:4-146` and
`packages/darklang/stdlib/math.dark:4-92`. Its runtime behavior is anchored at
`backend/src/Builtins/Builtins.Pure/Libs/Float.fs:12-367`,
`backend/src/Builtins/Builtins.Pure/Libs/Math.fs:13-213`, and
`backend/src/LibExecution/Builtin.fs:97-104`.

The compiler implementation is in `src/DarkCompiler/stdlib/Float.dark` and
`src/DarkCompiler/stdlib/Math.dark`. Public values continue to render through
`src/DarkCompiler/passes/1.6_ValueRendering.fs` and
`src/DarkCompiler/passes/2.6_PrintInsertion.fs`, both of which call the same
`Stdlib.Float.toString` implementation. Focused executable coverage is in
`src/Tests/e2e/stdlib/float.e2e` and `math.e2e`.

## Public surface

Interpreter parity includes `Float.ParseError.BadFormat`; `ceiling`,
`roundUp`, `floor`, `roundDown`, `round`, `roundTowardsZero`, `truncate`,
`clamp`, `parse`, `power`, and `isNaN`; and `Math.acos`, `asin`, `atan`,
`atan2`, `cos`, `cosh`, `sin`, `sinh`, `tan`, and `tanh`. Existing parity
functions such as Float arithmetic, `absoluteValue`, `negate`, `sqrt`,
`toString`, and Math `pi`, `tau`, `degrees`, `turns`, and `radians` remain.

No runtime type dispatch is introduced. The parity rounding functions return
arbitrary-precision `Int`; compiler implementation helpers are private.

## Rounding, classification, and clamp

`Float.ceiling` and `roundUp` round toward positive infinity. `floor` and
`roundDown` round toward negative infinity. `round` uses nearest, ties-to-even;
`roundTowardsZero` and `truncate` are identical truncation. Finite values are
decoded from their binary64 bits into canonical arbitrary-precision Ints, so
values beyond Int64 do not narrow. NaN and either infinity raise the language
runtime error `Encountered out-of-range value for type of Int`.

This resolves the earlier `Float.ceiling` uncertainty: both the interpreter
source (`floatCeiling` delegates to `System.Math.Ceiling`) and current public
probes establish rounding toward positive infinity with an Int result.

`isNaN` uses IEEE unordered self-comparison and therefore recognizes every NaN
payload. `clamp` rejects a NaN bound with exactly
`clamp requires arguments to be valid numbers`, accepts bounds in either
order, and returns a NaN value inside `Ok` without treating it as a bad bound.
Infinities and signed zero follow ordinary IEEE comparisons.

## Decimal parsing and formatting

`Float.parse` trims ASCII numeric whitespace and accepts an optional sign,
leading-dot or ordinary decimal notation, an optional decimal exponent, signed
zero, `NaN`, and signed `Infinity`. It converts the decimal rational with
arbitrary-precision arithmetic and ties-to-even rounding to binary64. Overflow
produces signed infinity; underflow and subnormal boundaries are rounded, not
narrowed through Int64. Invalid or incomplete input returns typed
`Error(Float.ParseError.BadFormat)` without a host exception or fallback.

Finite `Float.toString` output is the closest shortest decimal significand
that parses back to the identical `Float.toBits` value. It uses fixed notation
for decimal exponents from -4 through 11 and lowercase scientific notation
outside that range. Positive scientific exponents use `+` without zero
padding. Only finite, fixed, integer forms receive `.0`. Special spellings are
exactly `-0.0`, `Infinity`, `-Infinity`, and `NaN`.

The shortest-roundtrip finite format is an intentional improvement over the
pinned interpreter's `G12` formatting (`Float.fs:325-352`), which can discard
bits. The fixed/scientific boundary, lowercase `e`, whole-number convention,
and nonfinite spellings remain interpreter-compatible. Previously unresolved
edges are locked by minimum-subnormal and maximum-finite round trips,
powers-of-ten on both notation boundaries, halfway decimal input, negative
zero, and a value requiring 17 significant digits.

## Power and transcendental behavior

`Float.power` handles ordinary and fractional powers, negative exponents,
negative bases, signed zero, infinities, NaNs, overflow, underflow, and invalid
real domains using IEEE results. Exact integer exponents use exponentiation by
squaring; square-root exponents share the native square-root primitive; the
remaining positive-domain cases use the same deterministic software kernels on
both targets. Calls are not constant-folded to a different host operation.

Math range reduction and kernels are written in portable Dark, so ARM64 and
x64 execute the same binary64 operation sequence. `acos` and `asin` return
`None` for out-of-domain and NaN inputs and `Some` otherwise. The other
functions preserve the tested NaN, infinity, quadrant, overflow, and signed-zero
behavior.

At `I@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`, `mathTanh` calls
`System.Math.Sinh` (`Math.fs:201-209`). This revision-specific anomaly is
intentionally reproduced: `Math.tanh(1.0)` equals the sinh result, not the
mathematical hyperbolic tangent. A nonzero E2E probe prevents accidental
"correction" away from the compatibility baseline.

## Error phase and focused probes

Static argument and result types are enforced during AOT type checking.
Value-dependent parse failures remain `Result`; inverse-trigonometric domain
failures remain `Option`; nonfinite Float-to-Int conversion remains a language
runtime error. None of these contracts introduces interpreter-style runtime
type decisions.

Revalidated same-source probes cover aliases and rounding direction, an Int
beyond Int64, nonfinite rounding failure, reversed/NaN clamp bounds, leading-dot
and whitespace parsing, halfway/subnormal/overflow parsing, shortest bit
round-trips, scalar/list/tuple rendering, power domains and signed zero,
inverse-trigonometric Options, atan2 quadrants, hyperbolic overflow, and the
nonzero tanh anomaly.
