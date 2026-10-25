# Primitive literal parity

Semantic comparison baseline: `darklang/dark@04fbe9dcc995c6188757d583e273cbd30a3e2d3d`.
The [source ledger](../../porting/README.md) owns current support and test dispositions.

The interpreter contract is anchored at backend/src/LibParser/Lexer.fs:31-135
(shared escape/scalar decoding), backend/src/LibParser/Lexer.fs (number and raw literals),
backend/src/LibParser/Parser.fs (minimum magnitudes and validation), and
LibExecution/RuntimeTypes.fs:941-966 (scalar Dval forms).

## Implemented syntax

The compiler parser now uses one scalar-aware decoder for regular String, Char,
and interpolated literal text. It accepts the interpreter escape alphabet,
including control escapes, slash, and scalar escapes; rejects surrogates and
out-of-range scalars; and supports raw triple strings and raw triple
interpolation. All literal text is normalized to NFC before it enters the AST.
Focused acceptance and invalid-scalar coverage is in
`test/fixtures/e2e/literal_parity.e2e` and `test/fixtures/syntax/literals.syntax`.

Literal lowering remains in `src/passes/anf/lowering/AtomLowering.ml` and
`src/passes/anf/lowering/ExpressionLowering.ml`. The existing
src/frontend/ValueRendering.ml and src/passes/anf/PrintInsertion.ml paths remain the only
eval-boundary rendering implementation.

## Retained divergences

Float literals preserve the decimal precision specified in the source. After
conversion to binary64, rounding the exact stored value back to the source's
last specified decimal place must recover the exact written decimal value.
Both conversions use nearest, ties-to-even rounding. For example, `0.1` is
accepted, but `10000000000000001.0` and `0.10000000000000000` are rejected.
Trailing zeros specify precision; exponents shift its position. Overflow and
nonzero underflow to zero are errors. Negative zero and qualifying subnormals
are preserved. Validation and exponent-free normalization do not use host
formatting or impose an arbitrary source-exponent limit. The policy applies to
expression and pattern literals; runtime string parsing is unchanged.

This language change matches the interpreter implementation merged in
[darklang/dark#5985](https://github.com/darklang/dark/pull/5985).
The ledger pins that PR's merged head commit, preserving the existing source
inventory while including the float-literal changes.

Bare decimal literals lower to arbitrary-precision Int, matching the
interpreter. Int128/UInt128 lower to managed fixed blocks containing two UInt64 limbs;
this native representation is an AOT difference. Static type checking remains intentionally
compile-time rather than interpreter runtime dispatch.
