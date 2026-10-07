# Primitive literal parity

Current compiler source review: 2026-10-07 at `7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`.
See the [current audit](../current-audit.md) for post-port status and validation.
Older revision pairs and executed counts below are historical evidence, not
a fresh test result for this revision.

## Pinned evidence

This revalidation used compiler evidence revision
b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899 and darklang/dark revision
04fbe9dcc995c6188757d583e273cbd30a3e2d3d. Implementation began at rebased
compiler HEAD 07b61696c207e974a2dc3aa3714a8841c0cc07c8; DCB1 report 8a402797
was used only as a lead.

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

Bare decimal literals lower to arbitrary-precision Int, matching the
interpreter. Int128/UInt128 lower to managed fixed blocks containing two UInt64 limbs;
this native representation is an AOT difference. Static type checking remains intentionally
compile-time rather than interpreter runtime dispatch.
