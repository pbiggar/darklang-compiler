# Primitive literal parity

## Pinned evidence

This revalidation used compiler evidence revision
b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899 and darklang/dark revision
04fbe9dcc995c6188757d583e273cbd30a3e2d3d. Implementation began at rebased
compiler HEAD 07b61696c207e974a2dc3aa3714a8841c0cc07c8; DCB1 report 8a402797
was used only as a lead.

The interpreter contract is anchored at backend/src/LibParser/Lexer.fs:31-135
(shared escape/scalar decoding), Lexer.fs:528-831 (number and raw literals),
Parser.fs:398-535 (minimum magnitudes and validation), and
LibExecution/RuntimeTypes.fs:941-966 (scalar Dval forms).

## Implemented syntax

The compiler parser now uses one scalar-aware decoder for regular String, Char,
and interpolated literal text. It accepts the interpreter escape alphabet,
including control escapes, slash, and scalar escapes; rejects surrogates and
out-of-range scalars; and supports raw triple strings and raw triple
interpolation. All literal text is normalized to NFC before it enters the AST.
Focused acceptance and invalid-scalar coverage is in
`test/fixtures/e2e/literal_parity.e2e` and `test/fixtures/syntax/literals.syntax`.

Literal lowering remains in `passes/anf/lowering/AtomLowering.fs` and
`passes/anf/lowering/ExpressionLowering.fs`. The existing
ValueRendering.fs and PrintInsertion.fs paths remain the only
eval-boundary rendering implementation.

## Retained divergences

Bare decimal literals lower to arbitrary-precision Int, matching the
interpreter. Int128/UInt128 decimal-string lowering remains an AOT
representation divergence. Static type checking remains intentionally
compile-time rather than interpreter runtime dispatch.
