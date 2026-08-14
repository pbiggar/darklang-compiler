// ParserTests.fs - Focused invariants for parser token-stream construction.
//
// These tests cover internal representation choices that materially affect
// parser complexity and are not observable in end-to-end language tests.

module ParserTests

type TestResult = Result<unit, string>

let private testInterpolatedExpressionsKeepLexerTerminator () : TestResult =
    match Parser.lex "$\"value: {1 + 2}\"" with
    | Ok [Parser.TInterpString [Parser.InterpText "value: "; Parser.InterpTokens tokens]; Parser.TEOF] ->
        match List.tryLast tokens with
        | Some Parser.TEOF -> Ok ()
        | Some token -> Error $"Expected interpolated expression token stream to retain TEOF, got {token}"
        | None -> Error "Expected interpolated expression token stream, got no tokens"
    | Ok tokens -> Error $"Unexpected interpolated string tokens: {tokens}"
    | Error err -> Error err

let tests : (string * (unit -> TestResult)) list = [
    ("Interpolated expressions retain lexer terminator", testInterpolatedExpressionsKeepLexerTerminator)
]
