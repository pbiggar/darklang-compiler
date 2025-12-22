module Parser

open AST

/// Token types for lexer
type Token =
    | TInt of int64
    | TPlus
    | TMinus
    | TStar
    | TSlash
    | TLParen
    | TRParen
    | TEOF

/// Lexer: convert string to list of tokens
let lex (input: string) : Token list =
    // To be implemented
    []

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Program =
    // To be implemented
    Program (IntLiteral 0L)

/// Parse a string directly to AST
let parseString (input: string) : Program =
    input |> lex |> parse
