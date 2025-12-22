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
    let rec lexHelper (chars: char list) (acc: Token list) : Token list =
        match chars with
        | [] -> List.rev (TEOF :: acc)
        | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: rest ->
            // Skip whitespace
            lexHelper rest acc
        | '+' :: rest -> lexHelper rest (TPlus :: acc)
        | '-' :: rest -> lexHelper rest (TMinus :: acc)
        | '*' :: rest -> lexHelper rest (TStar :: acc)
        | '/' :: rest -> lexHelper rest (TSlash :: acc)
        | '(' :: rest -> lexHelper rest (TLParen :: acc)
        | ')' :: rest -> lexHelper rest (TRParen :: acc)
        | c :: _ when System.Char.IsDigit(c) ->
            // Parse integer
            let rec parseDigits (cs: char list) (digits: char list) : int64 * char list =
                match cs with
                | d :: rest when System.Char.IsDigit(d) ->
                    parseDigits rest (d :: digits)
                | _ ->
                    let numStr = System.String(List.rev digits |> List.toArray)
                    (System.Int64.Parse(numStr), cs)

            let (num, remaining) = parseDigits chars []
            lexHelper remaining (TInt num :: acc)
        | c :: _ ->
            failwith $"Unexpected character: {c}"

    input |> Seq.toList |> fun cs -> lexHelper cs []

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Program =
    // To be implemented
    Program (IntLiteral 0L)

/// Parse a string directly to AST
let parseString (input: string) : Program =
    input |> lex |> parse
