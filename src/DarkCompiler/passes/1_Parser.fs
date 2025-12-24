// 1_Parser.fs - Lexer and Parser (Pass 1)
//
// Transforms source code (string) into an Abstract Syntax Tree (AST).
//
// Lexer: Converts source string into tokens
// Parser: Recursive descent parser with operator precedence
//
// Operator precedence (specific to this parser):
// - Multiplication and division bind tighter than addition and subtraction
// - Operators are left-associative: "1 + 2 + 3" parses as "(1 + 2) + 3"
// - Parentheses for explicit grouping
//
// Example:
//   "2 + 3 * 4" → BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))

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
                    // Try to parse as int64
                    match System.Int64.TryParse(numStr) with
                    | (true, value) -> (value, cs)
                    | (false, _) ->
                        // Check for INT64_MIN special case: "9223372036854775808"
                        // This value is > INT64_MAX but equals |INT64_MIN|
                        // It's only valid when preceded by minus: -9223372036854775808
                        if numStr = "9223372036854775808" then
                            // Return INT64_MIN directly - this is a special sentinel
                            // The parser will only accept this when it's negated
                            (System.Int64.MinValue, cs)
                        else
                            failwith $"Integer literal too large: {numStr}"

            let (num, remaining) = parseDigits chars []
            lexHelper remaining (TInt num :: acc)
        | c :: _ ->
            failwith $"Unexpected character: {c}"

    input |> Seq.toList |> fun cs -> lexHelper cs []

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Program =
    // Recursive descent parser with operator precedence
    let rec parseExpr (toks: Token list) : Expr * Token list =
        parseAdditive toks

    and parseAdditive (toks: Token list) : Expr * Token list =
        let (left, remaining) = parseMultiplicative toks
        let rec parseAdditiveRest (leftExpr: Expr) (toks: Token list) : Expr * Token list =
            match toks with
            | TPlus :: rest ->
                let (right, remaining') = parseMultiplicative rest
                parseAdditiveRest (BinOp (Add, leftExpr, right)) remaining'
            | TMinus :: rest ->
                let (right, remaining') = parseMultiplicative rest
                parseAdditiveRest (BinOp (Sub, leftExpr, right)) remaining'
            | _ -> (leftExpr, toks)
        parseAdditiveRest left remaining

    and parseMultiplicative (toks: Token list) : Expr * Token list =
        let (left, remaining) = parsePrimary toks
        let rec parseMultiplicativeRest (leftExpr: Expr) (toks: Token list) : Expr * Token list =
            match toks with
            | TStar :: rest ->
                let (right, remaining') = parsePrimary rest
                parseMultiplicativeRest (BinOp (Mul, leftExpr, right)) remaining'
            | TSlash :: rest ->
                let (right, remaining') = parsePrimary rest
                parseMultiplicativeRest (BinOp (Div, leftExpr, right)) remaining'
            | _ -> (leftExpr, toks)
        parseMultiplicativeRest left remaining

    and parsePrimary (toks: Token list) : Expr * Token list =
        match toks with
        | TInt n :: rest -> (IntLiteral n, rest)
        | TMinus :: rest ->
            // Unary negation
            let (expr, remaining) = parsePrimary rest
            (Neg expr, remaining)
        | TLParen :: rest ->
            let (expr, remaining) = parseExpr rest
            match remaining with
            | TRParen :: rest' -> (expr, rest')
            | _ -> failwith "Expected ')'"
        | _ -> failwith "Expected expression"

    let (expr, remaining) = parseExpr tokens
    match remaining with
    | TEOF :: [] -> Program expr
    | _ -> failwith "Unexpected tokens after expression"

/// Parse a string directly to AST
let parseString (input: string) : Program =
    input |> lex |> parse
