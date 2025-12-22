// 1_Parser.fs - Lexer and Parser (Pass 1)
//
// Transforms source code (string) into an Abstract Syntax Tree (AST).
//
// Components:
// - Lexer: Converts source string into a list of tokens
// - Parser: Recursive descent parser that builds AST from tokens
//
// Operator precedence:
// - Multiplication and division bind tighter than addition and subtraction
// - Operators are left-associative: "1 + 2 + 3" parses as "(1 + 2) + 3"
// - Parentheses for explicit grouping
//
// Input: Source code (string)
// Output: AST
//
// Examples:
//   Input:  "3 + 3 * 4"
//   Output: BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))

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
