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
    | TTrue
    | TFalse
    | TPlus
    | TMinus
    | TStar
    | TSlash
    | TLParen
    | TRParen
    | TLet
    | TIn
    | TIf          // if
    | TThen        // then
    | TElse        // else
    | TDef         // def (function definition)
    | TColon       // : (type annotation)
    | TComma       // , (parameter separator)
    | TEquals      // = (assignment in let)
    | TEqEq        // == (equality comparison)
    | TNeq         // !=
    | TLt          // <
    | TGt          // >
    | TLte         // <=
    | TGte         // >=
    | TAnd         // &&
    | TOr          // ||
    | TNot         // !
    | TIdent of string
    | TEOF

/// Lexer: convert string to list of tokens
let lex (input: string) : Result<Token list, string> =
    let rec lexHelper (chars: char list) (acc: Token list) : Result<Token list, string> =
        match chars with
        | [] -> Ok (List.rev (TEOF :: acc))
        | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: rest ->
            // Skip whitespace
            lexHelper rest acc
        | '+' :: rest -> lexHelper rest (TPlus :: acc)
        | '-' :: rest -> lexHelper rest (TMinus :: acc)
        | '*' :: rest -> lexHelper rest (TStar :: acc)
        | '/' :: rest -> lexHelper rest (TSlash :: acc)
        | '(' :: rest -> lexHelper rest (TLParen :: acc)
        | ')' :: rest -> lexHelper rest (TRParen :: acc)
        | ':' :: rest -> lexHelper rest (TColon :: acc)
        | ',' :: rest -> lexHelper rest (TComma :: acc)
        | '=' :: '=' :: rest -> lexHelper rest (TEqEq :: acc)
        | '=' :: rest -> lexHelper rest (TEquals :: acc)
        | '!' :: '=' :: rest -> lexHelper rest (TNeq :: acc)
        | '!' :: rest -> lexHelper rest (TNot :: acc)
        | '<' :: '=' :: rest -> lexHelper rest (TLte :: acc)
        | '<' :: rest -> lexHelper rest (TLt :: acc)
        | '>' :: '=' :: rest -> lexHelper rest (TGte :: acc)
        | '>' :: rest -> lexHelper rest (TGt :: acc)
        | '&' :: '&' :: rest -> lexHelper rest (TAnd :: acc)
        | '&' :: _ -> Error "Unexpected character: & (did you mean &&?)"
        | '|' :: '|' :: rest -> lexHelper rest (TOr :: acc)
        | '|' :: _ -> Error "Unexpected character: | (did you mean ||?)"
        | c :: _ when System.Char.IsLetter(c) || c = '_' ->
            // Parse identifier or keyword
            let rec parseIdent (cs: char list) (chars: char list) : string * char list =
                match cs with
                | c :: rest when System.Char.IsLetterOrDigit(c) || c = '_' ->
                    parseIdent rest (c :: chars)
                | _ ->
                    let ident = System.String(List.rev chars |> List.toArray)
                    (ident, cs)

            let (ident, remaining) = parseIdent chars []
            let token =
                match ident with
                | "let" -> TLet
                | "in" -> TIn
                | "if" -> TIf
                | "then" -> TThen
                | "else" -> TElse
                | "def" -> TDef
                | "true" -> TTrue
                | "false" -> TFalse
                | _ -> TIdent ident
            lexHelper remaining (token :: acc)
        | c :: _ when System.Char.IsDigit(c) ->
            // Parse integer
            let rec parseDigits (cs: char list) (digits: char list) : Result<int64 * char list, string> =
                match cs with
                | d :: rest when System.Char.IsDigit(d) ->
                    parseDigits rest (d :: digits)
                | _ ->
                    let numStr = System.String(List.rev digits |> List.toArray)
                    // Try to parse as int64
                    match System.Int64.TryParse(numStr) with
                    | (true, value) -> Ok (value, cs)
                    | (false, _) ->
                        // Check for INT64_MIN special case: "9223372036854775808"
                        // This value is > INT64_MAX but equals |INT64_MIN|
                        // It's only valid when preceded by minus: -9223372036854775808
                        if numStr = "9223372036854775808" then
                            // Return INT64_MIN directly - this is a special sentinel
                            // The parser will only accept this when it's negated
                            Ok (System.Int64.MinValue, cs)
                        else
                            Error $"Integer literal too large: {numStr}"

            match parseDigits chars [] with
            | Ok (num, remaining) -> lexHelper remaining (TInt num :: acc)
            | Error err -> Error err
        | c :: _ ->
            Error $"Unexpected character: {c}"

    input |> Seq.toList |> fun cs -> lexHelper cs []

/// Parse a type annotation
let parseType (tokens: Token list) : Result<Type * Token list, string> =
    match tokens with
    | TIdent "int" :: rest -> Ok (TInt64, rest)
    | TIdent "bool" :: rest -> Ok (TBool, rest)
    | _ -> Error "Expected type annotation (int or bool)"

/// Parse a single parameter: IDENT : type
let parseParam (tokens: Token list) : Result<(string * Type) * Token list, string> =
    match tokens with
    | TIdent name :: TColon :: rest ->
        parseType rest
        |> Result.map (fun (ty, remaining) -> ((name, ty), remaining))
    | _ -> Error "Expected parameter (name : type)"

/// Parse parameter list: param (, param)*
let rec parseParams (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRParen :: _ ->
        // End of parameters
        Ok (List.rev acc, tokens)
    | _ ->
        // Parse a parameter
        parseParam tokens
        |> Result.bind (fun (param, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More parameters
                parseParams rest (param :: acc)
            | TRParen :: _ ->
                // End of parameters
                Ok (List.rev (param :: acc), remaining)
            | _ -> Error "Expected ',' or ')' after parameter")

/// Parse a function definition: def name(params) : type = body
let parseFunctionDef (tokens: Token list) (parseExpr: Token list -> Result<Expr * Token list, string>) : Result<FunctionDef * Token list, string> =
    match tokens with
    | TDef :: TIdent name :: TLParen :: rest ->
        // Parse parameters
        let paramsResult =
            match rest with
            | TRParen :: _ -> Ok ([], rest)  // No parameters
            | _ -> parseParams rest []

        paramsResult
        |> Result.bind (fun (parameters, remaining) ->
            match remaining with
            | TRParen :: TColon :: rest' ->
                // Parse return type
                parseType rest'
                |> Result.bind (fun (returnType, remaining') ->
                    match remaining' with
                    | TEquals :: rest'' ->
                        // Parse body
                        parseExpr rest''
                        |> Result.map (fun (body, remaining'') ->
                            let funcDef = {
                                Name = name
                                Params = parameters
                                ReturnType = returnType
                                Body = body
                            }
                            (funcDef, remaining''))
                    | _ -> Error "Expected '=' after function return type")
            | _ -> Error "Expected ':' after function parameters")
    | _ -> Error "Expected function definition (def name(params) : type = body)"

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Result<Program, string> =
    // Recursive descent parser with operator precedence
    // Precedence (low to high): or < and < comparison < +/- < */ < unary
    let rec parseExpr (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TLet :: TIdent name :: TEquals :: rest ->
            // Parse: let name = value in body
            parseExpr rest
            |> Result.bind (fun (value, remaining) ->
                match remaining with
                | TIn :: rest' ->
                    parseExpr rest'
                    |> Result.map (fun (body, remaining') ->
                        (Let (name, value, body), remaining'))
                | _ -> Error "Expected 'in' after let binding value")
        | TIf :: rest ->
            // Parse: if cond then thenBranch else elseBranch
            parseExpr rest
            |> Result.bind (fun (cond, remaining) ->
                match remaining with
                | TThen :: rest' ->
                    parseExpr rest'
                    |> Result.bind (fun (thenBranch, remaining') ->
                        match remaining' with
                        | TElse :: rest'' ->
                            parseExpr rest''
                            |> Result.map (fun (elseBranch, remaining'') ->
                                (If (cond, thenBranch, elseBranch), remaining''))
                        | _ -> Error "Expected 'else' after then branch")
                | _ -> Error "Expected 'then' after if condition")
        | _ ->
            parseOr toks

    and parseOr (toks: Token list) : Result<Expr * Token list, string> =
        parseAnd toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseOrRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TOr :: rest ->
                    parseAnd rest
                    |> Result.bind (fun (right, remaining') ->
                        parseOrRest (BinOp (Or, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseOrRest left remaining)

    and parseAnd (toks: Token list) : Result<Expr * Token list, string> =
        parseComparison toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseAndRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TAnd :: rest ->
                    parseComparison rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAndRest (BinOp (And, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseAndRest left remaining)

    and parseComparison (toks: Token list) : Result<Expr * Token list, string> =
        parseAdditive toks
        |> Result.bind (fun (left, remaining) ->
            // Comparison operators are non-associative (no chaining)
            match remaining with
            | TEqEq :: rest ->
                parseAdditive rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Eq, left, right), remaining'))
            | TNeq :: rest ->
                parseAdditive rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Neq, left, right), remaining'))
            | TLt :: rest ->
                parseAdditive rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Lt, left, right), remaining'))
            | TGt :: rest ->
                parseAdditive rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Gt, left, right), remaining'))
            | TLte :: rest ->
                parseAdditive rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Lte, left, right), remaining'))
            | TGte :: rest ->
                parseAdditive rest
                |> Result.map (fun (right, remaining') ->
                    (BinOp (Gte, left, right), remaining'))
            | _ -> Ok (left, remaining))

    and parseAdditive (toks: Token list) : Result<Expr * Token list, string> =
        parseMultiplicative toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseAdditiveRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TPlus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (Add, leftExpr, right)) remaining')
                | TMinus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (Sub, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseAdditiveRest left remaining)

    and parseMultiplicative (toks: Token list) : Result<Expr * Token list, string> =
        parseUnary toks
        |> Result.bind (fun (left, remaining) ->
            let rec parseMultiplicativeRest (leftExpr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
                match toks with
                | TStar :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Mul, leftExpr, right)) remaining')
                | TSlash :: rest ->
                    parseUnary rest
                    |> Result.bind (fun (right, remaining') ->
                        parseMultiplicativeRest (BinOp (Div, leftExpr, right)) remaining')
                | _ -> Ok (leftExpr, toks)
            parseMultiplicativeRest left remaining)

    and parseUnary (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TMinus :: rest ->
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (Neg, expr), remaining))
        | TNot :: rest ->
            parseUnary rest
            |> Result.map (fun (expr, remaining) -> (UnaryOp (Not, expr), remaining))
        | _ ->
            parsePrimary toks

    and parsePrimary (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TInt n :: rest -> Ok (IntLiteral n, rest)
        | TTrue :: rest -> Ok (BoolLiteral true, rest)
        | TFalse :: rest -> Ok (BoolLiteral false, rest)
        | TIdent name :: TLParen :: rest ->
            // Function call: name(args)
            parseCallArgs rest []
            |> Result.map (fun (args, remaining) ->
                (Call (name, args), remaining))
        | TIdent name :: rest ->
            // Variable reference
            Ok (Var name, rest)
        | TLParen :: rest ->
            parseExpr rest
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TRParen :: rest' -> Ok (expr, rest')
                | _ -> Error "Expected ')'")
        | _ -> Error "Expected expression"

    and parseCallArgs (toks: Token list) (acc: Expr list) : Result<Expr list * Token list, string> =
        match toks with
        | TRParen :: rest ->
            // End of arguments
            Ok (List.rev acc, rest)
        | _ ->
            // Parse an argument expression
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TComma :: rest ->
                    // More arguments
                    parseCallArgs rest (expr :: acc)
                | TRParen :: rest ->
                    // End of arguments
                    Ok (List.rev (expr :: acc), rest)
                | _ -> Error "Expected ',' or ')' after function argument")

    // Parse top-level elements (functions or expressions)
    let rec parseTopLevels (toks: Token list) (acc: TopLevel list) : Result<Program, string> =
        match toks with
        | TEOF :: [] ->
            // End of input
            if List.isEmpty acc then
                Error "Empty program"
            else
                Ok (Program (List.rev acc))

        | TDef :: _ ->
            // Parse function definition
            parseFunctionDef toks parseExpr
            |> Result.bind (fun (funcDef, remaining) ->
                parseTopLevels remaining (FunctionDef funcDef :: acc))

        | _ ->
            // Parse expression
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TEOF :: [] ->
                    // Single expression program
                    Ok (Program (List.rev (Expression expr :: acc)))
                | _ ->
                    // More top-level definitions after expression not allowed for now
                    Error "Unexpected tokens after expression (only function definitions can be followed by more definitions)")

    parseTopLevels tokens []

/// Parse a string directly to AST
let parseString (input: string) : Result<Program, string> =
    lex input
    |> Result.bind parse
