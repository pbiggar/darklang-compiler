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
    | TFloat of float
    | TStringLit of string  // String literal token (named to avoid conflict with AST.TString type)
    | TTrue
    | TFalse
    | TPlus
    | TPlusPlus    // ++ (string concatenation)
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
    | TType        // type (type definition)
    | TColon       // : (type annotation)
    | TComma       // , (parameter separator)
    | TDot         // . (tuple/record access)
    | TLBrace      // { (record literal)
    | TRBrace      // } (record literal)
    | TBar         // | (sum type variant separator / pattern separator)
    | TOf          // of (sum type payload)
    | TMatch       // match (pattern matching)
    | TWith        // with (pattern matching)
    | TArrow       // -> (pattern matching)
    | TFatArrow    // => (lambda)
    | TUnderscore  // _ (wildcard pattern)
    | TLBracket    // [ (list literal)
    | TRBracket    // ] (list literal)
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
    | TDotDotDot    // ... (rest pattern in lists)
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
        | '+' :: '+' :: rest -> lexHelper rest (TPlusPlus :: acc)
        | '+' :: rest -> lexHelper rest (TPlus :: acc)
        | '-' :: '>' :: rest -> lexHelper rest (TArrow :: acc)
        | '-' :: rest -> lexHelper rest (TMinus :: acc)
        | '=' :: '>' :: rest -> lexHelper rest (TFatArrow :: acc)
        | '*' :: rest -> lexHelper rest (TStar :: acc)
        | '/' :: rest -> lexHelper rest (TSlash :: acc)
        | '(' :: rest -> lexHelper rest (TLParen :: acc)
        | ')' :: rest -> lexHelper rest (TRParen :: acc)
        | '{' :: rest -> lexHelper rest (TLBrace :: acc)
        | '}' :: rest -> lexHelper rest (TRBrace :: acc)
        | '[' :: rest -> lexHelper rest (TLBracket :: acc)
        | ']' :: rest -> lexHelper rest (TRBracket :: acc)
        | ':' :: rest -> lexHelper rest (TColon :: acc)
        | ',' :: rest -> lexHelper rest (TComma :: acc)
        | '.' :: '.' :: '.' :: rest -> lexHelper rest (TDotDotDot :: acc)
        | '.' :: rest -> lexHelper rest (TDot :: acc)
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
        | '|' :: rest -> lexHelper rest (TBar :: acc)
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
                | "type" -> TType
                | "of" -> TOf
                | "match" -> TMatch
                | "with" -> TWith
                | "true" -> TTrue
                | "false" -> TFalse
                | "_" -> TUnderscore
                | _ -> TIdent ident
            lexHelper remaining (token :: acc)
        | c :: _ when System.Char.IsDigit(c) ->
            // Parse number (integer or float)
            // First collect all digits
            let rec collectDigits (cs: char list) (acc: char list) : char list * char list =
                match cs with
                | d :: rest when System.Char.IsDigit(d) -> collectDigits rest (d :: acc)
                | _ -> (List.rev acc, cs)

            let (intDigits, afterInt) = collectDigits chars []

            // Check if this is a float (has decimal point or exponent)
            match afterInt with
            | '.' :: rest when not (List.isEmpty rest) && System.Char.IsDigit(List.head rest) ->
                // Float with decimal point: 3.14
                let (fracDigits, afterFrac) = collectDigits rest []
                // Check for exponent
                match afterFrac with
                | ('e' :: rest' | 'E' :: rest') ->
                    // Scientific notation: 3.14e10 or 3.14e-10
                    let (expSign, afterSign) =
                        match rest' with
                        | '+' :: r -> (['+'], r)
                        | '-' :: r -> (['-'], r)
                        | _ -> ([], rest')
                    let (expDigits, remaining) = collectDigits afterSign []
                    if List.isEmpty expDigits then
                        Error "Expected exponent digits after 'e'"
                    else
                        let numStr = System.String(Array.ofList (intDigits @ ['.'] @ fracDigits @ ['e'] @ expSign @ expDigits))
                        match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                        | (true, value) -> lexHelper remaining (TFloat value :: acc)
                        | (false, _) -> Error $"Invalid float literal: {numStr}"
                | _ ->
                    // Float without exponent: 3.14
                    let numStr = System.String(Array.ofList (intDigits @ ['.'] @ fracDigits))
                    match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> lexHelper afterFrac (TFloat value :: acc)
                    | (false, _) -> Error $"Invalid float literal: {numStr}"
            | ('e' :: rest | 'E' :: rest) ->
                // Scientific notation without decimal: 1e10 or 1e-10
                let (expSign, afterSign) =
                    match rest with
                    | '+' :: r -> (['+'], r)
                    | '-' :: r -> (['-'], r)
                    | _ -> ([], rest)
                let (expDigits, remaining) = collectDigits afterSign []
                if List.isEmpty expDigits then
                    Error "Expected exponent digits after 'e'"
                else
                    let numStr = System.String(Array.ofList (intDigits @ ['e'] @ expSign @ expDigits))
                    match System.Double.TryParse(numStr, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | (true, value) -> lexHelper remaining (TFloat value :: acc)
                    | (false, _) -> Error $"Invalid float literal: {numStr}"
            | _ ->
                // Integer
                let numStr = System.String(List.toArray intDigits)
                // Try to parse as int64
                match System.Int64.TryParse(numStr) with
                | (true, value) -> lexHelper afterInt (TInt value :: acc)
                | (false, _) ->
                    // Check for INT64_MIN special case: "9223372036854775808"
                    // This value is > INT64_MAX but equals |INT64_MIN|
                    // It's only valid when preceded by minus: -9223372036854775808
                    if numStr = "9223372036854775808" then
                        // Return INT64_MIN directly - this is a special sentinel
                        // The parser will only accept this when it's negated
                        lexHelper afterInt (TInt System.Int64.MinValue :: acc)
                    else
                        Error $"Integer literal too large: {numStr}"
        | '"' :: rest ->
            // Parse string literal with escape sequences
            let rec parseString (cs: char list) (chars: char list) : Result<string * char list, string> =
                match cs with
                | [] -> Error "Unterminated string literal"
                | '"' :: remaining ->
                    // End of string
                    let str = System.String(List.rev chars |> List.toArray)
                    Ok (str, remaining)
                | '\\' :: 'n' :: remaining ->
                    parseString remaining ('\n' :: chars)
                | '\\' :: 't' :: remaining ->
                    parseString remaining ('\t' :: chars)
                | '\\' :: 'r' :: remaining ->
                    parseString remaining ('\r' :: chars)
                | '\\' :: '\\' :: remaining ->
                    parseString remaining ('\\' :: chars)
                | '\\' :: '"' :: remaining ->
                    parseString remaining ('"' :: chars)
                | '\\' :: '0' :: remaining ->
                    parseString remaining ('\000' :: chars)
                | '\\' :: 'x' :: h1 :: h2 :: remaining ->
                    // Hex escape: \xNN
                    let hexStr = System.String([| h1; h2 |])
                    match System.Int32.TryParse(hexStr, System.Globalization.NumberStyles.HexNumber, null) with
                    | (true, value) ->
                        parseString remaining (char value :: chars)
                    | (false, _) ->
                        Error $"Invalid hex escape sequence: \\x{hexStr}"
                | '\\' :: c :: _ ->
                    Error $"Unknown escape sequence: \\{c}"
                | c :: remaining ->
                    parseString remaining (c :: chars)

            match parseString rest [] with
            | Ok (str, remaining) -> lexHelper remaining (TStringLit str :: acc)
            | Error err -> Error err
        | c :: _ ->
            Error $"Unexpected character: {c}"

    input |> Seq.toList |> fun cs -> lexHelper cs []

/// Parse function type parameters: type, type, ...) returning list and remaining tokens
let rec parseFunctionTypeParams (typeParams: Set<string>) (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    match tokens with
    | TRParen :: rest ->
        // End of parameter list
        Ok (List.rev acc, rest)
    | _ ->
        // Parse a type
        parseTypeBase typeParams tokens
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TRParen :: rest -> Ok (List.rev (ty :: acc), rest)
            | TComma :: rest -> parseFunctionTypeParams typeParams rest (ty :: acc)
            | _ -> Error "Expected ',' or ')' in function type parameters")

/// Base type parser (no function types - used to parse function type components)
and parseTypeBase (typeParams: Set<string>) (tokens: Token list) : Result<Type * Token list, string> =
    match tokens with
    | TIdent "int" :: rest -> Ok (TInt64, rest)
    | TIdent "bool" :: rest -> Ok (TBool, rest)
    | TIdent "string" :: rest -> Ok (TString, rest)
    | TIdent "float" :: rest -> Ok (TFloat64, rest)
    | TIdent typeName :: rest when Set.contains typeName typeParams ->
        Ok (TVar typeName, rest)
    | TIdent typeName :: rest when System.Char.IsUpper(typeName.[0]) ->
        Ok (TRecord typeName, rest)
    | TLParen :: rest ->
        // Could be a function type: (int, int) -> bool
        parseFunctionTypeParams typeParams rest []
        |> Result.bind (fun (paramTypes, afterParams) ->
            match afterParams with
            | TArrow :: returnRest ->
                parseTypeWithContext typeParams returnRest
                |> Result.map (fun (returnType, remaining) ->
                    (TFunction (paramTypes, returnType), remaining))
            | _ -> Error "Expected '->' after function type parameters")
    | _ -> Error "Expected type annotation (int, bool, string, float, TypeName, type variable, or function type)"

/// Parse a type annotation with context for type parameters in scope
and parseTypeWithContext (typeParams: Set<string>) (tokens: Token list) : Result<Type * Token list, string> =
    parseTypeBase typeParams tokens

/// Parse a type annotation (no type parameters in scope)
let parseType (tokens: Token list) : Result<Type * Token list, string> =
    parseTypeWithContext Set.empty tokens

/// Parse type parameters: <T, U, V> (names only, for function definitions)
let rec parseTypeParams (tokens: Token list) (acc: string list) : Result<string list * Token list, string> =
    match tokens with
    | TIdent name :: TGt :: rest when System.Char.IsUpper(name.[0]) ->
        // Last type parameter
        Ok (List.rev (name :: acc), rest)
    | TIdent name :: TComma :: rest when System.Char.IsUpper(name.[0]) ->
        // More type parameters to come
        parseTypeParams rest (name :: acc)
    | TIdent name :: _ when not (System.Char.IsUpper(name.[0])) ->
        Error $"Type parameter must start with uppercase letter: {name}"
    | TGt :: rest when List.isEmpty acc ->
        // Empty type parameters: <>
        Ok ([], rest)
    | _ -> Error "Expected type parameter name (uppercase identifier)"

/// Parse type arguments: <int, bool, Point> (concrete types, for call sites)
let rec parseTypeArgs (tokens: Token list) (acc: Type list) : Result<Type list * Token list, string> =
    parseType tokens
    |> Result.bind (fun (ty, remaining) ->
        match remaining with
        | TGt :: rest ->
            // Last type argument
            Ok (List.rev (ty :: acc), rest)
        | TComma :: rest ->
            // More type arguments to come
            parseTypeArgs rest (ty :: acc)
        | _ -> Error "Expected ',' or '>' after type argument")

/// Parse a single parameter: IDENT : type (with type parameter context)
let parseParamWithContext (typeParams: Set<string>) (tokens: Token list) : Result<(string * Type) * Token list, string> =
    match tokens with
    | TIdent name :: TColon :: rest ->
        parseTypeWithContext typeParams rest
        |> Result.map (fun (ty, remaining) -> ((name, ty), remaining))
    | _ -> Error "Expected parameter (name : type)"

/// Parse a single parameter: IDENT : type (no type parameters in scope)
let parseParam (tokens: Token list) : Result<(string * Type) * Token list, string> =
    parseParamWithContext Set.empty tokens

/// Parse parameter list: param (, param)* (with type parameter context)
let rec parseParamsWithContext (typeParams: Set<string>) (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRParen :: _ ->
        // End of parameters
        Ok (List.rev acc, tokens)
    | _ ->
        // Parse a parameter
        parseParamWithContext typeParams tokens
        |> Result.bind (fun (param, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More parameters
                parseParamsWithContext typeParams rest (param :: acc)
            | TRParen :: _ ->
                // End of parameters
                Ok (List.rev (param :: acc), remaining)
            | _ -> Error "Expected ',' or ')' after parameter")

/// Parse parameter list: param (, param)* (no type parameters in scope)
let rec parseParams (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    parseParamsWithContext Set.empty tokens acc

/// Parse record fields in a type definition: { name: Type, name: Type, ... }
let rec parseRecordFields (tokens: Token list) (acc: (string * Type) list) : Result<(string * Type) list * Token list, string> =
    match tokens with
    | TRBrace :: rest ->
        // End of fields
        Ok (List.rev acc, rest)
    | TIdent name :: TColon :: rest ->
        parseType rest
        |> Result.bind (fun (ty, remaining) ->
            match remaining with
            | TComma :: rest' ->
                // More fields
                parseRecordFields rest' ((name, ty) :: acc)
            | TRBrace :: rest' ->
                // End of fields
                Ok (List.rev ((name, ty) :: acc), rest')
            | _ -> Error "Expected ',' or '}' after record field")
    | _ -> Error "Expected field name in record definition"

/// Parse sum type variants: Variant1 | Variant2 of Type | ...
/// Returns list of variants and remaining tokens
let rec parseVariants (tokens: Token list) (acc: Variant list) : Result<Variant list * Token list, string> =
    match tokens with
    | TIdent variantName :: TOf :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Variant with payload: Variant of Type
        parseType rest
        |> Result.bind (fun (payloadType, afterType) ->
            let variant = { Name = variantName; Payload = Some payloadType }
            match afterType with
            | TBar :: rest' ->
                // More variants
                parseVariants rest' (variant :: acc)
            | _ ->
                // End of variants
                Ok (List.rev (variant :: acc), afterType))
    | TIdent variantName :: rest when System.Char.IsUpper(variantName.[0]) ->
        // Simple enum variant (no payload)
        let variant = { Name = variantName; Payload = None }
        match rest with
        | TBar :: rest' ->
            // More variants
            parseVariants rest' (variant :: acc)
        | _ ->
            // End of variants (next token is not a bar)
            Ok (List.rev (variant :: acc), rest)
    | _ -> Error "Expected variant name (must start with uppercase letter)"

/// Parse a type definition: type Name = { fields } or type Name = Variant1 | Variant2 of Type | ...
let parseTypeDef (tokens: Token list) : Result<TypeDef * Token list, string> =
    match tokens with
    | TType :: TIdent name :: TEquals :: TLBrace :: rest when System.Char.IsUpper(name.[0]) ->
        // Record type: type Name = { field: Type, ... }
        parseRecordFields rest []
        |> Result.map (fun (fields, remaining) ->
            (RecordDef (name, fields), remaining))
    | TType :: TIdent name :: TEquals :: TIdent variantName :: TOf :: rest when System.Char.IsUpper(name.[0]) && System.Char.IsUpper(variantName.[0]) ->
        // Sum type with first variant having payload: type Name = Variant of Type | ...
        parseType rest
        |> Result.bind (fun (payloadType, afterType) ->
            let firstVariant = { Name = variantName; Payload = Some payloadType }
            match afterType with
            | TBar :: rest' ->
                // More variants
                parseVariants rest' [firstVariant]
                |> Result.map (fun (variants, remaining) ->
                    (SumTypeDef (name, variants), remaining))
            | _ ->
                // Single variant sum type
                Ok (SumTypeDef (name, [firstVariant]), afterType))
    | TType :: TIdent name :: TEquals :: TIdent variantName :: rest when System.Char.IsUpper(name.[0]) && System.Char.IsUpper(variantName.[0]) ->
        // Sum type: type Name = Variant1 | Variant2 | ...
        let firstVariant = { Name = variantName; Payload = None }
        match rest with
        | TBar :: rest' ->
            // More variants
            parseVariants rest' [firstVariant]
            |> Result.map (fun (variants, remaining) ->
                (SumTypeDef (name, variants), remaining))
        | _ ->
            // Single variant sum type
            Ok (SumTypeDef (name, [firstVariant]), rest)
    | TType :: TIdent name :: _ when not (System.Char.IsUpper(name.[0])) ->
        Error $"Type name must start with uppercase letter: {name}"
    | _ -> Error "Expected type definition: type Name = { fields } or type Name = Variant1 | Variant2"

/// Parse a function definition: def name<T, U>(params) : type = body
/// Type parameters are optional: def name(params) : type = body is also valid
let parseFunctionDef (tokens: Token list) (parseExpr: Token list -> Result<Expr * Token list, string>) : Result<FunctionDef * Token list, string> =
    match tokens with
    | TDef :: TIdent name :: TLt :: rest ->
        // Generic function: def name<T, U>(...)
        parseTypeParams rest []
        |> Result.bind (fun (typeParams, afterTypeParams) ->
            // Check for duplicate type parameters
            if List.length typeParams <> (typeParams |> List.distinct |> List.length) then
                Error "Duplicate type parameter names"
            else
            let typeParamsSet = Set.ofList typeParams
            match afterTypeParams with
            | TLParen :: paramsStart ->
                // Parse parameters with type params in scope
                let paramsResult =
                    match paramsStart with
                    | TRParen :: _ -> Ok ([], paramsStart)
                    | _ -> parseParamsWithContext typeParamsSet paramsStart []

                paramsResult
                |> Result.bind (fun (parameters, remaining) ->
                    match remaining with
                    | TRParen :: TColon :: rest' ->
                        // Parse return type with type params in scope
                        parseTypeWithContext typeParamsSet rest'
                        |> Result.bind (fun (returnType, remaining') ->
                            match remaining' with
                            | TEquals :: rest'' ->
                                // Parse body
                                parseExpr rest''
                                |> Result.map (fun (body, remaining'') ->
                                    let funcDef = {
                                        Name = name
                                        TypeParams = typeParams
                                        Params = parameters
                                        ReturnType = returnType
                                        Body = body
                                    }
                                    (funcDef, remaining''))
                            | _ -> Error "Expected '=' after function return type")
                    | _ -> Error "Expected ':' after function parameters")
            | _ -> Error "Expected '(' after type parameters")
    | TDef :: TIdent name :: TLParen :: rest ->
        // Non-generic function: def name(...)
        let paramsResult =
            match rest with
            | TRParen :: _ -> Ok ([], rest)
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
                                TypeParams = []
                                Params = parameters
                                ReturnType = returnType
                                Body = body
                            }
                            (funcDef, remaining''))
                    | _ -> Error "Expected '=' after function return type")
            | _ -> Error "Expected ':' after function parameters")
    | _ -> Error "Expected function definition (def name(params) : type = body)"

/// Parse a pattern for pattern matching
let rec parsePattern (tokens: Token list) : Result<Pattern * Token list, string> =
    match tokens with
    | TUnderscore :: rest ->
        // Wildcard pattern: _
        Ok (PWildcard, rest)
    | TInt n :: rest ->
        // Integer literal pattern
        Ok (PLiteral n, rest)
    | TTrue :: rest ->
        // Boolean true pattern
        Ok (PBool true, rest)
    | TFalse :: rest ->
        // Boolean false pattern
        Ok (PBool false, rest)
    | TStringLit s :: rest ->
        // String literal pattern
        Ok (PString s, rest)
    | TFloat f :: rest ->
        // Float literal pattern
        Ok (PFloat f, rest)
    | TLParen :: rest ->
        // Tuple pattern: (a, b, c)
        parseTuplePattern rest []
    | TLBrace :: _ ->
        // Anonymous record pattern is no longer supported
        Error "Record pattern requires type name: use 'TypeName { field = pattern, ... }'"
    | TLBracket :: rest ->
        // List pattern: [a, b, c] or []
        parseListPattern rest []
    | TIdent name :: TLParen :: rest when System.Char.IsUpper(name.[0]) ->
        // Constructor with payload pattern: Some(x)
        parsePattern rest
        |> Result.bind (fun (payloadPattern, remaining) ->
            match remaining with
            | TRParen :: rest' ->
                Ok (PConstructor (name, Some payloadPattern), rest')
            | _ -> Error "Expected ')' after constructor pattern payload")
    | TIdent typeName :: TLBrace :: rest when System.Char.IsUpper(typeName.[0]) ->
        // Record pattern with type name: Point { x = a, y = b }
        parseRecordPatternWithTypeName typeName rest []
    | TIdent name :: rest when System.Char.IsUpper(name.[0]) ->
        // Constructor pattern without payload: Red, None
        Ok (PConstructor (name, None), rest)
    | TIdent name :: rest ->
        // Variable pattern: x (binds the value)
        Ok (PVar name, rest)
    | _ -> Error "Expected pattern (_, variable, literal, or constructor)"

and parseTuplePattern (tokens: Token list) (acc: Pattern list) : Result<Pattern * Token list, string> =
    parsePattern tokens
    |> Result.bind (fun (pat, remaining) ->
        match remaining with
        | TRParen :: rest ->
            // End of tuple pattern
            let patterns = List.rev (pat :: acc)
            Ok (PTuple patterns, rest)
        | TComma :: rest ->
            // More elements
            parseTuplePattern rest (pat :: acc)
        | _ -> Error "Expected ',' or ')' in tuple pattern")

and parseRecordPatternWithTypeName (typeName: string) (tokens: Token list) (acc: (string * Pattern) list) : Result<Pattern * Token list, string> =
    // Parse record pattern with explicit type name: TypeName { field = pattern, ... }
    match tokens with
    | TRBrace :: rest ->
        // Empty record or end of fields
        let fields = List.rev acc
        Ok (PRecord (typeName, fields), rest)
    | TIdent fieldName :: TEquals :: rest ->
        parsePattern rest
        |> Result.bind (fun (pat, remaining) ->
            let field = (fieldName, pat)
            match remaining with
            | TRBrace :: rest' ->
                // End of record pattern
                let fields = List.rev (field :: acc)
                Ok (PRecord (typeName, fields), rest')
            | TComma :: rest' ->
                // More fields
                parseRecordPatternWithTypeName typeName rest' (field :: acc)
            | _ -> Error "Expected ',' or '}' in record pattern")
    | _ -> Error "Expected field name in record pattern"

and parseListPattern (tokens: Token list) (acc: Pattern list) : Result<Pattern * Token list, string> =
    match tokens with
    | TRBracket :: rest ->
        // Empty list or end of list pattern
        Ok (PList (List.rev acc), rest)
    | TDotDotDot :: rest ->
        // Rest pattern at start: [...t]
        parsePattern rest
        |> Result.bind (fun (tailPat, remaining) ->
            match remaining with
            | TRBracket :: rest' ->
                Ok (PListCons (List.rev acc, tailPat), rest')
            | _ -> Error "Expected ']' after rest pattern")
    | _ ->
        parsePattern tokens
        |> Result.bind (fun (pat, remaining) ->
            match remaining with
            | TRBracket :: rest ->
                // End of list pattern
                Ok (PList (List.rev (pat :: acc)), rest)
            | TComma :: TDotDotDot :: rest ->
                // Rest pattern after element: [a, b, ...t]
                parsePattern rest
                |> Result.bind (fun (tailPat, remaining') ->
                    match remaining' with
                    | TRBracket :: rest' ->
                        Ok (PListCons (List.rev (pat :: acc), tailPat), rest')
                    | _ -> Error "Expected ']' after rest pattern")
            | TComma :: rest ->
                // More elements
                parseListPattern rest (pat :: acc)
            | _ -> Error "Expected ',' or ']' in list pattern")

/// Parse a single case: | pattern -> expr
let parseCase (tokens: Token list) (parseExprFn: Token list -> Result<Expr * Token list, string>) : Result<(Pattern * Expr) * Token list, string> =
    match tokens with
    | TBar :: rest ->
        parsePattern rest
        |> Result.bind (fun (pattern, remaining) ->
            match remaining with
            | TArrow :: rest' ->
                parseExprFn rest'
                |> Result.map (fun (body, remaining') ->
                    ((pattern, body), remaining'))
            | _ -> Error "Expected '->' after pattern")
    | _ -> Error "Expected '|' before pattern"

/// Try to parse lambda parameters: (ident : type, ident : type, ...)
/// Returns Some (params, remaining) if successful, None otherwise
let tryParseLambdaParams (tokens: Token list) : ((string * Type) list * Token list) option =
    let rec parseParams (toks: Token list) (acc: (string * Type) list) : ((string * Type) list * Token list) option =
        match toks with
        | TRParen :: rest ->
            // End of parameters
            Some (List.rev acc, rest)
        | TIdent name :: TColon :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Parameter: name : type
            match parseType rest with
            | Ok (ty, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More parameters
                    parseParams rest' ((name, ty) :: acc)
                | TRParen :: rest' ->
                    // End of parameters
                    Some (List.rev ((name, ty) :: acc), rest')
                | _ -> None  // Not a valid lambda
            | Error _ -> None  // Type parse failed
        | _ -> None  // Not a valid lambda parameter

    parseParams tokens []

/// Parser: convert tokens to AST
let parse (tokens: Token list) : Result<Program, string> =
    // Recursive descent parser with operator precedence
    // Precedence (low to high): or < and < comparison < +/- < */ < unary

    /// Parse multiple cases for pattern matching: | p1 -> e1 | p2 -> e2 ...
    let rec parseCases (toks: Token list) (acc: (Pattern * Expr) list) : Result<(Pattern * Expr) list * Token list, string> =
        match toks with
        | TBar :: _ ->
            // Another case
            parseCase toks parseExpr
            |> Result.bind (fun (case, remaining) ->
                parseCases remaining (case :: acc))
        | _ ->
            // End of cases
            if List.isEmpty acc then
                Error "Match expression must have at least one case"
            else
                Ok (List.rev acc, toks)

    and parseExpr (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TLet :: rest ->
            // Parse: let pattern = value in body
            // Supports simple let (let x = ...) and pattern matching (let (a, b) = ...)
            parsePattern rest
            |> Result.bind (fun (pattern, remaining) ->
                match remaining with
                | TEquals :: rest' ->
                    parseExpr rest'
                    |> Result.bind (fun (value, remaining') ->
                        match remaining' with
                        | TIn :: rest'' ->
                            parseExpr rest''
                            |> Result.map (fun (body, remaining'') ->
                                // If pattern is just PVar, use Let; otherwise desugar to Match
                                match pattern with
                                | PVar name -> (Let (name, value, body), remaining'')
                                | _ -> (Match (value, [(pattern, body)]), remaining''))
                        | _ -> Error "Expected 'in' after let binding value")
                | _ -> Error "Expected '=' after let binding pattern")
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
        | TMatch :: rest ->
            // Parse: match scrutinee with | p1 -> e1 | p2 -> e2
            parseExpr rest
            |> Result.bind (fun (scrutinee, remaining) ->
                match remaining with
                | TWith :: rest' ->
                    parseCases rest' []
                    |> Result.map (fun (cases, remaining') ->
                        (Match (scrutinee, cases), remaining'))
                | _ -> Error "Expected 'with' after match scrutinee")
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
                | TPlusPlus :: rest ->
                    parseMultiplicative rest
                    |> Result.bind (fun (right, remaining') ->
                        parseAdditiveRest (BinOp (StringConcat, leftExpr, right)) remaining')
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
        // Parse a primary expression, then handle any postfix operations
        parsePrimaryBase toks
        |> Result.bind (fun (expr, remaining) ->
            parsePostfix expr remaining)

    and parsePrimaryBase (toks: Token list) : Result<Expr * Token list, string> =
        match toks with
        | TInt n :: rest -> Ok (IntLiteral n, rest)
        | TFloat f :: rest -> Ok (FloatLiteral f, rest)
        | TStringLit s :: rest -> Ok (StringLiteral s, rest)
        | TTrue :: rest -> Ok (BoolLiteral true, rest)
        | TFalse :: rest -> Ok (BoolLiteral false, rest)
        | TIdent name :: TLt :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Could be generic function call: name<type, ...>(args)
            // Or could be comparison: name < expr
            // Disambiguate by checking if next token looks like a type
            let looksLikeType =
                match rest with
                | TIdent typeName :: _ when System.Char.IsUpper(typeName.[0]) -> true  // Uppercase = type name
                | TIdent "int" :: _ -> true
                | TIdent "bool" :: _ -> true
                | TIdent "string" :: _ -> true
                | TIdent "float" :: _ -> true
                | _ -> false
            if looksLikeType then
                // Parse as generic call
                parseTypeArgs rest []
                |> Result.bind (fun (typeArgs, afterTypes) ->
                    match afterTypes with
                    | TLParen :: argsStart ->
                        parseCallArgs argsStart []
                        |> Result.map (fun (args, remaining) ->
                            (TypeApp (name, typeArgs, args), remaining))
                    | _ -> Error "Expected '(' after type arguments in generic call")
            else
                // Not a type application, treat name as variable and let comparison parsing handle <
                Ok (Var name, TLt :: rest)
        | TIdent name :: TLParen :: rest when not (System.Char.IsUpper(name.[0])) ->
            // Function call: name(args) - only for lowercase names
            parseCallArgs rest []
            |> Result.map (fun (args, remaining) ->
                (Call (name, args), remaining))
        | TIdent name :: TLParen :: rest when System.Char.IsUpper(name.[0]) ->
            // Constructor with payload: Constructor(payload)
            parseExpr rest
            |> Result.bind (fun (payloadExpr, remaining) ->
                match remaining with
                | TRParen :: rest' ->
                    Ok (Constructor ("", name, Some payloadExpr), rest')
                | _ -> Error "Expected ')' after constructor payload")
        | TIdent typeName :: TLBrace :: rest when System.Char.IsUpper(typeName.[0]) ->
            // Record literal with type name: Point { x = 1, y = 2 }
            parseRecordLiteralFieldsWithTypeName typeName rest []
        | TIdent name :: rest when System.Char.IsUpper(name.[0]) ->
            // Constructor without payload (enum variant)
            Ok (Constructor ("", name, None), rest)
        | TIdent name :: rest ->
            // Variable reference (lowercase identifier)
            Ok (Var name, rest)
        | TLParen :: rest ->
            // Could be lambda, parenthesized expression, or tuple literal
            // Check if it looks like lambda params: (ident : type, ...) =>
            match tryParseLambdaParams rest with
            | Some (lambdaParams, TFatArrow :: bodyStart) ->
                // It's a lambda: (params) => body
                parseExpr bodyStart
                |> Result.map (fun (body, remaining) ->
                    (Lambda (lambdaParams, body), remaining))
            | _ ->
                // Not a lambda - parse as expression/tuple
                parseExpr rest
                |> Result.bind (fun (firstExpr, remaining) ->
                    match remaining with
                    | TRParen :: rest' ->
                        // Parenthesized expression (single element)
                        Ok (firstExpr, rest')
                    | TComma :: rest' ->
                        // Tuple literal: (expr, expr, ...)
                        parseTupleElements rest' [firstExpr]
                    | _ -> Error "Expected ')' or ',' in tuple/parenthesized expression")
        | TLBrace :: _ ->
            // Anonymous record literal is no longer supported
            Error "Record literal requires type name: use 'TypeName { field = value, ... }'"
        | TLBracket :: rest ->
            // List literal: [1, 2, 3] or []
            parseListLiteralElements rest []
        | _ -> Error "Expected expression"

    and parseTupleElements (toks: Token list) (acc: Expr list) : Result<Expr * Token list, string> =
        // Parse remaining tuple elements after the first comma
        parseExpr toks
        |> Result.bind (fun (expr, remaining) ->
            match remaining with
            | TComma :: rest ->
                // More elements
                parseTupleElements rest (expr :: acc)
            | TRParen :: rest ->
                // End of tuple
                let elements = List.rev (expr :: acc)
                Ok (TupleLiteral elements, rest)
            | _ -> Error "Expected ',' or ')' in tuple literal")

    and parseRecordLiteralFieldsWithTypeName (typeName: string) (toks: Token list) (acc: (string * Expr) list) : Result<Expr * Token list, string> =
        // Parse record literal fields with explicit type name: TypeName { name = expr, ... }
        match toks with
        | TRBrace :: rest ->
            // Empty record or end of fields
            Ok (RecordLiteral (typeName, List.rev acc), rest)
        | TIdent fieldName :: TEquals :: rest ->
            parseExpr rest
            |> Result.bind (fun (value, remaining) ->
                match remaining with
                | TComma :: rest' ->
                    // More fields
                    parseRecordLiteralFieldsWithTypeName typeName rest' ((fieldName, value) :: acc)
                | TRBrace :: rest' ->
                    // End of record
                    Ok (RecordLiteral (typeName, List.rev ((fieldName, value) :: acc)), rest')
                | _ -> Error "Expected ',' or '}' after record field value")
        | _ -> Error "Expected field name in record literal"

    and parseListLiteralElements (toks: Token list) (acc: Expr list) : Result<Expr * Token list, string> =
        // Parse list literal elements: [expr, expr, ...] or []
        match toks with
        | TRBracket :: rest ->
            // Empty list or end of list
            Ok (ListLiteral (List.rev acc), rest)
        | _ ->
            parseExpr toks
            |> Result.bind (fun (expr, remaining) ->
                match remaining with
                | TComma :: rest ->
                    // More elements
                    parseListLiteralElements rest (expr :: acc)
                | TRBracket :: rest ->
                    // End of list
                    Ok (ListLiteral (List.rev (expr :: acc)), rest)
                | _ -> Error "Expected ',' or ']' in list literal")

    and parsePostfix (expr: Expr) (toks: Token list) : Result<Expr * Token list, string> =
        // Handle postfix operations: tuple access (.0, .1), field access (.fieldName), or function application (args)
        match toks with
        | TDot :: TInt index :: rest ->
            if index < 0L then
                Error "Tuple index cannot be negative"
            else
                let accessExpr = TupleAccess (expr, int index)
                parsePostfix accessExpr rest
        | TDot :: TIdent fieldName :: rest ->
            // Record field access
            let accessExpr = RecordAccess (expr, fieldName)
            parsePostfix accessExpr rest
        | TLParen :: rest ->
            // Function application: expr(args)
            // Only allow if expr is not a simple named variable (those are handled by Call)
            match expr with
            | Var _ ->
                // This is handled by Call in parsePrimaryBase, not Apply
                // But if we get here with a Var, it means the Var came from a postfix
                // chain (e.g., x.field(args)), so we should use Apply
                parseCallArgs rest []
                |> Result.bind (fun (args, remaining) ->
                    let applyExpr = Apply (expr, args)
                    parsePostfix applyExpr remaining)
            | _ ->
                // Lambda or other expression being applied
                parseCallArgs rest []
                |> Result.bind (fun (args, remaining) ->
                    let applyExpr = Apply (expr, args)
                    parsePostfix applyExpr remaining)
        | _ -> Ok (expr, toks)

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

        | TType :: _ ->
            // Parse type definition
            parseTypeDef toks
            |> Result.bind (fun (typeDef, remaining) ->
                parseTopLevels remaining (TypeDef typeDef :: acc))

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
