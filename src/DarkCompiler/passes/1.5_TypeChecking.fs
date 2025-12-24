// 1.5_TypeChecking.fs - Type Checking Pass (Phase 0)
//
// Simple top-down type checker for the Dark compiler.
//
// Design:
// - Function parameters and return types REQUIRE explicit type signatures (Phase 4+)
// - Let bindings have optional type annotations (Phase 1+)
// - Type checking proceeds top-down (expression context known from surrounding code)
// - No type inference - when type cannot be determined from context, require annotation
//
// Current Phase 0 implementation:
// - Only integers supported (TInt64)
// - All operations must be on integers
// - Returns Result<Type, TypeError> for functional error handling
//
// Example:
//   Input:  2 + 3 * 4
//   Output: Ok TInt64

module TypeChecking

open AST

/// Type errors
type TypeError =
    | TypeMismatch of expected:Type * actual:Type * context:string
    | UndefinedVariable of name:string
    | MissingTypeAnnotation of context:string
    | InvalidOperation of op:string * types:Type list

/// Pretty-print a type for error messages
let rec typeToString (t: Type) : string =
    match t with
    | TInt64 -> "int"
    | TBool -> "bool"
    | TFloat64 -> "float"
    | TString -> "string"
    | TUnit -> "unit"
    | TFunction (params', ret) ->
        let paramStr = params' |> List.map typeToString |> String.concat ", "
        $"({paramStr}) -> {typeToString ret}"

/// Pretty-print a type error
let typeErrorToString (err: TypeError) : string =
    match err with
    | TypeMismatch (expected, actual, context) ->
        $"Type mismatch in {context}: expected {typeToString expected}, got {typeToString actual}"
    | UndefinedVariable name ->
        $"Undefined variable: {name}"
    | MissingTypeAnnotation context ->
        $"Missing type annotation: {context}"
    | InvalidOperation (op, types) ->
        let typesStr = types |> List.map typeToString |> String.concat ", "
        $"Invalid operation '{op}' on types: {typesStr}"

/// Type environment - maps variable names to their types
type TypeEnv = Map<string, Type>

/// Check expression type top-down
/// Parameters:
///   - expr: Expression to type-check
///   - env: Type environment (variable name -> type mappings)
///   - expectedType: Optional expected type from context (for checking)
/// Returns: Result<Type, TypeError>
let rec checkExpr (expr: Expr) (env: TypeEnv) (expectedType: Type option) : Result<Type, TypeError> =
    match expr with
    | IntLiteral _ ->
        // Integer literals are always TInt64
        match expectedType with
        | Some TInt64 | None -> Ok TInt64
        | Some other -> Error (TypeMismatch (other, TInt64, "integer literal"))

    | BinOp (op, left, right) ->
        // For now, all binary operators work on integers and return integers
        // This will be extended in Phase 2 for booleans and comparisons
        let opName =
            match op with
            | Add -> "+"
            | Sub -> "-"
            | Mul -> "*"
            | Div -> "/"

        // Check left operand is int64
        checkExpr left env (Some TInt64)
        |> Result.bind (fun leftType ->
            if leftType <> TInt64 then
                Error (TypeMismatch (TInt64, leftType, $"left operand of {opName}"))
            else
                // Check right operand is int64
                checkExpr right env (Some TInt64)
                |> Result.bind (fun rightType ->
                    if rightType <> TInt64 then
                        Error (TypeMismatch (TInt64, rightType, $"right operand of {opName}"))
                    else
                        // Result is int64
                        match expectedType with
                        | Some TInt64 | None -> Ok TInt64
                        | Some other -> Error (TypeMismatch (other, TInt64, $"result of {opName}"))))

    | Neg inner ->
        // Negation works on integers and returns integers
        checkExpr inner env (Some TInt64)
        |> Result.bind (fun innerType ->
            if innerType <> TInt64 then
                Error (TypeMismatch (TInt64, innerType, "operand of negation"))
            else
                match expectedType with
                | Some TInt64 | None -> Ok TInt64
                | Some other -> Error (TypeMismatch (other, TInt64, "result of negation")))

    | Let (name, value, body) ->
        // Let binding: check value, extend environment, check body
        checkExpr value env None
        |> Result.bind (fun valueType ->
            let env' = Map.add name valueType env
            checkExpr body env' expectedType)

    | Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some varType ->
            match expectedType with
            | Some expected when expected <> varType ->
                Error (TypeMismatch (expected, varType, $"variable {name}"))
            | _ -> Ok varType
        | None ->
            Error (UndefinedVariable name)

/// Type-check a program (Phase 0: just an expression)
/// Returns the type of the expression or a type error
let checkProgram (program: Program) : Result<Type, TypeError> =
    let (Program expr) = program
    let emptyEnv = Map.empty
    checkExpr expr emptyEnv None
