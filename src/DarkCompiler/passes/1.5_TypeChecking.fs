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
    | GenericError of string

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
    | GenericError msg ->
        msg

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

    | BoolLiteral _ ->
        // Boolean literals are always TBool
        match expectedType with
        | Some TBool | None -> Ok TBool
        | Some other -> Error (TypeMismatch (other, TBool, "boolean literal"))

    | StringLiteral _ ->
        // String literals are always TString
        match expectedType with
        | Some TString | None -> Ok TString
        | Some other -> Error (TypeMismatch (other, TString, "string literal"))

    | BinOp (op, left, right) ->
        match op with
        // Arithmetic operators: int -> int -> int
        | Add | Sub | Mul | Div ->
            let opName =
                match op with
                | Add -> "+"
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | _ -> failwith "unreachable"

            checkExpr left env (Some TInt64)
            |> Result.bind (fun leftType ->
                if leftType <> TInt64 then
                    Error (TypeMismatch (TInt64, leftType, $"left operand of {opName}"))
                else
                    checkExpr right env (Some TInt64)
                    |> Result.bind (fun rightType ->
                        if rightType <> TInt64 then
                            Error (TypeMismatch (TInt64, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TInt64 | None -> Ok TInt64
                            | Some other -> Error (TypeMismatch (other, TInt64, $"result of {opName}"))))

        // Comparison operators: int -> int -> bool
        | Eq | Neq | Lt | Gt | Lte | Gte ->
            let opName =
                match op with
                | Eq -> "=="
                | Neq -> "!="
                | Lt -> "<"
                | Gt -> ">"
                | Lte -> "<="
                | Gte -> ">="
                | _ -> failwith "unreachable"

            checkExpr left env (Some TInt64)
            |> Result.bind (fun leftType ->
                if leftType <> TInt64 then
                    Error (TypeMismatch (TInt64, leftType, $"left operand of {opName}"))
                else
                    checkExpr right env (Some TInt64)
                    |> Result.bind (fun rightType ->
                        if rightType <> TInt64 then
                            Error (TypeMismatch (TInt64, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok TBool
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))))

        // Boolean operators: bool -> bool -> bool
        | And | Or ->
            let opName = if op = And then "&&" else "||"

            checkExpr left env (Some TBool)
            |> Result.bind (fun leftType ->
                if leftType <> TBool then
                    Error (TypeMismatch (TBool, leftType, $"left operand of {opName}"))
                else
                    checkExpr right env (Some TBool)
                    |> Result.bind (fun rightType ->
                        if rightType <> TBool then
                            Error (TypeMismatch (TBool, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok TBool
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))))

    | UnaryOp (op, inner) ->
        match op with
        | Neg ->
            // Negation works on integers and returns integers
            checkExpr inner env (Some TInt64)
            |> Result.bind (fun innerType ->
                if innerType <> TInt64 then
                    Error (TypeMismatch (TInt64, innerType, "operand of negation"))
                else
                    match expectedType with
                    | Some TInt64 | None -> Ok TInt64
                    | Some other -> Error (TypeMismatch (other, TInt64, "result of negation")))

        | Not ->
            // Boolean not works on booleans and returns booleans
            checkExpr inner env (Some TBool)
            |> Result.bind (fun innerType ->
                if innerType <> TBool then
                    Error (TypeMismatch (TBool, innerType, "operand of !"))
                else
                    match expectedType with
                    | Some TBool | None -> Ok TBool
                    | Some other -> Error (TypeMismatch (other, TBool, "result of !")))

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

    | If (cond, thenBranch, elseBranch) ->
        // If expression: condition must be bool, branches must have same type
        checkExpr cond env (Some TBool)
        |> Result.bind (fun condType ->
            if condType <> TBool then
                Error (TypeMismatch (TBool, condType, "if condition"))
            else
                checkExpr thenBranch env expectedType
                |> Result.bind (fun thenType ->
                    checkExpr elseBranch env (Some thenType)
                    |> Result.bind (fun elseType ->
                        if thenType <> elseType then
                            Error (TypeMismatch (thenType, elseType, "if branches must have same type"))
                        else
                            match expectedType with
                            | Some expected when expected <> thenType ->
                                Error (TypeMismatch (expected, thenType, "if expression"))
                            | _ -> Ok thenType)))

    | Call (funcName, args) ->
        // Function call: look up function signature, check arguments match
        match Map.tryFind funcName env with
        | Some (TFunction (paramTypes, returnType)) ->
            // Check argument count
            if List.length args <> List.length paramTypes then
                Error (GenericError $"Function {funcName} expects {List.length paramTypes} arguments, got {List.length args}")
            else
                // Check each argument type
                List.zip args paramTypes
                |> List.fold (fun result (arg, expectedParamType) ->
                    result |> Result.bind (fun () ->
                        checkExpr arg env (Some expectedParamType)
                        |> Result.bind (fun argType ->
                            if argType = expectedParamType then
                                Ok ()
                            else
                                Error (TypeMismatch (expectedParamType, argType, $"argument to {funcName}")))))
                    (Ok ())
                |> Result.bind (fun () ->
                    // All arguments type-checked, return the return type
                    match expectedType with
                    | Some expected when expected <> returnType ->
                        Error (TypeMismatch (expected, returnType, $"result of call to {funcName}"))
                    | _ -> Ok returnType)
        | Some other ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            Error (UndefinedVariable funcName)

/// Type-check a function definition
let checkFunctionDef (funcDef: FunctionDef) (env: TypeEnv) : Result<unit, TypeError> =
    // Build environment with parameters
    let paramEnv =
        funcDef.Params
        |> List.fold (fun e (name, ty) -> Map.add name ty e) env

    // Check body has return type
    checkExpr funcDef.Body paramEnv (Some funcDef.ReturnType)
    |> Result.bind (fun bodyType ->
        if bodyType = funcDef.ReturnType then
            Ok ()
        else
            Error (TypeMismatch (funcDef.ReturnType, bodyType, $"function {funcDef.Name} body")))

/// Type-check a program
/// Returns the type of the main expression or unit for function-only programs
let checkProgram (program: Program) : Result<Type, TypeError> =
    let (Program topLevels) = program

    // First pass: collect all function signatures
    let funcSigs =
        topLevels
        |> List.choose (function
            | FunctionDef funcDef ->
                Some (funcDef.Name, (List.map snd funcDef.Params, funcDef.ReturnType))
            | _ -> None)
        |> Map.ofList

    // Build environment with function signatures
    let funcEnv =
        funcSigs
        |> Map.map (fun _ (paramTypes, returnType) -> TFunction (paramTypes, returnType))

    // Second pass: type check all function definitions
    let checkAllFunctions () =
        topLevels
        |> List.fold (fun result topLevel ->
            result |> Result.bind (fun () ->
                match topLevel with
                | FunctionDef funcDef -> checkFunctionDef funcDef funcEnv
                | Expression _ -> Ok ())) (Ok ())

    // Type check functions first
    checkAllFunctions ()
    |> Result.bind (fun () ->
        // Then type check main expression if any
        let mainExpr = topLevels |> List.tryPick (function Expression e -> Some e | _ -> None)
        match mainExpr with
        | Some expr -> checkExpr expr funcEnv None
        | None ->
            // No main expression - just functions
            // For now, require a "main" function with signature () -> int
            match Map.tryFind "main" funcSigs with
            | Some ([], TInt64) -> Ok TInt64  // main() : int
            | Some _ -> Error (GenericError "main function must have signature () -> int")
            | None -> Error (GenericError "Program must have either a main expression or a main() : int function"))
