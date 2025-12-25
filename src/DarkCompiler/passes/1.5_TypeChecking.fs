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
    | TTuple elemTypes ->
        let elemsStr = elemTypes |> List.map typeToString |> String.concat ", "
        $"({elemsStr})"
    | TRecord name -> name
    | TSum name -> name
    | TList -> "list"

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

/// Type registry - maps record type names to their field definitions
type TypeRegistry = Map<string, (string * Type) list>

/// Sum type registry - maps sum type names to their variant lists (name, tag, payload)
type SumTypeRegistry = Map<string, (string * int * Type option) list>

/// Variant lookup - maps variant names to (type name, tag index, payload type)
type VariantLookup = Map<string, (string * int * Type option)>

/// Check expression type top-down
/// Parameters:
///   - expr: Expression to type-check
///   - env: Type environment (variable name -> type mappings)
///   - typeReg: Type registry (record type name -> field definitions)
///   - variantLookup: Maps variant names to (type name, tag index)
///   - expectedType: Optional expected type from context (for checking)
/// Returns: Result<Type, TypeError>
let rec checkExpr (expr: Expr) (env: TypeEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (expectedType: Type option) : Result<Type, TypeError> =
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

    | FloatLiteral _ ->
        // Float literals are always TFloat64
        match expectedType with
        | Some TFloat64 | None -> Ok TFloat64
        | Some other -> Error (TypeMismatch (other, TFloat64, "float literal"))

    | BinOp (op, left, right) ->
        match op with
        // Arithmetic operators: T -> T -> T (where T is int or float)
        | Add | Sub | Mul | Div ->
            let opName =
                match op with
                | Add -> "+"
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | _ -> "?"

            // Check left operand to determine numeric type
            checkExpr left env typeReg variantLookup None
            |> Result.bind (fun leftType ->
                match leftType with
                | TInt64 | TFloat64 ->
                    // Right operand must be same type
                    checkExpr right env typeReg variantLookup (Some leftType)
                    |> Result.bind (fun rightType ->
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some expected when expected <> leftType ->
                                Error (TypeMismatch (expected, leftType, $"result of {opName}"))
                            | _ -> Ok leftType)
                | other ->
                    Error (InvalidOperation (opName, [other])))

        // Comparison operators: T -> T -> bool (where T is int or float)
        | Eq | Neq | Lt | Gt | Lte | Gte ->
            let opName =
                match op with
                | Eq -> "=="
                | Neq -> "!="
                | Lt -> "<"
                | Gt -> ">"
                | Lte -> "<="
                | Gte -> ">="
                | _ -> "?"

            // Check left operand to determine numeric type
            checkExpr left env typeReg variantLookup None
            |> Result.bind (fun leftType ->
                match leftType with
                | TInt64 | TFloat64 ->
                    // Right operand must be same type
                    checkExpr right env typeReg variantLookup (Some leftType)
                    |> Result.bind (fun rightType ->
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok TBool
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))
                | other ->
                    Error (InvalidOperation (opName, [other])))

        // Boolean operators: bool -> bool -> bool
        | And | Or ->
            let opName = if op = And then "&&" else "||"

            checkExpr left env typeReg variantLookup (Some TBool)
            |> Result.bind (fun leftType ->
                if leftType <> TBool then
                    Error (TypeMismatch (TBool, leftType, $"left operand of {opName}"))
                else
                    checkExpr right env typeReg variantLookup (Some TBool)
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
            // Negation works on integers and floats
            checkExpr inner env typeReg variantLookup None
            |> Result.bind (fun innerType ->
                match innerType with
                | TInt64 | TFloat64 ->
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of negation"))
                    | _ -> Ok innerType
                | other ->
                    Error (InvalidOperation ("-", [other])))

        | Not ->
            // Boolean not works on booleans and returns booleans
            checkExpr inner env typeReg variantLookup (Some TBool)
            |> Result.bind (fun innerType ->
                if innerType <> TBool then
                    Error (TypeMismatch (TBool, innerType, "operand of !"))
                else
                    match expectedType with
                    | Some TBool | None -> Ok TBool
                    | Some other -> Error (TypeMismatch (other, TBool, "result of !")))

    | Let (name, value, body) ->
        // Let binding: check value, extend environment, check body
        checkExpr value env typeReg variantLookup None
        |> Result.bind (fun valueType ->
            let env' = Map.add name valueType env
            checkExpr body env' typeReg variantLookup expectedType)

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
        checkExpr cond env typeReg variantLookup (Some TBool)
        |> Result.bind (fun condType ->
            if condType <> TBool then
                Error (TypeMismatch (TBool, condType, "if condition"))
            else
                checkExpr thenBranch env typeReg variantLookup expectedType
                |> Result.bind (fun thenType ->
                    checkExpr elseBranch env typeReg variantLookup (Some thenType)
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
                        checkExpr arg env typeReg variantLookup (Some expectedParamType)
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

    | TupleLiteral elements ->
        // Type-check each element and build tuple type
        let rec checkElements elems acc =
            match elems with
            | [] -> Ok (List.rev acc)
            | e :: rest ->
                checkExpr e env typeReg variantLookup None
                |> Result.bind (fun elemType ->
                    checkElements rest (elemType :: acc))

        checkElements elements []
        |> Result.bind (fun elemTypes ->
            let tupleType = TTuple elemTypes
            match expectedType with
            | Some (TTuple expectedElemTypes) when expectedElemTypes <> elemTypes ->
                Error (TypeMismatch (TTuple expectedElemTypes, tupleType, "tuple literal"))
            | Some other when other <> tupleType ->
                Error (TypeMismatch (other, tupleType, "tuple literal"))
            | _ -> Ok tupleType)

    | TupleAccess (tupleExpr, index) ->
        // Check the tuple expression
        checkExpr tupleExpr env typeReg variantLookup None
        |> Result.bind (fun tupleType ->
            match tupleType with
            | TTuple elemTypes ->
                if index < 0 || index >= List.length elemTypes then
                    Error (GenericError $"Tuple index {index} out of bounds (tuple has {List.length elemTypes} elements)")
                else
                    let elemType = List.item index elemTypes
                    match expectedType with
                    | Some expected when expected <> elemType ->
                        Error (TypeMismatch (expected, elemType, $"tuple access .{index}"))
                    | _ -> Ok elemType
            | other ->
                Error (GenericError $"Cannot access .{index} on non-tuple type {typeToString other}"))

    | RecordLiteral (typeName, fields) ->
        // For anonymous records (typeName = ""), try to infer from expected type or field names
        // For named records, look up type definition
        let recordTypeName =
            if typeName = "" then
                match expectedType with
                | Some (TRecord name) -> Some name
                | _ ->
                    // Try to infer record type from field names
                    let fieldNames = fields |> List.map fst |> Set.ofList
                    typeReg
                    |> Map.toList
                    |> List.tryFind (fun (_, typeFields) ->
                        let typeFieldNames = typeFields |> List.map fst |> Set.ofList
                        typeFieldNames = fieldNames)
                    |> Option.map fst
            else
                Some typeName

        match recordTypeName with
        | None ->
            Error (GenericError "Cannot infer record type - no matching type definition found")
        | Some name ->
            match Map.tryFind name typeReg with
            | None ->
                Error (GenericError $"Unknown record type: {name}")
            | Some expectedFields ->
                // Check that all fields are present and have correct types
                let fieldMap = Map.ofList fields

                // Check for missing fields
                let missingFields =
                    expectedFields
                    |> List.filter (fun (fname, _) -> not (Map.containsKey fname fieldMap))
                    |> List.map fst

                if not (List.isEmpty missingFields) then
                    let missingStr = String.concat ", " missingFields
                    Error (GenericError $"Missing fields in record literal: {missingStr}")
                else
                    // Check for extra fields
                    let expectedFieldNames = expectedFields |> List.map fst |> Set.ofList
                    let extraFields =
                        fields
                        |> List.filter (fun (fname, _) -> not (Set.contains fname expectedFieldNames))
                        |> List.map fst

                    if not (List.isEmpty extraFields) then
                        let extraStr = String.concat ", " extraFields
                        Error (GenericError $"Unknown fields in record literal: {extraStr}")
                    else
                        // Type check each field
                        expectedFields
                        |> List.fold (fun result (fname, expectedFieldType) ->
                            result |> Result.bind (fun () ->
                                match Map.tryFind fname fieldMap with
                                | Some fieldExpr ->
                                    checkExpr fieldExpr env typeReg variantLookup (Some expectedFieldType)
                                    |> Result.bind (fun actualType ->
                                        if actualType = expectedFieldType then Ok ()
                                        else Error (TypeMismatch (expectedFieldType, actualType, $"field {fname}")))
                                | None -> Ok ())) // Already checked for missing fields
                            (Ok ())
                        |> Result.map (fun () -> TRecord name)

    | RecordAccess (recordExpr, fieldName) ->
        // Check the record expression
        checkExpr recordExpr env typeReg variantLookup None
        |> Result.bind (fun recordType ->
            match recordType with
            | TRecord typeName ->
                match Map.tryFind typeName typeReg with
                | None ->
                    Error (GenericError $"Unknown record type: {typeName}")
                | Some fields ->
                    match List.tryFind (fun (name, _) -> name = fieldName) fields with
                    | None ->
                        Error (GenericError $"Record type {typeName} has no field '{fieldName}'")
                    | Some (_, fieldType) ->
                        match expectedType with
                        | Some expected when expected <> fieldType ->
                            Error (TypeMismatch (expected, fieldType, $"field access .{fieldName}"))
                        | _ -> Ok fieldType
            | other ->
                Error (GenericError $"Cannot access .{fieldName} on non-record type {typeToString other}"))

    | Constructor (_, variantName, payload) ->
        // Look up the variant to find its type and expected payload
        match Map.tryFind variantName variantLookup with
        | None ->
            Error (GenericError $"Unknown constructor: {variantName}")
        | Some (typeName, _tag, expectedPayload) ->
            match expectedPayload, payload with
            | None, None ->
                // Variant without payload, no payload provided - OK
                let sumType = TSum typeName
                match expectedType with
                | Some expected when expected <> sumType ->
                    Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                | _ -> Ok sumType
            | None, Some _ ->
                // Variant doesn't take payload but one was provided
                Error (GenericError $"Constructor {variantName} does not take a payload")
            | Some _, None ->
                // Variant requires payload but none provided
                Error (GenericError $"Constructor {variantName} requires a payload")
            | Some payloadType, Some payloadExpr ->
                // Variant with payload - check payload has correct type
                checkExpr payloadExpr env typeReg variantLookup (Some payloadType)
                |> Result.bind (fun actualPayloadType ->
                    if actualPayloadType <> payloadType then
                        Error (TypeMismatch (payloadType, actualPayloadType, $"payload of {variantName}"))
                    else
                        let sumType = TSum typeName
                        match expectedType with
                        | Some expected when expected <> sumType ->
                            Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                        | _ -> Ok sumType)

    | Match (scrutinee, cases) ->
        // Type check the scrutinee first
        checkExpr scrutinee env typeReg variantLookup None
        |> Result.bind (fun scrutineeType ->
            // Extract bindings from a pattern based on scrutinee type
            let rec extractPatternBindings (pattern: Pattern) (patternType: Type) : Result<(string * Type) list, TypeError> =
                match pattern with
                | PWildcard -> Ok []
                | PLiteral _ -> Ok []
                | PBool _ -> Ok []
                | PString _ -> Ok []
                | PFloat _ -> Ok []
                | PVar name -> Ok [(name, patternType)]
                | PConstructor (variantName, payloadPattern) ->
                    match Map.tryFind variantName variantLookup with
                    | None -> Error (GenericError $"Unknown variant in pattern: {variantName}")
                    | Some (_, _, payloadType) ->
                        match payloadPattern, payloadType with
                        | None, _ -> Ok []
                        | Some innerPattern, Some pType ->
                            extractPatternBindings innerPattern pType
                        | Some _, None ->
                            Error (GenericError $"Variant {variantName} has no payload but pattern expects one")
                | PTuple patterns ->
                    match patternType with
                    | TTuple elementTypes when List.length patterns = List.length elementTypes ->
                        List.zip patterns elementTypes
                        |> List.map (fun (p, t) -> extractPatternBindings p t)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    | TTuple elementTypes ->
                        Error (GenericError $"Tuple pattern has {List.length patterns} elements but type has {List.length elementTypes}")
                    | _ -> Error (GenericError "Tuple pattern used on non-tuple type")
                | PRecord (_, fieldPatterns) ->
                    match patternType with
                    | TRecord recordName ->
                        match Map.tryFind recordName typeReg with
                        | Some fields ->
                            fieldPatterns
                            |> List.map (fun (fieldName, pat) ->
                                match List.tryFind (fun (n, _) -> n = fieldName) fields with
                                | Some (_, fieldType) -> extractPatternBindings pat fieldType
                                | None -> Error (GenericError $"Unknown field in pattern: {fieldName}"))
                            |> List.fold (fun acc res ->
                                match acc, res with
                                | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                                | Error e, _ -> Error e
                                | _, Error e -> Error e) (Ok [])
                        | None -> Error (GenericError $"Unknown record type: {recordName}")
                    | _ -> Error (GenericError "Record pattern used on non-record type")
                | PList patterns ->
                    match patternType with
                    | TList ->
                        // Each element pattern binds int variables
                        patterns
                        |> List.map (fun p -> extractPatternBindings p TInt64)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    | _ -> Error (GenericError "List pattern used on non-list type")

            // Type check each case and ensure they all return the same type
            let rec checkCases (remaining: (Pattern * Expr) list) (resultType: Type option) : Result<Type, TypeError> =
                match remaining with
                | [] ->
                    match resultType with
                    | Some t -> Ok t
                    | None -> Error (GenericError "Match expression must have at least one case")
                | (pattern, body) :: rest ->
                    // Extract bindings from pattern and add to environment
                    extractPatternBindings pattern scrutineeType
                    |> Result.bind (fun bindings ->
                        let bodyEnv = List.fold (fun e (name, ty) -> Map.add name ty e) env bindings
                        checkExpr body bodyEnv typeReg variantLookup resultType
                        |> Result.bind (fun bodyType ->
                            match resultType with
                            | None -> checkCases rest (Some bodyType)
                            | Some expected when expected = bodyType -> checkCases rest resultType
                            | Some expected -> Error (TypeMismatch (expected, bodyType, "match case"))))

            checkCases cases None
            |> Result.bind (fun matchType ->
                match expectedType with
                | Some expected when expected <> matchType ->
                    Error (TypeMismatch (expected, matchType, "match expression"))
                | _ -> Ok matchType))

    | ListLiteral elements ->
        // Type-check each element (must all be int for now - monomorphic)
        let rec checkElements elems =
            match elems with
            | [] -> Ok ()
            | e :: rest ->
                checkExpr e env typeReg variantLookup (Some TInt64)
                |> Result.bind (fun elemType ->
                    if elemType <> TInt64 then
                        Error (TypeMismatch (TInt64, elemType, "list element"))
                    else
                        checkElements rest)

        checkElements elements
        |> Result.bind (fun () ->
            match expectedType with
            | Some TList | None -> Ok TList
            | Some other -> Error (TypeMismatch (other, TList, "list literal")))

/// Type-check a function definition
let checkFunctionDef (funcDef: FunctionDef) (env: TypeEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) : Result<unit, TypeError> =
    // Build environment with parameters
    let paramEnv =
        funcDef.Params
        |> List.fold (fun e (name, ty) -> Map.add name ty e) env

    // Check body has return type
    checkExpr funcDef.Body paramEnv typeReg variantLookup (Some funcDef.ReturnType)
    |> Result.bind (fun bodyType ->
        if bodyType = funcDef.ReturnType then
            Ok ()
        else
            Error (TypeMismatch (funcDef.ReturnType, bodyType, $"function {funcDef.Name} body")))

/// Type-check a program
/// Returns the type of the main expression or unit for function-only programs
let checkProgram (program: Program) : Result<Type, TypeError> =
    let (Program topLevels) = program

    // First pass: collect all type definitions (records)
    let typeReg : TypeRegistry =
        topLevels
        |> List.choose (function
            | TypeDef (RecordDef (name, fields)) -> Some (name, fields)
            | _ -> None)
        |> Map.ofList

    // Collect sum type definitions and build variant lookup
    // Maps variant name -> (type name, tag index, payload type)
    let variantLookup : VariantLookup =
        topLevels
        |> List.choose (function
            | TypeDef (SumTypeDef (typeName, variants)) ->
                Some (typeName, variants)
            | _ -> None)
        |> List.collect (fun (typeName, variants) ->
            variants
            |> List.mapi (fun idx variant -> (variant.Name, (typeName, idx, variant.Payload))))
        |> Map.ofList

    // Second pass: collect all function signatures
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

    // Third pass: type check all function definitions
    let checkAllFunctions () =
        topLevels
        |> List.fold (fun result topLevel ->
            result |> Result.bind (fun () ->
                match topLevel with
                | FunctionDef funcDef -> checkFunctionDef funcDef funcEnv typeReg variantLookup
                | TypeDef _ -> Ok ()  // Type definitions already processed
                | Expression _ -> Ok ())) (Ok ())

    // Type check functions first
    checkAllFunctions ()
    |> Result.bind (fun () ->
        // Then type check main expression if any
        let mainExpr = topLevels |> List.tryPick (function Expression e -> Some e | _ -> None)
        match mainExpr with
        | Some expr -> checkExpr expr funcEnv typeReg variantLookup None
        | None ->
            // No main expression - just functions
            // For now, require a "main" function with signature () -> int
            match Map.tryFind "main" funcSigs with
            | Some ([], TInt64) -> Ok TInt64  // main() : int
            | Some _ -> Error (GenericError "main function must have signature () -> int")
            | None -> Error (GenericError "Program must have either a main expression or a main() : int function"))
