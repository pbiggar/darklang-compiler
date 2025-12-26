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
    | TVar name -> name  // Type variable (for generics)

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

/// Generic function registry - maps function names to their type parameters
/// Only contains entries for functions that have type parameters
type GenericFuncRegistry = Map<string, string list>

/// Type substitution - maps type variable names to concrete types
type Substitution = Map<string, Type>

/// Apply a substitution to a type, replacing type variables with concrete types
let rec applySubst (subst: Substitution) (typ: Type) : Type =
    match typ with
    | TVar name ->
        match Map.tryFind name subst with
        | Some concreteType -> concreteType
        | None -> typ  // Unbound type variable remains as-is
    | TFunction (paramTypes, returnType) ->
        TFunction (List.map (applySubst subst) paramTypes, applySubst subst returnType)
    | TTuple elemTypes ->
        TTuple (List.map (applySubst subst) elemTypes)
    | TInt64 | TBool | TFloat64 | TString | TUnit | TRecord _ | TSum _ | TList ->
        typ  // Concrete types are unchanged

/// Build a substitution from type parameters and type arguments
let buildSubstitution (typeParams: string list) (typeArgs: Type list) : Result<Substitution, string> =
    if List.length typeParams <> List.length typeArgs then
        Error $"Expected {List.length typeParams} type arguments, got {List.length typeArgs}"
    else
        Ok (List.zip typeParams typeArgs |> Map.ofList)

// =============================================================================
// Type Inference for Generic Function Calls
// =============================================================================
// When a generic function is called without explicit type arguments, we infer
// the type arguments from the actual argument types. For example:
//   def identity<T>(x: T) : T = x
//   identity(42)  // Infers T=int from argument type

/// Match a pattern type against an actual type, extracting type variable bindings.
/// Returns a list of (typeVarName, concreteType) pairs.
/// Example: matchTypes (TVar "T") TInt64 = Ok [("T", TInt64)]
let rec matchTypes (pattern: Type) (actual: Type) : Result<(string * Type) list, string> =
    match pattern with
    | TVar name ->
        // Type variable matches anything - record the binding
        Ok [(name, actual)]
    | TInt64 ->
        if actual = TInt64 then Ok []
        else Error $"Expected int, got {typeToString actual}"
    | TBool ->
        if actual = TBool then Ok []
        else Error $"Expected bool, got {typeToString actual}"
    | TFloat64 ->
        if actual = TFloat64 then Ok []
        else Error $"Expected float, got {typeToString actual}"
    | TString ->
        if actual = TString then Ok []
        else Error $"Expected string, got {typeToString actual}"
    | TUnit ->
        if actual = TUnit then Ok []
        else Error $"Expected unit, got {typeToString actual}"
    | TList ->
        if actual = TList then Ok []
        else Error $"Expected list, got {typeToString actual}"
    | TRecord name ->
        if actual = TRecord name then Ok []
        else Error $"Expected {name}, got {typeToString actual}"
    | TSum name ->
        if actual = TSum name then Ok []
        else Error $"Expected {name}, got {typeToString actual}"
    | TFunction (patternParams, patternRet) ->
        match actual with
        | TFunction (actualParams, actualRet) ->
            if List.length patternParams <> List.length actualParams then
                Error $"Function arity mismatch: expected {List.length patternParams} params, got {List.length actualParams}"
            else
                // Match each parameter type and return type
                let paramResults =
                    List.zip patternParams actualParams
                    |> List.map (fun (p, a) -> matchTypes p a)
                let retResult = matchTypes patternRet actualRet
                // Combine all results
                List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) retResult paramResults
        | _ -> Error $"Expected function, got {typeToString actual}"
    | TTuple patternElems ->
        match actual with
        | TTuple actualElems ->
            if List.length patternElems <> List.length actualElems then
                Error $"Tuple size mismatch: expected {List.length patternElems}, got {List.length actualElems}"
            else
                List.zip patternElems actualElems
                |> List.map (fun (p, a) -> matchTypes p a)
                |> List.fold (fun acc res ->
                    match acc, res with
                    | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e) (Ok [])
        | _ -> Error $"Expected tuple, got {typeToString actual}"

/// Consolidate bindings, checking for conflicts where the same type variable
/// is bound to different types. Returns a map from type var name to concrete type.
let consolidateBindings (bindings: (string * Type) list) : Result<Map<string, Type>, string> =
    bindings
    |> List.fold (fun acc (name, typ) ->
        acc |> Result.bind (fun m ->
            match Map.tryFind name m with
            | None -> Ok (Map.add name typ m)
            | Some existingType ->
                if existingType = typ then Ok m
                else Error $"Type variable {name} has conflicting inferences: {typeToString existingType} vs {typeToString typ}"))
        (Ok Map.empty)

/// Infer type arguments for a generic function call.
/// Given type parameters, parameter types (with type variables), and actual argument types,
/// returns the inferred type arguments in order matching typeParams.
let inferTypeArgs (typeParams: string list) (paramTypes: Type list) (argTypes: Type list) : Result<Type list, string> =
    if List.length paramTypes <> List.length argTypes then
        Error $"Argument count mismatch: expected {List.length paramTypes}, got {List.length argTypes}"
    else
        // Match each parameter type against argument type
        let matchResults =
            List.zip paramTypes argTypes
            |> List.map (fun (paramT, argT) -> matchTypes paramT argT)

        // Combine all bindings
        matchResults
        |> List.fold (fun acc res ->
            match acc, res with
            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
            | Error e, _ -> Error e
            | _, Error e -> Error e) (Ok [])
        |> Result.bind consolidateBindings
        |> Result.bind (fun bindingMap ->
            // Extract type arguments in order of type parameters
            typeParams
            |> List.fold (fun acc paramName ->
                acc |> Result.bind (fun args ->
                    match Map.tryFind paramName bindingMap with
                    | Some typ -> Ok (args @ [typ])
                    | None -> Error $"Cannot infer type for {paramName}: no arguments constrain it"))
                (Ok []))

/// Check expression type top-down, potentially transforming the expression.
/// Parameters:
///   - expr: Expression to type-check
///   - env: Type environment (variable name -> type mappings)
///   - typeReg: Type registry (record type name -> field definitions)
///   - variantLookup: Maps variant names to (type name, tag index)
///   - genericFuncReg: Registry of generic functions (function name -> type params)
///   - expectedType: Optional expected type from context (for checking)
/// Returns: Result<Type * Expr, TypeError>
///   - Type: The type of the expression
///   - Expr: The (possibly transformed) expression
let rec checkExpr (expr: Expr) (env: TypeEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (expectedType: Type option) : Result<Type * Expr, TypeError> =
    match expr with
    | IntLiteral _ ->
        // Integer literals are always TInt64
        match expectedType with
        | Some TInt64 | None -> Ok (TInt64, expr)
        | Some other -> Error (TypeMismatch (other, TInt64, "integer literal"))

    | BoolLiteral _ ->
        // Boolean literals are always TBool
        match expectedType with
        | Some TBool | None -> Ok (TBool, expr)
        | Some other -> Error (TypeMismatch (other, TBool, "boolean literal"))

    | StringLiteral _ ->
        // String literals are always TString
        match expectedType with
        | Some TString | None -> Ok (TString, expr)
        | Some other -> Error (TypeMismatch (other, TString, "string literal"))

    | FloatLiteral _ ->
        // Float literals are always TFloat64
        match expectedType with
        | Some TFloat64 | None -> Ok (TFloat64, expr)
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
            checkExpr left env typeReg variantLookup genericFuncReg None
            |> Result.bind (fun (leftType, left') ->
                match leftType with
                | TInt64 | TFloat64 ->
                    // Right operand must be same type
                    checkExpr right env typeReg variantLookup genericFuncReg (Some leftType)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some expected when expected <> leftType ->
                                Error (TypeMismatch (expected, leftType, $"result of {opName}"))
                            | _ -> Ok (leftType, BinOp (op, left', right')))
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
            checkExpr left env typeReg variantLookup genericFuncReg None
            |> Result.bind (fun (leftType, left') ->
                match leftType with
                | TInt64 | TFloat64 ->
                    // Right operand must be same type
                    checkExpr right env typeReg variantLookup genericFuncReg (Some leftType)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> leftType then
                            Error (TypeMismatch (leftType, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}")))
                | other ->
                    Error (InvalidOperation (opName, [other])))

        // Boolean operators: bool -> bool -> bool
        | And | Or ->
            let opName = if op = And then "&&" else "||"

            checkExpr left env typeReg variantLookup genericFuncReg (Some TBool)
            |> Result.bind (fun (leftType, left') ->
                if leftType <> TBool then
                    Error (TypeMismatch (TBool, leftType, $"left operand of {opName}"))
                else
                    checkExpr right env typeReg variantLookup genericFuncReg (Some TBool)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> TBool then
                            Error (TypeMismatch (TBool, rightType, $"right operand of {opName}"))
                        else
                            match expectedType with
                            | Some TBool | None -> Ok (TBool, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TBool, $"result of {opName}"))))

        // String concatenation: string -> string -> string
        | StringConcat ->
            checkExpr left env typeReg variantLookup genericFuncReg (Some TString)
            |> Result.bind (fun (leftType, left') ->
                if leftType <> TString then
                    Error (InvalidOperation ("++", [leftType]))
                else
                    checkExpr right env typeReg variantLookup genericFuncReg (Some TString)
                    |> Result.bind (fun (rightType, right') ->
                        if rightType <> TString then
                            Error (TypeMismatch (TString, rightType, "right operand of ++"))
                        else
                            match expectedType with
                            | Some TString | None -> Ok (TString, BinOp (op, left', right'))
                            | Some other -> Error (TypeMismatch (other, TString, "result of ++"))))

    | UnaryOp (op, inner) ->
        match op with
        | Neg ->
            // Negation works on integers and floats
            checkExpr inner env typeReg variantLookup genericFuncReg None
            |> Result.bind (fun (innerType, inner') ->
                match innerType with
                | TInt64 | TFloat64 ->
                    match expectedType with
                    | Some expected when expected <> innerType ->
                        Error (TypeMismatch (expected, innerType, "result of negation"))
                    | _ -> Ok (innerType, UnaryOp (op, inner'))
                | other ->
                    Error (InvalidOperation ("-", [other])))

        | Not ->
            // Boolean not works on booleans and returns booleans
            checkExpr inner env typeReg variantLookup genericFuncReg (Some TBool)
            |> Result.bind (fun (innerType, inner') ->
                if innerType <> TBool then
                    Error (TypeMismatch (TBool, innerType, "operand of !"))
                else
                    match expectedType with
                    | Some TBool | None -> Ok (TBool, UnaryOp (op, inner'))
                    | Some other -> Error (TypeMismatch (other, TBool, "result of !")))

    | Let (name, value, body) ->
        // Let binding: check value, extend environment, check body
        checkExpr value env typeReg variantLookup genericFuncReg None
        |> Result.bind (fun (valueType, value') ->
            let env' = Map.add name valueType env
            checkExpr body env' typeReg variantLookup genericFuncReg expectedType
            |> Result.map (fun (bodyType, body') -> (bodyType, Let (name, value', body'))))

    | Var name ->
        // Variable reference: look up in environment
        match Map.tryFind name env with
        | Some varType ->
            match expectedType with
            | Some expected when expected <> varType ->
                Error (TypeMismatch (expected, varType, $"variable {name}"))
            | _ -> Ok (varType, expr)
        | None ->
            Error (UndefinedVariable name)

    | If (cond, thenBranch, elseBranch) ->
        // If expression: condition must be bool, branches must have same type
        checkExpr cond env typeReg variantLookup genericFuncReg (Some TBool)
        |> Result.bind (fun (condType, cond') ->
            if condType <> TBool then
                Error (TypeMismatch (TBool, condType, "if condition"))
            else
                checkExpr thenBranch env typeReg variantLookup genericFuncReg expectedType
                |> Result.bind (fun (thenType, then') ->
                    checkExpr elseBranch env typeReg variantLookup genericFuncReg (Some thenType)
                    |> Result.bind (fun (elseType, else') ->
                        if thenType <> elseType then
                            Error (TypeMismatch (thenType, elseType, "if branches must have same type"))
                        else
                            match expectedType with
                            | Some expected when expected <> thenType ->
                                Error (TypeMismatch (expected, thenType, "if expression"))
                            | _ -> Ok (thenType, If (cond', then', else')))))

    | Call (funcName, args) ->
        // Function call: look up function signature, check arguments match
        match Map.tryFind funcName env with
        | Some (TFunction (paramTypes, returnType)) ->
            // Check if this is a generic function - if so, infer type arguments
            match Map.tryFind funcName genericFuncReg with
            | Some typeParams ->
                // Generic function called without explicit type args: infer them
                // First, type-check all arguments to get their types
                let rec checkArgs remaining accTypes accExprs =
                    match remaining with
                    | [] -> Ok (List.rev accTypes, List.rev accExprs)
                    | arg :: rest ->
                        checkExpr arg env typeReg variantLookup genericFuncReg None
                        |> Result.bind (fun (argType, arg') ->
                            checkArgs rest (argType :: accTypes) (arg' :: accExprs))

                checkArgs args [] []
                |> Result.bind (fun (argTypes, args') ->
                    // Infer type arguments from parameter types and argument types
                    inferTypeArgs typeParams paramTypes argTypes
                    |> Result.mapError GenericError
                    |> Result.bind (fun inferredTypeArgs ->
                        // Build substitution and compute concrete types
                        buildSubstitution typeParams inferredTypeArgs
                        |> Result.mapError GenericError
                        |> Result.bind (fun subst ->
                            let concreteReturnType = applySubst subst returnType
                            match expectedType with
                            | Some expected when expected <> concreteReturnType ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ ->
                                // Transform Call to TypeApp with inferred type arguments
                                Ok (concreteReturnType, TypeApp (funcName, inferredTypeArgs, args')))))

            | None ->
                // Non-generic function: regular call
                // Check argument count
                if List.length args <> List.length paramTypes then
                    Error (GenericError $"Function {funcName} expects {List.length paramTypes} arguments, got {List.length args}")
                else
                    // Check each argument type and collect transformed args
                    let rec checkArgsWithTypes remaining paramTys accArgs =
                        match remaining, paramTys with
                        | [], [] -> Ok (List.rev accArgs)
                        | arg :: restArgs, paramT :: restParams ->
                            checkExpr arg env typeReg variantLookup genericFuncReg (Some paramT)
                            |> Result.bind (fun (argType, arg') ->
                                if argType = paramT then
                                    checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                else
                                    Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                        | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                    checkArgsWithTypes args paramTypes []
                    |> Result.bind (fun args' ->
                        match expectedType with
                        | Some expected when expected <> returnType ->
                            Error (TypeMismatch (expected, returnType, $"result of call to {funcName}"))
                        | _ -> Ok (returnType, Call (funcName, args')))
        | Some other ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            Error (UndefinedVariable funcName)

    | TypeApp (funcName, typeArgs, args) ->
        // Generic function call with explicit type arguments: func<Type1, Type2>(args)
        // 1. Look up function signature
        match Map.tryFind funcName env with
        | Some (TFunction (paramTypes, returnType)) ->
            // 2. Look up type parameters
            match Map.tryFind funcName genericFuncReg with
            | Some typeParams ->
                // 3. Build substitution from type params to type args
                buildSubstitution typeParams typeArgs
                |> Result.mapError GenericError
                |> Result.bind (fun subst ->
                    // 4. Apply substitution to get concrete types
                    let concreteParamTypes = List.map (applySubst subst) paramTypes
                    let concreteReturnType = applySubst subst returnType

                    // 5. Check argument count
                    if List.length args <> List.length concreteParamTypes then
                        Error (GenericError $"Function {funcName} expects {List.length concreteParamTypes} arguments, got {List.length args}")
                    else
                        // 6. Type check each argument and collect transformed args
                        let rec checkArgsWithTypes remaining paramTys accArgs =
                            match remaining, paramTys with
                            | [], [] -> Ok (List.rev accArgs)
                            | arg :: restArgs, paramT :: restParams ->
                                checkExpr arg env typeReg variantLookup genericFuncReg (Some paramT)
                                |> Result.bind (fun (argType, arg') ->
                                    if argType = paramT then
                                        checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                                    else
                                        Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                            | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                        checkArgsWithTypes args concreteParamTypes []
                        |> Result.bind (fun args' ->
                            // 7. Return the concrete return type
                            match expectedType with
                            | Some expected when expected <> concreteReturnType ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ -> Ok (concreteReturnType, TypeApp (funcName, typeArgs, args'))))
            | None ->
                Error (GenericError $"Function {funcName} is not generic, use regular call syntax")
        | Some other ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            Error (UndefinedVariable funcName)

    | TupleLiteral elements ->
        // Type-check each element and build tuple type
        let rec checkElements elems accTypes accExprs =
            match elems with
            | [] -> Ok (List.rev accTypes, List.rev accExprs)
            | e :: rest ->
                checkExpr e env typeReg variantLookup genericFuncReg None
                |> Result.bind (fun (elemType, e') ->
                    checkElements rest (elemType :: accTypes) (e' :: accExprs))

        checkElements elements [] []
        |> Result.bind (fun (elemTypes, elements') ->
            let tupleType = TTuple elemTypes
            match expectedType with
            | Some (TTuple expectedElemTypes) when expectedElemTypes <> elemTypes ->
                Error (TypeMismatch (TTuple expectedElemTypes, tupleType, "tuple literal"))
            | Some other when other <> tupleType ->
                Error (TypeMismatch (other, tupleType, "tuple literal"))
            | _ -> Ok (tupleType, TupleLiteral elements'))

    | TupleAccess (tupleExpr, index) ->
        // Check the tuple expression
        checkExpr tupleExpr env typeReg variantLookup genericFuncReg None
        |> Result.bind (fun (tupleType, tupleExpr') ->
            match tupleType with
            | TTuple elemTypes ->
                if index < 0 || index >= List.length elemTypes then
                    Error (GenericError $"Tuple index {index} out of bounds (tuple has {List.length elemTypes} elements)")
                else
                    let elemType = List.item index elemTypes
                    match expectedType with
                    | Some expected when expected <> elemType ->
                        Error (TypeMismatch (expected, elemType, $"tuple access .{index}"))
                    | _ -> Ok (elemType, TupleAccess (tupleExpr', index))
            | other ->
                Error (GenericError $"Cannot access .{index} on non-tuple type {typeToString other}"))

    | RecordLiteral (typeName, fields) ->
        // Type name is required (parser enforces this, but check for safety)
        if typeName = "" then
            Error (GenericError "Record literal requires type name: use 'TypeName { field = value, ... }'")
        else
            match Map.tryFind typeName typeReg with
            | None ->
                Error (GenericError $"Unknown record type: {typeName}")
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
                        // Type check each field and collect transformed fields
                        let rec checkFieldsInOrder remaining accFields =
                            match remaining with
                            | [] -> Ok (List.rev accFields)
                            | (fname, expectedFieldType) :: rest ->
                                match Map.tryFind fname fieldMap with
                                | Some fieldExpr ->
                                    checkExpr fieldExpr env typeReg variantLookup genericFuncReg (Some expectedFieldType)
                                    |> Result.bind (fun (actualType, fieldExpr') ->
                                        if actualType = expectedFieldType then
                                            checkFieldsInOrder rest ((fname, fieldExpr') :: accFields)
                                        else
                                            Error (TypeMismatch (expectedFieldType, actualType, $"field {fname}")))
                                | None -> checkFieldsInOrder rest accFields // Already checked for missing fields

                        checkFieldsInOrder expectedFields []
                        |> Result.map (fun fields' -> (TRecord typeName, RecordLiteral (typeName, fields')))

    | RecordAccess (recordExpr, fieldName) ->
        // Check the record expression
        checkExpr recordExpr env typeReg variantLookup genericFuncReg None
        |> Result.bind (fun (recordType, recordExpr') ->
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
                        | _ -> Ok (fieldType, RecordAccess (recordExpr', fieldName))
            | other ->
                Error (GenericError $"Cannot access .{fieldName} on non-record type {typeToString other}"))

    | Constructor (constrTypeName, variantName, payload) ->
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
                | _ -> Ok (sumType, expr)
            | None, Some _ ->
                // Variant doesn't take payload but one was provided
                Error (GenericError $"Constructor {variantName} does not take a payload")
            | Some _, None ->
                // Variant requires payload but none provided
                Error (GenericError $"Constructor {variantName} requires a payload")
            | Some payloadType, Some payloadExpr ->
                // Variant with payload - check payload has correct type
                checkExpr payloadExpr env typeReg variantLookup genericFuncReg (Some payloadType)
                |> Result.bind (fun (actualPayloadType, payloadExpr') ->
                    if actualPayloadType <> payloadType then
                        Error (TypeMismatch (payloadType, actualPayloadType, $"payload of {variantName}"))
                    else
                        let sumType = TSum typeName
                        match expectedType with
                        | Some expected when expected <> sumType ->
                            Error (TypeMismatch (expected, sumType, $"constructor {variantName}"))
                        | _ -> Ok (sumType, Constructor (constrTypeName, variantName, Some payloadExpr')))

    | Match (scrutinee, cases) ->
        // Type check the scrutinee first
        checkExpr scrutinee env typeReg variantLookup genericFuncReg None
        |> Result.bind (fun (scrutineeType, scrutinee') ->
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
            // Returns (resultType, transformedCases)
            let rec checkCases (remaining: (Pattern * Expr) list) (resultType: Type option) (accCases: (Pattern * Expr) list) : Result<Type * (Pattern * Expr) list, TypeError> =
                match remaining with
                | [] ->
                    match resultType with
                    | Some t -> Ok (t, List.rev accCases)
                    | None -> Error (GenericError "Match expression must have at least one case")
                | (pattern, body) :: rest ->
                    // Extract bindings from pattern and add to environment
                    extractPatternBindings pattern scrutineeType
                    |> Result.bind (fun bindings ->
                        let bodyEnv = List.fold (fun e (name, ty) -> Map.add name ty e) env bindings
                        checkExpr body bodyEnv typeReg variantLookup genericFuncReg resultType
                        |> Result.bind (fun (bodyType, body') ->
                            match resultType with
                            | None -> checkCases rest (Some bodyType) ((pattern, body') :: accCases)
                            | Some expected when expected = bodyType -> checkCases rest resultType ((pattern, body') :: accCases)
                            | Some expected -> Error (TypeMismatch (expected, bodyType, "match case"))))

            checkCases cases None []
            |> Result.bind (fun (matchType, cases') ->
                match expectedType with
                | Some expected when expected <> matchType ->
                    Error (TypeMismatch (expected, matchType, "match expression"))
                | _ -> Ok (matchType, Match (scrutinee', cases'))))

    | ListLiteral elements ->
        // Type-check each element (must all be int for now - monomorphic)
        let rec checkElements elems accExprs =
            match elems with
            | [] -> Ok (List.rev accExprs)
            | e :: rest ->
                checkExpr e env typeReg variantLookup genericFuncReg (Some TInt64)
                |> Result.bind (fun (elemType, e') ->
                    if elemType <> TInt64 then
                        Error (TypeMismatch (TInt64, elemType, "list element"))
                    else
                        checkElements rest (e' :: accExprs))

        checkElements elements []
        |> Result.bind (fun elements' ->
            match expectedType with
            | Some TList | None -> Ok (TList, ListLiteral elements')
            | Some other -> Error (TypeMismatch (other, TList, "list literal")))

/// Type-check a function definition
/// Returns the transformed function body (with Call -> TypeApp transformations)
let checkFunctionDef (funcDef: FunctionDef) (env: TypeEnv) (typeReg: TypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) : Result<FunctionDef, TypeError> =
    // Build environment with parameters
    let paramEnv =
        funcDef.Params
        |> List.fold (fun e (name, ty) -> Map.add name ty e) env

    // Check body has return type
    checkExpr funcDef.Body paramEnv typeReg variantLookup genericFuncReg (Some funcDef.ReturnType)
    |> Result.bind (fun (bodyType, body') ->
        // For generic functions, we need to compare types considering type variables
        // For now, if the body type contains type variables, just check structural equality
        if bodyType = funcDef.ReturnType then
            Ok { funcDef with Body = body' }
        else
            Error (TypeMismatch (funcDef.ReturnType, bodyType, $"function {funcDef.Name} body")))

/// Type-check a program
/// Returns the type of the main expression and the transformed program
/// The transformed program has Call nodes converted to TypeApp where type inference was applied
let checkProgram (program: Program) : Result<Type * Program, TypeError> =
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

    // Build generic function registry - maps function names to type parameters
    let genericFuncReg : GenericFuncRegistry =
        topLevels
        |> List.choose (function
            | FunctionDef funcDef when not (List.isEmpty funcDef.TypeParams) ->
                Some (funcDef.Name, funcDef.TypeParams)
            | _ -> None)
        |> Map.ofList

    // Third pass: type check all function definitions and collect transformed top-levels
    let rec checkAllTopLevels remaining accTopLevels =
        match remaining with
        | [] -> Ok (List.rev accTopLevels)
        | topLevel :: rest ->
            match topLevel with
            | FunctionDef funcDef ->
                checkFunctionDef funcDef funcEnv typeReg variantLookup genericFuncReg
                |> Result.bind (fun funcDef' ->
                    checkAllTopLevels rest (FunctionDef funcDef' :: accTopLevels))
            | TypeDef _ ->
                // Type definitions pass through unchanged
                checkAllTopLevels rest (topLevel :: accTopLevels)
            | Expression expr ->
                // Main expression - check and transform
                checkExpr expr funcEnv typeReg variantLookup genericFuncReg None
                |> Result.bind (fun (_, expr') ->
                    checkAllTopLevels rest (Expression expr' :: accTopLevels))

    // Type check all top-levels
    checkAllTopLevels topLevels []
    |> Result.bind (fun topLevels' ->
        // Determine result type from main expression or main function
        let mainExpr = topLevels' |> List.tryPick (function Expression e -> Some e | _ -> None)
        match mainExpr with
        | Some expr ->
            // Re-check to get type (we already have transformed expr in topLevels')
            checkExpr expr funcEnv typeReg variantLookup genericFuncReg None
            |> Result.map (fun (typ, _) -> (typ, Program topLevels'))
        | None ->
            // No main expression - just functions
            // For now, require a "main" function with signature () -> int
            match Map.tryFind "main" funcSigs with
            | Some ([], TInt64) -> Ok (TInt64, Program topLevels')
            | Some _ -> Error (GenericError "main function must have signature () -> int")
            | None -> Error (GenericError "Program must have either a main expression or a main() : int function"))
