// SpecializationIdentity.fs - Name concrete generic instances and normalize typed parameters.

module SpecializationIdentity

open ANF

/// Generic function registry - maps generic function names to their definitions
type GenericFuncDefs = Map<string, AST.FunctionDef>

/// Specialization key - a generic function instantiated with specific types
type SpecKey = string * AST.Type list  // (funcName, typeArgs)

/// Specialization registry - tracks which specializations are needed
/// Maps (funcName, typeArgs) -> specialized name
type SpecRegistry = Map<SpecKey, string>

/// Result of specializing generic functions from a spec set
type SpecializationResult = {
    SpecializedFuncs: AST.FunctionDef list
    SpecRegistry: SpecRegistry
    ExternalSpecs: Set<SpecKey>
}

/// Extract generic function definitions (functions with type parameters)
/// from a program. Used for on-demand monomorphization of stdlib generics.
let extractGenericFuncDefs (program: AST.Program) : GenericFuncDefs =
    let (AST.Program topLevels) = program
    topLevels
    |> List.choose (function
        | AST.FunctionDef f when not (List.isEmpty f.TypeParams) -> Some (f.Name, f)
        | _ -> None)
    |> Map.ofList

let private mangleTypeVarName (name: string) : string =
    name.Replace("_", "$u")

/// Convert a type to a string for name mangling
let rec typeToMangledName (t: AST.Type) : string =
    match t with
    | AST.TInt8 -> "i8"
    | AST.TInt16 -> "i16"
    | AST.TInt32 -> "i32"
    | AST.TInt64 -> "i64"
    | AST.TInt128 -> "i128"
    | AST.TInt -> "int"
    | AST.TUInt8 -> "u8"
    | AST.TUInt16 -> "u16"
    | AST.TUInt32 -> "u32"
    | AST.TUInt64 -> "u64"
    | AST.TUInt128 -> "u128"
    | AST.TBool -> "bool"
    | AST.TFloat64 -> "f64"
    | AST.TString -> "str"
    | AST.TBlob -> "blob"
    | AST.TChar -> "char"
    | AST.TDateTime -> "datetime"
    | AST.TUnit -> "unit"
    | AST.TRuntimeError -> "runtime_error"
    | AST.TFunction (paramTypes, retType) ->
        let paramStr = paramTypes |> List.map typeToMangledName |> String.concat "_"
        let retStr = typeToMangledName retType
        $"fn_{paramStr}_to_{retStr}"
    | AST.TTuple elemTypes ->
        let elemsStr = elemTypes |> List.map typeToMangledName |> String.concat "_"
        $"tup{List.length elemTypes}_{elemsStr}"
    | AST.TEnumFields fieldTypes ->
        let fieldsStr = fieldTypes |> List.map typeToMangledName |> String.concat "_"
        $"enumfields_{fieldsStr}"
    | AST.TRecord (name, []) -> name
    | AST.TRecord (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{name}_{argsStr}"
    | AST.TSum (name, []) -> name
    | AST.TSum (name, typeArgs) ->
        let argsStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{name}_{argsStr}"
    | AST.TList elemType -> $"list_{typeToMangledName elemType}"
    | AST.TStream elemType -> $"stream_{typeToMangledName elemType}"
    | AST.TDict (keyType, valueType) -> $"dict_{typeToMangledName keyType}_{typeToMangledName valueType}"
    | AST.TVar name -> mangleTypeVarName name  // Should not appear after monomorphization
    | AST.TRawPtr -> "rawptr"  // Internal raw pointer type

/// Check if a type contains any type variables
let rec containsTypeVar (t: AST.Type) : bool =
    match t with
    | AST.TVar _ -> true
    | AST.TFunction (paramTypes, retType) ->
        List.exists containsTypeVar paramTypes || containsTypeVar retType
    | AST.TTuple elemTypes -> List.exists containsTypeVar elemTypes
    | AST.TEnumFields fieldTypes -> List.exists containsTypeVar fieldTypes
    | AST.TRecord (_, typeArgs) -> List.exists containsTypeVar typeArgs
    | AST.TSum (_, typeArgs) -> List.exists containsTypeVar typeArgs
    | AST.TList elemType -> containsTypeVar elemType
    | AST.TDict (keyType, valueType) -> containsTypeVar keyType || containsTypeVar valueType
    | _ -> false

/// Generate a specialized function name
let specName (funcName: string) (typeArgs: AST.Type list) : string =
    if List.isEmpty typeArgs then
        funcName
    else
        let typeStr = typeArgs |> List.map typeToMangledName |> String.concat "_"
        $"{funcName}_{typeStr}"

let internal isGenericKeyIntrinsicName (funcName: string) : bool =
    funcName = "__hash" || funcName = "__key_eq" || funcName = "__dict_key_string" || funcName = "__dict_validate_key"

let internal exprArgsToList (args: AST.NonEmptyList<AST.Expr>) : AST.Expr list =
    AST.NonEmptyList.toList args

let internal exprArgsFromList (args: AST.Expr list) : AST.NonEmptyList<AST.Expr> =
    match AST.NonEmptyList.tryFromList args with
    | Some nonEmptyArgs -> nonEmptyArgs
    | None -> AST.NonEmptyList.singleton AST.UnitLiteral

let internal paramsToList (parameters: AST.NonEmptyList<string * AST.Type>) : (string * AST.Type) list =
    AST.NonEmptyList.toList parameters

let internal lambdaParameterType (parameter: AST.LambdaParameter) : AST.Type =
    parameter.InferredType
    |> Option.defaultWith (fun () -> Crash.crash "Lambda parameter reached ANF lowering without an inferred type")

let rec internal letPatternBindingTypes
    (pattern: AST.LetPattern)
    (typ: AST.Type)
    : (string * AST.Type) list =
    match pattern, typ with
    | AST.LPVariable name, bindingType -> [(name, bindingType)]
    | AST.LPWildcard, _ | AST.LPUnit, _ -> []
    | AST.LPTuple (first, second, rest), AST.TTuple elementTypes ->
        let patterns = first :: second :: rest
        if List.length patterns <> List.length elementTypes then
            Crash.crash "Typed lambda tuple pattern changed arity before ANF lowering"
        else
            List.zip patterns elementTypes
            |> List.collect (fun (innerPattern, innerType) ->
                letPatternBindingTypes innerPattern innerType)
    | AST.LPTuple _, _ ->
        Crash.crash "Typed lambda tuple pattern lost its tuple type before ANF lowering"

let internal lambdaParameterBindings (parameter: AST.LambdaParameter) : (string * AST.Type) list =
    letPatternBindingTypes parameter.Pattern (lambdaParameterType parameter)

let internal lowerLambdaParameters
    (parameters: AST.NonEmptyList<AST.LambdaParameter>)
    (body: AST.Expr)
    : (string * AST.Type) list * AST.Expr =
    parameters
    |> AST.NonEmptyList.toList
    |> List.mapi (fun index parameter ->
        let parameterType = lambdaParameterType parameter
        match parameter.Pattern with
        | AST.LPVariable name -> ((name, parameterType), None)
        | pattern ->
            let argumentName = $"__lambda_pattern_arg_{index}"
            ((argumentName, parameterType), Some (pattern, argumentName)))
    |> fun lowered ->
        let functionParameters = lowered |> List.map fst
        let destructuredBody =
            lowered
            |> List.choose snd
            |> List.foldBack (fun (pattern, argumentName) continuation ->
                AST.Let (pattern, AST.Var argumentName, continuation)) <| body
        (functionParameters, destructuredBody)

let internal paramsFromList (context: string) (parameters: (string * AST.Type) list) : AST.NonEmptyList<string * AST.Type> =
    match AST.NonEmptyList.tryFromList parameters with
    | Some nonEmptyParams -> nonEmptyParams
    | None -> Crash.crash $"Internal error: {context} produced zero parameters"

let private syntheticUnitParamPrefix = "$unit"

let private isSyntheticUnitParam ((paramName, paramType): string * AST.Type) : bool =
    paramType = AST.TUnit && paramName.StartsWith(syntheticUnitParamPrefix)

let internal normalizeSyntheticNullaryParams (parameters: (string * AST.Type) list) : (string * AST.Type) list =
    match parameters with
    | [singleParam] when isSyntheticUnitParam singleParam -> []
    | _ -> parameters

let internal normalizeSyntheticNullaryArgAtoms
    (paramTypes: AST.Type list)
    (argExprs: AST.Expr list)
    (argAtoms: ANF.Atom list)
    : ANF.Atom list =
    match paramTypes, argExprs, argAtoms with
    | [], [AST.UnitLiteral], [_] -> []
    | _ -> argAtoms

let internal unresolvedKeyIntrinsicTypeArgErrorExpr (funcName: string) : AST.Expr =
    let helper =
        match funcName with
        | "__hash" -> "__unsupported_key_hash"
        | "__key_eq" -> "__unsupported_key_equal"
        | "__dict_key_string" -> "__unsupported_key_string"
        | "__dict_validate_key" -> "__unsupported_key_validate"
        | _ -> Crash.crash $"Unknown dictionary key intrinsic: {funcName}"
    AST.Call (helper, AST.NonEmptyList.singleton AST.UnitLiteral)

/// Preserve left-to-right argument evaluation before forcing a runtime error.
let internal wrapWithIgnoredArgEvaluations (args: AST.Expr list) (body: AST.Expr) : AST.Expr =
    args
    |> List.indexed
    |> List.rev
    |> List.fold (fun acc (index, argExpr) ->
        AST.Let (AST.LPVariable $"__dark_internal_unresolved_arg_eval_{index}", argExpr, acc)) body

/// Type substitution - maps type variable names to concrete types
