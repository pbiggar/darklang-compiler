// SpecializationIdentity.fs - Name concrete generic instances and normalize typed parameters.

module SpecializationIdentity

open ANF

type GenericFunctionArtifact = {
    Symbols: CheckedAST.Symbols
    Function: CheckedAST.FunctionDef
}

/// Generic function registry - maps names to definitions together with the
/// symbol namespace in which their local identities were allocated.
type GenericFuncDefs = Map<string, GenericFunctionArtifact>

/// Specialization key - a generic function instantiated with specific types
type SpecKey = string * AST.Type list  // (funcName, typeArgs)

/// Specialization registry - tracks which specializations are needed
/// Maps (funcName, typeArgs) -> specialized name
type SpecRegistry = Map<SpecKey, string>

/// Result of specializing generic functions from a spec set
type SpecializationResult = {
    SpecializedFuncs: GenericFunctionArtifact list
    SpecRegistry: SpecRegistry
    ExternalSpecs: Set<SpecKey>
}

/// Extract generic function definitions (functions with type parameters)
/// from a program. Used for on-demand monomorphization of stdlib generics.
let extractGenericFuncDefs (program: CheckedAST.Program) : GenericFuncDefs =
    let symbols = CheckedAST.programSymbols program
    let topLevels = CheckedAST.programTopLevels program
    topLevels
    |> List.choose (function
        | CheckedAST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
            Some (f.Name, { Symbols = symbols; Function = f })
        | _ -> None)
    |> Map.ofList

let importSpecializedFunctions
    (targetSymbols: CheckedAST.Symbols)
    (artifacts: GenericFunctionArtifact list)
    : CheckedAST.Symbols * CheckedAST.FunctionDef list =
    let rec groupByNamespace remaining =
        match remaining with
        | [] -> []
        | first :: _ ->
            let same, rest =
                remaining
                |> List.partition (fun artifact ->
                    CheckedAST.sameSymbolNamespace first.Symbols artifact.Symbols)
            same :: groupByNamespace rest
    groupByNamespace artifacts
    |> List.fold (fun (symbols, functions) group ->
        let sourceSymbols = (List.head group).Symbols
        if CheckedAST.sameSymbolNamespace sourceSymbols symbols then
            (symbols, functions @ (group |> List.map (fun artifact -> artifact.Function)))
        else
            let symbols, imported =
                group
                |> List.map (fun artifact -> CheckedAST.FunctionDef artifact.Function)
                |> CheckedAST.importTopLevels sourceSymbols symbols
            let importedFunctions =
                imported
                |> List.map (function
                    | CheckedAST.FunctionDef functionDef -> functionDef
                    | _ -> Crash.crash "Generic function import changed its top-level shape")
            (symbols, functions @ importedFunctions)) (targetSymbols, [])

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
    funcName = "__hash" || funcName = "__key_eq"

let internal exprArgsToList (args: AST.NonEmptyList<CheckedAST.Expr>) : CheckedAST.Expr list =
    AST.NonEmptyList.toList args

let internal exprArgsFromList (args: CheckedAST.Expr list) : AST.NonEmptyList<CheckedAST.Expr> =
    match AST.NonEmptyList.tryFromList args with
    | Some nonEmptyArgs -> nonEmptyArgs
    | None -> AST.NonEmptyList.singleton CheckedAST.UnitLiteral

let internal paramsToList
    (parameters: AST.NonEmptyList<AST.BindingId * AST.Type>)
    : (AST.BindingId * AST.Type) list =
    AST.NonEmptyList.toList parameters

let internal lambdaParameterType (parameter: CheckedAST.LambdaParameter) : AST.Type =
    parameter.Type

let rec internal letPatternBindingTypes
    (pattern: CheckedAST.LetPattern)
    (typ: AST.Type)
    : (AST.BindingId * AST.Type) list =
    match pattern, typ with
    | CheckedAST.LPVariable name, bindingType -> [(name, bindingType)]
    | CheckedAST.LPWildcard, _ | CheckedAST.LPUnit, _ -> []
    | CheckedAST.LPTuple (first, second, rest), AST.TTuple elementTypes ->
        let patterns = first :: second :: rest
        if List.length patterns <> List.length elementTypes then
            Crash.crash "Typed lambda tuple pattern changed arity before ANF lowering"
        else
            List.zip patterns elementTypes
            |> List.collect (fun (innerPattern, innerType) ->
                letPatternBindingTypes innerPattern innerType)
    | CheckedAST.LPTuple _, _ ->
        Crash.crash "Typed lambda tuple pattern lost its tuple type before ANF lowering"

let internal lambdaParameterBindings
    (parameter: CheckedAST.LambdaParameter)
    : (AST.BindingId * AST.Type) list =
    letPatternBindingTypes parameter.Pattern (lambdaParameterType parameter)

let internal lowerLambdaParameters
    (symbols: CheckedAST.Symbols)
    (parameters: AST.NonEmptyList<CheckedAST.LambdaParameter>)
    (body: CheckedAST.Expr)
    : (AST.BindingId * AST.Type) list * CheckedAST.Expr * CheckedAST.Symbols =
    parameters
    |> AST.NonEmptyList.toList
    |> List.mapi (fun index parameter -> index, parameter)
    |> List.mapFold (fun currentSymbols (index, parameter) ->
        let parameterType = lambdaParameterType parameter
        match parameter.Pattern with
        | CheckedAST.LPVariable id -> (((id, parameterType), None), currentSymbols)
        | pattern ->
            let (argumentId, nextSymbols) =
                CheckedAST.allocateBinding $"__lambda_pattern_arg_{index}" currentSymbols
            (((argumentId, parameterType), Some (pattern, argumentId)), nextSymbols)) symbols
    |> fun (lowered, symbols') ->
        let functionParameters = lowered |> List.map fst
        let destructuredBody =
            lowered
            |> List.choose snd
            |> List.foldBack (fun (pattern, argumentId) continuation ->
                CheckedAST.Let (pattern, CheckedAST.Local argumentId, continuation)) <| body
        (functionParameters, destructuredBody, symbols')

let internal paramsFromList
    (context: string)
    (parameters: (AST.BindingId * AST.Type) list)
    : AST.NonEmptyList<AST.BindingId * AST.Type> =
    match AST.NonEmptyList.tryFromList parameters with
    | Some nonEmptyParams -> nonEmptyParams
    | None -> Crash.crash $"Internal error: {context} produced zero parameters"

let private syntheticUnitParamPrefix = "$unit"

let private isSyntheticUnitParam
    (symbols: CheckedAST.Symbols)
    ((paramId, paramType): AST.BindingId * AST.Type)
    : bool =
    paramType = AST.TUnit
    && (CheckedAST.bindingName paramId symbols
        |> Option.map (fun name -> name.StartsWith(syntheticUnitParamPrefix))
        |> Option.defaultValue false)

let internal normalizeSyntheticNullaryParams
    (symbols: CheckedAST.Symbols)
    (parameters: (AST.BindingId * AST.Type) list)
    : (AST.BindingId * AST.Type) list =
    match parameters with
    | [singleParam] when isSyntheticUnitParam symbols singleParam -> []
    | _ -> parameters

let internal normalizeSyntheticNullaryArgAtoms
    (paramTypes: AST.Type list)
    (argExprs: CheckedAST.Expr list)
    (argAtoms: ANF.Atom list)
    : ANF.Atom list =
    match paramTypes, argExprs, argAtoms with
    | [], [CheckedAST.UnitLiteral], [_] -> []
    | _ -> argAtoms

let internal unresolvedKeyIntrinsicTypeArgErrorExpr (funcName: string) : CheckedAST.Expr =
    CheckedAST.Call (
        AST.functionIdForName "Builtin.testRuntimeError",
        AST.NonEmptyList.singleton (CheckedAST.StringLiteral $"Internal error: unresolved type arguments for {funcName}")
    )

/// Preserve left-to-right argument evaluation before forcing a runtime error.
let internal wrapWithIgnoredArgEvaluations
    (args: CheckedAST.Expr list)
    (body: CheckedAST.Expr)
    : CheckedAST.Expr =
    args
    |> List.rev
    |> List.fold (fun acc argExpr ->
        CheckedAST.Let (CheckedAST.LPWildcard, argExpr, acc)) body

/// Type substitution - maps type variable names to concrete types
