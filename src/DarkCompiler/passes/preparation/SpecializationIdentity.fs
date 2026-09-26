// SpecializationIdentity.fs - Name concrete generic instances and normalize typed parameters.

module SpecializationIdentity

open ANF

type GenericFunctionArtifact = {
    Symbols: CheckedAST.Symbols
    Function: CheckedAST.FunctionDef
    DirectDependencies: Set<AST.FunctionId>
}

/// Summarize direct semantic dependencies once at the checked-unit boundary.
/// Specialized bodies receive a fresh summary after type substitution.
let rec directDependencies (expr: CheckedAST.Expr) : Set<AST.FunctionId> =
    let combine expressions =
        expressions |> List.fold (fun calls item -> Set.union calls (directDependencies item)) Set.empty
    let args values = values |> AST.NonEmptyList.toList |> combine
    match expr with
    | CheckedAST.FuncRef id -> Set.singleton id
    | CheckedAST.BoundaryRender (id, value) -> Set.add id (directDependencies value)
    | CheckedAST.Call (id, values) | CheckedAST.TypeApp (id, _, values) -> Set.add id (args values)
    | CheckedAST.Closure (id, captures) -> Set.add id (combine captures)
    | CheckedAST.UnaryOp (_, value) | CheckedAST.TupleAccess (value, _)
    | CheckedAST.RecordAccess (value, _) -> directDependencies value
    | CheckedAST.BinOp (_, left, right) | CheckedAST.Sequence (left, right)
    | CheckedAST.Let (_, left, right) | CheckedAST.RecursiveLet (_, left, right) -> combine [left; right]
    | CheckedAST.If (condition, thenBranch, elseBranch) -> combine [condition; thenBranch; elseBranch]
    | CheckedAST.TupleLiteral values -> combine (CheckedAST.tupleElementsToList values)
    | CheckedAST.ListLiteral values -> combine values
    | CheckedAST.DictLiteral (_, _, entries) -> entries |> List.collect (fun (key, value) -> [key; value]) |> combine
    | CheckedAST.RecordLiteral (_, fields) -> fields |> CheckedAST.recordFieldsInSourceOrder |> List.map snd |> combine
    | CheckedAST.RecordUpdate (record, fields) -> combine (record :: List.map snd fields)
    | CheckedAST.Constructor (_, fields) -> combine fields
    | CheckedAST.Match (scrutinee, cases) ->
        cases
        |> AST.NonEmptyList.toList
        |> List.collect (fun case -> case.Body :: Option.toList case.Guard)
        |> fun bodies -> combine (scrutinee :: bodies)
    | CheckedAST.Lambda (_, _, body) -> directDependencies body
    | CheckedAST.Apply (func, values) | CheckedAST.IndirectApply (func, values) ->
        combine (func :: AST.NonEmptyList.toList values)
    | CheckedAST.InterpolatedString parts ->
        parts
        |> List.choose (function CheckedAST.StringExpr value -> Some value | CheckedAST.StringText _ -> None)
        |> combine
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _
    | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
    | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BigIntLiteral _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.BlobLiteral _
    | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.Local _
    | CheckedAST.RuntimeError _ -> Set.empty

/// Generic function registry - maps names to definitions together with the
/// symbol namespace in which their local identities were allocated.
type GenericFuncDefs = Map<string, GenericFunctionArtifact>

/// Specialization key - a generic function instantiated with specific types
type SpecKey = string * AST.SemanticType list  // (funcName, typeArgs)

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
            let artifactSymbols = CheckedAST.catalogForCheckedUnit symbols
            Some (f.Name, {
                Symbols = artifactSymbols
                Function = f
                DirectDependencies = directDependencies f.Body
            })
        | _ -> None)
    |> Map.ofList

let importSpecializedFunctions
    (targetSymbols: CheckedAST.Symbols)
    (artifacts: GenericFunctionArtifact list)
    : CheckedAST.Symbols * CheckedAST.FunctionDef list =
    artifacts
    |> List.fold (fun (symbols, functions) artifact ->
        // Specializations are produced from immutable forks of a source symbol
        // table. Equal namespace tokens therefore do not imply that ordinals
        // allocated on separate forks identify the same generated function.
        // Import each artifact through its names so the destination owns one
        // collision-free identity namespace.
        let symbols, imported =
            CheckedAST.composeTopLevels
                artifact.Symbols
                symbols
                [CheckedAST.FunctionDef artifact.Function]
        let importedFunction =
            match imported with
            | [CheckedAST.FunctionDef functionDef] -> functionDef
            | _ -> Crash.crash "Generic function import changed its top-level shape"
        (symbols, importedFunction :: functions)) (targetSymbols, [])
    |> fun (symbols, functions) -> (symbols, List.rev functions)

let private mangleTypeVarName (name: string) : string =
    name.Replace("_", "$u")

/// Convert a type to a string for name mangling
let rec typeToMangledName (t: AST.SemanticType) : string =
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
    | AST.TNever -> "runtime_error"
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
    | AST.TInferenceVar (displayName, _) -> mangleTypeVarName displayName
    | AST.TInternalRawPtr -> "rawptr"  // Internal raw pointer type

/// Check if a type contains any type variables
let rec containsTypeVar (t: AST.SemanticType) : bool =
    match t with
    | AST.TVar _ | AST.TInferenceVar _ -> true
    | AST.TFunction (paramTypes, retType) ->
        List.exists containsTypeVar paramTypes || containsTypeVar retType
    | AST.TTuple elemTypes -> List.exists containsTypeVar elemTypes
    | AST.TRecord (_, typeArgs) -> List.exists containsTypeVar typeArgs
    | AST.TSum (_, typeArgs) -> List.exists containsTypeVar typeArgs
    | AST.TList elemType -> containsTypeVar elemType
    | AST.TDict (keyType, valueType) -> containsTypeVar keyType || containsTypeVar valueType
    | _ -> false

/// Generate a specialized function name
let specName (funcName: string) (typeArgs: AST.SemanticType list) : string =
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
    (parameters: AST.NonEmptyList<AST.BindingId * AST.SemanticType>)
    : (AST.BindingId * AST.SemanticType) list =
    AST.NonEmptyList.toList parameters

let internal lambdaParameterType (parameter: CheckedAST.LambdaParameter) : AST.SemanticType =
    CheckedAST.semanticType parameter.Type

let rec internal letPatternBindingTypes
    (pattern: CheckedAST.LetPattern)
    (typ: AST.SemanticType)
    : (AST.BindingId * AST.SemanticType) list =
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
    : (AST.BindingId * AST.SemanticType) list =
    letPatternBindingTypes parameter.Pattern (lambdaParameterType parameter)

let internal lowerLambdaParameters
    (symbols: CheckedAST.Symbols)
    (parameters: AST.NonEmptyList<CheckedAST.LambdaParameter>)
    (body: CheckedAST.Expr)
    : (AST.BindingId * AST.SemanticType) list * CheckedAST.Expr * CheckedAST.Symbols =
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
    (parameters: (AST.BindingId * AST.SemanticType) list)
    : AST.NonEmptyList<AST.BindingId * AST.SemanticType> =
    match AST.NonEmptyList.tryFromList parameters with
    | Some nonEmptyParams -> nonEmptyParams
    | None -> Crash.crash $"Internal error: {context} produced zero parameters"

let private syntheticUnitParamPrefix = "$unit"

let private isSyntheticUnitParam
    (symbols: CheckedAST.Symbols)
    ((paramId, paramType): AST.BindingId * AST.SemanticType)
    : bool =
    paramType = AST.TUnit
    && (CheckedAST.bindingName paramId symbols
        |> Option.map (fun name -> name.StartsWith(syntheticUnitParamPrefix))
        |> Option.defaultValue false)

let internal normalizeSyntheticNullaryParams
    (symbols: CheckedAST.Symbols)
    (parameters: (AST.BindingId * AST.SemanticType) list)
    : (AST.BindingId * AST.SemanticType) list =
    match parameters with
    | [singleParam] when isSyntheticUnitParam symbols singleParam -> []
    | _ -> parameters

let internal normalizeSyntheticNullaryArgAtoms
    (paramTypes: AST.SemanticType list)
    (argExprs: CheckedAST.Expr list)
    (argAtoms: ANF.Atom list)
    : ANF.Atom list =
    match paramTypes, argExprs, argAtoms with
    | [], [CheckedAST.UnitLiteral], [_] -> []
    | _ -> argAtoms

let internal unresolvedKeyIntrinsicTypeArgErrorExpr
    (runtimeErrorId: AST.FunctionId)
    (funcName: string)
    : CheckedAST.Expr =
    CheckedAST.Call (
        runtimeErrorId,
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
