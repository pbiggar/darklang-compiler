// LiftFunctions.fs - Resolve lifted function references and program-level closure wrappers.

module LiftFunctions

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open TypeSubstitution
open ClosureAnalysis
open ClosureComparisons
open LiftExpressions

let liftLambdasInFunc (funcDef: CheckedAST.FunctionDef) (state: LiftState) : Result<CheckedAST.FunctionDef * LiftState, string> =
    // Add function parameters to the type environment
    let paramTypes = funcDef.Params |> paramsToList |> Map.ofList
    let stateWithParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv paramTypes }
    liftLambdasInExpr funcDef.Body stateWithParams
    |> Result.map (fun (body', state') ->
        // Restore original TypeEnv (remove parameters) after processing the function
        ({ funcDef with Body = body' }, { state' with TypeEnv = state.TypeEnv }))

/// State extended to include known function names and their parameters
type LiftStateWithFuncs = {
    State: LiftState
    FuncParams: Map<string, AST.SemanticType list>
    GeneratedWrappers: Map<string, AST.FunctionId * AST.FunctionId * AST.FunctionId>
}

/// Generate a wrapper for a named function used as a value
let generateFuncWrapper
    (origFuncName: string)
    (funcParams: Map<string, AST.SemanticType list>)
    (funcReturnTypes: Map<string, AST.SemanticType>)
    (stateWithFuncs: LiftStateWithFuncs)
    : Result<(CheckedAST.FunctionDef * LiftStateWithFuncs), string> =
    match Map.tryFind origFuncName funcParams, Map.tryFind origFuncName funcReturnTypes with
    | Some parameters, Some returnType ->
        // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
        let (wrapperName, stateWithName) = freshLiftedName stateWithFuncs.State "__funcref_wrapper_"
        let comparatorStorageType = AST.TInternalRawPtr
        let (closureId, symbols) =
            CheckedAST.allocateBinding "__closure" stateWithName.Symbols
        let parameters, symbols =
            parameters
            |> List.mapi (fun index typ -> (index, typ))
            |> List.mapFold (fun symbols (index, typ) ->
                let (id, symbols) = CheckedAST.allocateBinding $"__arg{index}" symbols
                ((id, typ), symbols)) symbols
        let closureParam =
            (closureId, AST.TTuple [AST.TInt64; comparatorStorageType])
        let (originalId, symbols) = CheckedAST.internFunction origFuncName symbols
        let (wrapperId, symbols) = CheckedAST.internFunction wrapperName symbols
        let wrapperBody =
            parameters
            |> List.map (fun (id, _) -> CheckedAST.Local id)
            |> exprArgsFromList
            |> fun args -> CheckedAST.Call (originalId, args)
        let wrapperDef : CheckedAST.FunctionDef = {
            Id = wrapperId
            Name = wrapperName
            TypeParams = []
            Params = paramsFromList "generateFuncWrapper" (closureParam :: parameters)
            ReturnType = returnType
            Body = wrapperBody
            Recursion = None
        }
        let comparisonDef, symbols =
            makeClosureComparator
                $"{wrapperName}__comparison"
                []
                false
                stateWithFuncs.State.VariantLookup
                symbols
        let newState = {
            stateWithFuncs with
                State = {
                    stateWithName with
                        Symbols = symbols
                        LiftedFunctions = comparisonDef :: stateWithName.LiftedFunctions
                }
                GeneratedWrappers =
                    Map.add
                        origFuncName
                        (originalId, wrapperId, comparisonDef.Id)
                        stateWithFuncs.GeneratedWrappers
        }
        Ok (wrapperDef, newState)
    | None, _ ->
        Error $"Cannot find parameters for function '{origFuncName}'"
    | _, None ->
        Error $"Cannot find return type for function '{origFuncName}'"

let rec private containsIndirectApply (expr: CheckedAST.Expr) : bool =
    let anyExpr exprs = List.exists containsIndirectApply exprs
    match expr with
    | CheckedAST.IndirectApply _ -> true
    | CheckedAST.BoundaryRender (_, value) -> containsIndirectApply value
    | CheckedAST.BinOp (_, left, right) -> containsIndirectApply left || containsIndirectApply right
    | CheckedAST.UnaryOp (_, inner) -> containsIndirectApply inner
    | CheckedAST.Let (_, value, body) -> containsIndirectApply value || containsIndirectApply body
    | CheckedAST.RecursiveLet (_, value, body) -> containsIndirectApply value || containsIndirectApply body
    | CheckedAST.If (condition, thenBranch, elseBranch) ->
        containsIndirectApply condition
        || containsIndirectApply thenBranch
        || containsIndirectApply elseBranch
    | CheckedAST.Sequence (first, next) -> containsIndirectApply first || containsIndirectApply next
    | CheckedAST.Call (_, args)
    | CheckedAST.TypeApp (_, _, args) -> args |> exprArgsToList |> anyExpr
    | CheckedAST.TupleLiteral elements
    | CheckedAST.ListLiteral elements -> anyExpr elements
    | CheckedAST.TupleAccess (tuple, _) -> containsIndirectApply tuple
    | CheckedAST.DictLiteral (_, _, entries) ->
        entries |> List.collect (fun (key, value) -> [key; value]) |> anyExpr
    | CheckedAST.RecordLiteral (_, entries) -> entries |> List.map snd |> anyExpr
    | CheckedAST.RecordUpdate (record, updates) ->
        containsIndirectApply record || (updates |> List.map snd |> anyExpr)
    | CheckedAST.RecordAccess (record, _) -> containsIndirectApply record
    | CheckedAST.Constructor (_, fields) -> List.exists containsIndirectApply fields
    | CheckedAST.Match (scrutinee, cases) ->
        containsIndirectApply scrutinee
        || (cases
            |> List.exists (fun case ->
                Option.exists containsIndirectApply case.Guard
                || containsIndirectApply case.Body))
    | CheckedAST.Lambda (_, _, body) -> containsIndirectApply body
    | CheckedAST.Apply (func, args) ->
        containsIndirectApply func || (args |> exprArgsToList |> anyExpr)
    | CheckedAST.Closure (_, captures) -> anyExpr captures
    | CheckedAST.InterpolatedString parts ->
        parts
        |> List.exists (function
            | CheckedAST.StringText _ -> false
            | CheckedAST.StringExpr partExpr -> containsIndirectApply partExpr)
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _
    | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.BlobLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Local _ | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> false

/// A separately compiled caller may compare any closure returned by this
/// compilation unit. Retain comparator metadata for function values nested in
/// exported result types without requiring advance knowledge of their callers.
let private collectEscapingFunctionParams
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (typ: AST.SemanticType)
    : Set<AST.SemanticType list> =
    let rec collect (visited: Set<AST.SemanticType>) (current: AST.SemanticType) =
        if Set.contains current visited then
            Set.empty
        else
            let visited = Set.add current visited
            let collectMany types =
                types
                |> List.map (collect visited)
                |> List.fold Set.union Set.empty
            match current with
            | AST.TFunction (paramTypes, returnType) ->
                Set.add paramTypes (collect visited returnType)
            | AST.TList elementType
            | AST.TStream elementType ->
                collect visited elementType
            | AST.TDict (keyType, valueType) ->
                Set.union (collect visited keyType) (collect visited valueType)
            | AST.TTuple elementTypes ->
                collectMany elementTypes
            | AST.TRecord (typeName, typeArgs) ->
                match Map.tryFind typeName typeReg with
                | None -> Set.empty
                | Some recordInfo ->
                    let substitution =
                        buildDeclaredRecordFieldSubst recordInfo typeArgs
                        |> Option.defaultValue Map.empty
                    recordInfo.Fields
                    |> List.map (snd >> applySubstToType substitution)
                    |> collectMany
            | AST.TSum (typeName, typeArgs) ->
                variantLookup
                |> Map.toList
                |> List.collect (fun (_, (declaredTypeName, typeParams, _, fieldTypes)) ->
                    if declaredTypeName <> typeName then
                        []
                    else
                        let substitution =
                            if List.length typeParams = List.length typeArgs then
                                List.zip typeParams typeArgs |> Map.ofList
                            else
                                Map.empty
                        fieldTypes |> List.map (applySubstToType substitution))
                |> List.distinct
                |> collectMany
            | AST.TInt64 | AST.TInt128 | AST.TInt | AST.TInt32 | AST.TInt16 | AST.TInt8
            | AST.TUInt64 | AST.TUInt128 | AST.TUInt32 | AST.TUInt16 | AST.TUInt8
            | AST.TBool | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
            | AST.TFloat64 | AST.TUnit | AST.TNever | AST.TInternalRawPtr | AST.TVar _ ->
                Set.empty
    collect Set.empty typ

/// Canonical type view reused by lambda lifting when a compilation unit adds
/// no local type declarations. Context builders compute it once; units with
/// local declarations still rebuild the merged view below.
let prepareLambdaLiftBaseTypes
    (baseTypeReg: TypeRegistry)
    (baseVariantLookup: VariantLookup)
    : TypeRegistry * VariantLookup =
    let sumTypeNames = sumTypeNamesFromVariantLookup baseVariantLookup
    let canonicalVariantLookup =
        baseVariantLookup
        |> Map.map (fun _ (typeName, typeParams, tag, fieldTypes) ->
            let isCatalogBoundaryType =
                typeName.StartsWith("Darklang.LanguageTools.ProgramTypes.")
                || typeName.StartsWith("Darklang.LanguageTools.RuntimeTypes.")
            (typeName,
             typeParams,
             tag,
             if isCatalogBoundaryType then
                 fieldTypes
                 |> List.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)
             else
                 fieldTypes))
    let recordNames = baseTypeReg |> Map.keys |> Set.ofSeq
    let canonicalTypeReg =
        baseTypeReg
        |> Map.map (fun _ info ->
            { info with
                Fields =
                    info.Fields
                    |> List.map (fun (fieldName, fieldType) ->
                        (fieldName,
                         canonicalizeNamedTypeRefs
                            recordNames
                            sumTypeNames
                            fieldType)) })
    (canonicalTypeReg, canonicalVariantLookup)

/// Lift lambdas in a program, generating new top-level functions
let rec liftLambdasInProgram
    (baseTypeReg: TypeRegistry)
    (baseVariantLookup: VariantLookup)
    (baseFuncParams: Map<string, (string * AST.SemanticType) list>)
    (baseFuncReturnTypes: Map<string, AST.SemanticType>)
    (program: CheckedAST.Program)
    : Result<CheckedAST.Program, string> =
    let (CheckedAST.Program (symbols, topLevels)) = program

    let typeRegBase : TypeRegistry =
        topLevels
        |> List.choose (function
            | CheckedAST.TypeDef (_, AST.RecordDef (name, typeParams, fields)) ->
                Some (name, { TypeParams = typeParams; Fields = firstDeclaredRecordFields fields })
            | _ -> None)
        |> Map.ofList

    let aliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | CheckedAST.TypeDef (_, AST.TypeAlias (name, typeParams, targetType)) -> Some (name, (typeParams, targetType))
            | _ -> None)
        |> Map.ofList

    let typeReg =
        typeRegBase
        |> resolveAliasesInTypeRegistry aliasReg
        |> fun reg -> expandTypeRegWithAliases reg aliasReg

    let variantLookup : VariantLookup =
        let localTypeDefs =
            topLevels
            |> List.choose (function | CheckedAST.TypeDef (_, typeDef) -> Some typeDef | _ -> None)
        let collidingCaseNames = AST.collidingConstructorCaseNames localTypeDefs
        topLevels
        |> List.choose (function
            | CheckedAST.TypeDef (_, AST.SumTypeDef (typeName, typeParams, variants)) ->
                Some (typeName, typeParams, variants)
            | _ -> None)
        |> List.fold (fun lookup (typeName, typeParams, variants) ->
            variants
            |> List.indexed
            |> List.fold (fun typeLookup (ordinal, variant) ->
                let tag =
                    if Set.contains variant.Name collidingCaseNames then
                        AST.constructorRuntimeIdentity typeName variant.Name
                    else
                        ordinal
                let info = (typeName, typeParams, tag, variant.Fields)
                let withBare =
                    if Map.containsKey variant.Name typeLookup then typeLookup
                    else Map.add variant.Name info typeLookup
                Map.add $"{typeName}.{variant.Name}" info withBare) lookup) Map.empty

    let mergeMapsLocal m1 m2 = Map.fold (fun acc k v -> Map.add k v acc) m1 m2
    let mergedTypeReg = mergeMapsLocal baseTypeReg typeReg
    let rawMergedVariantLookup = mergeMapsLocal baseVariantLookup variantLookup
    let mergedSumTypeNames = sumTypeNamesFromVariantLookup rawMergedVariantLookup
    let mergedVariantLookup =
        if Map.isEmpty variantLookup then
            baseVariantLookup
        else
            rawMergedVariantLookup
            |> Map.map (fun _ (typeName, typeParams, tag, fieldTypes) ->
                let isCatalogBoundaryType =
                    typeName.StartsWith("Darklang.LanguageTools.ProgramTypes.")
                    || typeName.StartsWith("Darklang.LanguageTools.RuntimeTypes.")
                (typeName,
                 typeParams,
                 tag,
                 if isCatalogBoundaryType then
                     fieldTypes
                     |> List.map (canonicalizeBareSumTypeRefsWithNames mergedSumTypeNames)
                 else
                     fieldTypes))
    let canonicalMergedTypeReg =
        if Map.isEmpty typeReg && Map.isEmpty variantLookup then
            baseTypeReg
        else
            let recordNames = mergedTypeReg |> Map.keys |> Set.ofSeq
            mergedTypeReg
            |> Map.map (fun _ info ->
                { info with
                    Fields =
                        info.Fields
                        |> List.map (fun (fieldName, fieldType) ->
                            (fieldName,
                             canonicalizeNamedTypeRefs recordNames mergedSumTypeNames fieldType)) })

    // First pass: collect all function definitions and their parameters
    let userFuncParams : Map<string, AST.SemanticType list> =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef f ->
                Some (f.Name, f.Params |> paramsToList |> List.map snd)
            | _ -> None)
        |> Map.ofList

    // Collect user function return types
    let userFuncReturnTypes : Map<string, AST.SemanticType> =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef f -> Some (f.Name, f.ReturnType)
            | _ -> None)
        |> Map.ofList

    // Add module function parameters from Stdlib
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let moduleFuncParams : Map<string, AST.SemanticType list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            (qualifiedName, moduleFunc.ParamTypes))
        |> Map.ofList

    // Collect module function return types
    let moduleFuncReturnTypes : Map<string, AST.SemanticType> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) -> (qualifiedName, moduleFunc.ReturnType))
        |> Map.ofList

    // Collect user generic function definitions (for TypeApp substitution)
    let userGenericFuncDefs : Map<string, string list * AST.SemanticType> =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                Some (f.Name, (f.TypeParams, f.ReturnType))
            | _ -> None)
        |> Map.ofList

    // Collect module generic function definitions (for TypeApp substitution)
    let moduleGenericFuncDefs : Map<string, string list * AST.SemanticType> =
        moduleRegistry
        |> Map.toList
        |> List.choose (fun (qualifiedName, moduleFunc) ->
            if not (List.isEmpty moduleFunc.TypeParams) then
                Some (qualifiedName, (moduleFunc.TypeParams, moduleFunc.ReturnType))
            else
                None)
        |> Map.ofList

    let funcParams =
        let baseFuncParamTypes = baseFuncParams |> Map.map (fun _ parameters -> parameters |> List.map snd)
        Map.fold (fun acc k v -> Map.add k v acc) baseFuncParamTypes (Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams)
    let funcReturnTypes =
        Map.fold (fun acc k v -> Map.add k v acc) baseFuncReturnTypes (Map.fold (fun acc k v -> Map.add k v acc) userFuncReturnTypes moduleFuncReturnTypes)
    let genericFuncDefs = Map.fold (fun acc k v -> Map.add k v acc) userGenericFuncDefs moduleGenericFuncDefs
    let locallyComparedFunctionParams =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef funcDef when containsIndirectApply funcDef.Body ->
                funcDef.Params
                |> paramsToList
                |> List.tryPick (fun (_, typ) ->
                    match typ with
                    | AST.TFunction (paramTypes, _) -> Some paramTypes
                    | _ -> None)
            | _ -> None)
        |> Set.ofList

    let escapingFunctionParams =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef funcDef -> Some funcDef.ReturnType
            | _ -> None)
        |> List.map (collectEscapingFunctionParams canonicalMergedTypeReg mergedVariantLookup)
        |> List.fold Set.union Set.empty

    let comparableFunctionParams =
        Set.union locallyComparedFunctionParams escapingFunctionParams

    let symbols =
        funcParams
        |> Map.keys
        |> Seq.fold (fun symbols name -> CheckedAST.internFunction name symbols |> snd) symbols
    let byFunctionId values =
        values
        |> Map.toList
        |> List.map (fun (name, value) ->
            match CheckedAST.tryFindFunctionId name symbols with
            | Some id -> id, value
            | None -> Crash.crash $"Lambda lifting function '{name}' is absent from symbols")
        |> Map.ofList

    let funcReturnTypesById =
        [ "Builtin.testRuntimeError"; "Builtin.crash" ]
        |> List.fold (fun returnTypes name ->
            match CheckedAST.tryFindFunctionId name symbols with
            | Some id -> Map.add id AST.TNever returnTypes
            | None -> returnTypes) (byFunctionId funcReturnTypes)

    let initialState = {
        Symbols = symbols
        Counter = 0
        LiftedFunctions = []
        ComparisonFuncs = Map.empty
        ComparableFunctionParams = comparableFunctionParams
        TypeEnv = Map.empty
        FuncParams = byFunctionId funcParams
        FuncReturnTypes = funcReturnTypesById
        GenericFuncDefs = byFunctionId genericFuncDefs
        TypeReg = canonicalMergedTypeReg
        VariantLookup = mergedVariantLookup
        RecursiveSelf = None
    }

    let rec processTopLevels (remaining: CheckedAST.TopLevel list) (state: LiftState) (acc: CheckedAST.TopLevel list) : Result<CheckedAST.TopLevel list * LiftState, string> =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | tl :: rest ->
            match tl with
            | CheckedAST.FunctionDef f ->
                liftLambdasInFunc f state
                |> Result.bind (fun (f', state') ->
                    processTopLevels rest state' (CheckedAST.FunctionDef f' :: acc))
            | CheckedAST.Expression e ->
                liftLambdasInExpr e state
                |> Result.bind (fun (e', state') ->
                    processTopLevels rest state' (CheckedAST.Expression e' :: acc))
            | CheckedAST.ValueDef valueDef ->
                liftLambdasInExpr (CheckedAST.valueDefBody valueDef) state
                |> Result.bind (fun (body, state') ->
                    let valueDef' = { valueDef with Body = body }
                    processTopLevels rest state' (CheckedAST.ValueDef valueDef' :: acc))
            | CheckedAST.TypeDef (id, t) ->
                processTopLevels rest state (CheckedAST.TypeDef (id, t) :: acc)

    processTopLevels topLevels initialState []
    |> Result.bind (fun (topLevels', state') ->
        // Second pass: find all functions used as values and generate wrappers
        // Look for Var references to known functions in Call arguments
        let funcNamesUsedAsValues =
            topLevels'
            |> List.collect (function
                | CheckedAST.FunctionDef f -> collectFuncRefsInExpr state'.Symbols f.Body funcParams
                | CheckedAST.Expression e -> collectFuncRefsInExpr state'.Symbols e funcParams
                | _ -> [])
            |> List.distinct

        // Generate wrappers for functions used as values
        let stateWithFuncs = { State = state'; FuncParams = funcParams; GeneratedWrappers = Map.empty }
        let rec generateWrappers (funcNames: string list) (st: LiftStateWithFuncs) (wrapperAcc: CheckedAST.FunctionDef list) =
            match funcNames with
            | [] -> Ok (wrapperAcc, st)
            | name :: rest ->
                generateFuncWrapper name funcParams funcReturnTypes st
                |> Result.bind (fun (wrapperDef, st') ->
                    generateWrappers rest st' (wrapperDef :: wrapperAcc))

        generateWrappers funcNamesUsedAsValues stateWithFuncs []
        |> Result.map (fun (wrappers, finalStateWithFuncs) ->
            // Replace function references with wrapper references in the program
            let topLevels'' = topLevels' |> List.map (replaceFuncRefsWithWrappers finalStateWithFuncs.GeneratedWrappers)
            // Add wrappers and lifted functions to the program
            let liftedFuncDefs = (wrappers @ finalStateWithFuncs.State.LiftedFunctions) |> List.rev |> List.map CheckedAST.FunctionDef
            CheckedAST.Program (finalStateWithFuncs.State.Symbols, liftedFuncDefs @ topLevels'')))

/// Collect function names that are used as values (not in Call position)
and collectFuncRefsInExpr
    (symbols: CheckedAST.Symbols)
    (expr: CheckedAST.Expr)
    (knownFuncs: Map<string, AST.SemanticType list>)
    : string list =
    let rec collect (bound: Set<AST.BindingId>) candidate =
        let collectChildren children = children |> List.collect (collect bound)
        match candidate with
        | CheckedAST.BoundaryRender (_, value) -> collect bound value
        | CheckedAST.FuncRef id ->
            match CheckedAST.functionName id symbols with
            | Some name when Map.containsKey name knownFuncs -> [name]
            | _ -> []
        | CheckedAST.Call (_, args) | CheckedAST.TypeApp (_, _, args) ->
            args |> exprArgsToList |> collectChildren
        | CheckedAST.Let (pattern, value, body) ->
            let bodyBound = Set.union bound (CheckedAST.letPatternBindings pattern |> Set.ofList)
            collect bound value @ collect bodyBound body
        | CheckedAST.RecursiveLet (recursion, value, body) ->
            let recursiveBound = Set.add (CheckedAST.recursiveBindingId recursion) bound
            collect recursiveBound value @ collect recursiveBound body
        | CheckedAST.If (condition, thenBranch, elseBranch) ->
            collectChildren [condition; thenBranch; elseBranch]
        | CheckedAST.Sequence (first, next) | CheckedAST.BinOp (_, first, next) ->
            collectChildren [first; next]
        | CheckedAST.UnaryOp (_, value) | CheckedAST.TupleAccess (value, _) | CheckedAST.RecordAccess (value, _) ->
            collect bound value
        | CheckedAST.TupleLiteral elements | CheckedAST.ListLiteral elements -> collectChildren elements
        | CheckedAST.DictLiteral (_, _, entries) ->
            entries |> List.collect (fun (key, value) -> [key; value]) |> collectChildren
        | CheckedAST.RecordLiteral (_, fields) -> fields |> List.map snd |> collectChildren
        | CheckedAST.RecordUpdate (record, fields) -> collectChildren (record :: (fields |> List.map snd))
        | CheckedAST.Constructor (_, fields) -> fields |> List.collect (collect bound)
        | CheckedAST.Match (scrutinee, cases) ->
            collect bound scrutinee
            @ (cases
               |> List.collect (fun case ->
                   let caseNames =
                       case.Patterns
                       |> AST.NonEmptyList.toList
                       |> List.collect CheckedAST.patternBindings
                       |> Set.ofList
                   let caseBound = Set.union bound caseNames
                   (case.Guard |> Option.map (collect caseBound) |> Option.defaultValue [])
                   @ collect caseBound case.Body))
        | CheckedAST.Lambda (parameters, _, body) ->
            let parameterNames =
                parameters
                |> AST.NonEmptyList.toList
                |> List.collect (fun parameter -> CheckedAST.letPatternBindings parameter.Pattern)
                |> Set.ofList
            collect (Set.union bound parameterNames) body
        | CheckedAST.Apply (func, args)
        | CheckedAST.IndirectApply (func, args) -> collectChildren (func :: exprArgsToList args)
        | CheckedAST.Closure (_, captures) -> collectChildren captures
        | _ -> []
    collect Set.empty expr

/// Replace function references with wrapper references in a TopLevel
and replaceFuncRefsWithWrappers
    (wrapperMap: Map<string, AST.FunctionId * AST.FunctionId * AST.FunctionId>)
    (topLevel: CheckedAST.TopLevel)
    : CheckedAST.TopLevel =
    match topLevel with
    | CheckedAST.FunctionDef f ->
        CheckedAST.FunctionDef { f with Body = replaceInExpr wrapperMap f.Body }
    | CheckedAST.Expression e ->
        CheckedAST.Expression (replaceInExpr wrapperMap e)
    | CheckedAST.ValueDef valueDef ->
        let body = replaceInExpr wrapperMap (CheckedAST.valueDefBody valueDef)
        CheckedAST.ValueDef { valueDef with Body = body }
    | CheckedAST.TypeDef (id, t) -> CheckedAST.TypeDef (id, t)

/// Replace function references with wrapper references in an expression
and replaceInExpr
    (wrapperMap: Map<string, AST.FunctionId * AST.FunctionId * AST.FunctionId>)
    (expr: CheckedAST.Expr)
    : CheckedAST.Expr =
    let rec replace (bound: Set<AST.BindingId>) candidate =
        let replaceArgs args = args |> AST.NonEmptyList.map (replace bound)
        match candidate with
        | CheckedAST.BoundaryRender (renderer, value) -> CheckedAST.BoundaryRender (renderer, replace bound value)
        | CheckedAST.FuncRef id ->
            match wrapperMap |> Map.values |> Seq.tryFind (fun (originalId, _, _) -> originalId = id) with
            | Some (_, wrapperId, comparisonId) ->
                CheckedAST.Closure (wrapperId, [CheckedAST.FuncRef comparisonId])
            | None -> candidate
        | CheckedAST.Closure (funcName, captures) ->
            let wrapper =
                wrapperMap
                |> Map.toSeq
                |> Seq.tryPick (fun (_, (originalId, wrapperId, comparisonId)) ->
                    if originalId = funcName then Some (wrapperId, comparisonId) else None)
            match wrapper with
            | Some (wrapperId, comparisonId) ->
                CheckedAST.Closure (
                    wrapperId,
                    CheckedAST.FuncRef comparisonId :: (captures |> List.map (replace bound))
                )
            | None -> CheckedAST.Closure (funcName, captures |> List.map (replace bound))
        | CheckedAST.Call (name, args) -> CheckedAST.Call (name, replaceArgs args)
        | CheckedAST.TypeApp (name, typeArgs, args) -> CheckedAST.TypeApp (name, typeArgs, replaceArgs args)
        | CheckedAST.Let (pattern, value, body) ->
            let bodyBound = Set.union bound (CheckedAST.letPatternBindings pattern |> Set.ofList)
            CheckedAST.Let (pattern, replace bound value, replace bodyBound body)
        | CheckedAST.RecursiveLet (recursion, value, body) ->
            let recursiveBound = Set.add (CheckedAST.recursiveBindingId recursion) bound
            CheckedAST.RecursiveLet (recursion, replace recursiveBound value, replace recursiveBound body)
        | CheckedAST.If (condition, thenBranch, elseBranch) ->
            CheckedAST.If (replace bound condition, replace bound thenBranch, replace bound elseBranch)
        | CheckedAST.Sequence (first, next) -> CheckedAST.Sequence (replace bound first, replace bound next)
        | CheckedAST.BinOp (op, left, right) -> CheckedAST.BinOp (op, replace bound left, replace bound right)
        | CheckedAST.UnaryOp (op, value) -> CheckedAST.UnaryOp (op, replace bound value)
        | CheckedAST.TupleLiteral elements -> CheckedAST.TupleLiteral (elements |> List.map (replace bound))
        | CheckedAST.TupleAccess (value, index) -> CheckedAST.TupleAccess (replace bound value, index)
        | CheckedAST.DictLiteral (keyType, valueType, entries) ->
            CheckedAST.DictLiteral (
                keyType,
                valueType,
                entries
                |> List.map (fun (key, value) -> (replace bound key, replace bound value))
            )
        | CheckedAST.RecordLiteral (typeName, fields) ->
            CheckedAST.RecordLiteral (typeName, fields |> List.map (fun (name, value) -> (name, replace bound value)))
        | CheckedAST.RecordUpdate (record, fields) ->
            CheckedAST.RecordUpdate (replace bound record, fields |> List.map (fun (name, value) -> (name, replace bound value)))
        | CheckedAST.RecordAccess (value, field) -> CheckedAST.RecordAccess (replace bound value, field)
        | CheckedAST.Constructor (reference, fields) ->
            CheckedAST.Constructor (reference, fields |> List.map (replace bound))
        | CheckedAST.Match (scrutinee, cases) ->
            let cases' =
                cases
                |> List.map (fun case ->
                    let caseNames =
                        case.Patterns
                        |> AST.NonEmptyList.toList
                        |> List.collect CheckedAST.patternBindings
                        |> Set.ofList
                    let caseBound = Set.union bound caseNames
                    { case with
                        Guard = case.Guard |> Option.map (replace caseBound)
                        Body = replace caseBound case.Body })
            CheckedAST.Match (replace bound scrutinee, cases')
        | CheckedAST.ListLiteral elements -> CheckedAST.ListLiteral (elements |> List.map (replace bound))
        | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
            let parameterNames =
                parameters
                |> AST.NonEmptyList.toList
                |> List.collect (fun parameter -> CheckedAST.letPatternBindings parameter.Pattern)
                |> Set.ofList
            CheckedAST.Lambda (parameters, returnAnnotation, replace (Set.union bound parameterNames) body)
        | CheckedAST.Apply (func, args) -> CheckedAST.Apply (replace bound func, replaceArgs args)
        | CheckedAST.IndirectApply (func, args) -> CheckedAST.IndirectApply (replace bound func, replaceArgs args)
        | _ -> candidate
    replace Set.empty expr

/// Monomorphize a program: collect all specializations, generate specialized functions, replace TypeApps
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
