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

let liftLambdasInFunc (funcDef: AST.FunctionDef) (state: LiftState) : Result<AST.FunctionDef * LiftState, string> =
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
    FuncParams: Map<string, (string * AST.Type) list>  // function name -> params (for generating wrappers)
    GeneratedWrappers: Map<string, string>  // original func name -> wrapper name
}

/// Generate a wrapper for a named function used as a value
let generateFuncWrapper
    (origFuncName: string)
    (funcParams: Map<string, (string * AST.Type) list>)
    (funcReturnTypes: Map<string, AST.Type>)
    (stateWithFuncs: LiftStateWithFuncs)
    : Result<(AST.FunctionDef * LiftStateWithFuncs), string> =
    match Map.tryFind origFuncName funcParams, Map.tryFind origFuncName funcReturnTypes with
    | Some parameters, Some returnType ->
        // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
        let (wrapperName, stateWithName) = freshLiftedName stateWithFuncs.State "__funcref_wrapper_"
        let comparatorStorageType = AST.TRawPtr
        let closureParam =
            ("__closure", AST.TTuple [AST.TInt64; comparatorStorageType])
        let wrapperBody =
            parameters
            |> List.map (fun (name, _) -> AST.Var name)
            |> exprArgsFromList
            |> fun args -> AST.Call (origFuncName, args)
        let wrapperDef : AST.FunctionDef = {
            Name = wrapperName
            TypeParams = []
            Params = paramsFromList "generateFuncWrapper" (closureParam :: parameters)
            ReturnType = returnType
            Body = wrapperBody
            Recursion = None
        }
        let comparisonDef =
            makeClosureComparator
                $"{wrapperName}__comparison"
                []
                false
                stateWithFuncs.State.VariantLookup
        let newState = {
            stateWithFuncs with
                State = {
                    stateWithName with
                        LiftedFunctions = comparisonDef :: stateWithName.LiftedFunctions
                }
                GeneratedWrappers = Map.add origFuncName wrapperName stateWithFuncs.GeneratedWrappers
        }
        Ok (wrapperDef, newState)
    | None, _ ->
        Error $"Cannot find parameters for function '{origFuncName}'"
    | _, None ->
        Error $"Cannot find return type for function '{origFuncName}'"

let rec private containsIndirectApply (expr: AST.Expr) : bool =
    let anyExpr exprs = List.exists containsIndirectApply exprs
    match expr with
    | AST.IndirectApply _ -> true
    | AST.BoundaryRender (_, value) -> containsIndirectApply value
    | AST.BinOp (_, left, right) -> containsIndirectApply left || containsIndirectApply right
    | AST.UnaryOp (_, inner) -> containsIndirectApply inner
    | AST.Let (_, value, body) -> containsIndirectApply value || containsIndirectApply body
    | AST.RecursiveLet (_, value, body) -> containsIndirectApply value || containsIndirectApply body
    | AST.If (condition, thenBranch, elseBranch) ->
        containsIndirectApply condition
        || containsIndirectApply thenBranch
        || containsIndirectApply elseBranch
    | AST.Sequence (first, next) -> containsIndirectApply first || containsIndirectApply next
    | AST.Call (_, args)
    | AST.TypeApp (_, _, args) -> args |> exprArgsToList |> anyExpr
    | AST.TupleLiteral elements
    | AST.ListLiteral elements -> anyExpr elements
    | AST.TupleAccess (tuple, _) -> containsIndirectApply tuple
    | AST.DictLiteral (_, _, entries) -> entries |> List.collect (fun (key, value) -> [key; value]) |> anyExpr
    | AST.RecordLiteral (_, entries) -> entries |> List.map snd |> anyExpr
    | AST.RecordUpdate (record, updates) ->
        containsIndirectApply record || (updates |> List.map snd |> anyExpr)
    | AST.RecordAccess (record, _) -> containsIndirectApply record
    | AST.Constructor (_, _, payload) -> Option.exists containsIndirectApply payload
    | AST.Match (scrutinee, cases) ->
        containsIndirectApply scrutinee
        || (cases
            |> List.exists (fun case ->
                Option.exists containsIndirectApply case.Guard
                || containsIndirectApply case.Body))
    | AST.Lambda (_, _, body) -> containsIndirectApply body
    | AST.Apply (func, args) ->
        containsIndirectApply func || (args |> exprArgsToList |> anyExpr)
    | AST.Closure (_, captures) -> anyExpr captures
    | AST.InterpolatedString parts ->
        parts
        |> List.exists (function
            | AST.StringText _ -> false
            | AST.StringExpr partExpr -> containsIndirectApply partExpr)
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _
    | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _
    | AST.Var _ | AST.FuncRef _ | AST.RuntimeError _ -> false

/// A separately compiled caller may compare any closure returned by this
/// compilation unit. Retain comparator metadata for function values nested in
/// exported result types without requiring advance knowledge of their callers.
let private collectEscapingFunctionParams
    (typeReg: TypeRegistry)
    (variantLookup: VariantLookup)
    (typ: AST.Type)
    : Set<AST.Type list> =
    let rec collect (visited: Set<AST.Type>) (current: AST.Type) =
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
            | AST.TTuple elementTypes
            | AST.TEnumFields elementTypes ->
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
                |> List.choose (fun (_, (declaredTypeName, typeParams, _, payloadType)) ->
                    if declaredTypeName <> typeName then
                        None
                    else
                        payloadType
                        |> Option.map (fun payload ->
                            let substitution =
                                if List.length typeParams = List.length typeArgs then
                                    List.zip typeParams typeArgs |> Map.ofList
                                else
                                    Map.empty
                            applySubstToType substitution payload))
                |> List.distinct
                |> collectMany
            | AST.TInt64 | AST.TInt128 | AST.TInt | AST.TInt32 | AST.TInt16 | AST.TInt8
            | AST.TUInt64 | AST.TUInt128 | AST.TUInt32 | AST.TUInt16 | AST.TUInt8
            | AST.TBool | AST.TString | AST.TBlob | AST.TChar | AST.TDateTime
            | AST.TFloat64 | AST.TUnit | AST.TRuntimeError | AST.TRawPtr | AST.TVar _ ->
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
        |> Map.map (fun _ (typeName, typeParams, tag, payloadType) ->
            let isCatalogBoundaryType =
                typeName.StartsWith("Darklang.LanguageTools.ProgramTypes.")
                || typeName.StartsWith("Darklang.LanguageTools.RuntimeTypes.")
            (typeName,
             typeParams,
             tag,
             if isCatalogBoundaryType then
                 payloadType
                 |> Option.map (canonicalizeBareSumTypeRefsWithNames sumTypeNames)
             else
                 payloadType))
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
    (baseFuncParams: Map<string, (string * AST.Type) list>)
    (baseFuncReturnTypes: Map<string, AST.Type>)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let (AST.Program topLevels) = program

    let typeRegBase : TypeRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.RecordDef (name, typeParams, fields)) ->
                Some (name, { TypeParams = typeParams; Fields = firstDeclaredRecordFields fields })
            | _ -> None)
        |> Map.ofList

    let aliasReg : AliasRegistry =
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.TypeAlias (name, typeParams, targetType)) -> Some (name, (typeParams, targetType))
            | _ -> None)
        |> Map.ofList

    let typeReg =
        typeRegBase
        |> resolveAliasesInTypeRegistry aliasReg
        |> fun reg -> expandTypeRegWithAliases reg aliasReg

    let variantLookup : VariantLookup =
        let localTypeDefs =
            topLevels
            |> List.choose (function | AST.TypeDef typeDef -> Some typeDef | _ -> None)
        let collidingCaseNames = AST.collidingConstructorCaseNames localTypeDefs
        topLevels
        |> List.choose (function
            | AST.TypeDef (AST.SumTypeDef (typeName, typeParams, variants)) ->
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
                let info = (typeName, typeParams, tag, variant.Payload)
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
            |> Map.map (fun _ (typeName, typeParams, tag, payloadType) ->
                let isCatalogBoundaryType =
                    typeName.StartsWith("Darklang.LanguageTools.ProgramTypes.")
                    || typeName.StartsWith("Darklang.LanguageTools.RuntimeTypes.")
                (typeName,
                 typeParams,
                 tag,
                 if isCatalogBoundaryType then
                     payloadType
                     |> Option.map (canonicalizeBareSumTypeRefsWithNames mergedSumTypeNames)
                 else
                     payloadType))
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
    let userFuncParams : Map<string, (string * AST.Type) list> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f -> Some (f.Name, paramsToList f.Params)
            | _ -> None)
        |> Map.ofList

    // Collect user function return types
    let userFuncReturnTypes : Map<string, AST.Type> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f -> Some (f.Name, f.ReturnType)
            | _ -> None)
        |> Map.ofList

    // Add module function parameters from Stdlib
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let moduleFuncParams : Map<string, (string * AST.Type) list> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) ->
            // Create parameter names like "arg0", "arg1" for each parameter type
            let paramList = moduleFunc.ParamTypes |> List.mapi (fun i t -> ($"arg{i}", t))
            (qualifiedName, paramList))
        |> Map.ofList

    // Collect module function return types
    let moduleFuncReturnTypes : Map<string, AST.Type> =
        moduleRegistry
        |> Map.toList
        |> List.map (fun (qualifiedName, moduleFunc) -> (qualifiedName, moduleFunc.ReturnType))
        |> Map.ofList

    // Collect user generic function definitions (for TypeApp substitution)
    let userGenericFuncDefs : Map<string, string list * AST.Type> =
        topLevels
        |> List.choose (function
            | AST.FunctionDef f when not (List.isEmpty f.TypeParams) ->
                Some (f.Name, (f.TypeParams, f.ReturnType))
            | _ -> None)
        |> Map.ofList

    // Collect module generic function definitions (for TypeApp substitution)
    let moduleGenericFuncDefs : Map<string, string list * AST.Type> =
        moduleRegistry
        |> Map.toList
        |> List.choose (fun (qualifiedName, moduleFunc) ->
            if not (List.isEmpty moduleFunc.TypeParams) then
                Some (qualifiedName, (moduleFunc.TypeParams, moduleFunc.ReturnType))
            else
                None)
        |> Map.ofList

    let funcParams =
        Map.fold (fun acc k v -> Map.add k v acc) baseFuncParams (Map.fold (fun acc k v -> Map.add k v acc) userFuncParams moduleFuncParams)
    let funcReturnTypes =
        Map.fold (fun acc k v -> Map.add k v acc) baseFuncReturnTypes (Map.fold (fun acc k v -> Map.add k v acc) userFuncReturnTypes moduleFuncReturnTypes)
    let genericFuncDefs = Map.fold (fun acc k v -> Map.add k v acc) userGenericFuncDefs moduleGenericFuncDefs

    let locallyComparedFunctionParams =
        topLevels
        |> List.choose (function
            | AST.FunctionDef funcDef when containsIndirectApply funcDef.Body ->
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
            | AST.FunctionDef funcDef -> Some funcDef.ReturnType
            | _ -> None)
        |> List.map (collectEscapingFunctionParams canonicalMergedTypeReg mergedVariantLookup)
        |> List.fold Set.union Set.empty

    let comparableFunctionParams =
        Set.union locallyComparedFunctionParams escapingFunctionParams

    let initialState = {
        Counter = 0
        LiftedFunctions = []
        ComparisonFuncs = Map.empty
        ComparableFunctionParams = comparableFunctionParams
        TypeEnv = Map.empty
        FuncParams = funcParams
        FuncReturnTypes = funcReturnTypes
        GenericFuncDefs = genericFuncDefs
        TypeReg = canonicalMergedTypeReg
        VariantLookup = mergedVariantLookup
        RecursiveSelf = None
    }

    let rec processTopLevels (remaining: AST.TopLevel list) (state: LiftState) (acc: AST.TopLevel list) : Result<AST.TopLevel list * LiftState, string> =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | tl :: rest ->
            match tl with
            | AST.FunctionDef f ->
                liftLambdasInFunc f state
                |> Result.bind (fun (f', state') ->
                    processTopLevels rest state' (AST.FunctionDef f' :: acc))
            | AST.Expression e ->
                liftLambdasInExpr e state
                |> Result.bind (fun (e', state') ->
                    processTopLevels rest state' (AST.Expression e' :: acc))
            | AST.ValueDef valueDef ->
                liftLambdasInExpr (AST.valueDefBody valueDef) state
                |> Result.bind (fun (body, state') ->
                    let valueDef' =
                        match valueDef with
                        | AST.UncheckedValueDef (name, _) -> AST.UncheckedValueDef (name, body)
                        | AST.CheckedValueDef (name, typ, _) -> AST.CheckedValueDef (name, typ, body)
                    processTopLevels rest state' (AST.ValueDef valueDef' :: acc))
            | AST.TypeDef t ->
                processTopLevels rest state (AST.TypeDef t :: acc)

    processTopLevels topLevels initialState []
    |> Result.bind (fun (topLevels', state') ->
        // Second pass: find all functions used as values and generate wrappers
        // Look for Var references to known functions in Call arguments
        let funcNamesUsedAsValues =
            topLevels'
            |> List.collect (function
                | AST.FunctionDef f -> collectFuncRefsInExpr f.Body funcParams
                | AST.Expression e -> collectFuncRefsInExpr e funcParams
                | _ -> [])
            |> List.distinct

        // Generate wrappers for functions used as values
        let stateWithFuncs = { State = state'; FuncParams = funcParams; GeneratedWrappers = Map.empty }
        let rec generateWrappers (funcNames: string list) (st: LiftStateWithFuncs) (wrapperAcc: AST.FunctionDef list) =
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
            let liftedFuncDefs = (wrappers @ finalStateWithFuncs.State.LiftedFunctions) |> List.rev |> List.map AST.FunctionDef
            AST.Program (liftedFuncDefs @ topLevels'')))

/// Collect function names that are used as values (not in Call position)
and collectFuncRefsInExpr (expr: AST.Expr) (knownFuncs: Map<string, (string * AST.Type) list>) : string list =
    let rec collect (bound: Set<string>) candidate =
        let collectChildren children = children |> List.collect (collect bound)
        match candidate with
        | AST.BoundaryRender (_, value) -> collect bound value
        | AST.Var name when Map.containsKey name knownFuncs && not (Set.contains name bound) -> [name]
        | AST.Call (_, args) | AST.TypeApp (_, _, args) ->
            args |> exprArgsToList |> collectChildren
        | AST.Let (pattern, value, body) ->
            let bodyBound = Set.union bound (AST.letPatternBindings pattern |> Set.ofList)
            collect bound value @ collect bodyBound body
        | AST.RecursiveLet (recursion, value, body) ->
            let recursiveBound = Set.add (AST.recursiveBindingName recursion) bound
            collect recursiveBound value @ collect recursiveBound body
        | AST.If (condition, thenBranch, elseBranch) ->
            collectChildren [condition; thenBranch; elseBranch]
        | AST.Sequence (first, next) | AST.BinOp (_, first, next) ->
            collectChildren [first; next]
        | AST.UnaryOp (_, value) | AST.TupleAccess (value, _) | AST.RecordAccess (value, _) ->
            collect bound value
        | AST.TupleLiteral elements | AST.ListLiteral elements -> collectChildren elements
        | AST.DictLiteral (_, _, entries) -> entries |> List.collect (fun (key, value) -> [key; value]) |> collectChildren
        | AST.RecordLiteral (_, fields) -> fields |> List.map snd |> collectChildren
        | AST.RecordUpdate (record, fields) -> collectChildren (record :: (fields |> List.map snd))
        | AST.Constructor (_, _, payload) -> payload |> Option.map (collect bound) |> Option.defaultValue []
        | AST.Match (scrutinee, cases) ->
            collect bound scrutinee
            @ (cases
               |> List.collect (fun case ->
                   let caseNames =
                       case.Patterns
                       |> AST.NonEmptyList.toList
                       |> List.collect (fun pattern ->
                           AST.validateBinders (AST.MatchBinderPattern pattern)
                           |> Result.defaultValue [])
                       |> Set.ofList
                   let caseBound = Set.union bound caseNames
                   (case.Guard |> Option.map (collect caseBound) |> Option.defaultValue [])
                   @ collect caseBound case.Body))
        | AST.Lambda (parameters, _, body) ->
            let parameterNames =
                parameters
                |> AST.NonEmptyList.toList
                |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
                |> Set.ofList
            collect (Set.union bound parameterNames) body
        | AST.Apply (func, args)
        | AST.IndirectApply (func, args) -> collectChildren (func :: exprArgsToList args)
        | AST.Closure (_, captures) -> collectChildren captures
        | _ -> []
    collect Set.empty expr

/// Replace function references with wrapper references in a TopLevel
and replaceFuncRefsWithWrappers (wrapperMap: Map<string, string>) (topLevel: AST.TopLevel) : AST.TopLevel =
    match topLevel with
    | AST.FunctionDef f ->
        AST.FunctionDef { f with Body = replaceInExpr wrapperMap f.Body }
    | AST.Expression e ->
        AST.Expression (replaceInExpr wrapperMap e)
    | AST.ValueDef valueDef ->
        let body = replaceInExpr wrapperMap (AST.valueDefBody valueDef)
        match valueDef with
        | AST.UncheckedValueDef (name, _) -> AST.ValueDef (AST.UncheckedValueDef (name, body))
        | AST.CheckedValueDef (name, typ, _) -> AST.ValueDef (AST.CheckedValueDef (name, typ, body))
    | AST.TypeDef t -> AST.TypeDef t

/// Replace function references with wrapper references in an expression
and replaceInExpr (wrapperMap: Map<string, string>) (expr: AST.Expr) : AST.Expr =
    let rec replace (bound: Set<string>) candidate =
        let replaceArgs args = args |> AST.NonEmptyList.map (replace bound)
        match candidate with
        | AST.BoundaryRender (renderer, value) -> AST.BoundaryRender (renderer, replace bound value)
        | AST.Var name when Map.containsKey name wrapperMap && not (Set.contains name bound) ->
            match Map.tryFind name wrapperMap with
            | Some wrapperName ->
                AST.Closure (
                    wrapperName,
                    [AST.FuncRef $"{wrapperName}__comparison"]
                )
            | None -> Crash.crash $"replaceInExpr expected wrapper for function '{name}'"
        | AST.Closure (funcName, captures) ->
            match Map.tryFind funcName wrapperMap with
            | Some wrapperName ->
                AST.Closure (
                    wrapperName,
                    AST.FuncRef $"{wrapperName}__comparison"
                    :: (captures |> List.map (replace bound))
                )
            | None -> AST.Closure (funcName, captures |> List.map (replace bound))
        | AST.Call (name, args) -> AST.Call (name, replaceArgs args)
        | AST.TypeApp (name, typeArgs, args) -> AST.TypeApp (name, typeArgs, replaceArgs args)
        | AST.Let (pattern, value, body) ->
            let bodyBound = Set.union bound (AST.letPatternBindings pattern |> Set.ofList)
            AST.Let (pattern, replace bound value, replace bodyBound body)
        | AST.RecursiveLet (recursion, value, body) ->
            let recursiveBound = Set.add (AST.recursiveBindingName recursion) bound
            AST.RecursiveLet (recursion, replace recursiveBound value, replace recursiveBound body)
        | AST.If (condition, thenBranch, elseBranch) ->
            AST.If (replace bound condition, replace bound thenBranch, replace bound elseBranch)
        | AST.Sequence (first, next) -> AST.Sequence (replace bound first, replace bound next)
        | AST.BinOp (op, left, right) -> AST.BinOp (op, replace bound left, replace bound right)
        | AST.UnaryOp (op, value) -> AST.UnaryOp (op, replace bound value)
        | AST.TupleLiteral elements -> AST.TupleLiteral (elements |> List.map (replace bound))
        | AST.TupleAccess (value, index) -> AST.TupleAccess (replace bound value, index)
        | AST.DictLiteral (keyType, valueType, entries) ->
            AST.DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> (replace bound key, replace bound value)))
        | AST.RecordLiteral (typeName, fields) ->
            AST.RecordLiteral (typeName, fields |> List.map (fun (name, value) -> (name, replace bound value)))
        | AST.RecordUpdate (record, fields) ->
            AST.RecordUpdate (replace bound record, fields |> List.map (fun (name, value) -> (name, replace bound value)))
        | AST.RecordAccess (value, field) -> AST.RecordAccess (replace bound value, field)
        | AST.Constructor (typeName, variant, payload) ->
            AST.Constructor (typeName, variant, payload |> Option.map (replace bound))
        | AST.Match (scrutinee, cases) ->
            let cases' =
                cases
                |> List.map (fun case ->
                    let caseNames =
                        case.Patterns
                        |> AST.NonEmptyList.toList
                        |> List.collect (fun pattern ->
                            AST.validateBinders (AST.MatchBinderPattern pattern)
                            |> Result.defaultValue [])
                        |> Set.ofList
                    let caseBound = Set.union bound caseNames
                    { case with
                        Guard = case.Guard |> Option.map (replace caseBound)
                        Body = replace caseBound case.Body })
            AST.Match (replace bound scrutinee, cases')
        | AST.ListLiteral elements -> AST.ListLiteral (elements |> List.map (replace bound))
        | AST.Lambda (parameters, returnAnnotation, body) ->
            let parameterNames =
                parameters
                |> AST.NonEmptyList.toList
                |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
                |> Set.ofList
            AST.Lambda (parameters, returnAnnotation, replace (Set.union bound parameterNames) body)
        | AST.Apply (func, args) -> AST.Apply (replace bound func, replaceArgs args)
        | AST.IndirectApply (func, args) -> AST.IndirectApply (replace bound func, replaceArgs args)
        | _ -> candidate
    replace Set.empty expr

/// Monomorphize a program: collect all specializations, generate specialized functions, replace TypeApps
/// Uses iterative approach: keep specializing until no new concrete TypeApps are found
