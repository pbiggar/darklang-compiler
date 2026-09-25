// ClosureComparisons.fs - Plan equality for lifted closures and their captures.

module ClosureComparisons

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open ClosureAnalysis

type internal LambdaComparisonPlan = {
    Identity: AST.FunctionId option
    CaptureNames: AST.BindingId list
    CaptureTypes: AST.SemanticType list
    CaptureExprs: CheckedAST.Expr list
    Body: CheckedAST.Expr
    CompareCaptures: bool
}

let internal comparisonNameForIdentity
    (identity: AST.FunctionId option)
    (captureTypes: AST.SemanticType list)
    (state: LiftState)
    : string * bool * LiftState =
    match identity with
    | None ->
        let (name, state') = freshLiftedName state "__closure_comparison_"
        (name, true, state')
    | Some identity ->
        let key = (identity, captureTypes)
        match Map.tryFind key state.ComparisonFuncs with
        | Some name -> (name, false, state)
        | None ->
            let (name, state') = freshLiftedName state "__closure_comparison_"
            let state'' =
                { state' with ComparisonFuncs = Map.add key name state'.ComparisonFuncs }
            (name, true, state'')

/// Recognize the lambdas synthesized for named partial application. Their
/// already-applied arguments are semantic identity, unlike ordinary lexical
/// captures, and must therefore become explicit closure payload slots.
let internal planLambdaComparison
    (parameters: AST.NonEmptyList<CheckedAST.LambdaParameter>)
    (body: CheckedAST.Expr)
    (state: LiftState)
    : Result<LambdaComparisonPlan * LiftState, string> =
    let parameterBindings =
        parameters
        |> AST.NonEmptyList.toList
        |> List.collect lambdaParameterBindings
    let parameterNames = parameterBindings |> List.map fst
    let simpleParameterNames =
        parameters
        |> AST.NonEmptyList.toList
        |> List.fold (fun names parameter ->
            match names, parameter.Pattern with
            | Some names, CheckedAST.LPVariable name -> Some (name :: names)
            | _ -> None) (Some [])
        |> Option.map List.rev
    let tryNamedPartial targetName args rebuild =
        let argumentList = exprArgsToList args
        let providedCount =
            List.length argumentList - (parameters |> AST.NonEmptyList.toList |> List.length)
        let synthesizedPartialParameters =
            simpleParameterNames
            |> Option.exists (List.forall (fun id ->
                CheckedAST.bindingName id state.Symbols
                |> Option.map (fun name -> name.StartsWith "__partial_")
                |> Option.defaultValue false))
        if providedCount <= 0 || not synthesizedPartialParameters then
            None
        else
            let providedArgs = List.take providedCount argumentList
            let remainingArgs = List.skip providedCount argumentList
            let isSynthesizedSuffix =
                simpleParameterNames
                |> Option.exists (fun names ->
                    List.map2
                        (fun parameterId argument -> argument = CheckedAST.Local parameterId)
                        names
                        remainingArgs
                    |> List.forall id)
            match isSynthesizedSuffix, Map.tryFind targetName state.FuncParams with
            | true, Some targetParams when List.length targetParams >= providedCount ->
                let captureNames, state =
                    [0 .. providedCount - 1]
                    |> List.mapFold (fun current index ->
                        let (id, symbols) =
                            CheckedAST.allocateBinding $"__comparison_applied_{index}" current.Symbols
                        (id, { current with Symbols = symbols })) state
                let captureTypes = targetParams |> List.take providedCount
                let replacementArgs =
                    (captureNames |> List.map CheckedAST.Local) @ remainingArgs
                    |> exprArgsFromList
                Some ({
                    Identity = Some targetName
                    CaptureNames = captureNames
                    CaptureTypes = captureTypes
                    CaptureExprs = providedArgs
                    Body = rebuild replacementArgs
                    CompareCaptures = true
                }, state)
            | _ -> None

    let partialPlan =
        match body with
        | CheckedAST.Call (targetName, args) ->
            tryNamedPartial targetName args (fun rebuiltArgs -> CheckedAST.Call (targetName, rebuiltArgs))
        | CheckedAST.TypeApp (targetName, typeArgs, args) ->
            tryNamedPartial targetName args (fun rebuiltArgs -> CheckedAST.TypeApp (targetName, typeArgs, rebuiltArgs))
        | _ -> None

    match partialPlan with
    | Some plan -> Ok plan
    | None ->
        let parameterSet =
            match state.RecursiveSelf with
            | Some (_, closureId, _, _) -> Set.add closureId (parameterNames |> Set.ofList)
            | None -> parameterNames |> Set.ofList
        let captures =
            freeVars body parameterSet
            |> Set.filter (fun name -> Map.containsKey name state.TypeEnv)
            |> Set.toList
        let rec collectTypes remaining acc =
            match remaining with
            | [] -> Ok (List.rev acc)
            | name :: rest ->
                match Map.tryFind name state.TypeEnv with
                | Some typ -> collectTypes rest (typ :: acc)
                | None -> Error "Missing type for captured variable identity"
        collectTypes captures []
        |> Result.map (fun captureTypes ->
            ({ Identity = None
               CaptureNames = captures
               CaptureTypes = captureTypes
               CaptureExprs = captures |> List.map CheckedAST.Local
               Body = body
               CompareCaptures = false },
             state))

let private comparisonForCapturedValue
    (symbols: CheckedAST.Symbols)
    (_variantLookup: VariantLookup)
    (typ: AST.SemanticType)
    (left: CheckedAST.Expr)
    (right: CheckedAST.Expr)
    : CheckedAST.Expr =
    let needsStructuralHelper =
        match typ with
        | AST.TFunction _ | AST.TList _ | AST.TDict _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ -> true
        | _ -> false
    let resolvedFunctionId name =
        CheckedAST.tryFindFunctionId name symbols
        |> Option.defaultWith (fun () ->
            Crash.crash $"Closure comparison function '{name}' is absent from symbols")
    if needsStructuralHelper then
        CheckedAST.Call (
            resolvedFunctionId (ComparisonPlanning.eqHelperName typ),
            exprArgsFromList [left; right]
        )
    elif typ = AST.TString then
        CheckedAST.BinOp (AST.Eq, left, right)
    elif typ = AST.TInt then
        CheckedAST.Call (
            resolvedFunctionId "Darklang.Stdlib.Int.__equals",
            exprArgsFromList [left; right]
        )
    else
        CheckedAST.BinOp (AST.Eq, left, right)

let internal makeClosureComparator
    (comparisonName: string)
    (captureTypes: AST.SemanticType list)
    (compareCaptures: bool)
    (variantLookup: VariantLookup)
    (symbols: CheckedAST.Symbols)
    : CheckedAST.FunctionDef * CheckedAST.Symbols =
    let comparatorStorageType = AST.TInternalRawPtr
    let runtimeClosureType =
        AST.TTuple (AST.TInt64 :: comparatorStorageType :: captureTypes)
    let (leftId, symbols) = CheckedAST.allocateBinding "__comparison_left_closure" symbols
    let (rightId, symbols) = CheckedAST.allocateBinding "__comparison_right_closure" symbols
    let comparisons =
        if compareCaptures then
            captureTypes
            |> List.mapi (fun index captureType ->
                comparisonForCapturedValue
                    symbols
                    variantLookup
                    captureType
                    (CheckedAST.TupleAccess (CheckedAST.Local leftId, index + 2))
                    (CheckedAST.TupleAccess (CheckedAST.Local rightId, index + 2)))
        else []
    let body =
        match comparisons with
        | [] -> CheckedAST.BoolLiteral true
        | first :: rest -> rest |> List.fold (fun acc item -> CheckedAST.BinOp (AST.And, acc, item)) first
    let (comparisonId, symbols) = CheckedAST.internFunction comparisonName symbols
    ({
        Id = comparisonId
        Name = comparisonName
        TypeParams = []
        Params =
            paramsFromList
                "makeClosureComparator"
                [
                    (leftId, runtimeClosureType)
                    (rightId, runtimeClosureType)
                ]
        ReturnType = AST.TBool
        Body = body
        Recursion = None
     }, symbols)

/// Replace references already resolved to a singleton recursive binder with
/// the closure value passed to its lifted code. This creates no closure-to-self
/// capture edge: the operational closure parameter is reused directly.
let rec internal rewriteRecursiveSelfReferences
    (selfId: AST.BindingId)
    (closureId: AST.BindingId)
    (expr: CheckedAST.Expr)
    : CheckedAST.Expr =
    let recurse = rewriteRecursiveSelfReferences selfId closureId
    let mapArgs = AST.NonEmptyList.map recurse
    match expr with
    | CheckedAST.Local id when id = selfId -> CheckedAST.Local closureId
    | CheckedAST.Apply (CheckedAST.Local id, args) when id = selfId ->
        CheckedAST.Apply (CheckedAST.Local closureId, mapArgs args)
    | CheckedAST.Let (pattern, value, body) ->
        CheckedAST.Let (pattern, recurse value, recurse body)
    | CheckedAST.RecursiveLet (recursion, value, body) when CheckedAST.recursiveBindingId recursion = selfId ->
        match CheckedAST.recursiveBindingAvailability recursion with
        | AST.OrdinaryBinding -> CheckedAST.RecursiveLet (recursion, recurse value, body)
        | _ -> expr
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        CheckedAST.RecursiveLet (recursion, recurse value, recurse body)
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        CheckedAST.Lambda (parameters, returnAnnotation, recurse body)
    | CheckedAST.Match (scrutinee, cases) ->
        let cases' =
            cases
            |> AST.NonEmptyList.map (fun case ->
                { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body })
        CheckedAST.Match (recurse scrutinee, cases')
    | CheckedAST.BoundaryRender (renderer, value) -> CheckedAST.BoundaryRender (renderer, recurse value)
    | CheckedAST.BinOp (op, left, right) -> CheckedAST.BinOp (op, recurse left, recurse right)
    | CheckedAST.UnaryOp (op, value) -> CheckedAST.UnaryOp (op, recurse value)
    | CheckedAST.If (condition, thenBranch, elseBranch) -> CheckedAST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | CheckedAST.Sequence (first, next) -> CheckedAST.Sequence (recurse first, recurse next)
    | CheckedAST.Call (name, args) -> CheckedAST.Call (name, mapArgs args)
    | CheckedAST.TypeApp (name, types, args) -> CheckedAST.TypeApp (name, types, mapArgs args)
    | CheckedAST.TupleLiteral values -> CheckedAST.TupleLiteral (CheckedAST.mapTupleElements recurse values)
    | CheckedAST.TupleAccess (tuple, index) -> CheckedAST.TupleAccess (recurse tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            keyType,
            valueType,
            entries |> List.map (fun (key, value) -> (recurse key, recurse value))
        )
    | CheckedAST.RecordLiteral (name, fields) -> CheckedAST.RecordLiteral (name, CheckedAST.mapRecordFields recurse fields)
    | CheckedAST.RecordUpdate (record, fields) -> CheckedAST.RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | CheckedAST.RecordAccess (record, field) -> CheckedAST.RecordAccess (recurse record, field)
    | CheckedAST.Constructor (reference, fields) -> CheckedAST.Constructor (reference, List.map recurse fields)
    | CheckedAST.ListLiteral values -> CheckedAST.ListLiteral (List.map recurse values)
    | CheckedAST.Apply (func, args) -> CheckedAST.Apply (recurse func, mapArgs args)
    | CheckedAST.IndirectApply (func, args) -> CheckedAST.IndirectApply (recurse func, mapArgs args)
    | CheckedAST.Closure (name, captures) -> CheckedAST.Closure (name, List.map recurse captures)
    | CheckedAST.InterpolatedString parts ->
        CheckedAST.InterpolatedString (parts |> List.map (function CheckedAST.StringText _ as text -> text | CheckedAST.StringExpr value -> CheckedAST.StringExpr (recurse value)))
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _
    | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
    | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BoolLiteral _
    | CheckedAST.StringLiteral _ | CheckedAST.BlobLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Local _ | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> expr

/// Once the lifted member has a code identity, recursive closure calls become
/// direct calls with the existing group environment as their first argument.
let rec internal rewriteLiftedSelfCalls
    (liftedId: AST.FunctionId)
    (closureId: AST.BindingId)
    (expr: CheckedAST.Expr)
    : CheckedAST.Expr =
    let recurse = rewriteLiftedSelfCalls liftedId closureId
    let mapArgs = AST.NonEmptyList.map recurse
    match expr with
    | CheckedAST.Apply (CheckedAST.Local id, args) when id = closureId ->
        CheckedAST.Call (
            liftedId,
            AST.NonEmptyList.cons (CheckedAST.Local closureId) (mapArgs args)
        )
    | CheckedAST.BoundaryRender (renderer, value) -> CheckedAST.BoundaryRender (renderer, recurse value)
    | CheckedAST.BinOp (op, left, right) -> CheckedAST.BinOp (op, recurse left, recurse right)
    | CheckedAST.UnaryOp (op, value) -> CheckedAST.UnaryOp (op, recurse value)
    | CheckedAST.Let (pattern, value, body) -> CheckedAST.Let (pattern, recurse value, recurse body)
    | CheckedAST.RecursiveLet (recursion, value, body) -> CheckedAST.RecursiveLet (recursion, recurse value, recurse body)
    | CheckedAST.If (condition, thenBranch, elseBranch) -> CheckedAST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | CheckedAST.Sequence (first, next) -> CheckedAST.Sequence (recurse first, recurse next)
    | CheckedAST.Call (name, args) -> CheckedAST.Call (name, mapArgs args)
    | CheckedAST.TypeApp (name, types, args) -> CheckedAST.TypeApp (name, types, mapArgs args)
    | CheckedAST.TupleLiteral values -> CheckedAST.TupleLiteral (CheckedAST.mapTupleElements recurse values)
    | CheckedAST.TupleAccess (tuple, index) -> CheckedAST.TupleAccess (recurse tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            keyType,
            valueType,
            entries |> List.map (fun (key, value) -> (recurse key, recurse value))
        )
    | CheckedAST.RecordLiteral (name, fields) -> CheckedAST.RecordLiteral (name, CheckedAST.mapRecordFields recurse fields)
    | CheckedAST.RecordUpdate (record, fields) -> CheckedAST.RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | CheckedAST.RecordAccess (record, field) -> CheckedAST.RecordAccess (recurse record, field)
    | CheckedAST.Constructor (reference, fields) -> CheckedAST.Constructor (reference, List.map recurse fields)
    | CheckedAST.Match (scrutinee, cases) ->
        CheckedAST.Match (recurse scrutinee, cases |> AST.NonEmptyList.map (fun case -> { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body }))
    | CheckedAST.ListLiteral values -> CheckedAST.ListLiteral (List.map recurse values)
    | CheckedAST.Lambda (parameters, returnAnnotation, body) -> CheckedAST.Lambda (parameters, returnAnnotation, recurse body)
    | CheckedAST.Apply (func, args) -> CheckedAST.Apply (recurse func, mapArgs args)
    | CheckedAST.IndirectApply (func, args) -> CheckedAST.IndirectApply (recurse func, mapArgs args)
    | CheckedAST.Closure (name, captures) -> CheckedAST.Closure (name, List.map recurse captures)
    | CheckedAST.InterpolatedString parts ->
        CheckedAST.InterpolatedString (parts |> List.map (function CheckedAST.StringText _ as text -> text | CheckedAST.StringExpr value -> CheckedAST.StringExpr (recurse value)))
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _
    | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
    | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BoolLiteral _
    | CheckedAST.StringLiteral _ | CheckedAST.BlobLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Local _ | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> expr

/// Lift lambdas in an expression, returning (transformed expr, new state)
