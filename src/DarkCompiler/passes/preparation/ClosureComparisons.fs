// ClosureComparisons.fs - Plan equality for lifted closures and their captures.

module ClosureComparisons

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open ClosureAnalysis

type internal LambdaComparisonPlan = {
    Identity: string option
    CaptureNames: string list
    CaptureTypes: AST.Type list
    CaptureExprs: AST.Expr list
    Body: AST.Expr
    CompareCaptures: bool
}

let internal comparisonNameForIdentity
    (identity: string option)
    (captureTypes: AST.Type list)
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
    (parameters: AST.NonEmptyList<AST.LambdaParameter>)
    (body: AST.Expr)
    (state: LiftState)
    : Result<LambdaComparisonPlan, string> =
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
            | Some names, AST.LPVariable name -> Some (name :: names)
            | _ -> None) (Some [])
        |> Option.map List.rev
    let tryNamedPartial targetName args rebuild =
        let argumentList = exprArgsToList args
        let providedCount =
            List.length argumentList - (parameters |> AST.NonEmptyList.toList |> List.length)
        let synthesizedPartialParameters =
            simpleParameterNames
            |> Option.exists (List.forall (fun name -> name.StartsWith "__partial_"))
        if providedCount <= 0 || not synthesizedPartialParameters then
            None
        else
            let providedArgs = List.take providedCount argumentList
            let remainingArgs = List.skip providedCount argumentList
            let isSynthesizedSuffix =
                simpleParameterNames
                |> Option.exists (fun names ->
                    List.map2
                        (fun parameterName argument -> argument = AST.Var parameterName)
                        names
                        remainingArgs
                    |> List.forall id)
            match isSynthesizedSuffix, Map.tryFind targetName state.FuncParams with
            | true, Some targetParams when List.length targetParams >= providedCount ->
                let captureNames =
                    [0 .. providedCount - 1]
                    |> List.map (fun index -> $"__comparison_applied_{index}")
                let captureTypes = targetParams |> List.take providedCount |> List.map snd
                let replacementArgs =
                    (captureNames |> List.map AST.Var) @ remainingArgs
                    |> exprArgsFromList
                Some {
                    Identity = Some targetName
                    CaptureNames = captureNames
                    CaptureTypes = captureTypes
                    CaptureExprs = providedArgs
                    Body = rebuild replacementArgs
                    CompareCaptures = true
                }
            | _ -> None

    let partialPlan =
        match body with
        | AST.Call (targetName, args) ->
            tryNamedPartial targetName args (fun rebuiltArgs -> AST.Call (targetName, rebuiltArgs))
        | AST.TypeApp (targetName, typeArgs, args) ->
            tryNamedPartial targetName args (fun rebuiltArgs -> AST.TypeApp (targetName, typeArgs, rebuiltArgs))
        | _ -> None

    match partialPlan with
    | Some plan -> Ok plan
    | None ->
        let parameterSet = parameterNames |> Set.ofList
        let captures =
            freeVars body parameterSet
            |> Set.filter (fun name -> not (Option.isSome state.RecursiveSelf && name = "__closure"))
            |> Set.filter (fun name -> Map.containsKey name state.TypeEnv)
            |> Set.toList
        let rec collectTypes remaining acc =
            match remaining with
            | [] -> Ok (List.rev acc)
            | name :: rest ->
                match Map.tryFind name state.TypeEnv with
                | Some typ -> collectTypes rest (typ :: acc)
                | None -> Error $"Missing type for captured variable: {name}"
        collectTypes captures []
        |> Result.map (fun captureTypes -> {
            Identity = None
            CaptureNames = captures
            CaptureTypes = captureTypes
            CaptureExprs = captures |> List.map AST.Var
            Body = body
            CompareCaptures = false
        })

let private comparisonForCapturedValue
    (_variantLookup: VariantLookup)
    (typ: AST.Type)
    (left: AST.Expr)
    (right: AST.Expr)
    : AST.Expr =
    let needsStructuralHelper =
        match typ with
        | AST.TFunction _ | AST.TList _ | AST.TDict _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ -> true
        | _ -> false
    if needsStructuralHelper then
        AST.Call (ComparisonPlanning.eqHelperName typ, exprArgsFromList [left; right])
    elif typ = AST.TString then
        AST.BinOp (AST.Eq, left, right)
    elif typ = AST.TInt then
        AST.Call ("Stdlib.Int.__equals", exprArgsFromList [left; right])
    else
        AST.BinOp (AST.Eq, left, right)

let internal makeClosureComparator
    (comparisonName: string)
    (captureTypes: AST.Type list)
    (compareCaptures: bool)
    (variantLookup: VariantLookup)
    : AST.FunctionDef =
    let comparatorStorageType = AST.TRawPtr
    let runtimeClosureType =
        AST.TTuple (AST.TInt64 :: comparatorStorageType :: captureTypes)
    let leftName = "__comparison_left_closure"
    let rightName = "__comparison_right_closure"
    let comparisons =
        if compareCaptures then
            captureTypes
            |> List.mapi (fun index captureType ->
                comparisonForCapturedValue
                    variantLookup
                    captureType
                    (AST.TupleAccess (AST.Var leftName, index + 2))
                    (AST.TupleAccess (AST.Var rightName, index + 2)))
        else []
    let body =
        match comparisons with
        | [] -> AST.BoolLiteral true
        | first :: rest -> rest |> List.fold (fun acc item -> AST.BinOp (AST.And, acc, item)) first
    {
        Name = comparisonName
        TypeParams = []
        Params =
            paramsFromList
                "makeClosureComparator"
                [
                    (leftName, runtimeClosureType)
                    (rightName, runtimeClosureType)
                ]
        ReturnType = AST.TBool
        Body = body
        Recursion = None
    }

/// Replace references already resolved to a singleton recursive binder with
/// the closure value passed to its lifted code. This creates no closure-to-self
/// capture edge: the operational closure parameter is reused directly.
let rec internal rewriteRecursiveSelfReferences (selfName: string) (expr: AST.Expr) : AST.Expr =
    let recurse = rewriteRecursiveSelfReferences selfName
    let mapArgs = AST.NonEmptyList.map recurse
    let patternShadows pattern = AST.letPatternBindings pattern |> List.contains selfName
    match expr with
    | AST.Var name when name = selfName -> AST.Var "__closure"
    | AST.Call (name, args) when name = selfName -> AST.Apply (AST.Var "__closure", mapArgs args)
    | AST.FuncRef name when name = selfName -> AST.Var "__closure"
    | AST.Let (pattern, value, body) ->
        AST.Let (pattern, recurse value, if patternShadows pattern then body else recurse body)
    | AST.RecursiveLet (recursion, value, body) when AST.recursiveBindingName recursion = selfName ->
        match AST.recursiveBindingAvailability recursion with
        | Some AST.OrdinaryBinding -> AST.RecursiveLet (recursion, recurse value, body)
        | _ -> expr
    | AST.RecursiveLet (recursion, value, body) ->
        AST.RecursiveLet (recursion, recurse value, recurse body)
    | AST.Lambda (parameters, returnAnnotation, body) ->
        let shadows =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> AST.letPatternBindings parameter.Pattern)
            |> List.contains selfName
        AST.Lambda (parameters, returnAnnotation, if shadows then body else recurse body)
    | AST.Match (scrutinee, cases) ->
        let cases' =
            cases
            |> List.map (fun case ->
                let shadows =
                    case.Patterns
                    |> AST.NonEmptyList.toList
                    |> List.collect (fun pattern ->
                        AST.validateBinders (AST.MatchBinderPattern pattern)
                        |> Result.defaultValue [])
                    |> List.contains selfName
                if shadows then case
                else { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body })
        AST.Match (recurse scrutinee, cases')
    | AST.BoundaryRender (renderer, value) -> AST.BoundaryRender (renderer, recurse value)
    | AST.BinOp (op, left, right) -> AST.BinOp (op, recurse left, recurse right)
    | AST.UnaryOp (op, value) -> AST.UnaryOp (op, recurse value)
    | AST.If (condition, thenBranch, elseBranch) -> AST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | AST.Sequence (first, next) -> AST.Sequence (recurse first, recurse next)
    | AST.Call (name, args) -> AST.Call (name, mapArgs args)
    | AST.TypeApp (name, types, args) -> AST.TypeApp (name, types, mapArgs args)
    | AST.TupleLiteral values -> AST.TupleLiteral (List.map recurse values)
    | AST.TupleAccess (tuple, index) -> AST.TupleAccess (recurse tuple, index)
    | AST.DictLiteral (keyType, valueType, entries) -> AST.DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> (recurse key, recurse value)))
    | AST.RecordLiteral (name, fields) -> AST.RecordLiteral (name, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | AST.RecordUpdate (record, fields) -> AST.RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | AST.RecordAccess (record, field) -> AST.RecordAccess (recurse record, field)
    | AST.Constructor (reference, name, payload) -> AST.Constructor (reference, name, Option.map recurse payload)
    | AST.ListLiteral values -> AST.ListLiteral (List.map recurse values)
    | AST.Apply (func, args) -> AST.Apply (recurse func, mapArgs args)
    | AST.IndirectApply (func, args) -> AST.IndirectApply (recurse func, mapArgs args)
    | AST.Closure (name, captures) -> AST.Closure (name, List.map recurse captures)
    | AST.InterpolatedString parts ->
        AST.InterpolatedString (parts |> List.map (function AST.StringText _ as text -> text | AST.StringExpr value -> AST.StringExpr (recurse value)))
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _
    | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _
    | AST.UInt64Literal _ | AST.UInt128Literal _ | AST.BoolLiteral _
    | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _
    | AST.Var _ | AST.FuncRef _ | AST.RuntimeError _ -> expr

/// Once the lifted member has a code identity, recursive closure calls become
/// direct calls with the existing group environment as their first argument.
let rec internal rewriteLiftedSelfCalls (liftedName: string) (expr: AST.Expr) : AST.Expr =
    let recurse = rewriteLiftedSelfCalls liftedName
    let mapArgs = AST.NonEmptyList.map recurse
    match expr with
    | AST.Apply (AST.Var "__closure", args) ->
        AST.Call (liftedName, AST.NonEmptyList.cons (AST.Var "__closure") (mapArgs args))
    | AST.BoundaryRender (renderer, value) -> AST.BoundaryRender (renderer, recurse value)
    | AST.BinOp (op, left, right) -> AST.BinOp (op, recurse left, recurse right)
    | AST.UnaryOp (op, value) -> AST.UnaryOp (op, recurse value)
    | AST.Let (pattern, value, body) -> AST.Let (pattern, recurse value, recurse body)
    | AST.RecursiveLet (recursion, value, body) -> AST.RecursiveLet (recursion, recurse value, recurse body)
    | AST.If (condition, thenBranch, elseBranch) -> AST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | AST.Sequence (first, next) -> AST.Sequence (recurse first, recurse next)
    | AST.Call (name, args) -> AST.Call (name, mapArgs args)
    | AST.TypeApp (name, types, args) -> AST.TypeApp (name, types, mapArgs args)
    | AST.TupleLiteral values -> AST.TupleLiteral (List.map recurse values)
    | AST.TupleAccess (tuple, index) -> AST.TupleAccess (recurse tuple, index)
    | AST.DictLiteral (keyType, valueType, entries) -> AST.DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> (recurse key, recurse value)))
    | AST.RecordLiteral (name, fields) -> AST.RecordLiteral (name, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | AST.RecordUpdate (record, fields) -> AST.RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | AST.RecordAccess (record, field) -> AST.RecordAccess (recurse record, field)
    | AST.Constructor (reference, name, payload) -> AST.Constructor (reference, name, Option.map recurse payload)
    | AST.Match (scrutinee, cases) ->
        AST.Match (recurse scrutinee, cases |> List.map (fun case -> { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body }))
    | AST.ListLiteral values -> AST.ListLiteral (List.map recurse values)
    | AST.Lambda (parameters, returnAnnotation, body) -> AST.Lambda (parameters, returnAnnotation, recurse body)
    | AST.Apply (func, args) -> AST.Apply (recurse func, mapArgs args)
    | AST.IndirectApply (func, args) -> AST.IndirectApply (recurse func, mapArgs args)
    | AST.Closure (name, captures) -> AST.Closure (name, List.map recurse captures)
    | AST.InterpolatedString parts ->
        AST.InterpolatedString (parts |> List.map (function AST.StringText _ as text -> text | AST.StringExpr value -> AST.StringExpr (recurse value)))
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _
    | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _
    | AST.UInt64Literal _ | AST.UInt128Literal _ | AST.BoolLiteral _
    | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _
    | AST.Var _ | AST.FuncRef _ | AST.RuntimeError _ -> expr

/// Lift lambdas in an expression, returning (transformed expr, new state)
