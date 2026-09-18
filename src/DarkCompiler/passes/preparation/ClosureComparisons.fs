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
    CaptureExprs: CheckedAST.Expr list
    Body: CheckedAST.Expr
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
    (parameters: AST.NonEmptyList<CheckedAST.LambdaParameter>)
    (body: CheckedAST.Expr)
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
            | Some names, CheckedAST.LPVariable name -> Some (name :: names)
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
                        (fun parameterName argument -> argument = CheckedAST.Var parameterName)
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
                    (captureNames |> List.map CheckedAST.Var) @ remainingArgs
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
        | CheckedAST.Call (targetName, args) ->
            tryNamedPartial targetName args (fun rebuiltArgs -> CheckedAST.Call (targetName, rebuiltArgs))
        | CheckedAST.TypeApp (targetName, typeArgs, args) ->
            tryNamedPartial targetName args (fun rebuiltArgs -> CheckedAST.TypeApp (targetName, typeArgs, rebuiltArgs))
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
            CaptureExprs = captures |> List.map CheckedAST.Var
            Body = body
            CompareCaptures = false
        })

let private comparisonForCapturedValue
    (_variantLookup: VariantLookup)
    (typ: AST.Type)
    (left: CheckedAST.Expr)
    (right: CheckedAST.Expr)
    : CheckedAST.Expr =
    let needsStructuralHelper =
        match typ with
        | AST.TFunction _ | AST.TList _ | AST.TDict _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ -> true
        | _ -> false
    if needsStructuralHelper then
        CheckedAST.Call (ComparisonPlanning.eqHelperName typ, exprArgsFromList [left; right])
    elif typ = AST.TString then
        CheckedAST.BinOp (AST.Eq, left, right)
    elif typ = AST.TInt then
        CheckedAST.Call ("Stdlib.Int.__equals", exprArgsFromList [left; right])
    else
        CheckedAST.BinOp (AST.Eq, left, right)

let internal makeClosureComparator
    (comparisonName: string)
    (captureTypes: AST.Type list)
    (compareCaptures: bool)
    (variantLookup: VariantLookup)
    : CheckedAST.FunctionDef =
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
                    (CheckedAST.TupleAccess (CheckedAST.Var leftName, index + 2))
                    (CheckedAST.TupleAccess (CheckedAST.Var rightName, index + 2)))
        else []
    let body =
        match comparisons with
        | [] -> CheckedAST.BoolLiteral true
        | first :: rest -> rest |> List.fold (fun acc item -> CheckedAST.BinOp (AST.And, acc, item)) first
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
let rec internal rewriteRecursiveSelfReferences (selfName: string) (expr: CheckedAST.Expr) : CheckedAST.Expr =
    let recurse = rewriteRecursiveSelfReferences selfName
    let mapArgs = AST.NonEmptyList.map recurse
    let patternShadows pattern = CheckedAST.letPatternBindings pattern |> List.contains selfName
    match expr with
    | CheckedAST.Var name when name = selfName -> CheckedAST.Var "__closure"
    | CheckedAST.Call (name, args) when name = selfName -> CheckedAST.Apply (CheckedAST.Var "__closure", mapArgs args)
    | CheckedAST.FuncRef name when name = selfName -> CheckedAST.Var "__closure"
    | CheckedAST.Let (pattern, value, body) ->
        CheckedAST.Let (pattern, recurse value, if patternShadows pattern then body else recurse body)
    | CheckedAST.RecursiveLet (recursion, value, body) when CheckedAST.recursiveBindingName recursion = selfName ->
        match CheckedAST.recursiveBindingAvailability recursion with
        | AST.OrdinaryBinding -> CheckedAST.RecursiveLet (recursion, recurse value, body)
        | _ -> expr
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        CheckedAST.RecursiveLet (recursion, recurse value, recurse body)
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        let shadows =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect (fun parameter -> CheckedAST.letPatternBindings parameter.Pattern)
            |> List.contains selfName
        CheckedAST.Lambda (parameters, returnAnnotation, if shadows then body else recurse body)
    | CheckedAST.Match (scrutinee, cases) ->
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
        CheckedAST.Match (recurse scrutinee, cases')
    | CheckedAST.BoundaryRender (renderer, value) -> CheckedAST.BoundaryRender (renderer, recurse value)
    | CheckedAST.BinOp (op, left, right) -> CheckedAST.BinOp (op, recurse left, recurse right)
    | CheckedAST.UnaryOp (op, value) -> CheckedAST.UnaryOp (op, recurse value)
    | CheckedAST.If (condition, thenBranch, elseBranch) -> CheckedAST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | CheckedAST.Sequence (first, next) -> CheckedAST.Sequence (recurse first, recurse next)
    | CheckedAST.Call (name, args) -> CheckedAST.Call (name, mapArgs args)
    | CheckedAST.TypeApp (name, types, args) -> CheckedAST.TypeApp (name, types, mapArgs args)
    | CheckedAST.TupleLiteral values -> CheckedAST.TupleLiteral (List.map recurse values)
    | CheckedAST.TupleAccess (tuple, index) -> CheckedAST.TupleAccess (recurse tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            keyType,
            valueType,
            entries |> List.map (fun (key, value) -> (recurse key, recurse value))
        )
    | CheckedAST.RecordLiteral (name, fields) -> CheckedAST.RecordLiteral (name, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | CheckedAST.RecordUpdate (record, fields) -> CheckedAST.RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | CheckedAST.RecordAccess (record, field) -> CheckedAST.RecordAccess (recurse record, field)
    | CheckedAST.Constructor (reference, name, fields) -> CheckedAST.Constructor (reference, name, List.map recurse fields)
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
    | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Var _ | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> expr

/// Once the lifted member has a code identity, recursive closure calls become
/// direct calls with the existing group environment as their first argument.
let rec internal rewriteLiftedSelfCalls (liftedName: string) (expr: CheckedAST.Expr) : CheckedAST.Expr =
    let recurse = rewriteLiftedSelfCalls liftedName
    let mapArgs = AST.NonEmptyList.map recurse
    match expr with
    | CheckedAST.Apply (CheckedAST.Var "__closure", args) ->
        CheckedAST.Call (liftedName, AST.NonEmptyList.cons (CheckedAST.Var "__closure") (mapArgs args))
    | CheckedAST.BoundaryRender (renderer, value) -> CheckedAST.BoundaryRender (renderer, recurse value)
    | CheckedAST.BinOp (op, left, right) -> CheckedAST.BinOp (op, recurse left, recurse right)
    | CheckedAST.UnaryOp (op, value) -> CheckedAST.UnaryOp (op, recurse value)
    | CheckedAST.Let (pattern, value, body) -> CheckedAST.Let (pattern, recurse value, recurse body)
    | CheckedAST.RecursiveLet (recursion, value, body) -> CheckedAST.RecursiveLet (recursion, recurse value, recurse body)
    | CheckedAST.If (condition, thenBranch, elseBranch) -> CheckedAST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | CheckedAST.Sequence (first, next) -> CheckedAST.Sequence (recurse first, recurse next)
    | CheckedAST.Call (name, args) -> CheckedAST.Call (name, mapArgs args)
    | CheckedAST.TypeApp (name, types, args) -> CheckedAST.TypeApp (name, types, mapArgs args)
    | CheckedAST.TupleLiteral values -> CheckedAST.TupleLiteral (List.map recurse values)
    | CheckedAST.TupleAccess (tuple, index) -> CheckedAST.TupleAccess (recurse tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            keyType,
            valueType,
            entries |> List.map (fun (key, value) -> (recurse key, recurse value))
        )
    | CheckedAST.RecordLiteral (name, fields) -> CheckedAST.RecordLiteral (name, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | CheckedAST.RecordUpdate (record, fields) -> CheckedAST.RecordUpdate (recurse record, fields |> List.map (fun (field, value) -> (field, recurse value)))
    | CheckedAST.RecordAccess (record, field) -> CheckedAST.RecordAccess (recurse record, field)
    | CheckedAST.Constructor (reference, name, fields) -> CheckedAST.Constructor (reference, name, List.map recurse fields)
    | CheckedAST.Match (scrutinee, cases) ->
        CheckedAST.Match (recurse scrutinee, cases |> List.map (fun case -> { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body }))
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
    | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _
    | CheckedAST.Var _ | CheckedAST.FuncRef _ | CheckedAST.RuntimeError _ -> expr

/// Lift lambdas in an expression, returning (transformed expr, new state)
