// LiftExpressions.fs - Convert expression-local lambdas into lifted function definitions.

module LiftExpressions

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open ClosureAnalysis
open ClosureComparisons

let rec liftLambdasInExpr (expr: CheckedAST.Expr) (state: LiftState) : Result<CheckedAST.Expr * LiftState, string> =
    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _ | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
    | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.Local _ | CheckedAST.NamedValue _ | CheckedAST.FuncRef _ | CheckedAST.Closure _ | CheckedAST.RuntimeError _ ->
        Ok (expr, state)
    | CheckedAST.BoundaryRender (renderer, value) ->
        liftLambdasInExpr value state
        |> Result.map (fun (value', state') -> (CheckedAST.BoundaryRender (renderer, value'), state'))
    | CheckedAST.BinOp (op, left, right) ->
        liftLambdasInExpr left state
        |> Result.bind (fun (left', state1) ->
            liftLambdasInExpr right state1
            |> Result.map (fun (right', state2) -> (CheckedAST.BinOp (op, left', right'), state2)))
    | CheckedAST.UnaryOp (op, inner) ->
        liftLambdasInExpr inner state
        |> Result.map (fun (inner', state') -> (CheckedAST.UnaryOp (op, inner'), state'))
    | CheckedAST.Let (pattern, value, body) ->
        liftLambdasInExpr value state
        |> Result.bind (fun (value', state1) ->
            // Try to infer the type of the value for capturing in nested lambdas
            let valueType = simpleInferType value state1.TypeEnv state1.FuncParams state1.FuncReturnTypes state1.GenericFuncDefs state1.TypeReg state1.VariantLookup
            let state1' =
                match valueType with
                | Some typ ->
                    let newEnv =
                        letPatternBindingTypes pattern typ
                        |> List.fold (fun current (name, bindingType) -> Map.add name bindingType current) state1.TypeEnv
                    { state1 with TypeEnv = newEnv }
                | None -> state1
            liftLambdasInExpr body state1'
            |> Result.map (fun (body', state2) ->
                // The child scope must restore the complete incoming environment;
                // removing by text would lose an outer binding after shadowing.
                let state2' = { state2 with TypeEnv = state.TypeEnv }
                (CheckedAST.Let (pattern, value', body'), state2')))
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        let selfId = CheckedAST.recursiveBindingId recursion
        match CheckedAST.recursiveBindingAvailability recursion with
        | AST.OrdinaryBinding ->
            liftLambdasInExpr (CheckedAST.Let (CheckedAST.LPVariable selfId, value, body)) state
        | AST.SelfRecursiveMember ->
            let valueType = recursion.MonomorphicType
            let (closureId, symbols) = CheckedAST.allocateBinding "__closure" state.Symbols
            let rewrittenValue =
                match value with
                | CheckedAST.Lambda (parameters, returnAnnotation, lambdaBody) ->
                    CheckedAST.Lambda (
                        parameters,
                        returnAnnotation,
                        rewriteRecursiveSelfReferences selfId closureId lambdaBody
                    )
                | _ -> Crash.crash "RecursiveLet reached lambda lifting with a non-lambda value"
            let recursiveState =
                { state with
                    Symbols = symbols
                    TypeEnv = Map.add closureId valueType state.TypeEnv
                    RecursiveSelf = Some (selfId, closureId, valueType, recursion) }
            liftLambdasInExpr rewrittenValue recursiveState
            |> Result.bind (fun (value', state1) ->
                let continuationState =
                    { state1 with
                        TypeEnv = Map.add selfId valueType state.TypeEnv
                        RecursiveSelf = state.RecursiveSelf }
                liftLambdasInExpr body continuationState
                |> Result.map (fun (body', state2) ->
                    let restored =
                        { state2 with
                            TypeEnv = state.TypeEnv
                            RecursiveSelf = state.RecursiveSelf }
                    (CheckedAST.Let (CheckedAST.LPVariable selfId, value', body'), restored)))
        | AST.MutualRecursiveMember
        | AST.CompletedGroupMember
        | AST.ImportedGroupMember ->
            Error "Local RecursiveLet has invalid group availability"
    | CheckedAST.If (cond, thenBr, elseBr) ->
        liftLambdasInExpr cond state
        |> Result.bind (fun (cond', state1) ->
            liftLambdasInExpr thenBr state1
            |> Result.bind (fun (thenBr', state2) ->
                liftLambdasInExpr elseBr state2
                |> Result.map (fun (elseBr', state3) -> (CheckedAST.If (cond', thenBr', elseBr'), state3))))
    | CheckedAST.Sequence (first, next) ->
        liftLambdasInExpr first state
        |> Result.bind (fun (first', state1) ->
            liftLambdasInExpr next state1
            |> Result.map (fun (next', state2) -> (CheckedAST.Sequence (first', next'), state2)))
    | CheckedAST.Call (funcName, args) ->
        // Process args, lifting any lambdas
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (CheckedAST.Call (funcName, args'), state'))
    | CheckedAST.TypeApp (funcName, typeArgs, args) ->
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (CheckedAST.TypeApp (funcName, typeArgs, args'), state'))
    | CheckedAST.TupleLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (CheckedAST.TupleLiteral elems', state'))
    | CheckedAST.ListLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (CheckedAST.ListLiteral elems', state'))
    | CheckedAST.TupleAccess (tuple, index) ->
        liftLambdasInExpr tuple state
        |> Result.map (fun (tuple', state') -> (CheckedAST.TupleAccess (tuple', index), state'))
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        liftLambdasInDictEntries entries state
        |> Result.map (fun (entries', state') ->
            (CheckedAST.DictLiteral (keyType, valueType, entries'), state'))
    | CheckedAST.RecordLiteral (typeName, fields) ->
        liftLambdasInFields fields state
        |> Result.map (fun (fields', state') -> (CheckedAST.RecordLiteral (typeName, fields'), state'))
    | CheckedAST.RecordUpdate (record, updates) ->
        liftLambdasInExpr record state
        |> Result.bind (fun (record', state1) ->
            liftLambdasInFields updates state1
            |> Result.map (fun (updates', state2) -> (CheckedAST.RecordUpdate (record', updates'), state2)))
    | CheckedAST.RecordAccess (record, fieldName) ->
        liftLambdasInExpr record state
        |> Result.map (fun (record', state') -> (CheckedAST.RecordAccess (record', fieldName), state'))
    | CheckedAST.Constructor (reference, fields) ->
        liftLambdasInList fields state
        |> Result.map (fun (fields', state') -> (CheckedAST.Constructor (reference, fields'), state'))
    | CheckedAST.Match (scrutinee, cases) ->
        let scrutineeType =
            simpleInferType
                scrutinee
                state.TypeEnv
                state.FuncParams
                state.FuncReturnTypes
                state.GenericFuncDefs
                state.TypeReg
                state.VariantLookup
        liftLambdasInExpr scrutinee state
        |> Result.bind (fun (scrutinee', state1) ->
            liftLambdasInCases cases scrutineeType state1
            |> Result.map (fun (cases', state2) -> (CheckedAST.Match (scrutinee', cases'), state2)))
    | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
        // Lambda in expression position - lift it to a closure
        // Add lambda parameters to type environment before processing body
        let lambdaParamTypes =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect lambdaParameterBindings
            |> Map.ofList
        let stateWithLambdaParams =
            { state with
                TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes
                // Only this lambda is the recursive value. Lambdas nested in
                // its body capture the recursive closure like any other local.
                RecursiveSelf = None }
        // First, lift any lambdas within the body
        liftLambdasInExpr body stateWithLambdaParams
        |> Result.bind (fun (body', state1) ->
            let stateAfterBody = { state1 with RecursiveSelf = state.RecursiveSelf }
            planLambdaComparison parameters body' stateAfterBody
            |> Result.bind (fun (plan, plannedState) ->
                // Create lifted function
                let (funcName, stateWithName) = freshLiftedName plannedState "__closure_"
                let comparisonInfo =
                    if lambdaNeedsComparison parameters state then
                        let (name, addDef, nextState) =
                            comparisonNameForIdentity plan.Identity plan.CaptureTypes stateWithName
                        Some (name, addDef, nextState)
                    else
                        None
                let stateWithComparison =
                    comparisonInfo
                    |> Option.map (fun (_, _, nextState) -> nextState)
                    |> Option.defaultValue stateWithName
                let metadataTypes =
                    comparisonInfo |> Option.map (fun _ -> [AST.TRawPtr]) |> Option.defaultValue []
                let closureTupleTypes =
                    AST.TInt64 :: (metadataTypes @ plan.CaptureTypes)
                let (closureId, symbols) =
                    match state.RecursiveSelf with
                    | Some (_, closureId, _, _) -> (closureId, stateWithComparison.Symbols)
                    | None -> CheckedAST.allocateBinding "__closure" stateWithComparison.Symbols
                let closureParam = (closureId, AST.TTuple closureTupleTypes)
                let (loweredParameters, loweredBody, symbols) =
                    lowerLambdaParameters symbols parameters plan.Body
                let loweredBody =
                    match state.RecursiveSelf with
                    | Some _ -> rewriteLiftedSelfCalls funcName closureId loweredBody
                    | None -> loweredBody
                let captureOffset = if Option.isSome comparisonInfo then 2 else 1

                // Build body that extracts captures from closure tuple
                let bodyWithExtractions =
                    if List.isEmpty plan.CaptureNames then
                        loweredBody
                    else
                        plan.CaptureNames
                        |> List.mapi (fun i capName ->
                            (capName, CheckedAST.TupleAccess (CheckedAST.Local closureId, i + captureOffset)))
                        |> List.foldBack (fun (capName, accessor) acc ->
                            CheckedAST.Let (CheckedAST.LPVariable capName, accessor, acc)) <| loweredBody

                let stateForReturnType = {
                    stateWithLambdaParams with
                        FuncParams = state1.FuncParams
                        FuncReturnTypes = state1.FuncReturnTypes
                        GenericFuncDefs = state1.GenericFuncDefs
                }

                inferLambdaReturnType body stateForReturnType
                |> Result.bind (fun returnType ->
                    let funcDef : CheckedAST.FunctionDef = {
                        Name = funcName
                        TypeParams = []
                        Params = paramsFromList "lifted lambda" (closureParam :: loweredParameters)
                        ReturnType = returnType
                        Body = bodyWithExtractions
                        Recursion =
                            state.RecursiveSelf
                            |> Option.map (fun (_, _, _, typed) -> typed)
                    }
                    let comparisonDef, symbols =
                        comparisonInfo
                        |> Option.map (fun (comparisonName, addDef, _) ->
                            if addDef then
                                let comparisonDef, symbols =
                                    makeClosureComparator
                                        comparisonName
                                        plan.CaptureTypes
                                        plan.CompareCaptures
                                        state1.VariantLookup
                                        symbols
                                (Some comparisonDef, symbols)
                            else
                                (None, symbols))
                        |> Option.defaultValue (None, symbols)
                    let state' = {
                        Symbols = symbols
                        Counter = stateWithComparison.Counter
                        LiftedFunctions =
                            comparisonDef
                            |> Option.map (fun comparisonDef -> comparisonDef :: funcDef :: state1.LiftedFunctions)
                            |> Option.defaultValue (funcDef :: state1.LiftedFunctions)
                        ComparisonFuncs = stateWithComparison.ComparisonFuncs
                        ComparableFunctionParams = state.ComparableFunctionParams
                        TypeEnv = state.TypeEnv  // Restore original TypeEnv (exclude lambda params)
                        FuncParams = state1.FuncParams
                        FuncReturnTypes = state1.FuncReturnTypes
                        GenericFuncDefs = state1.GenericFuncDefs
                        TypeReg = state1.TypeReg
                        VariantLookup = state1.VariantLookup
                        RecursiveSelf = state.RecursiveSelf
                    }
                    let closureCaptures =
                        match comparisonInfo with
                        | Some (comparisonName, _, _) -> CheckedAST.FuncRef comparisonName :: plan.CaptureExprs
                        | None -> plan.CaptureExprs
                    Ok (CheckedAST.Closure (funcName, closureCaptures), state'))))
    | CheckedAST.Apply (func, args) ->
        liftLambdasInExpr func state
        |> Result.bind (fun (func', state1) ->
            liftLambdasInArgs args state1
            |> Result.map (fun (args', state2) -> (CheckedAST.Apply (func', args'), state2)))
    | CheckedAST.IndirectApply (func, args) ->
        liftLambdasInExpr func state
        |> Result.bind (fun (func', state1) ->
            liftLambdasInArgs args state1
            |> Result.map (fun (args', state2) -> (CheckedAST.IndirectApply (func', args'), state2)))
    | CheckedAST.InterpolatedString parts ->
        let rec liftParts
            (ps: CheckedAST.StringPart list)
            (st: LiftState)
            (acc: CheckedAST.StringPart list)
            : Result<CheckedAST.StringPart list * LiftState, string> =
            match ps with
            | [] -> Ok (List.rev acc, st)
            | CheckedAST.StringText s :: rest ->
                liftParts rest st (CheckedAST.StringText s :: acc)
            | CheckedAST.StringExpr e :: rest ->
                liftLambdasInExpr e st
                |> Result.bind (fun (e', st') ->
                    liftParts rest st' (CheckedAST.StringExpr e' :: acc))
        liftParts parts state []
        |> Result.map (fun (parts', state') -> (CheckedAST.InterpolatedString parts', state'))

/// Lift lambdas in function arguments, converting all lambdas to Closures
/// (even non-capturing lambdas become trivial closures for uniform calling convention)
/// Also wraps FuncRef in closures for uniform calling convention
and liftLambdasInArgs (args: AST.NonEmptyList<CheckedAST.Expr>) (state: LiftState) : Result<AST.NonEmptyList<CheckedAST.Expr> * LiftState, string> =
    let rec loop (remaining: CheckedAST.Expr list) (state: LiftState) (acc: CheckedAST.Expr list) =
        match remaining with
        | [] -> Ok (exprArgsFromList (List.rev acc), state)
        | arg :: rest ->
            match arg with
            | CheckedAST.Lambda (parameters, returnAnnotation, body) ->
                // Add lambda parameters to type environment before processing body
                let lambdaParamTypes =
                    parameters
                    |> AST.NonEmptyList.toList
                    |> List.collect lambdaParameterBindings
                    |> Map.ofList
                let stateWithLambdaParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes }
                // First, recursively lift any nested lambdas in the body
                liftLambdasInExpr body stateWithLambdaParams
                |> Result.bind (fun (body', state1) ->
                    planLambdaComparison parameters body' state1
                    |> Result.bind (fun (plan, plannedState) ->
                        // All lambdas become closures (even non-capturing ones) for uniform calling convention
                        // The lifted function takes closure as first param, then original params
                        let (funcName, stateWithName) = freshLiftedName plannedState "__closure_"
                        let comparisonInfo =
                            if lambdaNeedsComparison parameters state then
                                let (name, addDef, nextState) =
                                    comparisonNameForIdentity plan.Identity plan.CaptureTypes stateWithName
                                Some (name, addDef, nextState)
                            else
                                None
                        let stateWithComparison =
                            comparisonInfo
                            |> Option.map (fun (_, _, nextState) -> nextState)
                            |> Option.defaultValue stateWithName
                        let metadataTypes =
                            comparisonInfo |> Option.map (fun _ -> [AST.TRawPtr]) |> Option.defaultValue []
                        let closureTupleTypes =
                            AST.TInt64 :: (metadataTypes @ plan.CaptureTypes)
                        let (closureId, symbols) =
                            CheckedAST.allocateBinding "__closure" stateWithComparison.Symbols
                        let closureParam = (closureId, AST.TTuple closureTupleTypes)
                        let (loweredParameters, loweredBody, symbols) =
                            lowerLambdaParameters symbols parameters plan.Body
                        let captureOffset = if Option.isSome comparisonInfo then 2 else 1

                        // Build body that extracts captures from closure tuple:
                        // let cap1 = __closure.1 in let cap2 = __closure.2 in ... original_body
                        let bodyWithExtractions =
                            if List.isEmpty plan.CaptureNames then
                                loweredBody
                            else
                                plan.CaptureNames
                                |> List.mapi (fun i capName ->
                                    (capName, CheckedAST.TupleAccess (CheckedAST.Local closureId, i + captureOffset)))
                                |> List.foldBack (fun (capName, accessor) acc ->
                                    CheckedAST.Let (CheckedAST.LPVariable capName, accessor, acc)) <| loweredBody

                        let stateForReturnType = {
                            stateWithLambdaParams with
                                FuncParams = state1.FuncParams
                                FuncReturnTypes = state1.FuncReturnTypes
                                GenericFuncDefs = state1.GenericFuncDefs
                        }

                        inferLambdaReturnType body stateForReturnType
                        |> Result.bind (fun returnType ->
                            let funcDef : CheckedAST.FunctionDef = {
                                Name = funcName
                                TypeParams = []
                                Params = paramsFromList "lifted argument lambda" (closureParam :: loweredParameters)
                                ReturnType = returnType
                                Body = bodyWithExtractions
                                Recursion = None
                            }
                            let comparisonDef, symbols =
                                comparisonInfo
                                |> Option.map (fun (comparisonName, addDef, _) ->
                                    if addDef then
                                        let comparisonDef, symbols =
                                            makeClosureComparator
                                                comparisonName
                                                plan.CaptureTypes
                                                plan.CompareCaptures
                                                state1.VariantLookup
                                                symbols
                                        (Some comparisonDef, symbols)
                                    else
                                        (None, symbols))
                                |> Option.defaultValue (None, symbols)
                            let state' = {
                                Symbols = symbols
                                Counter = stateWithComparison.Counter
                                LiftedFunctions =
                                    comparisonDef
                                    |> Option.map (fun comparisonDef -> comparisonDef :: funcDef :: state1.LiftedFunctions)
                                    |> Option.defaultValue (funcDef :: state1.LiftedFunctions)
                                ComparisonFuncs = stateWithComparison.ComparisonFuncs
                                ComparableFunctionParams = state.ComparableFunctionParams
                                TypeEnv = state.TypeEnv  // Restore original TypeEnv (exclude lambda params)
                                FuncParams = state1.FuncParams
                                FuncReturnTypes = state1.FuncReturnTypes
                                GenericFuncDefs = state1.GenericFuncDefs
                                TypeReg = state1.TypeReg
                                VariantLookup = state1.VariantLookup
                                RecursiveSelf = state.RecursiveSelf
                            }
                            let closureCaptures =
                                match comparisonInfo with
                                | Some (comparisonName, _, _) -> CheckedAST.FuncRef comparisonName :: plan.CaptureExprs
                                | None -> plan.CaptureExprs
                            loop rest state' (CheckedAST.Closure (funcName, closureCaptures) :: acc))))

            | CheckedAST.FuncRef origFuncName ->
                // Named function used as value - wrap in a closure for uniform calling convention
                // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
                // Look up the actual function signature to generate correct wrapper
                match Map.tryFind origFuncName state.FuncParams, Map.tryFind origFuncName state.FuncReturnTypes with
                | Some origParams, Some origReturnType ->
                    let (wrapperName, stateWithName) = freshLiftedName state "__funcref_wrapper_"
                    let (comparisonName, addComparisonDef, stateWithComparisonName) =
                        comparisonNameForIdentity (Some origFuncName) [] stateWithName
                    let comparatorStorageType = AST.TRawPtr
                    let (closureId, symbols) =
                        CheckedAST.allocateBinding "__closure" stateWithComparisonName.Symbols
                    let closureParam =
                        (closureId, AST.TTuple [AST.TInt64; comparatorStorageType])
                    // Generate parameter names for wrapper that match original function's parameters
                    let wrapperParams, symbols =
                        origParams
                        |> List.mapi (fun i (_, typ) -> (i, typ))
                        |> List.mapFold (fun symbols (i, typ) ->
                            let (id, symbols) = CheckedAST.allocateBinding $"__arg{i}" symbols
                            ((id, typ), symbols)) symbols
                    let wrapperArgs = wrapperParams |> List.map (fun (id, _) -> CheckedAST.Local id)
                    let wrapperBody = CheckedAST.Call (origFuncName, exprArgsFromList wrapperArgs)
                    let wrapperDef : CheckedAST.FunctionDef = {
                        Name = wrapperName
                        TypeParams = []
                        Params = paramsFromList "liftLambdasInArgs:wrapperDef" (closureParam :: wrapperParams)
                        ReturnType = origReturnType
                        Body = wrapperBody
                        Recursion = None
                    }
                    let comparisonDef, symbols =
                        makeClosureComparator comparisonName [] false state.VariantLookup symbols
                    let state' = {
                        Symbols = symbols
                        Counter = stateWithComparisonName.Counter
                        LiftedFunctions =
                            if addComparisonDef then
                                comparisonDef :: wrapperDef :: state.LiftedFunctions
                            else
                                wrapperDef :: state.LiftedFunctions
                        ComparisonFuncs = stateWithComparisonName.ComparisonFuncs
                        ComparableFunctionParams = state.ComparableFunctionParams
                        TypeEnv = state.TypeEnv
                        FuncParams = state.FuncParams
                        FuncReturnTypes = state.FuncReturnTypes
                        GenericFuncDefs = state.GenericFuncDefs
                        TypeReg = state.TypeReg
                        VariantLookup = state.VariantLookup
                        RecursiveSelf = state.RecursiveSelf
                    }
                    let closure =
                        CheckedAST.Closure (
                            wrapperName,
                            [CheckedAST.FuncRef comparisonName]
                        )
                    loop rest state' (closure :: acc)
                | None, _ ->
                    Error $"FuncRef to unknown function '{origFuncName}': function parameters not found"
                | _, None ->
                    Error $"FuncRef to unknown function '{origFuncName}': return type not found"

            | CheckedAST.Local _
            | CheckedAST.NamedValue _ ->
                // Check if this is a function being passed as value
                // For now, treat as potential function ref - will be handled at ANF level
                liftLambdasInExpr arg state
                |> Result.bind (fun (arg', state') -> loop rest state' (arg' :: acc))

            | other ->
                liftLambdasInExpr other state
                |> Result.bind (fun (other', state') -> loop rest state' (other' :: acc))
    loop (exprArgsToList args) state []

/// Helper to lift lambdas in a list of expressions
and liftLambdasInList (exprs: CheckedAST.Expr list) (state: LiftState) : Result<CheckedAST.Expr list * LiftState, string> =
    let rec loop (remaining: CheckedAST.Expr list) (state: LiftState) (acc: CheckedAST.Expr list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | e :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' (e' :: acc))
    loop exprs state []

/// Helper to lift lambdas in record fields
and liftLambdasInFields (fields: (string * CheckedAST.Expr) list) (state: LiftState) : Result<(string * CheckedAST.Expr) list * LiftState, string> =
    let rec loop (remaining: (string * CheckedAST.Expr) list) (state: LiftState) (acc: (string * CheckedAST.Expr) list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | (name, e) :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' ((name, e') :: acc))
    loop fields state []

and liftLambdasInDictEntries (entries: (CheckedAST.Expr * CheckedAST.Expr) list) (state: LiftState) : Result<(CheckedAST.Expr * CheckedAST.Expr) list * LiftState, string> =
    let rec loop remaining currentState acc =
        match remaining with
        | [] -> Ok (List.rev acc, currentState)
        | (key, value) :: rest ->
            liftLambdasInExpr key currentState
            |> Result.bind (fun (key', keyState) ->
                liftLambdasInExpr value keyState
                |> Result.bind (fun (value', valueState) ->
                    loop rest valueState ((key', value') :: acc)))
    loop entries state []

/// Helper to lift lambdas in match cases
and liftLambdasInCases
    (cases: CheckedAST.MatchCase list)
    (scrutineeType: AST.Type option)
    (state: LiftState)
    : Result<CheckedAST.MatchCase list * LiftState, string> =
    let rec loop (remaining: CheckedAST.MatchCase list) (state: LiftState) (acc: CheckedAST.MatchCase list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | mc :: rest ->
            let caseBindings =
                match scrutineeType with
                | Some typ ->
                    mc.Patterns
                    |> AST.NonEmptyList.toList
                    |> List.map (fun pattern ->
                        matchPatternBindingTypes state.TypeReg state.VariantLookup pattern typ)
                    |> List.fold (fun current bindings ->
                        Map.fold (fun acc name bindingType -> Map.add name bindingType acc) current bindings) Map.empty
                | None -> Map.empty
            let caseState =
                { state with
                    TypeEnv =
                        Map.fold (fun current name typ -> Map.add name typ current) state.TypeEnv caseBindings }
            // Lift lambdas in guard if present
            let guardResult =
                match mc.Guard with
                | None -> Ok (None, caseState)
                | Some g ->
                    liftLambdasInExpr g caseState
                    |> Result.map (fun (g', s) -> (Some g', s))
            guardResult
            |> Result.bind (fun (guard', state1) ->
                liftLambdasInExpr mc.Body state1
                |> Result.bind (fun (body', state2) ->
                    let newCase = { mc with Guard = guard'; Body = body' }
                    loop rest { state2 with TypeEnv = state.TypeEnv } (newCase :: acc)))
    loop cases state []

/// Lift lambdas in a function definition
