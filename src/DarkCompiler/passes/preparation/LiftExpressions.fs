// LiftExpressions.fs - Convert expression-local lambdas into lifted function definitions.

module LiftExpressions

open MemoryModel
open ANF
open LoweringPrimitives
open TypeRegistries
open SpecializationIdentity
open ClosureAnalysis
open ClosureComparisons

let rec liftLambdasInExpr (expr: AST.Expr) (state: LiftState) : Result<AST.Expr * LiftState, string> =
    match expr with
    | AST.UnitLiteral | AST.Int64Literal _ | AST.Int128Literal _ | AST.BigIntLiteral _ | AST.Int8Literal _ | AST.Int16Literal _ | AST.Int32Literal _
    | AST.UInt8Literal _ | AST.UInt16Literal _ | AST.UInt32Literal _ | AST.UInt64Literal _ | AST.UInt128Literal _
    | AST.BoolLiteral _ | AST.StringLiteral _ | AST.CharLiteral _ | AST.FloatLiteral _ | AST.Var _ | AST.FuncRef _ | AST.Closure _ | AST.RuntimeError _ ->
        Ok (expr, state)
    | AST.BoundaryRender (renderer, value) ->
        liftLambdasInExpr value state
        |> Result.map (fun (value', state') -> (AST.BoundaryRender (renderer, value'), state'))
    | AST.BinOp (op, left, right) ->
        liftLambdasInExpr left state
        |> Result.bind (fun (left', state1) ->
            liftLambdasInExpr right state1
            |> Result.map (fun (right', state2) -> (AST.BinOp (op, left', right'), state2)))
    | AST.UnaryOp (op, inner) ->
        liftLambdasInExpr inner state
        |> Result.map (fun (inner', state') -> (AST.UnaryOp (op, inner'), state'))
    | AST.Let (pattern, value, body) ->
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
                (AST.Let (pattern, value', body'), state2')))
    | AST.RecursiveLet (recursion, value, body) ->
        let name = AST.recursiveBindingName recursion
        match AST.recursiveBindingAvailability recursion with
        | Some AST.OrdinaryBinding ->
            liftLambdasInExpr (AST.Let (AST.LPVariable name, value, body)) state
        | Some AST.SelfRecursiveMember ->
            let valueType =
                match recursion with
                | AST.TypedRecursiveBinding typed -> typed.MonomorphicType
                | _ -> Crash.crash "RecursiveLet reached lambda lifting without a typed member"
            let rewrittenValue =
                match value with
                | AST.Lambda (parameters, returnAnnotation, lambdaBody) ->
                    AST.Lambda (parameters, returnAnnotation, rewriteRecursiveSelfReferences name lambdaBody)
                | _ -> Crash.crash "RecursiveLet reached lambda lifting with a non-lambda value"
            let recursiveState =
                match recursion, AST.recursiveBindingId recursion with
                | AST.TypedRecursiveBinding typed, Some bindingId ->
                    { state with
                        TypeEnv = Map.add "__closure" valueType state.TypeEnv
                        RecursiveSelf = Some (bindingId, valueType, typed) }
                | _ -> Crash.crash "Typed recursive binding has no binding identity"
            liftLambdasInExpr rewrittenValue recursiveState
            |> Result.bind (fun (value', state1) ->
                let continuationState =
                    { state1 with
                        TypeEnv = Map.add name valueType state.TypeEnv
                        RecursiveSelf = state.RecursiveSelf }
                liftLambdasInExpr body continuationState
                |> Result.map (fun (body', state2) ->
                    let restored =
                        { state2 with
                            TypeEnv = state.TypeEnv
                            RecursiveSelf = state.RecursiveSelf }
                    (AST.Let (AST.LPVariable name, value', body'), restored)))
        | Some AST.MutualRecursiveMember
        | Some AST.CompletedGroupMember
        | Some AST.ImportedGroupMember ->
            Error "Local RecursiveLet has invalid group availability"
        | None ->
            Error "Local RecursiveLet has not been resolved"
    | AST.If (cond, thenBr, elseBr) ->
        liftLambdasInExpr cond state
        |> Result.bind (fun (cond', state1) ->
            liftLambdasInExpr thenBr state1
            |> Result.bind (fun (thenBr', state2) ->
                liftLambdasInExpr elseBr state2
                |> Result.map (fun (elseBr', state3) -> (AST.If (cond', thenBr', elseBr'), state3))))
    | AST.Sequence (first, next) ->
        liftLambdasInExpr first state
        |> Result.bind (fun (first', state1) ->
            liftLambdasInExpr next state1
            |> Result.map (fun (next', state2) -> (AST.Sequence (first', next'), state2)))
    | AST.Call (funcName, args) ->
        // Process args, lifting any lambdas
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (AST.Call (funcName, args'), state'))
    | AST.TypeApp (funcName, typeArgs, args) ->
        liftLambdasInArgs args state
        |> Result.map (fun (args', state') -> (AST.TypeApp (funcName, typeArgs, args'), state'))
    | AST.TupleLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (AST.TupleLiteral elems', state'))
    | AST.ListLiteral elems ->
        liftLambdasInList elems state
        |> Result.map (fun (elems', state') -> (AST.ListLiteral elems', state'))
    | AST.TupleAccess (tuple, index) ->
        liftLambdasInExpr tuple state
        |> Result.map (fun (tuple', state') -> (AST.TupleAccess (tuple', index), state'))
    | AST.DictLiteral (keyType, valueType, entries) ->
        liftLambdasInDictEntries entries state
        |> Result.map (fun (entries', state') -> (AST.DictLiteral (keyType, valueType, entries'), state'))
    | AST.RecordLiteral (typeName, fields) ->
        liftLambdasInFields fields state
        |> Result.map (fun (fields', state') -> (AST.RecordLiteral (typeName, fields'), state'))
    | AST.RecordUpdate (record, updates) ->
        liftLambdasInExpr record state
        |> Result.bind (fun (record', state1) ->
            liftLambdasInFields updates state1
            |> Result.map (fun (updates', state2) -> (AST.RecordUpdate (record', updates'), state2)))
    | AST.RecordAccess (record, fieldName) ->
        liftLambdasInExpr record state
        |> Result.map (fun (record', state') -> (AST.RecordAccess (record', fieldName), state'))
    | AST.Constructor (typeName, variantName, payload) ->
        match payload with
        | None -> Ok (expr, state)
        | Some p ->
            liftLambdasInExpr p state
            |> Result.map (fun (p', state') -> (AST.Constructor (typeName, variantName, Some p'), state'))
    | AST.Match (scrutinee, cases) ->
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
            |> Result.map (fun (cases', state2) -> (AST.Match (scrutinee', cases'), state2)))
    | AST.Lambda (parameters, returnAnnotation, body) ->
        // Lambda in expression position - lift it to a closure
        // Add lambda parameters to type environment before processing body
        let lambdaParamTypes =
            parameters
            |> AST.NonEmptyList.toList
            |> List.collect lambdaParameterBindings
            |> Map.ofList
        let stateWithLambdaParams = { state with TypeEnv = Map.fold (fun acc k v -> Map.add k v acc) state.TypeEnv lambdaParamTypes }
        // First, lift any lambdas within the body
        liftLambdasInExpr body stateWithLambdaParams
        |> Result.bind (fun (body', state1) ->
            planLambdaComparison parameters body' state
            |> Result.bind (fun plan ->
                // Create lifted function
                let (funcName, stateWithName) = freshLiftedName state1 "__closure_"
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
                let closureParam = ("__closure", AST.TTuple closureTupleTypes)
                let (loweredParameters, loweredBody) = lowerLambdaParameters parameters plan.Body
                let loweredBody =
                    match state.RecursiveSelf with
                    | Some _ -> rewriteLiftedSelfCalls funcName loweredBody
                    | None -> loweredBody
                let captureOffset = if Option.isSome comparisonInfo then 2 else 1

                // Build body that extracts captures from closure tuple
                let bodyWithExtractions =
                    if List.isEmpty plan.CaptureNames then
                        loweredBody
                    else
                        plan.CaptureNames
                        |> List.mapi (fun i capName ->
                            (capName, AST.TupleAccess (AST.Var "__closure", i + captureOffset)))
                        |> List.foldBack (fun (capName, accessor) acc ->
                            AST.Let (AST.LPVariable capName, accessor, acc)) <| loweredBody

                let stateForReturnType = {
                    stateWithLambdaParams with
                        FuncParams = state1.FuncParams
                        FuncReturnTypes = state1.FuncReturnTypes
                        GenericFuncDefs = state1.GenericFuncDefs
                }

                inferLambdaReturnType body stateForReturnType
                |> Result.bind (fun returnType ->
                    let funcDef : AST.FunctionDef = {
                        Name = funcName
                        TypeParams = []
                        Params = paramsFromList "lifted lambda" (closureParam :: loweredParameters)
                        ReturnType = returnType
                        Body = bodyWithExtractions
                        Recursion =
                            state.RecursiveSelf
                            |> Option.map (fun (_, _, typed) -> AST.TypedRecursiveBinding typed)
                    }
                    let comparisonDef =
                        comparisonInfo
                        |> Option.bind (fun (comparisonName, addDef, _) ->
                            if addDef then
                                Some (
                                    makeClosureComparator
                                        comparisonName
                                        plan.CaptureTypes
                                        plan.CompareCaptures
                                        state1.VariantLookup
                                )
                            else
                                None)
                    let state' = {
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
                        | Some (comparisonName, _, _) -> AST.FuncRef comparisonName :: plan.CaptureExprs
                        | None -> plan.CaptureExprs
                    Ok (AST.Closure (funcName, closureCaptures), state'))))
    | AST.Apply (func, args) ->
        liftLambdasInExpr func state
        |> Result.bind (fun (func', state1) ->
            liftLambdasInArgs args state1
            |> Result.map (fun (args', state2) -> (AST.Apply (func', args'), state2)))
    | AST.IndirectApply (func, args) ->
        liftLambdasInExpr func state
        |> Result.bind (fun (func', state1) ->
            liftLambdasInArgs args state1
            |> Result.map (fun (args', state2) -> (AST.IndirectApply (func', args'), state2)))
    | AST.InterpolatedString parts ->
        let rec liftParts (ps: AST.StringPart list) (st: LiftState) (acc: AST.StringPart list) : Result<AST.StringPart list * LiftState, string> =
            match ps with
            | [] -> Ok (List.rev acc, st)
            | AST.StringText s :: rest ->
                liftParts rest st (AST.StringText s :: acc)
            | AST.StringExpr e :: rest ->
                liftLambdasInExpr e st
                |> Result.bind (fun (e', st') ->
                    liftParts rest st' (AST.StringExpr e' :: acc))
        liftParts parts state []
        |> Result.map (fun (parts', state') -> (AST.InterpolatedString parts', state'))

/// Lift lambdas in function arguments, converting all lambdas to Closures
/// (even non-capturing lambdas become trivial closures for uniform calling convention)
/// Also wraps FuncRef in closures for uniform calling convention
and liftLambdasInArgs (args: AST.NonEmptyList<AST.Expr>) (state: LiftState) : Result<AST.NonEmptyList<AST.Expr> * LiftState, string> =
    let rec loop (remaining: AST.Expr list) (state: LiftState) (acc: AST.Expr list) =
        match remaining with
        | [] -> Ok (exprArgsFromList (List.rev acc), state)
        | arg :: rest ->
            match arg with
            | AST.Lambda (parameters, returnAnnotation, body) ->
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
                    planLambdaComparison parameters body' state
                    |> Result.bind (fun plan ->
                        // All lambdas become closures (even non-capturing ones) for uniform calling convention
                        // The lifted function takes closure as first param, then original params
                        let (funcName, stateWithName) = freshLiftedName state1 "__closure_"
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
                        let closureParam = ("__closure", AST.TTuple closureTupleTypes)
                        let (loweredParameters, loweredBody) = lowerLambdaParameters parameters plan.Body
                        let captureOffset = if Option.isSome comparisonInfo then 2 else 1

                        // Build body that extracts captures from closure tuple:
                        // let cap1 = __closure.1 in let cap2 = __closure.2 in ... original_body
                        let bodyWithExtractions =
                            if List.isEmpty plan.CaptureNames then
                                loweredBody
                            else
                                plan.CaptureNames
                                |> List.mapi (fun i capName ->
                                    (capName, AST.TupleAccess (AST.Var "__closure", i + captureOffset)))
                                |> List.foldBack (fun (capName, accessor) acc ->
                                    AST.Let (AST.LPVariable capName, accessor, acc)) <| loweredBody

                        let stateForReturnType = {
                            stateWithLambdaParams with
                                FuncParams = state1.FuncParams
                                FuncReturnTypes = state1.FuncReturnTypes
                                GenericFuncDefs = state1.GenericFuncDefs
                        }

                        inferLambdaReturnType body stateForReturnType
                        |> Result.bind (fun returnType ->
                            let funcDef : AST.FunctionDef = {
                                Name = funcName
                                TypeParams = []
                                Params = paramsFromList "lifted argument lambda" (closureParam :: loweredParameters)
                                ReturnType = returnType
                                Body = bodyWithExtractions
                                Recursion = None
                            }
                            let comparisonDef =
                                comparisonInfo
                                |> Option.bind (fun (comparisonName, addDef, _) ->
                                    if addDef then
                                        Some (
                                            makeClosureComparator
                                                comparisonName
                                                plan.CaptureTypes
                                                plan.CompareCaptures
                                                state1.VariantLookup
                                        )
                                    else
                                        None)
                            let state' = {
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
                                | Some (comparisonName, _, _) -> AST.FuncRef comparisonName :: plan.CaptureExprs
                                | None -> plan.CaptureExprs
                            loop rest state' (AST.Closure (funcName, closureCaptures) :: acc))))

            | AST.FuncRef origFuncName ->
                // Named function used as value - wrap in a closure for uniform calling convention
                // Create wrapper: __funcref_wrapper_N(__closure, ...params) = origFunc(...params)
                // Look up the actual function signature to generate correct wrapper
                match Map.tryFind origFuncName state.FuncParams, Map.tryFind origFuncName state.FuncReturnTypes with
                | Some origParams, Some origReturnType ->
                    let (wrapperName, stateWithName) = freshLiftedName state "__funcref_wrapper_"
                    let (comparisonName, addComparisonDef, stateWithComparisonName) =
                        comparisonNameForIdentity (Some origFuncName) [] stateWithName
                    let comparatorStorageType = AST.TRawPtr
                    let closureParam =
                        ("__closure", AST.TTuple [AST.TInt64; comparatorStorageType])
                    // Generate parameter names for wrapper that match original function's parameters
                    let wrapperParams = origParams |> List.mapi (fun i (_, t) -> ($"__arg{i}", t))
                    let wrapperArgs = wrapperParams |> List.map (fun (name, _) -> AST.Var name)
                    let wrapperBody = AST.Call (origFuncName, exprArgsFromList wrapperArgs)
                    let wrapperDef : AST.FunctionDef = {
                        Name = wrapperName
                        TypeParams = []
                        Params = paramsFromList "liftLambdasInArgs:wrapperDef" (closureParam :: wrapperParams)
                        ReturnType = origReturnType
                        Body = wrapperBody
                        Recursion = None
                    }
                    let comparisonDef =
                        makeClosureComparator comparisonName [] false state.VariantLookup
                    let state' = {
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
                        AST.Closure (
                            wrapperName,
                            [AST.FuncRef comparisonName]
                        )
                    loop rest state' (closure :: acc)
                | None, _ ->
                    Error $"FuncRef to unknown function '{origFuncName}': function parameters not found"
                | _, None ->
                    Error $"FuncRef to unknown function '{origFuncName}': return type not found"

            | AST.Var varName ->
                // Check if this is a function being passed as value
                // For now, treat as potential function ref - will be handled at ANF level
                liftLambdasInExpr arg state
                |> Result.bind (fun (arg', state') -> loop rest state' (arg' :: acc))

            | other ->
                liftLambdasInExpr other state
                |> Result.bind (fun (other', state') -> loop rest state' (other' :: acc))
    loop (exprArgsToList args) state []

/// Helper to lift lambdas in a list of expressions
and liftLambdasInList (exprs: AST.Expr list) (state: LiftState) : Result<AST.Expr list * LiftState, string> =
    let rec loop (remaining: AST.Expr list) (state: LiftState) (acc: AST.Expr list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | e :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' (e' :: acc))
    loop exprs state []

/// Helper to lift lambdas in record fields
and liftLambdasInFields (fields: (string * AST.Expr) list) (state: LiftState) : Result<(string * AST.Expr) list * LiftState, string> =
    let rec loop (remaining: (string * AST.Expr) list) (state: LiftState) (acc: (string * AST.Expr) list) =
        match remaining with
        | [] -> Ok (List.rev acc, state)
        | (name, e) :: rest ->
            liftLambdasInExpr e state
            |> Result.bind (fun (e', state') -> loop rest state' ((name, e') :: acc))
    loop fields state []

and liftLambdasInDictEntries (entries: (AST.Expr * AST.Expr) list) (state: LiftState) : Result<(AST.Expr * AST.Expr) list * LiftState, string> =
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
    (cases: AST.MatchCase list)
    (scrutineeType: AST.Type option)
    (state: LiftState)
    : Result<AST.MatchCase list * LiftState, string> =
    let rec loop (remaining: AST.MatchCase list) (state: LiftState) (acc: AST.MatchCase list) =
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
