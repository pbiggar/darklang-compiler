// RefCountInsertion.fs - Orchestrate function RC elaboration and verify complete type and join interfaces.

module RefCountInsertion

open MemoryModel
open MemoryPlanning
open ANF
open ClosureComparisons
open LiftExpressions
open LiftFunctions
open LoweringExpressions
open AST_to_ANF
open RcTypeFacts
open RcReturnAnalysis
open RcShapePlanning
open RcCleanup
open RcInsertExpression

type private FunctionPhaseTimings = {
    ReturnAnalysisMs: float
    ParameterAnalysisMs: float
    BodyInsertionMs: float
    AccumulatorCleanupMs: float
    CleanupPlanningMs: float
    CleanupRewriteMs: float
}

let private emptyFunctionPhaseTimings = {
    ReturnAnalysisMs = 0.0
    ParameterAnalysisMs = 0.0
    BodyInsertionMs = 0.0
    AccumulatorCleanupMs = 0.0
    CleanupPlanningMs = 0.0
    CleanupRewriteMs = 0.0
}

let private addFunctionPhaseTimings
    (left: FunctionPhaseTimings)
    (right: FunctionPhaseTimings)
    : FunctionPhaseTimings =
    {
        ReturnAnalysisMs = left.ReturnAnalysisMs + right.ReturnAnalysisMs
        ParameterAnalysisMs = left.ParameterAnalysisMs + right.ParameterAnalysisMs
        BodyInsertionMs = left.BodyInsertionMs + right.BodyInsertionMs
        AccumulatorCleanupMs = left.AccumulatorCleanupMs + right.AccumulatorCleanupMs
        CleanupPlanningMs = left.CleanupPlanningMs + right.CleanupPlanningMs
        CleanupRewriteMs = left.CleanupRewriteMs + right.CleanupRewriteMs
    }

let private measureFunctionPhase
    (enabled: bool)
    (work: unit -> 'a)
    : 'a * float =
    if enabled then
        let started = System.Diagnostics.Stopwatch.GetTimestamp()
        let result = work ()
        let elapsed = System.Diagnostics.Stopwatch.GetElapsedTime started
        (result, elapsed.TotalMilliseconds)
    else
        (work (), 0.0)

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let private insertRCInFunctionInternal
    (tracePhases: bool)
    (ctx: TypeContext)
    (func: Function)
    (varGen: VarGen)
    (types: Map<TempId, AST.Type>)
    : Function * VarGen * Map<TempId, AST.Type> * FunctionPhaseTimings =
    let typesWithParams =
        func.TypedParams
        |> List.fold (fun m tp -> Map.add tp.Id tp.Type m) types
    let ctxWithParams = withTempTypes ctx typesWithParams

    let (bodyInfo, returnAnalysisMs) =
        measureFunctionPhase tracePhases (fun () -> analyzeReturns Map.empty Map.empty func.Body)
    let (parameterInfos, parameterAnalysisMs) =
        measureFunctionPhase tracePhases (fun () ->
            func.TypedParams
            |> List.mapi (fun index param -> (index, param))
            |> List.map (fun (index, param) ->
                let shape = rcShapeForType ctxWithParams param.Type
                let transfersOwnedAccumulator =
                    functionParamReturnTransfersOwnedAccumulator
                        ctxWithParams
                        func.Name
                        index
                        param.Type
                let internalOwnedAccumulator =
                    isInternalRecordTailAccumulator func index param
                (param, shape, transfersOwnedAccumulator, internalOwnedAccumulator)))
    let internalOwnedParams =
        parameterInfos
        |> List.choose (fun (param, shape, _, isInternalOwned) ->
            if isInternalOwned then Some (param, shape) else None)
    let internalOwnedParamIds =
        internalOwnedParams |> List.map (fun (param, _) -> param.Id) |> Set.ofList
    let paramIncsRev =
        parameterInfos
        |> List.fold (fun acc (param, shape, transfersOwnedAccumulator, _) ->
            if rcShapeNeedsBorrowedRetain shape
               && not transfersOwnedAccumulator
               && not (Set.contains param.Id internalOwnedParamIds) then
                (param.Id, param.Type, shape) :: acc
            else
                acc
        ) []
    let paramIncs = List.rev paramIncsRev
    let ownedParamDecs =
        parameterInfos
        |> List.choose (fun (param, shape, transfersOwnedAccumulator, _) ->
            if transfersOwnedAccumulator
               || Set.contains param.Id internalOwnedParamIds then
                Some (createReturnDec ctxWithParams param.Id param.Type shape None)
            else
                None)

    // Process function body with return analysis
    let ((bodyWithRC, varGen', accTypes), bodyInsertionMs) =
        measureFunctionPhase tracePhases (fun () ->
            insertRCWithAnalysis
                Map.empty
                []
                ctxWithParams
                (Some func.Name)
                bodyInfo
                varGen
                []
                []
                paramIncs
                typesWithParams)
    let retainInternalParam
        ((param, shape): TypedParam * RcShape)
        (body: AExpr, currentVarGen: VarGen, currentTypes: Map<TempId, AST.Type>)
        : AExpr * VarGen * Map<TempId, AST.Type> =
        let (dummyId, nextVarGen) = freshVar currentVarGen
        let retain = retainExprForShape ctxWithParams param.Id param.Type shape
        (Let (dummyId, retain, body), nextVarGen, Map.add dummyId AST.TUnit currentTypes)
    let ((bodyWithInternalParamRetains, varGen''', accTypes''), accumulatorCleanupMs) =
        measureFunctionPhase tracePhases (fun () ->
            let (bodyWithOwnedAccumulatorDecs, varGen'', accTypes') =
                if List.isEmpty ownedParamDecs then
                    (bodyWithRC, varGen', accTypes)
                else
                    insertOwnedAccumulatorDecsBeforeSelfTailCalls
                        ctxWithParams
                        func.Name
                        ownedParamDecs
                        bodyWithRC
                        varGen'
                        accTypes
            List.foldBack
                retainInternalParam
                internalOwnedParams
                (bodyWithOwnedAccumulatorDecs, varGen'', accTypes'))
    let ((needsClosureMapRetains, needsTailDecMove), cleanupPlanningMs) =
        measureFunctionPhase tracePhases (fun () ->
            requiredFunctionCleanups func.Name bodyWithInternalParamRetains)
    let ((body', varGen'''', accTypes'''), cleanupRewriteMs) =
        measureFunctionPhase tracePhases (fun () ->
            let (bodyWithClosureMapSourceRetains, nextVarGen, nextTypes) =
                if needsClosureMapRetains then
                    insertClosureMapSourceRetainsBeforeHelperCalls
                        ctxWithParams
                        func.Name
                        bodyWithInternalParamRetains
                        varGen'''
                        accTypes''
                else
                    (bodyWithInternalParamRetains, varGen''', accTypes'')
            let rewrittenBody =
                if needsTailDecMove then
                    moveDecsBeforeNonSelfTailCalls func.Name bodyWithClosureMapSourceRetains
                else
                    bodyWithClosureMapSourceRetains
            (rewrittenBody, nextVarGen, nextTypes))
    let timings = {
        ReturnAnalysisMs = returnAnalysisMs
        ParameterAnalysisMs = parameterAnalysisMs
        BodyInsertionMs = bodyInsertionMs
        AccumulatorCleanupMs = accumulatorCleanupMs
        CleanupPlanningMs = cleanupPlanningMs
        CleanupRewriteMs = cleanupRewriteMs
    }
    ({ func with Body = body' }, varGen'''', accTypes''', timings)

/// Insert RC operations into a function
/// Returns (transformed function, varGen, accumulated TempTypes)
let insertRCInFunction (ctx: TypeContext) (func: Function) (varGen: VarGen) : Function * VarGen * Map<TempId, AST.Type> =
    let (func', varGen', types', _timings) =
        insertRCInFunctionInternal false ctx func varGen Map.empty
    (func', varGen', types')

// ============================================================================
// TypeMap Completeness Verification
// ============================================================================

let private isTempMissing (typeMap: ANF.TypeMap) (tempId: TempId) : bool =
    not (Map.containsKey tempId typeMap)

let rec collectMissingTempIdsInExpr
    (typeMap: ANF.TypeMap)
    (expr: AExpr)
    (acc: TempId list)
    : TempId list =
    match expr with
    | Jump _ | Return _ -> acc
    | Let (tempId, _, body) ->
        let acc' = if isTempMissing typeMap tempId then tempId :: acc else acc
        collectMissingTempIdsInExpr typeMap body acc'
    | Join (parameter, continuation, entry) ->
        let acc' = if isTempMissing typeMap parameter.Id then parameter.Id :: acc else acc
        collectMissingTempIdsInExpr typeMap continuation acc' |> collectMissingTempIdsInExpr typeMap entry
    | If (_, thenBranch, elseBranch) ->
        let acc' = collectMissingTempIdsInExpr typeMap thenBranch acc
        collectMissingTempIdsInExpr typeMap elseBranch acc'

let collectMissingTempIdsInFunction
    (typeMap: ANF.TypeMap)
    (func: Function)
    (acc: TempId list)
    : TempId list =
    let acc' =
        func.TypedParams
        |> List.fold (fun acc tp -> if isTempMissing typeMap tp.Id then tp.Id :: acc else acc) acc
    collectMissingTempIdsInExpr typeMap func.Body acc'

/// Find the greatest TempId defined by an ANF expression. ANF variables are
/// introduced only by function parameters and Let bindings, so definitions
/// are sufficient to place a fresh-variable generator beyond every use.
let rec private maxDefinedTempIdInExpr (expr: AExpr) : int =
    match expr with
    | Jump _ | Return _ -> -1
    | Let (TempId tempId, _, body) ->
        max tempId (maxDefinedTempIdInExpr body)
    | Join (parameter, continuation, entry) ->
        let (TempId id) = parameter.Id
        max id (max (maxDefinedTempIdInExpr continuation) (maxDefinedTempIdInExpr entry))
    | If (_, thenBranch, elseBranch) ->
        max
            (maxDefinedTempIdInExpr thenBranch)
            (maxDefinedTempIdInExpr elseBranch)

let private maxDefinedTempIdInFunction (func: Function) : int =
    let paramMax =
        func.TypedParams
        |> List.fold (fun current param ->
            let (TempId tempId) = param.Id
            max current tempId) -1
    max paramMax (maxDefinedTempIdInExpr func.Body)

let private freshVarGenAfterProgram (Program (functions, mainExpr)) : VarGen =
    let functionMax =
        functions
        |> List.fold (fun current func ->
            max current (maxDefinedTempIdInFunction func)) -1
    VarGen (max functionMax (maxDefinedTempIdInExpr mainExpr) + 1)

/// Verify that all defined TempIds have types in the TypeMap
/// Returns a list of TempIds that are missing from the TypeMap
let verifyTypeMapCompleteness (program: ANF.Program) (typeMap: ANF.TypeMap) : TempId list =
    let (ANF.Program (functions, mainExpr)) = program
    let missing =
        functions
        |> List.fold (fun acc func -> collectMissingTempIdsInFunction typeMap func acc) []
        |> collectMissingTempIdsInExpr typeMap mainExpr
    List.rev missing

/// Check immediate join interfaces and lexical captures after type recovery.
/// Legacy tree-only functions are outside this verifier's migration boundary.
let verifyJoinInterfaces (ctx: TypeContext) (program: Program) : Result<unit, string> =
    let rec containsJoin = function
        | Join _ | Jump _ -> true
        | Let (_, _, body) -> containsJoin body
        | If (_, yes, no) -> containsJoin yes || containsJoin no
        | Return _ -> false
    let atomUses = function Var id -> Set.singleton id | _ -> Set.empty
    let checkUses visible uses =
        let missing = Set.difference uses visible
        if Set.isEmpty missing then Ok ()
        else Error $"ANF join interface: operands outside lexical scope: {missing}"
    let rec check visible joins canReturn expr =
        match expr with
        | Return atom ->
            if canReturn then checkUses visible (atomUses atom)
            else Error "ANF join interface: entry returns a value instead of transferring control"
        | Jump (target, atom) ->
            checkUses visible (atomUses atom) |> Result.bind (fun () ->
                match Map.tryFind target joins, inferAtomType ctx atom with
                | None, _ -> Error $"ANF join interface: target {target} is outside lexical scope"
                | Some expected, Some actual when expected = actual -> Ok ()
                | Some expected, actual -> Error $"ANF join interface: target {target} expects {expected}, got {actual}")
        | Let (id, operation, body) ->
            checkUses visible (ANFEffects.cexprTempUses operation)
            |> Result.bind (fun () ->
                match operation with
                | RuntimeError _ | RuntimeErrorString _ -> Ok ()
                | _ -> check (Set.add id visible) joins canReturn body)
        | If (condition, yes, no) ->
            checkUses visible (atomUses condition)
            |> Result.bind (fun () -> check visible joins canReturn yes)
            |> Result.bind (fun () -> check visible joins canReturn no)
        | Join (parameter, continuation, entry) ->
            if not (ANFContinuations.isSupportedJoinArgumentType parameter.Type) then
                Error $"ANF join interface: managed or unsupported block argument {parameter.Type}"
            elif Set.contains parameter.Id visible || Map.containsKey parameter.Id joins then
                Error $"ANF join interface: target {parameter.Id} shadows an enclosing identity"
            else
                check (Set.add parameter.Id visible) joins canReturn continuation
                |> Result.bind (fun () -> check visible (Map.add parameter.Id parameter.Type joins) false entry)
    let verify visible expr =
        if containsJoin expr then check visible Map.empty true expr else Ok ()
    let (Program (functions, main)) = program
    functions
    |> List.fold (fun result func ->
        result |> Result.bind (fun () ->
            verify (func.TypedParams |> List.map (fun parameter -> parameter.Id) |> Set.ofList) func.Body
            |> Result.mapError (fun error -> $"{func.Name}: {error}"))) (Ok ())
    |> Result.bind (fun () -> verify Set.empty main)

let private insertRCInProgramInternal
    (phaseRecorder: (string -> float -> unit) option)
    (result: ConversionResult)
    : Result<ANF.Program * ANF.TypeMap, string> =
    let startPhase () =
        phaseRecorder |> Option.map (fun _ -> System.Diagnostics.Stopwatch.StartNew())
    let recordPhase name timer =
        match phaseRecorder, timer with
        | Some record, Some (timer: System.Diagnostics.Stopwatch) ->
            timer.Stop()
            record name timer.Elapsed.TotalMilliseconds
        | _ -> ()

    let contextTimer = startPhase ()
    let ctx = createContext result
    recordPhase "Reference Count Context" contextTimer
    let (ANF.Program (functions, mainExpr)) = result.Program
    // Inlining and generated JSON helpers can produce thousands of existing
    // temporaries. A fixed starting value eventually collides with them, and
    // sibling-branch type state can then suppress a required retain.
    let varGen = freshVarGenAfterProgram result.Program

    // Process all functions, accumulating types
    let rec processFuncs
        (funcs: Function list)
        (vg: VarGen)
        (accFuncs: Function list)
        (accTypes: Map<TempId, AST.Type>)
        (accTimings: FunctionPhaseTimings)
        : Function list * VarGen * Map<TempId, AST.Type> * FunctionPhaseTimings =
        match funcs with
        | [] -> (List.rev accFuncs, vg, accTypes, accTimings)
        | f :: rest ->
            let (f', vg', types, timings) =
                insertRCInFunctionInternal
                    (Option.isSome phaseRecorder)
                    ctx
                    f
                    vg
                    Map.empty
            let accTypes' =
                Map.fold (fun acc tempId typ -> Map.add tempId typ acc) accTypes types
            processFuncs
                rest
                vg'
                (f' :: accFuncs)
                accTypes'
                (addFunctionPhaseTimings accTimings timings)

    let functionsTimer = startPhase ()
    let (functions', varGen1, typesFromFuncs, functionTimings) =
        processFuncs functions varGen [] Map.empty emptyFunctionPhaseTimings
    phaseRecorder
    |> Option.iter (fun record ->
        record "Reference Count Return Analysis" functionTimings.ReturnAnalysisMs
        record "Reference Count Parameter Analysis" functionTimings.ParameterAnalysisMs
        record "Reference Count Body Insertion" functionTimings.BodyInsertionMs
        record "Reference Count Accumulator Cleanup" functionTimings.AccumulatorCleanupMs
        record "Reference Count Cleanup Planning" functionTimings.CleanupPlanningMs
        record "Reference Count Cleanup Rewrite" functionTimings.CleanupRewriteMs)
    recordPhase "Reference Count Functions" functionsTimer

    // Process main expression
    let mainTimer = startPhase ()
    let (mainExpr', _, finalTypeMap) =
        insertRCInternal ctx mainExpr varGen1 typesFromFuncs
    recordPhase "Reference Count Main" mainTimer

    // Verify TypeMap completeness - all defined TempIds should have types
    let verificationTimer = startPhase ()
    let program' = ANF.Program (functions', mainExpr')
    let missingTypes = verifyTypeMapCompleteness program' finalTypeMap
    recordPhase "Reference Count Verification" verificationTimer
    if not (List.isEmpty missingTypes) then
        let missingStr = missingTypes |> List.map (fun (TempId n) -> $"t{n}") |> String.concat ", "
        Crash.crash $"RefCountInsertion: TypeMap incomplete - missing types for: {missingStr}"

    verifyJoinInterfaces (withTempTypes ctx finalTypeMap) program'
    |> Result.map (fun () -> program', finalTypeMap)

/// Insert RC operations into a program
/// Returns (ANF.Program, TypeMap) where TypeMap contains all TempId -> Type mappings
let insertRCInProgram (result: ConversionResult) : Result<ANF.Program * ANF.TypeMap, string> =
    insertRCInProgramInternal None result

/// Insert RC operations while reporting nested phase timings.
let insertRCInProgramWithTrace
    (phaseRecorder: (string -> float -> unit) option)
    (result: ConversionResult)
    : Result<ANF.Program * ANF.TypeMap, string> =
    insertRCInProgramInternal phaseRecorder result
