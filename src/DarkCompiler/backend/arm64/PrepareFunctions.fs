// PrepareFunctions.fs - Attach target planning facts and outline expensive release operations.

module ARM64PrepareFunctions

open ARM64CodeGenTypes
open ARM64ReleasePlanSummary

/// Attach backend-specific helper planning to a compilation batch. Release
/// plans shared by sibling functions are traversed once, while each function
/// retains only its own semantic requirements for later tree-shaken unions.
let attachARM64CodegenFactsToFunctionsWithCache
    (summaryCache: ReleasePlanSummaryCache option)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (functions: LIR.Function list)
    : LIR.Function list =
    functions
    |> List.mapFold
        (fun releasePlanSummaries func ->
            let facts =
                match func.CodegenFacts with
                | Some facts -> facts
                | None ->
                    Crash.crash $"ARM64 metadata planning requires LIR facts for function '{func.Name}'"
            let plannedSlotInitFacts =
                planRawSlotInitRetainTargets
                    recordRegistry
                    sumShapeRegistry
                    facts
            let requirementsWithMemo =
                planFunctionArm64RcRequirements
                    summaryCache
                    releasePlanSummaries
                    func.Name
                    plannedSlotInitFacts
            let functionRequirements = {
                requirementsWithMemo with
                    ReleasePlanSummaries = Map.empty
            }
            let plannedFacts = {
                plannedSlotInitFacts with
                    Arm64RcHelperRequirements = Some functionRequirements
            }
            ({ func with CodegenFacts = Some plannedFacts },
             requirementsWithMemo.ReleasePlanSummaries))
        Map.empty
    |> fst

let attachARM64CodegenFactsToFunctions
    (functions: LIR.Function list)
    : LIR.Function list =
    functions
    |> attachARM64CodegenFactsToFunctionsWithCache None Map.empty Map.empty
    |> List.map (fun func ->
        let facts =
            func.CodegenFacts
            |> Option.map (fun facts ->
                { facts with Arm64RawSlotInitRetainTargets = None })
        { func with CodegenFacts = facts })

let private outlineExpensiveGenericReleasesInFunction
    (func: LIR.Function)
    : LIR.Function =
    let helperLabelsByMemoKey =
        match func.CodegenFacts |> Option.bind (fun facts -> facts.Arm64RcHelperRequirements) with
        | None ->
            Crash.crash $"ARM64 generic release outlining requires helper facts for '{func.Name}'"
        | Some requirements ->
            requirements.PlannedGenericDecHelpers
            |> Map.toList
            |> List.collect (fun (label, spec) ->
                spec.ReleasePlanMemoKeys
                |> Set.toList
                |> List.map (fun memoKey -> memoKey, label))
            |> Map.ofList
    if Map.isEmpty helperLabelsByMemoKey then
        func
    else
        let outlineInstr instr =
            match instr with
            | LIR.RefCountDec (addr, _, LIR.GenericHeap, metadata) ->
                match Map.tryFind (LIR.rcReleasePlanMemoKey metadata) helperLabelsByMemoKey with
                | Some helperLabel ->
                    [
                        LIR.SaveRegs ([], [])
                        LIR.ArgMoves [(LIR.X0, LIR.Reg addr)]
                        // The physical destination declares that this effect has no
                        // virtual result while retaining normal call liveness.
                        LIR.Call (LIR.Physical LIR.X0, AST.functionIdForName helperLabel, [LIR.Reg addr])
                        LIR.RestoreRegs ([], [])
                    ]
                | _ ->
                    [instr]
            | _ ->
                [instr]
        let blocks =
            func.CFG.Blocks
            |> Map.map (fun _ block ->
                { block with Instrs = List.collect outlineInstr block.Instrs })
        { func with CFG = { func.CFG with Blocks = blocks } }

/// Plan ARM64 helpers from finalized symbolic LIR, then expose expensive
/// generic releases as ordinary calls before register allocation. Attached
/// facts still describe the original release effects and survive allocation.
let prepareARM64FunctionsForAllocationWithCache
    (summaryCache: ReleasePlanSummaryCache option)
    (phaseRecorder: (string -> float -> unit) option)
    (recordRegistry: LIR.RecordRegistry)
    (sumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (functions: LIR.Function list)
    : LIR.Function list =
    let recordPhase name (timer: System.Diagnostics.Stopwatch) =
        match phaseRecorder with
        | Some record ->
            timer.Stop()
            record name timer.Elapsed.TotalMilliseconds
        | None -> ()
    let factsTimer = System.Diagnostics.Stopwatch.StartNew()
    let functionsWithFacts =
        functions
        |> attachARM64CodegenFactsToFunctionsWithCache
            summaryCache
            recordRegistry
            sumShapeRegistry
    recordPhase "ARM64 Function Facts Planning" factsTimer
    let outliningTimer = System.Diagnostics.Stopwatch.StartNew()
    let outlinedFunctions =
        functionsWithFacts |> List.map outlineExpensiveGenericReleasesInFunction
    recordPhase "ARM64 Generic Release Outlining" outliningTimer
    outlinedFunctions

let prepareARM64FunctionsForAllocation
    (functions: LIR.Function list)
    : LIR.Function list =
    let functionsWithFacts = attachARM64CodegenFactsToFunctions functions
    functionsWithFacts |> List.map outlineExpensiveGenericReleasesInFunction

/// Explicit preparation entry point for tools that construct LIR directly.
/// Production performs the same preparation before register allocation.
let prepareARM64Program
    (LIR.Program (functions, variants, records))
    : LIR.Program =
    let sumShapeRegistry = rcSumShapeRegistryFromVariantRegistry variants
    let functionsWithFacts =
        functions
        |> List.map (fun func ->
            match func.CodegenFacts with
            | Some _ -> func
            | None -> LIR.attachFunctionCodegenFacts func)
        |> prepareARM64FunctionsForAllocationWithCache
            None
            None
            records
            sumShapeRegistry
    LIR.Program (functionsWithFacts, variants, records)
