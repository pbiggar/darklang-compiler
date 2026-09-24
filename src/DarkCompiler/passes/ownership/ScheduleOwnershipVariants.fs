// ScheduleOwnershipVariants.fs - Drive verified ownership specialization to a bounded fixed point.

module ScheduleOwnershipVariants

open System.Diagnostics
open OwnedIR

type Limits = {
    MaxIterations: int
    MaxGeneratedGroups: int
    MaxRewrittenCalls: int
}

let defaultLimits = {
    MaxIterations = 32
    MaxGeneratedGroups = 256
    MaxRewrittenCalls = 4096
}

type Iteration = {
    Number: int
    AddedCalls: CallSiteIdentity list
    GeneratedGroups: int
}

/// A cache consumer needs the exact source bodies as well as their canonical
/// ownership identity and dependencies. Keeping this structural avoids a
/// process-local or formatting-dependent hash in the compiler pipeline.
type CacheDescriptor<'leaf, 'id when 'id: comparison> = {
    Identity: SelectOwnershipVariants.CandidateIdentity
    Sources: Function<'leaf, 'id> list
    Boundaries: (string * CallSignature) list
    InternalDependencies: Set<AST.FunctionId>
    ExternalTargets: Set<AST.FunctionId>
}

type Plan<'leaf, 'id when 'id: comparison> = private {
    Materialization: MaterializeOwnershipVariants.Plan<'leaf, 'id>
    Iterations: Iteration list
    Cache: CacheDescriptor<'leaf, 'id> list
}

type SchedulingError<'id when 'id: comparison> =
    | InvalidLimits of Limits
    | InvalidFunctionBoundary of AST.FunctionId * VerificationError<'id>
    | InferenceFailed of InferOwnedFunctionGroups.InferenceError<'id>
    | CatalogFailed of SelectOwnershipVariants.SelectionError
    | AnalysisFailed of VerifyOwnedHIR.VerificationError<'id>
    | SelectionFailed of CallSiteIdentity * SelectOwnershipVariants.SelectionError
    | MaterializationFailed of MaterializeOwnershipVariants.MaterializationError<'id>
    | IterationLimitExceeded of int
    | GeneratedGroupLimitExceeded of int
    | RewrittenCallLimitExceeded of int
    | MissingOriginalCall of CallSiteIdentity

type private DemandKey = {
    Target: AST.FunctionId
    Established: CallSignature
    UniqueArguments: Set<int>
}

type private DemandResolution<'id> = {
    Selection: SelectOwnershipVariants.Selection<'id>
}

let materialization plan = plan.Materialization
let functions plan = MaterializeOwnershipVariants.functions plan.Materialization
let hirContracts plan source = MaterializeOwnershipVariants.hirContracts plan.Materialization source
let ownershipSemantics plan source = MaterializeOwnershipVariants.ownershipSemantics plan.Materialization source
let iterations plan = plan.Iterations
let cacheDescriptors plan = plan.Cache

let rec private calls (block: Block<'leaf, 'id>) =
    block.Body.Operations
    |> List.collect (function
        | Evaluate (HIR.Call call) -> [call]
        | Evaluate (HIR.Branch (_, _, yes, no)) -> calls yes @ calls no
        | Evaluate (HIR.Leaf _ | HIR.ScalarBinding _) | Dup _ | Drop _ -> [])

let private originalCalls definitions =
    definitions
    |> List.collect (fun definition ->
        calls definition.Definition.Body
        |> List.map (fun call ->
            { Caller = definition.Definition.Id; Result = call.Result.Id }, call))
    |> Map.ofList

let private candidateIdentity candidate =
    candidate
    |> InferOwnedFunctionGroups.candidateBoundaries
    |> List.map (fun boundary ->
        match VerifyOwnership.callSignatureOfFunction boundary.Ownership with
        | Ok signature -> boundary.Name, signature
        | Error _ -> Crash.crash "Inferred ownership candidate has an invalid boundary")
    |> List.sortBy fst

let private cacheDescriptor definitionsByName groups identity =
    groups
    |> List.tryPick (fun group ->
        group
        |> InferOwnedFunctionGroups.candidates
        |> List.tryPick (fun candidate ->
            let boundaries = candidateIdentity candidate
            if boundaries = SelectOwnershipVariants.identityBoundaries identity then
                let sources =
                    boundaries
                    |> List.map (fun (name, _) ->
                        match Map.tryFind name definitionsByName with
                        | Some definition -> definition
                        | None -> Crash.crash "Inferred ownership candidate lost its source body")
                Some {
                    Identity = identity
                    Sources = sources
                    Boundaries = boundaries
                    InternalDependencies = InferOwnedFunctionGroups.internalDependencies group
                    ExternalTargets = InferOwnedFunctionGroups.externalTargets group
                }
            else None))

let private validateLimits limits =
    if limits.MaxIterations <= 0
       || limits.MaxGeneratedGroups < 0
       || limits.MaxRewrittenCalls < 0 then Error (InvalidLimits limits)
    else Ok ()

let private refinableUniqueArguments established uniqueArguments =
    established.Parameters
    |> List.indexed
    |> List.choose (fun (index, ownership) ->
        match ownership with
        | ConsumedCallParameter when Set.contains index uniqueArguments -> Some index
        | UnmanagedCallParameter
        | BorrowedCallParameter
        | ConsumedCallParameter
        | UniqueCallParameter -> None)
    |> Set.ofList

let private withInternalOwnership semantics definitions =
    definitions
    |> List.fold (fun result definition ->
        result |> Result.bind (fun registry ->
            VerifyOwnership.callSignatureOfFunction definition.Ownership
            |> Result.mapError (fun error -> InvalidFunctionBoundary (definition.Definition.Id, error))
            |> Result.map (fun boundary -> Map.add definition.Definition.Id boundary registry))) (Ok Map.empty)
    |> Result.map (fun registry -> {
        semantics with
            CallOwnership = fun call ->
                match Map.tryFind call.Target registry with
                | Some boundary -> Some boundary
                | None -> semantics.CallOwnership call
    })

/// Reanalyze the complete cumulatively materialized program after each round.
/// Only calls in original functions to original functions are scheduling
/// roots; recursive edges remain atomic inside their selected SCC clone.
let scheduleWithTrace
    (recordTiming: (string -> System.TimeSpan -> unit) option)
    limits
    (hir: VerifyOwnedHIR.HIRContracts<'leaf>)
    (semantics: Semantics<'leaf, 'id>)
    reservedFunctions
    (definitions: Function<'leaf, 'id> list)
    : Result<Plan<'leaf, 'id>, SchedulingError<'id>> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordTiming
        |> Option.iter (fun record -> record name timer.Elapsed)
        result
    validateLimits limits
    |> Result.bind (fun () ->
        measure
            "Ownership detail: Scheduling registry construction"
            (fun () -> withInternalOwnership semantics definitions))
    |> Result.bind (fun programSemantics ->
        InferOwnedFunctionGroups.prepareWithTrace recordTiming definitions
        |> Result.mapError InferenceFailed
        |> Result.map (fun program -> programSemantics, program))
    |> Result.bind (fun (programSemantics, program) ->
        let originalIds = definitions |> List.map (fun definition -> definition.Definition.Id) |> Set.ofList
        let definitionsById = definitions |> List.map (fun definition -> definition.Definition.Id, definition) |> Map.ofList
        let definitionsByName = definitions |> List.map (fun definition -> definition.Definition.Name, definition) |> Map.ofList
        let sourceCalls = originalCalls definitions
        let resolveDemand
            (fact: CallSiteFacts)
            (demandCache: Map<DemandKey, DemandResolution<'id> option>)
            inferredGroups =
            let uniqueArguments =
                refinableUniqueArguments fact.Established fact.UniqueArguments
            let key = {
                Target = fact.Call.Target
                Established = fact.Established
                UniqueArguments = uniqueArguments
            }
            match Map.tryFind key demandCache with
            | Some resolution -> Ok (resolution, demandCache, inferredGroups)
            | None ->
                let target =
                    match Map.tryFind fact.Call.Target definitionsById with
                    | Some definition -> definition.Definition.Name
                    | None -> Crash.crash "Filtered original call target has no definition"
                InferOwnedFunctionGroups.inferDemandWithTrace
                    recordTiming
                    programSemantics
                    program
                    fact.Call.Target
                    uniqueArguments
                |> Result.mapError InferenceFailed
                |> Result.bind (function
                    | None -> Ok (None, Map.add key None demandCache, inferredGroups)
                    | Some group ->
                        measure
                            "Ownership detail: Candidate catalog construction"
                            (fun () -> SelectOwnershipVariants.create [group])
                        |> Result.mapError CatalogFailed
                        |> Result.bind (fun catalog ->
                            SelectOwnershipVariants.select catalog {
                                Target = target
                                Established = fact.Established
                                UniqueArguments = fact.UniqueArguments
                            }
                            |> Result.mapError (fun error ->
                                SelectionFailed (callSiteIdentity fact, error)))
                        |> Result.map (function
                            | SelectOwnershipVariants.EstablishedBoundary _ ->
                                None, Map.add key None demandCache, inferredGroups
                            | SelectOwnershipVariants.InferredVariant selected
                                when SelectOwnershipVariants.selectedCallSignature selected = fact.Established ->
                                None, Map.add key None demandCache, inferredGroups
                            | SelectOwnershipVariants.InferredVariant _ as selection ->
                                let resolution = { Selection = selection }
                                Some resolution,
                                Map.add key (Some resolution) demandCache,
                                group :: inferredGroups))
        let rec loop
            number
            (requests: Map<CallSiteIdentity, MaterializeOwnershipVariants.Request<'id>>)
            history
            demandCache
            inferredGroups =
            if number > limits.MaxIterations then Error (IterationLimitExceeded limits.MaxIterations)
            else
                let orderedRequests = requests |> Map.toList |> List.map snd
                (if List.isEmpty orderedRequests then
                    measure
                        "Ownership detail: Scheduling materialization round"
                        (fun () -> Ok (MaterializeOwnershipVariants.unchanged definitions))
                 else
                    measure
                        "Ownership detail: Scheduling materialization round"
                        (fun () ->
                            MaterializeOwnershipVariants.materialize hir semantics reservedFunctions definitions orderedRequests))
                |> Result.mapError MaterializationFailed
                |> Result.bind (fun materialized ->
                    let groupCount = MaterializeOwnershipVariants.groups materialized |> List.length
                    let rewriteCount = MaterializeOwnershipVariants.rewrites materialized |> List.length
                    if groupCount > limits.MaxGeneratedGroups then Error (GeneratedGroupLimitExceeded limits.MaxGeneratedGroups)
                    elif rewriteCount > limits.MaxRewrittenCalls then Error (RewrittenCallLimitExceeded limits.MaxRewrittenCalls)
                    else
                        measure
                            "Ownership detail: Scheduling program analysis round"
                            (fun () ->
                                VerifyOwnedHIR.analyzeFunctions
                                    (MaterializeOwnershipVariants.hirContracts materialized hir)
                                    (MaterializeOwnershipVariants.ownershipSemantics materialized semantics)
                                    (MaterializeOwnershipVariants.functions materialized))
                        |> Result.mapError AnalysisFailed
                        |> Result.bind (fun facts ->
                            measure
                                "Ownership detail: Scheduling call selection round"
                                (fun () ->
                                    facts
                                    |> List.filter (fun fact ->
                                        Set.contains fact.Caller originalIds
                                        && Set.contains fact.Call.Target originalIds
                                        && not (InferOwnedFunctionGroups.isInternalRecursiveCall
                                            program
                                            fact.Caller
                                            fact.Call.Target)
                                        && not (Map.containsKey (callSiteIdentity fact) requests))
                                    |> List.sortBy callSiteIdentity
                                    |> List.fold (fun result fact ->
                                        result |> Result.bind (fun (additions, demandCache, inferredGroups) ->
                                            resolveDemand fact demandCache inferredGroups
                                            |> Result.bind (fun (resolution, demandCache, inferredGroups) ->
                                                match resolution with
                                                | None -> Ok (additions, demandCache, inferredGroups)
                                                | Some resolution ->
                                                    match Map.tryFind (callSiteIdentity fact) sourceCalls with
                                                    | None -> Error (MissingOriginalCall (callSiteIdentity fact))
                                                    | Some original ->
                                                        let request : MaterializeOwnershipVariants.Request<'id> = {
                                                            Caller = fact.Caller
                                                            Call = original
                                                            Selection = resolution.Selection
                                                        }
                                                        Ok (request :: additions, demandCache, inferredGroups))))
                                        (Ok ([], demandCache, inferredGroups)))
                            |> Result.bind (fun (additions, demandCache, inferredGroups) ->
                                let additions = additions |> List.rev
                                if List.isEmpty additions then
                                    let identities =
                                        MaterializeOwnershipVariants.groups materialized
                                        |> List.map (fun group -> group.Identity)
                                    let cache =
                                        measure
                                            "Ownership detail: Scheduling cache descriptor construction"
                                            (fun () ->
                                                identities
                                                |> List.choose (cacheDescriptor definitionsByName inferredGroups))
                                    Ok {
                                        Materialization = materialized
                                        Iterations = List.rev history
                                        Cache = cache
                                    }
                                else
                                    let next =
                                        additions
                                        |> List.fold (fun state request ->
                                            Map.add { Caller = request.Caller; Result = request.Call.Result.Id } request state) requests
                                    let iteration = {
                                        Number = number
                                        AddedCalls = additions |> List.map (fun request -> { Caller = request.Caller; Result = request.Call.Result.Id })
                                        GeneratedGroups = groupCount
                                    }
                                    loop
                                        (number + 1)
                                        next
                                        (iteration :: history)
                                        demandCache
                                        inferredGroups)))
        loop 1 Map.empty [] Map.empty [])

let schedule limits hir semantics reservedSymbols definitions =
    scheduleWithTrace None limits hir semantics reservedSymbols definitions
