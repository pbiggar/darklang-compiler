// ScheduleOwnershipVariants.fs - Drive verified ownership specialization to a bounded fixed point.

module ScheduleOwnershipVariants

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
let schedule
    limits
    (hir: VerifyOwnedHIR.HIRContracts<'leaf>)
    (semantics: Semantics<'leaf, 'id>)
    reservedSymbols
    (definitions: Function<'leaf, 'id> list)
    : Result<Plan<'leaf, 'id>, SchedulingError<'id>> =
    validateLimits limits
    |> Result.bind (fun () ->
        withInternalOwnership semantics definitions)
    |> Result.bind (fun programSemantics ->
        InferOwnedFunctionGroups.infer programSemantics definitions
        |> Result.mapError InferenceFailed)
    |> Result.bind (fun groups ->
        SelectOwnershipVariants.create groups
        |> Result.mapError CatalogFailed
        |> Result.map (fun catalog -> groups, catalog))
    |> Result.bind (fun (groups, catalog) ->
        let originalIds = definitions |> List.map (fun definition -> definition.Definition.Id) |> Set.ofList
        let definitionsById = definitions |> List.map (fun definition -> definition.Definition.Id, definition) |> Map.ofList
        let definitionsByName = definitions |> List.map (fun definition -> definition.Definition.Name, definition) |> Map.ofList
        let sourceCalls = originalCalls definitions
        let rec loop
            number
            (requests: Map<CallSiteIdentity, MaterializeOwnershipVariants.Request<'id>>)
            history =
            if number > limits.MaxIterations then Error (IterationLimitExceeded limits.MaxIterations)
            else
                let orderedRequests = requests |> Map.toList |> List.map snd
                MaterializeOwnershipVariants.materialize hir semantics reservedSymbols definitions orderedRequests
                |> Result.mapError MaterializationFailed
                |> Result.bind (fun materialized ->
                    let groupCount = MaterializeOwnershipVariants.groups materialized |> List.length
                    let rewriteCount = MaterializeOwnershipVariants.rewrites materialized |> List.length
                    if groupCount > limits.MaxGeneratedGroups then Error (GeneratedGroupLimitExceeded limits.MaxGeneratedGroups)
                    elif rewriteCount > limits.MaxRewrittenCalls then Error (RewrittenCallLimitExceeded limits.MaxRewrittenCalls)
                    else
                        VerifyOwnedHIR.analyzeFunctions
                            (MaterializeOwnershipVariants.hirContracts materialized hir)
                            (MaterializeOwnershipVariants.ownershipSemantics materialized semantics)
                            (MaterializeOwnershipVariants.functions materialized)
                        |> Result.mapError AnalysisFailed
                        |> Result.bind (fun facts ->
                            facts
                            |> List.filter (fun fact ->
                                Set.contains fact.Caller originalIds
                                && Set.contains fact.Call.Target originalIds
                                && not (Map.containsKey (callSiteIdentity fact) requests))
                            |> List.sortBy callSiteIdentity
                            |> List.fold (fun result fact ->
                                result |> Result.bind (fun additions ->
                                    let target =
                                        match Map.tryFind fact.Call.Target definitionsById with
                                        | Some definition -> definition.Definition.Name
                                        | None -> Crash.crash "Filtered original call target has no definition"
                                    SelectOwnershipVariants.select catalog {
                                        Target = target
                                        Established = fact.Established
                                        UniqueArguments = fact.UniqueArguments
                                    }
                                    |> Result.mapError (fun error -> SelectionFailed (callSiteIdentity fact, error))
                                    |> Result.bind (function
                                        | SelectOwnershipVariants.EstablishedBoundary _ -> Ok additions
                                        | SelectOwnershipVariants.InferredVariant selected
                                            when SelectOwnershipVariants.selectedCallSignature selected = fact.Established ->
                                            Ok additions
                                        | SelectOwnershipVariants.InferredVariant _ as selection ->
                                            match Map.tryFind (callSiteIdentity fact) sourceCalls with
                                            | None -> Error (MissingOriginalCall (callSiteIdentity fact))
                                            | Some original ->
                                                let request : MaterializeOwnershipVariants.Request<'id> = {
                                                    Caller = fact.Caller
                                                    Call = original
                                                    Selection = selection
                                                }
                                                Ok (request :: additions)))) (Ok [])
                            |> Result.bind (fun additions ->
                                let additions = additions |> List.rev
                                if List.isEmpty additions then
                                    let identities =
                                        MaterializeOwnershipVariants.groups materialized
                                        |> List.map (fun group -> group.Identity)
                                    let cache =
                                        identities
                                        |> List.choose (cacheDescriptor definitionsByName groups)
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
                                    loop (number + 1) next (iteration :: history))))
        loop 1 Map.empty [])
