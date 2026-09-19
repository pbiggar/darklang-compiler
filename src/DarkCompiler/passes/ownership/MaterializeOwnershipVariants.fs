// MaterializeOwnershipVariants.fs - Clone verified ownership candidates and route selected calls.

module MaterializeOwnershipVariants

open OwnedIR
open SelectOwnershipVariants

/// Value identities are local to a function; the result identifies a direct
/// call even when it occurs inside a nested branch.
type CallSiteIdentity = { Caller: AST.FunctionId; Result: HIR.ValueId }

type Request<'id> = {
    Caller: AST.FunctionId
    Call: HIR.FunctionCall
    Selection: Selection<'id>
}

type SpecializedFunction<'leaf, 'id> = {
    Original: AST.FunctionId
    Function: Function<'leaf, 'id>
}

type SpecializedGroup<'leaf, 'id> = {
    Identity: CandidateIdentity
    Members: AST.NonEmptyList<SpecializedFunction<'leaf, 'id>>
}

type CallRewrite = {
    Site: CallSiteIdentity
    Original: HIR.FunctionCall
    Specialized: HIR.FunctionCall
    Ownership: CallSignature
}

/// Original definitions and clones have one authoritative home in the plan.
/// Registries are derived from them, rather than retained as mutable caches.
type Plan<'leaf, 'id> = private {
    Originals: Function<'leaf, 'id> list
    Groups: SpecializedGroup<'leaf, 'id> list
    Rewrites: CallRewrite list
}

type MaterializationError<'id when 'id: comparison> =
    | GroupingFailed of OwnedFunctionGroups.GroupingError
    | InvalidOriginalProgram of VerifyOwnedHIR.VerificationError<'id>
    | MissingGroupMember of string
    | GroupMembershipMismatch of target: string
    | BoundaryMismatch of target: string
    | MissingCallSite of CallSiteIdentity
    | DuplicateCallSite of CallSiteIdentity
    | StaleCallSite of CallSiteIdentity
    | MixedRecursiveCandidate of CallSiteIdentity
    | SymbolCollision of string
    | InvalidMaterializedProgram of VerifyOwnedHIR.VerificationError<'id>

let groups plan = plan.Groups
let rewrites plan = plan.Rewrites
let functions plan =
    plan.Originals
    @ (plan.Groups
       |> List.collect (fun group ->
           AST.NonEmptyList.toList group.Members |> List.map (fun memberDefinition -> memberDefinition.Function)))

let private members plan =
    plan.Groups |> List.collect (fun group -> AST.NonEmptyList.toList group.Members)

let private typedSignature (definition: Function<'leaf, 'id>) : HIR.FunctionSignature = {
    Parameters = definition.Definition.Body.Body.Parameters |> List.map (fun parameter -> parameter.Value.Type)
    Result = definition.Definition.Body.Body.Result.Type
}

let private boundaryCall (definition: Function<'leaf, 'id>) : HIR.FunctionCall = {
    Target = definition.Definition.Id
    Arguments = definition.Definition.Body.Body.Parameters |> List.map (fun parameter -> parameter.Value)
    Result = definition.Definition.Body.Body.Result
}

let private verifiedCallSignature boundary =
    match VerifyOwnership.callSignatureOfFunction boundary with
    | Ok signature -> signature
    | Error _ -> Crash.crash "Materialized ownership boundary has no valid call signature"

/// Clone effects and aliases come from the source function's independent HIR
/// contract, instantiated with the actual call operands. Ownership is never
/// used to invent either fact.
let hirContracts plan (source: VerifyOwnedHIR.HIRContracts<'leaf>) =
    let registry =
        members plan
        |> List.map (fun memberDefinition -> memberDefinition.Function.Definition.Id, memberDefinition)
        |> Map.ofList
    { source with
        CallSignature = fun target ->
            match Map.tryFind target registry with
            | Some memberDefinition -> Some (typedSignature memberDefinition.Function)
            | None -> source.CallSignature target
        CallContract = fun call ->
            match Map.tryFind call.Target registry with
            | Some memberDefinition -> source.CallContract { call with Target = memberDefinition.Original }
            | None -> source.CallContract call }

let ownershipSemantics plan (source: Semantics<'leaf, 'id>) =
    let registry =
        members plan
        |> List.map (fun memberDefinition ->
            memberDefinition.Function.Definition.Id, verifiedCallSignature memberDefinition.Function.Ownership)
        |> Map.ofList
    { source with
        CallOwnership = fun call ->
            match Map.tryFind call.Target registry with
            | Some signature -> Some signature
            | None -> source.CallOwnership call }

let private symbolSuffix identity =
    let number (value: int) = value.ToString(System.Globalization.CultureInfo.InvariantCulture)
    let field (value: string) = number value.Length + ":" + value
    let parameter = function
        | UnmanagedCallParameter -> "u"
        | BorrowedCallParameter -> "b"
        | ConsumedCallParameter -> "c"
        | UniqueCallParameter -> "q"
    let result = function
        | UnmanagedCallResult -> "u"
        | BorrowedCallResult index -> "b" + number index
        | ProducedCallResult -> "p"
        | UniqueProducedCallResult -> "q"
    let encoded =
        identityBoundaries identity
        |> List.map (fun (name, signature) ->
            let parameters = signature.Parameters |> List.map parameter |> String.concat ""
            field (field name + field parameters + field (result signature.Result)))
        |> String.concat ""
    // A versioned, length-delimited encoding and full digest make names stable
    // across processes, cultures, request order, and callee-local identities.
    let digest =
        System.Text.Encoding.UTF8.GetBytes("ownership-v1:" + encoded)
        |> System.Security.Cryptography.SHA256.HashData
        |> System.Convert.ToHexString
    "__ownership_" + digest.ToLowerInvariant()

let private refines (original: FunctionSignature<'id>) (candidate: FunctionSignature<'id>) =
    let parameter first second =
        first = second
        || match first, second with
           | ConsumedParameter first, UniqueParameter second -> first = second
           | _ -> false
    let rec parameters first second =
        match first, second with
        | [], [] -> true
        | first :: firstRest, second :: secondRest -> parameter first second && parameters firstRest secondRest
        | _ -> false
    parameters original.Parameters candidate.Parameters
    && (original.Result = candidate.Result
        || match original.Result, candidate.Result with
           | ProducedResult first, UniqueProducedResult second -> first = second
           | _ -> false)

let rec private calls (block: Block<'leaf, 'id>) =
    block.Body.Operations
    |> List.collect (function
        | Evaluate (HIR.Call call) -> [call]
        | Evaluate (HIR.Branch (_, _, yes, no)) -> calls yes @ calls no
        | Evaluate (HIR.Leaf _ | HIR.ScalarBinding _) | Dup _ | Drop _ -> [])

let rec private rewriteCalls rewrite (block: Block<'leaf, 'id>) =
    let operations =
        block.Body.Operations
        |> List.map (function
            | Evaluate (HIR.Call call) -> Evaluate (HIR.Call (rewrite call))
            | Evaluate (HIR.Branch (result, condition, yes, no)) ->
                Evaluate (HIR.Branch (result, condition, rewriteCalls rewrite yes, rewriteCalls rewrite no))
            | step -> step)
    { Body = { block.Body with Operations = operations } }

let private site (request: Request<'id>) = { Caller = request.Caller; Result = request.Call.Result.Id }

let private validateRequests definitions discovered semantics requests =
    let definitionsByName = definitions |> List.map (fun definition -> definition.Definition.Name, definition) |> Map.ofList
    let definitionsById = definitions |> List.map (fun definition -> definition.Definition.Id, definition) |> Map.ofList
    let groupNames =
        discovered
        |> List.collect (fun group ->
            let names = OwnedFunctionGroups.functions group |> List.map (fun definition -> definition.Definition.Name) |> Set.ofList
            let ids = names |> Set.map AST.functionIdForName
            names |> Set.toList |> List.map (fun name -> AST.functionIdForName name, ids))
        |> Map.ofList
    let callSites =
        definitions
        |> List.collect (fun definition ->
            calls definition.Definition.Body
            |> List.map (fun call -> { Caller = definition.Definition.Id; Result = call.Result.Id }, call))
        |> Map.ofList
    let validateCandidate request selected =
        let target = selectedTargetBoundary selected
        let requestTargetName =
            match Map.tryFind request.Call.Target definitionsById with
            | Some definition -> definition.Definition.Name
            | None -> $"function#{AST.functionIdValue request.Call.Target}"
        let boundaries = selectedCandidate selected |> InferOwnedFunctionGroups.candidateBoundaries
        boundaries
        |> List.fold (fun result boundary ->
            result |> Result.bind (fun () ->
                match Map.tryFind boundary.Name definitionsByName with
                | None -> Error (MissingGroupMember boundary.Name)
                | Some original when not (refines original.Ownership boundary.Ownership) -> Error (BoundaryMismatch boundary.Name)
                | Some _ -> Ok ())) (Ok ())
        |> Result.bind (fun () ->
            let names = boundaries |> List.map (fun boundary -> boundary.Name) |> Set.ofList
            if AST.functionIdForName target.Name <> request.Call.Target then Error (BoundaryMismatch requestTargetName)
            elif Map.tryFind request.Call.Target groupNames <> Some (names |> Set.map AST.functionIdForName) then
                Error (GroupMembershipMismatch target.Name)
            elif Set.contains request.Caller (names |> Set.map AST.functionIdForName) then
                Error (MixedRecursiveCandidate (site request))
            else Ok ())
    requests
    |> List.fold (fun result request ->
        result |> Result.bind (fun seen ->
            let callSite = site request
            if Set.contains callSite seen then Error (DuplicateCallSite callSite)
            else
                match Map.tryFind callSite callSites with
                | None -> Error (MissingCallSite callSite)
                | Some actual when actual <> request.Call -> Error (StaleCallSite callSite)
                | Some _ ->
                    let valid =
                        match request.Selection with
                        | InferredVariant selected -> validateCandidate request selected
                        | EstablishedBoundary expected ->
                            let actual =
                                match Map.tryFind request.Call.Target definitionsById with
                                | Some definition ->
                                    VerifyOwnership.callSignatureOfFunction definition.Ownership
                                    |> Result.map Some
                                | None -> Ok (semantics.CallOwnership request.Call)
                            match actual with
                            | Ok (Some actual) when actual = expected -> Ok ()
                            | _ ->
                                let targetName =
                                    match Map.tryFind request.Call.Target definitionsById with
                                    | Some definition -> definition.Definition.Name
                                    | None -> $"function#{AST.functionIdValue request.Call.Target}"
                                Error (BoundaryMismatch targetName)
                    valid |> Result.map (fun () -> Set.add callSite seen))) (Ok Set.empty)
    |> Result.map ignore

let private cloneGroups
    (hir: VerifyOwnedHIR.HIRContracts<'leaf>)
    (semantics: Semantics<'leaf, 'id>)
    reserved
    definitions
    requests =
    let definitionsByName = definitions |> List.map (fun definition -> definition.Definition.Name, definition) |> Map.ofList
    let selections =
        requests
        |> List.choose (fun request ->
            match request.Selection with
            | EstablishedBoundary _ -> None
            | InferredVariant selected -> Some selected)
        |> List.map (fun selected -> selectedIdentity selected, selectedCandidate selected)
        |> Map.ofList
    let occupied = Set.union reserved (definitions |> List.map (fun definition -> definition.Definition.Name) |> Set.ofList)
    selections
    |> Map.toList
    |> List.fold (fun result (identity, candidate) ->
        result |> Result.bind (fun (occupied, groups) ->
            let suffix = symbolSuffix identity
            let boundaries = InferOwnedFunctionGroups.candidateBoundaries candidate |> List.sortBy (fun boundary -> boundary.Name)
            let symbols =
                boundaries
                |> List.map (fun boundary -> AST.functionIdForName boundary.Name, AST.functionIdForName (boundary.Name + suffix))
                |> Map.ofList
            let rewrite (call: HIR.FunctionCall) =
                match Map.tryFind call.Target symbols with
                | Some symbol -> { call with Target = symbol }
                | None -> call
            boundaries
            |> List.fold (fun result boundary ->
                result |> Result.bind (fun (occupied, members) ->
                    match Map.tryFind boundary.Name definitionsByName with
                    | None -> Error (MissingGroupMember boundary.Name)
                    | Some original ->
                        let name = boundary.Name + suffix
                        let clone = {
                            Ownership = boundary.Ownership
                            Definition = {
                                original.Definition with
                                    Id = AST.functionIdForName name
                                    Name = name
                                    Body = rewriteCalls rewrite original.Definition.Body
                            }
                        }
                        if Set.contains name occupied
                           || Option.isSome (hir.CallSignature clone.Definition.Id)
                           || Option.isSome (semantics.CallOwnership (boundaryCall clone)) then
                            Error (SymbolCollision name)
                        else
                            Ok (
                                Set.add name occupied,
                                { Original = original.Definition.Id; Function = clone } :: members))) (Ok (occupied, []))
            |> Result.map (fun (occupied, members) ->
                match AST.NonEmptyList.tryFromList (List.rev members) with
                | None -> Crash.crash "Selected ownership candidate has no members"
                | Some members -> occupied, { Identity = identity; Members = members } :: groups))) (Ok (occupied, []))
    |> Result.map (snd >> List.rev)

/// Requests address calls in the supplied original definitions. A recursive
/// edge cannot be selected independently: clones route every internal edge
/// through their complete candidate, and keep outside calls established.
/// The caller reserves all other symbols in the compilation unit. No changes
/// escape this pass unless the complete materialized program verifies.
let materialize
    (hir: VerifyOwnedHIR.HIRContracts<'leaf>)
    (semantics: Semantics<'leaf, 'id>)
    (reservedSymbols: Set<string>)
    (definitions: Function<'leaf, 'id> list)
    (requests: Request<'id> list)
    : Result<Plan<'leaf, 'id>, MaterializationError<'id>> =
    OwnedFunctionGroups.discover definitions
    |> Result.mapError GroupingFailed
    |> Result.bind (fun discovered ->
        validateRequests definitions discovered semantics requests
        |> Result.bind (fun () ->
            VerifyOwnedHIR.verifyFunctions hir semantics definitions
            |> Result.mapError InvalidOriginalProgram))
    |> Result.bind (fun () -> cloneGroups hir semantics reservedSymbols definitions requests)
    |> Result.bind (fun groups ->
        let targets =
            groups
            |> List.collect (fun group ->
                AST.NonEmptyList.toList group.Members
                |> List.map (fun memberDefinition ->
                    (group.Identity, memberDefinition.Original), memberDefinition.Function.Definition.Id))
            |> Map.ofList
        let rewrites =
            requests
            |> List.choose (fun request ->
                match request.Selection with
                | EstablishedBoundary _ -> None
                | InferredVariant selected ->
                    let target =
                        match Map.tryFind (selectedIdentity selected, request.Call.Target) targets with
                        | Some target -> target
                        | None -> Crash.crash "Validated ownership selection has no materialized target"
                    Some {
                        Site = site request
                        Original = request.Call
                        Specialized = { request.Call with Target = target }
                        Ownership = selectedCallSignature selected
                    })
            |> List.sortBy (fun rewrite -> rewrite.Site)
        let bySite = rewrites |> List.map (fun rewrite -> rewrite.Site, rewrite.Specialized) |> Map.ofList
        let originals =
            definitions |> List.map (fun definition ->
                let rewrite (call: HIR.FunctionCall) =
                    match Map.tryFind { Caller = definition.Definition.Id; Result = call.Result.Id } bySite with
                    | Some specialized -> specialized
                    | None -> call
                { definition with Definition = { definition.Definition with Body = rewriteCalls rewrite definition.Definition.Body } })
        let plan = { Originals = originals; Groups = groups; Rewrites = rewrites }
        VerifyOwnedHIR.verifyFunctions (hirContracts plan hir) (ownershipSemantics plan semantics) (functions plan)
        |> Result.mapError InvalidMaterializedProgram
        |> Result.map (fun () -> plan))
