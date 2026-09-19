// SelectOwnershipVariants.fs - Choose inferred ownership variants at direct call sites.

module SelectOwnershipVariants

open OwnedIR

/// Canonical group boundaries exclude callee-local identities and discovery
/// order so equivalent requests share one materialization/cache identity.
type CandidateIdentity =
    private
    | CandidateIdentity of AST.NonEmptyList<string * CallSignature>

type Catalog<'id> =
    private
    | Catalog of Map<string, InferOwnedFunctionGroups.Group<'id>>

type CallSite = {
    Target: string
    Established: CallSignature
    UniqueArguments: Set<int>
}

type SelectedVariant<'id> =
    private {
        Identity: CandidateIdentity
        Candidate: InferOwnedFunctionGroups.Candidate<'id>
        TargetBoundary: InferOwnedFunctionGroups.FunctionBoundary<'id>
        CallSignature: CallSignature
    }

type Selection<'id> =
    | EstablishedBoundary of CallSignature
    | InferredVariant of SelectedVariant<'id>

type SelectionError =
    | DuplicateFunctionName of string
    | UnknownFunction of string
    | InvalidUniqueArgumentIndex of target: string * parameterIndex: int
    | MissingEstablishedUniqueArgument of target: string * parameterIndex: int
    | InconsistentEstablishedBoundary of target: string

let selectedIdentity selected = selected.Identity
let selectedCandidate selected = selected.Candidate
let selectedTargetBoundary selected = selected.TargetBoundary
let selectedCallSignature selected = selected.CallSignature
let identityBoundaries (CandidateIdentity boundaries) = AST.NonEmptyList.toList boundaries

let private nonEmpty context values =
    match AST.NonEmptyList.tryFromList values with
    | Some values -> values
    | None -> Crash.crash context

let private callSignature
    (boundary: InferOwnedFunctionGroups.FunctionBoundary<'id>) =
    match VerifyOwnership.callSignatureOfFunction boundary.Ownership with
    | Ok signature -> signature
    | Error _ ->
        Crash.crash "Verifier-proven ownership candidate has an invalid call boundary"

let private candidateIdentity candidate =
    candidate
    |> InferOwnedFunctionGroups.candidateBoundaries
    |> List.map (fun boundary -> boundary.Name, callSignature boundary)
    |> List.sortBy fst
    |> nonEmpty "Ownership variant candidate has no function boundaries"
    |> CandidateIdentity

let private firstCandidate group =
    match InferOwnedFunctionGroups.candidates group with
    | head :: _ -> head
    | [] -> Crash.crash "Inferred ownership group has no candidates"

let private functionNames group =
    group
    |> firstCandidate
    |> InferOwnedFunctionGroups.candidateBoundaries
    |> List.map (fun boundary -> boundary.Name)

/// Index inferred groups by every function they contain. A recursive group maps
/// each member name back to the same group, so later selection always returns a
/// complete group candidate.
let create groups : Result<Catalog<'id>, SelectionError> =
    let addName group result name =
        result
        |> Result.bind (fun catalog ->
            match Map.tryFind name catalog with
            | Some _ -> Error (DuplicateFunctionName name)
            | None -> Ok (Map.add name group catalog))
    groups
    |> List.fold (fun result group ->
        functionNames group
        |> List.fold (addName group) result) (Ok Map.empty)
    |> Result.map Catalog

type private ParameterTransfer =
    | UnmanagedTransfer
    | BorrowedTransfer
    | ConsumedTransfer

type private ResultTransfer =
    | UnmanagedReturn
    | BorrowedReturn of parameterIndex: int
    | ProducedReturn

let private parameterTransfer = function
    | UnmanagedCallParameter -> UnmanagedTransfer
    | BorrowedCallParameter -> BorrowedTransfer
    | ConsumedCallParameter | UniqueCallParameter -> ConsumedTransfer

let private resultTransfer = function
    | UnmanagedCallResult -> UnmanagedReturn
    | BorrowedCallResult parameterIndex -> BorrowedReturn parameterIndex
    | ProducedCallResult | UniqueProducedCallResult -> ProducedReturn

let rec private sameParameterTransfers first second =
    match first, second with
    | [], [] -> true
    | first :: firstRest, second :: secondRest ->
        parameterTransfer first = parameterTransfer second
        && sameParameterTransfers firstRest secondRest
    | _ -> false

let private sameTransferShape first second =
    sameParameterTransfers first.Parameters second.Parameters
    && resultTransfer first.Result = resultTransfer second.Result

let private preservesEstablishedResult established candidate =
    match established.Result, candidate.Result with
    | UniqueProducedCallResult, UniqueProducedCallResult -> true
    | UniqueProducedCallResult, _ -> false
    | UnmanagedCallResult, UnmanagedCallResult
    | BorrowedCallResult _, BorrowedCallResult _
    | ProducedCallResult, ProducedCallResult
    | ProducedCallResult, UniqueProducedCallResult -> true
    | _ -> false

let private requiredUniqueArguments signature =
    signature.Parameters
    |> List.indexed
    |> List.choose (fun (index, ownership) ->
        match ownership with
        | UniqueCallParameter -> Some index
        | UnmanagedCallParameter
        | BorrowedCallParameter
        | ConsumedCallParameter -> None)
    |> Set.ofList

type private ApplicableCandidate<'id> = {
    Identity: CandidateIdentity
    Candidate: InferOwnedFunctionGroups.Candidate<'id>
    Boundary: InferOwnedFunctionGroups.FunctionBoundary<'id>
    Signature: CallSignature
    RequiredUniqueArguments: Set<int>
}

let private targetCandidate target candidate =
    let boundary =
        candidate
        |> InferOwnedFunctionGroups.candidateBoundaries
        |> List.tryFind (fun boundary -> boundary.Name = target)
    match boundary with
    | Some boundary ->
        let signature = callSignature boundary
        {
            Identity = candidateIdentity candidate
            Candidate = candidate
            Boundary = boundary
            Signature = signature
            RequiredUniqueArguments = requiredUniqueArguments signature
        }
    | None ->
        Crash.crash "Inferred ownership candidates disagree on their function group"

let private resultPreference signature =
    match signature.Result with
    | UniqueProducedCallResult -> 0
    | UnmanagedCallResult
    | BorrowedCallResult _
    | ProducedCallResult -> 1

let private preference candidate =
    resultPreference candidate.Signature,
    Set.count candidate.RequiredUniqueArguments,
    candidate.Identity

/// Select the best inferred group candidate applicable to the call's proven
/// unique arguments. Uniqueness may strengthen a consumed parameter or produced
/// result, but the established borrow/consume/produce shape cannot change.
/// When no inferred candidate applies, the established verified boundary is
/// retained. Recursive candidates remain atomic in `SelectedVariant.Candidate`.
let select
    (Catalog catalog)
    (site: CallSite)
    : Result<Selection<'id>, SelectionError> =
    match Map.tryFind site.Target catalog with
    | None -> Error (UnknownFunction site.Target)
    | Some group ->
        let parameterCount = List.length site.Established.Parameters
        match
            site.UniqueArguments
            |> Set.toList
            |> List.tryFind (fun index -> index < 0 || index >= parameterCount)
        with
        | Some index -> Error (InvalidUniqueArgumentIndex (site.Target, index))
        | None ->
            match
                Set.difference
                    (requiredUniqueArguments site.Established)
                    site.UniqueArguments
                |> Set.toList
            with
            | index :: _ ->
                Error (MissingEstablishedUniqueArgument (site.Target, index))
            | [] ->
                let candidates =
                    group
                    |> InferOwnedFunctionGroups.candidates
                    |> List.map (targetCandidate site.Target)
                match candidates with
                | head :: _ when not (sameTransferShape site.Established head.Signature) ->
                    Error (InconsistentEstablishedBoundary site.Target)
                | [] -> Crash.crash "Inferred ownership group has no candidates"
                | _ ->
                    let applicable =
                        candidates
                        |> List.filter (fun candidate ->
                            sameTransferShape site.Established candidate.Signature
                            && preservesEstablishedResult
                                site.Established
                                candidate.Signature
                            && Set.isSubset
                                candidate.RequiredUniqueArguments
                                site.UniqueArguments)
                        |> List.sortBy preference
                    match applicable with
                    | [] -> Ok (EstablishedBoundary site.Established)
                    | selected :: _ ->
                        Ok (
                            InferredVariant {
                                Identity = selected.Identity
                                Candidate = selected.Candidate
                                TargetBoundary = selected.Boundary
                                CallSignature = selected.Signature
                            })
