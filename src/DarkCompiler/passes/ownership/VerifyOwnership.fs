// VerifyOwnership.fs - Verify closed structured regions using dialect ownership contracts.

module VerifyOwnership

open OwnedIR

/// Definitions are globally fresh even across mutually exclusive branches;
/// live ownership is path-local and must agree at every shared continuation.
/// The function signature owns boundary transfer; primitive contracts continue
/// to own operation-local use and production without duplicating alias facts.
let verifyFunction
    (semantics: Semantics<'leaf, 'id>)
    (signature: FunctionSignature<'id>)
    (root: Block<'leaf, 'id>) =
    let rec release live = function
        | [] -> Ok live
        | value :: rest when Set.contains value live -> release (Set.remove value live) rest
        | value :: _ -> Error (InvalidRelease value)
    let rec define declared live = function
        | [] -> Ok (declared, live)
        | value :: _ when Set.contains value declared -> Error (DuplicateDefinition value)
        | value :: rest -> define (Set.add value declared) (Set.add value live) rest
    let require borrowed live uses =
        match Set.difference uses (Set.union borrowed live) |> Set.toList with
        | [] -> Ok ()
        | value :: _ -> Error (InvalidUse value)
    let scalar borrowed live operand = require borrowed live (semantics.ScalarUses operand)
    let managed value =
        match semantics.BlockArgument value with
        | Unmanaged -> None
        | Managed id -> Some id
    let requireManaged borrowed live value =
        match managed value with
        | None -> Ok ()
        | Some id -> require borrowed live (Set.singleton id)
    let leaf declared borrowed live operation =
        let contract = semantics.Leaf operation
        let uses = contract.Inputs |> List.map (function Borrowed id | Consumed id -> id) |> Set.ofList
        let consumes = contract.Inputs |> List.choose (function Consumed id -> Some id | Borrowed _ -> None)
        require borrowed live uses |> Result.bind (fun () ->
            release live consumes |> Result.bind (fun live -> define declared live contract.Outputs))
    let rec loop declared borrowed live = function
        | [] -> Ok (declared, live)
        | step :: rest ->
            let after =
                match step.Operation with
                | HIR.Leaf operation -> leaf declared borrowed live operation
                | HIR.ScalarBinding (_, value) -> scalar borrowed live value |> Result.map (fun () -> declared, live)
                | HIR.Branch (result, condition, yes, no) ->
                    scalar borrowed live condition |> Result.bind (fun () ->
                        block declared borrowed live yes |> Result.bind (fun (afterYes, yesLive, yesResult) ->
                            block afterYes borrowed live no |> Result.bind (fun (afterNo, noLive, noResult) ->
                                match managed yesResult, managed noResult, managed result with
                                | None, None, None ->
                                    if yesLive <> noLive then Error InconsistentJoin
                                    else Ok (afterNo, yesLive)
                                | Some yesId, Some noId, Some resultId ->
                                    release yesLive [yesId] |> Result.bind (fun yesRemainder ->
                                        release noLive [noId] |> Result.bind (fun noRemainder ->
                                            if yesRemainder <> noRemainder then Error InconsistentJoin
                                            else define afterNo yesRemainder [resultId]))
                                | _ -> Error InconsistentBlockArgument)))
            after |> Result.bind (fun (declared, live) ->
                release live step.Releases |> Result.bind (fun live -> loop declared borrowed live rest))
    and block declared borrowed live body =
        release live body.EntryReleases |> Result.bind (fun live ->
            loop declared borrowed live body.Body.Operations |> Result.bind (fun (declared, live) ->
                requireManaged borrowed live body.Body.Result
                |> Result.map (fun () -> declared, live, body.Body.Result)))
    let parameterId = function
        | BorrowedParameter id | ConsumedParameter id -> id
    let parameterIds = signature.Parameters |> List.map parameterId
    let duplicateParameter =
        parameterIds
        |> List.countBy id
        |> List.tryFind (fun (_, count) -> count > 1)
        |> Option.map fst
    let managedParameters =
        root.Body.Parameters
        |> Map.values
        |> Seq.choose managed
        |> Seq.toList
    match duplicateParameter with
    | Some id -> Error (DuplicateParameter id)
    | None when Set.ofList parameterIds <> Set.ofList managedParameters
                || List.length parameterIds <> List.length managedParameters ->
        Error InconsistentFunctionParameters
    | None ->
        let borrowed =
            signature.Parameters
            |> List.choose (function BorrowedParameter id -> Some id | ConsumedParameter _ -> None)
            |> Set.ofList
        let owned =
            signature.Parameters
            |> List.choose (function ConsumedParameter id -> Some id | BorrowedParameter _ -> None)
            |> Set.ofList
        block (Set.ofList parameterIds) borrowed owned root
        |> Result.bind (fun (_, live, result) ->
            let resultOwnership =
                match signature.Result, managed result with
                | UnmanagedResult, None -> Ok live
                | BorrowedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | BorrowedResult expected, Some _ when Set.contains expected borrowed -> Ok live
                | BorrowedResult expected, Some _ -> Error (InvalidBorrowedResult expected)
                | ProducedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | ProducedResult expected, Some _ when Set.contains expected live -> Ok (Set.remove expected live)
                | ProducedResult expected, Some _ -> Error (InvalidProducedResult expected)
                | _ -> Error InconsistentFunctionResult
            resultOwnership |> Result.bind (fun live ->
                if Set.isEmpty live then Ok () else Error (UnreleasedValues live)))

/// Closed regions are functions with no managed parameters and an unmanaged
/// result. Keeping this as a wrapper makes the existing boundary explicit.
let verifyClosed (semantics: Semantics<'leaf, 'id>) (root: Block<'leaf, 'id>) =
    verifyFunction semantics { Parameters = []; Result = UnmanagedResult } root
