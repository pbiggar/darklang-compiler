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
    let addUnit value units =
        Map.change value (fun count -> Some (Option.defaultValue 0 count + 1)) units
    let rec drop units = function
        | [] -> Ok units
        | value :: rest ->
            match Map.tryFind value units with
            | Some 1 -> drop (Map.remove value units) rest
            | Some count -> drop (Map.add value (count - 1) units) rest
            | None -> Error (InvalidDrop value)
    let rec define declared units = function
        | [] -> Ok (declared, units)
        | value :: _ when Set.contains value declared -> Error (DuplicateDefinition value)
        | value :: rest -> define (Set.add value declared) (addUnit value units) rest
    let owned units = units |> Map.keys |> Set.ofSeq
    let require borrowed units uses =
        match Set.difference uses (Set.union borrowed (owned units)) |> Set.toList with
        | [] -> Ok ()
        | value :: _ -> Error (InvalidUse value)
    let scalar borrowed units operand = require borrowed units (semantics.ScalarUses operand)
    let managed value =
        match semantics.BlockArgument value with
        | Unmanaged -> None
        | Managed id -> Some id
    let requireManaged borrowed units value =
        match managed value with
        | None -> Ok ()
        | Some id -> require borrowed units (Set.singleton id)
    let contract declared borrowed units (contract: Contract<'id>) =
        let uses = contract.Inputs |> List.map (function Borrowed id | Consumed id -> id) |> Set.ofList
        let consumes = contract.Inputs |> List.choose (function Consumed id -> Some id | Borrowed _ -> None)
        require borrowed units uses |> Result.bind (fun () ->
            drop units consumes |> Result.bind (fun units -> define declared units contract.Outputs))
    let leaf declared borrowed units operation = contract declared borrowed units (semantics.Leaf operation)
    let call declared borrowed units (call: HIR.FunctionCall) =
        match semantics.CallOwnership call with
        | None -> Error (UnknownCallOwnership call.Target)
        | Some signature when List.length signature.Parameters <> List.length call.Arguments ->
            Error (InconsistentCallOwnershipParameters call.Target)
        | Some signature ->
            let arguments = call.Arguments |> List.map managed
            let rec inputs index acc modes values =
                match modes, values with
                | [], [] -> Ok (List.rev acc)
                | UnmanagedCallParameter :: modes, None :: values -> inputs (index + 1) acc modes values
                | BorrowedCallParameter :: modes, Some id :: values -> inputs (index + 1) (Borrowed id :: acc) modes values
                | ConsumedCallParameter :: modes, Some id :: values -> inputs (index + 1) (Consumed id :: acc) modes values
                | _ -> Error (InconsistentCallOwnershipArgument (call.Target, index))
            inputs 0 [] signature.Parameters arguments |> Result.bind (fun callInputs ->
                match signature.Result, managed call.Result with
                | UnmanagedCallResult, None -> contract declared borrowed units { Inputs = callInputs; Outputs = [] }
                | ProducedCallResult, Some result -> contract declared borrowed units { Inputs = callInputs; Outputs = [result] }
                | BorrowedCallResult index, Some result when index >= 0 ->
                    match List.tryItem index signature.Parameters, List.tryItem index arguments with
                    | Some BorrowedCallParameter, Some (Some source) when source = result ->
                        contract declared borrowed units { Inputs = callInputs; Outputs = [] }
                    | Some BorrowedCallParameter, Some (Some _) -> Error (InconsistentCallOwnershipResult call.Target)
                    | _ -> Error (InvalidBorrowedCallResult (call.Target, index))
                | BorrowedCallResult index, Some _ -> Error (InvalidBorrowedCallResult (call.Target, index))
                | _ -> Error (InconsistentCallOwnershipResult call.Target))
    let rec loop declared borrowed units = function
        | [] -> Ok (declared, units)
        | step :: rest ->
            let after =
                match step with
                | Dup value ->
                    require borrowed units (Set.singleton value)
                    |> Result.map (fun () -> declared, addUnit value units)
                | Drop value -> drop units [value] |> Result.map (fun units -> declared, units)
                | Evaluate operation ->
                    match operation with
                    | HIR.Leaf leafOperation -> leaf declared borrowed units leafOperation
                    | HIR.ScalarBinding (_, value) -> scalar borrowed units value |> Result.map (fun () -> declared, units)
                    | HIR.Call functionCall -> call declared borrowed units functionCall
                    | HIR.Branch (result, condition, yes, no) ->
                        scalar borrowed units condition |> Result.bind (fun () ->
                            block declared borrowed units yes |> Result.bind (fun (afterYes, yesUnits, yesResult) ->
                                block afterYes borrowed units no |> Result.bind (fun (afterNo, noUnits, noResult) ->
                                    match managed yesResult, managed noResult, managed result with
                                    | None, None, None ->
                                        if yesUnits <> noUnits then Error InconsistentJoin
                                        else Ok (afterNo, yesUnits)
                                    | Some yesId, Some noId, Some resultId ->
                                        drop yesUnits [yesId] |> Result.bind (fun yesRemainder ->
                                            drop noUnits [noId] |> Result.bind (fun noRemainder ->
                                                if yesRemainder <> noRemainder then Error InconsistentJoin
                                                else define afterNo yesRemainder [resultId]))
                                    | _ -> Error InconsistentBlockArgument)))
            after |> Result.bind (fun (declared, units) -> loop declared borrowed units rest)
    and block declared borrowed units body =
        loop declared borrowed units body.Body.Operations |> Result.bind (fun (declared, units) ->
            requireManaged borrowed units body.Body.Result
            |> Result.map (fun () -> declared, units, body.Body.Result))
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
        let units =
            signature.Parameters
            |> List.choose (function ConsumedParameter id -> Some id | BorrowedParameter _ -> None)
            |> List.fold (fun units id -> addUnit id units) Map.empty
        block (Set.ofList parameterIds) borrowed units root
        |> Result.bind (fun (_, units, result) ->
            let resultOwnership =
                match signature.Result, managed result with
                | UnmanagedResult, None -> Ok units
                | BorrowedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | BorrowedResult expected, Some _ when Set.contains expected borrowed -> Ok units
                | BorrowedResult expected, Some _ -> Error (InvalidBorrowedResult expected)
                | ProducedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | ProducedResult expected, Some _ ->
                    match drop units [expected] with
                    | Ok units -> Ok units
                    | Error _ -> Error (InvalidProducedResult expected)
                | _ -> Error InconsistentFunctionResult
            resultOwnership |> Result.bind (fun units ->
                if Map.isEmpty units then Ok () else Error (UndroppedValues (owned units))))

/// Closed regions are functions with no managed parameters and an unmanaged
/// result. Keeping this as a wrapper makes the existing boundary explicit.
let verifyClosed (semantics: Semantics<'leaf, 'id>) (root: Block<'leaf, 'id>) =
    verifyFunction semantics { Parameters = []; Result = UnmanagedResult } root
