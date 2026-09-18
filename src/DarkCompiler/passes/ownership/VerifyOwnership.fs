// VerifyOwnership.fs - Verify closed structured regions using dialect ownership contracts.

module VerifyOwnership

open OwnedIR

type private OwnershipState<'id when 'id: comparison> = {
    Units: Map<'id, int>
    Exclusive: Set<'id>
}

/// Definitions are globally fresh even across mutually exclusive branches;
/// live ownership is path-local and must agree at every shared continuation.
/// The function signature owns boundary transfer; primitive contracts continue
/// to own operation-local use and production without duplicating alias facts.
let verifyFunction
    (semantics: Semantics<'leaf, 'id>)
    (signature: FunctionSignature<'id>)
    (root: Block<'leaf, 'id>) =
    let addUnit value state =
        { state with Units = Map.change value (fun count -> Some (Option.defaultValue 0 count + 1)) state.Units }
    let rec drop state = function
        | [] -> Ok state
        | value :: rest ->
            match Map.tryFind value state.Units with
            | Some 1 ->
                drop { Units = Map.remove value state.Units; Exclusive = Set.remove value state.Exclusive } rest
            | Some count ->
                drop { state with Units = Map.add value (count - 1) state.Units } rest
            | None -> Error (InvalidDrop value)
    let rec define uniqueOutputs declared state = function
        | [] -> Ok (declared, state)
        | value :: _ when Set.contains value declared -> Error (DuplicateDefinition value)
        | value :: rest ->
            let next = addUnit value state
            let next =
                if Set.contains value uniqueOutputs then { next with Exclusive = Set.add value next.Exclusive }
                else next
            define uniqueOutputs (Set.add value declared) next rest
    let owned state = state.Units |> Map.keys |> Set.ofSeq
    let require borrowed state uses =
        match Set.difference uses (Set.union borrowed (owned state)) |> Set.toList with
        | [] -> Ok ()
        | value :: _ -> Error (InvalidUse value)
    let isUnique state value =
        Set.contains value state.Exclusive && Map.tryFind value state.Units = Some 1
    let requireUnique state values =
        match values |> Set.toList |> List.tryFind (isUnique state >> not) with
        | None -> Ok ()
        | Some value -> Error (NonUniqueUse value)
    let scalar borrowed state operand =
        let uses = semantics.ScalarUses operand
        let escapes = semantics.ScalarEscapes operand
        require borrowed state uses |> Result.bind (fun () ->
            match Set.difference escapes uses |> Set.toList with
            | value :: _ -> Error (InvalidUniquenessContract value)
            | [] -> Ok { state with Exclusive = Set.difference state.Exclusive escapes })
    let managed value =
        match semantics.BlockArgument value with
        | Unmanaged -> None
        | Managed id -> Some id
    let requireManaged borrowed state value =
        match managed value with
        | None -> Ok ()
        | Some id -> require borrowed state (Set.singleton id)
    let contract declared borrowed state (contract: Contract<'id>) uniqueness =
        let uses = contract.Inputs |> List.map (function Borrowed id | Consumed id -> id) |> Set.ofList
        let consumes = contract.Inputs |> List.choose (function Consumed id -> Some id | Borrowed _ -> None)
        let outputs = Set.ofList contract.Outputs
        match Set.difference uniqueness.RequiredInputs uses |> Set.toList,
              Set.difference uniqueness.UniqueOutputs outputs |> Set.toList with
        | value :: _, _ | _, value :: _ -> Error (InvalidUniquenessContract value)
        | [], [] ->
            require borrowed state uses |> Result.bind (fun () ->
                requireUnique state uniqueness.RequiredInputs |> Result.bind (fun () ->
                    drop state consumes
                    |> Result.bind (fun state -> define uniqueness.UniqueOutputs declared state contract.Outputs)))
    let leaf declared borrowed state operation =
        contract declared borrowed state (semantics.Leaf operation) (semantics.LeafUniqueness operation)
    let call declared borrowed state (call: HIR.FunctionCall) =
        match semantics.CallOwnership call with
        | None -> Error (UnknownCallOwnership call.Target)
        | Some signature when List.length signature.Parameters <> List.length call.Arguments ->
            Error (InconsistentCallOwnershipParameters call.Target)
        | Some signature ->
            let arguments = call.Arguments |> List.map managed
            let rec inputs index acc required modes values =
                match modes, values with
                | [], [] -> Ok (List.rev acc, required)
                | UnmanagedCallParameter :: modes, None :: values -> inputs (index + 1) acc required modes values
                | BorrowedCallParameter :: modes, Some id :: values -> inputs (index + 1) (Borrowed id :: acc) required modes values
                | ConsumedCallParameter :: modes, Some id :: values -> inputs (index + 1) (Consumed id :: acc) required modes values
                | UniqueCallParameter :: modes, Some id :: values ->
                    inputs (index + 1) (Consumed id :: acc) (Set.add id required) modes values
                | _ -> Error (InconsistentCallOwnershipArgument (call.Target, index))
            inputs 0 [] Set.empty signature.Parameters arguments |> Result.bind (fun (callInputs, required) ->
                let ownership outputs uniqueOutputs =
                    contract declared borrowed state
                        { Inputs = callInputs; Outputs = outputs }
                        { RequiredInputs = required; UniqueOutputs = uniqueOutputs }
                match signature.Result, managed call.Result with
                | UnmanagedCallResult, None -> ownership [] Set.empty
                | ProducedCallResult, Some result -> ownership [result] Set.empty
                | UniqueProducedCallResult, Some result -> ownership [result] (Set.singleton result)
                | BorrowedCallResult index, Some result when index >= 0 ->
                    match List.tryItem index signature.Parameters, List.tryItem index arguments with
                    | Some BorrowedCallParameter, Some (Some source) when source = result ->
                        ownership [] Set.empty
                    | Some BorrowedCallParameter, Some (Some _) -> Error (InconsistentCallOwnershipResult call.Target)
                    | _ -> Error (InvalidBorrowedCallResult (call.Target, index))
                | BorrowedCallResult index, Some _ -> Error (InvalidBorrowedCallResult (call.Target, index))
                | _ -> Error (InconsistentCallOwnershipResult call.Target))
    let rec loop declared borrowed state = function
        | [] -> Ok (declared, state)
        | step :: rest ->
            let after =
                match step with
                | Dup value ->
                    require borrowed state (Set.singleton value)
                    |> Result.map (fun () -> declared, addUnit value state)
                | Drop value -> drop state [value] |> Result.map (fun state -> declared, state)
                | Evaluate operation ->
                    match operation with
                    | HIR.Leaf leafOperation -> leaf declared borrowed state leafOperation
                    | HIR.ScalarBinding (_, value) -> scalar borrowed state value |> Result.map (fun state -> declared, state)
                    | HIR.Call functionCall -> call declared borrowed state functionCall
                    | HIR.Branch (result, condition, yes, no) ->
                        scalar borrowed state condition |> Result.bind (fun branchState ->
                            block declared borrowed branchState yes |> Result.bind (fun (afterYes, yesState, yesResult) ->
                                block afterYes borrowed branchState no |> Result.bind (fun (afterNo, noState, noResult) ->
                                    match managed yesResult, managed noResult, managed result with
                                    | None, None, None ->
                                        if yesState <> noState then Error InconsistentJoin
                                        else Ok (afterNo, yesState)
                                    | Some yesId, Some noId, Some resultId ->
                                        let uniqueResult = isUnique yesState yesId && isUnique noState noId
                                        drop yesState [yesId] |> Result.bind (fun yesRemainder ->
                                            drop noState [noId] |> Result.bind (fun noRemainder ->
                                                if yesRemainder <> noRemainder then Error InconsistentJoin
                                                else
                                                    let unique = if uniqueResult then Set.singleton resultId else Set.empty
                                                    define unique afterNo yesRemainder [resultId]))
                                    | _ -> Error InconsistentBlockArgument)))
            after |> Result.bind (fun (declared, state) -> loop declared borrowed state rest)
    and block declared borrowed state body =
        loop declared borrowed state body.Body.Operations |> Result.bind (fun (declared, state) ->
            requireManaged borrowed state body.Body.Result
            |> Result.map (fun () -> declared, state, body.Body.Result))
    let parameterId = function
        | BorrowedParameter id | ConsumedParameter id | UniqueParameter id -> id
    let parameterIds = signature.Parameters |> List.map parameterId
    let duplicateParameter =
        parameterIds
        |> List.countBy id
        |> List.tryFind (fun (_, count) -> count > 1)
        |> Option.map fst
    let managedParameters =
        root.Body.Parameters
        |> List.choose (fun parameter -> managed parameter.Value)
    match duplicateParameter with
    | Some id -> Error (DuplicateParameter id)
    | None when Set.ofList parameterIds <> Set.ofList managedParameters
                || List.length parameterIds <> List.length managedParameters ->
        Error InconsistentFunctionParameters
    | None ->
        let borrowed =
            signature.Parameters
            |> List.choose (function
                | BorrowedParameter id -> Some id
                | ConsumedParameter _ | UniqueParameter _ -> None)
            |> Set.ofList
        let ownedParameters =
            signature.Parameters
            |> List.choose (function
                | ConsumedParameter id | UniqueParameter id -> Some id
                | BorrowedParameter _ -> None)
        let initial =
            ownedParameters
            |> List.fold (fun state id -> addUnit id state) { Units = Map.empty; Exclusive = Set.empty }
        let initial =
            signature.Parameters
            |> List.choose (function UniqueParameter id -> Some id | _ -> None)
            |> List.fold (fun state id -> { state with Exclusive = Set.add id state.Exclusive }) initial
        block (Set.ofList parameterIds) borrowed initial root
        |> Result.bind (fun (_, state, result) ->
            let resultOwnership =
                match signature.Result, managed result with
                | UnmanagedResult, None -> Ok state
                | BorrowedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | BorrowedResult expected, Some _ when Set.contains expected borrowed -> Ok state
                | BorrowedResult expected, Some _ -> Error (InvalidBorrowedResult expected)
                | ProducedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | UniqueProducedResult expected, Some actual when expected <> actual -> Error InconsistentFunctionResult
                | UniqueProducedResult expected, Some _ ->
                    requireUnique state (Set.singleton expected) |> Result.bind (fun () ->
                        match drop state [expected] with
                        | Ok state -> Ok state
                        | Error _ -> Error (InvalidProducedResult expected))
                | ProducedResult expected, Some _ ->
                    match drop state [expected] with
                    | Ok state -> Ok state
                    | Error _ -> Error (InvalidProducedResult expected)
                | _ -> Error InconsistentFunctionResult
            resultOwnership |> Result.bind (fun state ->
                if Map.isEmpty state.Units then Ok () else Error (UndroppedValues (owned state))))

/// Closed regions are functions with no managed parameters and an unmanaged
/// result. Keeping this as a wrapper makes the existing boundary explicit.
let verifyClosed (semantics: Semantics<'leaf, 'id>) (root: Block<'leaf, 'id>) =
    verifyFunction semantics { Parameters = []; Result = UnmanagedResult } root
