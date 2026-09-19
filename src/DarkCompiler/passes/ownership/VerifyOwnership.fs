// VerifyOwnership.fs - Verify ownership and derive call facts from the same state transitions.

module VerifyOwnership

open OwnedIR

type private OwnershipState<'id when 'id: comparison> = {
    Units: Map<'id, int>
    Exclusive: Set<'id>
}

let private parameterId = function
    | UnmanagedParameter -> None
    | BorrowedParameter id | ConsumedParameter id | UniqueParameter id -> Some id

/// Translate a verified function boundary into the positional ownership
/// contract used by calls. Borrowed results name their unique borrowed source
/// position; produced results do not expose the callee-local result identity.
let callSignatureOfFunction (signature: FunctionSignature<'id>) =
    let parameterIds = signature.Parameters |> List.choose parameterId
    let duplicateParameter =
        parameterIds
        |> List.countBy id
        |> List.tryFind (fun (_, count) -> count > 1)
        |> Option.map fst
    match duplicateParameter with
    | Some id -> Error (DuplicateParameter id)
    | None ->
        let parameters =
            signature.Parameters
            |> List.map (function
                | UnmanagedParameter -> UnmanagedCallParameter
                | BorrowedParameter _ -> BorrowedCallParameter
                | ConsumedParameter _ -> ConsumedCallParameter
                | UniqueParameter _ -> UniqueCallParameter)
        let result =
            match signature.Result with
            | UnmanagedResult -> Ok UnmanagedCallResult
            | ProducedResult _ -> Ok ProducedCallResult
            | UniqueProducedResult _ -> Ok UniqueProducedCallResult
            | BorrowedResult source ->
                let sources =
                    signature.Parameters
                    |> List.indexed
                    |> List.choose (fun (index, parameter) ->
                        match parameter with
                        | BorrowedParameter id when id = source -> Some index
                        | _ -> None)
                match sources with
                | [index] -> Ok (BorrowedCallResult index)
                | _ -> Error (InvalidBorrowedResult source)
        result |> Result.map (fun result -> { Parameters = parameters; Result = result })

/// Definitions are globally fresh even across mutually exclusive branches;
/// live ownership is path-local and must agree at every shared continuation.
/// The function signature owns boundary transfer; primitive contracts continue
/// to own operation-local use and production without duplicating alias facts.
let private foldFunctionCalls
    observeCall
    initialFacts
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
    let call facts declared borrowed state (call: HIR.FunctionCall) =
        match semantics.CallOwnership call with
        | None -> Error (UnknownCallOwnership call.Target)
        | Some signature when List.length signature.Parameters <> List.length call.Arguments ->
            Error (InconsistentCallOwnershipParameters call.Target)
        | Some signature ->
            let arguments = call.Arguments |> List.map managed
            let occurrences = arguments |> List.choose id |> List.countBy id |> Map.ofList
            let uniqueArgumentId id = isUnique state id && Map.tryFind id occurrences = Some 1
            let rec inputs index acc required modes values =
                match modes, values with
                | [], [] -> Ok (List.rev acc, required)
                | UnmanagedCallParameter :: modes, None :: values -> inputs (index + 1) acc required modes values
                | BorrowedCallParameter :: modes, Some id :: values -> inputs (index + 1) (Borrowed id :: acc) required modes values
                | ConsumedCallParameter :: modes, Some id :: values -> inputs (index + 1) (Consumed id :: acc) required modes values
                | UniqueCallParameter :: modes, Some id :: values ->
                    if uniqueArgumentId id then
                        inputs (index + 1) (Consumed id :: acc) (Set.add id required) modes values
                    else Error (NonUniqueUse id)
                | _ -> Error (InconsistentCallOwnershipArgument (call.Target, index))
            inputs 0 [] Set.empty signature.Parameters arguments |> Result.bind (fun (callInputs, required) ->
                let ownership outputs uniqueOutputs =
                    contract declared borrowed state
                        { Inputs = callInputs; Outputs = outputs }
                        { RequiredInputs = required; UniqueOutputs = uniqueOutputs }
                match signature.Result, managed call.Result with
                | UnmanagedCallResult, None -> ownership [] Set.empty
                | ProducedCallResult, Some result ->
                    ownership [result] Set.empty
                    |> Result.map (fun (declared, nextState) ->
                        // An ordinary result may alias any input, including a
                        // consumed unit with duplicates still live in the caller.
                        let mayAlias = arguments |> List.choose id |> Set.ofList
                        declared, { nextState with Exclusive = Set.difference nextState.Exclusive mayAlias })
                | UniqueProducedCallResult, Some result -> ownership [result] (Set.singleton result)
                | BorrowedCallResult index, Some result when index >= 0 ->
                    match List.tryItem index signature.Parameters, List.tryItem index arguments with
                    | Some BorrowedCallParameter, Some (Some source) when source = result ->
                        ownership [] Set.empty
                    | Some BorrowedCallParameter, Some (Some _) -> Error (InconsistentCallOwnershipResult call.Target)
                    | _ -> Error (InvalidBorrowedCallResult (call.Target, index))
                | BorrowedCallResult index, Some _ -> Error (InvalidBorrowedCallResult (call.Target, index))
                | _ -> Error (InconsistentCallOwnershipResult call.Target))
            |> Result.map (fun (declared, nextState) ->
                let uniqueArgument value =
                    match managed value with
                    | Some id -> uniqueArgumentId id
                    | None -> false
                declared, nextState, observeCall facts call signature uniqueArgument)
    let withFacts facts result =
        result |> Result.map (fun (declared, state) -> declared, state, facts)
    let rec loop facts declared borrowed state = function
        | [] -> Ok (declared, state, facts)
        | step :: rest ->
            let after =
                match step with
                | Dup value ->
                    require borrowed state (Set.singleton value)
                    |> Result.map (fun () -> declared, addUnit value state, facts)
                | Drop value -> drop state [value] |> Result.map (fun state -> declared, state, facts)
                | Evaluate operation ->
                    match operation with
                    | HIR.Leaf leafOperation -> leaf declared borrowed state leafOperation |> withFacts facts
                    | HIR.ScalarBinding (result, value) ->
                        scalar borrowed state value |> Result.bind (fun state ->
                            match managed result with
                            | None -> Ok (declared, state)
                            | Some id -> define Set.empty declared state [id])
                        |> withFacts facts
                    | HIR.Call functionCall -> call facts declared borrowed state functionCall
                    | HIR.Branch (result, condition, yes, no) ->
                        scalar borrowed state condition |> Result.bind (fun branchState ->
                            block facts declared borrowed branchState yes |> Result.bind (fun (afterYes, yesState, yesResult, yesFacts) ->
                                block yesFacts afterYes borrowed branchState no |> Result.bind (fun (afterNo, noState, noResult, noFacts) ->
                                    let joined =
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
                                        | _ -> Error InconsistentBlockArgument
                                    joined |> withFacts noFacts)))
            after |> Result.bind (fun (declared, state, facts) -> loop facts declared borrowed state rest)
    and block facts declared borrowed state body =
        loop facts declared borrowed state body.Body.Operations |> Result.bind (fun (declared, state, facts) ->
            requireManaged borrowed state body.Body.Result
            |> Result.map (fun () -> declared, state, body.Body.Result, facts))
    let parameterIds = signature.Parameters |> List.choose parameterId
    let duplicateParameter =
        parameterIds
        |> List.countBy id
        |> List.tryFind (fun (_, count) -> count > 1)
        |> Option.map fst
    let rec parametersMatch ownership (parameters: HIR.Parameter list) =
        match ownership, parameters with
        | [], [] -> true
        | UnmanagedParameter :: ownership, parameter :: parameters ->
            managed parameter.Value = None && parametersMatch ownership parameters
        | (BorrowedParameter expected | ConsumedParameter expected | UniqueParameter expected) :: ownership,
          parameter :: parameters ->
            managed parameter.Value = Some expected && parametersMatch ownership parameters
        | _ -> false
    match duplicateParameter with
    | Some id -> Error (DuplicateParameter id)
    | None when not (parametersMatch signature.Parameters root.Body.Parameters) ->
        Error InconsistentFunctionParameters
    | None ->
        let borrowed =
            signature.Parameters
            |> List.choose (function
                | BorrowedParameter id -> Some id
                | UnmanagedParameter | ConsumedParameter _ | UniqueParameter _ -> None)
            |> Set.ofList
        let ownedParameters =
            signature.Parameters
            |> List.choose (function
                | ConsumedParameter id | UniqueParameter id -> Some id
                | UnmanagedParameter | BorrowedParameter _ -> None)
        let initial =
            ownedParameters
            |> List.fold (fun state id -> addUnit id state) { Units = Map.empty; Exclusive = Set.empty }
        let initial =
            signature.Parameters
            |> List.choose (function UniqueParameter id -> Some id | _ -> None)
            |> List.fold (fun state id -> { state with Exclusive = Set.add id state.Exclusive }) initial
        block initialFacts (Set.ofList parameterIds) borrowed initial root
        |> Result.bind (fun (_, state, result, facts) ->
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
                if Map.isEmpty state.Units then Ok facts else Error (UndroppedValues (owned state))))

let verifyFunction semantics signature root =
    foldFunctionCalls (fun () _ _ _ -> ()) () semantics signature root

let private collectCallFacts caller facts (call: HIR.FunctionCall) established isUnique =
    let uniqueArguments =
        call.Arguments
        |> List.indexed
        |> List.choose (fun (index, argument) -> if isUnique argument then Some index else None)
        |> Set.ofList
    { Caller = caller; Call = call; Established = established; UniqueArguments = uniqueArguments } :: facts

/// Return facts only after the complete function, including its return and
/// cleanup, verifies. Verification-only callers do not allocate fact lists.
let analyzeFunction semantics (definition: Function<'leaf, 'id>) =
    foldFunctionCalls (collectCallFacts definition.Definition.Id) []
        semantics definition.Ownership definition.Definition.Body
    |> Result.map List.rev

/// Verify a mutually visible owned-function group. Internal call ownership is
/// derived from the paired definitions, so direct and recursive calls cannot
/// drift from their function boundaries. External registrations remain
/// available only for targets outside the group.
let private withFunctionSemantics
    (semantics: Semantics<'leaf, 'id>)
    (functions: Function<'leaf, 'id> list)
    analyze =
    let duplicateName =
        functions
        |> List.countBy (fun functionDefinition -> functionDefinition.Definition.Id)
        |> List.tryFind (fun (_, count) -> count > 1)
        |> Option.map fst
    match duplicateName with
    | Some name -> Error (DuplicateFunctionName name)
    | None ->
        let derived =
            functions
            |> List.fold (fun result functionDefinition ->
                result
                |> Result.bind (fun signatures ->
                    callSignatureOfFunction functionDefinition.Ownership
                    |> Result.map (fun signature -> (functionDefinition, signature) :: signatures))) (Ok [])
            |> Result.map List.rev
        derived |> Result.bind (fun signatures ->
            let conflictingRegistration =
                signatures
                |> List.tryPick (fun (functionDefinition, expected) ->
                    let body = functionDefinition.Definition.Body.Body
                    let boundaryCall : HIR.FunctionCall = {
                        Target = functionDefinition.Definition.Id
                        Arguments = body.Parameters |> List.map (fun parameter -> parameter.Value)
                        Result = body.Result
                    }
                    match semantics.CallOwnership boundaryCall with
                    | Some registered when registered <> expected ->
                        Some functionDefinition.Definition.Id
                    | _ -> None)
            match conflictingRegistration with
            | Some target -> Error (InconsistentRegisteredCallOwnership target)
            | None ->
                let registry =
                    signatures
                    |> List.map (fun (functionDefinition, signature) ->
                        functionDefinition.Definition.Id, signature)
                    |> Map.ofList
                let programSemantics = {
                    semantics with
                        CallOwnership = fun call ->
                            match Map.tryFind call.Target registry with
                            | Some signature -> Some signature
                            | None -> semantics.CallOwnership call
                }
                analyze programSemantics)

let verifyFunctions semantics functions =
    withFunctionSemantics semantics functions (fun programSemantics ->
        functions
        |> List.fold (fun result definition ->
            result |> Result.bind (fun () ->
                verifyFunction programSemantics definition.Ownership definition.Definition.Body)) (Ok ()))

/// Internal and recursive calls use boundaries derived from this exact group.
/// The accumulator is separate from path-local ownership: sibling branches
/// contribute facts without contributing ownership state to one another.
let analyzeFunctions semantics functions =
    withFunctionSemantics semantics functions (fun programSemantics ->
        functions
        |> List.fold (fun result definition ->
            result |> Result.bind (fun facts ->
                foldFunctionCalls (collectCallFacts definition.Definition.Id) facts
                    programSemantics definition.Ownership definition.Definition.Body)) (Ok [])
        |> Result.map List.rev)

/// Closed regions are functions with no managed parameters and an unmanaged
/// result. Keeping this as a wrapper makes the existing boundary explicit.
let verifyClosed (semantics: Semantics<'leaf, 'id>) (root: Block<'leaf, 'id>) =
    let parameters =
        root.Body.Parameters
        |> List.map (fun _ -> UnmanagedParameter)
    verifyFunction semantics { Parameters = parameters; Result = UnmanagedResult } root
