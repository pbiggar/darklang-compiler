(*
   Translate a verified function boundary into the positional ownership
   contract used by calls. Borrowed results name their unique borrowed source
   position; produced results do not expose the callee-local result identity.
   Definitions are globally fresh even across mutually exclusive branches;
   live ownership is path-local and must agree at every shared continuation.
   The function signature owns boundary transfer; primitive contracts continue
   to own operation-local use and production without duplicating alias facts.
   An ordinary result may alias any input, including a
   consumed unit with duplicates still live in the caller.
   Return facts only after the complete function, including its return and
   cleanup, verifies. Verification-only callers do not allocate fact lists.
   Verify a mutually visible owned-function group. Internal call ownership is
   derived from the paired definitions, so direct and recursive calls cannot
   drift from their function boundaries. External registrations remain
   available only for targets outside the group.
   Internal and recursive calls use boundaries derived from this exact group.
   The accumulator is separate from path-local ownership: sibling branches
   contribute facts without contributing ownership state to one another.
   Closed regions are functions with no managed parameters and an unmanaged
   result. Keeping this as a wrapper makes the existing boundary explicit.
*)
(* VerifyOwnership.ml - Verify ownership and derive call facts from the same state transitions. *)
[@@@warning "-4"]

module Make (Identity : OwnedIR.Identity) = struct
  module Ownership = OwnedIR.Make (Identity)
  module O = OwnedIR
  module V = Ownership
  module H = HIR
  module S = Identity.Set

  module M = Map.Make (struct
    type t = Identity.t

    let compare = Identity.compare
  end)

  type ownershipState = { units : int M.t; exclusive : S.t }

  let ( let* ) = Result.bind
  let equal left right = Identity.compare left right = 0

  let add left right =
    Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))

  let parameterId = function
    | O.UnmanagedParameter -> None
    | O.BorrowedParameter id | O.ConsumedParameter id | O.UniqueParameter id ->
        Some id

  let counts values =
    List.fold_left
      (fun counts value ->
        M.update value
          (fun count -> Some (add (Option.value count ~default:0) 1))
          counts)
      M.empty values

  let duplicate values =
    let counts = counts values in
    List.find_opt (fun value -> M.find value counts > 1) values

  let callSignatureOfFunction (signature : Identity.t O.functionSignature) =
    let ids = List.filter_map parameterId signature.O.parameters in
    match duplicate ids with
    | Some id -> Error (V.DuplicateParameter id)
    | None ->
        let parameters =
          List.map
            (function
              | O.UnmanagedParameter -> O.UnmanagedCallParameter
              | O.BorrowedParameter _ -> O.BorrowedCallParameter
              | O.ConsumedParameter _ -> O.ConsumedCallParameter
              | O.UniqueParameter _ -> O.UniqueCallParameter)
            signature.O.parameters
        in
        let result =
          match signature.O.result with
          | O.UnmanagedResult -> Ok O.UnmanagedCallResult
          | O.ProducedResult _ -> Ok O.ProducedCallResult
          | O.UniqueProducedResult _ -> Ok O.UniqueProducedCallResult
          | O.BorrowedResult source -> (
              let sources =
                List.mapi
                  (fun index param -> (index, param))
                  signature.O.parameters
                |> List.filter_map (fun (index, param) ->
                    match param with
                    | O.BorrowedParameter id when equal id source -> Some index
                    | _ -> None)
              in
              match sources with
              | [ index ] -> Ok (O.BorrowedCallResult index)
              | _ -> Error (V.InvalidBorrowedResult source))
        in
        let* result = result in
        Ok ({ O.parameters; result } : O.callSignature)

  let foldFunctionCalls observe initialFacts (semantics : 'leaf V.semantics)
      (signature : Identity.t O.functionSignature)
      (root : ('leaf, Identity.t) O.block) =
    let addUnit value state =
      {
        state with
        units =
          M.update value
            (fun count -> Some (add (Option.value count ~default:0) 1))
            state.units;
      }
    in
    let rec drop state = function
      | [] -> Ok state
      | value :: rest -> (
          match M.find_opt value state.units with
          | Some 1 ->
              drop
                {
                  units = M.remove value state.units;
                  exclusive = S.remove value state.exclusive;
                }
                rest
          | Some count ->
              drop
                { state with units = M.add value (add count (-1)) state.units }
                rest
          | None -> Error (V.InvalidDrop value))
    in
    let rec define unique declared state = function
      | [] -> Ok (declared, state)
      | value :: _ when S.mem value declared ->
          Error (V.DuplicateDefinition value)
      | value :: rest ->
          let next = addUnit value state in
          let next =
            if S.mem value unique then
              { next with exclusive = S.add value next.exclusive }
            else next
          in
          define unique (S.add value declared) next rest
    in
    let owned state = S.of_list (List.map fst (M.bindings state.units)) in
    let join yes no =
      if not (M.equal Int.equal yes.units no.units) then
        Error V.InconsistentJoin
      else
        Ok { units = yes.units; exclusive = S.inter yes.exclusive no.exclusive }
    in
    let require borrowed state uses =
      match S.elements (S.diff uses (S.union borrowed (owned state))) with
      | [] -> Ok ()
      | value :: _ -> Error (V.InvalidUse value)
    in
    let isUnique state value =
      S.mem value state.exclusive && M.find_opt value state.units = Some 1
    in
    let requireUnique state values =
      match
        List.find_opt
          (fun value -> not (isUnique state value))
          (S.elements values)
      with
      | None -> Ok ()
      | Some value -> Error (V.NonUniqueUse value)
    in
    let scalar borrowed state operand =
      let uses = semantics.V.scalarUses operand in
      let escapes = semantics.V.scalarEscapes operand in
      let* () = require borrowed state uses in
      match S.elements (S.diff escapes uses) with
      | value :: _ -> Error (V.InvalidUniquenessContract value)
      | [] -> Ok { state with exclusive = S.diff state.exclusive escapes }
    in
    let managed value =
      match semantics.V.blockArgument value with
      | O.Unmanaged -> None
      | O.Managed id -> Some id
    in
    let requireManaged borrowed state value =
      match managed value with
      | None -> Ok ()
      | Some id -> require borrowed state (S.singleton id)
    in
    let contract declared borrowed state (contract : Identity.t O.contract)
        (uniqueness : V.uniquenessContract) =
      let uses =
        S.of_list
          (List.map
             (function O.Borrowed id | O.Consumed id -> id)
             contract.O.inputs)
      in
      let consumes =
        List.filter_map
          (function O.Consumed id -> Some id | O.Borrowed _ -> None)
          contract.O.inputs
      in
      let outputs = S.of_list contract.O.outputs in
      let required = S.elements (S.diff uniqueness.V.requiredInputs uses) in
      let produced = S.elements (S.diff uniqueness.V.uniqueOutputs outputs) in
      match (required, produced) with
      | value :: _, _ | _, value :: _ ->
          Error (V.InvalidUniquenessContract value)
      | [], [] ->
          let* () = require borrowed state uses in
          let* () = requireUnique state uniqueness.V.requiredInputs in
          let* state = drop state consumes in
          define uniqueness.V.uniqueOutputs declared state contract.O.outputs
    in
    let leaf declared borrowed state operation =
      let contract_ = semantics.V.leaf operation in
      let uniqueness = semantics.V.leafUniqueness operation in
      contract declared borrowed state contract_ uniqueness
    in
    let call facts declared borrowed state (call : H.functionCall) =
      match semantics.V.callOwnership call with
      | None -> Error (V.UnknownCallOwnership call.H.target)
      | Some signature
        when List.length signature.O.parameters <> List.length call.H.arguments
        ->
          Error (V.InconsistentCallOwnershipParameters call.H.target)
      | Some signature ->
          let arguments = List.map managed call.H.arguments in
          let occurrences = counts (List.filter_map Fun.id arguments) in
          let uniqueArgumentId id =
            isUnique state id && M.find_opt id occurrences = Some 1
          in
          let rec inputs index acc required modes values =
            match (modes, values) with
            | [], [] -> Ok (List.rev acc, required)
            | O.UnmanagedCallParameter :: modes, None :: values ->
                inputs (add index 1) acc required modes values
            | O.BorrowedCallParameter :: modes, Some id :: values ->
                inputs (add index 1) (O.Borrowed id :: acc) required modes
                  values
            | O.ConsumedCallParameter :: modes, Some id :: values ->
                inputs (add index 1) (O.Consumed id :: acc) required modes
                  values
            | O.UniqueCallParameter :: modes, Some id :: values ->
                if uniqueArgumentId id then
                  inputs (add index 1) (O.Consumed id :: acc)
                    (S.add id required) modes values
                else Error (V.NonUniqueUse id)
            | _ ->
                Error
                  (V.InconsistentCallOwnershipArgument (call.H.target, index))
          in
          let* callInputs, required =
            inputs 0 [] S.empty signature.O.parameters arguments
          in
          let ownership outputs uniqueOutputs =
            contract declared borrowed state
              { O.inputs = callInputs; outputs }
              { V.requiredInputs = required; uniqueOutputs }
          in
          let argumentAt index values =
            if index < 0 then None else List.nth_opt values index
          in
          let* declared, next =
            match (signature.O.result, managed call.H.result) with
            | O.UnmanagedCallResult, None -> ownership [] S.empty
            | O.ProducedCallResult, Some result ->
                let* declared, next = ownership [ result ] S.empty in
                let mayAlias = S.of_list (List.filter_map Fun.id arguments) in
                Ok
                  ( declared,
                    { next with exclusive = S.diff next.exclusive mayAlias } )
            | O.UniqueProducedCallResult, Some result ->
                ownership [ result ] (S.singleton result)
            | O.BorrowedCallResult index, Some result when index >= 0 -> (
                match
                  ( argumentAt index signature.O.parameters,
                    argumentAt index arguments )
                with
                | Some O.BorrowedCallParameter, Some (Some source)
                  when equal source result ->
                    ownership [] S.empty
                | Some O.BorrowedCallParameter, Some (Some _) ->
                    Error (V.InconsistentCallOwnershipResult call.H.target)
                | _ ->
                    Error (V.InvalidBorrowedCallResult (call.H.target, index)))
            | O.BorrowedCallResult index, Some _ ->
                Error (V.InvalidBorrowedCallResult (call.H.target, index))
            | _ -> Error (V.InconsistentCallOwnershipResult call.H.target)
          in
          let uniqueArgument value =
            match managed value with
            | Some id -> uniqueArgumentId id
            | None -> false
          in
          Ok (declared, next, observe facts call signature uniqueArgument)
    in
    let withFacts facts result =
      let* declared, state = result in
      Ok (declared, state, facts)
    in
    let rec loop facts declared borrowed state = function
      | [] -> Ok (declared, state, facts)
      | step :: rest ->
          let after =
            match step with
            | O.Dup value ->
                let* () = require borrowed state (S.singleton value) in
                Ok (declared, addUnit value state, facts)
            | O.Drop value ->
                let* state = drop state [ value ] in
                Ok (declared, state, facts)
            | O.Evaluate operation -> (
                match operation with
                | H.Leaf operation ->
                    withFacts facts (leaf declared borrowed state operation)
                | H.ScalarBinding (result, value) ->
                    let after =
                      let* state = scalar borrowed state value in
                      match managed result with
                      | None -> Ok (declared, state)
                      | Some id -> define S.empty declared state [ id ]
                    in
                    withFacts facts after
                | H.Call functionCall ->
                    call facts declared borrowed state functionCall
                | H.Branch (result, condition, yes, no) ->
                    let* branchState = scalar borrowed state condition in
                    let* afterYes, yesState, yesResult, yesFacts =
                      block facts declared borrowed branchState yes
                    in
                    let* afterNo, noState, noResult, noFacts =
                      block yesFacts afterYes borrowed branchState no
                    in
                    let joined =
                      match
                        (managed yesResult, managed noResult, managed result)
                      with
                      | None, None, None ->
                          let* joined = join yesState noState in
                          Ok (afterNo, joined)
                      | Some yesId, Some noId, Some resultId ->
                          let unique =
                            isUnique yesState yesId && isUnique noState noId
                          in
                          let* yes = drop yesState [ yesId ] in
                          let* no = drop noState [ noId ] in
                          let* remainder = join yes no in
                          define
                            (if unique then S.singleton resultId else S.empty)
                            afterNo remainder [ resultId ]
                      | _ -> Error V.InconsistentBlockArgument
                    in
                    withFacts noFacts joined)
          in
          let* declared, state, facts = after in
          loop facts declared borrowed state rest
    and block facts declared borrowed state (body : ('leaf, Identity.t) O.block)
        =
      let* declared, state, facts =
        loop facts declared borrowed state body.O.body.H.operations
      in
      let* () = requireManaged borrowed state body.O.body.H.result in
      Ok (declared, state, body.O.body.H.result, facts)
    in
    let ids = List.filter_map parameterId signature.O.parameters in
    let rec parametersMatch ownership parameters =
      match (ownership, parameters) with
      | [], [] -> true
      | O.UnmanagedParameter :: rest, (param : H.parameter) :: params ->
          managed param.H.value = None && parametersMatch rest params
      | ( ( O.BorrowedParameter expected
          | O.ConsumedParameter expected
          | O.UniqueParameter expected )
          :: rest,
          (param : H.parameter) :: params ) ->
          (match managed param.H.value with
            | Some actual -> equal expected actual
            | None -> false)
          && parametersMatch rest params
      | _ -> false
    in
    match duplicate ids with
    | Some id -> Error (V.DuplicateParameter id)
    | None
      when not (parametersMatch signature.O.parameters root.O.body.H.parameters)
      ->
        Error V.InconsistentFunctionParameters
    | None ->
        let borrowed =
          S.of_list
            (List.filter_map
               (function O.BorrowedParameter id -> Some id | _ -> None)
               signature.O.parameters)
        in
        let ownedParameters =
          List.filter_map
            (function
              | O.ConsumedParameter id | O.UniqueParameter id -> Some id
              | _ -> None)
            signature.O.parameters
        in
        let initial =
          List.fold_left
            (fun state id -> addUnit id state)
            { units = M.empty; exclusive = S.empty }
            ownedParameters
        in
        let initial =
          List.filter_map
            (function O.UniqueParameter id -> Some id | _ -> None)
            signature.O.parameters
          |> List.fold_left
               (fun state id ->
                 { state with exclusive = S.add id state.exclusive })
               initial
        in
        let* _, state, result, facts =
          block initialFacts (S.of_list ids) borrowed initial root
        in
        let resultOwnership =
          match (signature.O.result, managed result) with
          | O.UnmanagedResult, None -> Ok state
          | O.BorrowedResult expected, Some actual
            when not (equal expected actual) ->
              Error V.InconsistentFunctionResult
          | O.BorrowedResult expected, Some _ when S.mem expected borrowed ->
              Ok state
          | O.BorrowedResult expected, Some _ ->
              Error (V.InvalidBorrowedResult expected)
          | O.ProducedResult expected, Some actual
            when not (equal expected actual) ->
              Error V.InconsistentFunctionResult
          | O.UniqueProducedResult expected, Some actual
            when not (equal expected actual) ->
              Error V.InconsistentFunctionResult
          | O.UniqueProducedResult expected, Some _ -> (
              let* () = requireUnique state (S.singleton expected) in
              match drop state [ expected ] with
              | Ok state -> Ok state
              | Error _ -> Error (V.InvalidProducedResult expected))
          | O.ProducedResult expected, Some _ -> (
              match drop state [ expected ] with
              | Ok state -> Ok state
              | Error _ -> Error (V.InvalidProducedResult expected))
          | _ -> Error V.InconsistentFunctionResult
        in
        let* state = resultOwnership in
        if M.is_empty state.units then Ok facts
        else Error (V.UndroppedValues (owned state))

  let verifyFunction semantics signature root =
    foldFunctionCalls (fun () _ _ _ -> ()) () semantics signature root

  let collectCallFacts caller facts (call : H.functionCall) established unique =
    let uniqueArguments =
      List.mapi (fun index argument -> (index, argument)) call.H.arguments
      |> List.filter_map (fun (index, argument) ->
          if unique argument then Some index else None)
      |> O.IntSet.of_list
    in
    { O.caller; call; established; uniqueArguments } :: facts

  let analyzeFunction semantics (definition : ('leaf, Identity.t) O.functionDef)
      =
    Result.map List.rev
      (foldFunctionCalls
         (collectCallFacts definition.O.definition.H.id)
         [] semantics definition.O.ownership definition.O.definition.H.body)

  let withFunctionSemantics (semantics : 'leaf V.semantics)
      (functions : ('leaf, Identity.t) O.functionDef list) analyze =
    let counts =
      List.fold_left
        (fun counts definition ->
          let id = definition.O.definition.H.id in
          FunctionIdMap.add id
            (Option.value (FunctionIdMap.tryFind id counts) ~default:0 + 1)
            counts)
        FunctionIdMap.empty functions
    in
    let duplicate =
      List.find_opt
        (fun definition ->
          FunctionIdMap.find definition.O.definition.H.id counts > 1)
        functions
    in
    match duplicate with
    | Some definition ->
        Error (V.DuplicateFunctionName definition.O.definition.H.id)
    | None -> (
        let* signatures =
          List.fold_left
            (fun result definition ->
              let* signatures = result in
              let* signature = callSignatureOfFunction definition.O.ownership in
              Ok ((definition, signature) :: signatures))
            (Ok []) functions
          |> Result.map List.rev
        in
        let conflicting =
          List.find_map
            (fun (definition, expected) ->
              let body = definition.O.definition.H.body.O.body in
              let boundary : H.functionCall =
                {
                  H.target = definition.O.definition.H.id;
                  arguments =
                    List.map
                      (fun (param : H.parameter) -> param.H.value)
                      body.H.parameters;
                  result = body.H.result;
                }
              in
              match semantics.V.callOwnership boundary with
              | Some registered when registered <> expected ->
                  Some definition.O.definition.H.id
              | _ -> None)
            signatures
        in
        match conflicting with
        | Some target -> Error (V.InconsistentRegisteredCallOwnership target)
        | None ->
            let registry =
              FunctionIdMap.ofList
                (List.map
                   (fun (definition, signature) ->
                     (definition.O.definition.H.id, signature))
                   signatures)
            in
            let program =
              {
                semantics with
                V.callOwnership =
                  (fun call ->
                    match FunctionIdMap.tryFind call.H.target registry with
                    | Some signature -> Some signature
                    | None -> semantics.V.callOwnership call);
              }
            in
            analyze program)

  let verifyFunctions semantics functions =
    withFunctionSemantics semantics functions (fun program ->
        List.fold_left
          (fun result definition ->
            let* () = result in
            verifyFunction program definition.O.ownership
              definition.O.definition.H.body)
          (Ok ()) functions)

  let analyzeFunctions semantics functions =
    withFunctionSemantics semantics functions (fun program ->
        List.fold_left
          (fun result definition ->
            let* facts = result in
            foldFunctionCalls
              (collectCallFacts definition.O.definition.H.id)
              facts program definition.O.ownership
              definition.O.definition.H.body)
          (Ok []) functions
        |> Result.map List.rev)

  let verifyClosed semantics (root : ('leaf, Identity.t) O.block) =
    let parameters =
      List.map (fun _ -> O.UnmanagedParameter) root.O.body.H.parameters
    in
    verifyFunction semantics { O.parameters; result = O.UnmanagedResult } root
end
