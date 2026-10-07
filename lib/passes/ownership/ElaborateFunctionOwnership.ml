(* ElaborateFunctionOwnership.fs - Infer conservative boundaries and place whole-function ownership steps. *)
[@@@warning "-4"]
module H = HIR
module O = OwnedIR
module F = FunctionIdMap
module S = H.ValueSet
module FS = SpecializationIdentity.FunctionSet
module V = VerifyOwnership.Make (ListLiveness.Identity)
module Ownership = V.Ownership
let ( let* ) = Result.bind
type ('leaf, 'block) dialect = {body : 'block -> ('leaf, 'block) H.operation H.block; leafOwnership : 'leaf -> H.valueId O.contract; leafUniqueness : 'leaf -> Ownership.uniquenessContract; isManaged : H.value -> bool; externalCallOwnership : H.functionCall -> O.callSignature option}
type elaborationError = UnknownCallOwnership of AST.functionId | InconsistentCallParameters of AST.functionId | InvalidFunctionBoundary of AST.functionId * Ownership.verificationError
type 'leaf analysis = {functions : ('leaf, H.valueId) O.functionDef list; semantics : 'leaf Ownership.semantics}
let functions analysis = analysis.functions
let semantics analysis = analysis.semantics
let managedId dialect (value : H.value) = if dialect.isManaged value then Some value.H.id else None
let unmanagedOr mode dialect value = match managedId dialect value with Some id -> mode id | None -> O.UnmanagedParameter
let initialBoundary dialect (definition : 'block H.functionDef) : H.valueId O.functionSignature =
 let block = dialect.body definition.H.body in
 {O.parameters = List.map (fun (parameter : H.parameter) -> unmanagedOr (fun id -> O.ConsumedParameter id) dialect parameter.H.value) block.H.parameters;
  result = (match managedId dialect block.H.result with Some id -> O.ProducedResult id | None -> O.UnmanagedResult)}
let signatureRegistry (boundaries : H.valueId O.functionSignature F.t) =
 List.fold_left (fun result (target, boundary) -> let* registry = result in match V.callSignatureOfFunction boundary with
 | Error error -> Error (InvalidFunctionBoundary (target, error)) | Ok signature -> Ok (F.add target signature registry)) (Ok F.empty) (F.toList boundaries)
let resolveCall externalOwnership registry (call : H.functionCall) = match F.tryFind call.H.target registry with Some signature -> Some signature | None -> externalOwnership call
let callInputs dialect (signature : O.callSignature) (call : H.functionCall) =
 if List.length signature.O.parameters <> List.length call.H.arguments then Error (InconsistentCallParameters call.H.target)
 else List.fold_left (fun result (mode, value) -> let* borrowed, consumed = result in match mode, managedId dialect value with
 | O.UnmanagedCallParameter, None -> Ok (borrowed, consumed)
 | O.BorrowedCallParameter, Some id -> Ok (S.add id borrowed, consumed)
 | (O.ConsumedCallParameter | O.UniqueCallParameter), Some id -> Ok (borrowed, id :: consumed)
 | _ -> Error (InconsistentCallParameters call.H.target)) (Ok (S.empty, [])) (List.combine signature.O.parameters call.H.arguments)
let inferBoundary dialect ownership (definition : 'block H.functionDef) =
 let rec demandBlock demanded block =
  let body = dialect.body block in
  List.fold_right (fun operation result -> let* demanded = result in match operation with
  | H.Leaf leaf -> let contract = dialect.leafOwnership leaf in
    let definitions = S.of_list contract.O.outputs in
    let consumed = S.of_list (List.filter_map (function O.Consumed id -> Some id | O.Borrowed _ -> None) contract.O.inputs) in
    Ok (S.union consumed (S.diff demanded definitions))
  | H.ScalarBinding (output, _) -> Ok (S.diff demanded (S.of_list (Option.to_list (managedId dialect output))))
  | H.Call call -> (match ownership call with None -> Error (UnknownCallOwnership call.H.target) | Some (signature : O.callSignature) ->
    let* _, consumed = callInputs dialect signature call in
    let definitions = match signature.O.result, managedId dialect call.H.result with (O.ProducedCallResult | O.UniqueProducedCallResult), Some id -> S.singleton id | _ -> S.empty in
    Ok (S.union (S.of_list consumed) (S.diff demanded definitions)))
  | H.Branch (output, _, yes, no) ->
    let continuation = match managedId dialect output with Some id -> S.remove id demanded | None -> demanded in
    let branchDemand branch = let body = dialect.body branch in match managedId dialect body.H.result with Some id -> S.add id continuation | None -> continuation in
    let* yesDemand = demandBlock (branchDemand yes) yes in
    let* noDemand = demandBlock (branchDemand no) no in Ok (S.union yesDemand noDemand)) body.H.operations (Ok demanded) in
 let body = dialect.body definition.H.body in
 let resultDemand = S.of_list (Option.to_list (managedId dialect body.H.result)) in
 let* demanded = demandBlock resultDemand definition.H.body in
 Ok ({O.parameters = List.map (fun (parameter : H.parameter) -> match managedId dialect parameter.H.value with
  | None -> O.UnmanagedParameter | Some id when S.mem id demanded -> O.ConsumedParameter id | Some id -> O.BorrowedParameter id) body.H.parameters;
  result = (match managedId dialect body.H.result with Some id -> O.ProducedResult id | None -> O.UnmanagedResult)} : H.valueId O.functionSignature)
let rec blockCalls dialect block = List.fold_left (fun calls -> function
 | H.Call call -> FS.add call.H.target calls
 | H.Branch (_, _, yes, no) -> let yes = blockCalls dialect yes in let no = blockCalls dialect no in FS.union calls (FS.union yes no)
 | H.Leaf _ | H.ScalarBinding _ -> calls) FS.empty (dialect.body block).H.operations
let convergeBoundaries dialect (definitions : 'block H.functionDef list) =
 let initial = F.ofList (List.map (fun (definition : 'block H.functionDef) -> definition.H.id, initialBoundary dialect definition) definitions) in
 let callsByFunction = F.ofList (List.map (fun (definition : 'block H.functionDef) -> definition.H.id, blockCalls dialect definition.H.body) definitions) in
 let groups = OwnedFunctionGroups.orderedFunctionIds (List.map (fun (definition : 'block H.functionDef) -> definition.H.id) definitions) (F.map (fun _ calls -> FS.elements calls) callsByFunction) in
 let definitionsById = F.ofList (List.map (fun (definition : 'block H.functionDef) -> definition.H.id, definition) definitions) in
 let boundaryState group boundaries = F.ofList (List.map (fun (definition : 'block H.functionDef) -> match F.tryFind definition.H.id boundaries with
  | Some boundary -> definition.H.id, boundary | None -> Crash.crash "Whole-function ownership group lost its boundary") group) in
 let updateSignatures boundaries signatures group = List.fold_left (fun result (definition : 'block H.functionDef) ->
  let* signatures = result in match F.tryFind definition.H.id boundaries with
  | None -> Crash.crash "Inferred ownership group lost its boundary"
  | Some boundary -> (match V.callSignatureOfFunction boundary with Error error -> Error (InvalidFunctionBoundary (definition.H.id, error)) | Ok signature -> Ok (F.add definition.H.id signature signatures))) (Ok signatures) group in
 let inferGroup (boundaries, signatures) group =
  let ownership = resolveCall dialect.externalCallOwnership signatures in
  let* nextBoundaries = List.fold_left (fun result (definition : 'block H.functionDef) -> let* inferred = result in
   let* boundary = inferBoundary dialect ownership definition in Ok (F.add definition.H.id boundary inferred)) (Ok boundaries) group in
  let* nextSignatures = updateSignatures nextBoundaries signatures group in Ok (nextBoundaries, nextSignatures) in
 let convergeGroup state group =
  let rec loop seen ((boundaries, _) as state) =
   let current = boundaryState group boundaries |> F.toList in
   if List.mem current seen then Crash.crash "Recursive whole-function ownership boundary inference did not converge"
   else let* ((nextBoundaries, _) as next) = inferGroup state group in
    if F.toList (boundaryState group nextBoundaries) = current then Ok next else loop (current :: seen) next in
  loop [] state in
 let* initialSignatures = signatureRegistry initial in
 let* boundaries, signatures = List.fold_left (fun result ids -> let* state = result in
  let group = List.map (fun id -> match F.tryFind id definitionsById with Some definition -> definition | None -> Crash.crash "Whole-function ownership group lost its definition") ids in
  match group with
  | [definition] -> let recursive = match F.tryFind definition.H.id callsByFunction with Some calls -> FS.mem definition.H.id calls | None -> Crash.crash "Whole-function ownership group lost its call set" in
    if recursive then convergeGroup state group else inferGroup state group
  | _ :: _ -> convergeGroup state group | [] -> Crash.crash "Whole-function ownership SCC discovery returned an empty group") (Ok (initial, initialSignatures)) groups in
 Ok (boundaries, resolveCall dialect.externalCallOwnership signatures)
let collectDefinitions dialect ownership (definition : 'block H.functionDef) =
 let rec block acc source = List.fold_left (fun result operation -> let* acc = result in
  let addManaged value = match managedId dialect value with Some id -> S.add id acc | None -> acc in
  match operation with
  | H.Leaf leaf -> Ok (S.union acc (S.of_list (dialect.leafOwnership leaf).O.outputs))
  | H.ScalarBinding (output, _) -> Ok (addManaged output)
  | H.Call call -> (match ownership call with None -> Error (UnknownCallOwnership call.H.target) | Some (signature : O.callSignature) ->
    match signature.O.result with O.ProducedCallResult | O.UniqueProducedCallResult -> Ok (addManaged call.H.result) | O.UnmanagedCallResult | O.BorrowedCallResult _ -> Ok acc)
  | H.Branch (output, _, yes, no) -> let* acc = block (addManaged output) yes in block acc no) (Ok acc) (dialect.body source).H.operations in
 block S.empty definition.H.body
let elaborateFunction dialect ownership (boundary : H.valueId O.functionSignature) (definition : 'block H.functionDef) =
 let consumedParameters = S.of_list (List.filter_map (function O.ConsumedParameter id | O.UniqueParameter id -> Some id | _ -> None) boundary.O.parameters) in
 let* definitions = collectDefinitions dialect ownership definition in
 let owned = S.union consumedParameters definitions in
 let drops ids = S.elements (S.inter ids owned) |> List.rev |> List.map (fun id -> O.Drop id) in
 let dups consumed liveAfter =
  let names, counts = List.fold_left (fun (names, counts) id -> match H.ValueMap.find_opt id counts with
   | Some count -> names, H.ValueMap.add id (count + 1) counts | None -> id :: names, H.ValueMap.add id 1 counts) ([], H.ValueMap.empty) consumed in
  List.concat_map (fun id -> let required = H.ValueMap.find id counts + (if S.mem id liveAfter then 1 else 0) in List.init (max 0 (required - 1)) (fun _ -> O.Dup id)) (List.rev names) in
 let scalarUses (operand : H.operand) = CheckedAST.BindingIdMap.bindings operand.H.inputs |> List.filter_map (fun (_, value) -> managedId dialect value) |> S.of_list in
 let rec elaborateBlock liveAfter source =
  let body = dialect.body source in
  let* operations, before = List.fold_right (fun operation result -> let* tail, live = result in match operation with
  | H.Branch (output, condition, yes, no) ->
    let continuation = match managedId dialect output with Some id -> S.remove id live | None -> live in
    let branchLive branch = match managedId dialect (dialect.body branch).H.result with Some id -> S.add id continuation | None -> continuation in
    let* ownedYes, yesBefore = elaborateBlock (branchLive yes) yes in
    let* ownedNo, noBefore = elaborateBlock (branchLive no) no in
    let conditionUses = scalarUses condition in let before = S.union conditionUses (S.union yesBefore noBefore) in
    let edge required (block : ('leaf, H.valueId) O.block) =
     let cleanup = drops (S.diff before required) in
     let preserveResult = match managedId dialect block.O.body.H.result with Some id when S.mem id continuation -> [O.Dup id] | _ -> [] in
     {O.body = {block.O.body with H.operations = cleanup @ block.O.body.H.operations @ preserveResult}} in
    let yes = edge yesBefore ownedYes in let no = edge noBefore ownedNo in
    let branch = H.Branch (output, condition, yes, no) in
    let unusedOutput = match managedId dialect output with Some id when not (S.mem id live) -> S.singleton id | _ -> S.empty in
    Ok (O.Evaluate branch :: (drops unusedOutput @ tail), before)
  | _ ->
    let operationOwnership = match operation with
    | H.Leaf leaf -> let contract = dialect.leafOwnership leaf in
      let borrowed = S.of_list (List.filter_map (function O.Borrowed id -> Some id | O.Consumed _ -> None) contract.O.inputs) in
      let consumed = List.filter_map (function O.Consumed id -> Some id | O.Borrowed _ -> None) contract.O.inputs in
      Ok (borrowed, consumed, S.of_list contract.O.outputs)
    | H.ScalarBinding (output, operand) -> Ok (scalarUses operand, [], S.of_list (Option.to_list (managedId dialect output)))
    | H.Call call -> (match ownership call with None -> Error (UnknownCallOwnership call.H.target) | Some (signature : O.callSignature) ->
      let* borrowed, consumed = callInputs dialect signature call in
      let definitions = match signature.O.result, managedId dialect call.H.result with (O.ProducedCallResult | O.UniqueProducedCallResult), Some id -> S.singleton id | _ -> S.empty in
      Ok (borrowed, consumed, definitions))
    | H.Branch _ -> Crash.crash "Whole-function ownership: branch handled separately" in
    let* borrowed, consumed, definitions = operationOwnership in
    let uses = S.union borrowed (S.of_list consumed) in let before = S.union uses (S.diff live definitions) in
    let consumedSet = S.of_list consumed in let lastBorrowed = S.diff (S.diff uses live) consumedSet in
    let unusedDefinitions = S.diff definitions live in
    let evaluation = match operation with H.Leaf leaf -> H.Leaf leaf | H.ScalarBinding (output, operand) -> H.ScalarBinding (output, operand) | H.Call call -> H.Call call | H.Branch _ -> Crash.crash "Whole-function ownership: branch handled separately" in
    let steps = dups consumed live @ [O.Evaluate evaluation] @ drops (S.union lastBorrowed unusedDefinitions) in
    Ok (steps @ tail, before)) body.H.operations (Ok ([], liveAfter)) in
  Ok ({O.body = {H.parameters = body.H.parameters; operations; result = body.H.result}}, before) in
 let sourceBody = dialect.body definition.H.body in
 let liveResult = S.of_list (Option.to_list (managedId dialect sourceBody.H.result)) in
 let* body, liveBefore = elaborateBlock liveResult definition.H.body in
 let unusedParameters = drops (S.diff consumedParameters liveBefore) in
 let body = {O.body = {body.O.body with H.operations = unusedParameters @ body.O.body.H.operations}} in
 Ok {O.definition = {H.id = definition.H.id; name = definition.H.name; body}; ownership = boundary}
let elaborateFunctionsWithTrace recordTiming dialect definitions =
 let measure name operation = let start = HostClock.milliseconds () in let result = operation () in let elapsed = HostClock.milliseconds () -. start in Option.iter (fun record -> record name elapsed) recordTiming; result in
 let* boundaries, ownership = measure "Ownership detail: Boundary inference" (fun () -> convergeBoundaries dialect definitions) in
 let* functions = measure "Ownership detail: Ownership elaboration" (fun () -> List.fold_left (fun result (definition : 'block H.functionDef) ->
  let* functions = result in match F.tryFind definition.H.id boundaries with
  | None -> Crash.crash "Whole-function ownership boundary disappeared during elaboration"
  | Some boundary -> let* owned = elaborateFunction dialect ownership boundary definition in Ok (owned :: functions)) (Ok []) definitions) in
 let scalarIds (operand : H.operand) = CheckedAST.BindingIdMap.bindings operand.H.inputs |> List.filter_map (fun (_, value) -> managedId dialect value) |> S.of_list in
 let ownershipSemantics : 'leaf Ownership.semantics = {Ownership.leaf = dialect.leafOwnership; leafUniqueness = dialect.leafUniqueness; callOwnership = ownership;
  scalarUses = scalarIds; scalarEscapes = scalarIds; blockArgument = (fun value -> match managedId dialect value with Some id -> O.Managed id | None -> O.Unmanaged)} in
 Ok {functions = List.rev functions; semantics = ownershipSemantics}
let elaborateFunctions dialect definitions = elaborateFunctionsWithTrace None dialect definitions
