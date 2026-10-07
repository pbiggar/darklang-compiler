(*
   Verify the best ownership refinement usable by one concrete call. Search is
   lazy and bounded: inability to prove an optimization retains the established
   boundary instead of rejecting an otherwise valid program.
*)
(* InferOwnershipUniqueness.fs - Derive verifier-proven uniqueness boundary variants. *)
[@@@warning "-4"]
module O = OwnedIR
let maximumVariants = 256
let parameterVariants = function O.ConsumedParameter id -> [O.ConsumedParameter id; O.UniqueParameter id] | ownership -> [ownership]
let resultVariants = function O.ProducedResult id -> [O.ProducedResult id; O.UniqueProducedResult id] | ownership -> [ownership]
let refinableModeCount (signature : 'id O.functionSignature) =
 let parameters = List.fold_left (fun count -> function O.ConsumedParameter _ -> count + 1 | _ -> count) 0 signature.O.parameters in
 match signature.O.result with O.ProducedResult _ -> parameters + 1 | _ -> parameters
let withinVariantLimit refinableModes =
 let rec doubleWithinLimit remaining variants = if remaining = 0 then true else if variants > maximumVariants / 2 then false else doubleWithinLimit (remaining - 1) (variants * 2) in
 doubleWithinLimit refinableModes 1
let rec parameterSignatures = function
 | [] -> Seq.return []
 | ownership :: rest -> Seq.flat_map (fun variant -> Seq.map (fun remaining -> variant :: remaining) (parameterSignatures rest)) (List.to_seq (parameterVariants ownership))
let signatureSequence (signature : 'id O.functionSignature) =
 Seq.flat_map (fun parameterModes -> Seq.map (fun resultMode -> ({O.parameters = parameterModes; result = resultMode} : 'id O.functionSignature)) (List.to_seq (resultVariants signature.O.result))) (parameterSignatures signature.O.parameters)
let signatures signature = List.of_seq (signatureSequence signature)
let rec combinations size values () = match size, values with
 | 0, _ -> Seq.Cons ([], Seq.empty)
 | _, [] -> Seq.Nil
 | size, head :: tail when size > 0 -> Seq.append (Seq.map (fun rest -> head :: rest) (combinations (size - 1) tail)) (combinations size tail) ()
 | _ -> Seq.Nil
(*
   Produce only refinements that a concrete call can satisfy. Candidates are
   ordered by the number and then positions of required unique arguments, so
   verification may stop at the first successful boundary.
*)
let demandedSignatures uniqueArguments (signature : 'id O.functionSignature) =
 match signature.O.result with
 | O.ProducedResult result ->
   let available = List.mapi (fun index ownership -> match ownership with O.ConsumedParameter _ when O.IntSet.mem index uniqueArguments -> Some index | _ -> None) signature.O.parameters |> List.filter_map Fun.id in
   Seq.flat_map (fun size -> Seq.map (fun selected ->
    let selected = O.IntSet.of_list selected in
    let parameters = List.mapi (fun index ownership -> match ownership with O.ConsumedParameter id when O.IntSet.mem index selected -> O.UniqueParameter id | _ -> ownership) signature.O.parameters in
    ({O.parameters; result = O.UniqueProducedResult result} : 'id O.functionSignature)) (combinations size available)) (Seq.ints 0 |> Seq.take (List.length available + 1))
 | O.UnmanagedResult | O.BorrowedResult _ | O.UniqueProducedResult _ -> Seq.empty
let parameterStrength = function O.UnmanagedParameter | O.BorrowedParameter _ -> 0 | O.ConsumedParameter _ -> 1 | O.UniqueParameter _ -> 2
let resultStrength = function O.UnmanagedResult | O.BorrowedResult _ -> 0 | O.ProducedResult _ -> 1 | O.UniqueProducedResult _ -> 2
let boundaryRelation (first : 'id O.functionSignature) (second : 'id O.functionSignature) =
 let rec compareParameters noStronger strictlyBetter first second = match first, second with
 | [], [] -> noStronger, strictlyBetter
 | first :: firstRest, second :: secondRest -> compareParameters (noStronger && parameterStrength first <= parameterStrength second) (strictlyBetter || parameterStrength first < parameterStrength second) firstRest secondRest
 | _ -> Crash.crash "Uniqueness variants changed function parameter arity" in
 let parametersNoStronger, strictlyWeakerParameters = compareParameters true false first.O.parameters second.O.parameters in
 let resultNoWeaker = resultStrength first.O.result >= resultStrength second.O.result in
 let strictlyBetter = strictlyWeakerParameters || resultStrength first.O.result > resultStrength second.O.result in
 parametersNoStronger && resultNoWeaker, strictlyBetter
(*
   A boundary dominates another when it requires no stronger parameter modes
   and promises no weaker result mode, with at least one strict improvement.
*)
let dominates first second = match boundaryRelation first second with true, true -> true | _ -> false
let rec callsTarget target (block : ('leaf, 'id) O.block) =
 List.exists (function
 | O.Evaluate (HIR.Call call) -> call.HIR.target = target
 | O.Evaluate (HIR.Branch (_, _, yes, no)) -> callsTarget target yes || callsTarget target no
 | O.Evaluate (HIR.Leaf _ | HIR.ScalarBinding _) | O.Dup _ | O.Drop _ -> false) block.O.body.HIR.operations
module Make (Identity : O.Identity) = struct
 module Verify = VerifyOwnership.Make (Identity)
 module Ownership = Verify.Ownership
(*
   Ownership transfer modes are established before this pass. Inference may
   strengthen consumed parameters and produced results with exclusivity, but
   it never changes whether ownership crosses the boundary.
*)
 type candidates = Candidates of Identity.t O.functionSignature * Identity.t O.functionSignature list
 type inferenceError = VariantLimitExceeded of int * int | RecursiveFunctionRequiresGroupInference of AST.functionId | NoVerifiedBoundary of Ownership.verificationError | NoVerifiedFunctionGroup of Ownership.verificationError
 let toList (Candidates (head, tail)) = head :: tail
(*
   Enumerate the nondominated uniqueness refinements accepted by the
   ownership verifier. Returning every tradeoff keeps specialization policy
   separate from proof: for example, a weaker input requirement and a stronger
   result guarantee can both remain useful boundaries. Typed HIR, primitive
   contracts, and non-recursive call ownership remain independent prerequisites;
   recursive boundaries use `InferRecursiveOwnership.infer` so calls are
   checked against the same group-wide candidate.
*)
 let infer semantics (functionDefinition : ('leaf, Identity.t) O.functionDef) =
  let signature = functionDefinition.O.ownership in let root = functionDefinition.O.definition.HIR.body in
  let refinableModes = refinableModeCount signature in
  if callsTarget functionDefinition.O.definition.HIR.id root then Error (RecursiveFunctionRequiresGroupInference functionDefinition.O.definition.HIR.id)
  else if not (withinVariantLimit refinableModes) then Error (VariantLimitExceeded (refinableModes, maximumVariants))
  else
   let verified, firstFailure = List.fold_left (fun (verified, firstFailure) candidate -> match Verify.verifyFunction semantics candidate root with
    | Ok () -> candidate :: verified, firstFailure
    | Error error -> verified, (match firstFailure with Some _ -> firstFailure | None -> Some error)) ([], None) (signatures signature) in
   let verified = List.rev verified in
   let nondominated = List.filter (fun candidate -> not (List.exists (fun other -> dominates other candidate) verified)) verified in
   match nondominated, firstFailure with
   | head :: tail, _ -> Ok (Candidates (head, tail))
   | [], Some error -> Error (NoVerifiedBoundary error)
   | [], None -> Crash.crash "Ownership uniqueness inference generated no boundary candidates"
 let inferDemand semantics uniqueArguments (functionDefinition : ('leaf, Identity.t) O.functionDef) =
  let root = functionDefinition.O.definition.HIR.body in
  if callsTarget functionDefinition.O.definition.HIR.id root then Error (RecursiveFunctionRequiresGroupInference functionDefinition.O.definition.HIR.id)
  else Ok (Seq.find_map (fun candidate -> match Verify.verifyFunction semantics candidate root with Ok () -> Some candidate | Error _ -> None) (Seq.take maximumVariants (demandedSignatures uniqueArguments functionDefinition.O.ownership)))
end
