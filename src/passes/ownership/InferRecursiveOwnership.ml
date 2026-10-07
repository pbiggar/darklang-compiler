(*
   Resolve one concrete call demand against a recursive SCC. The target
   boundary is restricted to refinements usable by that call; remaining member
   boundaries are explored lazily because recursive proof remains atomic.
   Exhausting the bounded search is an optimization miss, not a compile error.
*)
(* InferRecursiveOwnership.ml - Solve uniqueness boundaries across visible function groups. *)
[@@@warning "-4"]
module O = OwnedIR
module U = InferOwnershipUniqueness
module F = FunctionIdMap
type 'id functionBoundary = {id : AST.functionId; name : string; ownership : 'id O.functionSignature}
type 'id groupBoundary = GroupBoundary of 'id functionBoundary * 'id functionBoundary list
type 'id candidates = Candidates of 'id groupBoundary * 'id groupBoundary list
let toList (Candidates (head, tail)) = head :: tail
let boundaryToList (GroupBoundary (head, tail)) = head :: tail
let dominates (first : ('leaf, 'id) O.functionDef list) (second : ('leaf, 'id) O.functionDef list) =
 let rec compare noWorse strictlyBetter first second = match first, second with
 | [], [] -> noWorse && strictlyBetter
 | (first : ('leaf, 'id) O.functionDef) :: firstRest, (second : ('leaf, 'id) O.functionDef) :: secondRest ->
   let boundaryNoWorse, boundaryStrict = U.boundaryRelation first.O.ownership second.O.ownership in
   compare (noWorse && boundaryNoWorse) (strictlyBetter || boundaryStrict) firstRest secondRest
 | _ -> Crash.crash "Uniqueness group variants changed function count" in
 compare true false first second
let variants (functions : ('leaf, 'id) O.functionDef list) =
 List.fold_left (fun groups (functionDefinition : ('leaf, 'id) O.functionDef) ->
  List.concat_map (fun group -> List.map (fun ownership -> group @ [{functionDefinition with O.ownership}]) (U.signatures functionDefinition.O.ownership)) groups) [[]] functions
let rec variantsWithTarget target targetOwnership (functions : ('leaf, 'id) O.functionDef list) () = match functions with
 | [] -> Seq.Cons ([], Seq.empty)
 | functionDefinition :: rest ->
   let boundaries = if functionDefinition.O.definition.HIR.id = target then Seq.return targetOwnership else U.signatureSequence functionDefinition.O.ownership in
   Seq.flat_map (fun ownership -> Seq.map (fun group -> {functionDefinition with O.ownership} :: group) (variantsWithTarget target targetOwnership rest)) boundaries ()
let boundary = function
 | [] -> Crash.crash "Ownership uniqueness inference produced an empty function group"
 | head :: tail ->
   let functionBoundary (functionDefinition : ('leaf, 'id) O.functionDef) = {id = functionDefinition.O.definition.HIR.id; name = functionDefinition.O.definition.HIR.name; ownership = functionDefinition.O.ownership} in
   GroupBoundary (functionBoundary head, List.map functionBoundary tail)
module Make (Identity : O.Identity) = struct
 module Verify = VerifyOwnership.Make (Identity)
 module Ownership = Verify.Ownership
 module Uniqueness = U.Make (Identity)
 let withCandidateGroupSemantics (semantics : 'leaf Ownership.semantics) (definitions : ('leaf, Identity.t) O.functionDef list) =
  let members = F.ofList (List.map (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.HIR.id, ()) definitions) in
  {semantics with Ownership.callOwnership = (fun call -> if F.containsKey call.HIR.target members then None else semantics.Ownership.callOwnership call)}
(*
   Infer a mutually visible function group as one proof unit. Internal direct
   and recursive calls receive ownership contracts derived from each candidate
   group, while external call contracts continue to come from the dialect.
   Nondominance is evaluated across every function boundary together.
*)
 let infer semantics definitions =
  let definitions = AST.NonEmptyList.toList definitions in
  let candidateSemantics = withCandidateGroupSemantics semantics definitions in
  let refinableModes = List.fold_left (fun count (definition : ('leaf, Identity.t) O.functionDef) -> count + U.refinableModeCount definition.O.ownership) 0 definitions in
  if not (U.withinVariantLimit refinableModes) then Error (Uniqueness.VariantLimitExceeded (refinableModes, U.maximumVariants))
  else
   let verified, firstFailure = List.fold_left (fun (verified, firstFailure) candidate -> match Verify.verifyFunctions candidateSemantics candidate with
    | Ok () -> candidate :: verified, firstFailure
    | Error error -> verified, (match firstFailure with Some _ -> firstFailure | None -> Some error)) ([], None) (variants definitions) in
   let verified = List.rev verified in
   let nondominated = List.filter (fun candidate -> not (List.exists (fun other -> dominates other candidate) verified)) verified in
   match nondominated, firstFailure with
   | head :: tail, _ -> Ok (Candidates (boundary head, List.map boundary tail))
   | [], Some error -> Error (Uniqueness.NoVerifiedFunctionGroup error)
   | [], None -> Crash.crash "Ownership uniqueness inference generated no function-group candidates"
 let inferDemand semantics target uniqueArguments definitions =
  let definitions = AST.NonEmptyList.toList definitions in
  let candidateSemantics = withCandidateGroupSemantics semantics definitions in
  let targetDefinition = match List.find_opt (fun (definition : ('leaf, Identity.t) O.functionDef) -> definition.O.definition.HIR.id = target) definitions with
   | Some definition -> definition | None -> Crash.crash "Recursive ownership demand target is outside its function group" in
  Ok (Seq.find_map (fun candidate -> match Verify.verifyFunctions candidateSemantics candidate with Ok () -> Some (boundary candidate) | Error _ -> None)
   (U.demandedSignatures uniqueArguments targetDefinition.O.ownership |> Seq.flat_map (fun targetOwnership -> variantsWithTarget target targetOwnership definitions) |> Seq.take U.maximumVariants))
end
