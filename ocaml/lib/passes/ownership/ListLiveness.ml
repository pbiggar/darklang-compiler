(* ListLiveness.fs - Representation-independent collection liveness and entry verification. *)
[@@@warning "-4"]
module H = HIR
module L = ListRegion
module Identity = struct type t = H.valueId let compare (H.ValueId a) (H.ValueId b) = Int.compare a b module Set = H.ValueSet end
module Liveness = ValueLiveness.Make (Identity)
module S = H.ValueSet
(*
   Region aliases are canonical identities. Opaque scalar evaluations cannot
   access them; callbacks cannot capture them. These are value-edge contracts,
   not permission to reorder scalar effects or to mutate a borrowed parameter.
*)
let rec valueContract operation : Liveness.contract =
 let managed (value : H.value) = if value.H.typ = AST.TList AST.TInt64 then S.singleton value.H.id else S.empty in
 let uses, defines = match operation with
 | H.Branch (output, _, yes, no) -> let branchUses (L.FunctionalBlock body as block) = entryLive block (managed body.H.result) in
   let yes = branchUses yes in let no = branchUses no in S.union yes no, managed output
 | H.Leaf leaf -> let contract = L.primitiveContract leaf in
   S.of_list (List.map (fun (value : H.value) -> value.H.id) contract.H.inputs), S.of_list (List.map (fun (value : H.value) -> value.H.id) (H.managedOutputs contract))
 | H.Call call -> S.of_list (List.concat_map (fun value -> S.elements (managed value)) call.H.arguments), managed call.H.result
 | H.ScalarBinding _ -> S.empty, S.empty in
 {Liveness.uses = uses; defines}
and entryLive (L.FunctionalBlock block) liveAfter = List.fold_right (fun operation live -> Liveness.liveBefore (valueContract operation) live) block.H.operations liveAfter
(*
   No physical ownership is needed to check the region's incoming value
   interface. The current representation permits no external collection roots.
*)
let verifyFunctional (L.FunctionalRegion block) =
 let dialect : ((L.transform * L.reuseSelection) L.operation, L.functionalBlock) VerifyHIR.dialect = {VerifyHIR.body = (fun (L.FunctionalBlock block) -> block); leaf = L.primitiveContract; callSignature = (fun _ -> None); callContract = (fun _ -> None)} in
 match VerifyHIR.verify dialect block with
 | Error error -> Error ("List HIR: " ^ VerifyHIR.errorToString error)
 | Ok () -> if S.is_empty (entryLive block S.empty) then Ok () else Error "List HIR: external collection roots in a closed region"
