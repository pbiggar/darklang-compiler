(* VerifyListOwnership.ml - Independently verify region ownership, layouts, and typed edges. *)
[@@@warning "-4"]
module H = HIR
module L = ListRegion
module O = OwnedIR
module S = H.ValueSet
module V = VerifyOwnership.Make (ListLiveness.Identity)
module Contracts = V.Ownership
(*
   A dialect must describe every access and possible escape, including opaque
   scalar operands and typed block results. A managed result transfers its
   ownership identity to the branch target; Unmanaged means the value has no
   ownership unit.
   Extraction proves scalars and callbacks cannot access the canonical list
   identities. List-specific storage facts remain here; accounting is shared.
*)
let semantics : (L.transform * L.ownership) L.operation Contracts.semantics = {
 Contracts.leaf = (function
 | L.Construct (output, _) -> {O.inputs = []; outputs = [output.H.id]}
 | L.Transform (output, input, (_, ownership)) -> let use = match ownership with L.Consume | L.ConsumeOrCopy -> O.Consumed input.H.id | L.BorrowAndCopy -> O.Borrowed input.H.id in {O.inputs = [use]; outputs = [output.H.id]}
 | L.Fold (_, input, _, _) -> {O.inputs = [O.Borrowed input.H.id]; outputs = []});
 leafUniqueness = (function
 | L.Construct (output, _) -> {Contracts.requiredInputs = S.empty; uniqueOutputs = S.singleton output.H.id}
 | L.Transform (output, input, (_, L.Consume)) -> {Contracts.requiredInputs = S.singleton input.H.id; uniqueOutputs = S.singleton output.H.id}
 | L.Transform (output, _, (_, (L.BorrowAndCopy | L.ConsumeOrCopy))) -> {Contracts.requiredInputs = S.empty; uniqueOutputs = S.singleton output.H.id}
 | L.Fold _ -> {Contracts.requiredInputs = S.empty; uniqueOutputs = S.empty});
 callOwnership = (fun _ -> None);
 scalarUses = (fun (operand : H.operand) -> CheckedAST.BindingIdMap.bindings operand.H.inputs |> List.filter_map (fun (_, (value : H.value)) -> if value.H.typ = AST.TList AST.TInt64 then Some value.H.id else None) |> S.of_list);
 scalarEscapes = (fun _ -> S.empty);
 blockArgument = (fun (value : H.value) -> if value.H.typ = AST.TList AST.TInt64 then O.Managed value.H.id else O.Unmanaged)
}
let verifyBlockOwnership block = Result.map_error (fun error -> "List HIR: " ^ Contracts.errorToString (fun (H.ValueId id) -> StructuralValue.Union ("ValueId", [StructuralValue.Scalar (string_of_int id)])) error) (V.verifyClosed semantics block)
let verify (L.OwnedRegion (block, layouts)) =
 let rec blockValid (block : L.ownedBlock) = L.immediate block.O.body.H.result.H.typ && List.for_all typesValid block.O.body.H.operations
 and typesValid step = match step with
 | O.Dup id | O.Drop id -> H.ValueMap.mem id layouts
 | O.Evaluate (H.Leaf (L.Construct (output, L.Literal elements))) -> List.for_all (fun (element : H.operand) -> element.H.typ = AST.TInt64) elements && output.H.typ = AST.TList AST.TInt64 && L.extent (L.lookup "construction layout" output.H.id layouts) = L.ConstantLength (List.length elements)
 | O.Evaluate (H.Leaf (L.Construct (output, L.Repeat (count, value)))) -> count.H.typ = AST.TInt && value.H.typ = AST.TInt64 && output.H.typ = AST.TList AST.TInt64 && L.extent (L.lookup "repeat layout" output.H.id layouts) = L.RuntimeLength output.H.id
 | O.Evaluate (H.Leaf (L.Transform (output, input, (operation, _)))) -> output.H.typ = AST.TList AST.TInt64 && input.H.typ = AST.TList AST.TInt64 && L.lookup "output layout" output.H.id layouts = L.lookup "input layout" input.H.id layouts && (match operation with L.Map callback -> callback.H.typ = AST.TFunction ([AST.TInt64], AST.TInt64) | L.Reverse -> true)
 | O.Evaluate (H.Leaf (L.Fold (output, input, initial, callback))) -> output.H.typ = AST.TInt64 && input.H.typ = AST.TList AST.TInt64 && initial.H.typ = AST.TInt64 && callback.H.typ = AST.TFunction ([AST.TInt64;AST.TInt64], AST.TInt64)
 | O.Evaluate (H.ScalarBinding (output, value)) -> output.H.typ = value.H.typ && L.immediate value.H.typ
 | O.Evaluate (H.Call _) -> false
 | O.Evaluate (H.Branch (_, condition, yes, no)) -> condition.H.typ = AST.TBool && yes.O.body.H.result.H.typ = no.O.body.H.result.H.typ && blockValid yes && blockValid no in
 if H.ValueMap.exists (fun _ layout -> match layout with L.RecycledArray length -> length < 0 || length > L.recycledCapacityLimit | L.MappedArray length -> length <= L.recycledCapacityLimit || length > L.maxCapacity | L.RuntimeArray _ -> false) layouts then Error "List HIR: unsupported allocation size" else if not (blockValid block) then Error "List HIR: invalid storage operand types" else verifyBlockOwnership block
