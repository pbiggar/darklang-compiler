(* Destruction.fs - Prove inert destruction locally and through function scope contracts. *)
[@@@warning "-4"]
module S = SpecializationIdentity.FunctionSet
(*
   Structural proof that releasing a value cannot invoke user code. This is
   not an effect/purity claim about evaluating it. Nominal payloads and opaque
   closures require layout/capture evidence unavailable from the type alone.
*)
let rec hasInertDestruction = function
 | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64
 | AST.TInt | AST.TInt128 | AST.TUInt128 | AST.TBool | AST.TFloat64 | AST.TString | AST.TChar | AST.TBlob
 | AST.TUnit | AST.TDateTime | AST.TInternalRawPtr | AST.TNever -> true
 | AST.TList element -> hasInertDestruction element
 | AST.TTuple elements -> List.for_all hasInertDestruction elements
 | AST.TDict (key, value) -> hasInertDestruction key && hasInertDestruction value
 | _ -> false
type scopeDestruction = InertScope | UnprovenScope
type functionScopeContract = {localDestruction : scopeDestruction; calls : S.t}
(*
   These compiler primitives can perform effects, but neither owns a value
   whose destruction invokes user code. Unknown external calls remain unproven.
*)
let inertPrimitiveNames = ["Builtin.printLine";"Builtin.print"]
(*
   Reject callers transitively from locally unproven scopes and unavailable
   callees. Safe recursive components are accepted without unfolding paths.
*)
let inertFunctionScopesWithBase known functionIds contracts =
 let names = S.of_seq (FunctionIdMap.keys contracts) in
 let inertPrimitives = List.filter_map (fun name -> StringOrder.Map.find_opt name functionIds) inertPrimitiveNames |> S.of_list in
 let primitives = S.diff (S.union known inertPrimitives) names in let unavailable calls = not (S.subset calls (S.union names primitives)) in
 let unproven = FunctionIdMap.toList contracts |> List.filter_map (fun (name, contract) -> match contract.localDestruction with UnprovenScope -> Some name | InertScope when unavailable contract.calls -> Some name | InertScope -> None) |> S.of_list in
 let callers = FunctionIdMap.fold (fun callers name contract -> S.fold (fun target callers -> FunctionIdMap.change target (fun previous -> Some (S.add name (Option.value previous ~default:S.empty))) callers) contract.calls callers) FunctionIdMap.empty contracts in
 let rejected = CallGraphReachability.findReachable callers unproven in S.union primitives (S.diff names rejected)
let inertFunctionScopes functionIds contracts = inertFunctionScopesWithBase S.empty functionIds contracts
