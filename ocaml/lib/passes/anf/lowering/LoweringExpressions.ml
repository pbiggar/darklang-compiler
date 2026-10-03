(* Expressions.fs - Tie recursive ANF lowering handlers and list-region selection together. *)
[@@@warning "-4"]
module A = ANF
module R = TypeRegistries
module K = Continuations
let ( let* ) = Result.bind
let rec toANFCore ids sums typeNames inert expr gen env registry variants functions names modules =
 let resolveFunction name = match StringOrder.Map.find_opt name ids with Some id -> id | None -> Crash.crash ("List-region helper '" ^ name ^ "' is absent from registries") in
 let infer localTypes value =
  let types = R.BindingMap.fold (fun name typ types -> R.BindingMap.add name typ types) localTypes (R.typeEnvFromVarEnv env) in
  LoweringTypeInference.inferTypeCore sums typeNames value types registry variants functions names modules in
 match ExtractListRegions.tryExtract inert names (R.typeEnvFromVarEnv env) infer (fun value -> ClosureAnalysis.freeVars value ClosureAnalysis.BindingSet.empty) expr with
 | Some region ->
   let lower value gen env = toANFUnplannedCore ids sums typeNames inert value gen env registry variants functions names modules in
   let* () = ListLiveness.verifyFunctional region in
   LowerListRegions.lower resolveFunction lower env gen (ElaborateListOwnership.elaborateOwnership (SelectListStorage.selectStorage region))
 | None -> toANFUnplannedCore ids sums typeNames inert expr gen env registry variants functions names modules
and toANFUnplannedCore ids sums typeNames inert expr gen env registry variants functions names modules =
 ExpressionLowering.lowerExpression (toANFCore ids) (toAtomCore ids) (toANFBoundAtomCore ids) ids sums typeNames inert expr gen env registry variants functions names modules
and toAtomCore ids sums typeNames inert expr gen env registry variants functions names modules =
 AtomLowering.lowerAtom (toANFCore ids) (toAtomCore ids) (toANFBoundAtomCore ids) ids sums typeNames inert expr gen env registry variants functions names modules
(*
   Keep existing atom lowering behavior unchanged when toAtom succeeds:
   do not introduce extra temp ids in the common path.
   Pattern-bound generic values can retain a TVar in the recovered
   TypeMap even though checking established the match result type.
   Give each return path an explicit boundary type before it jumps.
*)
and toANFBoundAtomCore ids sums typeNames inert expr gen env registry variants functions names modules =
 match toAtomCore ids sums typeNames inert expr gen env registry variants functions names modules with
 | Ok (atom, bindings, gen) -> Ok (K.wrapBindings bindings (A.Return atom), atom, gen)
 | Error _ ->
   let lowerWithBranchLocalBinding () =
    let bound, gen = A.freshVar gen in
    let* expr, gen = toANFCore ids sums typeNames inert expr gen env registry variants functions names modules in
    Ok (K.bindReturns expr (fun atom -> A.Let (bound, A.Atom atom, A.Return (A.Var bound))), A.Var bound, gen) in
   match expr with
   | CheckedAST.Match _ ->
     let* resultType = LoweringTypeInference.inferTypeCore sums typeNames expr (R.typeEnvFromVarEnv env) registry variants functions names modules in
     if K.isSupportedJoinArgumentType resultType then
      let bound, gen = A.freshVar gen in
      let* expr, gen = toANFCore ids sums typeNames inert expr gen env registry variants functions names modules in
      let rec returnsToTypedJumps expr gen = match expr with
       | A.Return atom -> let typed, gen = A.freshVar gen in A.Let (typed, A.TypedAtom (atom, resultType), A.Jump (bound, A.Var typed)), gen
       | A.Jump _ -> expr, gen
       | A.Join (parameter, continuation, entry) ->
         let continuation, gen = returnsToTypedJumps continuation gen in let entry, gen = returnsToTypedJumps entry gen in A.Join (parameter, continuation, entry), gen
       | A.Let (id, cexpr, rest) -> let rest, gen = returnsToTypedJumps rest gen in A.Let (id, cexpr, rest), gen
       | A.If (condition, yes, no) -> let yes, gen = returnsToTypedJumps yes gen in let no, gen = returnsToTypedJumps no gen in A.If (condition, yes, no), gen in
      let entry, gen = returnsToTypedJumps expr gen in
      Ok (A.Join ({A.id = bound; typ = resultType}, A.Return (A.Var bound), entry), A.Var bound, gen)
     else lowerWithBranchLocalBinding ()
   | _ -> lowerWithBranchLocalBinding ()
