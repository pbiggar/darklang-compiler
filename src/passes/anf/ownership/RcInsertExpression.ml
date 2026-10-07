(* RcInsertExpression.ml - Elaborate ANF expression ownership using return and alias facts. *)
[@@@warning "-4"]
module A = ANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module S = RcShapePlanning
module C = RcCleanup
module P = MemoryPlanning
module M = F.TempMap
module Set = R.TempSet
let tryItem index values = if index < 0 then None else List.nth_opt values index
let decId (id, _, _, _, _, _) = id
let ids decs = Set.of_list (List.map decId decs)
let exists predicate = function Some value -> predicate value | None -> false
let starts prefix value = String.starts_with ~prefix value
(*
   Recover a binding's type without relying on ownership rewrites. Some raw
   projections carry their concrete type only at the next use site.
*)
let inferBindingType ctx tempId cexpr bodyInfo =
 let A.TempId index = tempId in
 let rec inferAliasedVarTypeFromUse aliased next =
  let inferFromCall func args = match FunctionIdMap.tryFind func ctx.F.funcReg with
   | Some (_, AST.TFunction (params, _)) -> List.find_map (fun (index, atom) -> match atom, tryItem index params with A.Var id, Some typ when id = aliased -> Some typ | _ -> None) (List.mapi (fun index atom -> index, atom) args)
   | _ -> None in
  match next with
  | R.RLet (_, A.RawSlotInit (_, _, A.Var value, typ), _, _) when value = aliased -> Some typ
  | R.RLet (_, A.Call (func, args), _, _) | R.RLet (_, A.BorrowedCall (func, args), _, _) | R.RLet (_, A.TailCall (func, args), _, _) -> inferFromCall func args
  | R.RLet (id, A.Atom (A.Var source), body, _) when source = aliased -> inferAliasedVarTypeFromUse id body
  | R.RLet (id, A.TypedAtom (A.Var source, typ), body, _) when source = aliased -> if S.shapeNeedsManagedAliasRootPreservation ctx typ then Some typ else inferAliasedVarTypeFromUse id body
  | R.RIf (A.Var id, _, _, _) when id = aliased -> Some AST.TBool
  | _ -> None in
 match F.inferCExprType ctx cexpr with
 | Some typ ->
   let aliasType = match bodyInfo with
    | R.RLet (_, A.TypedAtom (A.Var source, typ), _, _) when source = tempId -> Some typ
    | R.RLet (id, A.Atom (A.Var source), body, _) when source = tempId -> inferAliasedVarTypeFromUse id body
    | _ -> None in
   (match aliasType with Some alias when S.shapeNeedsManagedAliasRootPreservation ctx alias && not (S.shapeNeedsManagedAliasRootPreservation ctx typ) -> alias | _ -> typ)
 | None ->
   let rawType = AST.TVar ("raw_get_" ^ string_of_int index) in
   match cexpr, bodyInfo with
   | A.TupleGet _, R.RLet (_, A.TypedAtom (A.Var source, typ), _, _) when source = tempId -> typ
   | A.RawGet (_, _, None), R.RLet (_, A.TypedAtom (A.Var source, typ), _, _) when source = tempId -> typ
   | A.RawGet (_, _, None), R.RLet (id, A.Atom (A.Var source), body, _) when source = tempId -> Option.value ~default:rawType (inferAliasedVarTypeFromUse id body)
   | A.RawGet (_, _, None), _ -> rawType
   | _ -> AST.TVar ("inferred_" ^ string_of_int index)
(*
   Generated result printing consumes the returned root after
   lowering. Finalize every other ownership obligation first,
   matching the ordinary return boundary while keeping the output
   effect visible to ownership analysis.
   This walk visits each binding once. Branch-local type state must
   remain separate when sibling branches reuse a TempId.
   Track closure function names for later ClosureCall type resolution
   TempIds are unique within the function, so the current binding's
   pending-release multiplicity is known while prepending it. Keep
   that fact instead of rescanning the growing release stack.
   List<Record { List<Dict<_, _>> }> construction retains the
   dict once for the inner list payload and once for the returned graph.
   The current shape-specific ARM64 helpers release that graph, but the
   local dict temp still needs both ownership edges balanced.
   A materialized Stream seeds its RC word at zero; the
   first typed slot retain establishes the owning edge.
   RawSlotInit's backend retain is unnecessary when the slot
   adopts the producer's existing owned edge.
   Ownership transfers are rare. Preserve the existing frame
   spine when there is nothing to clear instead of rebuilding
   every preceding frame for every ordinary binding.
   A borrowed call has no owned result edge to transfer from
   its callee. Materialize one for the local binding; its
   ordinary pending decrement then balances this retain.
   IfValue selects one of two existing heap values.
   Materialize ownership on the selected temp before source temps are decref'd.
   Returning a pure alias of an already-returned owned value should not inc again.
   Process the body iteratively, then rebuild on the way back out
*)
let rec insertRCWithAnalysis joinScopes inheritedBranchDecs ctx currentFunc expr gen returnDecs inheritedTransferable paramIncs types =
 let functionHasName id name = exists (fun (actual, _) -> actual = name) (FunctionIdMap.tryFind id ctx.F.funcReg) in
 let pendingIds = ids returnDecs in
 let branchDecs = List.filter (fun dec -> not (Set.mem (decId dec) (R.returnedSet expr)) && not (Set.mem (decId dec) pendingIds)) inheritedBranchDecs in
 let returnDecs = branchDecs @ returnDecs in
 let ctxWithTypes = F.withTempTypes ctx types in
 let nestedRecordListDict = match Option.bind currentFunc (F.tryGetFuncReturnTypeFromReg ctx) with
  | Some (AST.TList (AST.TRecord (name, _))) -> exists (fun (record : TypeRegistries.recordTypeInfo) -> match List.map snd record.TypeRegistries.fields with [AST.TList (AST.TDict _)] -> true | _ -> false) (StringOrder.Map.find_opt name ctx.F.typeReg)
  | _ -> false in
 let mapTransfersSecond = exists (fun func -> match FunctionIdMap.tryFind func ctx.F.funcReg with
  | Some (name, AST.TFunction (_ :: second :: _, _)) -> (name = "Darklang.Stdlib.List.__mapHelper" || starts "Darklang.Stdlib.List.__mapHelper_" name) && P.rcShapeIsOwnershipTransferRoot (S.rcShapeForType ctx second)
  | _ -> false) currentFunc in
 let rec descend ctx expr gen returnDecs inheritedTransferable inheritedBranchDecs (frames : C.letFrame list) types =
  let rebuild state = C.applyLetFrames ctx frames state in
  let conditional () = List.filter_map (fun frame -> frame.C.branchDec) frames @ inheritedBranchDecs in
  let transferable () = List.filter_map (fun frame -> frame.C.transferableOwnership) frames @ inheritedTransferable in
  match expr with
  | R.RJump (target, atom, _) ->
    let deferred = match M.find_opt target joinScopes with Some ids -> ids | None -> let A.TempId id = target in Crash.crash ("RC insertion: join target TempId " ^ string_of_int id ^ " is not in scope") in
    rebuild (C.insertReturnDecs (List.filter (fun dec -> not (Set.mem (decId dec) deferred)) returnDecs) (A.Jump (target, atom)) gen types)
  | R.RJoin (parameter, continuation, entry, _) ->
    let conditional = conditional () and transferable = transferable () in
    let deferred = ids (conditional @ returnDecs) in
    let continuationTypes = M.add parameter.A.id parameter.A.typ types in
    let body, afterBody, bodyTypes = insertRCWithAnalysis joinScopes conditional ctx currentFunc continuation gen returnDecs transferable paramIncs continuationTypes in
    let entry, final, finalTypes = insertRCWithAnalysis (M.add parameter.A.id deferred joinScopes) conditional ctx currentFunc entry afterBody returnDecs transferable paramIncs bodyTypes in
    rebuild (A.Join (parameter, body, entry), final, finalTypes)
  | R.RReturn (atom, returned) ->
    let body, gen, types = C.insertParamIncsAtReturn ctx paramIncs returned (A.Return atom) gen types in
    rebuild (C.insertReturnDecs returnDecs body gen types)
  | R.RIf (cond, yes, no, _) ->
    let conditional = conditional () and transferable = transferable () in
    let pending = ids returnDecs in
    let local returned = List.filter_map (fun frame -> match frame.C.branchDec with Some dec when not (Set.mem (decId dec) returned) && not (Set.mem (decId dec) pending) -> Some dec | _ -> None) frames in
    let yes', gen, types = insertRCWithAnalysis joinScopes conditional ctx currentFunc yes gen (local (R.returnedSet yes) @ returnDecs) transferable paramIncs types in
    let no', gen, types = insertRCWithAnalysis joinScopes conditional ctx currentFunc no gen (local (R.returnedSet no) @ returnDecs) transferable paramIncs types in
    rebuild (A.If (cond, yes', no'), gen, types)
  | R.RLet (id, (A.Print _ as operation), R.RReturn (atom, returned), _) ->
    let printed = A.Let (id, operation, A.Return atom) in
    let body, gen, types = C.insertParamIncsAtReturn ctx paramIncs returned printed gen (M.add id AST.TUnit types) in
    rebuild (C.insertReturnDecs returnDecs body gen types)
  | R.RLet (id, operation, bodyInfo, _) ->
    let typ = inferBindingType ctx id operation bodyInfo in
    let typesWithBinding = match operation with A.TypedAtom (A.Var source, aliasType) -> M.add source aliasType (M.add id typ types) | _ -> M.add id typ types in
    let ctxWithTypes = F.withTempTypes ctx typesWithBinding in
    let ctxNext = match operation with A.ClosureAlloc (func, _) -> F.addClosureFunc ctxWithTypes id func | _ -> ctxWithTypes in
    let shape = S.rcShapeForType ctx typ in
    let returned = R.returnedSet bodyInfo in
    let isPush func = functionHasName func "Darklang.Stdlib.List.__push_i64" || functionHasName func "Darklang.Stdlib.List.__pushBack_i64" in
    let immediatePush = match bodyInfo with
     | R.RLet (_, A.Call (func, _ :: A.Var value :: _), _, _) | R.RLet (_, A.TailCall (func, _ :: A.Var value :: _), _, _) -> isPush func && value = id
     | _ -> false in
    let skipMap = mapTransfersSecond && (match typ with AST.TList _ -> true | _ -> false) in
    let materialized = match operation with A.BorrowedCall _ -> true | _ -> false in
    let bindingDec = if S.bindingNeedsShapeAutomaticDec ctx operation typ shape && (not (R.isBorrowingExpr operation) || materialized) && not (S.cexprProducesNonRcSentinel operation) && not skipMap && not immediatePush then
     let kind = match typ with AST.TList (AST.TFunction _) -> Some MemoryModel.TaggedList | _ -> None in Some (C.createReturnDec ctx id typ shape kind) else None in
    let returnDecs', singlePending = match bindingDec with
     | Some dec when not (Set.mem id returned) -> if nestedRecordListDict && (match typ with AST.TDict _ -> true | _ -> false) then dec :: dec :: returnDecs, false else dec :: returnDecs, true
     | _ -> returnDecs, false in
    let rec tempSentinel target = match List.find_opt (fun frame -> frame.C.tempId = target) frames with
     | None -> false
     | Some frame -> match R.tryOwnershipPreservingAliasSource frame.C.cExpr with
       | Some source -> tempSentinel source
       | None -> match frame.C.cExpr with A.IfValue (_, yes, no) -> atomSentinel yes && atomSentinel no | operation -> S.cexprProducesNonRcSentinel operation
    and atomSentinel = function A.StringLiteral _ -> true | A.Var source -> tempSentinel source | _ -> false in
    let allocationTargets lookupCtx atoms = List.filter_map (function A.Var value -> (match F.tryGetType lookupCtx value with Some typ -> let shape = S.rcShapeForType ctx typ in if P.rcShapeNeedsBorrowedRetain shape && not (tempSentinel value) then Some (value, typ, shape) else None | None -> None) | _ -> None) atoms in
    let compoundTargets = match operation with
     | A.TupleAlloc atoms | A.ClosureAlloc (_, atoms) -> allocationTargets ctxWithTypes atoms
     | A.RecordAlloc (_, atoms) | A.RecordClone (_, _, atoms) | A.RecordReuse (_, _, _, atoms) -> allocationTargets ctx atoms
     | _ -> [] in
    let erasedTargets = match operation with
     | A.Call (func, [_; A.Var value]) | A.TailCall (func, [_; A.Var value]) when isPush func ->
       let immediateOwned = match frames with previous :: _ when previous.C.tempId = value -> not (R.isBorrowingExpr previous.C.cExpr) | _ -> false in
       if immediateOwned then [] else allocationTargets ctxWithTypes [A.Var value]
     | _ -> [] in
    let allocationIncTargets = compoundTargets @ erasedTargets in
    let rawSlotTargets = match operation with
     | A.RawSlotInit (_, _, _, AST.TStream _) -> []
     | A.RawSlotInit (_, _, A.Var value, typ) -> let shape = S.rcShapeForType ctx typ in if P.rcShapeNeedsBorrowedRetain shape && not (tempSentinel value) then [value, typ, shape] else []
     | _ -> [] in
    let transferableOwnership = match bindingDec, singlePending with Some dec, true when R.transfersIntoReturnedAggregate id bodyInfo || R.transfersIntoRawSlot id bodyInfo -> Some dec | _ -> None in
    let rec resolveOwner target = match List.find_opt (fun frame -> frame.C.tempId = target) frames with Some frame -> (match R.tryOwnershipPreservingAliasSource frame.C.cExpr with Some source -> resolveOwner source | None -> target) | None -> target in
    let transfers, _ = List.fold_left (fun (transfers, owners) (target, _, _) ->
     let owner = resolveOwner target in
     if Set.mem owner owners then transfers, owners else
     let local = List.find_map (fun candidate -> match candidate.C.transferableOwnership with Some dec when decId dec = owner -> Some dec | _ -> None) frames in
     let pending = match local with Some _ -> local | None -> List.find_opt (fun dec -> decId dec = owner) inheritedTransferable in
     match pending with Some dec -> (target, dec) :: transfers, Set.add owner owners | None -> transfers, owners) ([], Set.empty) (allocationIncTargets @ rawSlotTargets) in
    let transfers = List.rev transfers in
    let operationAfterTransfers = match operation with A.RawSlotInit (ptr, offset, A.Var value, _) when List.exists (fun (target, _) -> target = value) transfers -> A.RawWriteWord (ptr, offset, A.Var value) | _ -> operation in
    let transferredOwners = ids (List.map snd transfers) in
    let removeFirst predicate values =
     let rec loop prefix = function [] -> List.rev prefix | head :: tail when predicate head -> List.rev_append prefix tail | head :: tail -> loop (head :: prefix) tail in loop [] values in
    let allocationAfterTransfers = List.fold_left (fun targets (target, _) -> removeFirst (fun (candidate, _, _) -> candidate = target) targets) allocationIncTargets transfers in
    let removePending pending = List.fold_left (fun pending (_, target) -> removeFirst ((=) target) pending) pending transfers in
    let returnDecsAfter = removePending returnDecs' in
    let inheritedAfter = removePending inheritedTransferable in
    let framesAfter = if Set.is_empty transferredOwners then frames else List.map (fun candidate -> if Set.mem candidate.C.tempId transferredOwners then {candidate with C.transferableOwnership = None; branchDec = None} else candidate) frames in
    let retainedType = function A.Var value -> (match F.tryGetType ctx value with Some typ -> let shape = S.rcShapeForType ctx typ in if P.rcShapeNeedsBorrowedRetain shape then Some (typ, shape) else None | None -> None) | _ -> None in
    let ownedParent source =
     let rec loop visited candidate = if Set.mem candidate visited then false else match List.find_opt (fun frame -> frame.C.tempId = candidate) frames with
      | Some frame -> (match frame.C.cExpr with A.Atom (A.Var alias) | A.TypedAtom (A.Var alias, _) -> loop (Set.add candidate visited) alias | operation -> not (R.isBorrowingExpr operation))
      | None -> false in loop Set.empty source in
    let projectionFeedsSelf = match currentFunc, operation with Some func, A.TupleGet (A.Var source, _) | Some func, A.RecordGet (_, A.Var source, _) -> ownedParent source && C.isTempUsedAsSelfTailCallArg ctx func id bodyInfo | _ -> false in
    let returnInc = match operation with
     | A.BorrowedCall _ when P.rcShapeNeedsBorrowedRetain shape -> Some (typ, shape)
     | A.IfValue (_, yes, no) -> (match retainedType yes with Some _ as info -> info | None -> retainedType no)
     | _ when projectionFeedsSelf && P.rcShapeNeedsBorrowedRetain shape -> Some (typ, shape)
     | A.Atom (A.Var source) | A.TypedAtom (A.Var source, _) -> if P.rcShapeNeedsBorrowedRetain shape && Set.mem id returned && R.isBorrowingExpr operation && not (Set.mem source returned) then Some (typ, shape) else None
     | _ -> if P.rcShapeNeedsBorrowedRetain shape && Set.mem id returned && R.isBorrowingExpr operation then Some (typ, shape) else None in
    let reuseCleanup = match operationAfterTransfers with
     | A.RecordReuse (descriptor, _, A.Var source, _) -> let fields = List.filter_map (fun (index, (_, typ)) -> let shape = S.rcShapeForType ctx typ in if P.rcShapeNeedsOwnedScopeRelease shape then Some (index, typ, shape) else None) (List.mapi (fun index field -> index, field) descriptor.A.fields) in Some {C.descriptor; source; fields}
     | _ -> None in
    let frame = {C.tempId = id; cExpr = operationAfterTransfers; allocationIncTargets = allocationAfterTransfers; recordReuseCleanup = reuseCleanup; transferableOwnership; returnInc; branchDec = bindingDec} in
    descend ctxNext bodyInfo gen returnDecsAfter inheritedAfter (List.filter (fun dec -> not (Set.mem (decId dec) transferredOwners)) inheritedBranchDecs) (frame :: framesAfter) typesWithBinding in
 descend ctxWithTypes expr gen returnDecs inheritedTransferable inheritedBranchDecs [] types
(*
   Insert reference counting operations into an AExpr
   Returns (transformed expr, varGen, accumulated TempTypes)
*)
let insertRCInternal ctx expr gen types =
 let ctx = F.withTempTypes ctx types in
 let analyzed = R.analyzeReturns M.empty M.empty expr in
 insertRCWithAnalysis M.empty [] ctx None analyzed gen [] [] [] types
(*
   Insert reference counting operations into an AExpr
   Returns (transformed expr, varGen, accumulated TempTypes)
*)
let insertRC ctx expr gen = insertRCInternal ctx expr gen M.empty
