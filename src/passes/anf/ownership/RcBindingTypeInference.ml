(* RcBindingTypeInference.ml - Recover source binding types before SSA freshening. *)
[@@@warning "-4"]

module A = ANF
module F = RcTypeFacts
module R = RcReturnAnalysis
module S = RcShapePlanning

let tryItem index values = if index < 0 then None else List.nth_opt values index

(*
   Recover a binding's type without relying on ownership rewrites. Some raw
   projections carry their concrete type only at the next use site.
*)
let inferBindingType ctx tempId cexpr bodyInfo =
  let (A.TempId index) = tempId in
  let rec inferAliasedVarTypeFromUse aliased next =
    let inferFromCall func args =
      match FunctionIdMap.tryFind func ctx.F.funcReg with
      | Some (_, AST.TFunction (params, _)) ->
          List.find_map
            (fun (index, atom) ->
              match (atom, tryItem index params) with
              | A.Var id, Some typ when id = aliased -> Some typ
              | _ -> None)
            (List.mapi (fun index atom -> (index, atom)) args)
      | _ -> None
    in
    match next with
    | R.RLet (_, A.RawSlotInit (_, _, A.Var value, typ), _, _)
      when value = aliased ->
        Some typ
    | R.RLet (_, A.Call (func, args), _, _)
    | R.RLet (_, A.BorrowedCall (func, args), _, _)
    | R.RLet (_, A.TailCall (func, args), _, _) ->
        inferFromCall func args
    | R.RLet (id, A.Atom (A.Var source), body, _) when source = aliased ->
        inferAliasedVarTypeFromUse id body
    | R.RLet (id, A.TypedAtom (A.Var source, typ), body, _)
      when source = aliased ->
        if S.shapeNeedsManagedAliasRootPreservation ctx typ then Some typ
        else inferAliasedVarTypeFromUse id body
    | R.RIf (A.Var id, _, _, _) when id = aliased -> Some AST.TBool
    | _ -> None
  in
  match F.inferCExprType ctx cexpr with
  | Some typ -> (
      let aliasType =
        match bodyInfo with
        | R.RLet (_, A.TypedAtom (A.Var source, typ), _, _) when source = tempId
          ->
            Some typ
        | R.RLet (id, A.Atom (A.Var source), body, _) when source = tempId ->
            inferAliasedVarTypeFromUse id body
        | _ -> None
      in
      match aliasType with
      | Some alias
        when S.shapeNeedsManagedAliasRootPreservation ctx alias
             && not (S.shapeNeedsManagedAliasRootPreservation ctx typ) ->
          alias
      | _ -> typ)
  | None -> (
      let rawType = AST.TVar ("raw_get_" ^ string_of_int index) in
      match (cexpr, bodyInfo) with
      | A.TupleGet _, R.RLet (_, A.TypedAtom (A.Var source, typ), _, _)
        when source = tempId ->
          typ
      | A.RawGet (_, _, None), R.RLet (_, A.TypedAtom (A.Var source, typ), _, _)
        when source = tempId ->
          typ
      | A.RawGet (_, _, None), R.RLet (id, A.Atom (A.Var source), body, _)
        when source = tempId ->
          Option.value ~default:rawType (inferAliasedVarTypeFromUse id body)
      | A.RawGet (_, _, None), _ -> rawType
      | _ -> AST.TVar ("inferred_" ^ string_of_int index))
