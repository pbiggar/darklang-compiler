(* Direct checking of tuples, lists, dictionaries, and match arms. *)
[@@@warning "-4"]

module WT = WrittenTypes
module C = CheckedAST
open! C
module M = StringOrder.Map

let bind = Result.bind
let map = Result.map

let orElse first fallback =
  match first with Some _ -> first | None -> fallback

let tuple checkExpression literal globals locals symbols expected elements =
  let types =
    match expected with
    | Some (AST.TTuple types) when List.length types = List.length elements ->
        List.map Option.some types
    | _ -> []
  in
  let rec checkMany symbols reversed elements types =
    match elements with
    | [] -> Ok (List.rev reversed, symbols)
    | item :: tail ->
        let expected, rest =
          match types with head :: tail -> (head, tail) | [] -> (None, [])
        in
        bind (checkExpression globals locals symbols expected item)
          (fun (typ, expr, symbols) ->
            checkMany symbols ((typ, expr) :: reversed) tail rest)
  in
  bind (checkMany symbols [] elements types) (fun (elements, symbols) ->
      match elements with
      | (_, first) :: (_, second) :: rest ->
          literal expected symbols
            (AST.TTuple (List.map fst elements))
            (C.TupleLiteral { C.first; second; rest = List.map snd rest })
      | _ -> Error "Tuple syntax must contain at least two expressions")

let list checkExpression literal globals locals symbols expected contents =
  let check = checkExpression globals locals in
  let elementExpected =
    match expected with Some (AST.TList typ) -> Some typ | _ -> None
  in
  let initial =
    match elementExpected with
    | Some typ when not (Unification.containsTVar typ) -> Some typ
    | _ ->
        List.find_map
          (fun (item, _) ->
            match check symbols None item with
            | Ok (typ, _, _)
              when typ <> AST.TNever && not (Unification.containsTVar typ) ->
                Some typ
            | _ -> None)
          contents
  in
  let rec elements symbols inferred reversed = function
    | [] -> Ok (List.rev reversed, inferred, symbols)
    | (item, _) :: tail ->
        bind (check symbols inferred item) (fun (typ, expr, symbols) ->
            let next =
              match inferred with
              | Some current -> Unification.reconcileTypes None current typ
              | None -> Some typ
            in
            match next with
            | None -> Error "List elements must have the same type"
            | Some typ -> elements symbols (Some typ) (expr :: reversed) tail)
  in
  bind (elements symbols initial [] contents)
    (fun (elements, inferred, symbols) ->
      match orElse inferred elementExpected with
      | None ->
          literal expected symbols
            (AST.TList (AST.TVar Unification.emptyListElementVar))
            (C.ListLiteral [])
      | Some typ ->
          literal expected symbols (AST.TList typ) (C.ListLiteral elements))

let dict checkExpression literal globals locals symbols expected entries =
  let check = checkExpression globals locals in
  let types =
    match expected with
    | Some (AST.TDict (key, value)) -> Some (key, value)
    | _ -> None
  in
  let rec entriesLoop symbols types reversed = function
    | [] -> Ok (types, List.rev reversed, symbols)
    | (_, key, _, value) :: tail ->
        bind
          (check symbols (Option.map fst types) key)
          (fun (keyType, checkedKey, symbols) ->
            let repeated =
              match checkedKey with
              | C.StringLiteral text
                when List.exists
                       (fun (key, _) ->
                         match key with
                         | C.StringLiteral existing -> text = existing
                         | _ -> false)
                       reversed ->
                  Some text
              | _ -> None
            in
            match repeated with
            | Some text -> Error ("Duplicate dictionary key \"" ^ text ^ "\"")
            | None ->
                bind
                  (check symbols (Option.map snd types) value)
                  (fun (valueType, checkedValue, symbols) ->
                    match types with
                    | None ->
                        entriesLoop symbols
                          (Some (keyType, valueType))
                          ((checkedKey, checkedValue) :: reversed)
                          tail
                    | Some (wantedKey, wantedValue) -> (
                        match
                          ( Unification.reconcileTypes None wantedKey keyType,
                            Unification.reconcileTypes None wantedValue
                              valueType )
                        with
                        | Some key, Some value ->
                            entriesLoop symbols
                              (Some (key, value))
                              ((checkedKey, checkedValue) :: reversed)
                              tail
                        | _ ->
                            Error
                              "Dictionary entries must have the same key and \
                               value types")))
  in
  bind (entriesLoop symbols types [] entries) (fun (types, entries, symbols) ->
      let key, value =
        Option.value types ~default:(AST.TVar "dictKey", AST.TVar "dictValue")
      in
      literal expected symbols
        (AST.TDict (key, value))
        (C.DictLiteral (C.checkedType key, C.checkedType value, entries)))

let matchExpression checkExpression literal globals locals symbols expected
    scrutinee cases =
  bind (checkExpression globals locals symbols None scrutinee)
    (fun (scrutineeType, checkedScrutinee, symbols) ->
      let armLocals bindings =
        M.union (fun _ _ newer -> Some newer) locals bindings
      in
      let resultHint =
        match expected with
        | Some _ -> expected
        | None ->
            List.find_map
              (fun (arm : WT.matchCase) ->
                match
                  WrittenPatternSupport.checkMatchPattern globals symbols None
                    scrutineeType arm.WT.pat
                with
                | Error _ -> None
                | Ok (_, bindings, afterPattern) -> (
                    match
                      checkExpression globals (armLocals bindings) afterPattern
                        None arm.WT.rhs
                    with
                    | Ok (typ, _, _) -> Some typ
                    | Error _ -> None))
              cases
      in
      let rec checkCases symbols resultType reversed = function
        | [] -> (
            match resultType with
            | None -> Error "Match expression must have at least one case"
            | Some typ -> Ok (typ, List.rev reversed, symbols))
        | (arm : WT.matchCase) :: tail ->
            bind
              (WrittenPatternSupport.checkMatchPattern globals symbols None
                 scrutineeType arm.WT.pat) (fun (pattern, bindings, symbols) ->
                let locals = armLocals bindings in
                let guardResult =
                  match arm.WT.whenCondition with
                  | None -> Ok (None, symbols)
                  | Some (_, guard) ->
                      map
                        (fun (_, expr, symbols) -> (Some expr, symbols))
                        (checkExpression globals locals symbols (Some AST.TBool)
                           guard)
                in
                bind guardResult (fun (guard, symbols) ->
                    let bodyExpected =
                      orElse
                        (Option.bind resultType (fun typ ->
                             if typ = AST.TNever then None else Some typ))
                        resultHint
                    in
                    bind
                      (checkExpression globals locals symbols bodyExpected
                         arm.WT.rhs) (fun (bodyType, body, symbols) ->
                        let joined =
                          match (resultType, bodyType) with
                          | Some AST.TNever, typ -> Ok typ
                          | Some typ, AST.TNever -> Ok typ
                          | None, typ -> Ok typ
                          | Some typ, other -> (
                              match
                                Unification.reconcileTypes None typ other
                              with
                              | Some typ -> Ok typ
                              | None ->
                                  Error "Match arm result types do not agree")
                        in
                        bind joined (fun typ ->
                            let patterns =
                              match pattern with
                              | C.POr alternatives -> alternatives
                              | other -> NonEmptyList.singleton other
                            in
                            let arm : C.matchCase = { patterns; guard; body } in
                            checkCases symbols (Some typ) (arm :: reversed) tail))))
      in
      bind (checkCases symbols None [] cases) (fun (typ, cases, symbols) ->
          if
            not
              (WrittenPatternSupport.matchIsExhaustive globals symbols
                 scrutineeType checkedScrutinee cases)
          then Error "Non-exhaustive match expression"
          else
            match NonEmptyList.tryFromList cases with
            | None -> Error "Match expression must have at least one case"
            | Some arms ->
                literal expected symbols typ (C.Match (checkedScrutinee, arms))))
