(* TraitResolution.ml - Deterministic static trait selection and recursive obligations. *)
[@@@warning "-4"]

module T = TraitCatalog
module M = StringOrder.Map

let bind = Result.bind

let substitute substitution (name, args) =
  (name, List.map (Types.applySubst substitution) args)

let matchArguments patterns actuals =
  if List.length patterns <> List.length actuals then None
  else
    match Unification.matchTypes (AST.TTuple patterns) (AST.TTuple actuals) with
    | Error _ -> None
    | Ok bindings ->
        List.fold_left
          (fun result (name, typ) ->
            Option.bind result (fun substitution ->
                match M.find_opt name substitution with
                | None -> Some (M.add name typ substitution)
                | Some existing when existing = typ -> Some substitution
                | Some _ -> None))
          (Some M.empty) bindings

let rec candidates catalog stack ((traitName, args) as obligation) =
  if List.exists Unification.containsTVar args then
    Error ("Unresolved trait obligation '" ^ traitName ^ "'")
  else if List.mem obligation stack then
    Error ("Cyclic trait obligation '" ^ traitName ^ "'")
  else
    let stack = obligation :: stack in
    let matches =
      M.bindings catalog.T.implementations
      |> List.filter_map (fun (_, (implementation : T.implementation)) ->
          if implementation.T.traitName <> traitName then None
          else
            Option.map
              (fun substitution -> (implementation, substitution))
              (matchArguments implementation.T.typeArgs args))
    in
    let applicable =
      List.fold_left
        (fun result (implementation, substitution) ->
          bind result (fun (accepted, rejected) ->
              let check =
                List.fold_left
                  (fun result bound ->
                    bind result (fun () ->
                        satisfy catalog stack (substitute substitution bound)))
                  (Ok ()) implementation.T.bounds
              in
              match check with
              | Ok () ->
                  Ok ((implementation, substitution) :: accepted, rejected)
              | Error error -> Ok (accepted, error :: rejected)))
        (Ok ([], []))
        matches
    in
    bind applicable (fun (accepted, rejected) ->
        let specific =
          List.filter
            (fun ((implementation : T.implementation), _) ->
              match implementation.T.typeArgs with
              | AST.TVar _ :: _ -> false
              | _ -> true)
            accepted
        in
        let accepted = if specific = [] then accepted else specific in
        match accepted with
        | [] -> (
            match rejected with
            | error :: _
              when String.starts_with ~prefix:"Cyclic trait obligation" error ->
                Error error
            | _ -> Error ("No implementation of trait '" ^ traitName ^ "'"))
        | [ candidate ] -> Ok candidate
        | _ -> Error ("Ambiguous implementation of trait '" ^ traitName ^ "'"))

and satisfy catalog stack obligation =
  Result.map (fun _ -> ()) (candidates catalog stack obligation)

let check catalog available obligation =
  if List.mem obligation available then Ok () else satisfy catalog [] obligation

let checkBounds catalog available substitution bounds =
  List.fold_left
    (fun result obligation ->
      bind result (fun () ->
          check catalog available (substitute substitution obligation)))
    (Ok ()) bounds

let functionBounds catalog available name types =
  match M.find_opt name catalog.T.functionBounds with
  | None -> Ok ()
  | Some (params, bounds) ->
      if List.length params <> List.length types then
        Error "Trait-bound type argument count mismatch"
      else
        checkBounds catalog available
          (M.of_list (List.combine params types))
          bounds

let select catalog (dispatch : T.dispatch) types =
  if List.length types < dispatch.T.arity then
    Error "Trait method type argument count mismatch"
  else
    let rec split count reversed remaining =
      if count = 0 then (List.rev reversed, remaining)
      else
        match remaining with
        | head :: tail -> split (count - 1) (head :: reversed) tail
        | [] -> Crash.crash "Checked trait method argument arity changed"
    in
    let args, methodArgs = split dispatch.T.arity [] types in
    bind
      (candidates catalog [] (dispatch.T.traitName, args))
      (fun (implementation, substitution) ->
        match M.find_opt dispatch.T.methodName implementation.T.methods with
        | None -> Crash.crash "Validated trait implementation lost a method"
        | Some target ->
            bind
              (ResultList.traverse
                 (fun param ->
                   match M.find_opt param substitution with
                   | Some typ -> Ok typ
                   | None ->
                       Error
                         ("Unresolved implementation type parameter '" ^ param
                        ^ "'"))
                 implementation.T.typeParams)
              (fun inherited -> Ok (target.T.name, inherited @ methodArgs)))

let rec checkType catalog available typ =
  if M.is_empty catalog.T.typeBounds then Ok ()
  else
    let many types =
      List.fold_left
        (fun result typ ->
          bind result (fun () -> checkType catalog available typ))
        (Ok ()) types
    in
    match typ with
    | AST.TRecord (name, args) | AST.TSum (name, args) ->
        bind (many args) (fun () ->
            match M.find_opt name catalog.T.typeBounds with
            | None -> Ok ()
            | Some (params, bounds) ->
                if List.length params <> List.length args then
                  Error "Bounded type argument count mismatch"
                else
                  checkBounds catalog available
                    (M.of_list (List.combine params args))
                    bounds)
    | AST.TList inner | AST.TStream inner -> checkType catalog available inner
    | AST.TTuple types -> many types
    | AST.TDict (key, value) -> many [ key; value ]
    | AST.TFunction (parameters, result) -> many (result :: parameters)
    | _ -> Ok ()
