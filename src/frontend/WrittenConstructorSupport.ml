(* Constructor ownership, alias inference, and ordered checked payloads. *)
[@@@warning "-4"]

module WT = WrittenTypes
module C = CheckedAST
open! WT
open! C
module M = StringOrder.Map
module S = StringOrder.Set
open! WrittenTypeSupport

let bind = Result.bind
let map = Result.map

let check checkExpression literal globals locals symbols expected
    (typeName : WT.qualifiedTypeIdentifier) caseName fields =
  let check = checkExpression globals locals in
  let checkFields symbols fields types =
    let rec loop symbols reversed fields types =
      match fields with
      | [] -> Ok (List.rev reversed, symbols)
      | field :: tail ->
          let expected, rest =
            match types with
            | head :: tail -> (Some head, tail)
            | [] -> (None, [])
          in
          bind (check symbols expected field) (fun (typ, value, symbols) ->
              loop symbols ((typ, value) :: reversed) tail rest)
    in
    loop symbols [] fields types
  in
  let findCase cases =
    List.find_opt
      (fun (_, (variant : WT.enumCaseSyntax)) -> snd variant.name = caseName)
      cases
  in
  let variantTypes entry (variant : WT.enumCaseSyntax) =
    ResultList.traverse
      (fun (field : WT.enumFieldSyntax) ->
        resolveWrittenType globals.allowInternal globals.types entry.path
          (S.of_list entry.params) field.typ)
      variant.fields
  in
  let rec fieldsForInference canonical (entry : typeEntry) =
    match entry.definition with
    | WT.TDEnum cases -> (
        match findCase cases with
        | None ->
            Error ("Unknown constructor '" ^ canonical ^ "." ^ caseName ^ "'")
        | Some (_, variant)
          when List.length variant.fields <> List.length fields ->
            Error
              ("Constructor '" ^ canonical ^ "." ^ caseName ^ "' expects "
              ^ string_of_int (List.length variant.fields)
              ^ " fields")
        | Some (_, variant) ->
            map Option.some (variantTypes entry variant))
    | WT.TDAlias target ->
        bind
          (resolveWrittenType globals.allowInternal globals.types entry.path
             (S.of_list entry.params) target)
          (function
            | AST.TSum (target, args) -> (
                match M.find_opt target globals.types with
                | Some entry when List.length entry.params = List.length args ->
                    let subst = M.of_list (List.combine entry.params args) in
                    map
                      (Option.map (List.map (Types.applyTypeArguments subst)))
                      (fieldsForInference target entry)
                | _ -> Error ("Unknown enum type '" ^ target ^ "'"))
            | _ -> Error "Expected an enum type")
    | _ -> Ok None
  in
  let resolved =
    if typeName.typ.name <> "" then
      let inferred =
        bind (findNamedType globals typeName) (fun (canonical, entry) ->
            bind (fieldsForInference canonical entry) (fun types ->
                if typeName.typeArgs <> [] || entry.params = [] then Ok None
                else
                  match types with
                  | None -> Ok None
                  | Some types ->
                      bind (checkFields symbols fields []) (fun (checked, _) ->
                          map Option.some
                            (Unification.inferTypeArgs entry.params types
                               (List.map fst checked) None None))))
      in
      bind inferred (resolveNamedType globals expected typeName)
    else
      match expected with
      | Some (AST.TSum (canonical, args)) -> (
          match M.find_opt canonical globals.types with
          | Some entry -> Ok (canonical, entry, args)
          | None -> Error ("Unknown type '" ^ canonical ^ "'"))
      | _ -> (
          let candidates =
            List.filter
              (fun (canonical, (entry : typeEntry)) ->
                (not
                   (restrictedIdentifier globals.allowInternal
                      (String.split_on_char '.' canonical)))
                &&
                match entry.definition with
                | WT.TDEnum cases -> Option.is_some (findCase cases)
                | _ -> false)
              (M.bindings globals.types)
          in
          match candidates with
          | [ (canonical, entry) ] -> (
              match entry.definition with
              | WT.TDEnum cases -> (
                  match findCase cases with
                  | None -> Error ("Unknown constructor '" ^ caseName ^ "'")
                  | Some (_, variant)
                    when List.length variant.fields <> List.length fields ->
                      Error
                        ("Constructor '" ^ caseName ^ "' expects "
                        ^ string_of_int (List.length variant.fields)
                        ^ " fields")
                  | Some (_, variant) ->
                      bind (variantTypes entry variant) (fun types ->
                          bind (checkFields symbols fields [])
                            (fun (checked, _) ->
                              map
                                (fun args -> (canonical, entry, args))
                                (Unification.inferTypeArgs entry.params types
                                   (List.map fst checked) None None))))
              | _ -> Error ("Type '" ^ canonical ^ "' is not an enum"))
          | [] -> Error ("Unknown constructor '" ^ caseName ^ "'")
          | _ ->
              Error
                ("Constructor '" ^ caseName ^ "' requires an expected enum type")
          )
  in
  bind resolved (fun (canonical, entry, args) ->
      match entry.definition with
      | WT.TDEnum cases -> (
          match
            List.find_opt
              (fun (_, (_, (variant : WT.enumCaseSyntax))) ->
                snd variant.name = caseName)
              (List.mapi (fun ordinal value -> (ordinal, value)) cases)
          with
          | None ->
              Error ("Unknown constructor '" ^ canonical ^ "." ^ caseName ^ "'")
          | Some (_, (_, variant))
            when List.length fields <> List.length variant.fields ->
              Error
                ("Constructor '" ^ canonical ^ "." ^ caseName ^ "' expects "
                ^ string_of_int (List.length variant.fields)
                ^ " fields")
          | Some (ordinal, (_, variant)) ->
              let subst = M.of_list (List.combine entry.params args) in
              bind
                (map
                   (List.map (Types.applySubst subst))
                   (variantTypes entry variant))
                (fun types ->
                  bind (checkFields symbols fields types)
                    (fun (checked, symbols) ->
                      let inference =
                        List.fold_left
                          (fun result (declared, actual) ->
                            bind result (fun subst ->
                                map
                                  (fun inferred ->
                                    M.union
                                      (fun _ _ newer -> Some newer)
                                      subst inferred)
                                  (Unification.unifyTypes
                                     (Types.applySubst subst declared)
                                     actual)))
                          (Ok M.empty)
                          (List.combine types (List.map fst checked))
                      in
                      bind inference (fun subst ->
                          let args = List.map (Types.applySubst subst) args in
                          let constructorId, symbols =
                            WrittenCheckingState.internConstructor canonical
                              caseName
                              (caseTag globals.collidingCases canonical caseName
                                 ordinal)
                              symbols
                          in
                          let typeId, symbols =
                            WrittenCheckingState.internType canonical symbols
                          in
                          let reference : C.constructorReference =
                            {
                              typeId;
                              constructorId;
                              typeArgs = List.map C.checkedType args;
                            }
                          in
                          literal expected symbols
                            (AST.TSum (canonical, args))
                            (C.Constructor (reference, List.map snd checked)))))
          )
      | _ -> Error ("Type '" ^ canonical ^ "' is not an enum"))
