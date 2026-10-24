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
  (* The interpreter assigns these bare names before type inference, so an
     expected type or a colliding enum cannot change their owner. *)
  let typeName =
    match (typeName.modules, typeName.typ.name, caseName) with
    | [], "", ("Some" | "None" | "Ok" | "Error") ->
        let owner =
          match caseName with "Some" | "None" -> "Option" | _ -> "Result"
        in
        {
          typeName with
          modules =
            List.map
              (fun name ->
                ( ({ range = typeName.range; name } : WT.identifier),
                  typeName.range ))
              [ "Stdlib"; owner ];
          typ = { typeName.typ with name = owner };
        }
    | _ -> typeName
  in
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
  let inferArgs canonical params types checked =
    (* Nested constructors must not bind each other's declaration parameters. *)
    let freshParams, renaming =
      CheckingDiagnostics.freshenTypeParams (Some canonical) params
    in
    Unification.inferTypeArgs freshParams
      (List.map (CheckingDiagnostics.applyTypeVarRenaming renaming) types)
      (List.map fst checked) None None
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
        | Some (_, variant) -> map Option.some (variantTypes entry variant))
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
                let hasExpectedOwner =
                  match expected with
                  | Some (AST.TSum (owner, _)) -> owner = canonical
                  | _ -> false
                in
                if
                  typeName.typeArgs <> [] || entry.params = []
                  || hasExpectedOwner
                then Ok None
                else
                  match types with
                  | None -> Ok None
                  | Some types ->
                      let context, symbols =
                        WrittenCheckingState.freshenType S.empty
                          (AST.TTuple types) symbols
                      in
                      let contexts =
                        match context with
                        | AST.TTuple values -> values
                        | _ -> Crash.crash "Constructor context tuple"
                      in
                      bind (checkFields symbols fields contexts)
                        (fun (checked, _) ->
                          map Option.some
                            (inferArgs canonical entry.params types checked))))
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
                          let context, symbols =
                            WrittenCheckingState.freshenType S.empty
                              (AST.TTuple types) symbols
                          in
                          let contexts =
                            match context with
                            | AST.TTuple values -> values
                            | _ -> Crash.crash "Constructor context tuple"
                          in
                          bind (checkFields symbols fields contexts)
                            (fun (checked, _) ->
                              map
                                (fun args -> (canonical, entry, args))
                                (inferArgs canonical entry.params types checked)))
                  )
              | _ -> Error ("Type '" ^ canonical ^ "' is not an enum"))
          | [] -> Error ("Unknown constructor '" ^ caseName ^ "'")
          | _ ->
              Error
                ("Constructor '" ^ caseName ^ "' requires an expected enum type")
          )
  in
  bind resolved (fun (canonical, entry, args) ->
      (* A nullary constructor can leave parameters unconstrained. Register
         those variables so later branches refine this constructor's layout too. *)
      let typ, symbols =
        WrittenCheckingState.freshenType globals.typeParams
          (AST.TSum (canonical, args))
          symbols
      in
      let args =
        match typ with
        | AST.TSum (_, args) -> args
        | _ -> Crash.crash "Fresh constructor type is not an enum"
      in
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
                   (List.map (Types.applyTypeArguments subst))
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
                          (* Context can fill phantom parameters that payload
                             inference leaves empty; lowering needs that same
                             result type to choose the constructor layout. *)
                          map
                            (fun (typ, value, symbols) ->
                              match (typ, value) with
                              | ( AST.TSum (_, args),
                                  C.Constructor (reference, fields) ) ->
                                  ( typ,
                                    C.Constructor
                                      ( {
                                          reference with
                                          C.typeArgs =
                                            List.map C.checkedType args;
                                        },
                                        fields ),
                                    symbols )
                              | _ ->
                                  Crash.crash
                                    "Checked constructor lost its enum result \
                                     type")
                            (literal expected symbols
                               (AST.TSum (canonical, args))
                               (C.Constructor (reference, List.map snd checked))))))
          )
      | _ -> Error ("Type '" ^ canonical ^ "' is not an enum"))
