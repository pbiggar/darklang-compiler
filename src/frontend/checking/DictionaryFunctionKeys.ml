(* Keep function-bearing public keys an AOT boundary, proving empty payloads when possible. *)
[@@@warning "-4"]

module C = CheckedAST
open! C
open! Types
module M = StringOrder.Map
module S = StringOrder.Set
module T = HelperDependencies.TypeSet

let ( let* ) = Result.bind

let check knownFunctions symbols aliases records sums tops =
  let functions =
    List.fold_left
      (fun acc -> function
        | C.FunctionDef fn -> FunctionIdMap.add fn.C.id fn acc | _ -> acc)
      knownFunctions tops
  in
  let rec contains seen typ =
    let typ = Types.resolveType aliases typ in
    if T.mem typ seen then false
    else
      match typ with
      | AST.TFunction _ -> true
      | _ ->
          List.exists
            (contains (T.add typ seen))
            (DictionaryKeyValidation.children aliases records sums typ)
  in
  let hasFunction = contains T.empty in
  let rec bindPattern env pattern expr =
    match pattern with
    | C.LPVariable id -> C.BindingIdMap.add id expr env
    | C.LPTuple (a, b, rest) ->
        List.mapi (fun index pattern -> (index, pattern)) (a :: b :: rest)
        |> List.fold_left
             (fun env (index, pattern) ->
               bindPattern env pattern (C.TupleAccess (expr, index)))
             env
    | _ -> env
  in
  let rec safe seen env typ expr =
    let typ = Types.resolveType aliases typ in
    if not (hasFunction typ) then true
    else
      let same = safe seen env typ in
      match expr with
      | C.Local id -> (
          match C.BindingIdMap.find_opt id env with
          | Some value -> same value
          | None -> false)
      | C.Let (pattern, value, body) ->
          safe seen (bindPattern env pattern value) typ body
      | C.Sequence (_, body) -> same body
      | C.If (_, yes, no) -> same yes && same no
      | C.TypeApp (id, _, args)
        when C.functionName id symbols = Some "__dict_check_key" -> (
          match NonEmptyList.toList args with [ key ] -> same key | _ -> false)
      | C.Call (id, args) | C.TypeApp (id, _, args) -> (
          match FunctionIdMap.tryFind id functions with
          | Some fn when not (S.mem fn.C.name seen) ->
              let params = NonEmptyList.toList fn.C.params
              and args = NonEmptyList.toList args in
              if List.length params <> List.length args then false
              else
                let env =
                  List.fold_left2
                    (fun env (id, _) arg -> C.BindingIdMap.add id arg env)
                    env params args
                in
                safe (S.add fn.C.name seen) env typ fn.C.body
          | _ -> false)
      | C.TupleAccess (C.TupleLiteral values, index) -> (
          match List.nth_opt (C.tupleElementsToList values) index with
          | Some value -> same value
          | None -> false)
      | C.ListLiteral values -> (
          match typ with
          | AST.TList inner -> List.for_all (safe seen env inner) values
          | _ -> false)
      | C.TupleLiteral values -> (
          match typ with
          | AST.TTuple types ->
              let values = C.tupleElementsToList values in
              List.length types = List.length values
              && List.for_all2 (safe seen env) types values
          | _ -> false)
      | C.Constructor (reference, values) -> (
          match typ with
          | AST.TSum (name, args) -> (
              match M.find_opt name sums with
              | Some (info : Types.sumTypeInfo) -> (
                  let tag =
                    C.constructorTag reference.C.constructorId symbols
                  in
                  match
                    List.find_opt
                      (fun (v : Types.sumVariantInfo) -> Some v.tag = tag)
                      info.variants
                  with
                  | Some variant ->
                      let subst =
                        M.of_list (List.combine info.typeParams args)
                      in
                      let types =
                        List.map (Types.applyTypeArguments subst) variant.fields
                      in
                      List.length types = List.length values
                      && List.for_all2 (safe seen env) types values
                  | None -> false)
              | None -> false)
          | _ -> false)
      | C.RecordLiteral (_, values) -> (
          match typ with
          | AST.TRecord (name, args) -> (
              match M.find_opt name records with
              | Some (info : Types.recordTypeInfo) ->
                  let subst = M.of_list (List.combine info.typeParams args) in
                  List.for_all
                    (fun (id, value) ->
                      match C.fieldIndex id symbols with
                      | Some index -> (
                          match List.nth_opt info.fields index with
                          | Some (_, typ) ->
                              safe seen env
                                (Types.applyTypeArguments subst typ)
                                value
                          | None -> false)
                      | None -> false)
                    (C.recordFieldsInSourceOrder values)
              | None -> false)
          | _ -> false)
      | C.DictLiteral (_, _, entries) -> (
          match typ with
          | AST.TDict (key, value) ->
              List.for_all
                (fun (k, v) -> safe seen env key k && safe seen env value v)
                entries
          | _ -> false)
      | _ -> false
  in
  let functionKeyError =
    "Functions cannot be used as dictionary keys; function-bearing \
     containers must be provably empty of functions"
  in
  let require env typ value =
    if safe S.empty env typ value then Ok ()
    else Error functionKeyError
  in
  let rec visit active subst env expr =
    let many values =
      ResultList.traverse (visit active subst env) values
      |> Result.map (fun _ -> ())
    in
    match expr with
    | C.Let (pattern, value, body) ->
        let* () = visit active subst env value in
        visit active subst (bindPattern env pattern value) body
    | C.GenericFuncRef (id, typeArgs, _) -> (
        let types =
          List.map (Types.applyTypeArguments subst)
            (C.semanticTypeArgs typeArgs)
        in
        match FunctionIdMap.tryFind id functions with
        | Some fn
          when fn.C.typeParams <> []
               && not (S.mem fn.C.name active)
               && not
                    (String.starts_with ~prefix:"Darklang.Stdlib." fn.C.name)
               && List.length fn.C.typeParams = List.length types ->
            (* No arguments are known for a first-class function value. Any
               function-bearing key it constructs must be empty independently
               of those arguments, just as in an explicitly typed function. *)
            visit (S.add fn.C.name active)
              (M.of_list (List.combine fn.C.typeParams types))
              env fn.C.body
        | _ -> (
            match (C.functionName id symbols, types) with
            | ( Some
                  ( "Darklang.Stdlib.Dict.singleton" | "Dict.singleton"
                  | "Darklang.Stdlib.Dict.get" | "Darklang.Stdlib.Dict.set"
                  | "Darklang.Stdlib.Dict.setOverridingDuplicates"
                  | "Darklang.Stdlib.Dict.remove" | "Darklang.Stdlib.Dict.member"
                  | "Darklang.Stdlib.Dict.fromList"
                  | "Darklang.Stdlib.Dict.fromListOverwritingDuplicates" ),
                key :: _ )
              when hasFunction key -> Error functionKeyError
            | _ -> Ok ()))
    | C.TypeApp (id, types, args) -> (
        let name = Option.value (C.functionName id symbols) ~default:"" in
        let types =
          List.map (Types.applyTypeArguments subst) (C.semanticTypeArgs types)
        and args = NonEmptyList.toList args in
        let* () =
          match (name, types, args) with
          | "__dict_check_key", [ key ], [ value ] -> require env key value
          | ( ("Darklang.Stdlib.Dict.singleton" | "Dict.singleton"),
              key :: _,
              value :: _ ) ->
              require env key value
          | ( ( "Darklang.Stdlib.Dict.get" | "Darklang.Stdlib.Dict.set"
              | "Darklang.Stdlib.Dict.setOverridingDuplicates"
              | "Darklang.Stdlib.Dict.remove" | "Darklang.Stdlib.Dict.member" ),
              key :: _,
              _ :: value :: _ ) ->
              require env key value
          | ( ( "Darklang.Stdlib.Dict.fromList"
              | "Darklang.Stdlib.Dict.fromListOverwritingDuplicates" ),
              key :: _,
              [ entries ] ) ->
              require env (AST.TList (AST.TTuple [ key; AST.TUnit ])) entries
          | _ -> Ok ()
        in
        let* () = many args in
        match FunctionIdMap.tryFind id functions with
        | Some fn
          when fn.C.typeParams <> []
               && (not (S.mem fn.C.name active))
               && (not
                     (String.starts_with ~prefix:"Darklang.Stdlib." fn.C.name))
               && List.length fn.C.typeParams = List.length types ->
            let parameters = NonEmptyList.toList fn.C.params in
            if List.length parameters <> List.length args then Ok ()
            else
              let env =
                List.fold_left2
                  (fun env (id, _) value -> C.BindingIdMap.add id value env)
                  env parameters args
              in
              visit (S.add fn.C.name active)
                (M.of_list (List.combine fn.C.typeParams types))
                env fn.C.body
        | _ -> Ok ())
    | C.DictLiteral (key, _, entries) ->
        let* _ =
          ResultList.traverse
            (fun (k, _) ->
              require env
                (Types.applyTypeArguments subst (C.semanticType key))
                k)
            entries
        in
        many (List.concat_map (fun (k, v) -> [ k; v ]) entries)
    | C.Call (_, args) -> many (NonEmptyList.toList args)
    | C.TupleLiteral values -> many (C.tupleElementsToList values)
    | C.ListLiteral values | C.Constructor (_, values) -> many values
    | C.RecordLiteral (_, values) ->
        many (List.map snd (C.recordFieldsInSourceOrder values))
    | C.RecordUpdate (value, fields) -> many (value :: List.map snd fields)
    | C.If (a, b, c) -> many [ a; b; c ]
    | C.BinOp (_, a, b) | C.Sequence (a, b) | C.RecursiveLet (_, a, b) ->
        many [ a; b ]
    | C.UnaryOp (_, value)
    | C.TupleAccess (value, _)
    | C.RecordAccess (value, _)
    | C.BoundaryRender (_, value) ->
        visit active subst env value
    | C.Match (value, cases) ->
        many
          (value
          :: List.concat_map
               (fun (c : C.matchCase) -> c.body :: Option.to_list c.guard)
               (NonEmptyList.toList cases))
    | C.Apply (value, args) | C.IndirectApply (value, args) ->
        many (value :: NonEmptyList.toList args)
    | C.Lambda (_, _, body) -> visit active subst env body
    | C.Closure (_, values) -> many values
    | C.InterpolatedString parts ->
        many
          (List.filter_map
             (function C.StringExpr e -> Some e | _ -> None)
             parts)
    | _ -> Ok ()
  in
  let env =
    List.fold_left
      (fun env -> function
        | C.ValueDef v -> C.BindingIdMap.add v.C.id v.body env | _ -> env)
      C.BindingIdMap.empty tops
  in
  ResultList.traverse
    (function
      | C.FunctionDef fn
        when not (String.starts_with ~prefix:"Darklang.Stdlib." fn.C.name) ->
          visit S.empty M.empty env fn.C.body
      | C.Expression expr -> visit S.empty M.empty env expr
      | C.ValueDef v -> visit S.empty M.empty env v.C.body
      | _ -> Ok ())
    tops
  |> Result.map (fun _ -> ())
