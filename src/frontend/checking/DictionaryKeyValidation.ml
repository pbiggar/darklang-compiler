(* Validate occupied public-key payloads without changing private HAMT key semantics. *)
[@@@warning "-4"]
open AST
module M = StringOrder.Map
module S = HelperDependencies.TypeSet

let name typ = ComparisonPlanning.compareHelperName typ ^ "_public_key"

let children aliases records sums typ =
  let fields params args fields =
    let subst = M.of_list (List.combine params args) in
    List.map (fun typ -> Types.resolveType aliases (Types.applyTypeArguments subst typ)) fields
  in
  match Types.resolveType aliases typ with
  | TTuple types -> types
  | TList inner | TStream inner -> [inner]
  | TDict (key, value) -> [key; value]
  | TRecord (name, args) -> (match M.find_opt name records with
      | Some (info : Types.recordTypeInfo) -> fields info.typeParams args (List.map snd info.fields)
      | None -> [])
  | TSum (name, args) -> (match M.find_opt name sums with
      | Some (info : Types.sumTypeInfo) -> fields info.typeParams args (List.concat_map (fun (v : Types.sumVariantInfo) -> v.fields) info.variants)
      | None -> [])
  | _ -> []

let needsValidation aliases records sums typ =
  let rec inspect seen typ =
    let typ = Types.resolveType aliases typ in
    if S.mem typ seen then false else
    match typ with
    | TBlob | TStream _ | TFunction _ | TInternalRawPtr -> true
    | _ -> List.exists (inspect (S.add typ seen)) (children aliases records sums typ)
  in
  inspect S.empty typ

let generate aliases records sums roots =
  let needed = needsValidation aliases records sums in
  let call typ value = applyNamed (name typ) (NonEmptyList.singleton value) in
  let rec ensure typ generated =
    let typ = Types.resolveType aliases typ in
    if not (needed typ) || M.mem (name typ) generated then generated else
    let definition : AST.functionDef = {
      name = name typ; typeParams = []; params = NonEmptyList.singleton ("__key", typ);
      returnType = typ; body = Var "__key"; recursion = None
    } in
    let generated = M.add definition.name definition generated in
    let childTypes = match typ with TDict (k,v) -> [TList (TTuple [k;v])] | _ -> children aliases records sums typ in
    let generated = List.fold_left (fun current typ -> ensure typ current) generated childTypes in
    let validate typ value = if needed typ then Let (LPWildcard, call typ value, UnitLiteral) else UnitLiteral in
    let sequence values = List.fold_right (fun value rest -> Sequence (value, rest)) values UnitLiteral in
    let case = EqualityHelpers.makeSimpleMatchCase in
    let value = Var "__key" in
    let body = match typ with
      | TBlob | TStream _ | TFunction _ | TInternalRawPtr -> RuntimeError "Unsupported dictionary key type"
      | TList inner -> Match (value, [case (PList []) UnitLiteral;
          case (PListCons ([PVar "__head"], PVar "__tail"))
            (sequence [validate inner (Var "__head"); validate typ (Var "__tail")])])
      | TTuple types -> sequence (List.mapi (fun index typ -> validate typ (TupleAccess (value, index))) types)
      | TRecord (record, args) -> (match M.find_opt record records with
          | None -> Crash.crash ("Missing public key record " ^ record)
          | Some (info : Types.recordTypeInfo) ->
              let subst = M.of_list (List.combine info.typeParams args) in
              sequence (List.mapi (fun index (_, typ) -> validate (Types.applyTypeArguments subst typ) (TupleAccess (value, index))) info.fields))
      | TSum (sum, args) -> (match M.find_opt sum sums with
          | None -> Crash.crash ("Missing public key enum " ^ sum)
          | Some (info : Types.sumTypeInfo) ->
              let subst = M.of_list (List.combine info.typeParams args) in
              Match (value, List.map (fun (variant : Types.sumVariantInfo) ->
                let names = List.mapi (fun index _ -> "__field" ^ string_of_int index) variant.fields in
                case (PResolvedConstructor (sum, variant.name, variant.tag, List.map (fun name -> PVar name) names))
                  (sequence (List.map2 (fun typ name -> validate (Types.applyTypeArguments subst typ) (Var name)) variant.fields names))) info.variants))
      | TDict (key, item) -> validate (TList (TTuple [key;item]))
          (applyNamedWithTypes "Darklang.Stdlib.Dict.__entries" [key;item] (NonEmptyList.singleton value))
      | _ -> UnitLiteral
    in
    M.add definition.name {definition with body = Sequence (body, value)} generated
  in
  S.fold ensure roots M.empty
