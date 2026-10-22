(* SourceContracts.ml - Compare semantic signatures and public type shapes of selected replacements. *)
module W = WrittenTypes
module M = StringOrder.Map
module S = StringOrder.Set

type context = {
  types : WrittenTypeSupport.typeInventory;
  traits : S.t;
  environment : WrittenChecking.environment option;
}

let emptyContext = { types = M.empty; traits = S.empty; environment = None }
let ( let* ) = Result.bind

let parameters declared references =
  List.fold_left WrittenTypeSupport.collectWrittenTypeParams declared references

let normalize names typ =
  Types.applyTypeArguments
    (List.mapi
       (fun index name -> (name, AST.TVar ("#" ^ string_of_int index)))
       names
    |> M.of_list)
    typ

let parameterType = function
  | W.FPUnit _ -> W.TUnit W.synthRange
  | W.FPNormal (_, _, typ, _, _, _, _) -> typ

let effects names =
  Option.map
    (fun names ->
      List.map (fun (name : W.identifier) -> name.W.name) names
      |> List.sort_uniq String.compare)
    names

let bounds types traits path names bounds =
  let resolve typ =
    let* typ =
      WrittenTypeSupport.resolveWrittenType true types path (S.of_list names)
        typ
    in
    Ok (normalize names typ)
  in
  let traitName name =
    let name =
      if String.starts_with ~prefix:"Stdlib." name then "Darklang." ^ name
      else name
    in
    let rec candidates path =
      let candidate =
        String.concat "." (path @ String.split_on_char '.' name)
      in
      if S.mem candidate traits then candidate
      else
        match List.rev path with
        | [] -> name
        | _ :: rest -> candidates (List.rev rest)
    in
    candidates path
  in
  ResultList.traverse
    (fun (bound : W.typeParamBound) ->
      let* args = ResultList.traverse resolve bound.W.trait_.W.typeArgs in
      let param = normalize names (AST.TVar bound.W.param) in
      Ok (param, traitName (WrittenSource.qualifiedType bound.W.trait_), args))
    bounds
  |> Result.map (List.sort_uniq Stdlib.compare)

let functionContract types traits path (fn : W.fnDecl) =
  let refs = List.map parameterType fn.W.parameters @ [ fn.W.returnType ] in
  let names = parameters (List.map fst fn.W.typeParams) refs in
  let* signature =
    ResultList.traverse
      (fun typ ->
        let* typ =
          WrittenTypeSupport.resolveWrittenType true types path
            (S.of_list names) typ
        in
        Ok (normalize names typ))
      refs
  in
  let* bounds = bounds types traits path names fn.W.bounds in
  Ok (List.length names, signature, bounds, effects fn.W.effects)

type shape =
  | Alias of AST.semanticType
  | Record of (string * AST.semanticType) list
  | Enum of (string * (string option * AST.semanticType) list) list

let typeContract types traits path (declaration : W.typeDecl) =
  let names = List.map fst declaration.W.typeParams in
  let resolve typ =
    let* typ =
      WrittenTypeSupport.resolveWrittenType true types path (S.of_list names)
        typ
    in
    Ok (normalize names typ)
  in
  let* shape =
    match declaration.W.definition with
    | W.TDAlias typ -> Result.map (fun typ -> Alias typ) (resolve typ)
    | W.TDRecord fields ->
        let* fields =
          ResultList.traverse
            (fun ((field : W.recordFieldSyntax), _) ->
              let* typ = resolve field.W.typ in
              Ok (snd field.W.name, typ))
            fields
        in
        Ok (Record fields)
    | W.TDEnum cases ->
        let* cases =
          ResultList.traverse
            (fun (_, (case : W.enumCaseSyntax)) ->
              let* fields =
                ResultList.traverse
                  (fun (field : W.enumFieldSyntax) ->
                    let* typ = resolve field.W.typ in
                    Ok (Option.map snd field.W.label, typ))
                  case.W.fields
              in
              Ok (snd case.W.name, fields))
            cases
        in
        Ok (Enum cases)
  in
  let* bounds = bounds types traits path names declaration.W.bounds in
  Ok (List.length names, shape, bounds)

let initializerType context sources path body =
  let expression = W.DExpr body in
  let declarations =
    if path = [] then [ expression ]
    else
      [
        W.DModule
          {
            W.range = W.synthRange;
            name = (W.synthRange, String.concat "." path);
            declarations = [ expression ];
            keywordModule = W.synthRange;
          };
      ]
  in
  let tree = { W.range = W.synthRange; declarations; exprsToEval = [] } in
  let* unit_ =
    Validation.validate Validation.Script tree
    |> Result.map_error (fun _ -> "Invalid value contract expression")
  in
  let* typ, _, _ =
    WrittenChecking.checkSourceUnitsWithBase context.environment true false
      (sources @ [ unit_ ])
  in
  Ok typ

let check ~context ~sources contracts =
  if contracts = [] then Ok ()
  else
    let* items =
      ResultList.traverse WrittenSource.items sources |> Result.map List.concat
    in
    let* selectedTypes = WrittenDeclarations.predeclareTypes items in
    let types = M.fold M.add selectedTypes context.types in
    let traits =
      List.fold_left
        (fun found -> function
          | WrittenSource.Trait (path, declaration) ->
              S.add
                (String.concat "." (path @ [ declaration.W.name.W.name ]))
                found
          | WrittenSource.Function _ | WrittenSource.Value _
          | WrittenSource.Type _ | WrittenSource.Impl _
          | WrittenSource.Expression _ ->
              found)
        context.traits items
    in
    let check (path, original, replacement) =
      let mismatch name =
        Error
          ("Replacement contract mismatch for "
          ^ String.concat "." (path @ [ name ]))
      in
      match (original, replacement) with
      | W.DFunction original, W.DFunction replacement ->
          let* expected = functionContract types traits path original in
          let* actual = functionContract types traits path replacement in
          if expected = actual then Ok () else mismatch original.W.name.W.name
      | W.DType original, W.DType replacement ->
          let* expected = typeContract types traits path original in
          let* actual = typeContract types traits path replacement in
          if expected = actual then Ok () else mismatch original.W.name.W.name
      | W.DValue original, W.DValue replacement ->
          let* expected =
            initializerType context sources path original.W.body
            |> Result.map_error (fun message ->
                "Cannot establish upstream val contract for "
                ^ original.W.name.W.name ^ ": " ^ message)
          in
          let* actual =
            initializerType context sources path replacement.W.body
          in
          if expected = actual then Ok () else mismatch original.W.name.W.name
      | ( ( W.DFunction _ | W.DType _ | W.DValue _ | W.DTrait _ | W.DImpl _
          | W.DModule _ | W.DExpr _ | W.DAssertion _ | W.DTest _ ),
          ( W.DFunction _ | W.DType _ | W.DValue _ | W.DTrait _ | W.DImpl _
          | W.DModule _ | W.DExpr _ | W.DAssertion _ | W.DTest _ ) ) ->
          Error
            "Replacements currently require a function, type or val of the \
             same kind"
    in
    ResultList.traverse check contracts |> Result.map (fun _ -> ())
