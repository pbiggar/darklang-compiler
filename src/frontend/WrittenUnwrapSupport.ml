(* WrittenUnwrapSupport.ml - Check postfix propagation and retain its lexical return boundary. *)
[@@@warning "-4"]
open WrittenTypeSupport
module C = CheckedAST
module State = WrittenCheckingState
module S = StringOrder.Set
let ( let* ) = Result.bind

let optionName = "Darklang.Stdlib.Option.Option"
let resultName = "Darklang.Stdlib.Result.Result"

let check checkExpression globals locals state expected operand =
  match globals.returnBoundary with
  | None -> Error "Postfix ? requires an enclosing function or lambda"
  | Some (target, returned) ->
      let inner, state =
        State.freshenType S.empty
          (AST.TInferenceVar ("?payload", "?payload")) state
      in
      let returnedType = State.resolve state returned in
      let operandExpected =
        match returnedType with
        | AST.TSum (name, [_]) when name = optionName ->
            Some (AST.TSum (name, [inner]))
        | AST.TSum (name, [_; error]) when name = resultName ->
            Some (AST.TSum (name, [inner; error]))
        | _ -> None
      in
      let* operandType, operand, state =
        checkExpression globals locals state operandExpected operand
      in
      let* owner, payload, error =
        match State.resolve state operandType with
        | AST.TSum (name, [value]) when name = optionName -> Ok (name, value, None)
        | AST.TSum (name, [value; error]) when name = resultName -> Ok (name, value, Some error)
        | _ -> Error "Postfix ? requires an Option or Result operand"
      in
      let successType, state = State.freshenType S.empty
        (AST.TInferenceVar ("?success", "?success")) state in
      let returnArgs = match error with None -> [successType] | Some e -> [successType; e] in
      let* state =
        match State.constrain returned (AST.TSum (owner, returnArgs)) state with
        | Ok state -> Ok state
        | Error _ -> Error "Postfix ? requires a matching Option or Result return type and Result error type"
      in
      let returnedArgs =
        match State.resolve state returned with
        | AST.TSum (_, args) -> args
        | _ -> Crash.crash "Checked propagation return type is not a container"
      in
      let successName, failureName = if owner = optionName then "Some", "None" else "Ok", "Error" in
      let constructor name ordinal state =
        State.internConstructor owner name (caseTag globals.collidingCases owner name ordinal) state
      in
      let successId, state = constructor successName 0 state in
      let failureId, state = constructor failureName 1 state in
      let typeId, state = State.internType owner state in
      let valueId, state = State.allocateBinding "?value" state in
      let failureFields, failureValues, state =
        match error with
        | None -> [], [], state
        | Some _ ->
            let id, state = State.allocateBinding "?error" state in
            [C.PVariable id], [C.Local id], state
      in
      let failure = C.Constructor
        ({C.typeId; constructorId = failureId; typeArgs = List.map C.checkedType returnedArgs}, failureValues) in
      let arm pattern body = { C.patterns = NonEmptyList.singleton pattern; guard = None; body } in
      let expression = C.Match (operand,
        NonEmptyList.appendList (NonEmptyList.singleton
          (arm (C.PConstructor (successId, [C.PVariable valueId])) (C.Local valueId)))
          [arm (C.PConstructor (failureId, failureFields)) (C.EarlyReturn (target, failure))]) in
      checkedLiteral expected (State.markEarlyReturn target state) payload expression

(* Recover the outer result shape before retrying a lambda whose ? operand is
   still unknown. Full checking remains authoritative for every payload type. *)
let rec returnHint globals expression =
  let standard name =
    let count = if name = optionName then 1 else 2 in
    Some (AST.TSum (name, List.init count (fun i ->
      AST.TInferenceVar ("?hint" ^ string_of_int i, "?hint" ^ string_of_int i))))
  in
  match expression with
  | WrittenTypes.EEnum (_, name, (_, caseName), _, _) ->
      let owner =
        if name.WrittenTypes.typ.name <> "" then
          Result.to_option (findNamedType globals name) |> Option.map fst
        else
          match List.filter_map (fun (owner, entry) ->
            match entry.definition with
            | WrittenTypes.TDEnum cases when List.exists (fun (_, (item : WrittenTypes.enumCaseSyntax)) -> snd item.WrittenTypes.name = caseName) cases -> Some owner
            | _ -> None) (StringOrder.Map.bindings globals.types) with
          | [owner] -> Some owner
          | _ -> None
      in
      (match owner with Some name when name = optionName || name = resultName -> standard name | _ -> None)
  | WrittenTypes.ELet (_, _, _, body, _, _)
  | WrittenTypes.EStatement (_, _, body) -> returnHint globals body
  | WrittenTypes.EIf (_, _, yes, no, _, _, _) ->
      (match returnHint globals yes with Some _ as found -> found | None -> Option.bind no (returnHint globals))
  | WrittenTypes.EMatch (_, _, cases, _, _) ->
      List.find_map (fun case -> returnHint globals case.WrittenTypes.rhs) cases
  | WrittenTypes.EApply (_, WrittenTypes.EFnName (_, name), _, _) ->
      (match resolveFunction globals (qualifiedFnName name) with
       | Some signature -> (match signature.return with
           | AST.TSum (name, _) when name = optionName || name = resultName -> standard name
           | _ -> None)
       | None -> None)
  | _ -> None
