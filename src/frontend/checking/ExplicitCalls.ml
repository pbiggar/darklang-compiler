(* ExplicitCalls.ml - Complete explicit type application paths from ExplicitCalls.ml. *)
open! AST
open CheckingDiagnostics
module U = Unification
module T = Types
module C = ComparisonPlanning

let check checkExpr paramNames sumNames sums env registry lookup generic
    warnings modules aliases expected name typeArgs args =
  let ( let* ) = Result.bind in
  let typeError result =
    Result.map_error (fun message -> GenericError message) result
  in
  let args = NonEmptyList.toList args in
  let canonical = T.canonicalizeBareSumTypeRefsWithNames sumNames in
  let checkResolved declared resolved params ret typeParams =
    let typeArgs =
      if declared then
        let publicDict =
          String.starts_with ~prefix:"Darklang.Stdlib.Dict." resolved
          && not (String.starts_with ~prefix:"Darklang.Stdlib.Dict.__" resolved)
          || String.starts_with ~prefix:"Dict." resolved
             && not (String.starts_with ~prefix:"Dict.__" resolved)
        in
        if publicDict && List.length typeArgs + 1 = List.length typeParams then
          TString :: typeArgs
        else typeArgs
      else typeArgs
    in
    if List.length typeParams <> List.length typeArgs then
      Error
        (GenericError
           (T.formatTypeArgumentArityError name (List.length typeParams)
              (List.length typeArgs)))
    else
      let typeArgs = List.map canonical typeArgs in
      let* () =
        C.validateCanonicalSortableCall aliases registry sums resolved typeArgs
      in
      let* () =
        if declared then
          match (resolved, typeArgs) with
          | ( ("Darklang.Stdlib.Json.serialize" | "Darklang.Stdlib.Json.parse"),
              [ target ] ) ->
              C.validateJsonTargetType aliases registry lookup sums target
          | _ -> Ok ()
        else Ok ()
      in
      let* subst = typeError (T.buildSubstitution typeParams typeArgs) in
      let params =
        List.map (fun typ -> canonical (T.applySubst subst typ)) params
      and ret = canonical (T.applySubst subst ret) in
      let count = List.length params in
      let args = normalizeNullaryCallArgs count args in
      let supplied = List.length args in
      if supplied > count then
        Error
          (GenericError (T.formatValueArgumentArityError name count supplied))
      else
        let compatible =
          if declared then U.typesCompatibleWithAliases aliases
          else U.typesCompatible
        in
        let rec checkArgs remaining types index acc =
          match (remaining, types) with
          | [], [] -> Ok (List.rev acc)
          | arg :: rest, param :: params ->
              let paramName =
                ExpressionSupport.paramNameForLegacyError paramNames resolved
                  index
              in
              let legacy actual =
                GenericError
                  (formatLegacyParamTypeError name index paramName param actual
                     arg)
              in
              let checked =
                checkExpr arg env registry lookup generic warnings modules
                  aliases (Some param)
                |> Result.map_error (function
                  | TypeMismatch (_, actual, _) when not (isNeverType actual) ->
                      legacy actual
                  | other -> other)
              in
              let* actual, arg = checked in
              if compatible param actual then
                checkArgs rest params (index + 1) (arg :: acc)
              else Error (legacy actual)
          | _ ->
              Error
                (GenericError "Internal error: argument/param length mismatch")
        in
        if supplied < count then
          let* args = checkArgs args (List.take supplied params) 1 [] in
          let remainingTypes = List.drop supplied params in
          let remaining = makePartialParams resolved remainingTypes in
          let allArgs = args @ List.map (fun (name, _) -> Var name) remaining in
          let body =
            AST.applyNamedWithTypes resolved typeArgs (toCallArgs allArgs)
          in
          let expr = Lambda (toLambdaParams remaining, None, body) in
          let typ = TFunction (remainingTypes, ret) in
          match expected with
          | Some expected when not (U.typesCompatible expected typ) ->
              Error
                (TypeMismatch (expected, typ, "partial application of " ^ name))
          | Some _ | None -> Ok (typ, expr)
        else
          let* args = checkArgs args params 1 [] in
          match expected with
          | Some expected when not (compatible expected ret) ->
              Error (TypeMismatch (expected, ret, "result of call to " ^ name))
          | Some _ | None ->
              Ok
                ( ret,
                  AST.applyNamedWithTypes resolved typeArgs (toCallArgs args) )
  in
  match U.tryLookupResolved name env with
  | Some (TFunction (params, ret), resolved) -> (
      match U.tryLookupResolved resolved generic.T.functions with
      | Some (typeParams, _) ->
          checkResolved true resolved params ret typeParams
      | None ->
          Error
            (GenericError
               ("Function " ^ name ^ " is not generic, use regular call syntax"))
      )
  | Some (other, _) ->
      Error
        (GenericError
           (name ^ " is not a function (has type " ^ typeToString other ^ ")"))
  | None -> (
      match DarkStdlib.tryGetFunction modules name with
      | Some (func, resolved) when func.AST.typeParams <> [] ->
          checkResolved false resolved func.AST.paramTypes func.AST.returnType
            func.AST.typeParams
      | Some _ ->
          Error
            (GenericError
               ("Function " ^ name ^ " is not generic, use regular call syntax"))
      | None -> Error (UndefinedCallTarget name))
[@@warning "-4"]
