(* Contextual lambda inference and currying from WrittenLambdaSupport.ml. *)
open WrittenTypeSupport
module WT = WrittenTypes
module C = CheckedAST
module M = StringOrder.Map

type expressionChecker =
  globals ->
  locals ->
  WrittenCheckingState.t ->
  AST.semanticType option ->
  WT.expr ->
  (checkedExpression, string) result

let[@warning "-4"] check checkExpression (globals : globals) locals symbols
    expected (range : WT.range) patterns body keywordFun symbolArrow =
  let check symbols expected expr =
    checkExpression globals locals symbols expected expr
  in
  if
    List.exists (function WT.LPVariable (_, "") -> true | _ -> false) patterns
  then
    let patterns =
      List.filter
        (function WT.LPVariable (_, "") -> false | _ -> true)
        patterns
    in
    match patterns with
    | [] -> check symbols expected body
    | _ ->
        check symbols expected
          (WT.ELambda (range, patterns, body, keywordFun, symbolArrow))
  else
    match expected with
    | Some (AST.TFunction (argumentTypes, _))
      when argumentTypes <> []
           && List.length argumentTypes < List.length patterns ->
        let outer =
          List.filteri
            (fun index _ -> index < List.length argumentTypes)
            patterns
        and inner =
          List.filteri
            (fun index _ -> index >= List.length argumentTypes)
            patterns
        in
        check symbols expected
          (WT.ELambda
             ( range,
               outer,
               WT.ELambda (range, inner, body, keywordFun, symbolArrow),
               keywordFun,
               symbolArrow ))
    | _ -> (
        let lambdaExpected =
          match expected with
          | Some (AST.TVar _ | AST.TInferenceVar _) -> None
          | other -> other
        in
        let lambdaExpected =
          match lambdaExpected with
          | Some _ -> lambdaExpected
          | None ->
              let args =
                List.mapi
                  (fun index _ ->
                    let name = "t$anonymous_argument_" ^ string_of_int index in
                    AST.TInferenceVar (name, name))
                  patterns
              in
              Some
                (AST.TFunction
                   ( args,
                     AST.TInferenceVar
                       ("t$anonymous_return", "t$anonymous_return") ))
        in
        let lambdaExpected, symbols =
          match lambdaExpected with
          | Some typ ->
              let typ, symbols =
                WrittenCheckingState.freshenType globals.typeParams typ symbols
              in
              (Some typ, symbols)
          | None -> (None, symbols)
        in
        match lambdaExpected with
        | Some (AST.TFunction (argumentTypes, returnType))
          when List.length patterns = List.length argumentTypes ->
            let result =
              List.fold_left
                (fun result (pattern, typ) ->
                  Result.bind result (fun (reversed, capturedLocals, symbols) ->
                      Result.bind
                        (WrittenPatternSupport.checkLetPattern pattern typ
                           symbols) (fun (pattern, bindings, symbols) ->
                          Result.map
                            (fun merged ->
                              let param : C.lambdaParameter =
                                { C.pattern; typ = C.checkedType typ }
                              in
                              (param :: reversed, merged, symbols))
                            (WrittenPatternSupport.mergePatternBindings
                               capturedLocals bindings))))
                (Ok ([], M.empty, symbols))
                (List.combine patterns argumentTypes)
            in
            Result.bind result (fun (reversed, parameters, symbols) ->
                match NonEmptyList.tryFromList (List.rev reversed) with
                | None -> Error "Lambda requires at least one parameter"
                | Some parametersChecked ->
                    let bodyLocals = M.fold M.add parameters locals in
                    let bodyExpected =
                      match (returnType, body) with
                      | AST.TInferenceVar _, _ -> Some returnType
                      | _, WT.EIf (_, _, _, Some _, _, _, _)
                        when Unification.containsTVar returnType ->
                          None
                      | _ -> Some returnType
                    in
                    Result.map
                      (fun (bodyType, body, symbols) ->
                        let argumentTypes =
                          List.map
                            (WrittenCheckingState.resolve symbols)
                            argumentTypes
                        in
                        let parametersChecked =
                          NonEmptyList.map
                            (fun (parameter : C.lambdaParameter) ->
                              {
                                parameter with
                                C.typ =
                                  C.checkedType
                                    (WrittenCheckingState.resolve symbols
                                       (C.semanticType parameter.C.typ));
                              })
                            parametersChecked
                        in
                        let body =
                          WrittenCheckingState.resolveExpression symbols body
                        in
                        let bodyType =
                          WrittenCheckingState.resolve symbols bodyType
                        in
                        let returnType =
                          WrittenCheckingState.resolve symbols returnType
                        in
                        let inferredReturn =
                          if Unification.containsTVar returnType then bodyType
                          else returnType
                        in
                        ( AST.TFunction (argumentTypes, inferredReturn),
                          C.Lambda
                            ( parametersChecked,
                              Some (C.checkedType inferredReturn),
                              body ),
                          symbols ))
                      (checkExpression globals bodyLocals symbols bodyExpected
                         body))
        | Some (AST.TFunction _) -> Error "Lambda parameter count mismatch"
        | _ -> Error "Lambda requires an expected function type")
