(* Declared full and partial applications from WrittenCallSupport.ml. *)
open WrittenTypeSupport
module WT = WrittenTypes
module C = CheckedAST
module M = StringOrder.Map

type literalChecker =
  AST.semanticType option ->
  WrittenCheckingState.t ->
  AST.semanticType ->
  C.expr ->
  (checkedExpression, string) result

let bind = Result.bind
let map = Result.map

let rec take count list =
  if count = 0 then []
  else
    match list with
    | head :: tail -> head :: take (count - 1) tail
    | [] -> Crash.crash "Function prefix exceeds its argument count"

let rec drop count list =
  if count = 0 then list
  else
    match list with
    | _ :: tail -> drop (count - 1) tail
    | [] -> Crash.crash "Function prefix exceeds its argument count"

let[@warning "-4"] checkNamed checkExpression checkedLiteral globals locals
    symbols expected range (name : WT.qualifiedFnIdentifier) typeArgs args =
  let check symbols expected expr =
    checkExpression globals locals symbols expected expr
  in
  match resolveFunction globals (qualifiedFnName name) with
  | None ->
      Error
        ("Unknown function '" ^ String.concat "." (qualifiedFnName name) ^ "'")
  | Some signature
    when args <> [] && List.length args < List.length signature.parameters ->
      bind
        (ResultList.traverse
           (resolveWrittenType globals.allowInternal globals.types
              globals.modulePath globals.typeParams)
           typeArgs)
        (fun givenTypeArgs ->
          if List.length givenTypeArgs <> List.length signature.typeParams then
            Error
              ("Partial application of generic function '" ^ name.WT.fn.WT.name
             ^ "' requires explicit type arguments")
          else
            let substitution =
              M.of_list (List.combine signature.typeParams givenTypeArgs)
            in
            let concreteParameters =
              List.map (Types.applySubst substitution) signature.parameters
            and concreteReturn =
              Types.applySubst substitution signature.return
            in
            let arguments =
              List.fold_left
                (fun result (arg, typ) ->
                  bind result (fun (reversed, symbols) ->
                      map
                        (fun (_, arg, symbols) -> (arg :: reversed, symbols))
                        (check symbols (Some typ) arg)))
                (Ok ([], symbols))
                (List.combine args (take (List.length args) concreteParameters))
            in
            bind arguments (fun (reversed, symbols) ->
                let captured, symbols =
                  List.fold_left
                    (fun (reversed, symbols) (index, argument) ->
                      let id, symbols =
                        WrittenCheckingState.allocateBinding
                          ("__partial_capture_" ^ string_of_int index)
                          symbols
                      in
                      ((id, argument) :: reversed, symbols))
                    ([], symbols)
                    (List.mapi
                       (fun index value -> (index, value))
                       (List.rev reversed))
                in
                let captured = List.rev captured
                and remainingTypes =
                  drop (List.length args) concreteParameters
                in
                let parameters, symbols =
                  List.fold_left
                    (fun (reversed, symbols) (index, typ) ->
                      let id, symbols =
                        WrittenCheckingState.allocateBinding
                          ("__partial_arg_" ^ string_of_int index)
                          symbols
                      in
                      let param : C.lambdaParameter =
                        { C.pattern = C.LPVariable id; typ = C.checkedType typ }
                      in
                      ((id, param) :: reversed, symbols))
                    ([], symbols)
                    (List.mapi
                       (fun index value -> (index, value))
                       remainingTypes)
                in
                let parameters = List.rev parameters in
                match NonEmptyList.tryFromList (List.map snd parameters) with
                | None ->
                    Error "Partial application requires remaining parameters"
                | Some lambdaParameters ->
                    let arguments =
                      NonEmptyList.fromList
                        (List.map (fun (id, _) -> C.Local id) captured
                        @ List.map (fun (id, _) -> C.Local id) parameters)
                    in
                    let body =
                      if signature.typeParams = [] then
                        C.Call (signature.id, arguments)
                      else
                        C.TypeApp
                          ( signature.id,
                            List.map C.checkedType givenTypeArgs,
                            arguments )
                    in
                    let lambda =
                      C.Lambda
                        ( lambdaParameters,
                          Some (C.checkedType concreteReturn),
                          body )
                    in
                    let partial =
                      List.fold_right
                        (fun (id, argument) expr ->
                          C.Let (C.LPVariable id, argument, expr))
                        captured lambda
                    in
                    checkedLiteral expected symbols
                      (AST.TFunction (remainingTypes, concreteReturn))
                      partial))
  | Some signature ->
      let signature =
        if signature.typeParams = [] then signature
        else
          let scope = String.concat "." (qualifiedFnName name) in
          let freshParams, renaming =
            CheckingDiagnostics.freshenTypeParams (Some scope)
              signature.typeParams
          in
          {
            signature with
            typeParams = freshParams;
            parameters =
              List.map
                (CheckingDiagnostics.applyTypeVarRenaming renaming)
                signature.parameters;
            return =
              CheckingDiagnostics.applyTypeVarRenaming renaming signature.return;
          }
      in
      let normalizedArgs =
        match (signature.parameters, args) with
        | [], [ WT.EUnit _ ] -> []
        | [ AST.TUnit ], [] -> [ WT.EUnit range ]
        | _ -> args
      in
      let isStdlibEquality =
        match qualifiedFnName name with
        | [ "Stdlib"; "equals" ] | [ "Stdlib"; "notEquals" ] -> true
        | _ -> false
      in
      let isDictKeyOperation =
        match (qualifiedFnName name, signature.parameters) with
        | "Stdlib" :: "Dict" :: _, AST.TDict (keyType, _) :: parameterType :: _
          ->
            parameterType = keyType
        | _ -> false
      in
      if
        typeArgs <> []
        && List.length typeArgs <> List.length signature.typeParams
      then
        Error
          ("Function '" ^ name.WT.fn.WT.name ^ "' expects "
          ^ string_of_int (List.length signature.typeParams)
          ^ " type arguments")
      else if List.length normalizedArgs <> List.length signature.parameters
      then
        Error
          ("Function '" ^ name.WT.fn.WT.name ^ "' expects "
          ^ string_of_int (List.length signature.parameters)
          ^ " arguments")
      else
        let explicitArgs =
          ResultList.traverse
            (resolveWrittenType globals.allowInternal globals.types
               globals.modulePath globals.typeParams)
            typeArgs
        in
        let lookaheadTypeArgs =
          if signature.typeParams = [] then None
          else
            let pairs =
              List.filter_map
                (fun (arg, typ) ->
                  match arg with
                  | WT.ELambda _ -> None
                  | _ -> (
                      match check symbols None arg with
                      | Ok (actual, _, _) -> Some (typ, actual)
                      | Error _ -> None))
                (List.combine normalizedArgs signature.parameters)
            in
            match pairs with
            | [] -> None
            | pairs ->
                Result.to_option
                  (Unification.inferTypeArgs signature.typeParams
                     (List.map fst pairs) (List.map snd pairs)
                     (Some signature.return) expected)
        in
        let rec checkArgs givenTypeArgs symbols reversed remaining =
          match remaining with
          | [] -> Ok (List.rev reversed, symbols)
          | (arg, parameterType) :: tail ->
              let structuralDictKey =
                isDictKeyOperation
                && List.length reversed = 1
                &&
                match reversed with
                | (AST.TDict (AST.TRecord _, _), _) :: _ -> true
                | _ -> false
              in
              let argumentExpected =
                if isStdlibEquality || structuralDictKey then None
                else if signature.typeParams = [] then Some parameterType
                else if givenTypeArgs <> [] then
                  Some
                    (Types.applySubst
                       (M.of_list
                          (List.combine signature.typeParams givenTypeArgs))
                       parameterType)
                else
                  let prefix = List.map fst (List.rev reversed) in
                  let inferredPrefix =
                    Unification.inferTypeArgs signature.typeParams
                      (take (List.length prefix) signature.parameters)
                      prefix (Some signature.return) expected
                  in
                  let inferred =
                    match (inferredPrefix, lookaheadTypeArgs) with
                    | Ok prefix, Some later ->
                        Some
                          (List.map2
                             (fun first second ->
                               if Unification.containsTVar first then second
                               else first)
                             prefix later)
                    | Ok prefix, None -> Some prefix
                    | Error _, Some later -> Some later
                    | Error _, None -> None
                  in
                  Option.map
                    (fun args ->
                      Types.applySubst
                        (M.of_list (List.combine signature.typeParams args))
                        parameterType)
                    inferred
              in
              bind (check symbols argumentExpected arg)
                (fun (typ, arg, symbols) ->
                  checkArgs givenTypeArgs symbols ((typ, arg) :: reversed) tail)
        in
        bind explicitArgs (fun givenTypeArgs ->
            if
              givenTypeArgs <> []
              && List.length givenTypeArgs <> List.length signature.typeParams
            then
              Error
                ("Function '" ^ name.WT.fn.WT.name ^ "' expects "
                ^ string_of_int (List.length signature.typeParams)
                ^ " type arguments")
            else
              bind
                (checkArgs givenTypeArgs symbols []
                   (List.combine normalizedArgs signature.parameters))
                (fun (checkedArgs, symbols) ->
                  let rec abortingArgument preceding = function
                    | [] -> None
                    | (AST.TNever, abort) :: _ ->
                        let prefix =
                          List.filter
                            (function C.ListLiteral [] -> false | _ -> true)
                            (List.rev preceding)
                        in
                        Some
                          (List.fold_right
                             (fun prior next -> C.Sequence (prior, next))
                             prefix abort)
                    | (_, arg) :: tail ->
                        abortingArgument (arg :: preceding) tail
                  in
                  match abortingArgument [] checkedArgs with
                  | Some abort ->
                      checkedLiteral expected symbols AST.TNever abort
                  | None ->
                      let mixedCharString =
                        match List.map fst checkedArgs with
                        | [ AST.TChar; AST.TString ]
                        | [ AST.TString; AST.TChar ] ->
                            true
                        | _ -> false
                      in
                      let inferenceTypes =
                        match List.map fst checkedArgs with
                        | first :: second :: rest
                          when isStdlibEquality
                               && structuralEqualityCompatible globals first
                                    second ->
                            first :: first :: rest
                        | (AST.TDict (keyType, _) as dictType) :: second :: rest
                          when isDictKeyOperation
                               && structuralEqualityCompatible globals keyType
                                    second ->
                            dictType :: keyType :: rest
                        | types -> types
                      in
                      let inferred =
                        if isStdlibEquality && mixedCharString then
                          Error "Cannot compare Char and String"
                        else if signature.typeParams = [] then Ok []
                        else if givenTypeArgs <> [] then Ok givenTypeArgs
                        else
                          Unification.inferTypeArgs signature.typeParams
                            signature.parameters inferenceTypes
                            (Some signature.return) expected
                      in
                      let inferred =
                        bind inferred (fun types ->
                            match globals.currentFunction with
                            | Some (currentId, currentName, currentParams)
                              when currentId = signature.id
                                   && types
                                      <> List.map
                                           (fun name -> AST.TVar name)
                                           currentParams ->
                                Error
                                  ("Polymorphic recursion is not supported \
                                    inside recursive group member: "
                                 ^ currentName)
                            | _ -> Ok types)
                      in
                      bind inferred (fun inferredTypeArgs ->
                          let substitution =
                            M.of_list
                              (List.combine signature.typeParams
                                 inferredTypeArgs)
                          in
                          let parameterTypes =
                            List.map
                              (Types.applySubst substitution)
                              signature.parameters
                          and returnType =
                            Types.applySubst substitution signature.return
                          in
                          let pairs =
                            List.mapi
                              (fun index pair -> (index, pair))
                              (List.combine checkedArgs parameterTypes)
                          in
                          let mismatch =
                            List.find_opt
                              (fun (index, ((typ, _), parameterType)) ->
                                (not
                                   (((isStdlibEquality && index = 1)
                                    || (isDictKeyOperation && index = 1))
                                   && structuralEqualityCompatible globals
                                        parameterType typ))
                                && Option.is_none
                                     (Unification.reconcileTypes None
                                        parameterType typ))
                              pairs
                          in
                          match mismatch with
                          | Some (_, ((actual, _), wanted)) ->
                              Error
                                ("Function argument type mismatch: expected "
                                ^ StructuralFormat.semanticType wanted
                                ^ ", got "
                                ^ StructuralFormat.semanticType actual)
                          | None ->
                              let converted =
                                List.fold_left
                                  (fun state
                                       (index, ((actualType, value), targetType))
                                     ->
                                    bind state (fun (reversed, symbols) ->
                                        if
                                          index = 1
                                          && (isStdlibEquality
                                            || isDictKeyOperation)
                                          && Option.is_none
                                               (Unification.reconcileTypes None
                                                  targetType actualType)
                                          && structuralEqualityCompatible
                                               globals targetType actualType
                                        then
                                          map
                                            (fun (value, symbols) ->
                                              (value :: reversed, symbols))
                                            (convertStructuralRecord globals
                                               targetType actualType value
                                               symbols)
                                        else Ok (value :: reversed, symbols)))
                                  (Ok ([], symbols))
                                  pairs
                              in
                              bind converted (fun (reversed, symbols) ->
                                  let args =
                                    match List.rev reversed with
                                    | [] -> NonEmptyList.singleton C.UnitLiteral
                                    | args -> NonEmptyList.fromList args
                                  in
                                  let call =
                                    if signature.typeParams = [] then
                                      C.Call (signature.id, args)
                                    else
                                      C.TypeApp
                                        ( signature.id,
                                          List.map C.checkedType
                                            inferredTypeArgs,
                                          args )
                                  in
                                  checkedLiteral expected symbols returnType
                                    call))))
