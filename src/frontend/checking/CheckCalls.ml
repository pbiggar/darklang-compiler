(*
   CheckCalls.ml - Check Call expressions while preserving source diagnostics and order.
*)
(* CheckCalls.ml - Check Call expressions while preserving source diagnostics and order. *)
open! AST
open CheckingDiagnostics
module M = StringOrder.Map
module U = Unification
module T = Types

(*
   The resolution boundary has already attached the canonical callable
   identity. Type checking only validates that identity's signature.
   `Option.None |> Builtin.unwrap` and `Result.Error(_) |> Builtin.unwrap`
   are guaranteed runtime failures. When unconstrained, their payload type
   remains a type variable; normalize to Unit to keep IR monomorphic.
   Bottom-like behavior: if context expects a type, use it.
   Unconstrained top-level failures still need a concrete type.
   Check if this is a generic function.
   Freshen type params to avoid name clashes with caller's scope
   Generic function called without explicit type args: infer them
   Partial application of generic function
   Type-check the provided arguments
   Infer type arguments from provided args (some may remain as TVar)
   Build substitution and compute concrete types for remaining params
   Create unique parameter names for the remaining parameters
   Create the lambda body: TypeApp with all args
   Create the lambda: fun p0 p1 ... -> funcName<types>(providedArgs, p0, p1, ...)
   The resulting type is a function from remaining params to return type
   Full application - type-check arguments left-to-right while propagating bindings.
   Infer type arguments from parameter types, argument types, and expected return type
   Apply substitution to nested expressions (e.g., inner TypeApp nodes)
   This ensures that when empty() returns Dict<k$3, v$4> and we later
   infer k$3 -> Int64, v$4 -> Int64, the inner TypeApp gets updated
   Transform Call to TypeApp with inferred type arguments (using resolved name)
   Non-generic function: regular call or partial application
   Partial application: type-check provided args, then create lambda for remaining
   Create the lambda body: call the original function with all args (using resolved name)
   Create the lambda: fun p0 p1 ... -> funcName(providedArgs, p0, p1, ...)
   Check each argument type and collect transformed args
   In public source, higher-order generic parameters may reach call sites
   before their function shape is concretized (for example in nested List.map).
   Keep the call typable and let surrounding generic reconciliation specialize it.
   Check if it's a module function (e.g., Stdlib.Int64.add, __raw_get)
   Check argument count - allow partial application
   Partial application of non-generic module function
   Partial application of generic module function
   Type-check provided arguments and collect bindings for type inference
   Match param type against arg type to get type variable bindings
   Consolidate bindings and build substitution
   Build type arguments list from inferred bindings
   For partial application, some type params may not be inferrable yet
   Keep as type variable if not inferred
   Build full substitution (inferred types only, not type vars)
   Apply substitution to remaining param types and return type
   Create the lambda body: TypeApp call with all args (using resolved name)
   Create the lambda
   Generic module function: infer type arguments from actual argument types
   Type-check arguments left-to-right while propagating inferred bindings.
   This lets later args see concrete expectations inferred from earlier args.
   Build substitution and compute concrete types
   Non-generic module function: regular call
*)
let check checkExpr paramNames sums env registry lookup generic warnings modules
    aliases expected name args =
  let ( let* ) = Result.bind in
  let error result =
    Result.map_error (fun message -> GenericError message) result
  in
  let recurse value expected =
    checkExpr value env registry lookup generic warnings modules aliases
      expected
  in
  let args = NonEmptyList.toList args in
  let arity expected args =
    Error
      (GenericError
         ("Function " ^ name ^ " expects " ^ string_of_int expected
        ^ " arguments, got "
         ^ string_of_int (List.length args)))
  in
  let returnContext compatible typ expr context =
    match expected with
    | Some expected when not (compatible expected typ) ->
        Error (TypeMismatch (expected, typ, context ^ name))
    | Some _ | None -> Ok (typ, expr)
  in
  let rec provided infer compatible remaining types accTypes accExprs =
    match (remaining, types) with
    | [], [] -> Ok (List.rev accTypes, List.rev accExprs)
    | arg :: args, param :: params ->
        let* typ, arg = recurse arg (Some param) in
        if infer || compatible param typ then
          provided infer compatible args params (typ :: accTypes)
            (arg :: accExprs)
        else Error (TypeMismatch (param, typ, "argument to " ^ name))
    | _ -> Error (GenericError "Internal error: argument/param length mismatch")
  in
  let rec withBindings remaining types accTypes accExprs bindings =
    match (remaining, types) with
    | [], [] -> Ok (List.rev accTypes, List.rev accExprs)
    | arg :: args, param :: params -> (
        let* subst = error (U.consolidateBindings bindings) in
        let concrete = T.applySubst subst param in
        let* typ, arg = recurse arg (Some concrete) in
        match U.matchTypes concrete typ with
        | Error message ->
            Error
              (TypeMismatch
                 (concrete, typ, "argument to " ^ name ^ ": " ^ message))
        | Ok newBindings ->
            let combined = bindings @ newBindings in
            let* subst = error (U.consolidateBindings combined) in
            withBindings args params
              (T.applySubst subst typ :: accTypes)
              (arg :: accExprs) combined)
    | _ -> Error (GenericError "Argument count mismatch")
  in
  let partial resolved types ret args typeArgs compatible =
    let remaining = makePartialParams resolved types in
    let allArgs = args @ List.map (fun (name, _) -> Var name) remaining in
    let body =
      match typeArgs with
      | None -> AST.applyNamed resolved (toCallArgs allArgs)
      | Some args -> AST.applyNamedWithTypes resolved args (toCallArgs allArgs)
    in
    returnContext compatible
      (TFunction (types, ret))
      (Lambda (toLambdaParams remaining, None, body))
      "partial application of "
  in
  if isBuiltinUnwrapName name then
    match args with
    | [ arg ] -> (
        let* typ, arg = recurse arg None in
        let* output =
          match T.resolveType aliases typ with
          | TSum ("Darklang.Stdlib.Option.Option", [ value ]) -> Ok value
          | TSum ("Darklang.Stdlib.Result.Result", [ ok; _ ]) -> Ok ok
          | actual ->
              Error
                (GenericError
                   ("Can only unwrap Options and Results, yet got "
                  ^ typeToString actual))
        in
        let output =
          if isKnownFailureConstructorExpr arg then
            match expected with
            | Some expected -> expected
            | None when U.containsTVar output -> TUnit
            | None -> output
          else output
        in
        match expected with
        | Some expected -> (
            match U.reconcileTypes (Some aliases) expected output with
            | Some typ ->
                Ok
                  ( typ,
                    AST.applyNamed "Builtin.unwrap" (NonEmptyList.singleton arg)
                  )
            | None ->
                Error
                  (TypeMismatch (expected, output, "result of call to " ^ name))
            )
        | None ->
            Ok
              ( output,
                AST.applyNamed "Builtin.unwrap" (NonEmptyList.singleton arg) ))
    | _ -> arity 1 args
  else if isRuntimeFailureName name then
    match args with
    | [ arg ] ->
        let* _, arg = recurse arg (Some TString) in
        let output =
          match expected with
          | Some (TVar _ | TInferenceVar _) -> TUnit
          | Some expected -> expected
          | None -> TNever
        in
        Ok (output, AST.applyNamed name (NonEmptyList.singleton arg))
    | _ -> arity 1 args
  else
    match U.tryLookupResolved name env with
    | Some (TFunction (originalParams, originalReturn), resolved) -> (
        match U.tryLookupResolved resolved generic.T.functions with
        | Some (originalTypeParams, _) ->
            let scope =
              if String.starts_with ~prefix:"Darklang.Stdlib." resolved then
                None
              else Some resolved
            in
            let typeParams, renaming =
              freshenTypeParams scope originalTypeParams
            in
            let params = List.map (applyTypeVarRenaming renaming) originalParams
            and ret = applyTypeVarRenaming renaming originalReturn in
            let count = List.length params in
            let args = normalizeNullaryCallArgs count args in
            let supplied = List.length args in
            if supplied > count then
              Error
                (GenericError
                   (T.formatValueArgumentArityError name count supplied))
            else if supplied < count then
              let suppliedTypes = List.take supplied params
              and remainingTypes = List.drop supplied params in
              let* argTypes, args =
                provided true U.typesCompatible args suppliedTypes [] []
              in
              let* inferred =
                error
                  (U.inferTypeArgs typeParams suppliedTypes argTypes (Some ret)
                     None)
              in
              let* subst = error (T.buildSubstitution typeParams inferred) in
              partial resolved
                (List.map (T.applySubst subst) remainingTypes)
                (T.applySubst subst ret)
                (List.map (T.applySubstToExpr subst) args)
                (Some inferred) U.typesCompatible
            else
              let* argTypes, args = withBindings args params [] [] [] in
              let* inferred =
                error
                  (U.inferTypeArgs typeParams params argTypes (Some ret)
                     expected)
              in
              let* () =
                ComparisonPlanning.validateCanonicalSortableCall aliases
                  registry sums resolved inferred
              in
              let* () =
                ComparisonPlanning.validateDictKeyCall aliases registry sums
                  resolved inferred
              in
              let* subst = error (T.buildSubstitution typeParams inferred) in
              returnContext U.typesCompatible (T.applySubst subst ret)
                (AST.applyNamedWithTypes resolved inferred
                   (toCallArgs (List.map (T.applySubstToExpr subst) args)))
                "result of call to "
        | None ->
            let count = List.length originalParams in
            let args = normalizeNullaryCallArgs count args in
            let supplied = List.length args in
            if supplied > count then
              Error
                (GenericError
                   (T.formatValueArgumentArityError name count supplied))
            else if supplied < count then
              let* _, args =
                provided false
                  (U.typesCompatibleWithAliases aliases)
                  args
                  (List.take supplied originalParams)
                  [] []
              in
              partial resolved
                (List.drop supplied originalParams)
                originalReturn args None
                (U.typesCompatibleWithAliases aliases)
            else
              let rec full remaining types index acc =
                match (remaining, types) with
                | [], [] -> Ok (List.rev acc)
                | arg :: args, typ :: types ->
                    let paramName =
                      ExpressionSupport.paramNameForLegacyError paramNames
                        resolved index
                    in
                    let legacy actual =
                      GenericError
                        (formatLegacyParamTypeError name index paramName typ
                           actual arg)
                    in
                    let checked =
                      recurse arg (Some typ)
                      |> Result.map_error (function
                        | TypeMismatch (_, actual, _)
                          when not (isNeverType actual) ->
                            legacy actual
                        | other -> other)
                    in
                    let* actual, arg = checked in
                    if U.typesCompatibleWithAliases aliases typ actual then
                      full args types (index + 1) (arg :: acc)
                    else Error (legacy actual)
                | _ ->
                    Error
                      (GenericError
                         "Internal error: argument/param length mismatch")
              in
              let* args = full args originalParams 1 [] in
              returnContext
                (U.typesCompatibleWithAliases aliases)
                originalReturn
                (AST.applyNamed resolved (toCallArgs args))
                "result of call to ")
    | Some (TVar key, resolved) | Some (TInferenceVar (_, key), resolved) ->
        let rec unknown remaining acc =
          match remaining with
          | [] -> Ok (List.rev acc)
          | arg :: rest ->
              let* _, arg = recurse arg None in
              unknown rest (arg :: acc)
        in
        let* args = unknown args [] in
        let ret =
          match expected with
          | Some expected -> expected
          | None -> TVar ("__call_result_" ^ key)
        in
        Ok (ret, AST.applyNamed resolved (toCallArgs args))
    | Some (other, _) ->
        Error
          (GenericError
             (name ^ " is not a function (has type " ^ typeToString other ^ ")"))
    | None -> (
        match DarkStdlib.tryGetFunction modules name with
        | None -> Error (UndefinedCallTarget name)
        | Some (func, resolved) ->
            let typeParams, renaming =
              freshenTypeParams None func.AST.typeParams
            in
            let params =
              List.map (applyTypeVarRenaming renaming) func.AST.paramTypes
            and ret = applyTypeVarRenaming renaming func.AST.returnType in
            let count = List.length func.AST.paramTypes in
            let args = normalizeNullaryCallArgs count args in
            let supplied = List.length args in
            if supplied > count then
              Error
                (GenericError
                   (T.formatValueArgumentArityError name count supplied))
            else if supplied < count && typeParams = [] then
              let compatible expected actual =
                T.typesEqual aliases actual expected
              in
              let* _, args =
                provided false compatible args (List.take supplied params) [] []
              in
              partial resolved
                (List.drop supplied params)
                ret args None (T.typesEqual aliases)
            else if supplied < count then
              let rec infer remaining types acc bindings =
                match (remaining, types) with
                | [], [] -> Ok (List.rev acc, bindings)
                | arg :: args, param :: params -> (
                    let* typ, arg = recurse arg (Some param) in
                    match U.matchTypes param typ with
                    | Ok newBindings ->
                        infer args params (arg :: acc) (bindings @ newBindings)
                    | Error message ->
                        Error
                          (TypeMismatch
                             (param, typ, "argument to " ^ name ^ ": " ^ message))
                    )
                | _ ->
                    Error
                      (GenericError
                         "Internal error: argument/param length mismatch")
              in
              let* args, bindings =
                infer args (List.take supplied params) [] []
              in
              let* subst = error (U.consolidateBindings bindings) in
              let inferred =
                List.map
                  (fun name ->
                    Option.value (M.find_opt name subst)
                      ~default:(inferenceVarForKey name))
                  typeParams
              in
              partial resolved
                (List.map (T.applySubst subst) (List.drop supplied params))
                (T.applySubst subst ret) args (Some inferred) U.typesCompatible
            else if typeParams <> [] then
              let* argTypes, args = withBindings args params [] [] [] in
              let* inferred =
                error
                  (U.inferTypeArgs typeParams params argTypes (Some ret)
                     expected)
              in
              let* subst = error (T.buildSubstitution typeParams inferred) in
              returnContext U.typesCompatible (T.applySubst subst ret)
                (AST.applyNamedWithTypes resolved inferred (toCallArgs args))
                "result of call to "
            else
              let* _, args =
                provided false
                  (U.typesCompatibleWithAliases aliases)
                  args params [] []
              in
              returnContext
                (U.typesCompatibleWithAliases aliases)
                ret
                (AST.applyNamed resolved (toCallArgs args))
                "result of call to ")
[@@warning "-4"]
