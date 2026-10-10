(*
   Unification.ml - Unify source types and infer concrete generic arguments.
*)
(* Unification.ml - Preserve source type matching and inference ordering. *)
open! AST
module M = StringOrder.Map

let equal first second = AST.compareSemanticType first second = 0

let unificationVar = function
  | TVar name | TInferenceVar (_, name) -> Some name
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr | TFunction _ | TTuple _
  | TRecord _ | TSum _ | TList _ | TStream _ | TDict _ ->
      None

let typeToString = CheckingDiagnostics.typeToString

(*
   Type Inference for Generic Function Calls
   When a generic function is called without explicit type arguments, we infer
   the type arguments from the actual argument types. For example:
   let identity<'T>(x: T) : T = x
   identity(42)  // Infers T=int from argument type
   Match a pattern type against an actual type, extracting type variable bindings.
   Returns a list of (typeVarName, concreteType) pairs.
   Example: matchTypes (TVar "T") TInt64 = Ok [("T", TInt64)]
   Helper for matching concrete types - also handles when actual is a TVar
   Runtime-error expressions are bottom-like and can inhabit any expected type.
   Bind a type variable to a concrete type
*)
let matchConcrete expected actual =
  if expected = TNever || actual = TNever || equal expected actual then Ok []
  else
    match unificationVar actual with
    | Some name -> Ok [ (name, expected) ]
    | None ->
        Error
          ("Expected " ^ typeToString expected ^ ", got " ^ typeToString actual)

let combineResults initial results =
  List.fold_left
    (fun acc result ->
      match (acc, result) with
      | Ok bindings, Ok following -> Ok (bindings @ following)
      | Error error, _ | _, Error error -> Error error)
    initial results

(*
   Type variable matches anything - record the binding
   Same var, no binding needed
   Bind type variable to List type
   Unify type arguments if both have them
   Bind type variable to Record type
   Bind type variable to Sum type
   Match each parameter type and return type
   Combine all results
   Bind type variable to Function type
   Bind type variable to Tuple type
   Match both key and value types
   Bind type variable to Dict type
*)
let rec matchTypes pattern actual =
  let matchMany patterns actuals =
    List.map2 matchTypes patterns actuals |> combineResults (Ok [])
  in
  let bindOrError description =
    match unificationVar actual with
    | Some name -> Ok [ (name, pattern) ]
    | None -> Error ("Expected " ^ description ^ ", got " ^ typeToString actual)
  in
  match pattern with
  | TVar name | TInferenceVar (_, name) ->
      if equal actual pattern then Ok [] else Ok [ (name, actual) ]
  | ( TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
    | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TChar | TBlob
    | TDateTime | TUnit | TNever | TInternalRawPtr ) as concrete ->
      matchConcrete concrete actual
  | TList inner -> (
      (match actual with
      | TList value -> matchTypes inner value
      | _ -> bindOrError "List<...>")
      [@warning "-4"])
  | TStream inner -> (
      (match actual with
      | TStream value -> matchTypes inner value
      | _ -> bindOrError "Stream<...>")
      [@warning "-4"])
  | TRecord (name, args) -> (
      (match actual with
      | (TRecord (owner, values) | TSum (owner, values)) when name = owner ->
          if List.length args <> List.length values then
            Error ("Record type arity mismatch for " ^ name)
          else matchMany args values
      | _ -> bindOrError name)
      [@warning "-4"])
  | TSum (name, args) -> (
      (match actual with
      | (TSum (owner, values) | TRecord (owner, values)) when name = owner ->
          if List.length args <> List.length values then
            Error ("Sum type arity mismatch for " ^ name)
          else matchMany args values
      | _ -> bindOrError name)
      [@warning "-4"])
  | TFunction (params, result) -> (
      (match actual with
      | TFunction (values, return) ->
          if List.length params <> List.length values then
            Error
              (Printf.sprintf
                 "Function arity mismatch: expected %d params, got %d"
                 (List.length params) (List.length values))
          else
            let paramResults = List.map2 matchTypes params values in
            let retResult = matchTypes result return in
            combineResults retResult paramResults
      | _ -> bindOrError "function")
      [@warning "-4"])
  | TTuple params -> (
      (match actual with
      | TTuple values ->
          if List.length params <> List.length values then
            Error
              (Printf.sprintf "Tuple size mismatch: expected %d, got %d"
                 (List.length params) (List.length values))
          else matchMany params values
      | _ -> bindOrError "tuple")
      [@warning "-4"])
  | TDict (key, value) -> (
      (match actual with
      | TDict (actualKey, actualValue) ->
          combineResults (matchTypes key actualKey)
            [ matchTypes value actualValue ]
      | _ -> bindOrError "Dict<...>")
      [@warning "-4"])

(*
   Check if a type contains type variables
   The element variable the checker gives an empty list literal that nothing
   has typed yet. Spelled like a freshened
   parameter so that inference may bind it; a declared `'t` must stay rigid.
*)
let emptyListElementVar = "t$empty"

(*
   A variable inference may bind: a callee's freshened parameter, the open
   element of an empty list literal, or one of the checker's own placeholders
   (an untyped lambda binding, a call result through a variable, a pattern's
   element). A declared type parameter is not one.
*)
let isInferenceVar name =
  String.starts_with ~prefix:"#infer:" name
  || Text.contains name "$"
  || String.starts_with ~prefix:"binding_" name
  || String.starts_with ~prefix:"__" name
  || String.starts_with ~prefix:"recursiveParameter" name

let rec containsTVar = function
  | TVar _ | TInferenceVar _ -> true
  | TList inner | TStream inner -> containsTVar inner
  | TDict (key, value) -> containsTVar key || containsTVar value
  | TTuple args | TRecord (_, args) | TSum (_, args) ->
      List.exists containsTVar args
  | TFunction (args, result) ->
      List.exists containsTVar args || containsTVar result
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr ->
      false

(*
   Check if two types are compatible (can be unified)
   Type variables in either type can match concrete types
*)
let typesCompatible expected actual = Result.is_ok (matchTypes expected actual)

let rec nominalComparisonType = function
  | TSum (name, args) | TRecord (name, args) ->
      TRecord (name, List.map nominalComparisonType args)
  | TFunction (args, result) ->
      TFunction
        (List.map nominalComparisonType args, nominalComparisonType result)
  | TTuple args -> TTuple (List.map nominalComparisonType args)
  | TList inner -> TList (nominalComparisonType inner)
  | TDict (key, value) ->
      TDict (nominalComparisonType key, nominalComparisonType value)
  | ( TVar _ | TInferenceVar _ | TInt8 | TInt16 | TInt32 | TInt64 | TInt128
    | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64
    | TString | TBlob | TChar | TDateTime | TUnit | TNever | TInternalRawPtr
    | TStream _ ) as other ->
      other

(*
   Check if two types are compatible after resolving type aliases
   Combines alias resolution with type variable unification
*)
let typesCompatibleWithAliases aliases expected actual =
  let expected = Types.resolveType aliases expected
  and actual = Types.resolveType aliases actual in
  typesCompatible expected actual
  || typesCompatible
       (nominalComparisonType expected)
       (nominalComparisonType actual)

(*
   Consolidate bindings, checking for conflicts where the same type variable
   is bound to different types. Returns a map from type var name to concrete type.
   When a type var is bound to both a type containing TVars and a concrete type, prefer the concrete type.
   existing contains TVars, new is concrete - prefer new
   new contains TVars, existing is concrete - keep existing
   Both contain TVars. Where one side's variables are ones
   inference may bind (a generic seed passed to a generic
   fold: b$0 = Parser<a$1> from the seed and Parser<a> from
   the expected result; or `([], 0)` as a seed next to the
   (List<a>, Int) the lambda returns), bind those to the
   other side and keep the other side; the caller applies
   the map transitively. Otherwise keep the first.
   Both are concrete but different - that's an error
*)
let consolidateBindings bindings =
  List.fold_left
    (fun acc (name, typ) ->
      Result.bind acc (fun map ->
          match M.find_opt name map with
          | None -> Ok (M.add name typ map)
          | Some existing ->
              if equal existing typ then Ok map
              else if containsTVar existing && not (containsTVar typ) then
                Ok (M.add name typ map)
              else if containsTVar typ && not (containsTVar existing) then
                Ok map
              else if containsTVar existing && containsTVar typ then
                let freshenedOnly bindings =
                  List.for_all (fun (name, _) -> isInferenceVar name) bindings
                in
                let includeAbsent extra map =
                  List.fold_left
                    (fun map (name, typ) ->
                      if M.mem name map then map else M.add name typ map)
                    map extra
                in
                match matchTypes existing typ with
                | Ok extra when freshenedOnly extra ->
                    Ok (includeAbsent extra (M.add name typ map))
                | Ok _ | Error _ -> (
                    match matchTypes typ existing with
                    | Ok extra when freshenedOnly extra ->
                        Ok (includeAbsent extra map)
                    | Ok _ | Error _ -> Ok map)
              else
                Error
                  ("Type variable "
                  ^ typeToString (CheckingDiagnostics.inferenceVarForKey name)
                  ^ " has conflicting inferences: " ^ typeToString existing
                  ^ " vs " ^ typeToString typ)))
    (Ok M.empty) bindings

(*
   Unify a type pattern (may contain TVar) with a concrete type.
   Returns a substitution mapping type variables to concrete types.
   Example: unifyTypes (TVar "t") TInt64 = Ok (Map.ofList [("t", TInt64)])
*)
let unifyTypes pattern actual =
  Result.bind (matchTypes pattern actual) consolidateBindings

(*
   Reconcile two types where one might contain type variables.
   If one type is concrete and the other has type variables that can unify with it,
   returns the concrete type. If both are concrete and equal, returns the type.
   If both are concrete and different, returns None.
   The optional aliasReg parameter allows type alias resolution before comparison.
   Resolve type aliases if registry is provided
   Alias declarations are collected before constructor lookup has
   canonicalized a bare nominal reference from TRecord to TSum. Treat the
   two internal spellings as equivalent when their fully-qualified names
   and arguments agree. A source program cannot declare a record and sum
   with the same fully-qualified name, so this does not erase a meaningful
   nominal distinction.
   t2 is concrete, check if t1 can unify with it
   Return the concrete type
   t1 is concrete, check if t2 can unify with it
   Both have type variables. Bind the side whose variables inference
   may bind and keep the other: `(List<t>, Int)` from a `([], 0)` seed
   meets `(List<a>, Int)` from the other arm, and the answer is the
   arm's, not the seed's, whichever came first.
*)
let rec reconcileTypes aliases first second =
  let resolve typ =
    match aliases with
    | None -> typ
    | Some aliases -> Types.resolveType aliases typ
  in
  let first = resolve first and second = resolve second in
  let reconcileMany left right =
    if List.length left <> List.length right then None
    else
      List.fold_left
        (fun acc (left, right) ->
          match (acc, reconcileTypes aliases left right) with
          | Some types, Some typ -> Some (typ :: types)
          | _ -> None)
        (Some []) (List.combine left right)
      |> Option.map List.rev
  in
  if equal (nominalComparisonType first) (nominalComparisonType second) then
    Some first
  else
    (match (first, second) with
    | TNever, right -> Some right
    | left, TNever -> Some left
    | TList left, TList right ->
        Option.map
          (fun inner -> TList inner)
          (reconcileTypes aliases left right)
    | TTuple left, TTuple right ->
        Option.map (fun args -> TTuple args) (reconcileMany left right)
    | TDict (leftKey, leftValue), TDict (rightKey, rightValue) -> (
        match
          ( reconcileTypes aliases leftKey rightKey,
            reconcileTypes aliases leftValue rightValue )
        with
        | Some key, Some value -> Some (TDict (key, value))
        | _ -> None)
    | TFunction (leftArgs, leftReturn), TFunction (rightArgs, rightReturn) -> (
        match
          ( reconcileMany leftArgs rightArgs,
            reconcileTypes aliases leftReturn rightReturn )
        with
        | Some args, Some return -> Some (TFunction (args, return))
        | _ -> None)
    | TRecord (name, leftArgs), TRecord (other, rightArgs) when name = other ->
        Option.map
          (fun args -> TRecord (name, args))
          (reconcileMany leftArgs rightArgs)
    | TSum (name, leftArgs), TSum (other, rightArgs) when name = other ->
        Option.map
          (fun args -> TSum (name, args))
          (reconcileMany leftArgs rightArgs)
    | left, right when containsTVar left && not (containsTVar right) ->
        if Result.is_ok (unifyTypes left right) then Some right else None
    | left, right when (not (containsTVar left)) && containsTVar right ->
        if Result.is_ok (unifyTypes right left) then Some left else None
    | left, right when containsTVar left && containsTVar right -> (
        let onlyInference subst =
          M.for_all (fun name _ -> isInferenceVar name) subst
        in
        match unifyTypes left right with
        | Ok subst when onlyInference subst ->
            Some (Types.applySubst subst left)
        | firstDirection -> (
            match unifyTypes right left with
            | Ok subst when onlyInference subst ->
                Some (Types.applySubst subst right)
            | Ok _ | Error _ -> (
                match firstDirection with
                | Ok subst -> Some (Types.applySubst subst left)
                | Error _ -> None)))
    | _ -> None)
    [@warning "-4"]

(*
   Infer type arguments for a generic function call.
   Given type parameters, parameter types (with type variables), and actual argument types,
   returns the inferred type arguments in order matching typeParams.
   Also takes optional function return type and expected return type for additional inference.
   Match each parameter type against argument type
   Also match return type against expected return type if both are provided
   Combine all bindings
   Extract type arguments in order of type parameters, preserving unresolved type vars.
*)
let inferTypeArgs params parameterTypes argumentTypes returnType
    expectedReturnType =
  if List.length parameterTypes <> List.length argumentTypes then
    Error
      (Printf.sprintf "Argument count mismatch: expected %d, got %d"
         (List.length parameterTypes)
         (List.length argumentTypes))
  else
    let argResults = List.map2 matchTypes parameterTypes argumentTypes in
    let returnResult =
      match (returnType, expectedReturnType) with
      | Some left, Some right -> matchTypes left right
      | _ -> Ok []
    in
    Result.bind
      (combineResults (Ok []) (argResults @ [ returnResult ]))
      (fun bindings ->
        Result.map
          (fun map ->
            List.map
              (fun param ->
                match M.find_opt param map with
                | Some typ -> Types.applySubst map typ
                | None -> CheckingDiagnostics.inferenceVarForKey param)
              params)
          (consolidateBindings bindings))

(*
   Look up a name already resolved and canonicalized by the semantic boundary.
*)
let tryLookupResolved name map =
  Option.map (fun value -> (value, name)) (M.find_opt name map)
