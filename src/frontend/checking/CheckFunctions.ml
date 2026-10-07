(* CheckFunctions.ml - Check function bodies and collect concrete declaration specializations. *)
open! AST
open CheckingDiagnostics
open Types

module SpecificationSet = Set.Make (struct
  type t = string * semanticType list

  let rec compareTypes left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | left :: leftTail, right :: rightTail ->
        let c = AST.compareSemanticType left right in
        if c = 0 then compareTypes leftTail rightTail else c

  let compare (leftName, leftArgs) (rightName, rightArgs) =
    let c = StringOrder.compare leftName rightName in
    if c = 0 then compareTypes leftArgs rightArgs else c
end)

(* Type-check a function definition
   Returns the transformed function body (with Call -> TypeApp transformations) *)
let[@warning "-4"] checkFunctionDefWithSumTypeNames funcParamNameReg
    sumTypeNames indexedSumTypeReg (funcDef : functionDef) env typeReg
    variantLookup genericFuncReg warningSettings moduleRegistry aliasReg =
  let canonical typ =
    canonicalizeDeclaredTypeRefsWithSumTypeNames typeReg sumTypeNames typ
    |> resolveType aliasReg
    |> canonicalizeBareSumTypeRefsWithNames sumTypeNames
  in
  let canonicalParams =
    NonEmptyList.map (fun (name, typ) -> (name, canonical typ)) funcDef.params
  in
  let canonicalReturnType = canonical funcDef.returnType in
  let canonicalFuncDef =
    { funcDef with params = canonicalParams; returnType = canonicalReturnType }
  in
  (* Build environment with parameters *)
  let paramEnv =
    List.fold_left
      (fun e (name, typ) -> StringOrder.Map.add name typ e)
      env
      (NonEmptyList.toList canonicalParams)
  in
  (* Check body has return type *)
  let bodyCheckResult =
    CheckExpressions.checkExprWithParamNamesAndSumTypeNames funcParamNameReg
      sumTypeNames indexedSumTypeReg funcDef.body paramEnv typeReg variantLookup
      genericFuncReg warningSettings moduleRegistry aliasReg
      (Some canonicalReturnType)
  in
  let bodyCheckWithLegacyInterpreterErrors =
    if genericFuncReg.requireExplicitTypeArgsForBareCalls then
      Result.map_error
        (function
          | TypeMismatch (expectedType, actualType, _)
            when Unification.typesCompatibleWithAliases aliasReg expectedType
                   canonicalReturnType
                 && not (isNeverType actualType) ->
              let actualValue =
                Option.value
                  (tryFormatLiteralValue funcDef.body)
                  ~default:(typeToString actualType)
              in
              GenericError
                (funcDef.name ^ "'s return value expects "
                ^ typeToString canonicalReturnType
                ^ ", but got " ^ typeToString actualType ^ " (" ^ actualValue
                ^ ")")
          | error -> error)
        bodyCheckResult
    else bodyCheckResult
  in
  Result.bind bodyCheckWithLegacyInterpreterErrors (fun (bodyType, body) ->
      let resolvedReturnType = resolveType aliasReg canonicalReturnType
      and resolvedBodyType = resolveType aliasReg bodyType in
      let allowGenericReturnSpecialization =
        Unification.containsTVar resolvedReturnType
        && (not (Unification.containsTVar resolvedBodyType))
        && Unification.typesCompatibleWithAliases aliasReg resolvedReturnType
             resolvedBodyType
      in
      let rec nominallyIdentical left right =
        match (left, right) with
        | TSum (leftName, leftArgs), TRecord (rightName, rightArgs)
        | TRecord (leftName, leftArgs), TSum (rightName, rightArgs)
        | TSum (leftName, leftArgs), TSum (rightName, rightArgs)
        | TRecord (leftName, leftArgs), TRecord (rightName, rightArgs)
          when leftName = rightName
               && List.length leftArgs = List.length rightArgs ->
            List.for_all2 nominallyIdentical leftArgs rightArgs
        | ( TFunction (leftParams, leftReturn),
            TFunction (rightParams, rightReturn) )
          when List.length leftParams = List.length rightParams ->
            List.for_all2 nominallyIdentical leftParams rightParams
            && nominallyIdentical leftReturn rightReturn
        | TTuple leftTypes, TTuple rightTypes
          when List.length leftTypes = List.length rightTypes ->
            List.for_all2 nominallyIdentical leftTypes rightTypes
        | TList leftType, TList rightType ->
            nominallyIdentical leftType rightType
        | TDict (leftKey, leftValue), TDict (rightKey, rightValue) ->
            nominallyIdentical leftKey rightKey
            && nominallyIdentical leftValue rightValue
        | _ -> left = right
      in
      if
        nominallyIdentical resolvedReturnType resolvedBodyType
        || allowGenericReturnSpecialization
      then
        let monomorphicType =
          TFunction
            ( List.map snd (NonEmptyList.toList canonicalParams),
              canonicalReturnType )
        in
        let typedRecursion =
          match canonicalFuncDef.recursion with
          | Some (ResolvedRecursiveBinding resolved) ->
              Some (TypedRecursiveBinding { resolved; monomorphicType })
          | Some (TypedRecursiveBinding typed) ->
              Some (TypedRecursiveBinding { typed with monomorphicType })
          | other -> other
        in
        Ok { canonicalFuncDef with body; recursion = typedRecursion }
      else
        Error
          (TypeMismatch
             ( canonicalReturnType,
               bodyType,
               "function " ^ funcDef.name ^ " body" )))

let specializeFunctionForTypeCheck (funcDef : functionDef) typeArgs =
  match buildSubstitution funcDef.typeParams typeArgs with
  | Error message -> Error (GenericError message)
  | Ok subst ->
      Ok
        {
          funcDef with
          typeParams = [];
          params =
            NonEmptyList.map
              (fun (name, typ) -> (name, applySubst subst typ))
              funcDef.params;
          returnType = applySubst subst funcDef.returnType;
          body = applySubstToExpr subst funcDef.body;
        }

let rec collectTypeAppSpecs (expr : expr) =
  let union = SpecificationSet.union in
  let collect values =
    List.fold_left
      (fun specs value -> union specs (collectTypeAppSpecs value))
      SpecificationSet.empty values
  in
  match expr with
  | BoundaryRender (_, value) -> collectTypeAppSpecs value
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
  | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
  | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
  | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _
  | Closure _ | RuntimeError _ ->
      SpecificationSet.empty
  | BinOp (_, left, right)
  | Let (_, left, right)
  | RecursiveLet (_, left, right)
  | Sequence (left, right) ->
      union (collectTypeAppSpecs left) (collectTypeAppSpecs right)
  | UnaryOp (_, inner)
  | TupleAccess (inner, _)
  | RecordAccess (inner, _)
  | Lambda (_, _, inner) ->
      collectTypeAppSpecs inner
  | If (condition, thenBranch, elseBranch) ->
      union
        (collectTypeAppSpecs condition)
        (union
           (collectTypeAppSpecs thenBranch)
           (collectTypeAppSpecs elseBranch))
  | Apply (Var name, typeArgs, args) ->
      let specs = collect (NonEmptyList.toList args) in
      if typeArgs = [] then specs
      else SpecificationSet.add (name, typeArgs) specs
  | TupleLiteral elements | ListLiteral elements | Constructor (_, _, elements)
    ->
      collect elements
  | DictLiteral (_, _, entries) ->
      collect (List.concat_map (fun (key, value) -> [ key; value ]) entries)
  | RecordLiteral (_, fields) -> collect (List.map snd fields)
  | RecordUpdate (record, updates) ->
      union (collectTypeAppSpecs record) (collect (List.map snd updates))
  | Match (scrutinee, cases) ->
      union
        (collectTypeAppSpecs scrutinee)
        (List.fold_left
           (fun specs (case : matchCase) ->
             union specs
               (union
                  (Option.fold ~none:SpecificationSet.empty
                     ~some:collectTypeAppSpecs case.guard)
                  (collectTypeAppSpecs case.body)))
           SpecificationSet.empty cases)
  | Apply (funcExpr, _, args) | IndirectApply (funcExpr, args) ->
      union (collectTypeAppSpecs funcExpr) (collect (NonEmptyList.toList args))
  | InterpolatedString parts ->
      List.fold_left
        (fun specs part ->
          match part with
          | StringText _ -> specs
          | StringExpr value -> union specs (collectTypeAppSpecs value))
        SpecificationSet.empty parts
