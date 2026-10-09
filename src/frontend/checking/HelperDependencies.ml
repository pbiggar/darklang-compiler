(*
   HelperDependencies.ml - Solve transitive generated comparison-helper dependencies.
*)
(* HelperDependencies.ml - Solve transitive generated comparison-helper dependencies. *)
[@@@warning "-30"]

open! AST
module M = StringOrder.Map
module S = StringOrder.Set

module TypeSet = Set.Make (struct
  type t = AST.semanticType

  let compare = AST.compareSemanticType
end)

type eqHelperGenerationState = {
  inProgress : S.t;
  generated : AST.functionDef M.t;
}

type compareHelperGenerationState = {
  inProgress : S.t;
  generated : AST.functionDef M.t;
}

let distinctBy name values =
  let _, reversed =
    List.fold_left
      (fun (seen, values) value ->
        let key = name value in
        if S.mem key seen then (seen, values)
        else (S.add key seen, value :: values))
      (S.empty, []) values
  in
  List.rev reversed

(*
   A checked record literal contains each declaration slot exactly once.
   The list retains source evaluation order; layout order is selected only
   after every initializer has been evaluated.
*)
let recordFields aliases registry name args =
  match M.find_opt name registry with
  | None -> []
  | Some (info : Types.recordTypeInfo) ->
      let subst =
        Types.buildRecordFieldSubstitutionFromParams info.Types.typeParams args
      in
      List.map
        (fun (_, typ) ->
          Types.resolveType aliases
            (match subst with
            | Ok subst -> Types.applyTypeArguments subst typ
            | Error _ -> typ))
        info.Types.fields

let sumFields sums name args =
  match M.find_opt name sums with
  | None -> []
  | Some (info : Types.sumTypeInfo) ->
      List.concat_map
        (fun (variant : Types.sumVariantInfo) ->
          let subst =
            if List.length info.Types.typeParams = List.length args then
              M.of_list (List.combine info.Types.typeParams args)
            else M.empty
          in
          List.map (Types.applySubst subst) variant.Types.fields)
        info.Types.variants

let collectDirectEqHelperDeps aliases registry lookup sums typ =
  let canonical typ =
    ComparisonPlanning.canonicalEqualityType lookup
      (Types.resolveType aliases typ)
  in
  let add typ =
    let resolved = canonical typ in
    if ComparisonPlanning.needsEqHelperForResolvedType lookup resolved then
      Some resolved
    else None
  in
  let deps =
    match canonical typ with
    | TList inner -> List.filter_map add [ inner ]
    | TDict (key, value) ->
        List.filter_map add [ TList (TTuple [ key; value ]) ]
    | TTuple args -> List.filter_map add args
    | TRecord (name, args) ->
        List.filter_map add (recordFields aliases registry name args)
    | TSum (name, args) -> List.filter_map add (sumFields sums name args)
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
    | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
    | TDateTime | TUnit | TNever | TInternalRawPtr | TVar _ | TInferenceVar _
    | TFunction _ | TStream _ ->
        []
  in
  distinctBy ComparisonPlanning.eqHelperName deps

let rec ensureEqHelperForType aliases registry lookup sums typ
    (state : eqHelperGenerationState) : eqHelperGenerationState =
  let canonical typ =
    ComparisonPlanning.canonicalEqualityType lookup
      (Types.resolveType aliases typ)
  in
  let resolved = canonical typ in
  let rec known typ =
    match canonical typ with
    | TRecord (name, args) -> M.mem name registry && List.for_all known args
    | TSum (name, args) -> M.mem name sums && List.for_all known args
    | TList inner -> known inner
    | TDict (key, value) -> known key && known value
    | TTuple args -> List.for_all known args
    | TFunction (args, result) -> List.for_all known args && known result
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
    | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
    | TDateTime | TUnit | TNever | TInternalRawPtr | TVar _ | TInferenceVar _
    | TStream _ ->
        true
  in
  if
    (not (ComparisonPlanning.needsEqHelperForResolvedType lookup resolved))
    || not (known resolved)
  then state
  else
    let helper = ComparisonPlanning.eqHelperName resolved in
    if M.mem helper state.generated || S.mem helper state.inProgress then state
    else
      let state : eqHelperGenerationState =
        { state with inProgress = S.add helper state.inProgress }
      in
      let state =
        List.fold_left
          (fun state typ ->
            ensureEqHelperForType aliases registry lookup sums typ state)
          state
          (collectDirectEqHelperDeps aliases registry lookup sums resolved)
      in
      let left = "__dark_eq_left" and right = "__dark_eq_right" in
      let body =
        EqualityHelpers.buildEqHelperExpr aliases registry lookup sums
          EqualityHelpers.ExpandCurrent resolved (Var left) (Var right)
      in
      let definition : AST.functionDef =
        {
          name = helper;
          typeParams = [];
          params = NonEmptyList.fromList [ (left, resolved); (right, resolved) ];
          returnType = TBool;
          body;
          recursion = None;
        }
      in
      {
        inProgress = S.remove helper state.inProgress;
        generated = M.add helper definition state.generated;
      }

let collectDirectCompareHelperDeps aliases registry sums typ =
  let resolved = Types.resolveType aliases typ in
  let deps =
    match resolved with
    | TList inner -> [ Types.resolveType aliases inner; resolved ]
    | TDict (key, value) ->
        [
          TList
            (TTuple
               [
                 Types.resolveType aliases key; Types.resolveType aliases value;
               ]);
        ]
    | TTuple args -> List.map (Types.resolveType aliases) args
    | TRecord (name, args) -> recordFields aliases registry name args
    | TSum (name, args) ->
        List.map (Types.resolveType aliases) (sumFields sums name args)
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
    | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
    | TDateTime | TUnit | TNever | TInternalRawPtr | TVar _ | TInferenceVar _
    | TFunction _ | TStream _ ->
        []
  in
  distinctBy ComparisonPlanning.compareHelperName deps

let rec ensureCompareHelperForType aliases registry lookup sums typ
    (state : compareHelperGenerationState) : compareHelperGenerationState =
  let resolved = Types.resolveType aliases typ in
  let helper = ComparisonPlanning.compareHelperName resolved in
  if M.mem helper state.generated || S.mem helper state.inProgress then state
  else
    let state : compareHelperGenerationState =
      { state with inProgress = S.add helper state.inProgress }
    in
    let state =
      List.fold_left
        (fun state typ ->
          ensureCompareHelperForType aliases registry lookup sums typ state)
        state
        (collectDirectCompareHelperDeps aliases registry sums resolved)
    in
    let left = "__dark_compare_left" and right = "__dark_compare_right" in
    let definition : AST.functionDef =
      {
        name = helper;
        typeParams = [];
        params = NonEmptyList.fromList [ (left, resolved); (right, resolved) ];
        returnType = TInt64;
        body =
          OrderingHelpers.buildCompareHelperExpr aliases registry lookup sums
            EqualityHelpers.ExpandCurrent resolved (Var left) (Var right);
        recursion = None;
      }
    in
    {
      inProgress = S.remove helper state.inProgress;
      generated = M.add helper definition state.generated;
    }

type helperKind = Equality | Compare

let rec collectHelperTypes kind aliases expr =
  let recurse = collectHelperTypes kind aliases in
  let collect values =
    List.fold_left
      (fun acc expr -> TypeSet.union acc (recurse expr))
      TypeSet.empty values
  in
  let concrete typ =
    let typ = Types.resolveType aliases typ in
    if Unification.containsTVar typ then None else Some typ
  in
  match expr with
  | BoundaryRender (_, value) -> recurse value
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
  | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
  | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
  | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _
  | RuntimeError _ ->
      TypeSet.empty
  | BinOp (_, left, right)
  | Let (_, left, right)
  | RecursiveLet (_, left, right)
  | Sequence (left, right) ->
      TypeSet.union (recurse left) (recurse right)
  | UnaryOp (_, inner) -> recurse inner
  | If (condition, yes, no) ->
      TypeSet.union (recurse condition)
        (TypeSet.union (recurse yes) (recurse no))
  | Apply (func, args, values) as applied -> (
      let nested = collect (NonEmptyList.toList values) in
      match kind with
      | Equality -> (
          match ComparisonPlanning.tryDecodeInternalTypeApp applied with
          | Some
              (ComparisonPlanning.EqHelperDispatchTypeApp (target, left, right))
            ->
              TypeSet.add
                (Types.resolveType aliases target)
                (TypeSet.union (recurse left) (recurse right))
          | None ->
              List.fold_left
                (fun helpers typ ->
                  match concrete typ with
                  | None -> helpers
                  | Some typ -> TypeSet.add typ helpers)
                (TypeSet.union (recurse func) nested)
                args)
      | Compare -> (
          let selected =
            (match (func, args) with
            | ( Var
                  ( "__compare" | "Darklang.Stdlib.List.sort"
                  | "Darklang.Stdlib.List.unique" ),
                [ typ ] )
            | Var "Darklang.Stdlib.List.uniqueBy", [ typ; _ ] ->
                Some typ
            | Var "Darklang.Stdlib.List.sortBy", [ value; key ] ->
                Some
                  (TTuple
                     [
                       Types.resolveType aliases key;
                       Types.resolveType aliases value;
                     ])
            | _ -> None)
            [@warning "-4"]
          in
          match selected with
          | Some typ -> (
              match concrete typ with
              | Some typ -> TypeSet.add typ nested
              | None -> nested)
          | None -> TypeSet.union (recurse func) nested))
  | TupleLiteral values
  | ListLiteral values
  | Constructor (_, _, values)
  | Closure (_, values) ->
      collect values
  | TupleAccess (tuple, _) | RecordAccess (tuple, _) -> recurse tuple
  | DictLiteral (_, _, entries) ->
      collect (List.concat_map (fun (key, value) -> [ key; value ]) entries)
  | RecordLiteral (_, fields) -> collect (List.map snd fields)
  | RecordUpdate (record, fields) ->
      TypeSet.union (recurse record) (collect (List.map snd fields))
  | Match (scrutinee, cases) ->
      let cases =
        List.fold_left
          (fun acc (case : AST.matchCase) ->
            let guards =
              match case.guard with
              | Some guard -> recurse guard
              | None -> TypeSet.empty
            in
            TypeSet.union acc (TypeSet.union guards (recurse case.body)))
          TypeSet.empty cases
      in
      TypeSet.union (recurse scrutinee) cases
  | Lambda (_, _, body) -> recurse body
  | IndirectApply (func, args) ->
      TypeSet.union (recurse func) (collect (NonEmptyList.toList args))
  | InterpolatedString parts ->
      List.fold_left
        (fun acc -> function
          | StringText _ -> acc
          | StringExpr expr -> TypeSet.union acc (recurse expr))
        TypeSet.empty parts

let collectCompareHelperTypesFromExpr aliases expr =
  collectHelperTypes Compare aliases expr

let collectEqHelperTypesFromExpr aliases expr =
  collectHelperTypes Equality aliases expr
