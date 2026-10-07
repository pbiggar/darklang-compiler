(*
   ComparisonPlanning.ml - Plan typed equality and ordering operations.
*)
(* ComparisonPlanning.ml - Plan typed equality and ordering operations. *)
open! AST
module M = StringOrder.Map
module S = StringOrder.Set

module TypeSet = Set.Make (struct
  type t = AST.semanticType

  let compare = AST.compareSemanticType
end)

(*
   Internal type-app markers emitted during type checking.
   These markers are materialized to direct calls before leaving this pass.
*)
type internalTypeAppMarker = EqHelperDispatch

(*
   Internal type-app values carried in `Expr.TypeApp` nodes.
   We encode/decode them through marker names at the pass boundary.
*)
type internalTypeApp =
  | EqHelperDispatchTypeApp of AST.semanticType * AST.expr * AST.expr

(*
   A comparison is classified while both resolved operand types are available.
   Equality plans may still contain type variables in a generic body; the
   internal typed marker carries them through substitution and is materialized
   only after a concrete specialization exists.
*)
type comparisonPlan =
  | EqualityComparison of AST.semanticType
  | OrderingComparison of AST.semanticType

let internalTypeAppMarkerName EqHelperDispatch =
  "__dark_internal_eq_helper_dispatch"

let tryParseInternalTypeAppMarker name =
  if name = internalTypeAppMarkerName EqHelperDispatch then
    Some EqHelperDispatch
  else None

let makeInternalTypeApp (EqHelperDispatchTypeApp (target, left, right)) =
  Apply
    ( Var (internalTypeAppMarkerName EqHelperDispatch),
      [ target ],
      NonEmptyList.fromList [ left; right ] )

let tryDecodeInternalTypeApp expr =
  (match expr with
  | Apply (Var name, [ target ], { NonEmptyList.head = left; tail = [ right ] })
    ->
      Option.map
        (fun EqHelperDispatch -> EqHelperDispatchTypeApp (target, left, right))
        (tryParseInternalTypeAppMarker name)
  | _ -> None)
  [@warning "-4"]

let sumTypeHasPayload lookup name =
  M.exists (fun _ (owner, _, _, fields) -> owner = name && fields <> []) lookup

(*
   Parsed source names are initially represented as TRecord. Canonicalize
   names owned by the variant registry before constructing equality plans.
*)
let rec canonicalEqualityType lookup typ =
  let visit = canonicalEqualityType lookup in
  match typ with
  | TRecord (name, args)
    when M.exists (fun _ (owner, _, _, _) -> owner = name) lookup ->
      TSum (name, List.map visit args)
  | TRecord (name, args) -> TRecord (name, List.map visit args)
  | TSum (name, args) -> TSum (name, List.map visit args)
  | TTuple args -> TTuple (List.map visit args)
  | TList inner -> TList (visit inner)
  | TDict (key, value) -> TDict (visit key, visit value)
  | TFunction (args, result) -> TFunction (List.map visit args, visit result)
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr | TVar _ | TInferenceVar _
  | TStream _ ->
      typ

(*
   Json conversion is fully planned at compile time. Reject shapes for which
   no plan can exist here, before specialization can turn them into runtime
   control flow.
*)
let validateJsonTargetType aliases registry lookup sums target =
  let unsupported typ =
    Error
      (CheckingDiagnostics.GenericError
         ("Unsupported type in JSON: "
         ^ CheckingDiagnostics.typeToString typ
         ^ ". Some types are not supported in Json serialization"))
  in
  let rec validate visited typ =
    let typ = canonicalEqualityType lookup (Types.resolveType aliases typ) in
    let identity = CheckingDiagnostics.typeToHelperIdentityString typ in
    if S.mem identity visited then Ok ()
    else
      let visited = S.add identity visited in
      let validateAll types =
        List.fold_left
          (fun acc typ -> Result.bind acc (fun () -> validate visited typ))
          (Ok ()) types
      in
      match typ with
      | TUnit | TBool | TInt8 | TUInt8 | TInt16 | TUInt16 | TInt32 | TUInt32
      | TInt64 | TUInt64 | TInt128 | TUInt128 | TInt | TFloat64 | TChar
      | TString | TDateTime ->
          Ok ()
      | TSum ("Uuid", []) -> Ok ()
      | TTuple args -> validateAll args
      | TList inner -> validate visited inner
      | TDict (TString, inner) -> validate visited inner
      | TRecord (name, args) -> (
          match M.find_opt name registry with
          | None -> unsupported typ
          | Some (info : Types.recordTypeInfo) -> (
              match Types.buildSubstitution info.Types.typeParams args with
              | Error _ -> unsupported typ
              | Ok subst ->
                  validateAll
                    (List.map
                       (fun (_, typ) -> Types.applySubst subst typ)
                       info.Types.fields)))
      | TSum (name, args) -> (
          match M.find_opt name sums with
          | None -> unsupported typ
          | Some (info : Types.sumTypeInfo) -> (
              match Types.buildSubstitution info.Types.typeParams args with
              | Error _ -> unsupported typ
              | Ok subst ->
                  validateAll
                    (List.concat_map
                       (fun (variant : Types.sumVariantInfo) ->
                         List.map (Types.applySubst subst) variant.Types.fields)
                       info.Types.variants)))
      | TFunction _ | TBlob | TInternalRawPtr | TNever | TStream _ | TVar _
      | TInferenceVar _ | TDict _ ->
          unsupported typ
  in
  validate S.empty target

(*
   Every concrete compound comparable type has one equality entry point.
*)
let needsEqHelperForResolvedType lookup typ =
  match canonicalEqualityType lookup typ with
  | TFunction _ | TList _ | TDict _ | TTuple _ | TRecord _ | TSum _ -> true
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr | TVar _ | TInferenceVar _
  | TStream _ ->
      false

let sanitizeHelperNamePrefix text =
  let units = Text.scalars text in
  let length = min 48 (Array.length units) in
  if length = 0 then "type"
  else
    Text.ofScalars
      (Array.init length (fun index ->
           let unit = units.(index) in
           if Text.isLetter unit || Text.isDigit unit then unit else 95))

(*
   Stable, deterministic hash used for generated helper function names.
*)
let stableHelperNameHash text =
  String.fold_left
    (fun acc byte ->
      Int64.mul
        (Int64.logxor acc (Int64.of_int (Char.code byte)))
        1099511628211L)
    0xcbf29ce484222325L text

let helperName prefix typ =
  let name =
    sanitizeHelperNamePrefix
      (CheckingDiagnostics.typeToHelperIdentityString typ)
  in
  let hash = stableHelperNameHash (StructuralFormat.semanticType typ) in
  Printf.sprintf "%s%s_%016Lx" prefix name hash

(*
   Name for a concrete structural equality helper.
   Helper identities encode both dictionary types and complete type structure,
   independently of how user-facing diagnostics are formatted.
*)
let eqHelperName typ = helperName "__dark_eq_" typ

(*
   Name for a concrete canonical three-way comparison helper.
*)
let compareHelperName typ = helperName "__dark_compare_" typ

(*
   Build a left-associative boolean conjunction chain.
*)
let chainAndExpr = function
  | [] -> BoolLiteral true
  | first :: rest ->
      List.fold_left (fun acc expr -> BinOp (And, acc, expr)) first rest

(*
   Build an equality expression for two already type-checked operands.
   For tuple/record/sum, emit a typed internal dispatch marker that will later
   be rewritten to a call to the generated concrete helper function.
   A generic comparison cannot select a representation-level operation
   until specialization. Preserve the typed plan through substitution.
*)
let buildEqExprForType aliases lookup typ left right =
  let resolved = canonicalEqualityType lookup (Types.resolveType aliases typ) in
  let dispatch typ =
    makeInternalTypeApp (EqHelperDispatchTypeApp (typ, left, right))
  in
  let call name = Apply (Var name, [], NonEmptyList.fromList [ left; right ]) in
  match resolved with
  | TVar _ | TInferenceVar _ | TFunction _ -> dispatch resolved
  | TString -> BinOp (Eq, left, right)
  | TInt -> call "Darklang.Stdlib.Int.__equals"
  | TInt128 -> call "Darklang.Stdlib.Int128.__equals"
  | TUInt128 -> call "Darklang.Stdlib.UInt128.__equals"
  | TList inner -> dispatch (TList (Types.resolveType aliases inner))
  | TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64
  | TBool | TFloat64 | TBlob | TChar | TDateTime | TUnit | TNever
  | TInternalRawPtr | TTuple _ | TRecord _ | TSum _ | TStream _ | TDict _ ->
      if needsEqHelperForResolvedType lookup resolved then dispatch resolved
      else BinOp (Eq, left, right)

let comparisonNumericType = function
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TFloat64 ->
      true
  | TBool | TString | TBlob | TChar | TDateTime | TUnit | TNever
  | TInternalRawPtr | TVar _ | TInferenceVar _ | TFunction _ | TTuple _
  | TRecord _ | TSum _ | TList _ | TStream _ | TDict _ ->
      false

type admission = Equality | Sortable | DictKey

let admittedType policy aliases registry sums typ =
  let rec admitted seen candidate =
    let resolved = Types.resolveType aliases candidate in
    if TypeSet.mem resolved seen then true
    else
      let seen = TypeSet.add resolved seen in
      let recurse = admitted seen in
      match resolved with
      | TVar _ | TInferenceVar _ | TUnit | TBool | TInt8 | TInt16 | TInt32
      | TInt64 | TInt128 | TInt | TUInt8 | TUInt16 | TUInt32 | TUInt64
      | TUInt128 | TFloat64 | TChar | TString | TDateTime ->
          true
      | TTuple args -> List.for_all recurse args
      | TList inner -> recurse inner
      | TFunction (args, result) ->
          policy = Equality && List.for_all recurse args && recurse result
      | TStream _ | TBlob -> policy = Equality
      | TDict (key, value) -> recurse key && recurse value
      | TRecord (name, args) -> (
          match M.find_opt name registry with
          | None ->
              policy <> Sortable && M.mem name sums
              && recurse (TSum (name, args))
          | Some (info : Types.recordTypeInfo) -> (
              match
                Types.buildRecordFieldSubstitutionFromParams
                  info.Types.typeParams args
              with
              | Error _ -> false
              | Ok subst ->
                  List.for_all
                    (fun (_, typ) -> recurse (Types.applySubst subst typ))
                    info.Types.fields))
      | TSum (name, args) -> (
          match M.find_opt name sums with
          | None -> true
          | Some (info : Types.sumTypeInfo) ->
              let subst =
                if List.length info.Types.typeParams = List.length args then
                  M.of_list (List.combine info.Types.typeParams args)
                else M.empty
              in
              List.for_all
                (fun (variant : Types.sumVariantInfo) ->
                  List.for_all
                    (fun field -> recurse (Types.applySubst subst field))
                    variant.Types.fields)
                info.Types.variants)
      | TInternalRawPtr | TNever -> false
  in
  admitted TypeSet.empty typ

(*
   Recursive nominal types are admissible when the cycle itself has
   introduced no rejected payload type.
   Dict key admission remains owned by Dict. Comparison reuses
   the key semantics already selected for an admitted key type.
*)
let equalityComparableType aliases registry sums typ =
  admittedType Equality aliases registry sums typ

(*
   Canonical sorting is selected statically. Values with no interpreter
   ordering in the compiler representation are rejected before lowering.
*)
let canonicalSortableType aliases registry sums typ =
  admittedType Sortable aliases registry sums typ

(*
   Dict keys use structural equality and therefore must not retain executable,
   streaming, opaque, or compiler-internal values anywhere in their shape.
*)
let dictKeyAdmissibleType aliases registry sums typ =
  admittedType DictKey aliases registry sums typ

let validateDictKeyCall aliases registry sums name args =
  match args with
  | key :: _
    when String.starts_with ~prefix:"Darklang.Stdlib.Dict." name
         && not (String.starts_with ~prefix:"Darklang.Stdlib.Dict.__" name)
         || String.starts_with ~prefix:"Dict." name
            && not (String.starts_with ~prefix:"Dict.__" name) ->
      if dictKeyAdmissibleType aliases registry sums key then Ok ()
      else
        Error
          (CheckingDiagnostics.GenericError
             ("Type "
             ^ CheckingDiagnostics.typeToString (Types.resolveType aliases key)
             ^ " cannot be used as a Dict key"))
  | [] | _ :: _ -> Ok ()

let validateCanonicalSortableCall aliases registry sums name args =
  let required =
    (match (name, args) with
    | ( ( "__compare" | "Darklang.Stdlib.List.sort"
        | "Darklang.Stdlib.List.unique" ),
        [ typ ] )
    | "Darklang.Stdlib.List.uniqueBy", [ typ; _ ] ->
        [ typ ]
    | "Darklang.Stdlib.List.sortBy", [ typ; key ] -> [ typ; key ]
    | _ -> [])
    [@warning "-4"]
  in
  match
    List.find_opt
      (fun typ -> not (canonicalSortableType aliases registry sums typ))
      required
  with
  | None -> Ok ()
  | Some typ ->
      Error
        (CheckingDiagnostics.GenericError
           ("Canonical sorting is not supported for type "
           ^ CheckingDiagnostics.typeToString (Types.resolveType aliases typ)))

let rec reconcileComparisonTypes aliases left right =
  let left = Types.resolveType aliases left
  and right = Types.resolveType aliases right in
  let rec reconcileMany left right acc =
    match (left, right) with
    | [], [] -> Some (List.rev acc)
    | first :: rest, second :: more ->
        Option.bind (reconcileComparisonTypes aliases first second) (fun typ ->
            reconcileMany rest more (typ :: acc))
    | [], _ :: _ | _ :: _, [] -> None
  in
  if AST.compareSemanticType left right = 0 then Some left
  else
    (match (left, right) with
    | TNever, other
    | other, TNever
    | TVar _, other
    | other, TVar _
    | TInferenceVar _, other
    | other, TInferenceVar _ ->
        Some other
    | TList left, TList right ->
        Option.map
          (fun typ -> TList typ)
          (reconcileComparisonTypes aliases left right)
    | TStream left, TStream right ->
        Option.map
          (fun typ -> TStream typ)
          (reconcileComparisonTypes aliases left right)
    | TDict (leftKey, leftValue), TDict (rightKey, rightValue) ->
        Option.bind (reconcileComparisonTypes aliases leftKey rightKey)
          (fun key ->
            Option.map
              (fun value -> TDict (key, value))
              (reconcileComparisonTypes aliases leftValue rightValue))
    | TTuple left, TTuple right ->
        Option.map (fun args -> TTuple args) (reconcileMany left right [])
    | TRecord (name, args), TRecord (other, more) when name = other ->
        Option.map
          (fun args -> TRecord (name, args))
          (reconcileMany args more [])
    | TSum (name, args), TSum (other, more) when name = other ->
        Option.map (fun args -> TSum (name, args)) (reconcileMany args more [])
    | TFunction (args, result), TFunction (more, return) ->
        Option.bind (reconcileMany args more []) (fun args ->
            Option.map
              (fun result -> TFunction (args, result))
              (reconcileComparisonTypes aliases result return))
    | _ -> None)
    [@warning "-4"]

(*
   A literal or other concrete operand constrains the unresolved
   generic operand. The enclosing call can remain polymorphic when
   no value reaches that operand (for example List.all []).
*)
let classifyComparison aliases registry _lookup sums op left right =
  let left = Types.resolveType aliases left
  and right = Types.resolveType aliases right in
  match op with
  | Eq | Neq -> (
      match reconcileComparisonTypes aliases left right with
      | Some typ when equalityComparableType aliases registry sums typ ->
          Ok (EqualityComparison typ)
      | Some _ | None ->
          Error (CheckingDiagnostics.IncompatibleEqualityOperands (left, right))
      )
  | Lt | Gt | Lte | Gte -> (
      match reconcileComparisonTypes aliases left right with
      | Some typ when comparisonNumericType typ -> Ok (OrderingComparison typ)
      | Some _ | None ->
          Error (CheckingDiagnostics.IncompatibleOrderingOperands (left, right))
      )
  | Add | Sub | Mul | Div | Mod | Pow | Shl | Shr | BitAnd | BitOr | BitXor
  | StringConcat | And | Or ->
      Crash.crash
        ("Non-comparison operator reached comparison classification: "
       ^ StructuralFormat.binOp op)

let orderingFunctionName = function
  | Lt -> "Darklang.Stdlib.Int.lessThan"
  | Gt -> "Darklang.Stdlib.Int.greaterThan"
  | Lte -> "Darklang.Stdlib.Int.lessThanOrEqualTo"
  | Gte -> "Darklang.Stdlib.Int.greaterThanOrEqualTo"
  | ( Add | Sub | Mul | Div | Mod | Pow | Shl | Shr | BitAnd | BitOr | BitXor
    | StringConcat | Eq | Neq | And | Or ) as op ->
      Crash.crash
        ("Non-ordering operator has no Int comparison helper: "
       ^ StructuralFormat.binOp op)

let buildOrderingExprForType op typ left right =
  match typ with
  | TInt128 | TUInt128 ->
      let name =
        if typ = TInt128 then "__int128_to_int" else "__uint128_to_int"
      in
      let convert value = Apply (Var name, [], NonEmptyList.singleton value) in
      Apply
        ( Var (orderingFunctionName op),
          [],
          NonEmptyList.fromList [ convert left; convert right ] )
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt | TUInt8 | TUInt16 | TUInt32
  | TUInt64 | TFloat64 | TBool | TString | TBlob | TChar | TDateTime | TUnit
  | TNever | TInternalRawPtr | TVar _ | TInferenceVar _ | TFunction _ | TTuple _
  | TRecord _ | TSum _ | TList _ | TStream _ | TDict _ ->
      BinOp (op, left, right)
