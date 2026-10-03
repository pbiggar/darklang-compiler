(*
   OrderingHelpers.fs - Generate structural ordering source expressions.
*)
(* OrderingHelpers.ml - Generate structural ordering source expressions. *)
open! AST
module M = StringOrder.Map
let comparisonResultLiteral value = Int64Literal value
let compareFromPredicates less greater = If (less, comparisonResultLiteral (-1L), If (greater, comparisonResultLiteral 1L, comparisonResultLiteral 0L))
let chainCompareExprs comparisons =
 let rec build index = function [] -> comparisonResultLiteral 0L | comparison :: rest ->
   let name = "__dark_compare_component_" ^ string_of_int index in
   Let (LPVariable name, comparison, If (BinOp (Eq, Var name, comparisonResultLiteral 0L), build (index + 1) rest, Var name)) in
 build 0 comparisons
let buildCompareHelperExpr aliases registry _lookup sums mode typ left right =
 let resolved = Types.resolveType aliases typ in
 let call name args = AST.applyNamed name (NonEmptyList.fromList args) in
 let callHelper typ left right = call (ComparisonPlanning.compareHelperName (Types.resolveType aliases typ)) [left; right] in
 let native left right = compareFromPredicates (BinOp (Lt, left, right)) (BinOp (Gt, left, right)) in
 let string left right =
   let name = "__dark_compare_string_result" in
   Let (LPVariable name, call "Darklang.Stdlib.Dict.__compareString" [left; right],
     compareFromPredicates (BinOp (Lt, Var name, Int32Literal 0l)) (BinOp (Gt, Var name, Int32Literal 0l))) in
 let case = EqualityHelpers.makeSimpleMatchCase in
 (match mode, resolved with
 | EqualityHelpers.UseHelperCall, typ -> callHelper typ left right
 | EqualityHelpers.ExpandCurrent, TUnit -> comparisonResultLiteral 0L
 | EqualityHelpers.ExpandCurrent, TBool -> If (BinOp (Eq, left, right), comparisonResultLiteral 0L, If (left, comparisonResultLiteral 1L, comparisonResultLiteral (-1L)))
 | EqualityHelpers.ExpandCurrent, TInt -> call "Darklang.Stdlib.Int.__compare" [left; right]
 | EqualityHelpers.ExpandCurrent, (TInt128 | TUInt128) ->
   let name = if resolved = TInt128 then "__int128_to_int" else "__uint128_to_int" in
   call "Darklang.Stdlib.Int.__compare" [AST.applyNamed name (NonEmptyList.singleton left); AST.applyNamed name (NonEmptyList.singleton right)]
 | EqualityHelpers.ExpandCurrent, TFloat64 ->
   let leftNan = BinOp (Neq, left, left) and rightNan = BinOp (Neq, right, right) in
   If (leftNan, If (rightNan, comparisonResultLiteral 0L, comparisonResultLiteral (-1L)), If (rightNan, comparisonResultLiteral 1L, native left right))
 | EqualityHelpers.ExpandCurrent, (TInt8 | TInt16 | TInt32 | TInt64 | TUInt8 | TUInt16 | TUInt32 | TUInt64) -> native left right
 | EqualityHelpers.ExpandCurrent, (TString | TChar) -> string left right
 | EqualityHelpers.ExpandCurrent, TDateTime -> compareFromPredicates (call "Darklang.Stdlib.DateTime.lessThan" [left; right]) (call "Darklang.Stdlib.DateTime.greaterThan" [left; right])
 | EqualityHelpers.ExpandCurrent, TList inner ->
   let leftHead = "__dark_compare_list_left_head" and leftTail = "__dark_compare_list_left_tail" in
   let rightHead = "__dark_compare_list_right_head" and rightTail = "__dark_compare_list_right_tail" in
   let head = callHelper inner (Var leftHead) (Var rightHead) in
   let tail = callHelper resolved (Var leftTail) (Var rightTail) in
   let resultName = "__dark_compare_list_head_result" in
   let nonEmptyBody = Let (LPVariable resultName, head, If (BinOp (Eq, Var resultName, comparisonResultLiteral 0L), tail, Var resultName)) in
   Match (TupleLiteral [left; right], [case (PTuple [PList []; PList []]) (comparisonResultLiteral 0L);
     case (PTuple [PList []; PWildcard]) (comparisonResultLiteral (-1L)); case (PTuple [PWildcard; PList []]) (comparisonResultLiteral 1L);
     case (PTuple [PListCons ([PVar leftHead], PVar leftTail); PListCons ([PVar rightHead], PVar rightTail)]) nonEmptyBody])
 | EqualityHelpers.ExpandCurrent, TDict (key, value) ->
   let listType = TList (TTuple [Types.resolveType aliases key; Types.resolveType aliases value]) in
   let entries expr = AST.applyNamedWithTypes "Darklang.Stdlib.Dict.toList" [key; value] (NonEmptyList.singleton expr) in
   callHelper listType (entries left) (entries right)
 | EqualityHelpers.ExpandCurrent, TTuple args ->
   let leftName = "__dark_compare_tuple_left" and rightName = "__dark_compare_tuple_right" in
   let comparisons = List.mapi (fun index typ -> callHelper typ (TupleAccess (Var leftName, index)) (TupleAccess (Var rightName, index))) args in
   Let (LPVariable leftName, left, Let (LPVariable rightName, right, chainCompareExprs comparisons))
 | EqualityHelpers.ExpandCurrent, TRecord (name, args) -> (match M.find_opt name registry with
   | None -> RuntimeError ("Canonical sorting is not supported for type " ^ CheckingDiagnostics.typeToString resolved)
   | Some (info : Types.recordTypeInfo) ->
     let subst = Types.buildRecordFieldSubstitutionFromParams info.Types.typeParams args in
     let fields = List.mapi (fun index (name, typ) -> let typ = match subst with Ok subst -> Types.applyTypeArguments subst typ | Error _ -> typ in name, index, Types.resolveType aliases typ) info.Types.fields
       |> List.stable_sort (fun (name, _, _) (other, _, _) -> StringOrder.compare name other) in
     let leftName = "__dark_compare_record_left" and rightName = "__dark_compare_record_right" in
     let comparisons = List.map (fun (_, index, typ) -> callHelper typ (TupleAccess (Var leftName, index)) (TupleAccess (Var rightName, index))) fields in
     Let (LPVariable leftName, left, Let (LPVariable rightName, right, chainCompareExprs comparisons)))
 | EqualityHelpers.ExpandCurrent, TSum (name, args) ->
   let variants = match M.find_opt name sums with None -> [] | Some (info : Types.sumTypeInfo) ->
     List.map (fun (variant : Types.sumVariantInfo) ->
       let subst = if List.length info.Types.typeParams = List.length args then M.of_list (List.combine info.Types.typeParams args) else M.empty in
       variant.Types.name, variant.Types.tag, List.map (fun typ -> Types.resolveType aliases (Types.applySubst subst typ)) variant.Types.fields) info.Types.variants
       |> List.stable_sort (fun (name, _, _) (other, _, _) -> StringOrder.compare name other) in
   let cases = List.concat_map (fun (leftCase, leftTag, leftFields) -> List.map (fun (rightCase, rightTag, rightFields) ->
     let order = StringOrder.compare leftCase rightCase in
     let leftConstructor fields = PResolvedConstructor (name, leftCase, leftTag, fields) and rightConstructor fields = PResolvedConstructor (name, rightCase, rightTag, fields) in
     if leftFields = [] && rightFields = [] && order = 0 then case (PTuple [leftConstructor []; rightConstructor []]) (comparisonResultLiteral 0L)
     else if order = 0 then
       let leftNames = List.mapi (fun index _ -> "__dark_compare_sum_left_field_" ^ string_of_int index) leftFields in
       let rightNames = List.mapi (fun index _ -> "__dark_compare_sum_right_field_" ^ string_of_int index) leftFields in
       let comparisons = List.map2 (fun (typ, left) right -> callHelper typ (Var left) (Var right)) (List.combine leftFields leftNames) rightNames in
       case (PTuple [leftConstructor (List.map (fun name -> PVar name) leftNames); rightConstructor (List.map (fun name -> PVar name) rightNames)]) (chainCompareExprs comparisons)
     else case (PTuple [leftConstructor (List.map (fun _ -> PWildcard) leftFields); rightConstructor (List.map (fun _ -> PWildcard) rightFields)]) (comparisonResultLiteral (if order < 0 then -1L else 1L))) variants) variants in
   Match (TupleLiteral [left; right], cases)
 | EqualityHelpers.ExpandCurrent, _ -> RuntimeError ("Canonical sorting is not supported for type " ^ CheckingDiagnostics.typeToString resolved)) [@warning "-4"]
