(*
   EqualityHelpers.ml - Generate structural equality source expressions.
*)
(* EqualityHelpers.ml - Generate structural equality source expressions. *)
open! AST
module M = StringOrder.Map

type eqHelperExprMode = ExpandCurrent | UseHelperCall

let makeSimpleMatchCase pattern body : AST.matchCase =
  { patterns = NonEmptyList.singleton pattern; guard = None; body }

(*
   Lambda lifting stores semantic identity and a capture-aware comparator
   after the operational code pointer. The comparator is fixed during
   specialization, so generic function equality requires no type dispatch.
*)
let rec buildEqHelperExpr aliases registry lookup sums mode typ left right =
  let resolved =
    ComparisonPlanning.canonicalEqualityType lookup
      (Types.resolveType aliases typ)
  in
  let call name args = AST.applyNamed name (NonEmptyList.fromList args) in
  let recurse typ left right =
    buildEqHelperExpr aliases registry lookup sums UseHelperCall typ left right
  in
  let chain = ComparisonPlanning.chainAndExpr in
  (match (mode, resolved) with
  | UseHelperCall, typ
    when ComparisonPlanning.needsEqHelperForResolvedType lookup typ ->
      call (ComparisonPlanning.eqHelperName typ) [ left; right ]
  | ExpandCurrent, TFunction _ ->
      let leftName = "__dark_eq_helper_fn_left"
      and rightName = "__dark_eq_helper_fn_right" in
      Let
        ( LPVariable leftName,
          left,
          Let
            ( LPVariable rightName,
              right,
              If
                ( BinOp
                    ( Eq,
                      TupleAccess (Var leftName, 1),
                      TupleAccess (Var rightName, 1) ),
                  IndirectApply
                    ( TupleAccess (Var leftName, 1),
                      NonEmptyList.fromList [ Var leftName; Var rightName ] ),
                  BoolLiteral false ) ) )
  | ExpandCurrent, TList inner ->
      let inner = Types.resolveType aliases inner in
      let leftHead = "__dark_eq_helper_list_left_head"
      and leftTail = "__dark_eq_helper_list_left_tail" in
      let rightHead = "__dark_eq_helper_list_right_head"
      and rightTail = "__dark_eq_helper_list_right_tail" in
      let heads = recurse inner (Var leftHead) (Var rightHead) in
      let tails =
        call
          (ComparisonPlanning.eqHelperName resolved)
          [ Var leftTail; Var rightTail ]
      in
      let empty =
        makeSimpleMatchCase (PTuple [ PList []; PList [] ]) (BoolLiteral true)
      in
      let nonEmpty =
        makeSimpleMatchCase
          (PTuple
             [
               PListCons ([ PVar leftHead ], PVar leftTail);
               PListCons ([ PVar rightHead ], PVar rightTail);
             ])
          (BinOp (And, heads, tails))
      in
      Match
        ( TupleLiteral [ left; right ],
          [ empty; nonEmpty; makeSimpleMatchCase PWildcard (BoolLiteral false) ]
        )
  | UseHelperCall, TList inner ->
      call
        (ComparisonPlanning.eqHelperName
           (TList (Types.resolveType aliases inner)))
        [ left; right ]
  | _, TString -> BinOp (Eq, left, right)
  | _, TInt -> call "Darklang.Stdlib.Int.__equals" [ left; right ]
  | ExpandCurrent, TDict (key, value) ->
      AST.applyNamedWithTypes "Darklang.Stdlib.Dict.__equals" [ key; value ]
        (NonEmptyList.fromList [ left; right ])
  | ExpandCurrent, TTuple args ->
      let leftName = "__dark_eq_helper_tuple_left"
      and rightName = "__dark_eq_helper_tuple_right" in
      let comparisons =
        List.mapi
          (fun index typ ->
            recurse typ
              (TupleAccess (Var leftName, index))
              (TupleAccess (Var rightName, index)))
          args
      in
      Let
        ( LPVariable leftName,
          left,
          Let (LPVariable rightName, right, chain comparisons) )
  | ExpandCurrent, TRecord (name, args) -> (
      match M.find_opt name registry with
      | None -> BinOp (Eq, left, right)
      | Some (info : Types.recordTypeInfo) ->
          let subst =
            Types.buildRecordFieldSubstitutionFromParams info.Types.typeParams
              args
          in
          let fields =
            List.mapi
              (fun index (name, typ) ->
                let typ =
                  match subst with
                  | Ok subst -> Types.applyTypeArguments subst typ
                  | Error _ -> typ
                in
                (index, name, Types.resolveType aliases typ))
              info.Types.fields
          in
          let leftName = "__dark_eq_helper_record_left"
          and rightName = "__dark_eq_helper_record_right" in
          let comparisons =
            List.map
              (fun (index, fieldName, typ) ->
                let field =
                  AST.resolvedRecordFieldReference name fieldName index
                in
                recurse typ
                  (RecordAccess (Var leftName, field))
                  (RecordAccess (Var rightName, field)))
              fields
          in
          Let
            ( LPVariable leftName,
              left,
              Let (LPVariable rightName, right, chain comparisons) ))
  | ExpandCurrent, TSum (name, args) ->
      if not (ComparisonPlanning.sumTypeHasPayload lookup name) then
        BinOp (Eq, left, right)
      else
        let variants =
          match M.find_opt name sums with
          | None -> []
          | Some (info : Types.sumTypeInfo) ->
              List.map
                (fun (variant : Types.sumVariantInfo) ->
                  let subst =
                    if List.length info.Types.typeParams = List.length args then
                      M.of_list (List.combine info.Types.typeParams args)
                    else M.empty
                  in
                  ( variant.Types.name,
                    variant.Types.tag,
                    List.map
                      (fun typ ->
                        Types.resolveType aliases (Types.applySubst subst typ))
                      variant.Types.fields ))
                info.Types.variants
        in
        let cases =
          List.map
            (fun (variant, tag, fields) ->
              let constructor fields =
                PResolvedConstructor (name, variant, tag, fields)
              in
              match fields with
              | [] ->
                  makeSimpleMatchCase
                    (PTuple [ constructor []; constructor [] ])
                    (BoolLiteral true)
              | _ :: _ ->
                  let leftFields =
                    List.mapi
                      (fun index _ ->
                        Printf.sprintf "__dark_eq_helper_left_field_%d_%d" tag
                          index)
                      fields
                  in
                  let rightFields =
                    List.mapi
                      (fun index _ ->
                        Printf.sprintf "__dark_eq_helper_right_field_%d_%d" tag
                          index)
                      fields
                  in
                  let comparisons =
                    List.map2
                      (fun (typ, left) right ->
                        recurse typ (Var left) (Var right))
                      (List.combine fields leftFields)
                      rightFields
                    |> chain
                  in
                  makeSimpleMatchCase
                    (PTuple
                       [
                         constructor
                           (List.map (fun name -> PVar name) leftFields);
                         constructor
                           (List.map (fun name -> PVar name) rightFields);
                       ])
                    comparisons)
            variants
        in
        let pairName = "__dark_eq_helper_sum_pair" in
        Let
          ( LPVariable pairName,
            TupleLiteral [ left; right ],
            Match
              ( Var pairName,
                cases @ [ makeSimpleMatchCase PWildcard (BoolLiteral false) ] )
          )
  | _, _ -> BinOp (Eq, left, right))
  [@warning "-4"]
