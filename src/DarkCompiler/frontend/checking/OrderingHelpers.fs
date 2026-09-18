// OrderingHelpers.fs - Generate structural ordering source expressions.

module OrderingHelpers

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open CheckedFreeVariables
open EqualityHelpers

let private comparisonResultLiteral (value: int64) : Expr = Int64Literal value

let private compareFromPredicates (lessExpr: Expr) (greaterExpr: Expr) : Expr =
    If (
        lessExpr,
        comparisonResultLiteral -1L,
        If (greaterExpr, comparisonResultLiteral 1L, comparisonResultLiteral 0L)
    )

let private chainCompareExprs (comparisons: Expr list) : Expr =
    let rec build index remaining =
        match remaining with
        | [] -> comparisonResultLiteral 0L
        | comparison :: rest ->
            let resultName = $"__dark_compare_component_{index}"
            Let (
                LPVariable resultName,
                comparison,
                If (
                    BinOp (Eq, Var resultName, comparisonResultLiteral 0L),
                    build (index + 1) rest,
                    Var resultName
                )
            )
    build 0 comparisons

let rec internal buildCompareHelperExpr
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (mode: EqHelperExprMode)
    (typ: Type)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let resolvedType = resolveType aliasReg typ
    let callHelper helperType left right =
        Call (
            compareHelperName (resolveType aliasReg helperType),
            NonEmptyList.fromList [left; right]
        )
    let compareNative left right =
        compareFromPredicates (BinOp (Lt, left, right)) (BinOp (Gt, left, right))
    let compareString left right =
        let resultName = "__dark_compare_string_result"
        Let (
            LPVariable resultName,
            Call ("Stdlib.Dict.__compareString", NonEmptyList.fromList [left; right]),
            compareFromPredicates
                (BinOp (Lt, Var resultName, Int32Literal 0))
                (BinOp (Gt, Var resultName, Int32Literal 0))
        )

    match mode, resolvedType with
    | UseHelperCall, helperType -> callHelper helperType leftExpr rightExpr

    | ExpandCurrent, TUnit -> comparisonResultLiteral 0L
    | ExpandCurrent, TBool ->
        If (
            BinOp (Eq, leftExpr, rightExpr),
            comparisonResultLiteral 0L,
            If (leftExpr, comparisonResultLiteral 1L, comparisonResultLiteral -1L)
        )
    | ExpandCurrent, TInt ->
        Call ("Stdlib.Int.__compare", NonEmptyList.fromList [leftExpr; rightExpr])
    | ExpandCurrent, TInt128 ->
        Call (
            "Stdlib.Int.__compare",
            NonEmptyList.fromList [
                Call ("__int128_to_int", NonEmptyList.singleton leftExpr)
                Call ("__int128_to_int", NonEmptyList.singleton rightExpr)
            ]
        )
    | ExpandCurrent, TUInt128 ->
        Call (
            "Stdlib.Int.__compare",
            NonEmptyList.fromList [
                Call ("__uint128_to_int", NonEmptyList.singleton leftExpr)
                Call ("__uint128_to_int", NonEmptyList.singleton rightExpr)
            ]
        )
    | ExpandCurrent, TFloat64 ->
        let leftNan = BinOp (Neq, leftExpr, leftExpr)
        let rightNan = BinOp (Neq, rightExpr, rightExpr)
        If (
            leftNan,
            If (rightNan, comparisonResultLiteral 0L, comparisonResultLiteral -1L),
            If (rightNan, comparisonResultLiteral 1L, compareNative leftExpr rightExpr)
        )
    | ExpandCurrent, (TInt8 | TInt16 | TInt32 | TInt64
                     | TUInt8 | TUInt16 | TUInt32 | TUInt64) ->
        compareNative leftExpr rightExpr
    | ExpandCurrent, (TString | TChar) -> compareString leftExpr rightExpr
    | ExpandCurrent, TDateTime ->
        compareFromPredicates
            (Call ("Stdlib.DateTime.lessThan", NonEmptyList.fromList [leftExpr; rightExpr]))
            (Call ("Stdlib.DateTime.greaterThan", NonEmptyList.fromList [leftExpr; rightExpr]))

    | ExpandCurrent, TList elemType ->
        let leftHead = "__dark_compare_list_left_head"
        let leftTail = "__dark_compare_list_left_tail"
        let rightHead = "__dark_compare_list_right_head"
        let rightTail = "__dark_compare_list_right_tail"
        let headComparison = callHelper elemType (Var leftHead) (Var rightHead)
        let tailComparison = callHelper resolvedType (Var leftTail) (Var rightTail)
        let nonEmptyBody =
            let headResult = "__dark_compare_list_head_result"
            Let (
                LPVariable headResult,
                headComparison,
                If (
                    BinOp (Eq, Var headResult, comparisonResultLiteral 0L),
                    tailComparison,
                    Var headResult
                )
            )
        Match (
            TupleLiteral [leftExpr; rightExpr],
            [ makeSimpleMatchCase (PTuple [PList []; PList []]) (comparisonResultLiteral 0L)
              makeSimpleMatchCase (PTuple [PList []; PWildcard]) (comparisonResultLiteral -1L)
              makeSimpleMatchCase (PTuple [PWildcard; PList []]) (comparisonResultLiteral 1L)
              makeSimpleMatchCase
                (PTuple [
                    PListCons ([PVar leftHead], PVar leftTail)
                    PListCons ([PVar rightHead], PVar rightTail)
                ])
                nonEmptyBody ]
        )

    | ExpandCurrent, TDict (keyType, valueType) ->
        let entryType =
            TTuple [resolveType aliasReg keyType; resolveType aliasReg valueType]
        let listType = TList entryType
        let leftEntries =
            TypeApp ("Stdlib.Dict.toList", [keyType; valueType], NonEmptyList.singleton leftExpr)
        let rightEntries =
            TypeApp ("Stdlib.Dict.toList", [keyType; valueType], NonEmptyList.singleton rightExpr)
        callHelper listType leftEntries rightEntries

    | ExpandCurrent, TTuple elemTypes ->
        let leftName = "__dark_compare_tuple_left"
        let rightName = "__dark_compare_tuple_right"
        let comparisons =
            elemTypes
            |> List.mapi (fun index elemType ->
                callHelper elemType (TupleAccess (Var leftName, index)) (TupleAccess (Var rightName, index)))
        Let (
            LPVariable leftName,
            leftExpr,
            Let (LPVariable rightName, rightExpr, chainCompareExprs comparisons)
        )

    | ExpandCurrent, TRecord (recordTypeName, typeArgs) ->
        match Map.tryFind recordTypeName typeReg with
        | None -> RuntimeError $"Canonical sorting is not supported for type {typeToString resolvedType}"
        | Some recordInfo ->
            let concreteFields =
                match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                | Ok subst ->
                    recordInfo.Fields
                    |> List.map (fun (name, fieldType) ->
                        (name, resolveType aliasReg (applyTypeArguments subst fieldType)))
                | Error _ ->
                    recordInfo.Fields
                    |> List.map (fun (name, fieldType) -> (name, resolveType aliasReg fieldType))
            let orderedFields =
                concreteFields
                |> List.mapi (fun index (name, fieldType) -> (name, index, fieldType))
                |> List.sortBy (fun (name, _, _) -> name)
            let leftName = "__dark_compare_record_left"
            let rightName = "__dark_compare_record_right"
            let comparisons =
                orderedFields
                |> List.map (fun (_, index, fieldType) ->
                    callHelper fieldType (TupleAccess (Var leftName, index)) (TupleAccess (Var rightName, index)))
            Let (
                LPVariable leftName,
                leftExpr,
                Let (LPVariable rightName, rightExpr, chainCompareExprs comparisons)
            )

    | ExpandCurrent, TSum (sumTypeName, sumTypeArgs) ->
        let variants =
            indexedSumTypeReg
            |> Map.tryFind sumTypeName
            |> Option.map (fun info ->
                info.Variants
                |> List.map (fun variant ->
                    let concretePayload =
                        match variant.Payload with
                        | Some payload when List.length info.TypeParams = List.length sumTypeArgs ->
                            Some (resolveType aliasReg (applySubst (List.zip info.TypeParams sumTypeArgs |> Map.ofList) payload))
                        | Some payload -> Some (resolveType aliasReg payload)
                        | None -> None
                    (variant.Name, $"{sumTypeName}.{variant.Name}", concretePayload))
                |> List.sortBy (fun (caseName, _, _) -> caseName))
            |> Option.defaultValue []
        let cases =
            variants
            |> List.collect (fun (leftCaseName, leftVariant, leftPayload) ->
                variants
                |> List.map (fun (rightCaseName, rightVariant, rightPayload) ->
                    let order = System.String.CompareOrdinal(leftCaseName, rightCaseName)
                    match leftPayload, rightPayload, order with
                    | None, None, 0 ->
                        makeSimpleMatchCase
                            (PTuple [PConstructor (leftVariant, None); PConstructor (rightVariant, None)])
                            (comparisonResultLiteral 0L)
                    | Some payloadType, Some _, 0 ->
                        let leftPayloadName = "__dark_compare_sum_left_payload"
                        let rightPayloadName = "__dark_compare_sum_right_payload"
                        makeSimpleMatchCase
                            (PTuple [
                                PConstructor (leftVariant, Some (PVar leftPayloadName))
                                PConstructor (rightVariant, Some (PVar rightPayloadName))
                            ])
                            (callHelper payloadType (Var leftPayloadName) (Var rightPayloadName))
                    | _ ->
                        let leftPattern =
                            match leftPayload with
                            | None -> PConstructor (leftVariant, None)
                            | Some _ -> PConstructor (leftVariant, Some PWildcard)
                        let rightPattern =
                            match rightPayload with
                            | None -> PConstructor (rightVariant, None)
                            | Some _ -> PConstructor (rightVariant, Some PWildcard)
                        makeSimpleMatchCase
                            (PTuple [leftPattern; rightPattern])
                            (comparisonResultLiteral (if order < 0 then -1L else 1L))))
        Match (TupleLiteral [leftExpr; rightExpr], cases)

    | ExpandCurrent, _ ->
        RuntimeError $"Canonical sorting is not supported for type {typeToString resolvedType}"
