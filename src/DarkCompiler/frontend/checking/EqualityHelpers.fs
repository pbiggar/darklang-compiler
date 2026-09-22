// EqualityHelpers.fs - Generate structural equality source expressions.

module EqualityHelpers

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open CheckedFreeVariables

type internal EqHelperExprMode =
    | ExpandCurrent
    | UseHelperCall

let internal makeSimpleMatchCase (pattern: Pattern) (body: Expr) : MatchCase =
    { Patterns = NonEmptyList.singleton pattern
      Guard = None
      Body = body }

let rec internal buildEqHelperExpr
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (mode: EqHelperExprMode)
    (typ: SemanticType)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let resolvedType = typ |> resolveType aliasReg |> canonicalEqualityType variantLookup

    match (mode, resolvedType) with
    | UseHelperCall, helperType when needsEqHelperForResolvedType variantLookup helperType ->
        applyNamed (eqHelperName helperType) (NonEmptyList.fromList [leftExpr; rightExpr])

    | ExpandCurrent, TFunction _ ->
        // Lambda lifting stores semantic identity and a capture-aware comparator
        // after the operational code pointer. The comparator is fixed during
        // specialization, so generic function equality requires no type dispatch.
        let leftVar = "__dark_eq_helper_fn_left"
        let rightVar = "__dark_eq_helper_fn_right"
        Let (
            LPVariable leftVar,
            leftExpr,
            Let (
                LPVariable rightVar,
                rightExpr,
                If (
                    BinOp (
                        Eq,
                        TupleAccess (Var leftVar, 1),
                        TupleAccess (Var rightVar, 1)
                    ),
                    IndirectApply (
                        TupleAccess (Var leftVar, 1),
                        NonEmptyList.fromList [Var leftVar; Var rightVar]
                    ),
                    BoolLiteral false
                )
            )
        )

    | ExpandCurrent, TList elemType ->
        let resolvedElemType = resolveType aliasReg elemType
        let leftHead = "__dark_eq_helper_list_left_head"
        let leftTail = "__dark_eq_helper_list_left_tail"
        let rightHead = "__dark_eq_helper_list_right_head"
        let rightTail = "__dark_eq_helper_list_right_tail"
        let headsEqual =
            buildEqHelperExpr
                aliasReg
                typeReg
                variantLookup
                indexedSumTypeReg
                UseHelperCall
                resolvedElemType
                (Var leftHead)
                (Var rightHead)
        let tailsEqual =
            applyNamed
                (eqHelperName resolvedType)
                (NonEmptyList.fromList [Var leftTail; Var rightTail])
        let bothEmpty =
            makeSimpleMatchCase (PTuple [PList []; PList []]) (BoolLiteral true)
        let bothNonEmpty =
            makeSimpleMatchCase
                (PTuple [
                    PListCons ([PVar leftHead], PVar leftTail)
                    PListCons ([PVar rightHead], PVar rightTail)
                ])
                (BinOp (And, headsEqual, tailsEqual))
        Match (TupleLiteral [leftExpr; rightExpr], [bothEmpty; bothNonEmpty; makeSimpleMatchCase PWildcard (BoolLiteral false)])

    | UseHelperCall, TList elemType ->
        applyNamed
            (eqHelperName (TList (resolveType aliasReg elemType)))
            (NonEmptyList.fromList [leftExpr; rightExpr])

    | ExpandCurrent, TString ->
        BinOp (Eq, leftExpr, rightExpr)

    | _, TString ->
        BinOp (Eq, leftExpr, rightExpr)

    | _, TInt ->
        applyNamed "Darklang.Stdlib.Int.__equals" (NonEmptyList.fromList [leftExpr; rightExpr])

    | ExpandCurrent, TDict (keyType, valueType) ->
        let entryType =
            TTuple [resolveType aliasReg keyType; resolveType aliasReg valueType]
        let listType = TList entryType
        let leftEntries =
            applyNamedWithTypes "Darklang.Stdlib.Dict.toList" [keyType; valueType] (NonEmptyList.singleton leftExpr)
        let rightEntries =
            applyNamedWithTypes "Darklang.Stdlib.Dict.toList" [keyType; valueType] (NonEmptyList.singleton rightExpr)
        buildEqHelperExpr
            aliasReg
            typeReg
            variantLookup
            indexedSumTypeReg
            UseHelperCall
            listType
            leftEntries
            rightEntries

    | ExpandCurrent, TTuple elemTypes ->
        let leftTupleVar = "__dark_eq_helper_tuple_left"
        let rightTupleVar = "__dark_eq_helper_tuple_right"
        let elementComparisons =
            elemTypes
            |> List.mapi (fun index elemType ->
                buildEqHelperExpr
                    aliasReg
                    typeReg
                    variantLookup
                    indexedSumTypeReg
                    UseHelperCall
                    elemType
                    (TupleAccess (Var leftTupleVar, index))
                    (TupleAccess (Var rightTupleVar, index)))
        Let (LPVariable leftTupleVar, leftExpr, Let (LPVariable rightTupleVar, rightExpr, chainAndExpr elementComparisons))

    | ExpandCurrent, TRecord (recordTypeName, typeArgs) ->
        match Map.tryFind recordTypeName typeReg with
        | None ->
            BinOp (Eq, leftExpr, rightExpr)
        | Some recordInfo ->
            let fields = recordInfo.Fields
            let concreteFields =
                match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                | Ok subst ->
                    fields |> List.mapi (fun index (name, fieldType) -> (index, name, resolveType aliasReg (applyTypeArguments subst fieldType)))
                | Error _ ->
                    fields |> List.mapi (fun index (name, fieldType) -> (index, name, resolveType aliasReg fieldType))

            let leftRecordVar = "__dark_eq_helper_record_left"
            let rightRecordVar = "__dark_eq_helper_record_right"
            let fieldComparisons =
                concreteFields
                |> List.map (fun (fieldIndex, fieldName, fieldType) ->
                    buildEqHelperExpr
                        aliasReg
                        typeReg
                        variantLookup
                        indexedSumTypeReg
                        UseHelperCall
                        fieldType
                        (RecordAccess (Var leftRecordVar, resolvedRecordFieldReference recordTypeName fieldName fieldIndex))
                        (RecordAccess (Var rightRecordVar, resolvedRecordFieldReference recordTypeName fieldName fieldIndex)))
            Let (LPVariable leftRecordVar, leftExpr, Let (LPVariable rightRecordVar, rightExpr, chainAndExpr fieldComparisons))

    | ExpandCurrent, TSum (sumTypeName, sumTypeArgs) ->
        if not (sumTypeHasPayload variantLookup sumTypeName) then
            BinOp (Eq, leftExpr, rightExpr)
        else
            let variantsForType =
                indexedSumTypeReg
                |> Map.tryFind sumTypeName
                |> Option.map (fun info ->
                    info.Variants
                    |> List.map (fun variant ->
                        let concreteFields =
                            let subst =
                                if List.length info.TypeParams = List.length sumTypeArgs then
                                    List.zip info.TypeParams sumTypeArgs |> Map.ofList
                                else
                                    Map.empty
                            variant.Fields
                            |> List.map (applySubst subst >> resolveType aliasReg)
                        (variant.Name, variant.Tag, concreteFields)))
                |> Option.defaultValue []

            let variantCases =
                variantsForType
                |> List.map (fun (variantName, tag, fieldTypes) ->
                    match fieldTypes with
                    | [] ->
                        let constructor fields =
                            PResolvedConstructor (sumTypeName, variantName, tag, fields)
                        let pairPattern =
                            PTuple [constructor []; constructor []]
                        makeSimpleMatchCase pairPattern (BoolLiteral true)
                    | _ ->
                        let leftFields = fieldTypes |> List.mapi (fun index _ -> $"__dark_eq_helper_left_field_{tag}_{index}")
                        let rightFields = fieldTypes |> List.mapi (fun index _ -> $"__dark_eq_helper_right_field_{tag}_{index}")
                        let fieldEqExpr =
                            List.zip3 fieldTypes leftFields rightFields
                            |> List.map (fun (fieldType, leftField, rightField) ->
                                buildEqHelperExpr
                                    aliasReg
                                    typeReg
                                    variantLookup
                                    indexedSumTypeReg
                                    UseHelperCall
                                    fieldType
                                    (Var leftField)
                                    (Var rightField))
                            |> chainAndExpr
                        let constructor fields =
                            PResolvedConstructor (sumTypeName, variantName, tag, fields)
                        let pairPattern =
                            PTuple [
                                constructor (List.map PVar leftFields)
                                constructor (List.map PVar rightFields)
                            ]
                        makeSimpleMatchCase pairPattern fieldEqExpr)

            let defaultCase = makeSimpleMatchCase PWildcard (BoolLiteral false)
            let sumPairVar = "__dark_eq_helper_sum_pair"
            Let (LPVariable sumPairVar, TupleLiteral [leftExpr; rightExpr], Match (Var sumPairVar, variantCases @ [defaultCase]))

    | _, _ ->
        BinOp (Eq, leftExpr, rightExpr)
