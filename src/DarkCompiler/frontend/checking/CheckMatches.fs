// CheckMatches.fs - Check Match expressions while preserving source diagnostics and order.

module CheckMatches

open AST
open CheckingDiagnostics
open CheckingTypes
open CheckedFreeVariables
open TypeUnification
open CheckExpressionSupport

let internal check (checkExpr: ExpressionChecker) (sumTypeNames: Set<string>) (indexedSumTypeReg: IndexedSumTypeRegistry) (env: TypeEnv) (typeReg: IndexedTypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (warningSettings: WarningSettings) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) (expectedType: Type option) (scrutinee: Expr) (cases: MatchCase list) : Result<Type * Expr, TypeError> =
    let scrutineeExpectedType =
        match scrutinee with
        | ListLiteral [] -> Some (TList (TVar emptyListElementVar))
        | _ -> None

    // Type check the scrutinee first
    checkExpr scrutinee env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg scrutineeExpectedType
    |> Result.bind (fun (scrutineeType, scrutinee') ->
        // Extract bindings from a pattern based on scrutinee type
        let rec extractPatternBindings
            (pattern: Pattern)
            (patternType: Type)
            (allowNoMatchForKnownListLengthMismatch: bool)
            : Result<(string * Type) list, TypeError> =
            let ensureLiteralType
                (expectedType: Type)
                : Result<(string * Type) list, TypeError> =
                let expectedPatternTypeTextOverride = None
                let resolvedPatternType = resolveType aliasReg patternType
                match resolvedPatternType with
                | t when isRuntimeErrorType t ->
                    // Runtime error scrutinees are bottom-like: allow typechecking to proceed
                    // so evaluation order preserves the runtime failure at execution time.
                    Ok []
                | t when t = expectedType ->
                    Ok []
                | TVar _ ->
                    // Leave unresolved pattern literals flexible until concrete type information arrives.
                    // This is important for patterns like `match [] with | [1L] -> ...`.
                    Ok []
                | _ ->
                    let message =
                        formatPatternMismatchError
                            scrutinee'
                            resolvedPatternType
                            expectedType
                            expectedPatternTypeTextOverride
                    Error (GenericError message)

            let ensureStringOrCharPatternType () : Result<(string * Type) list, TypeError> =
                let resolvedPatternType = resolveType aliasReg patternType
                match resolvedPatternType with
                | t when isRuntimeErrorType t ->
                    // See ensureLiteralType: preserve runtime-error propagation by not
                    // rejecting pattern type checks on known failing scrutinees.
                    Ok []
                | TString
                | TChar
                | TVar _ ->
                    Ok []
                | _ ->
                    let message =
                        formatPatternMismatchError scrutinee' resolvedPatternType TString None
                    Error (GenericError message)

            match pattern with
            | POr alternatives ->
                alternatives
                |> NonEmptyList.toList
                |> ResultList.traverse (fun alternative ->
                    extractPatternBindings alternative patternType allowNoMatchForKnownListLengthMismatch)
                |> Result.map NonEmptyList.fromList
                |> Result.bind (fun alternativeBindings ->
                    let first = NonEmptyList.head alternativeBindings
                    let expectedNames = first |> List.map fst |> Set.ofList
                    let bindingsAgree =
                        alternativeBindings
                        |> NonEmptyList.toList
                        |> List.forall (fun bindings ->
                            bindings |> List.map fst |> Set.ofList |> (=) expectedNames)
                    if bindingsAgree then Ok first
                    else Error (GenericError "Every branch of an or-pattern must bind the same names"))
            | PUnit -> ensureLiteralType TUnit
            | PWildcard -> Ok []
            | PInt64 _ -> ensureLiteralType TInt64
            | PBigInt _ -> ensureLiteralType TInt
            | PInt128Literal _ -> ensureLiteralType TInt128
            | PInt8Literal _ -> ensureLiteralType TInt8
            | PInt16Literal _ -> ensureLiteralType TInt16
            | PInt32Literal _ -> ensureLiteralType TInt32
            | PUInt8Literal _ -> ensureLiteralType TUInt8
            | PUInt16Literal _ -> ensureLiteralType TUInt16
            | PUInt32Literal _ -> ensureLiteralType TUInt32
            | PUInt64Literal _ -> ensureLiteralType TUInt64
            | PUInt128Literal _ -> ensureLiteralType TUInt128
            | PBool _ -> ensureLiteralType TBool
            | PString _ -> ensureStringOrCharPatternType ()
            | PChar _ -> ensureLiteralType TChar
            | PFloat _ -> ensureLiteralType TFloat64
            | PVar name -> Ok [(name, patternType)]
            | PConstructor (variantName, payloadPattern) ->
                match Map.tryFind variantName variantLookup with
                | None -> Error (GenericError $"Unknown variant in pattern: {variantName}")
                | Some (typeName, typeParams, _, payloadType) ->
                    // Get type arguments from scrutinee type to substitute into payload type
                    let typeArgs =
                        match patternType with
                        | TSum (_, args) -> args
                        | _ -> []
                    // Build substitution from type params to type args
                    let subst =
                        if List.length typeParams = List.length typeArgs then
                            List.zip typeParams typeArgs |> Map.ofList
                        else
                            Map.empty
                    match payloadPattern, payloadType with
                    | None, None -> Ok []
                    | None, Some _ ->
                        // Pattern omitted payload for a payload-carrying variant.
                        // Treat as a non-binding pattern; match lowering will make it non-matching.
                        Ok []
                    | Some innerPattern, Some pType ->
                        // Apply substitution to get concrete payload type
                        let concretePayloadType =
                            pType
                            |> applySubst subst
                            |> canonicalizeBareSumTypeRefsWithNames sumTypeNames
                            |> function
                                | TEnumFields fieldTypes -> TTuple fieldTypes
                                | other -> other

                        extractPatternBindings innerPattern concretePayloadType allowNoMatchForKnownListLengthMismatch
                    | Some _, None ->
                        // Pattern supplied payload for a nullary variant.
                        // Treat as a non-binding pattern; match lowering will make it non-matching.
                        Ok []
            | PTuple patterns ->
                let rec containsVariableBinding (innerPattern: Pattern) : bool =
                    match innerPattern with
                    | PVar _ -> true
                    | PConstructor (_, Some payloadPattern) -> containsVariableBinding payloadPattern
                    | PTuple nestedPatterns
                    | PList nestedPatterns ->
                        nestedPatterns |> List.exists containsVariableBinding
                    | PListCons (headPatterns, tailPattern) ->
                        List.exists containsVariableBinding headPatterns
                        || containsVariableBinding tailPattern
                    | _ -> false

                let collectTupleBindingsWithTypes (elementTypes: Type list) : Result<(string * Type) list, TypeError> =
                    List.zip patterns elementTypes
                    |> List.map (fun (p, t) ->
                        extractPatternBindings p t allowNoMatchForKnownListLengthMismatch)
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])

                // Resolve type alias before matching (e.g., Pair<Int64> -> (Int64, Int64))
                let resolvedPatternType = resolveType aliasReg patternType
                match resolvedPatternType with
                | TTuple elementTypes when List.length patterns = List.length elementTypes ->
                    collectTupleBindingsWithTypes elementTypes
                | TEnumFields elementTypes when List.length patterns = List.length elementTypes ->
                    collectTupleBindingsWithTypes elementTypes
                | TVar tupleTypeVar ->
                    let unresolvedElementTypes =
                        patterns
                        |> List.mapi (fun idx _ -> TVar $"__tuple_elem_{tupleTypeVar}_{idx}")
                    collectTupleBindingsWithTypes unresolvedElementTypes
                | TTuple _ ->
                    // Tuple arity mismatch in pattern should be treated as a non-match.
                    // Match lowering emits a false condition for this pattern shape.
                    Ok []
                | TEnumFields _ ->
                    // As above, a field-count mismatch is a non-matching enum case.
                    Ok []
                | _ ->
                    if isRuntimeErrorType resolvedPatternType then
                        // Preserve runtime error propagation for known failing scrutinees.
                        Ok []
                    elif patterns |> List.exists containsVariableBinding then
                        // Keep non-binding behavior for tuple patterns that would otherwise
                        // introduce guard/body variables on an incompatible scrutinee type.
                        Ok []
                    else
                        let valueText =
                            match formatPatternMismatchValue scrutinee' with
                            | Some text -> text
                            | None -> "<unknown>"
                        let message =
                            $"Cannot match {typeToString resolvedPatternType} value {valueText} with a Tuple pattern"
                        Error (GenericError message)
            | PList patterns ->
                let resolvedPatternType = resolveType aliasReg patternType
                match resolvedPatternType with
                | TList elemType ->
                    let hasDefiniteLiteralTypeMismatch =
                        let resolvedElemType = resolveType aliasReg elemType
                        let isKnownMismatch expectedType =
                            match resolvedElemType with
                            | TVar _ -> false
                            | _ -> resolvedElemType <> expectedType
                        let isKnownStringPatternMismatch () =
                            match resolvedElemType with
                            | TVar _
                            | TString
                            | TChar -> false
                            | _ -> true
                        patterns
                        |> List.exists (fun pattern ->
                            match pattern with
                            | PUnit -> isKnownMismatch TUnit
                            | PInt64 _ -> isKnownMismatch TInt64
                            | PBigInt _ -> isKnownMismatch TInt
                            | PInt128Literal _ -> isKnownMismatch TInt128
                            | PInt8Literal _ -> isKnownMismatch TInt8
                            | PInt16Literal _ -> isKnownMismatch TInt16
                            | PInt32Literal _ -> isKnownMismatch TInt32
                            | PUInt8Literal _ -> isKnownMismatch TUInt8
                            | PUInt16Literal _ -> isKnownMismatch TUInt16
                            | PUInt32Literal _ -> isKnownMismatch TUInt32
                            | PUInt64Literal _ -> isKnownMismatch TUInt64
                            | PUInt128Literal _ -> isKnownMismatch TUInt128
                            | PBool _ -> isKnownMismatch TBool
                            | PString _ -> isKnownStringPatternMismatch ()
                            | PChar _ -> isKnownMismatch TChar
                            | PFloat _ -> isKnownMismatch TFloat64
                            | _ -> false)
                    match scrutinee' with
                    | ListLiteral scrutineeElements
                        when allowNoMatchForKnownListLengthMismatch
                             && hasDefiniteLiteralTypeMismatch
                             && List.length scrutineeElements <> List.length patterns ->
                        let valueText = formatListLiteralForNoMatch scrutineeElements
                        Error (GenericError $"No match for {valueText}")
                    | _ ->
                        // Each element pattern binds variables of the list's element type
                        patterns
                        |> List.map (fun p ->
                            extractPatternBindings p elemType allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                | TVar patternTypeVar ->
                    let unresolvedElemType = TVar $"__list_elem_{patternTypeVar}"
                    patterns
                    |> List.map (fun p ->
                        extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                | TRuntimeError ->
                    let unresolvedElemType = TVar "__list_elem_runtime_error"
                    patterns
                    |> List.map (fun p ->
                        extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                    |> List.fold (fun acc res ->
                        match acc, res with
                        | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e) (Ok [])
                | _ ->
                    let valueText =
                        match formatPatternMismatchValue scrutinee' with
                        | Some text -> text
                        | None -> "<unknown>"
                    let message =
                        $"Cannot match {typeToString resolvedPatternType} value {valueText} with a List pattern"
                    Error (GenericError message)
            | PListCons (headPatterns, tailPattern) ->
                let resolvedPatternType = resolveType aliasReg patternType
                match resolvedPatternType with
                | TList elemType ->
                    // Head patterns bind to element type
                    let headBindings =
                        headPatterns
                        |> List.map (fun p ->
                            extractPatternBindings p elemType allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    // Tail pattern binds to List<elemType>
                    let tailBindings =
                        extractPatternBindings
                            tailPattern
                            (TList elemType)
                            allowNoMatchForKnownListLengthMismatch
                    match headBindings, tailBindings with
                    | Ok hb, Ok tb -> Ok (hb @ tb)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                | TVar patternTypeVar ->
                    let unresolvedElemType = TVar $"__list_elem_{patternTypeVar}"
                    let headBindings =
                        headPatterns
                        |> List.map (fun p ->
                            extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    let tailBindings =
                        extractPatternBindings
                            tailPattern
                            (TList unresolvedElemType)
                            allowNoMatchForKnownListLengthMismatch
                    match headBindings, tailBindings with
                    | Ok hb, Ok tb -> Ok (hb @ tb)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                | TRuntimeError ->
                    let unresolvedElemType = TVar "__list_elem_runtime_error"
                    let headBindings =
                        headPatterns
                        |> List.map (fun p ->
                            extractPatternBindings p unresolvedElemType allowNoMatchForKnownListLengthMismatch)
                        |> List.fold (fun acc res ->
                            match acc, res with
                            | Ok bindings, Ok newBindings -> Ok (bindings @ newBindings)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e) (Ok [])
                    let tailBindings =
                        extractPatternBindings
                            tailPattern
                            (TList unresolvedElemType)
                            allowNoMatchForKnownListLengthMismatch
                    match headBindings, tailBindings with
                    | Ok hb, Ok tb -> Ok (hb @ tb)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                | _ ->
                    let valueText =
                        match formatPatternMismatchValue scrutinee' with
                        | Some text -> text
                        | None -> "<unknown>"
                    let message =
                        $"Cannot match {typeToString resolvedPatternType} value {valueText} with a List pattern"
                    Error (GenericError message)

        let rec patternBindingNames (pattern: Pattern) : string list =
            match pattern with
            | PUnit
            | PWildcard
            | PInt64 _
            | PBigInt _
            | PInt128Literal _
            | PInt8Literal _
            | PInt16Literal _
            | PInt32Literal _
            | PUInt8Literal _
            | PUInt16Literal _
            | PUInt32Literal _
            | PUInt64Literal _
            | PUInt128Literal _
            | PBool _
            | PString _
            | PChar _
            | PFloat _ ->
                []
            | PVar name ->
                [name]
            | PConstructor (_, payloadOpt) ->
                match payloadOpt with
                | Some payloadPattern -> patternBindingNames payloadPattern
                | None -> []
            | PTuple patterns
            | PList patterns ->
                patterns |> List.collect patternBindingNames
            | PListCons (headPatterns, tailPattern) ->
                (headPatterns |> List.collect patternBindingNames) @ patternBindingNames tailPattern
            | POr alternatives ->
                alternatives |> NonEmptyList.head |> patternBindingNames

        let duplicatePatternBindings (pattern: Pattern) : string list =
            patternBindingNames pattern
            |> List.countBy id
            |> List.choose (fun (name, count) -> if count > 1 then Some name else None)
            |> List.sort

        let formatBindingSet (bindingSet: Set<string>) : string =
            bindingSet
            |> Set.toList
            |> List.sort
            |> String.concat ", "

        let validatePatternGroupBindings (patterns: NonEmptyList<Pattern>) : Result<unit, TypeError> =
            let allPatterns = NonEmptyList.toList patterns

            let rec loop (remaining: Pattern list) (expectedBindings: Set<string> option) : Result<unit, TypeError> =
                match remaining with
                | [] -> Ok ()
                | pattern :: rest ->
                    match validateBinders (MatchBinderPattern pattern) with
                    | Error message -> Error (GenericError message)
                    | Ok _ ->
                        let bindings =
                            collectPatternBindings pattern
                            |> Set.filter (fun name ->
                                name <> "" && not (name.StartsWith "_"))
                        match expectedBindings with
                        | None -> loop rest (Some bindings)
                        | Some expected when expected = bindings ->
                            loop rest expectedBindings
                        | Some expected ->
                            let expectedText = formatBindingSet expected
                            let actualText = formatBindingSet bindings
                            Error (
                                GenericError
                                    $"Pattern matches require all branches to provide the same variables - expected [{expectedText}], got [{actualText}]"
                            )

            loop allPatterns None

        let allowNoMatchForKnownListLengthMismatchInThisMatch = List.length cases = 1

        let rec patternAlwaysMatchesType (pattern: Pattern) (patternType: Type) : bool =
            let resolvedPatternType = resolveType aliasReg patternType
            match pattern, resolvedPatternType with
            | PWildcard, _
            | PVar _, _ ->
                true
            | PUnit, TUnit ->
                true
            | PTuple patterns, TTuple elementTypes when List.length patterns = List.length elementTypes ->
                List.zip patterns elementTypes
                |> List.forall (fun (innerPattern, innerType) ->
                    patternAlwaysMatchesType innerPattern innerType)
            | _ ->
                false

        let variantNamesMatch (leftName: string) (rightName: string) : bool =
            leftName = rightName
            || leftName.EndsWith($".{rightName}")
            || rightName.EndsWith($".{leftName}")

        let rec combinePatternMatchStatuses (statuses: bool option list) : bool option =
            if statuses |> List.exists (fun status -> status = Some false) then
                Some false
            elif statuses |> List.forall (fun status -> status = Some true) then
                Some true
            else
                None

        let rec patternDefinitelyMatchesExpr (pattern: Pattern) (valueExpr: Expr) : bool option =
            match pattern, valueExpr with
            | PWildcard, _
            | PVar _, _ ->
                Some true
            | PUnit, UnitLiteral ->
                Some true
            | PInt64 expected, Int64Literal actual ->
                Some (expected = actual)
            | PBigInt expected, BigIntLiteral actual ->
                Some (expected = actual)
            | PInt128Literal expected, Int128Literal actual ->
                Some (expected = actual)
            | PInt8Literal expected, Int8Literal actual ->
                Some (expected = actual)
            | PInt16Literal expected, Int16Literal actual ->
                Some (expected = actual)
            | PInt32Literal expected, Int32Literal actual ->
                Some (expected = actual)
            | PUInt8Literal expected, UInt8Literal actual ->
                Some (expected = actual)
            | PUInt16Literal expected, UInt16Literal actual ->
                Some (expected = actual)
            | PUInt32Literal expected, UInt32Literal actual ->
                Some (expected = actual)
            | PUInt64Literal expected, UInt64Literal actual ->
                Some (expected = actual)
            | PUInt128Literal expected, UInt128Literal actual ->
                Some (expected = actual)
            | PBool expected, BoolLiteral actual ->
                Some (expected = actual)
            | PString expected, StringLiteral actual ->
                Some (expected = actual)
            | PChar expected, CharLiteral actual ->
                Some (expected = actual)
            | PFloat expected, FloatLiteral actual ->
                Some (expected = actual)
            | PTuple patterns, TupleLiteral values ->
                if List.length patterns <> List.length values then
                    Some false
                else
                    List.zip patterns values
                    |> List.map (fun (innerPattern, innerValue) ->
                        patternDefinitelyMatchesExpr innerPattern innerValue)
                    |> combinePatternMatchStatuses
            | PList patterns, ListLiteral values ->
                if List.length patterns <> List.length values then
                    Some false
                else
                    List.zip patterns values
                    |> List.map (fun (innerPattern, innerValue) ->
                        patternDefinitelyMatchesExpr innerPattern innerValue)
                    |> combinePatternMatchStatuses
            | PListCons (headPatterns, tailPattern), ListLiteral values ->
                if List.length values < List.length headPatterns then
                    Some false
                else
                    let headValues = values |> List.take (List.length headPatterns)
                    let tailValues = values |> List.skip (List.length headPatterns)
                    let headStatuses =
                        List.zip headPatterns headValues
                        |> List.map (fun (innerPattern, innerValue) ->
                            patternDefinitelyMatchesExpr innerPattern innerValue)
                    let tailStatus = patternDefinitelyMatchesExpr tailPattern (ListLiteral tailValues)
                    combinePatternMatchStatuses (headStatuses @ [tailStatus])
            | PConstructor (patternVariantName, patternPayload), Constructor (_, valueVariantName, valuePayload) ->
                if not (variantNamesMatch patternVariantName valueVariantName) then
                    Some false
                else
                    match patternPayload, valuePayload with
                    | None, None ->
                        Some true
                    | Some patternPayloadExpr, Some valuePayloadExpr ->
                        patternDefinitelyMatchesExpr patternPayloadExpr valuePayloadExpr
                    | _ ->
                        Some false
            | _ ->
                None

        let knownCaseMatchStatus (matchCase: MatchCase) : bool option =
            match matchCase.Guard with
            | Some _ ->
                None
            | None ->
                let statuses =
                    matchCase.Patterns
                    |> NonEmptyList.toList
                    |> List.map (fun pattern ->
                        if patternAlwaysMatchesType pattern scrutineeType then
                            Some true
                        else
                            patternDefinitelyMatchesExpr pattern scrutinee')
                if statuses |> List.exists (fun status -> status = Some true) then
                    Some true
                elif statuses |> List.forall (fun status -> status = Some false) then
                    Some false
                else
                    None

        let rec patternIsBinderOnly (pattern: Pattern) : bool =
            match pattern with
            | PVar _
            | PWildcard ->
                true
            | PTuple patterns
            | PList patterns ->
                patterns |> List.forall patternIsBinderOnly
            | PListCons (headPatterns, tailPattern) ->
                (headPatterns |> List.forall patternIsBinderOnly)
                && patternIsBinderOnly tailPattern
            | _ ->
                false

        let caseCanShortCircuit (matchCase: MatchCase) : bool =
            Option.isNone matchCase.Guard
            && (
                matchCase.Patterns
                |> NonEmptyList.toList
                |> List.forall patternIsBinderOnly
            )

        // Exhaustiveness is an AOT property: lowering must never need a
        // synthetic runtime match-failure arm. A guarded case cannot cover
        // any value because its guard may be false.
        let rec patternCoversType (pattern: Pattern) (patternType: Type) : bool =
            match pattern, resolveType aliasReg patternType with
            | PVar _, _
            | PWildcard, _
            | PUnit, TUnit -> true
            | PTuple patterns, TTuple elementTypes
            | PTuple patterns, TEnumFields elementTypes when List.length patterns = List.length elementTypes ->
                List.zip patterns elementTypes
                |> List.forall (fun (innerPattern, elementType) ->
                    patternCoversType innerPattern elementType)
            | PListCons ([headPattern], tailPattern), TList elementType ->
                patternCoversType headPattern elementType
                && patternCoversType tailPattern (TList elementType)
            | _ -> false

        // Constructor payload types can retain their declaration-level
        // generic shape in the variant registry. Once type checking has
        // accepted a payload pattern, a variable/wildcard tuple payload is
        // irrefutable regardless of that unresolved representation.
        let rec patternIsIrrefutablePayload (pattern: Pattern) : bool =
            match pattern with
            | PVar _ | PWildcard | PUnit -> true
            | PTuple elements -> elements |> List.forall patternIsIrrefutablePayload
            | _ -> false

        let payloadPatternCoversType (pattern: Pattern) (patternType: Type) : bool =
            patternCoversType pattern patternType || patternIsIrrefutablePayload pattern

        let variantNamesMatchForExhaustiveness (leftName: string) (rightName: string) : bool =
            leftName = rightName
            || leftName.EndsWith($".{rightName}")
            || rightName.EndsWith($".{leftName}")

        let sumTypeNamesMatchForExhaustiveness (leftName: string) (rightName: string) : bool =
            let lastNameSegment (name: string) =
                name.Split('.') |> Array.last
            leftName = rightName
            || leftName.EndsWith($".{rightName}")
            || rightName.EndsWith($".{leftName}")
            // A module may share its type's name: the variant registry
            // records `Stdlib.Result`, while the concrete type is
            // `Stdlib.Result.Result`.
            || rightName = $"{leftName}.{lastNameSegment leftName}"
            || leftName = $"{rightName}.{lastNameSegment rightName}"

        let instantiateVariantPayloadForExhaustiveness
            (typeParams: string list)
            (typeArgs: Type list)
            (payloadType: Type option)
            : Type option =
            let substitution =
                if List.length typeParams = List.length typeArgs then
                    List.zip typeParams typeArgs |> Map.ofList
                else
                    Map.empty
            payloadType
            |> Option.map (fun payload ->
                payload
                |> applySubst substitution
                |> canonicalizeBareSumTypeRefsWithNames sumTypeNames
                |> function
                    | TEnumFields fields -> TTuple fields
                    | other -> other)

        let variantsForExhaustiveness
            (sumTypeName: string)
            (sumTypeArgs: Type list)
            : (string * Type option) list =
            let sumInfo =
                match Map.tryFind sumTypeName indexedSumTypeReg with
                | Some info -> Some info
                | None ->
                    indexedSumTypeReg
                    |> Map.toSeq
                    |> Seq.tryPick (fun (owner, info) ->
                        if sumTypeNamesMatchForExhaustiveness owner sumTypeName then
                            Some info
                        else
                            None)
            match sumInfo with
            | None -> []
            | Some info ->
                info.Variants
                |> List.map (fun variant ->
                    (variant.Name,
                     instantiateVariantPayloadForExhaustiveness
                         info.TypeParams
                         sumTypeArgs
                         variant.Payload))

        // Tuple matches are decision matrices. Split each finite head type
        // into its public constructors, then prove that the remaining
        // columns cover every resulting row. This covers, for example,
        // Result.map2's (Ok, Ok), (Error, _), (_, Error) matrix.
        let rec tupleDecisionMatrixIsExhaustive
            (remainingTypes: Type list)
            (rows: Pattern list list)
            : bool =
            match remainingTypes with
            | [] -> not (List.isEmpty rows)
            | currentType :: restTypes ->
                let resolvedCurrentType = resolveType aliasReg currentType
                let rowsCoveringCurrentType =
                    rows
                    |> List.choose (function
                        | pattern :: rest when patternCoversType pattern resolvedCurrentType -> Some rest
                        | _ -> None)
                match resolvedCurrentType with
                | TBool ->
                    let rowsFor value =
                        rows
                        |> List.choose (function
                            | (PWildcard | PVar _) :: rest -> Some rest
                            | PBool patternValue :: rest when patternValue = value -> Some rest
                            | _ -> None)
                    tupleDecisionMatrixIsExhaustive restTypes (rowsFor true)
                    && tupleDecisionMatrixIsExhaustive restTypes (rowsFor false)
                | TSum (sumTypeName, sumTypeArgs) ->
                    let variants = variantsForExhaustiveness sumTypeName sumTypeArgs
                    variants <> []
                    && variants
                       |> List.forall (fun (variantName, payloadType) ->
                           let rowsForVariant =
                               rows
                               |> List.choose (function
                                   | (PWildcard | PVar _) :: rest -> Some rest
                                   | PConstructor (patternName, patternPayload) :: rest
                                       when variantNamesMatchForExhaustiveness patternName variantName ->
                                           match patternPayload, payloadType with
                                           | None, None -> Some rest
                                           | Some payloadPattern, Some payloadType
                                               when payloadPatternCoversType payloadPattern payloadType -> Some rest
                                           | _ -> None
                                   | _ -> None)
                           tupleDecisionMatrixIsExhaustive restTypes rowsForVariant)
                | _ ->
                    tupleDecisionMatrixIsExhaustive restTypes rowsCoveringCurrentType

        let tupleMatchIsExhaustive (elementTypes: Type list) (patterns: Pattern list) : bool =
            let rows =
                patterns
                |> List.choose (function
                    | PTuple elements when List.length elements = List.length elementTypes -> Some elements
                    | PWildcard | PVar _ -> Some (elementTypes |> List.map (fun _ -> PWildcard))
                    | _ -> None)
            tupleDecisionMatrixIsExhaustive elementTypes rows

        // Coverage composes through constructor payloads. In particular,
        // `Ok(Linux) | Ok(MacOS) | ...` covers `Ok(OS)` when the nested
        // OS constructors are complete; requiring one `Ok(_)` arm loses
        // that information and rejects valid interpreter programs.
        let rec patternsCoverType (patternType: Type) (patterns: Pattern list) : bool =
            if patterns |> List.exists (fun pattern -> patternCoversType pattern patternType) then
                true
            else
                match resolveType aliasReg patternType with
                | TBool ->
                    patterns
                    |> List.choose (function PBool value -> Some value | _ -> None)
                    |> Set.ofList
                    |> (=) (Set.ofList [true; false])
                | TList elementType ->
                    let rec listPatternCoverageAt (pattern: Pattern) : Set<int> * int option =
                        match pattern with
                        | PList elements when elements |> List.forall (fun element -> patternCoversType element elementType) ->
                            (Set.singleton (List.length elements), None)
                        | PListCons (heads, tail) when heads |> List.forall (fun head -> patternCoversType head elementType) ->
                            let headCount = List.length heads
                            match tail with
                            | PWildcard | PVar _ -> (Set.empty, Some headCount)
                            | _ ->
                                let (tailLengths, tailMinimum) = listPatternCoverageAt tail
                                (tailLengths |> Set.map (fun length -> headCount + length),
                                 tailMinimum |> Option.map (fun minimum -> headCount + minimum))
                        | _ -> (Set.empty, None)
                    let (exactLengths, minimumLengths) =
                        patterns
                        |> List.fold (fun (allExact, allMinimums) pattern ->
                            let (exact, minimum) = listPatternCoverageAt pattern
                            (Set.union allExact exact, (minimum |> Option.toList) @ allMinimums)) (Set.empty, [])
                    match minimumLengths |> List.sort with
                    | [] -> false
                    | minimum :: _ ->
                        [0 .. minimum - 1] |> List.forall (fun length -> Set.contains length exactLengths)
                | TTuple elementTypes
                | TEnumFields elementTypes ->
                    tupleMatchIsExhaustive elementTypes patterns
                | TSum (sumTypeName, sumTypeArgs) ->
                    let variants = variantsForExhaustiveness sumTypeName sumTypeArgs
                    variants <> []
                    && variants
                       |> List.forall (fun (variantName, payloadType) ->
                           let matchingPayloads =
                               patterns
                               |> List.choose (function
                                   | PConstructor (patternName, payloadPattern)
                                       when variantNamesMatchForExhaustiveness patternName variantName ->
                                           Some payloadPattern
                                   | _ -> None)
                           match payloadType with
                           | None -> matchingPayloads |> List.exists Option.isNone
                           | Some payload ->
                               let coversPayloadDirectly =
                                   matchingPayloads
                                   |> List.exists (function
                                       | Some payloadPattern ->
                                           payloadPatternCoversType payloadPattern payload
                                       | None -> false)
                               coversPayloadDirectly
                               || (matchingPayloads
                                   |> List.choose id
                                   |> patternsCoverType payload))
                | _ -> false

        let rec listPatternCoverage (elementType: Type) (pattern: Pattern) : Set<int> * int option =
            match pattern with
            | PList elements when elements |> List.forall (fun element -> patternCoversType element elementType) ->
                (Set.singleton (List.length elements), None)
            | PListCons (heads, tail) when heads |> List.forall (fun head -> patternCoversType head elementType) ->
                let headCount = List.length heads
                match tail with
                | PWildcard
                | PVar _ -> (Set.empty, Some headCount)
                | _ ->
                    let (tailLengths, tailMinimum) = listPatternCoverage elementType tail
                    (tailLengths |> Set.map (fun length -> headCount + length),
                     tailMinimum |> Option.map (fun minimum -> headCount + minimum))
            | _ -> (Set.empty, None)

        let listPatternsCoverAllLengths (elementType: Type) (patterns: Pattern list) : bool =
            let (exactLengths, minimumLengths) =
                patterns
                |> List.fold (fun (allExact, allMinimums) pattern ->
                    let (exact, minimum) = listPatternCoverage elementType pattern
                    (Set.union allExact exact, (minimum |> Option.toList) @ allMinimums)) (Set.empty, [])
            match minimumLengths |> List.sort with
            | [] -> false
            | minimum :: _ ->
                [0 .. minimum - 1] |> List.forall (fun length -> Set.contains length exactLengths)

        // A literal scrutinee is safe when at least one arm definitely
        // matches it. Earlier unknown arms do not invalidate that proof:
        // they either select a body themselves or fall through to the
        // definitely matching arm.
        let knownScrutineeSelectsACase (matchCases: MatchCase list) : bool =
            matchCases
            |> List.exists (knownCaseMatchStatus >> (=) (Some true))

        let matchIsExhaustive (matchCases: MatchCase list) : bool =
            let unguardedPatterns =
                matchCases
                |> List.collect (fun matchCase ->
                    match matchCase.Guard with
                    | Some _ -> []
                    | None -> NonEmptyList.toList matchCase.Patterns)

            knownScrutineeSelectsACase matchCases
            || patternsCoverType scrutineeType unguardedPatterns

        // Type check each case and ensure they all return the same type
        // Returns (resultType, transformedCases)
        let rec checkCases (remaining: MatchCase list) (resultType: Type option) (accCases: MatchCase list) : Result<Type * MatchCase list, TypeError> =
            match remaining with
            | [] ->
                match resultType with
                | Some t -> Ok (t, List.rev accCases)
                | None -> Error (GenericError "Match expression must have at least one case")
            | matchCase :: rest ->
                validatePatternGroupBindings matchCase.Patterns
                |> Result.bind (fun () ->
                    // Extract bindings from first pattern after validation.
                    let firstPattern = NonEmptyList.head matchCase.Patterns
                    let allowNoMatchForKnownListLengthMismatch =
                        allowNoMatchForKnownListLengthMismatchInThisMatch
                        && List.isEmpty matchCase.Patterns.Tail
                    extractPatternBindings
                        firstPattern
                        scrutineeType
                        allowNoMatchForKnownListLengthMismatch
                    |> Result.bind (fun bindings ->
                        let caseEnv = List.fold (fun e (name, ty) -> Map.add name ty e) env bindings
                        // Type check guard if present (must be Bool)
                        let guardResult =
                            match matchCase.Guard with
                            | None -> Ok None
                            | Some guardExpr ->
                                let checkedGuardResult =
                                    checkExpr
                                        guardExpr
                                        caseEnv
                                        typeReg
                                        variantLookup
                                        genericFuncReg
                                        warningSettings
                                        moduleRegistry
                                        aliasReg
                                        (Some TBool)
                                let normalizedGuardResult =
                                    match checkedGuardResult with
                                    | Error (UndefinedVariable name) ->
                                        Error (UndefinedCallTarget name)
                                    | _ ->
                                        checkedGuardResult
                                normalizedGuardResult
                                |> Result.bind (fun (guardType, guard') ->
                                    if guardType = TBool then
                                        Ok (Some guard')
                                    else
                                        Error (TypeMismatch (TBool, guardType, "guard clause")))
                        guardResult
                        |> Result.bind (fun guard' ->
                            let checkedBodyResult =
                                checkExpr
                                    matchCase.Body
                                    caseEnv
                                    typeReg
                                    variantLookup
                                    genericFuncReg
                                    warningSettings
                                    moduleRegistry
                                    aliasReg
                                    resultType
                            let normalizedBodyResult =
                                match resultType, checkedBodyResult with
                                | Some expectedBodyType, Error (TypeMismatch (expectedTypeFromContext, _, mismatchContext))
                                    when expectedTypeFromContext = expectedBodyType
                                         && mismatchContext = "boolean literal" ->
                                    // Retry unconstrained so we can report mismatch against
                                    // the case result type ("match body"), not literal context.
                                    checkExpr
                                        matchCase.Body
                                        caseEnv
                                        typeReg
                                        variantLookup
                                        genericFuncReg
                                        warningSettings
                                        moduleRegistry
                                        aliasReg
                                        None
                                | _ ->
                                    checkedBodyResult
                            normalizedBodyResult
                            |> Result.bind (fun (bodyType, body') ->
                                let newCase = { Patterns = matchCase.Patterns; Guard = guard'; Body = body' }
                                match resultType with
                                | None ->
                                    checkCases rest (Some bodyType) (newCase :: accCases)
                                | Some expected ->
                                    // Use reconcileTypes to handle type variables and type aliases
                                    match reconcileTypes (Some aliasReg) expected bodyType with
                                    | Some reconciledType ->
                                        // Update resultType to the reconciled (concrete) type
                                        checkCases rest (Some reconciledType) (newCase :: accCases)
                                    | None ->
                                        Error (TypeMismatch (expected, bodyType, "match body"))))))

        // Pass expectedType to first case so empty lists, None, etc. get the right type
        checkCases cases expectedType []
        |> Result.bind (fun (matchType, cases') ->
            if not (matchIsExhaustive cases') then
                Error (GenericError $"Non-exhaustive match expression for {typeToString scrutineeType}")
            else
                match expectedType with
                | Some expected ->
                    // Use reconcileTypes for expected type check too
                    match reconcileTypes (Some aliasReg) expected matchType with
                    | Some reconciledType -> Ok (reconciledType, Match (scrutinee', cases'))
                    | None -> Error (TypeMismatch (expected, matchType, "match expression"))
                | None -> Ok (matchType, Match (scrutinee', cases'))))
