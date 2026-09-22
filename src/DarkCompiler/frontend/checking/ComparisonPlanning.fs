// ComparisonPlanning.fs - Plan typed equality and ordering operations.

module ComparisonPlanning

open AST
open CheckingDiagnostics
open CheckingTypes

/// Internal type-app markers emitted during type checking.
/// These markers are materialized to direct calls before leaving this pass.
type internal InternalTypeAppMarker =
    | EqHelperDispatch

/// Internal type-app values carried in `Expr.TypeApp` nodes.
/// We encode/decode them through marker names at the pass boundary.
type internal InternalTypeApp =
    | EqHelperDispatchTypeApp of targetType: SemanticType * leftExpr: Expr * rightExpr: Expr

let internal internalTypeAppMarkerName (marker: InternalTypeAppMarker) : string =
    match marker with
    | EqHelperDispatch -> "__dark_internal_eq_helper_dispatch"

let private tryParseInternalTypeAppMarker (funcName: string) : InternalTypeAppMarker option =
    if funcName = internalTypeAppMarkerName EqHelperDispatch then
        Some EqHelperDispatch
    else
        None

let internal makeInternalTypeApp (internalTypeApp: InternalTypeApp) : Expr =
    match internalTypeApp with
    | EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr) ->
        Apply (
            Var (internalTypeAppMarkerName EqHelperDispatch),
            [targetType],
            NonEmptyList.fromList [leftExpr; rightExpr]
        )

let internal tryDecodeInternalTypeApp (expr: Expr) : InternalTypeApp option =
    match expr with
    | Apply (Var funcName, [targetType], { Head = leftExpr; Tail = [rightExpr] }) ->
        match tryParseInternalTypeAppMarker funcName with
        | Some EqHelperDispatch ->
            Some (EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr))
        | None ->
            None
    | _ ->
        None

let internal sumTypeHasPayload (variantLookup: VariantLookup) (sumTypeName: string) : bool =
    variantLookup
    |> Map.exists (fun _ (variantTypeName, _, _, fields) ->
        variantTypeName = sumTypeName && not (List.isEmpty fields))

/// Parsed source names are initially represented as TRecord. Canonicalize
/// names owned by the variant registry before constructing equality plans.
let rec internal canonicalEqualityType (variantLookup: VariantLookup) (typ: SemanticType) : SemanticType =
    let canonical = canonicalEqualityType variantLookup
    match typ with
    | TRecord (name, typeArgs) when
        variantLookup |> Map.exists (fun _ (owner, _, _, _) -> owner = name) ->
        TSum (name, List.map canonical typeArgs)
    | TRecord (name, typeArgs) -> TRecord (name, List.map canonical typeArgs)
    | TSum (name, typeArgs) -> TSum (name, List.map canonical typeArgs)
    | TTuple elementTypes -> TTuple (List.map canonical elementTypes)
    | TList elementType -> TList (canonical elementType)
    | TDict (keyType, valueType) -> TDict (canonical keyType, canonical valueType)
    | TFunction (parameterTypes, returnType) ->
        TFunction (List.map canonical parameterTypes, canonical returnType)
    | _ -> typ

/// Json conversion is fully planned at compile time. Reject shapes for which
/// no plan can exist here, before specialization can turn them into runtime
/// control flow.
let internal validateJsonTargetType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (targetType: SemanticType)
    : Result<unit, TypeError> =
    let unsupported typ =
        Error (
            GenericError
                $"Unsupported type in JSON: {typeToString typ}. Some types are not supported in Json serialization"
        )
    let rec validate visited typ =
        let typ = resolveType aliasReg typ |> canonicalEqualityType variantLookup
        let identity = typeToString typ
        if Set.contains identity visited then Ok ()
        else
            let visited = Set.add identity visited
            let validateAll types =
                types
                |> List.fold (fun result item -> result |> Result.bind (fun () -> validate visited item)) (Ok ())
            match typ with
            | TUnit | TBool | TInt8 | TUInt8 | TInt16 | TUInt16 | TInt32 | TUInt32
            | TInt64 | TUInt64 | TInt128 | TUInt128 | TInt | TFloat64 | TChar | TString | TDateTime -> Ok ()
            | TSum ("Uuid", []) -> Ok ()
            | TTuple types -> validateAll types
            | TList inner -> validate visited inner
            | TDict (TString, inner) -> validate visited inner
            | TRecord (name, typeArgs) ->
                match Map.tryFind name typeReg with
                | None -> unsupported typ
                | Some info ->
                    match buildSubstitution info.TypeParams typeArgs with
                    | Error _ -> unsupported typ
                    | Ok subst -> info.Fields |> List.map (snd >> applySubst subst) |> validateAll
            | TSum (name, typeArgs) ->
                match Map.tryFind name indexedSumTypeReg with
                | None -> unsupported typ
                | Some info ->
                    match buildSubstitution info.TypeParams typeArgs with
                    | Error _ -> unsupported typ
                    | Ok subst ->
                        info.Variants
                        |> List.collect (fun variant -> List.map (applySubst subst) variant.Fields)
                        |> validateAll
            | TFunction _ | TBlob | TInternalRawPtr | TNever | TStream _ | TVar _ | TDict _ -> unsupported typ
    validate Set.empty targetType

/// Every concrete compound comparable type has one equality entry point.
let rec needsEqHelperForResolvedType (variantLookup: VariantLookup) (typ: SemanticType) : bool =
    match canonicalEqualityType variantLookup typ with
    | TFunction _ | TList _ | TDict _ | TTuple _ | TRecord _ -> true
    | TSum (sumTypeName, _) -> sumTypeHasPayload variantLookup sumTypeName
    | _ -> false

let private sanitizeHelperNamePrefix (input: string) : string =
    let chars =
        input
        |> Seq.map (fun c -> if System.Char.IsLetterOrDigit c then c else '_')
        |> Seq.truncate 48
        |> Seq.toArray

    if Array.isEmpty chars then
        "type"
    else
        System.String(chars)

/// Stable, deterministic hash used for generated helper function names.
let private stableHelperNameHash (input: string) : uint64 =
    let initial = 14695981039346656037UL
    let prime = 1099511628211UL
    input
    |> Seq.fold (fun acc ch -> (acc ^^^ uint64 (int ch)) * prime) initial

/// Name for a concrete structural equality helper.
let eqHelperName (typ: SemanticType) : string =
    let typeText = typeToString typ
    let prefix = sanitizeHelperNamePrefix typeText
    let hash = stableHelperNameHash typeText
    $"__dark_eq_{prefix}_{hash:x16}"

/// Name for a concrete canonical three-way comparison helper.
let compareHelperName (typ: SemanticType) : string =
    let typeText = typeToString typ
    let prefix = sanitizeHelperNamePrefix typeText
    let hash = stableHelperNameHash typeText
    $"__dark_compare_{prefix}_{hash:x16}"

/// Build a left-associative boolean conjunction chain.
let internal chainAndExpr (exprs: Expr list) : Expr =
    match exprs with
    | [] ->
        BoolLiteral true
    | first :: rest ->
        List.fold (fun acc expr -> BinOp (And, acc, expr)) first rest

/// Build an equality expression for two already type-checked operands.
/// For tuple/record/sum, emit a typed internal dispatch marker that will later
/// be rewritten to a call to the generated concrete helper function.
let internal buildEqExprForType
    (aliasReg: AliasRegistry)
    (variantLookup: VariantLookup)
    (typ: SemanticType)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let resolvedType = typ |> resolveType aliasReg |> canonicalEqualityType variantLookup
    match resolvedType with
    | TVar _ ->
        // A generic comparison cannot select a representation-level operation
        // until specialization. Preserve the typed plan through substitution.
        makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
    | TFunction _ ->
        makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
    | TString ->
        BinOp (Eq, leftExpr, rightExpr)
    | TInt ->
        Apply (Var "Darklang.Stdlib.Int.__equals", [], NonEmptyList.fromList [leftExpr; rightExpr])
    | TInt128 ->
        Apply (Var "Darklang.Stdlib.Int128.__equals", [], NonEmptyList.fromList [leftExpr; rightExpr])
    | TUInt128 ->
        Apply (Var "Darklang.Stdlib.UInt128.__equals", [], NonEmptyList.fromList [leftExpr; rightExpr])
    | TList elemType ->
        let resolvedElemType = resolveType aliasReg elemType
        makeInternalTypeApp (EqHelperDispatchTypeApp (TList resolvedElemType, leftExpr, rightExpr))
    | _ when needsEqHelperForResolvedType variantLookup resolvedType ->
        makeInternalTypeApp (EqHelperDispatchTypeApp (resolvedType, leftExpr, rightExpr))
    | _ ->
        BinOp (Eq, leftExpr, rightExpr)

/// A comparison is classified while both resolved operand types are available.
/// Equality plans may still contain type variables in a generic body; the
/// internal typed marker carries them through substitution and is materialized
/// only after a concrete specialization exists.
type internal ComparisonPlan =
    | EqualityComparison of comparableType:SemanticType
    | OrderingComparison of numericType:SemanticType

let private comparisonNumericType (typ: SemanticType) : bool =
    match typ with
    | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
    | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
    | TFloat64 -> true
    | _ -> false

let private equalityComparableType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: SemanticType)
    : bool =
    let rec comparable (seen: Set<SemanticType>) (candidate: SemanticType) : bool =
        let resolved = resolveType aliasReg candidate
        if Set.contains resolved seen then
            // Recursive nominal types are admissible when the cycle itself has
            // introduced no rejected payload type.
            true
        else
            let seen = Set.add resolved seen
            let recurse = comparable seen
            match resolved with
            | TVar _ -> true
            | TUnit | TBool | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
            | TFloat64 | TChar | TString | TDateTime -> true
            | TFunction (parameterTypes, returnType) ->
                List.forall recurse parameterTypes && recurse returnType
            | TTuple elementTypes ->
                List.forall recurse elementTypes
            | TList elementType ->
                recurse elementType
            | TStream _ -> true
            | TDict (keyType, valueType) ->
                // Dict key admission remains owned by Dict. Comparison reuses
                // the key semantics already selected for an admitted key type.
                recurse keyType && recurse valueType
            | TRecord (recordName, typeArgs) ->
                match Map.tryFind recordName typeReg with
                | None when Map.containsKey recordName indexedSumTypeReg ->
                    comparable seen (TSum (recordName, typeArgs))
                | None -> false
                | Some recordInfo ->
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Error _ -> false
                    | Ok subst ->
                        recordInfo.Fields
                        |> List.forall (fun (_, fieldType) -> recurse (applySubst subst fieldType))
            | TSum (sumName, typeArgs) ->
                match Map.tryFind sumName indexedSumTypeReg with
                | None -> true
                | Some info ->
                    let subst =
                        if List.length info.TypeParams = List.length typeArgs then
                            List.zip info.TypeParams typeArgs |> Map.ofList
                        else
                            Map.empty
                    info.Variants
                    |> List.forall (fun variant ->
                        variant.Fields
                        |> List.forall (fun field -> recurse (applySubst subst field)))
            | TBlob -> true
            | TInternalRawPtr | TNever -> false

    comparable Set.empty typ

/// Canonical sorting is selected statically. Values with no interpreter
/// ordering in the compiler representation are rejected before lowering.
let internal canonicalSortableType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: SemanticType)
    : bool =
    let rec sortable (seen: Set<SemanticType>) (candidate: SemanticType) : bool =
        let resolved = resolveType aliasReg candidate
        if Set.contains resolved seen then
            true
        else
            let seen = Set.add resolved seen
            let recurse = sortable seen
            match resolved with
            | TVar _ -> true
            | TUnit | TBool | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
            | TFloat64 | TChar | TString | TDateTime -> true
            | TTuple elementTypes -> List.forall recurse elementTypes
            | TList elementType -> recurse elementType
            | TStream _ -> false
            | TDict (keyType, valueType) -> recurse keyType && recurse valueType
            | TRecord (recordName, typeArgs) ->
                match Map.tryFind recordName typeReg with
                | None -> false
                | Some recordInfo ->
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Error _ -> false
                    | Ok subst ->
                        recordInfo.Fields
                        |> List.forall (fun (_, fieldType) -> recurse (applySubst subst fieldType))
            | TSum (sumName, typeArgs) ->
                match Map.tryFind sumName indexedSumTypeReg with
                | None -> true
                | Some info ->
                    let subst =
                        if List.length info.TypeParams = List.length typeArgs then
                            List.zip info.TypeParams typeArgs |> Map.ofList
                        else
                            Map.empty
                    info.Variants
                    |> List.forall (fun variant ->
                        variant.Fields
                        |> List.forall (fun field -> recurse (applySubst subst field)))
            | TDict _ | TFunction _ | TBlob | TInternalRawPtr | TNever -> false

    sortable Set.empty typ

/// Dict keys use structural equality and therefore must not retain executable,
/// streaming, opaque, or compiler-internal values anywhere in their shape.
let internal dictKeyAdmissibleType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: SemanticType)
    : bool =
    let rec admissible (seen: Set<SemanticType>) (candidate: SemanticType) : bool =
        let resolved = resolveType aliasReg candidate
        if Set.contains resolved seen then
            true
        else
            let seen = Set.add resolved seen
            let recurse = admissible seen
            match resolved with
            | TVar _ -> true
            | TUnit | TBool | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt
            | TUInt8 | TUInt16 | TUInt32 | TUInt64 | TUInt128
            | TFloat64 | TChar | TString | TDateTime -> true
            | TTuple elementTypes -> List.forall recurse elementTypes
            | TList elementType -> recurse elementType
            | TDict (keyType, valueType) -> recurse keyType && recurse valueType
            | TRecord (recordName, typeArgs) ->
                match Map.tryFind recordName typeReg with
                | None when Map.containsKey recordName indexedSumTypeReg ->
                    recurse (TSum (recordName, typeArgs))
                | None -> false
                | Some recordInfo ->
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Error _ -> false
                    | Ok subst ->
                        recordInfo.Fields
                        |> List.forall (fun (_, fieldType) -> recurse (applySubst subst fieldType))
            | TSum (sumName, typeArgs) ->
                match Map.tryFind sumName indexedSumTypeReg with
                | None -> true
                | Some info ->
                    let subst =
                        if List.length info.TypeParams = List.length typeArgs then
                            List.zip info.TypeParams typeArgs |> Map.ofList
                        else
                            Map.empty
                    info.Variants
                    |> List.forall (fun variant ->
                        variant.Fields
                        |> List.forall (fun field -> recurse (applySubst subst field)))
            | TFunction _ | TStream _ | TBlob | TInternalRawPtr | TNever -> false

    admissible Set.empty typ

let internal validateDictKeyCall
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (funcName: string)
    (typeArgs: SemanticType list)
    : Result<unit, TypeError> =
    match funcName, typeArgs with
    | name, keyType :: _ when
        (name.StartsWith("Darklang.Stdlib.Dict.") && not (name.StartsWith("Darklang.Stdlib.Dict.__")))
        || (name.StartsWith("Dict.") && not (name.StartsWith("Dict.__"))) ->
        if dictKeyAdmissibleType aliasReg typeReg indexedSumTypeReg keyType then
            Ok ()
        else
            Error (
                GenericError
                    $"Type {typeToString (resolveType aliasReg keyType)} cannot be used as a Dict key"
            )
    | _ -> Ok ()

let internal validateCanonicalSortableCall
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (funcName: string)
    (typeArgs: SemanticType list)
    : Result<unit, TypeError> =
    let requiredTypes =
        match funcName, typeArgs with
        | "__compare", [valueType]
        | "Darklang.Stdlib.List.sort", [valueType]
        | "Darklang.Stdlib.List.unique", [valueType]
        | "Darklang.Stdlib.List.uniqueBy", [valueType; _] -> [valueType]
        | "Darklang.Stdlib.List.sortBy", [valueType; keyType] -> [valueType; keyType]
        | _ -> []

    match requiredTypes |> List.tryFind (canonicalSortableType aliasReg typeReg indexedSumTypeReg >> not) with
    | None -> Ok ()
    | Some unsupportedType ->
        Error (
            GenericError
                $"Canonical sorting is not supported for type {typeToString (resolveType aliasReg unsupportedType)}"
        )

let rec private reconcileComparisonTypes
    (aliasReg: AliasRegistry)
    (leftType: SemanticType)
    (rightType: SemanticType)
    : SemanticType option =
    let leftResolved = resolveType aliasReg leftType
    let rightResolved = resolveType aliasReg rightType
    let rec reconcileMany leftTypes rightTypes acc =
        match leftTypes, rightTypes with
        | [], [] -> Some (List.rev acc)
        | left :: leftRest, right :: rightRest ->
            reconcileComparisonTypes aliasReg left right
            |> Option.bind (fun reconciled ->
                reconcileMany leftRest rightRest (reconciled :: acc))
        | _ -> None

    if leftResolved = rightResolved then
        Some leftResolved
    else
        match leftResolved, rightResolved with
        | TNever, other
        | other, TNever -> Some other
        | TVar _, other
        | other, TVar _ -> Some other
        | TList leftElement, TList rightElement ->
            reconcileComparisonTypes aliasReg leftElement rightElement
            |> Option.map TList
        | TStream leftElement, TStream rightElement ->
            reconcileComparisonTypes aliasReg leftElement rightElement
            |> Option.map TStream
        | TDict (leftKey, leftValue), TDict (rightKey, rightValue) ->
            reconcileComparisonTypes aliasReg leftKey rightKey
            |> Option.bind (fun keyType ->
                reconcileComparisonTypes aliasReg leftValue rightValue
                |> Option.map (fun valueType -> TDict (keyType, valueType)))
        | TTuple leftElements, TTuple rightElements ->
            reconcileMany leftElements rightElements [] |> Option.map TTuple
        | TRecord (leftName, leftArgs), TRecord (rightName, rightArgs)
            when leftName = rightName ->
            reconcileMany leftArgs rightArgs []
            |> Option.map (fun typeArgs -> TRecord (leftName, typeArgs))
        | TSum (leftName, leftArgs), TSum (rightName, rightArgs)
            when leftName = rightName ->
            reconcileMany leftArgs rightArgs []
            |> Option.map (fun typeArgs -> TSum (leftName, typeArgs))
        | TFunction (leftParams, leftReturn), TFunction (rightParams, rightReturn) ->
            reconcileMany leftParams rightParams []
            |> Option.bind (fun parameterTypes ->
                reconcileComparisonTypes aliasReg leftReturn rightReturn
                |> Option.map (fun returnType -> TFunction (parameterTypes, returnType)))
        | _ -> None

let internal classifyComparison
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (op: BinOp)
    (leftType: SemanticType)
    (rightType: SemanticType)
    : Result<ComparisonPlan, TypeError> =
    let leftResolved = resolveType aliasReg leftType
    let rightResolved = resolveType aliasReg rightType
    match op with
    | Eq | Neq ->
        let comparableType = reconcileComparisonTypes aliasReg leftResolved rightResolved
        match comparableType with
        | Some typ when equalityComparableType aliasReg typeReg indexedSumTypeReg typ ->
            Ok (EqualityComparison typ)
        | _ ->
            Error (IncompatibleEqualityOperands (leftResolved, rightResolved))
    | Lt | Gt | Lte | Gte ->
        match reconcileComparisonTypes aliasReg leftResolved rightResolved with
        | Some numericType when comparisonNumericType numericType ->
            // A literal or other concrete operand constrains the unresolved
            // generic operand. The enclosing call can remain polymorphic when
            // no value reaches that operand (for example List.all []).
            Ok (OrderingComparison numericType)
        | _ ->
            Error (IncompatibleOrderingOperands (leftResolved, rightResolved))
    | _ ->
        Crash.crash $"Non-comparison operator reached comparison classification: {op}"

let private orderingFunctionName (op: BinOp) : string =
    match op with
    | Lt -> "Darklang.Stdlib.Int.lessThan"
    | Gt -> "Darklang.Stdlib.Int.greaterThan"
    | Lte -> "Darklang.Stdlib.Int.lessThanOrEqualTo"
    | Gte -> "Darklang.Stdlib.Int.greaterThanOrEqualTo"
    | _ -> Crash.crash $"Non-ordering operator has no Int comparison helper: {op}"

let internal buildOrderingExprForType
    (op: BinOp)
    (numericType: SemanticType)
    (leftExpr: Expr)
    (rightExpr: Expr)
    : Expr =
    let convertedOperands =
        match numericType with
        | TInt128 ->
            (Apply (Var "__int128_to_int", [], NonEmptyList.singleton leftExpr),
             Apply (Var "__int128_to_int", [], NonEmptyList.singleton rightExpr))
        | TUInt128 ->
            (Apply (Var "__uint128_to_int", [], NonEmptyList.singleton leftExpr),
             Apply (Var "__uint128_to_int", [], NonEmptyList.singleton rightExpr))
        | _ ->
            (leftExpr, rightExpr)
    match numericType, convertedOperands with
    | (TInt128 | TUInt128), (leftInt, rightInt) ->
        Apply (Var (orderingFunctionName op), [], NonEmptyList.fromList [leftInt; rightInt])
    | _ ->
        BinOp (op, leftExpr, rightExpr)
