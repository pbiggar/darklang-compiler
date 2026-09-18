// MaterializeHelpers.fs - Insert reachable equality and ordering helper definitions.

module MaterializeHelpers

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open TypeUnification
open HelperDependencies

let rec private materializeHelperCallsInExpr
    (includeEquality: bool)
    (aliasReg: AliasRegistry)
    (variantLookup: VariantLookup)
    (expr: Expr)
    : Expr =
    let recurse = materializeHelperCallsInExpr includeEquality aliasReg variantLookup

    match expr with
    | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ ->
        expr
    | BinOp (op, left, right) ->
        BinOp (op, recurse left, recurse right)
    | UnaryOp (op, inner) ->
        UnaryOp (op, recurse inner)
    | Let (name, value, body) ->
        Let (name, recurse value, recurse body)
    | RecursiveLet (recursion, value, body) ->
        RecursiveLet (recursion, recurse value, recurse body)
    | If (cond, thenBranch, elseBranch) ->
        If (recurse cond, recurse thenBranch, recurse elseBranch)
    | Sequence (first, next) ->
        Sequence (recurse first, recurse next)
    | Call (funcName, args) ->
        Call (funcName, NonEmptyList.map recurse args)
    | TypeApp (funcName, typeArgs, args) as typeAppExpr ->
        match tryDecodeInternalTypeApp typeAppExpr with
        | Some (EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr)) when includeEquality ->
            let helperType = resolveType aliasReg targetType
            if needsEqHelperForResolvedType variantLookup helperType then
                Call (eqHelperName helperType, NonEmptyList.fromList [recurse leftExpr; recurse rightExpr])
            else
                TypeApp (funcName, typeArgs, NonEmptyList.fromList [recurse leftExpr; recurse rightExpr])
        | _ ->
            match funcName, typeArgs with
            | "__compare", [targetType] when not (containsTVar targetType) ->
                let helperType = resolveType aliasReg targetType
                Call (compareHelperName helperType, NonEmptyList.map recurse args)
            | _ -> TypeApp (funcName, typeArgs, NonEmptyList.map recurse args)
    | TupleLiteral elements ->
        TupleLiteral (List.map recurse elements)
    | TupleAccess (tupleExpr, index) ->
        TupleAccess (recurse tupleExpr, index)
    | DictLiteral (keyType, valueType, entries) ->
        DictLiteral (keyType, valueType, entries |> List.map (fun (key, value) -> (recurse key, recurse value)))
    | RecordLiteral (typeName, fields) ->
        RecordLiteral (typeName, fields |> List.map (fun (name, fieldExpr) -> (name, recurse fieldExpr)))
    | RecordUpdate (recordExpr, updates) ->
        RecordUpdate (recurse recordExpr, updates |> List.map (fun (name, updateExpr) -> (name, recurse updateExpr)))
    | RecordAccess (recordExpr, fieldName) ->
        RecordAccess (recurse recordExpr, fieldName)
    | Constructor (typeName, variantName, payload) ->
        Constructor (typeName, variantName, payload |> Option.map recurse)
    | Match (scrutinee, cases) ->
        Match (
            recurse scrutinee,
            cases
            |> List.map (fun matchCase -> {
                matchCase with
                    Guard = matchCase.Guard |> Option.map recurse
                    Body = recurse matchCase.Body
            })
        )
    | ListLiteral elements ->
        ListLiteral (List.map recurse elements)
    | Lambda (parameters, returnAnnotation, body) ->
        Lambda (parameters, returnAnnotation, recurse body)
    | Apply (funcExpr, args) ->
        Apply (recurse funcExpr, NonEmptyList.map recurse args)
    | IndirectApply (funcExpr, args) ->
        IndirectApply (recurse funcExpr, NonEmptyList.map recurse args)
    | Closure (funcName, captures) ->
        Closure (funcName, List.map recurse captures)
    | InterpolatedString parts ->
        InterpolatedString (
            parts
            |> List.map (function
                | StringText text -> StringText text
                | StringExpr partExpr -> StringExpr (recurse partExpr))
        )

let private materializeHelpersInTopLevels
    (includeEquality: bool)
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (topLevels: TopLevel list)
    : TopLevel list =
    let collectFromTopLevel (topLevel: TopLevel) : Set<Type> =
        match topLevel with
        | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
            collectEqHelperTypesFromExpr aliasReg funcDef.Body
        | FunctionDef _ ->
            // Generic templates are retained for later specialization. Their
            // comparison plans are materialized in each concrete copy.
            Set.empty
        | Expression expr ->
            collectEqHelperTypesFromExpr aliasReg expr
        | ValueDef valueDef ->
            collectEqHelperTypesFromExpr aliasReg (valueDefBody valueDef)
        | TypeDef _ ->
            Set.empty

    let collectCompareFromTopLevel (topLevel: TopLevel) : Set<Type> =
        match topLevel with
        | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
            collectCompareHelperTypesFromExpr aliasReg funcDef.Body
        | FunctionDef _ -> Set.empty
        | Expression expr -> collectCompareHelperTypesFromExpr aliasReg expr
        | ValueDef valueDef -> collectCompareHelperTypesFromExpr aliasReg (valueDefBody valueDef)
        | TypeDef _ -> Set.empty

    let rewriteTopLevel (topLevel: TopLevel) : TopLevel =
        match topLevel with
        | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
            FunctionDef {
                funcDef with
                    Body = materializeHelperCallsInExpr includeEquality aliasReg variantLookup funcDef.Body
            }
        | FunctionDef _ ->
            topLevel
        | Expression expr ->
            Expression (materializeHelperCallsInExpr includeEquality aliasReg variantLookup expr)
        | ValueDef valueDef ->
            let body = materializeHelperCallsInExpr includeEquality aliasReg variantLookup (valueDefBody valueDef)
            match valueDef with
            | UncheckedValueDef (name, _) -> ValueDef (UncheckedValueDef (name, body))
            | CheckedValueDef (name, typ, _) -> ValueDef (CheckedValueDef (name, typ, body))
        | TypeDef _ ->
            topLevel

    let helperTypes =
        if includeEquality then
            topLevels
            |> List.map collectFromTopLevel
            |> List.fold Set.union Set.empty
        else
            Set.empty

    let compareHelperTypes =
        topLevels
        |> List.map collectCompareFromTopLevel
        |> List.fold Set.union Set.empty

    let rewrittenTopLevels = topLevels |> List.map rewriteTopLevel

    if Set.isEmpty helperTypes && Set.isEmpty compareHelperTypes then
        rewrittenTopLevels
    else
        let initialEqState : EqHelperGenerationState = {
            InProgress = Set.empty
            Generated = Map.empty
        }

        let finalEqState =
            helperTypes
            |> Set.fold
                (fun currentState helperType ->
                    ensureEqHelperForType
                        aliasReg
                        typeReg
                        variantLookup
                        indexedSumTypeReg
                        helperType
                        currentState)
                initialEqState

        let initialCompareState : CompareHelperGenerationState = {
            InProgress = Set.empty
            Generated = Map.empty
        }

        let finalCompareState =
            compareHelperTypes
            |> Set.fold
                (fun currentState helperType ->
                    ensureCompareHelperForType
                        aliasReg
                        typeReg
                        variantLookup
                        indexedSumTypeReg
                        helperType
                        currentState)
                initialCompareState

        let helperTopLevels =
            let existingFunctionNames =
                topLevels
                |> List.choose (function
                    | FunctionDef funcDef -> Some funcDef.Name
                    | _ -> None)
                |> Set.ofList
            Map.fold
                (fun generated name helperDef -> Map.add name helperDef generated)
                finalEqState.Generated
                finalCompareState.Generated
            |> Map.toList
            |> List.map snd
            |> List.filter (fun helperDef -> not (Set.contains helperDef.Name existingFunctionNames))
            |> List.map FunctionDef

        helperTopLevels @ rewrittenTopLevels

/// Materialize equality and canonical comparison dispatches in a complete
/// concrete program.
let materializeEqHelpersInTopLevelsWithIndexedSums
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (topLevels: TopLevel list)
    : TopLevel list =
    materializeHelpersInTopLevels
        true
        aliasReg
        typeReg
        variantLookup
        indexedSumTypeReg
        topLevels

/// Compatibility entry point for callers that do not retain a type-checking
/// environment. Hot compilation paths pass its existing indexed sum registry.
let materializeEqHelpersInTopLevels
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (topLevels: TopLevel list)
    : TopLevel list =
    materializeEqHelpersInTopLevelsWithIndexedSums
        aliasReg
        typeReg
        variantLookup
        (indexSumTypeRegistry variantLookup)
        topLevels

/// Materialize only canonical comparison dispatches in newly-specialized
/// stdlib functions. Equality dispatches retain the established stdlib
/// specialization path.
let materializeCompareHelpersInTopLevels
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (topLevels: TopLevel list)
    : TopLevel list =
    materializeHelpersInTopLevels
        false
        aliasReg
        typeReg
        variantLookup
        (indexSumTypeRegistry variantLookup)
        topLevels
