// CheckedMaterializeHelpers.fs - Materialize comparison helpers in checked syntax.
//
// Concrete generic specializations are created after source checking. This
// pass keeps that late helper discovery entirely on CheckedAST while reusing
// the canonical helper-definition generators owned by semantic checking.

module CheckedMaterializeHelpers

open CheckingTypes
open CheckingDiagnostics
open ComparisonPlanning
open TypeUnification
open HelperDependencies

let rec private collectHelperTypes
    (aliasReg: AliasRegistry)
    (expr: CheckedAST.Expr)
    : Set<AST.Type> * Set<AST.Type> =
    let collect = collectHelperTypes aliasReg
    let combine expressions =
        expressions
        |> List.map collect
        |> List.fold
            (fun (eqTypes, compareTypes) (childEq, childCompare) ->
                (Set.union eqTypes childEq, Set.union compareTypes childCompare))
            (Set.empty, Set.empty)
    let withChildren eqTypes compareTypes children =
        let (childEq, childCompare) = combine children
        (Set.union eqTypes childEq, Set.union compareTypes childCompare)
    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _
    | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _
    | CheckedAST.Int32Literal _ | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _
    | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _
    | CheckedAST.FloatLiteral _ | CheckedAST.Var _ | CheckedAST.FuncRef _
    | CheckedAST.RuntimeError _ -> (Set.empty, Set.empty)
    | CheckedAST.BoundaryRender (_, value)
    | CheckedAST.UnaryOp (_, value)
    | CheckedAST.TupleAccess (value, _)
    | CheckedAST.RecordAccess (value, _) -> collect value
    | CheckedAST.BinOp (_, left, right)
    | CheckedAST.Let (_, left, right)
    | CheckedAST.RecursiveLet (_, left, right)
    | CheckedAST.Sequence (left, right) -> combine [left; right]
    | CheckedAST.If (condition, thenBranch, elseBranch) ->
        combine [condition; thenBranch; elseBranch]
    | CheckedAST.Call (_, args) -> args |> AST.NonEmptyList.toList |> combine
    | CheckedAST.TypeApp (name, typeArgs, args) ->
        let arguments = AST.NonEmptyList.toList args
        let concreteTypeArgs =
            typeArgs
            |> List.map (resolveType aliasReg)
            |> List.filter (containsTVar >> not)
            |> Set.ofList
        let equalityTypes =
            match name, typeArgs, arguments with
            | "__dark_internal_eq_helper_dispatch", [targetType], [_; _] ->
                Set.singleton (resolveType aliasReg targetType)
            | _ -> concreteTypeArgs
        let compareTypes =
            match name, typeArgs with
            | "__compare", [targetType]
            | ("Stdlib.List.sort" | "Stdlib.List.unique"), [targetType]
            | "Stdlib.List.uniqueBy", [targetType; _] ->
                let resolved = resolveType aliasReg targetType
                if containsTVar resolved then Set.empty else Set.singleton resolved
            | "Stdlib.List.sortBy", [valueType; keyType] ->
                let pairType =
                    AST.TTuple [resolveType aliasReg keyType; resolveType aliasReg valueType]
                if containsTVar pairType then Set.empty else Set.singleton pairType
            | _ -> Set.empty
        withChildren equalityTypes compareTypes arguments
    | CheckedAST.TupleLiteral elements
    | CheckedAST.ListLiteral elements -> combine elements
    | CheckedAST.DictLiteral (_, _, entries) ->
        entries |> List.collect (fun (key, value) -> [key; value]) |> combine
    | CheckedAST.RecordLiteral (_, entries) -> entries |> List.map snd |> combine
    | CheckedAST.RecordUpdate (record, updates) ->
        combine (record :: (updates |> List.map snd))
    | CheckedAST.Constructor (_, _, fields) ->
        combine fields
    | CheckedAST.Match (scrutinee, cases) ->
        let caseExpressions =
            cases
            |> List.collect (fun case -> case.Body :: (case.Guard |> Option.toList))
        combine (scrutinee :: caseExpressions)
    | CheckedAST.Lambda (_, _, body) -> collect body
    | CheckedAST.Apply (func, args)
    | CheckedAST.IndirectApply (func, args) ->
        combine (func :: AST.NonEmptyList.toList args)
    | CheckedAST.Closure (_, captures) -> combine captures
    | CheckedAST.InterpolatedString parts ->
        parts
        |> List.choose (function
            | CheckedAST.StringText _ -> None
            | CheckedAST.StringExpr partExpr -> Some partExpr)
        |> combine

let rec private rewriteHelperCalls
    (aliasReg: AliasRegistry)
    (variantLookup: VariantLookup)
    (expr: CheckedAST.Expr)
    : CheckedAST.Expr =
    let recurse = rewriteHelperCalls aliasReg variantLookup
    let recurseArgs = AST.NonEmptyList.map recurse
    match expr with
    | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _
    | CheckedAST.BigIntLiteral _ | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _
    | CheckedAST.Int32Literal _ | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _
    | CheckedAST.UInt32Literal _ | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _
    | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.CharLiteral _
    | CheckedAST.FloatLiteral _ | CheckedAST.Var _ | CheckedAST.FuncRef _
    | CheckedAST.RuntimeError _ -> expr
    | CheckedAST.BoundaryRender (renderer, value) -> CheckedAST.BoundaryRender (renderer, recurse value)
    | CheckedAST.BinOp (op, left, right) -> CheckedAST.BinOp (op, recurse left, recurse right)
    | CheckedAST.UnaryOp (op, value) -> CheckedAST.UnaryOp (op, recurse value)
    | CheckedAST.Let (pattern, value, body) -> CheckedAST.Let (pattern, recurse value, recurse body)
    | CheckedAST.RecursiveLet (recursion, value, body) ->
        CheckedAST.RecursiveLet (recursion, recurse value, recurse body)
    | CheckedAST.If (condition, thenBranch, elseBranch) ->
        CheckedAST.If (recurse condition, recurse thenBranch, recurse elseBranch)
    | CheckedAST.Sequence (first, next) -> CheckedAST.Sequence (recurse first, recurse next)
    | CheckedAST.Call (name, args) -> CheckedAST.Call (name, recurseArgs args)
    | CheckedAST.TypeApp
        ("__dark_internal_eq_helper_dispatch", [targetType], { Head = left; Tail = [right] }) ->
        let helperType = resolveType aliasReg targetType
        if needsEqHelperForResolvedType variantLookup helperType then
            CheckedAST.Call (
                eqHelperName helperType,
                AST.NonEmptyList.fromList [recurse left; recurse right]
            )
        else
            CheckedAST.TypeApp (
                "__dark_internal_eq_helper_dispatch",
                [targetType],
                AST.NonEmptyList.fromList [recurse left; recurse right]
            )
    | CheckedAST.TypeApp ("__compare", [targetType], args) when not (containsTVar targetType) ->
        let helperType = resolveType aliasReg targetType
        CheckedAST.Call (compareHelperName helperType, recurseArgs args)
    | CheckedAST.TypeApp (name, typeArgs, args) ->
        CheckedAST.TypeApp (name, typeArgs, recurseArgs args)
    | CheckedAST.TupleLiteral elements -> CheckedAST.TupleLiteral (List.map recurse elements)
    | CheckedAST.TupleAccess (tuple, index) -> CheckedAST.TupleAccess (recurse tuple, index)
    | CheckedAST.DictLiteral (keyType, valueType, entries) ->
        CheckedAST.DictLiteral (
            keyType,
            valueType,
            entries |> List.map (fun (key, value) -> recurse key, recurse value)
        )
    | CheckedAST.RecordLiteral (reference, fields) ->
        CheckedAST.RecordLiteral (reference, fields |> List.map (fun (name, value) -> name, recurse value))
    | CheckedAST.RecordUpdate (record, updates) ->
        CheckedAST.RecordUpdate (recurse record, updates |> List.map (fun (name, value) -> name, recurse value))
    | CheckedAST.RecordAccess (record, fieldName) -> CheckedAST.RecordAccess (recurse record, fieldName)
    | CheckedAST.Constructor (reference, variantName, fields) ->
        CheckedAST.Constructor (reference, variantName, List.map recurse fields)
    | CheckedAST.Match (scrutinee, cases) ->
        CheckedAST.Match (
            recurse scrutinee,
            cases
            |> List.map (fun case ->
                { case with Guard = Option.map recurse case.Guard; Body = recurse case.Body })
        )
    | CheckedAST.ListLiteral elements -> CheckedAST.ListLiteral (List.map recurse elements)
    | CheckedAST.Lambda (parameters, annotation, body) ->
        CheckedAST.Lambda (parameters, annotation, recurse body)
    | CheckedAST.Apply (func, args) -> CheckedAST.Apply (recurse func, recurseArgs args)
    | CheckedAST.IndirectApply (func, args) -> CheckedAST.IndirectApply (recurse func, recurseArgs args)
    | CheckedAST.Closure (name, captures) -> CheckedAST.Closure (name, List.map recurse captures)
    | CheckedAST.InterpolatedString parts ->
        CheckedAST.InterpolatedString (
            parts
            |> List.map (function
                | CheckedAST.StringText text -> CheckedAST.StringText text
                | CheckedAST.StringExpr partExpr -> CheckedAST.StringExpr (recurse partExpr))
        )

let private checkedGeneratedFunction (funcDef: AST.FunctionDef) : CheckedAST.FunctionDef =
    match CheckedAST.ofTypedFunction funcDef with
    | Ok checkedFunction -> checkedFunction
    | Error error -> Crash.crash error

let materializeEqHelpersInTopLevelsWithIndexedSums
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (topLevels: CheckedAST.TopLevel list)
    : CheckedAST.TopLevel list =
    let concreteTopLevels =
        topLevels
        |> List.choose (function
            | CheckedAST.FunctionDef functionDef when List.isEmpty functionDef.TypeParams -> Some functionDef.Body
            | CheckedAST.Expression expr -> Some expr
            | CheckedAST.ValueDef valueDef -> Some valueDef.Body
            | CheckedAST.FunctionDef _ | CheckedAST.TypeDef _ -> None)
    let (equalityTypes, compareTypes) =
        concreteTopLevels
        |> List.map (collectHelperTypes aliasReg)
        |> List.fold
            (fun (eqTypes, compareTypes) (nextEq, nextCompare) ->
                (Set.union eqTypes nextEq, Set.union compareTypes nextCompare))
            (Set.empty, Set.empty)
    let equalityState =
        equalityTypes
        |> Set.fold
            (fun state helperType ->
                ensureEqHelperForType aliasReg typeReg variantLookup indexedSumTypeReg helperType state)
            ({ InProgress = Set.empty; Generated = Map.empty }: EqHelperGenerationState)
    let compareState =
        compareTypes
        |> Set.fold
            (fun state helperType ->
                ensureCompareHelperForType aliasReg typeReg variantLookup indexedSumTypeReg helperType state)
            ({ InProgress = Set.empty; Generated = Map.empty }: CompareHelperGenerationState)
    let existingNames =
        topLevels
        |> List.choose (function CheckedAST.FunctionDef functionDef -> Some functionDef.Name | _ -> None)
        |> Set.ofList
    let helpers =
        Map.fold
            (fun generated name helper -> Map.add name helper generated)
            equalityState.Generated
            compareState.Generated
        |> Map.toList
        |> List.map snd
        |> List.filter (fun helper -> not (Set.contains helper.Name existingNames))
        |> List.map (checkedGeneratedFunction >> CheckedAST.FunctionDef)
    let rewritten =
        topLevels
        |> List.map (function
            | CheckedAST.FunctionDef functionDef when List.isEmpty functionDef.TypeParams ->
                CheckedAST.FunctionDef
                    { functionDef with Body = rewriteHelperCalls aliasReg variantLookup functionDef.Body }
            | CheckedAST.Expression expr ->
                CheckedAST.Expression (rewriteHelperCalls aliasReg variantLookup expr)
            | CheckedAST.ValueDef valueDef ->
                CheckedAST.ValueDef
                    { valueDef with Body = rewriteHelperCalls aliasReg variantLookup valueDef.Body }
            | other -> other)
    helpers @ rewritten

let materializeEqHelpersInTopLevels
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (topLevels: CheckedAST.TopLevel list)
    : CheckedAST.TopLevel list =
    materializeEqHelpersInTopLevelsWithIndexedSums
        aliasReg
        typeReg
        variantLookup
        (indexSumTypeRegistry variantLookup)
        topLevels
