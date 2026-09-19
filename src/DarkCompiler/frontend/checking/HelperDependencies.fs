// HelperDependencies.fs - Solve transitive generated comparison-helper dependencies.

module HelperDependencies

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open TypeUnification
open EqualityHelpers
open OrderingHelpers

let private collectDirectEqHelperDeps
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: Type)
    : Type list =
    let addIfHelperType (candidate: Type) : Type option =
        let resolved = candidate |> resolveType aliasReg |> canonicalEqualityType variantLookup
        if needsEqHelperForResolvedType variantLookup resolved then Some resolved else None

    let resolvedType = typ |> resolveType aliasReg |> canonicalEqualityType variantLookup
    let deps =
        match resolvedType with
        | TList elemType ->
            [elemType] |> List.choose addIfHelperType
        | TDict (keyType, valueType) ->
            [TList (TTuple [keyType; valueType])] |> List.choose addIfHelperType
        | TTuple elemTypes ->
            elemTypes |> List.choose addIfHelperType
        | TRecord (recordTypeName, typeArgs) ->
            match Map.tryFind recordTypeName typeReg with
            | None ->
                []
            | Some recordInfo ->
                let fields = recordInfo.Fields
                let concreteFields =
                    match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                    | Ok subst ->
                        fields |> List.map (fun (_, fieldType) -> resolveType aliasReg (applyTypeArguments subst fieldType))
                    | Error _ ->
                        fields |> List.map (fun (_, fieldType) -> resolveType aliasReg fieldType)
                concreteFields |> List.choose addIfHelperType
        | TSum (sumTypeName, sumTypeArgs) ->
            indexedSumTypeReg
            |> Map.tryFind sumTypeName
            |> Option.map (fun info ->
                info.Variants
                |> List.collect (fun variant ->
                    let subst =
                        if List.length info.TypeParams = List.length sumTypeArgs then
                            List.zip info.TypeParams sumTypeArgs |> Map.ofList
                        else
                            Map.empty
                    variant.Fields
                    |> List.choose (applySubst subst >> addIfHelperType)))
            |> Option.defaultValue []
        | _ ->
            []
    deps |> List.distinctBy eqHelperName

type internal EqHelperGenerationState = {
    InProgress: Set<string>
    Generated: Map<string, FunctionDef>
}

let rec internal ensureEqHelperForType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: Type)
    (state: EqHelperGenerationState)
    : EqHelperGenerationState =
    let resolvedType = typ |> resolveType aliasReg |> canonicalEqualityType variantLookup
    let rec referencesKnownNominals candidate =
        match candidate |> resolveType aliasReg |> canonicalEqualityType variantLookup with
        | TRecord (name, typeArgs) ->
            Map.containsKey name typeReg && List.forall referencesKnownNominals typeArgs
        | TSum (name, typeArgs) ->
            Map.containsKey name indexedSumTypeReg
            && List.forall referencesKnownNominals typeArgs
        | TList elementType -> referencesKnownNominals elementType
        | TDict (keyType, valueType) ->
            referencesKnownNominals keyType && referencesKnownNominals valueType
        | TTuple elementTypes -> List.forall referencesKnownNominals elementTypes
        | TFunction (parameterTypes, returnType) ->
            List.forall referencesKnownNominals parameterTypes
            && referencesKnownNominals returnType
        | _ -> true
    if not (needsEqHelperForResolvedType variantLookup resolvedType)
       || not (referencesKnownNominals resolvedType) then
        state
    else
        let helper = eqHelperName resolvedType
        if Map.containsKey helper state.Generated || Set.contains helper state.InProgress then
            state
        else
            let stateInProgress = { state with InProgress = Set.add helper state.InProgress }
            let deps =
                collectDirectEqHelperDeps
                    aliasReg
                    typeReg
                    variantLookup
                    indexedSumTypeReg
                    resolvedType
            let stateWithDeps =
                deps
                |> List.fold
                    (fun currentState depType ->
                        ensureEqHelperForType
                            aliasReg
                            typeReg
                            variantLookup
                            indexedSumTypeReg
                            depType
                            currentState)
                    stateInProgress

            let leftParam = "__dark_eq_left"
            let rightParam = "__dark_eq_right"
            let helperBody =
                buildEqHelperExpr
                    aliasReg
                    typeReg
                    variantLookup
                    indexedSumTypeReg
                    ExpandCurrent
                    resolvedType
                    (Var leftParam)
                    (Var rightParam)

            let helperDef : FunctionDef = {
                Name = helper
                TypeParams = []
                Params = NonEmptyList.fromList [ (leftParam, resolvedType); (rightParam, resolvedType) ]
                ReturnType = TBool
                Body = helperBody
                Recursion = None
            }

            {
                InProgress = Set.remove helper stateWithDeps.InProgress
                Generated = Map.add helper helperDef stateWithDeps.Generated
            }

let private collectDirectCompareHelperDeps
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: Type)
    : Type list =
    let resolvedType = resolveType aliasReg typ
    let deps =
        match resolvedType with
        | TList elemType -> [resolveType aliasReg elemType; resolvedType]
        | TDict (keyType, valueType) ->
            [TList (TTuple [resolveType aliasReg keyType; resolveType aliasReg valueType])]
        | TTuple elemTypes -> elemTypes |> List.map (resolveType aliasReg)
        | TRecord (recordTypeName, typeArgs) ->
            match Map.tryFind recordTypeName typeReg with
            | None -> []
            | Some recordInfo ->
                match buildRecordFieldSubstitutionFromParams recordInfo.TypeParams typeArgs with
                | Ok subst ->
                    recordInfo.Fields
                    |> List.map (fun (_, fieldType) -> resolveType aliasReg (applyTypeArguments subst fieldType))
                | Error _ ->
                    recordInfo.Fields |> List.map (snd >> resolveType aliasReg)
        | TSum (sumTypeName, sumTypeArgs) ->
            indexedSumTypeReg
            |> Map.tryFind sumTypeName
            |> Option.map (fun info ->
                info.Variants
                |> List.collect (fun variant ->
                    let subst =
                        if List.length info.TypeParams = List.length sumTypeArgs then
                            List.zip info.TypeParams sumTypeArgs |> Map.ofList
                        else
                            Map.empty
                    variant.Fields |> List.map (applySubst subst >> resolveType aliasReg)))
            |> Option.defaultValue []
        | _ -> []
    deps |> List.distinctBy compareHelperName

type internal CompareHelperGenerationState = {
    InProgress: Set<string>
    Generated: Map<string, FunctionDef>
}

let rec internal ensureCompareHelperForType
    (aliasReg: AliasRegistry)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (typ: Type)
    (state: CompareHelperGenerationState)
    : CompareHelperGenerationState =
    let resolvedType = resolveType aliasReg typ
    let helper = compareHelperName resolvedType
    if Map.containsKey helper state.Generated
       || Set.contains helper state.InProgress then
        state
    else
        let stateInProgress = { state with InProgress = Set.add helper state.InProgress }
        let stateWithDeps =
            collectDirectCompareHelperDeps
                aliasReg
                typeReg
                variantLookup
                indexedSumTypeReg
                resolvedType
            |> List.fold
                (fun currentState dependency ->
                    ensureCompareHelperForType
                        aliasReg
                        typeReg
                        variantLookup
                        indexedSumTypeReg
                        dependency
                        currentState)
                stateInProgress
        let leftParam = "__dark_compare_left"
        let rightParam = "__dark_compare_right"
        let helperDef : FunctionDef = {
            Name = helper
            TypeParams = []
            Params = NonEmptyList.fromList [leftParam, resolvedType; rightParam, resolvedType]
            ReturnType = TInt64
            Body =
                buildCompareHelperExpr
                    aliasReg
                    typeReg
                    variantLookup
                    indexedSumTypeReg
                    ExpandCurrent
                    resolvedType
                    (Var leftParam)
                    (Var rightParam)
            Recursion = None
        }
        {
            InProgress = Set.remove helper stateWithDeps.InProgress
            Generated = Map.add helper helperDef stateWithDeps.Generated
        }

let rec internal collectCompareHelperTypesFromExpr (aliasReg: AliasRegistry) (expr: Expr) : Set<Type> =
    let collectFromExprs expressions =
        expressions
        |> List.map (collectCompareHelperTypesFromExpr aliasReg)
        |> List.fold Set.union Set.empty
    let recurse = collectCompareHelperTypesFromExpr aliasReg

    match expr with
    | BoundaryRender (_, value) -> recurse value
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ -> Set.empty
    | BinOp (_, left, right) -> Set.union (recurse left) (recurse right)
    | UnaryOp (_, inner) -> recurse inner
    | Let (_, value, body) -> Set.union (recurse value) (recurse body)
    | RecursiveLet (_, value, body) -> Set.union (recurse value) (recurse body)
    | If (condition, thenBranch, elseBranch) ->
        Set.union (recurse condition) (Set.union (recurse thenBranch) (recurse elseBranch))
    | Sequence (first, next) -> Set.union (recurse first) (recurse next)
    | Call (_, args) -> collectFromExprs (NonEmptyList.toList args)
    | TypeApp ("__compare", [targetType], args) ->
        let nested = collectFromExprs (NonEmptyList.toList args)
        let resolvedType = resolveType aliasReg targetType
        if containsTVar resolvedType then nested else Set.add resolvedType nested
    | TypeApp (("Darklang.Stdlib.List.sort" | "Darklang.Stdlib.List.unique"), [valueType], args)
    | TypeApp ("Darklang.Stdlib.List.uniqueBy", [valueType; _], args) ->
        let nested = collectFromExprs (NonEmptyList.toList args)
        let resolvedType = resolveType aliasReg valueType
        if containsTVar resolvedType then nested else Set.add resolvedType nested
    | TypeApp ("Darklang.Stdlib.List.sortBy", [valueType; keyType], args) ->
        let nested = collectFromExprs (NonEmptyList.toList args)
        let pairType = TTuple [resolveType aliasReg keyType; resolveType aliasReg valueType]
        if containsTVar pairType then nested else Set.add pairType nested
    | TypeApp (_, _, args) ->
        collectFromExprs (NonEmptyList.toList args)
    | TupleLiteral elements | ListLiteral elements -> collectFromExprs elements
    | TupleAccess (tupleExpr, _) -> recurse tupleExpr
    | DictLiteral (_, _, entries) ->
        entries |> List.collect (fun (key, value) -> [key; value]) |> collectFromExprs
    | RecordLiteral (_, fields) -> fields |> List.map snd |> collectFromExprs
    | RecordUpdate (recordExpr, updates) ->
        Set.union (recurse recordExpr) (updates |> List.map snd |> collectFromExprs)
    | RecordAccess (recordExpr, _) -> recurse recordExpr
    | Constructor (_, _, fields) -> fields |> List.map recurse |> List.fold Set.union Set.empty
    | Match (scrutinee, cases) ->
        let caseTypes =
            cases
            |> List.map (fun matchCase ->
                let guardTypes = matchCase.Guard |> Option.map recurse |> Option.defaultValue Set.empty
                Set.union guardTypes (recurse matchCase.Body))
            |> List.fold Set.union Set.empty
        Set.union (recurse scrutinee) caseTypes
    | Lambda (_, _, body) -> recurse body
    | Apply (funcExpr, args) | IndirectApply (funcExpr, args) ->
        Set.union (recurse funcExpr) (collectFromExprs (NonEmptyList.toList args))
    | Closure (_, captures) -> collectFromExprs captures
    | InterpolatedString parts ->
        parts
        |> List.choose (function StringText _ -> None | StringExpr partExpr -> Some (recurse partExpr))
        |> List.fold Set.union Set.empty

let rec internal collectEqHelperTypesFromExpr (aliasReg: AliasRegistry) (expr: Expr) : Set<Type> =
    let collectFromExprs (exprs: Expr list) : Set<Type> =
        exprs
        |> List.map (collectEqHelperTypesFromExpr aliasReg)
        |> List.fold Set.union Set.empty

    match expr with
    | BoundaryRender (_, value) -> collectEqHelperTypesFromExpr aliasReg value
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | RuntimeError _ ->
        Set.empty
    | BinOp (_, left, right) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg left) (collectEqHelperTypesFromExpr aliasReg right)
    | UnaryOp (_, inner) ->
        collectEqHelperTypesFromExpr aliasReg inner
    | Let (_, value, body) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg value) (collectEqHelperTypesFromExpr aliasReg body)
    | RecursiveLet (_, value, body) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg value) (collectEqHelperTypesFromExpr aliasReg body)
    | If (cond, thenBranch, elseBranch) ->
        Set.union
            (collectEqHelperTypesFromExpr aliasReg cond)
            (Set.union
                (collectEqHelperTypesFromExpr aliasReg thenBranch)
                (collectEqHelperTypesFromExpr aliasReg elseBranch))
    | Sequence (first, next) ->
        Set.union
            (collectEqHelperTypesFromExpr aliasReg first)
            (collectEqHelperTypesFromExpr aliasReg next)
    | Call (_, args) ->
        collectFromExprs (NonEmptyList.toList args)
    | TypeApp (_, typeArgs, args) as typeAppExpr ->
        match tryDecodeInternalTypeApp typeAppExpr with
        | Some (EqHelperDispatchTypeApp (targetType, leftExpr, rightExpr)) ->
            Set.add
                (resolveType aliasReg targetType)
                (Set.union
                    (collectEqHelperTypesFromExpr aliasReg leftExpr)
                    (collectEqHelperTypesFromExpr aliasReg rightExpr))
        | None ->
            typeArgs
            |> List.map (resolveType aliasReg)
            |> List.filter (containsTVar >> not)
            |> List.fold
                (fun helpers typ -> Set.add typ helpers)
                (collectFromExprs (NonEmptyList.toList args))
    | TupleLiteral elements ->
        collectFromExprs elements
    | TupleAccess (tupleExpr, _) ->
        collectEqHelperTypesFromExpr aliasReg tupleExpr
    | DictLiteral (_, _, entries) ->
        entries |> List.collect (fun (key, value) -> [key; value]) |> collectFromExprs
    | RecordLiteral (_, fields) ->
        fields |> List.map snd |> collectFromExprs
    | RecordUpdate (recordExpr, updates) ->
        Set.union
            (collectEqHelperTypesFromExpr aliasReg recordExpr)
            (updates |> List.map snd |> collectFromExprs)
    | RecordAccess (recordExpr, _) ->
        collectEqHelperTypesFromExpr aliasReg recordExpr
    | Constructor (_, _, fields) ->
        fields
        |> List.map (collectEqHelperTypesFromExpr aliasReg)
        |> List.fold Set.union Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeTypes = collectEqHelperTypesFromExpr aliasReg scrutinee
        let caseTypes =
            cases
            |> List.map (fun matchCase ->
                let guardTypes =
                    matchCase.Guard
                    |> Option.map (collectEqHelperTypesFromExpr aliasReg)
                    |> Option.defaultValue Set.empty
                Set.union guardTypes (collectEqHelperTypesFromExpr aliasReg matchCase.Body))
            |> List.fold Set.union Set.empty
        Set.union scrutineeTypes caseTypes
    | ListLiteral elements ->
        collectFromExprs elements
    | Lambda (_, _, body) ->
        collectEqHelperTypesFromExpr aliasReg body
    | Apply (funcExpr, args)
    | IndirectApply (funcExpr, args) ->
        Set.union (collectEqHelperTypesFromExpr aliasReg funcExpr) (collectFromExprs (NonEmptyList.toList args))
    | Closure (_, captures) ->
        collectFromExprs captures
    | InterpolatedString parts ->
        parts
        |> List.choose (function
            | StringText _ -> None
            | StringExpr partExpr -> Some (collectEqHelperTypesFromExpr aliasReg partExpr))
        |> List.fold Set.union Set.empty
