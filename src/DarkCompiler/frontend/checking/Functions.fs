// Functions.fs - Check function bodies and collect concrete declaration specializations.

module CheckFunctions

open AST
open CheckingDiagnostics
open CheckingTypes
open TypeUnification
open CheckExpressions

/// Type-check a function definition
/// Returns the transformed function body (with Call -> TypeApp transformations)
let internal checkFunctionDefWithSumTypeNames
    (funcParamNameReg: Map<string, string list>)
    (sumTypeNames: Set<string>)
    (indexedSumTypeReg: IndexedSumTypeRegistry)
    (funcDef: FunctionDef)
    (env: TypeEnv)
    (typeReg: IndexedTypeRegistry)
    (variantLookup: VariantLookup)
    (genericFuncReg: GenericFuncRegistry)
    (warningSettings: WarningSettings)
    (moduleRegistry: ModuleRegistry)
    (aliasReg: AliasRegistry)
    : Result<FunctionDef, TypeError> =
    resetFreshening ()

    let canonicalParams =
        funcDef.Params
        |> NonEmptyList.map (fun (name, typ) ->
            (name,
             typ
             |> canonicalizeDeclaredTypeRefsWithSumTypeNames typeReg sumTypeNames
             |> resolveType aliasReg
             |> canonicalizeBareSumTypeRefsWithNames sumTypeNames))

    let canonicalReturnType =
        funcDef.ReturnType
        |> canonicalizeDeclaredTypeRefsWithSumTypeNames typeReg sumTypeNames
        |> resolveType aliasReg
        |> canonicalizeBareSumTypeRefsWithNames sumTypeNames

    let canonicalFuncDef =
        { funcDef with
            Params = canonicalParams
            ReturnType = canonicalReturnType }

    // Build environment with parameters
    let paramEnv =
        canonicalParams
        |> NonEmptyList.toList
        |> List.fold (fun e (name, ty) -> Map.add name ty e) env

    // Check body has return type
    let bodyCheckResult =
        checkExprWithParamNamesAndSumTypeNames
            funcParamNameReg
            sumTypeNames
            indexedSumTypeReg
            funcDef.Body
            paramEnv
            typeReg
            variantLookup
            genericFuncReg
            warningSettings
            moduleRegistry
            aliasReg
            (Some canonicalReturnType)

    let bodyCheckWithLegacyInterpreterErrors =
        if genericFuncReg.RequireExplicitTypeArgsForBareCalls then
            bodyCheckResult
            |> Result.mapError (fun err ->
                match err with
                | TypeMismatch (expectedType, actualType, _) when
                    typesCompatibleWithAliases aliasReg expectedType canonicalReturnType
                    && not (isRuntimeErrorType actualType) ->
                    let actualValue =
                        match tryFormatLiteralValue funcDef.Body with
                        | Some value -> value
                        | None -> typeToString actualType
                    GenericError
                        $"{funcDef.Name}'s return value expects {typeToString canonicalReturnType}, but got {typeToString actualType} ({actualValue})"
                | _ ->
                    err)
        else
            bodyCheckResult

    bodyCheckWithLegacyInterpreterErrors
    |> Result.bind (fun (bodyType, body') ->
        let resolvedReturnType = resolveType aliasReg canonicalReturnType
        let resolvedBodyType = resolveType aliasReg bodyType
        let allowGenericReturnSpecialization =
            containsTVar resolvedReturnType
            && not (containsTVar resolvedBodyType)
            && typesCompatibleWithAliases aliasReg resolvedReturnType resolvedBodyType

        let rec nominallyIdentical left right =
            match left, right with
            | TSum (leftName, leftArgs), TRecord (rightName, rightArgs)
            | TRecord (leftName, leftArgs), TSum (rightName, rightArgs)
                when leftName = rightName && List.length leftArgs = List.length rightArgs ->
                List.forall2 nominallyIdentical leftArgs rightArgs
            | TSum (leftName, leftArgs), TSum (rightName, rightArgs)
            | TRecord (leftName, leftArgs), TRecord (rightName, rightArgs)
                when leftName = rightName && List.length leftArgs = List.length rightArgs ->
                List.forall2 nominallyIdentical leftArgs rightArgs
            | TFunction (leftParams, leftReturn), TFunction (rightParams, rightReturn)
                when List.length leftParams = List.length rightParams ->
                List.forall2 nominallyIdentical leftParams rightParams
                && nominallyIdentical leftReturn rightReturn
            | TTuple leftTypes, TTuple rightTypes
            | TEnumFields leftTypes, TEnumFields rightTypes
                when List.length leftTypes = List.length rightTypes ->
                List.forall2 nominallyIdentical leftTypes rightTypes
            | TList leftType, TList rightType -> nominallyIdentical leftType rightType
            | TDict (leftKey, leftValue), TDict (rightKey, rightValue) ->
                nominallyIdentical leftKey rightKey
                && nominallyIdentical leftValue rightValue
            | _ -> left = right

        if nominallyIdentical resolvedReturnType resolvedBodyType
           || allowGenericReturnSpecialization then
            let monomorphicType =
                TFunction (
                    canonicalParams |> NonEmptyList.toList |> List.map snd,
                    canonicalReturnType
                )
            let typedRecursion =
                match canonicalFuncDef.Recursion with
                | Some (ResolvedRecursiveBinding resolved) ->
                    Some (TypedRecursiveBinding { Resolved = resolved; MonomorphicType = monomorphicType })
                | Some (TypedRecursiveBinding typed) ->
                    Some (TypedRecursiveBinding { typed with MonomorphicType = monomorphicType })
                | other -> other
            Ok { canonicalFuncDef with Body = body'; Recursion = typedRecursion }
        else
            Error (TypeMismatch (canonicalReturnType, bodyType, $"function {funcDef.Name} body")))

let internal specializeFunctionForTypeCheck
    (funcDef: FunctionDef)
    (typeArgs: Type list)
    : Result<FunctionDef, TypeError> =
    match buildSubstitution funcDef.TypeParams typeArgs with
    | Error msg ->
        Error (GenericError msg)
    | Ok subst ->
        let specializedParams =
            funcDef.Params
            |> NonEmptyList.map (fun (name, typ) -> (name, applySubst subst typ))
        Ok
            { funcDef with
                TypeParams = []
                Params = specializedParams
                ReturnType = applySubst subst funcDef.ReturnType
                Body = applySubstToExpr subst funcDef.Body }

let rec internal collectTypeAppSpecs (expr: Expr) : Set<string * Type list> =
    match expr with
    | BoundaryRender (_, value) -> collectTypeAppSpecs value
    | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _ | Int8Literal _ | Int16Literal _ | Int32Literal _
    | UInt8Literal _ | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
    | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _ | FuncRef _ | Closure _ | RuntimeError _ ->
        Set.empty
    | BinOp (_, left, right) ->
        Set.union (collectTypeAppSpecs left) (collectTypeAppSpecs right)
    | UnaryOp (_, inner) ->
        collectTypeAppSpecs inner
    | Let (_, value, body) ->
        Set.union (collectTypeAppSpecs value) (collectTypeAppSpecs body)
    | RecursiveLet (_, value, body) ->
        Set.union (collectTypeAppSpecs value) (collectTypeAppSpecs body)
    | If (cond, thenBranch, elseBranch) ->
        Set.union (collectTypeAppSpecs cond) (Set.union (collectTypeAppSpecs thenBranch) (collectTypeAppSpecs elseBranch))
    | Sequence (first, next) ->
        Set.union (collectTypeAppSpecs first) (collectTypeAppSpecs next)
    | Call (_, args) ->
        args |> NonEmptyList.toList |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
    | TypeApp (funcName, typeArgs, args) ->
        let argSpecs =
            args |> NonEmptyList.toList |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
        Set.add (funcName, typeArgs) argSpecs
    | TupleLiteral elements ->
        elements |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
    | TupleAccess (tuple, _) ->
        collectTypeAppSpecs tuple
    | DictLiteral (_, entries) ->
        entries |> List.map (snd >> collectTypeAppSpecs) |> List.fold Set.union Set.empty
    | RecordLiteral (_, fields) ->
        fields |> List.map (snd >> collectTypeAppSpecs) |> List.fold Set.union Set.empty
    | RecordUpdate (record, updates) ->
        Set.union
            (collectTypeAppSpecs record)
            (updates |> List.map (snd >> collectTypeAppSpecs) |> List.fold Set.union Set.empty)
    | RecordAccess (record, _) ->
        collectTypeAppSpecs record
    | Constructor (_, _, payload) ->
        payload |> Option.map collectTypeAppSpecs |> Option.defaultValue Set.empty
    | Match (scrutinee, cases) ->
        let scrutineeSpecs = collectTypeAppSpecs scrutinee
        let caseSpecs =
            cases
            |> List.map (fun matchCase ->
                Set.union
                    (matchCase.Guard |> Option.map collectTypeAppSpecs |> Option.defaultValue Set.empty)
                    (collectTypeAppSpecs matchCase.Body))
            |> List.fold Set.union Set.empty
        Set.union scrutineeSpecs caseSpecs
    | ListLiteral elements ->
        elements |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty
    | Lambda (_, _, body) ->
        collectTypeAppSpecs body
    | Apply (funcExpr, args)
    | IndirectApply (funcExpr, args) ->
        Set.union
            (collectTypeAppSpecs funcExpr)
            (args |> NonEmptyList.toList |> List.map collectTypeAppSpecs |> List.fold Set.union Set.empty)
    | InterpolatedString parts ->
        parts
        |> List.choose (fun part ->
            match part with
            | StringText _ -> None
            | StringExpr expr -> Some (collectTypeAppSpecs expr))
        |> List.fold Set.union Set.empty
