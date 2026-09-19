// CheckCalls.fs - Check Call expressions while preserving source diagnostics and order.

module CheckCalls

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open TypeUnification
open CheckExpressionSupport

let internal check (checkExpr: ExpressionChecker) (funcParamNameReg: Map<string, string list>) (indexedSumTypeReg: IndexedSumTypeRegistry) (env: TypeEnv) (typeReg: IndexedTypeRegistry) (variantLookup: VariantLookup) (genericFuncReg: GenericFuncRegistry) (warningSettings: WarningSettings) (moduleRegistry: ModuleRegistry) (aliasReg: AliasRegistry) (expectedType: Type option) (funcName: string) (args: NonEmptyList<Expr>) : Result<Type * Expr, TypeError> =
    // The resolution boundary has already attached the canonical callable
    // identity. Type checking only validates that identity's signature.
    let args = NonEmptyList.toList args
    let unavailableTypeVars =
        let fromEnv =
            env
            |> Map.values
            |> Seq.fold (fun names typ -> collectTypeVarsInType typ names) []
        expectedType
        |> Option.map (fun typ -> collectTypeVarsInType typ fromEnv)
        |> Option.defaultValue fromEnv
        |> Set.ofList
    if isBuiltinUnwrapName funcName then
        match args with
        | [argExpr] ->
            checkExpr argExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
            |> Result.bind (fun (argType, argExpr') ->
                let unwrapTypeResult =
                    match resolveType aliasReg argType with
                    | TSum ("Darklang.Stdlib.Option.Option", [valueType]) -> Ok valueType
                    | TSum ("Darklang.Stdlib.Result.Result", [okType; _]) -> Ok okType
                    | actualType ->
                        Error (GenericError $"Can only unwrap Options and Results, yet got {typeToString actualType}")

                unwrapTypeResult
                |> Result.bind (fun outputType ->
                    // `Option.None |> Builtin.unwrap` and `Result.Error(_) |> Builtin.unwrap`
                    // are guaranteed runtime failures. When unconstrained, their payload type
                    // remains a type variable; normalize to Unit to keep IR monomorphic.
                    let normalizedOutputType =
                        if isKnownFailureConstructorExpr argExpr' then
                            match expectedType with
                            // Bottom-like behavior: if context expects a type, use it.
                            | Some expected -> expected
                            // Unconstrained top-level failures still need a concrete type.
                            | None when containsTVar outputType -> TUnit
                            | None -> outputType
                        else
                            outputType

                    match expectedType with
                    | Some expected ->
                        match reconcileTypes (Some aliasReg) expected normalizedOutputType with
                        | Some reconciledType ->
                            Ok (reconciledType, Call ("Builtin.unwrap", NonEmptyList.singleton argExpr'))
                        | None ->
                            Error (TypeMismatch (expected, normalizedOutputType, $"result of call to {funcName}"))
                    | None ->
                        Ok (normalizedOutputType, Call ("Builtin.unwrap", NonEmptyList.singleton argExpr'))))
        | _ ->
            Error (GenericError $"Function {funcName} expects 1 arguments, got {List.length args}")
    elif isRuntimeFailureName funcName then
        match args with
        | [argExpr] ->
            checkExpr argExpr env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some TString)
            |> Result.bind (fun (_argType, argExpr') ->
                let outputType =
                    match expectedType with
                    | Some (TVar _) -> TUnit
                    | Some expected -> expected
                    | None -> TRuntimeError
                Ok (outputType, Call (funcName, NonEmptyList.singleton argExpr')))
        | _ ->
            Error (GenericError $"Function {funcName} expects 1 arguments, got {List.length args}")
    else
        match tryLookupResolved funcName env with
        | Some (TFunction (origParamTypes, origReturnType), resolvedFuncName) ->
            (
        // Check if this is a generic function.
        match tryLookupResolved resolvedFuncName genericFuncReg.Functions with
        | Some (origTypeParams, _) ->
            // Freshen type params to avoid name clashes with caller's scope
            let (freshTypeParams, renaming) =
                let scopeName =
                    if resolvedFuncName.StartsWith "Darklang.Stdlib." then None
                    else Some resolvedFuncName
                freshenTypeParamsAvoiding scopeName unavailableTypeVars origTypeParams
            let paramTypes = origParamTypes |> List.map (applyTypeVarRenaming renaming)
            let returnType = applyTypeVarRenaming renaming origReturnType
            let typeParams = freshTypeParams
            // Generic function called without explicit type args: infer them
            let numParams = List.length paramTypes
            let args = normalizeNullaryCallArgs numParams args
            let numArgs = List.length args

            if numArgs > numParams then
                Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
            else if numArgs < numParams then
                // Partial application of generic function
                let providedParamTypes = List.take numArgs paramTypes
                let remainingParamTypes = List.skip numArgs paramTypes

                // Type-check the provided arguments
                let rec checkProvidedArgs remaining paramTys accTypes accExprs =
                    match remaining, paramTys with
                    | [], [] -> Ok (List.rev accTypes, List.rev accExprs)
                    | arg :: restArgs, paramT :: restParams ->
                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                        |> Result.bind (fun (argType, arg') ->
                            checkProvidedArgs restArgs restParams (argType :: accTypes) (arg' :: accExprs))
                    | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                checkProvidedArgs args providedParamTypes [] []
                |> Result.bind (fun (argTypes, args') ->
                    // Infer type arguments from provided args (some may remain as TVar)
                    inferTypeArgs typeParams providedParamTypes argTypes (Some returnType) None
                    |> Result.mapError GenericError
                    |> Result.bind (fun inferredTypeArgs ->
                        // Build substitution and compute concrete types for remaining params
                        buildSubstitution typeParams inferredTypeArgs
                        |> Result.mapError GenericError
                        |> Result.bind (fun subst ->
                            let concreteRemainingParamTypes = List.map (applySubst subst) remainingParamTypes
                            let concreteReturnType = applySubst subst returnType
                            let concreteArgs = List.map (applySubstToExpr subst) args'

                            // Create unique parameter names for the remaining parameters
                            let remainingParams = makePartialParams resolvedFuncName concreteRemainingParamTypes

                            // Create the lambda body: TypeApp with all args
                            let allArgs = concreteArgs @ (remainingParams |> List.map (fun (name, _) -> Var name))
                            let lambdaBody = TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs allArgs)

                            // Create the lambda: fun p0 p1 ... -> funcName<types>(providedArgs, p0, p1, ...)
                            let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                            // The resulting type is a function from remaining params to return type
                            let partialType = TFunction (concreteRemainingParamTypes, concreteReturnType)

                            match expectedType with
                            | Some expected when not (typesCompatible expected partialType) ->
                                Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                            | _ -> Ok (partialType, lambdaExpr))))
            else
                // Full application - type-check arguments left-to-right while propagating bindings.
                let rec checkArgsWithBindings remaining remainingParamTypes accTypes accExprs accBindings =
                    match remaining, remainingParamTypes with
                    | [], [] ->
                        Ok (List.rev accTypes, List.rev accExprs)
                    | arg :: restArgs, paramT :: restParams ->
                        consolidateBindings accBindings
                        |> Result.mapError GenericError
                        |> Result.bind (fun bindingMap ->
                            let concreteParamType = applySubst bindingMap paramT
                            checkExpr
                                arg
                                env
                                typeReg
                                variantLookup
                                genericFuncReg
                                warningSettings
                                moduleRegistry
                                aliasReg
                                (Some concreteParamType)
                            |> Result.bind (fun (argType, arg') ->
                                match matchTypes concreteParamType argType with
                                | Ok newBindings ->
                                    let combinedBindings = accBindings @ newBindings
                                    consolidateBindings combinedBindings
                                    |> Result.mapError GenericError
                                    |> Result.bind (fun combinedBindingMap ->
                                        let concreteArgType = applySubst combinedBindingMap argType
                                        checkArgsWithBindings
                                            restArgs
                                            restParams
                                            (concreteArgType :: accTypes)
                                            (arg' :: accExprs)
                                            combinedBindings)
                                | Error msg ->
                                    Error (TypeMismatch (concreteParamType, argType, $"argument to {funcName}: {msg}")))
                        )
                    | _ ->
                        Error (GenericError "Argument count mismatch")

                checkArgsWithBindings args paramTypes [] [] []
                |> Result.bind (fun (argTypes, args') ->
                    // Infer type arguments from parameter types, argument types, and expected return type
                    inferTypeArgs typeParams paramTypes argTypes (Some returnType) expectedType
                    |> Result.mapError GenericError
                    |> Result.bind (fun inferredTypeArgs ->
                        validateCanonicalSortableCall
                            aliasReg
                            typeReg
                            indexedSumTypeReg
                            resolvedFuncName
                            inferredTypeArgs
                        |> Result.bind (fun () ->
                            validateDictKeyCall
                                aliasReg
                                typeReg
                                indexedSumTypeReg
                                resolvedFuncName
                                inferredTypeArgs)
                        |> Result.bind (fun () ->
                            buildSubstitution typeParams inferredTypeArgs
                            |> Result.mapError GenericError)
                        |> Result.bind (fun subst ->
                            let concreteReturnType = applySubst subst returnType
                            // Apply substitution to nested expressions (e.g., inner TypeApp nodes)
                            // This ensures that when empty() returns Dict<k$3, v$4> and we later
                            // infer k$3 -> Int64, v$4 -> Int64, the inner TypeApp gets updated
                            let concreteArgs = List.map (applySubstToExpr subst) args'
                            match expectedType with
                            | Some expected when not (typesCompatible expected concreteReturnType) ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ ->
                                // Transform Call to TypeApp with inferred type arguments (using resolved name)
                                Ok (
                                    concreteReturnType,
                                    TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs concreteArgs)
                                ))))

        | None ->
            // Non-generic function: regular call or partial application
            let numParams = List.length origParamTypes
            let args = normalizeNullaryCallArgs numParams args
            let numArgs = List.length args
            if numArgs > numParams then
                Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
            else if numArgs < numParams then
                // Partial application: type-check provided args, then create lambda for remaining
                let providedParamTypes = List.take numArgs origParamTypes
                let remainingParamTypes = List.skip numArgs origParamTypes

                // Type-check the provided arguments
                let rec checkProvidedArgs remaining paramTys accArgs =
                    match remaining, paramTys with
                    | [], [] -> Ok (List.rev accArgs)
                    | arg :: restArgs, paramT :: restParams ->
                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                        |> Result.bind (fun (argType, arg') ->
                            if typesCompatibleWithAliases aliasReg paramT argType then
                                checkProvidedArgs restArgs restParams (arg' :: accArgs)
                            else
                                Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                    | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                checkProvidedArgs args providedParamTypes []
                |> Result.bind (fun args' ->
                    // Create unique parameter names for the remaining parameters
                    let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                    // Create the lambda body: call the original function with all args (using resolved name)
                    let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                    let lambdaBody = Call (resolvedFuncName, toCallArgs allArgs)

                    // Create the lambda: fun p0 p1 ... -> funcName(providedArgs, p0, p1, ...)
                    let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                    // The resulting type is a function from remaining params to return type
                    let partialType = TFunction (remainingParamTypes, origReturnType)

                    match expectedType with
                    | Some expected when not (typesCompatibleWithAliases aliasReg expected partialType) ->
                        Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                    | _ -> Ok (partialType, lambdaExpr))
            else
                // Check each argument type and collect transformed args
                let rec checkArgsWithTypes remaining paramTys paramIndex accArgs =
                    match remaining, paramTys with
                    | [], [] -> Ok (List.rev accArgs)
                    | arg :: restArgs, paramT :: restParams ->
                        let paramName =
                            paramNameForLegacyError funcParamNameReg resolvedFuncName paramIndex

                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                        |> Result.mapError (fun err ->
                            match err with
                            | TypeMismatch (_, actualType, _) when not (isRuntimeErrorType actualType) ->
                                GenericError
                                    (formatLegacyParamTypeError
                                        funcName
                                        paramIndex
                                        paramName
                                        paramT
                                        actualType
                                        arg)
                            | _ ->
                                err)
                        |> Result.bind (fun (argType, arg') ->
                            if typesCompatibleWithAliases aliasReg paramT argType then
                                checkArgsWithTypes restArgs restParams (paramIndex + 1) (arg' :: accArgs)
                            else
                                Error (
                                    GenericError
                                        (formatLegacyParamTypeError
                                            funcName
                                            paramIndex
                                            paramName
                                            paramT
                                            argType
                                            arg)
                                ))
                    | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                checkArgsWithTypes args origParamTypes 1 []
                |> Result.bind (fun args' ->
                            match expectedType with
                            | Some expected when not (typesCompatibleWithAliases aliasReg expected origReturnType) ->
                                Error (TypeMismatch (expected, origReturnType, $"result of call to {funcName}"))
                            | _ -> Ok (origReturnType, Call (resolvedFuncName, toCallArgs args')))
            )
        | Some (TVar funcTypeVar, resolvedFuncName) ->
            // In public source, higher-order generic parameters may reach call sites
            // before their function shape is concretized (for example in nested List.map).
            // Keep the call typable and let surrounding generic reconciliation specialize it.
            let rec checkArgsWithUnknownCallableType remaining accArgs =
                match remaining with
                | [] -> Ok (List.rev accArgs)
                | arg :: restArgs ->
                    checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg None
                    |> Result.bind (fun (_argType, arg') ->
                        checkArgsWithUnknownCallableType restArgs (arg' :: accArgs))

            checkArgsWithUnknownCallableType args []
            |> Result.map (fun args' ->
                let inferredReturnType =
                    match expectedType with
                    | Some expected -> expected
                    | None -> TVar $"__call_result_{funcTypeVar}"
                (inferredReturnType, Call (resolvedFuncName, toCallArgs args')))
        | Some (other, _) ->
            Error (GenericError $"{funcName} is not a function (has type {typeToString other})")
        | None ->
            // Check if it's a module function (e.g., Stdlib.Int64.add, __raw_get)
            match Stdlib.tryGetFunction moduleRegistry funcName with
            | Some (moduleFunc, resolvedFuncName) ->
                (
            // Freshen type params to avoid name clashes with caller's scope
            let (freshTypeParams, renaming) =
                freshenTypeParamsAvoiding None unavailableTypeVars moduleFunc.TypeParams
            let paramTypes = moduleFunc.ParamTypes |> List.map (applyTypeVarRenaming renaming)
            let returnType = applyTypeVarRenaming renaming moduleFunc.ReturnType
            let typeParams = freshTypeParams
            let numParams = List.length moduleFunc.ParamTypes
            let args = normalizeNullaryCallArgs numParams args
            let numArgs = List.length args
            // Check argument count - allow partial application
            if numArgs > numParams then
                Error (GenericError (formatValueArgumentArityError funcName numParams numArgs))
            else if numArgs < numParams && List.isEmpty typeParams then
                // Partial application of non-generic module function
                let providedParamTypes = List.take numArgs paramTypes
                let remainingParamTypes = List.skip numArgs paramTypes

                // Type-check the provided arguments
                let rec checkProvidedArgs remaining paramTys accArgs =
                    match remaining, paramTys with
                    | [], [] -> Ok (List.rev accArgs)
                    | arg :: restArgs, paramT :: restParams ->
                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                        |> Result.bind (fun (argType, arg') ->
                            if typesEqual aliasReg argType paramT then
                                checkProvidedArgs restArgs restParams (arg' :: accArgs)
                            else
                                Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                    | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                checkProvidedArgs args providedParamTypes []
                |> Result.bind (fun args' ->
                    // Create unique parameter names for the remaining parameters
                    let remainingParams = makePartialParams resolvedFuncName remainingParamTypes

                    // Create the lambda body: call the original function with all args (using resolved name)
                    let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                    let lambdaBody = Call (resolvedFuncName, toCallArgs allArgs)

                    // Create the lambda: fun p0 p1 ... -> funcName(providedArgs, p0, p1, ...)
                    let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                    // The resulting type is a function from remaining params to return type
                    let partialType = TFunction (remainingParamTypes, returnType)

                    match expectedType with
                    | Some expected when not (typesEqual aliasReg expected partialType) ->
                        Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                    | _ -> Ok (partialType, lambdaExpr))
            else if numArgs < numParams then
                // Partial application of generic module function
                let providedParamTypes = List.take numArgs paramTypes
                let remainingParamTypes = List.skip numArgs paramTypes

                // Type-check provided arguments and collect bindings for type inference
                let rec checkArgsAndInfer remaining paramTys accArgs accBindings =
                    match remaining, paramTys with
                    | [], [] -> Ok (List.rev accArgs, accBindings)
                    | arg :: restArgs, paramT :: restParams ->
                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                        |> Result.bind (fun (argType, arg') ->
                            // Match param type against arg type to get type variable bindings
                            match matchTypes paramT argType with
                            | Ok bindings ->
                                checkArgsAndInfer restArgs restParams (arg' :: accArgs) (accBindings @ bindings)
                            | Error msg ->
                                Error (TypeMismatch (paramT, argType, $"argument to {funcName}: {msg}")))
                    | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                checkArgsAndInfer args providedParamTypes [] []
                |> Result.bind (fun (args', bindings) ->
                    // Consolidate bindings and build substitution
                    consolidateBindings bindings
                    |> Result.mapError GenericError
                    |> Result.bind (fun bindingMap ->
                        // Build type arguments list from inferred bindings
                        // For partial application, some type params may not be inferrable yet
                        let inferredTypeArgs =
                            typeParams
                            |> List.map (fun paramName ->
                                match Map.tryFind paramName bindingMap with
                                | Some typ -> typ
                                | None -> TVar paramName)  // Keep as type variable if not inferred

                        // Build full substitution (inferred types only, not type vars)
                        let subst = bindingMap

                        // Apply substitution to remaining param types and return type
                        let concreteRemainingTypes = remainingParamTypes |> List.map (applySubst subst)
                        let concreteReturnType = applySubst subst returnType

                        // Create unique parameter names for the remaining parameters
                        let remainingParams = makePartialParams resolvedFuncName concreteRemainingTypes

                        // Create the lambda body: TypeApp call with all args (using resolved name)
                        let allArgs = args' @ (remainingParams |> List.map (fun (name, _) -> Var name))
                        let lambdaBody = TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs allArgs)

                        // Create the lambda
                        let lambdaExpr = Lambda (toLambdaParams remainingParams, None, lambdaBody)

                        // The resulting type is a function from remaining params to return type
                        let partialType = TFunction (concreteRemainingTypes, concreteReturnType)

                        match expectedType with
                        | Some expected when not (typesCompatible expected partialType) ->
                            Error (TypeMismatch (expected, partialType, $"partial application of {funcName}"))
                        | _ -> Ok (partialType, lambdaExpr)))
            else if not (List.isEmpty typeParams) then
                // Generic module function: infer type arguments from actual argument types
                // Type-check arguments left-to-right while propagating inferred bindings.
                // This lets later args see concrete expectations inferred from earlier args.
                let rec checkArgsWithBindings remaining remainingParamTypes accTypes accExprs accBindings =
                    match remaining, remainingParamTypes with
                    | [], [] ->
                        Ok (List.rev accTypes, List.rev accExprs)
                    | arg :: restArgs, paramT :: restParams ->
                        consolidateBindings accBindings
                        |> Result.mapError GenericError
                        |> Result.bind (fun bindingMap ->
                            let concreteParamType = applySubst bindingMap paramT
                            checkExpr
                                arg
                                env
                                typeReg
                                variantLookup
                                genericFuncReg
                                warningSettings
                                moduleRegistry
                                aliasReg
                                (Some concreteParamType)
                            |> Result.bind (fun (argType, arg') ->
                                match matchTypes concreteParamType argType with
                                | Ok newBindings ->
                                    let combinedBindings = accBindings @ newBindings
                                    consolidateBindings combinedBindings
                                    |> Result.mapError GenericError
                                    |> Result.bind (fun combinedBindingMap ->
                                        let concreteArgType = applySubst combinedBindingMap argType
                                        checkArgsWithBindings
                                            restArgs
                                            restParams
                                            (concreteArgType :: accTypes)
                                            (arg' :: accExprs)
                                            combinedBindings)
                                | Error msg ->
                                    Error (TypeMismatch (concreteParamType, argType, $"argument to {funcName}: {msg}")))
                        )
                    | _ ->
                        Error (GenericError "Argument count mismatch")

                checkArgsWithBindings args paramTypes [] [] []
                |> Result.bind (fun (argTypes, args') ->
                    // Infer type arguments from parameter types, argument types, and expected return type
                    inferTypeArgs typeParams paramTypes argTypes (Some returnType) expectedType
                    |> Result.mapError GenericError
                    |> Result.bind (fun inferredTypeArgs ->
                        // Build substitution and compute concrete types
                        buildSubstitution typeParams inferredTypeArgs
                        |> Result.mapError GenericError
                        |> Result.bind (fun subst ->
                            let concreteReturnType = applySubst subst returnType
                            match expectedType with
                            | Some expected when not (typesCompatible expected concreteReturnType) ->
                                Error (TypeMismatch (expected, concreteReturnType, $"result of call to {funcName}"))
                            | _ ->
                                // Transform Call to TypeApp with inferred type arguments (using resolved name)
                                Ok (
                                    concreteReturnType,
                                    TypeApp (resolvedFuncName, inferredTypeArgs, toCallArgs args')
                                ))))
            else
                // Non-generic module function: regular call
                // Check each argument type and collect transformed args
                let rec checkArgsWithTypes remaining paramTys accArgs =
                    match remaining, paramTys with
                    | [], [] -> Ok (List.rev accArgs)
                    | arg :: restArgs, paramT :: restParams ->
                        checkExpr arg env typeReg variantLookup genericFuncReg warningSettings moduleRegistry aliasReg (Some paramT)
                        |> Result.bind (fun (argType, arg') ->
                            if typesCompatibleWithAliases aliasReg paramT argType then
                                checkArgsWithTypes restArgs restParams (arg' :: accArgs)
                            else
                                Error (TypeMismatch (paramT, argType, $"argument to {funcName}")))
                    | _ -> Error (GenericError "Internal error: argument/param length mismatch")

                checkArgsWithTypes args paramTypes []
                |> Result.bind (fun args' ->
                    match expectedType with
                    | Some expected when not (typesCompatibleWithAliases aliasReg expected returnType) ->
                        Error (TypeMismatch (expected, returnType, $"result of call to {funcName}"))
                    | _ -> Ok (returnType, Call (resolvedFuncName, toCallArgs args')))
                )
            | None ->
                Error (UndefinedCallTarget funcName)
