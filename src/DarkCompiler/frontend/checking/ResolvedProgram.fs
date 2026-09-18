// ResolvedProgram.fs - Check resolved declarations and expressions against explicit environments.

module CheckResolvedProgram

open AST
open CheckingDiagnostics
open CheckingTypes
open ComparisonPlanning
open CheckExpressions
open MaterializeHelpers
open CheckFunctions
open ResolveDeclarations
open CheckDeclarations

/// Internal: Type-check a program and return the type checking environment
/// This is the core implementation used by checkProgram, checkProgramWithEnv, and checkProgramWithBaseEnv
/// When baseEnv is provided, registries are merged with it (for separate compilation)
let internal checkResolvedProgramInternal
    (baseEnv: TypeCheckEnv option)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (requireEntry: bool)
    (program: Program)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    let (Program topLevels) = program
    let declarationSummary = summarizeTopLevelDeclarations topLevels
    let programTypeReg =
        resolveAliasesInTypeRegistry declarationSummary.AliasReg declarationSummary.TypeReg

    let programGenericFuncReg : GenericFuncRegistry = {
        Functions = declarationSummary.GenericFuncs
        RequireExplicitTypeArgsForBareCalls = requireExplicitTypeArgsForBareCalls
    }

    // Build module registry once (or reuse from base environment)
    let moduleRegistry =
        match baseEnv with
        | Some existingEnv -> existingEnv.ModuleRegistry
        | None -> Stdlib.buildModuleRegistry ()

    let programResolutionEnv =
        declarationResolutionEnvironment topLevels moduleRegistry (Option.isNone baseEnv)

    let programSumTypeNames =
        sumTypeNamesFromVariantLookup declarationSummary.VariantLookup
    let availableSumTypeNames =
        match baseEnv with
        | Some existingEnv -> Set.union existingEnv.SumTypeNames programSumTypeNames
        | None -> programSumTypeNames

    // The base environment is already canonical. Canonicalize only this
    // program's declarations, then overlay them on the immutable base instead
    // of mapping and merging the complete base registry again.
    let canonicalProgramVariantLookup =
        declarationSummary.VariantLookup
        |> Map.map (fun _ (typeName, typeParams, tag, payloadType) ->
            (typeName,
             typeParams,
             tag,
             payloadType
             |> Option.map (canonicalizeBareSumTypeRefsWithNames availableSumTypeNames)))

    let canonicalVariantLookup =
        match baseEnv with
        | Some existingEnv ->
            Map.fold
                (fun lookup name variant -> Map.add name variant lookup)
                existingEnv.VariantLookup
                canonicalProgramVariantLookup
        | None -> canonicalProgramVariantLookup

    let programIndexedSumTypeReg =
        indexSumTypeRegistry canonicalProgramVariantLookup

    let canonicalProgramTypeReg =
        programTypeReg
        |> Map.map (fun _ fields ->
            fields
            |> List.map (fun (fieldName, fieldType) ->
                (fieldName,
                 canonicalizeDeclaredTypeRefsWithSumTypeNames
                     programTypeReg
                     availableSumTypeNames
                     fieldType)))

    let programAliasReg =
        declarationSummary.AliasReg
        |> Map.map (fun _ (typeParams, targetType) ->
            (typeParams,
             canonicalizeBareSumTypeRefsWithNames availableSumTypeNames targetType))

    let functionAliasReg =
        match baseEnv with
        | Some existingEnv ->
            Map.fold
                (fun aliases name alias -> Map.add name alias aliases)
                existingEnv.AliasReg
                programAliasReg
        | None -> programAliasReg

    // Function calls must expose the same canonical types as checked function
    // bodies. In particular, interpreter fixtures commonly alias an external
    // recursive sum and then return it from a small wrapper.
    let programFuncEnv =
        declarationSummary.FuncSigs
        |> Map.map (fun _ (paramTypes, returnType) ->
            let canonicalize typ =
                typ
                |> canonicalizeDeclaredTypeRefsWithSumTypeNames
                    canonicalProgramTypeReg
                    availableSumTypeNames
                |> resolveType functionAliasReg
                |> canonicalizeBareSumTypeRefsWithNames availableSumTypeNames
            TFunction (List.map canonicalize paramTypes, canonicalize returnType))

    let programIndexedTypeReg =
        indexTypeRegistry
            canonicalVariantLookup
            declarationSummary.RecordTypeParams
            canonicalProgramTypeReg

    let initialValueFuncEnv =
        match baseEnv with
        | Some existingEnv ->
            Map.fold (fun env name typ -> Map.add name typ env) existingEnv.FuncEnv programFuncEnv
        | None -> programFuncEnv
    let initialValueFuncParamNames =
        match baseEnv with
        | Some existingEnv ->
            Map.fold
                (fun names name parameters -> Map.add name parameters names)
                existingEnv.FuncParamNames
                declarationSummary.FuncParamNames
        | None -> declarationSummary.FuncParamNames
    let initialValueIndexedTypeReg =
        match baseEnv with
        | Some existingEnv ->
            Map.fold
                (fun registry name info -> Map.add name info registry)
                existingEnv.IndexedTypeReg
                programIndexedTypeReg
        | None -> programIndexedTypeReg
    let initialValueIndexedSumTypeReg =
        match baseEnv with
        | Some existingEnv ->
            Map.fold
                (fun registry name info -> Map.add name info registry)
                existingEnv.IndexedSumTypeReg
                programIndexedSumTypeReg
        | None -> programIndexedSumTypeReg
    let initialValueGenericFuncReg =
        match baseEnv with
        | Some existingEnv ->
            {
                Functions =
                    Map.fold
                        (fun functions name typeParams -> Map.add name typeParams functions)
                        existingEnv.GenericFuncReg.Functions
                        programGenericFuncReg.Functions
                RequireExplicitTypeArgsForBareCalls =
                    existingEnv.GenericFuncReg.RequireExplicitTypeArgsForBareCalls
                    || programGenericFuncReg.RequireExplicitTypeArgsForBareCalls
            }
        | None -> programGenericFuncReg
    let initialValues = baseEnv |> Option.map (fun env -> env.Values) |> Option.defaultValue Map.empty
    let checkedValuesResult =
        topLevels
        |> List.fold (fun result topLevel ->
            result
            |> Result.bind (fun (valueFuncEnv, values, checkedDefs) ->
                match topLevel with
                | ValueDef (UncheckedValueDef (name, body)) ->
                    checkExprWithParamNamesAndSumTypeNames
                        initialValueFuncParamNames
                        availableSumTypeNames
                        initialValueIndexedSumTypeReg
                        body
                        valueFuncEnv
                        initialValueIndexedTypeReg
                        canonicalVariantLookup
                        initialValueGenericFuncReg
                        warningSettings
                        moduleRegistry
                        functionAliasReg
                        None
                    |> Result.map (fun (typ, checkedBody) ->
                        (Map.add name typ valueFuncEnv,
                         Map.add name (typ, checkedBody) values,
                         Map.add name (CheckedValueDef (name, typ, checkedBody)) checkedDefs))
                | ValueDef (CheckedValueDef (name, typ, body)) ->
                    Ok (
                        Map.add name typ valueFuncEnv,
                        Map.add name (typ, body) values,
                        Map.add name (CheckedValueDef (name, typ, body)) checkedDefs)
                | _ -> Ok (valueFuncEnv, values, checkedDefs)))
            (Ok (initialValueFuncEnv, initialValues, Map.empty))

    checkedValuesResult
    |> Result.bind (fun (valueFuncEnv, values, checkedValues) ->
    let topLevels =
        topLevels
        |> List.map (function
            | ValueDef valueDef ->
                match Map.tryFind (valueDefName valueDef) checkedValues with
                | Some checkedValue -> ValueDef checkedValue
                | None -> Crash.crash $"Checked value '{valueDefName valueDef}' was not retained"
            | other -> other)

    // Build the type check environment for THIS program
    let programEnv : TypeCheckEnv = {
        TypeReg = canonicalProgramTypeReg
        IndexedTypeReg = programIndexedTypeReg
        RecordTypeNames = programIndexedTypeReg |> Map.keys |> Set.ofSeq
        VariantLookup = canonicalProgramVariantLookup
        IndexedSumTypeReg = programIndexedSumTypeReg
        SumTypeNames = programSumTypeNames
        FuncEnv = valueFuncEnv
        Values = values
        FuncParamNames = declarationSummary.FuncParamNames
        GenericFuncReg = programGenericFuncReg
        // Checked bodies are installed after the function-definition pass.
        GenericFuncDefs = Map.empty
        ModuleRegistry = moduleRegistry
        AliasReg = programAliasReg
        ResolutionEnv = programResolutionEnv
    }

    // Merge with base environment if provided (for separate compilation)
    let typeCheckEnv =
        match baseEnv with
        | Some existingEnv -> mergeTypeCheckEnv existingEnv programEnv
        | None -> programEnv

    // Extract the merged registries for use in type checking
    let variantLookup = typeCheckEnv.VariantLookup
    let typeReg = typeCheckEnv.IndexedTypeReg
    let funcEnv = typeCheckEnv.FuncEnv
    let funcParamNameReg = typeCheckEnv.FuncParamNames
    let genericFuncReg = typeCheckEnv.GenericFuncReg
    let mergedAliasReg = typeCheckEnv.AliasReg
    let sumTypeNames = availableSumTypeNames
    let indexedSumTypeReg = typeCheckEnv.IndexedSumTypeReg

    // Third pass: type check all function definitions and collect transformed top-levels
    // The accumulator contains (type option * TopLevel) pairs where the type is Some for expressions
    let checkTopLevelWithType topLevel =
        match topLevel with
        | FunctionDef funcDef ->
            checkFunctionDefWithSumTypeNames
                funcParamNameReg
                sumTypeNames
                indexedSumTypeReg
                funcDef
                funcEnv
                typeReg
                variantLookup
                genericFuncReg
                warningSettings
                moduleRegistry
                mergedAliasReg
            |> Result.map (fun funcDef' -> (None, FunctionDef funcDef'))
        | TypeDef _ ->
            Ok (None, topLevel)
        | ValueDef _ ->
            Ok (None, topLevel)
        | Expression expr ->
            resetFreshening ()
            checkExprWithParamNamesAndSumTypeNames
                funcParamNameReg
                sumTypeNames
                indexedSumTypeReg
                expr
                funcEnv
                typeReg
                variantLookup
                genericFuncReg
                warningSettings
                moduleRegistry
                mergedAliasReg
                None
            |> Result.map (fun (exprType, expr') -> (Some exprType, Expression expr'))

    let checkAllTopLevelsWithTypes =
        topLevels
        |> List.fold
            (fun result topLevel ->
                result
                |> Result.bind (fun accTopLevels ->
                    checkTopLevelWithType topLevel
                    |> Result.map (fun checkedTopLevel -> checkedTopLevel :: accTopLevels)))
            (Ok [])
        |> Result.map List.rev

    // Type check all top-levels
    checkAllTopLevelsWithTypes
    |> Result.bind (fun topLevelsWithTypes ->
        // Extract just the top-levels
        let topLevels' = topLevelsWithTypes |> List.map snd
        let localGenericFuncDefs =
            topLevels'
            |> List.choose (function
                | FunctionDef funcDef when not (List.isEmpty funcDef.TypeParams) ->
                    Some (funcDef.Name, funcDef)
                | _ ->
                    None)
            |> Map.ofList

        let recursiveGroupsByMember =
            topLevels'
            |> List.choose (function
                | FunctionDef funcDef ->
                    match funcDef.Recursion with
                    | Some (TypedRecursiveBinding typed) ->
                        Some (funcDef.Name, typed.Resolved.Group)
                    | Some (ResolvedRecursiveBinding resolved) ->
                        Some (funcDef.Name, resolved.Group)
                    | _ -> None
                | _ -> None)
            |> Map.ofList

        let validateMonomorphicRecursiveReferences () =
            topLevels'
            |> List.choose (function FunctionDef funcDef -> Some funcDef | _ -> None)
            |> List.fold (fun result funcDef ->
                result
                |> Result.bind (fun () ->
                    match Map.tryFind funcDef.Name recursiveGroupsByMember with
                    | None -> Ok ()
                    | Some currentGroup ->
                        collectTypeAppSpecs funcDef.Body
                        |> Set.toList
                        |> List.tryPick (fun (targetName, typeArgs) ->
                            match Map.tryFind targetName recursiveGroupsByMember,
                                  Map.tryFind targetName localGenericFuncDefs with
                            | Some targetGroup, Some targetDef when targetGroup = currentGroup ->
                                let groupAssumption = targetDef.TypeParams |> List.map TVar
                                if typeArgs = groupAssumption then None
                                else Some targetName
                            | _ -> None)
                        |> function
                            | Some targetName -> Error (PolymorphicRecursion targetName)
                            | None -> Ok ())) (Ok ())

        let checkedGenericFuncDefs =
            Map.fold
                (fun defs name funcDef -> Map.add name funcDef defs)
                typeCheckEnv.GenericFuncDefs
                localGenericFuncDefs

        let localExplicitSpecs =
            topLevels'
            |> List.map (function
                | FunctionDef funcDef when List.isEmpty funcDef.TypeParams ->
                    collectTypeAppSpecs funcDef.Body
                | Expression expr ->
                    collectTypeAppSpecs expr
                | _ ->
                    Set.empty)
            |> List.fold Set.union Set.empty
            |> Set.filter (fun (funcName, _typeArgs) -> Map.containsKey funcName checkedGenericFuncDefs)

        let validateLocalSpecialization (funcName, typeArgs) =
            match Map.tryFind funcName checkedGenericFuncDefs with
            | None ->
                Ok ()
            | Some funcDef ->
                specializeFunctionForTypeCheck funcDef typeArgs
                |> Result.bind (fun specializedFunc ->
                    checkFunctionDefWithSumTypeNames
                        funcParamNameReg
                        sumTypeNames
                        indexedSumTypeReg
                        specializedFunc
                        funcEnv
                        typeReg
                        variantLookup
                        genericFuncReg
                        warningSettings
                        moduleRegistry
                        mergedAliasReg
                    |> Result.map (fun _ -> ()))

        let validateAllSpecializations specs =
            specs
            |> Set.toList
            |> List.fold
                (fun acc spec ->
                    acc |> Result.bind (fun () -> validateLocalSpecialization spec))
                (Ok ())

        validateMonomorphicRecursiveReferences ()
        |> Result.bind (fun () -> validateAllSpecializations localExplicitSpecs)
        |> Result.bind (fun () ->
            let checkedTypeCheckEnv =
                { typeCheckEnv with GenericFuncDefs = checkedGenericFuncDefs }
            let topLevelsWithEqHelpers =
                materializeEqHelpersInTopLevelsWithIndexedSums
                    mergedAliasReg
                    typeReg
                    variantLookup
                    typeCheckEnv.IndexedSumTypeReg
                    topLevels'
            let entryTypes =
                topLevelsWithTypes
                |> List.choose (function (Some typ, Expression _) -> Some typ | _ -> None)
            match requireEntry, entryTypes with
            | true, [typ] -> Ok (typ, Program topLevelsWithEqHelpers, checkedTypeCheckEnv)
            | true, [] -> Error (GenericError "Executable program must contain exactly one entry expression; found 0")
            | true, entries -> Error (GenericError $"Executable program must contain exactly one entry expression; found {entries.Length}")
            | false, [] -> Ok (TUnit, Program topLevelsWithEqHelpers, checkedTypeCheckEnv)
            | false, entries -> Error (GenericError $"Declaration-only program must not contain entry expressions; found {entries.Length}"))))

/// Check the common separate-compilation case without constructing and then
/// merging an empty declaration environment. Name resolution has already run,
/// and concrete generic specializations and equality helpers retain the same
/// validation/materialization path as a general program.
let internal checkResolvedExpressionWithBaseEnv
    (baseEnv: TypeCheckEnv)
    (resolutionEnv: NameResolution.ResolutionEnvironment)
    (requireExplicitTypeArgsForBareCalls: bool)
    (warningSettings: WarningSettings)
    (expr: Expr)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    let genericFuncReg = {
        baseEnv.GenericFuncReg with
            RequireExplicitTypeArgsForBareCalls =
                baseEnv.GenericFuncReg.RequireExplicitTypeArgsForBareCalls
                || requireExplicitTypeArgsForBareCalls
    }
    let sumTypeNames = baseEnv.SumTypeNames

    resetFreshening ()
    checkExprWithParamNamesAndSumTypeNames
        baseEnv.FuncParamNames
        sumTypeNames
        baseEnv.IndexedSumTypeReg
        expr
        baseEnv.FuncEnv
        baseEnv.IndexedTypeReg
        baseEnv.VariantLookup
        genericFuncReg
        warningSettings
        baseEnv.ModuleRegistry
        baseEnv.AliasReg
        None
    |> Result.bind (fun (exprType, typedExpr) ->
        let validateSpecialization (funcName, typeArgs) =
            match Map.tryFind funcName baseEnv.GenericFuncDefs with
            | None ->
                Ok ()
            | Some funcDef ->
                specializeFunctionForTypeCheck funcDef typeArgs
                |> Result.bind (fun specializedFunc ->
                    checkFunctionDefWithSumTypeNames
                        baseEnv.FuncParamNames
                        sumTypeNames
                        baseEnv.IndexedSumTypeReg
                        specializedFunc
                        baseEnv.FuncEnv
                        baseEnv.IndexedTypeReg
                        baseEnv.VariantLookup
                        genericFuncReg
                        warningSettings
                        baseEnv.ModuleRegistry
                        baseEnv.AliasReg
                    |> Result.map (fun _ -> ()))

        typedExpr
        |> collectTypeAppSpecs
        |> Set.filter (fun (funcName, _) -> Map.containsKey funcName baseEnv.GenericFuncDefs)
        |> Set.toList
        |> List.fold
            (fun result spec ->
                result |> Result.bind (fun () -> validateSpecialization spec))
            (Ok ())
        |> Result.map (fun () ->
            let topLevelsWithEqHelpers =
                materializeEqHelpersInTopLevelsWithIndexedSums
                    baseEnv.AliasReg
                    baseEnv.IndexedTypeReg
                    baseEnv.VariantLookup
                    baseEnv.IndexedSumTypeReg
                    [Expression typedExpr]
            let checkedEnv = {
                baseEnv with
                    GenericFuncReg = genericFuncReg
                    ResolutionEnv = resolutionEnv
            }
            (exprType, Program topLevelsWithEqHelpers, checkedEnv)))
