// StdlibCompilation.fs - Build reusable standard-library functions and concrete specializations.

module StdlibCompilation

open ARM64CodeGenTypes
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationSession
open NativePipeline
open ANFPipeline
open CompilationContexts
open SourcePreparation
open PreambleAnalysis

/// Load the stdlib and unicode_data.dark files
/// Returns the merged stdlib AST or an error message
let private loadStdlib () : Result<AST.Program, string> =
    let stdlibFiles = [
        "stdlib/Types.dark"
        "stdlib/NoModule.dark"
        "stdlib/Int8.dark"
        "stdlib/Int16.dark"
        "stdlib/Int32.dark"
        "stdlib/Int64.dark"
        "stdlib/__Integer.dark"
        "stdlib/Int.dark"
        "stdlib/Int128.dark"
        "stdlib/UInt8.dark"
        "stdlib/UInt16.dark"
        "stdlib/UInt32.dark"
        "stdlib/UInt64.dark"
        "stdlib/UInt128.dark"
        "stdlib/Bool.dark"
        "stdlib/Builtin.dark"
        "stdlib/Tuple2.dark"
        "stdlib/Tuple3.dark"
        "stdlib/Result.dark"
        "stdlib/Option.dark"
        "stdlib/ListSortByComparatorHelpers.dark"
        "stdlib/List.dark"
        "stdlib/Print.dark"
        "stdlib/Fun.dark"
        "stdlib/Float.dark"
        "stdlib/CliPosix.dark"
        "stdlib/CliPosixMode.dark"
        "stdlib/CliPosixError.dark"
        "stdlib/CliPosixStat.dark"
        "stdlib/Retry.dark"
        "stdlib/CliPath.dark"
        "stdlib/CliFile.dark"
        "unicode_data.dark"
        "unicode_data_index/00.dark"
        "unicode_data_index/01.dark"
        "unicode_data_index/02.dark"
        "unicode_data_index/03.dark"
        "unicode_data_index/04.dark"
        "unicode_data_index/05.dark"
        "unicode_data_index/06.dark"
        "unicode_data/00.dark"
        "unicode_data/01.dark"
        "unicode_data/02.dark"
        "unicode_data/03.dark"
        "unicode_data/04.dark"
        "unicode_data/05.dark"
        "unicode_data/06.dark"
        "unicode_data/07.dark"
        "unicode_data/08.dark"
        "unicode_data/09.dark"
        "unicode_data/10.dark"
        "unicode_data/11.dark"
        "unicode_data/12.dark"
        "unicode_data/13.dark"
        "unicode_data/14.dark"
        "unicode_data/15.dark"
        "unicode_data/16.dark"
        "unicode_data/17.dark"
        "unicode_data/18.dark"
        "unicode_data/19.dark"
        "unicode_data/20.dark"
        "unicode_data/21.dark"
        "unicode_data/22.dark"
        "unicode_data/23.dark"
        "unicode_data/24.dark"
        "unicode_data/25.dark"
        "unicode_data/26.dark"
        "unicode_data/27.dark"
        "unicode_data/28.dark"
        "unicode_data/29.dark"
        "unicode_data/30.dark"
        "unicode_data/31.dark"
        "unicode_data/32.dark"
        "unicode_data/33.dark"
        "unicode_data/34.dark"
        "unicode_data/35.dark"
        "unicode_data/36.dark"
        "unicode_data/37.dark"
        "unicode_data/38.dark"
        "unicode_data/39.dark"
        "unicode_data/40.dark"
        "unicode_data/41.dark"
        "unicode_data/42.dark"
        "unicode_data/43.dark"
        "unicode_data/44.dark"
        "unicode_data/45.dark"
        "unicode_data/46.dark"
        "unicode_data/47.dark"
        "unicode_data/48.dark"
        "unicode_data/49.dark"
        "unicode_data/50.dark"
        "unicode_data/51.dark"
        "unicode_data/52.dark"
        "unicode_data/53.dark"
        "unicode_data/54.dark"
        "unicode_data/55.dark"
        "unicode_data/56.dark"
        "unicode_data/57.dark"
        "unicode_data/58.dark"
        "unicode_data/59.dark"
        "unicode_data/60.dark"
        "unicode_data/61.dark"
        "unicode_data/62.dark"
        "unicode_data/63.dark"
        "stdlib/Unicode.dark"
        "stdlib/String.dark"
        "stdlib/__Hash.dark"
        "stdlib/Dict.dark"
        "stdlib/__HAMT.dark"
        "stdlib/Uuid.dark"
        "stdlib/Diff.dark"
        "stdlib/ProgramTypes.dark"
        "stdlib/RuntimeTypes.dark"
        "stdlib/RuntimeTypesBase.dark"
        "stdlib/RuntimeFQTypeName.dark"
        "stdlib/RuntimeTypeReference.dark"
        "stdlib/PrettyPrinterRuntimeTypes.dark"
        "stdlib/RuntimeValueType.dark"
        "stdlib/RuntimeValueTypeSupport.dark"
        "stdlib/PackageManager.dark"
        "stdlib/PackageManagerPickContext.dark"
        "stdlib/SCMBranch.dark"
        "stdlib/ValueSearch.dark"
        "stdlib/DateTime.dark"
        "stdlib/Duration.dark"
        "stdlib/Blob.dark"
        "stdlib/Stream.dark"
        "stdlib/Html.dark"
        "stdlib/Http.dark"
        "stdlib/HttpRequest.dark"
        "stdlib/HttpClientValues.dark"
        "stdlib/HttpServerValues.dark"
        "stdlib/Pretty.dark"
        "stdlib/Char.dark"
        "stdlib/Regex.dark"
        "stdlib/Base64.dark"
        "stdlib/X509.dark"
        "stdlib/Crypto.dark"
        "stdlib/Math.dark"
        "stdlib/__SkewList.dark"
        "stdlib/__ListArray.dark"
        "stdlib/CliColor.dark"
        "stdlib/CliTextField.dark"
        "stdlib/CliTerminalSession.dark"
        "stdlib/CliLog.dark"
        "stdlib/CliProgress.dark"
        "stdlib/CliPrompt.dark"
        "stdlib/CliSpinner.dark"
        "stdlib/CliTable.dark"
        "stdlib/CliExecution.dark"
        "stdlib/CliOS.dark"
        "stdlib/CliArchitecture.dark"
        "stdlib/CliShell.dark"
        "stdlib/CliHost.dark"
        "stdlib/CliEnv.dark"
        "stdlib/CliArgs.dark"
        "stdlib/CliProcess.dark"
        "stdlib/CliSys.dark"
        "stdlib/CliStdin.dark"
        "stdlib/CliStdinModifiers.dark"
        "stdlib/CliStdinKeyRead.dark"
        "stdlib/CliStdinRead.dark"
        "stdlib/AltJsonParseError.dark"
        "stdlib/AltJson.dark"
        "stdlib/AltJsonHelpers.dark"
        "stdlib/AltJsonBuilder.dark"
        "stdlib/LanguageTools.dark"
        "stdlib/JsonPathPart.dark"
        "stdlib/JsonPath.dark"
        "stdlib/JsonParseError.dark"
        "stdlib/Json.dark"
    ]
    let mergeFile (acc: AST.TopLevel list) (filename: string) : Result<AST.TopLevel list, string> =
        match loadDarkFileAllowInternal filename with
        | Error err -> Error err
        | Ok (AST.Program items) ->
            Ok (acc @ items)
    stdlibFiles
    |> List.fold (fun acc filename -> Result.bind (fun items -> mergeFile items filename) acc) (Ok [])
    |> Result.bind (fun items -> Ok (AST.Program items))

/// Build stdlib in isolation, returning reusable result
/// This can be called once and the result reused for multiple user program compilations
let buildStdlibWithTrace
    (target: Platform.Target)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    match loadStdlib() with
    | Error e ->
        Error e
    | Ok stdlibAst ->
        match TypeChecking.checkDeclarationProgramWithEnv stdlibAst with
        | Error e ->
            let msg = CheckingDiagnostics.typeErrorToString e
            Error msg
        | Ok (_, typedStdlib, typeCheckEnv) ->
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = SpecializationIdentity.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match
                convertTypedDeclarations
                    None
                    (Monomorphize None)
                    typedStdlib
            with
            | Error e ->
                Error e
            | Ok anfResult ->
                let sw = Stopwatch.StartNew()
                let registries = anfResult.Registries
                let returnTypes = extractReturnTypes registries.FuncReg
                let baseFuncNames = buildBaseFuncNames registries
                let context =
                    buildContext
                        target
                        typeCheckEnv
                        (CheckedAST.programValues typedStdlib)
                        genericFuncDefs
                        Map.empty
                        registries
                        baseFuncNames
                        returnTypes
                let stdlibFunctions = anfResult.Functions
                let stdlibOptions = defaultOptions
                match buildAnf 0 stdlibOptions sw registries stdlibInliningConfig Map.empty Set.empty stdlibFunctions false passTimingRecorder with
                | Error e ->
                    Error e
                | Ok (anfFunctions, typeMap) ->
                    let tcoFunctions = applyTco 0 stdlibOptions sw registries.RecursiveMembers anfFunctions passTimingRecorder
                    let stdlibFuncMap =
                        tcoFunctions
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    let stdlibInlineCandidates =
                        stdlibFunctions
                        |> ANF_Inlining.buildExternalCandidateInfoMap ANF_Inlining.defaultConfig
                    let stdlibLiftedFuncNames =
                        tcoFunctions
                        |> List.map (fun f -> f.Name)
                        |> Set.ofList
                    let baseFuncNames =
                        Set.union context.BaseFuncNames stdlibLiftedFuncNames
                    let contextWithLiftedNames = {
                        context with
                            BaseFuncNames = baseFuncNames
                            LambdaLiftFuncParams =
                                reserveBaseFunctionParams
                                    context.Registries.FuncParams
                                    baseFuncNames
                    }
                    let stdlibANFCallGraph = ANFDeadCodeElimination.buildCallGraph tcoFunctions

                    let externalReturnTypes = returnTypes
                    match lowerToAllocatedLir
                        target
                        0
                        stdlibOptions
                        sw
                        passTimingRecorder
                        None
                        None
                        "stdlib"
                        tcoFunctions
                        typeMap
                        registries
                        None
                        externalReturnTypes with
                    | Error e ->
                        Error e
                    | Ok allocatedFuncs ->
                        let stdlibCallGraph = DeadCodeElimination.buildCallGraph allocatedFuncs
                        Ok {
                            AST = stdlibAst
                            TypedAST = typedStdlib
                            Context = contextWithLiftedNames
                            AllocatedFunctions = allocatedFuncs
                            StdlibCallGraph = stdlibCallGraph
                            StdlibANFFunctions = stdlibFuncMap
                            StdlibInlineCandidates = stdlibInlineCandidates
                            StdlibANFCallGraph = stdlibANFCallGraph
                            StdlibTypeMap = typeMap
                        }

/// Build stdlib in isolation with default settings
let buildStdlib (target: Platform.Target) : Result<StdlibResult, string> =
    buildStdlibWithTrace target None

/// Build stdlib specializations for a spec set and merge them into the stdlib result
let buildStdlibSpecializations
    (stdlib: StdlibResult)
    (specs: Set<SpecializationIdentity.SpecKey>)
    (externalTypeReg: TypeRegistries.TypeRegistry)
    (externalVariantLookup: LoweringPrimitives.VariantLookup)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult, string> =
    if Set.isEmpty specs then
        Ok stdlib
    else
        let materializationVariantLookup =
            Map.fold
                (fun acc name variant -> Map.add name variant acc)
                stdlib.Context.TypeCheckEnv.VariantLookup
                externalVariantLookup
        let externalIndexedTypeReg =
            CheckingTypes.indexTypeRegistry
                materializationVariantLookup
                (TypeRegistries.recordTypeParamsRegistry externalTypeReg)
                (TypeRegistries.recordFieldsRegistry externalTypeReg)
        let materializationTypeReg =
            Map.fold
                (fun acc name typeInfo -> Map.add name typeInfo acc)
                stdlib.Context.TypeCheckEnv.IndexedTypeReg
                externalIndexedTypeReg
        let specialization = Monomorphization.specializeFromSpecs stdlib.Context.GenericFuncDefs specs
        let initialCombinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry
        let existingNames =
            stdlib.StdlibANFFunctions
            |> Map.keys
            |> Set.ofSeq
        let newSpecializedFuncs =
            specialization.SpecializedFuncs
            |> List.filter (fun f -> not (Set.contains f.Name existingNames))

        if List.isEmpty newSpecializedFuncs then
            let updatedContext = { stdlib.Context with SpecRegistry = initialCombinedSpecRegistry }
            Ok {
                stdlib with
                    Context = updatedContext
            }
        else
            AST_to_ANF.splitDeclarations stdlib.TypedAST
            |> Result.bind (fun (typeDefs, _functions) ->
                let initiallyMaterializedFunctions =
                    newSpecializedFuncs
                    |> List.collect (fun funcDef ->
                        [CheckedAST.FunctionDef funcDef]
                        |> CheckedMaterializeHelpers.materializeEqHelpersInTopLevels
                            stdlib.Context.TypeCheckEnv.AliasReg
                            materializationTypeReg
                            materializationVariantLookup)
                    |> List.choose (function
                        | CheckedAST.FunctionDef funcDef -> Some funcDef
                        | _ -> None)
                let helperSpecs =
                    initiallyMaterializedFunctions
                    |> List.map Monomorphization.collectTypeAppsFromFunc
                    |> List.fold Set.union Set.empty
                    |> Set.filter (fun (funcName, _) ->
                        Map.containsKey funcName stdlib.Context.GenericFuncDefs)
                let helperSpecialization =
                    Monomorphization.specializeFromSpecs stdlib.Context.GenericFuncDefs helperSpecs
                let combinedSpecRegistry =
                    mergeSpecRegistries initialCombinedSpecRegistry helperSpecialization.SpecRegistry
                let materializedFunctions =
                    (helperSpecialization.SpecializedFuncs @ initiallyMaterializedFunctions)
                    |> List.filter (fun f -> not (Set.contains f.Name existingNames))
                    |> List.map CheckedAST.FunctionDef
                    |> CheckedMaterializeHelpers.materializeEqHelpersInTopLevels
                        stdlib.Context.TypeCheckEnv.AliasReg
                        materializationTypeReg
                        materializationVariantLookup
                    |> List.choose (function
                        | CheckedAST.FunctionDef funcDef -> Some funcDef
                        | _ -> None)
                    |> List.distinctBy (fun funcDef -> funcDef.Name)
                let specializationProgram =
                    CheckedAST.Program (
                        (typeDefs |> List.map CheckedAST.TypeDef)
                        @ (materializedFunctions |> List.map CheckedAST.FunctionDef)
                    )
                prepareProgramForAnf
                    (ReplaceTypeApps combinedSpecRegistry)
                    stdlib.Context.LambdaLiftTypeReg
                    stdlib.Context.LambdaLiftVariantLookup
                    stdlib.Context.BaseFuncNames
                    stdlib.Context.LambdaLiftFuncParams
                    stdlib.Context.ReturnTypes
                    stdlib.Context.CheckedValues
                    passTimingRecorder
                    specializationProgram
                |> Result.bind AST_to_ANF.splitDeclarations
                |> Result.bind (fun (preparedTypeDefs, preparedFunctions) ->
                    let (registries, localRegistries, resolvedFunctions) =
                        buildRegistriesForProgram
                            true
                            stdlib.Context.Registries.ModuleRegistry
                            stdlib.Context.Registries
                            preparedTypeDefs
                            preparedFunctions
                    let registries = {
                        registries with
                            TypeReg =
                                Map.fold
                                    (fun acc name recordInfo -> Map.add name recordInfo acc)
                                    registries.TypeReg
                                    externalTypeReg
                            VariantLookup =
                                Map.fold (fun acc k v -> Map.add k v acc) registries.VariantLookup externalVariantLookup
                            SumTypeNames =
                                Set.union
                                    registries.SumTypeNames
                                    (LoweringPrimitives.sumTypeNamesFromVariantLookup externalVariantLookup)
                            RcSumShapeReg =
                                Map.fold
                                    (fun acc name shape -> Map.add name shape acc)
                                    registries.RcSumShapeReg
                                    (TypeRegistries.rcSumShapeRegistryFromVariantLookup externalVariantLookup)
                    }
                    let localReturnTypes = extractReturnTypes localRegistries.FuncReg
                    let varGen = ANF.VarGen 0
                    AST_to_ANF.convertFunctions registries varGen resolvedFunctions
                    |> Result.bind (fun (anfFuncs, _varGen1) ->
                        let stdlibOptions = defaultOptions
                        let sw = Stopwatch.StartNew()
                        buildAnf 0 stdlibOptions sw registries stdlibInliningConfig Map.empty Set.empty anfFuncs false passTimingRecorder
                        |> Result.bind (fun (anfFunctions, typeMap) ->
                            let tcoFunctions = applyTco 0 stdlibOptions sw registries.RecursiveMembers anfFunctions passTimingRecorder
                            let newAnfFuncMap =
                                tcoFunctions
                                |> List.map (fun f -> f.Name, f)
                                |> Map.ofList
                            let externalReturnTypes =
                                mergeReturnTypes stdlib.Context.ReturnTypes localReturnTypes
                            lowerToAllocatedLir
                                stdlib.Context.Target
                                0
                                stdlibOptions
                                sw
                                passTimingRecorder
                                None
                                None
                                "stdlib_specializations"
                                tcoFunctions
                                typeMap
                                registries
                                None
                                externalReturnTypes
                            |> Result.bind (fun allocatedFuncs ->
                                let allLirFuncs = stdlib.AllocatedFunctions @ allocatedFuncs
                                let mergedStdlibTypeMap =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap
                                let mergedStdlibAnfFunctions =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibANFFunctions newAnfFuncMap
                                let newInlineCandidateMap =
                                    anfFuncs
                                    |> ANF_Inlining.buildExternalCandidateInfoMap ANF_Inlining.defaultConfig
                                let mergedStdlibInlineCandidates =
                                    Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibInlineCandidates newInlineCandidateMap
                                let allAnfFunctions =
                                    mergedStdlibAnfFunctions
                                    |> Map.toList
                                    |> List.map snd
                                let stdlibCallGraph = DeadCodeElimination.buildCallGraph allLirFuncs
                                let stdlibAnfCallGraph = ANFDeadCodeElimination.buildCallGraph allAnfFunctions
                                let baseFuncNames =
                                    tcoFunctions
                                    |> List.fold
                                        (fun names func -> Set.add func.Name names)
                                        stdlib.Context.BaseFuncNames
                                let lambdaLiftFuncParams =
                                    reserveBaseFunctionParams registries.FuncParams baseFuncNames
                                let (lambdaLiftTypeReg, lambdaLiftVariantLookup) =
                                    LiftFunctions.prepareLambdaLiftBaseTypes
                                        registries.TypeReg
                                        registries.VariantLookup
                                let updatedContext = {
                                    stdlib.Context with
                                        Registries = registries
                                        SpecRegistry = combinedSpecRegistry
                                        BaseFuncNames = baseFuncNames
                                        LambdaLiftFuncParams = lambdaLiftFuncParams
                                        LambdaLiftTypeReg = lambdaLiftTypeReg
                                        LambdaLiftVariantLookup = lambdaLiftVariantLookup
                                        ReturnTypes = externalReturnTypes
                                }
                                Ok {
                                    stdlib with
                                        Context = updatedContext
                                        AllocatedFunctions = allLirFuncs
                                        StdlibCallGraph = stdlibCallGraph
                                        StdlibANFFunctions = mergedStdlibAnfFunctions
                                        StdlibInlineCandidates = mergedStdlibInlineCandidates
                                        StdlibANFCallGraph = stdlibAnfCallGraph
                                        StdlibTypeMap = mergedStdlibTypeMap
                                }
                            )
                        )
                    )
                )
            )
