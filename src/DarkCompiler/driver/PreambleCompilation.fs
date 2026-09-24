// PreambleCompilation.fs - Compile reusable preamble contexts and their dependencies.

module PreambleCompilation

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
open PackageCatalog

/// Build preamble with stdlib as base, returning extended context for test compilation
/// Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO)
/// The result is built once per file and reused for all tests in that file
let buildPreambleContext
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    (sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult * PreambleContext, string> =
    // Handle empty preamble - return a context that just wraps stdlib
    if String.IsNullOrWhiteSpace(preamble) then
        let emptyContext = {
            Context = stdlib.Context
            ANFFunctions = []
            TypeMap = stdlib.StdlibTypeMap
            SymbolicFunctions = []
            SymbolicCallGraph = Map.empty
        }
        Ok (stdlib, emptyContext)
    else
    match Parser.parseString allowInternal preamble with
        | Error err ->
            let msg = $"Preamble parse error: {err}"
            Error msg
        | Ok preambleAst ->
            // Type-check preamble with stdlib context
            match TypeChecking.checkParsedDeclarationProgramWithBaseEnv stdlib.Context.TypeCheckEnv preambleAst with
            | Error typeErr ->
                let msg = $"Preamble type error: {CheckingDiagnostics.typeErrorToString typeErr}"
                Error msg
            | Ok (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
                // Extract generic function definitions from preamble
                let preambleGenericDefs = SpecializationIdentity.extractGenericFuncDefs typedPreambleAst
                // Merge stdlib generics with preamble generics
                let mergedGenericDefs = Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs preambleGenericDefs

                // Convert preamble to ANF (mono → inline → lift → ANF)
                match
                    convertTypedDeclarationsWithTrace
                        passTimingRecorder
                        (Some stdlib.Context)
                        (Monomorphize (Some stdlib.Context.GenericFuncDefs))
                        typedPreambleAst
                with
                | Error err ->
                    let msg = $"Preamble ANF conversion error: {err}"
                    Error msg
                | Ok preambleUserOnly ->
                    let preambleRegistries = preambleUserOnly.Registries
                    let preambleOptions = defaultOptions
                    let sw = Stopwatch.StartNew()
                    let preambleReturnTypes =
                        mergeReturnTypes stdlib.Context.ReturnTypes preambleUserOnly.LocalReturnTypes
                    let baseFuncNames =
                        preambleUserOnly.Functions
                        |> List.fold
                            (fun names func -> Set.add func.Name names)
                            stdlib.Context.BaseFuncNames
                    let pipelineContext =
                        let checkedValues =
                            checkedValueArtifacts typedPreambleAst
                            |> Map.fold (fun values name value -> Map.add name value values) stdlib.Context.CheckedValues
                        buildContext
                            stdlib.Context.Target
                            preambleUserOnly.Symbols
                            preambleTypeCheckEnv
                            checkedValues
                            mergedGenericDefs
                            Map.empty
                            preambleRegistries
                            baseFuncNames
                            preambleReturnTypes
                    match buildAnf 0 preambleOptions sw preambleRegistries ANF_Inlining.defaultConfig Map.empty Map.empty Set.empty preambleUserOnly.Functions Map.empty false passTimingRecorder with
                    | Error err ->
                        let rcPrefix = "Reference count insertion error: "
                        let msg =
                            if err.StartsWith(rcPrefix) then
                                let suffix = err.Substring(rcPrefix.Length)
                                $"Preamble RC insertion error: {suffix}"
                            else
                                $"Preamble {err}"
                        Error msg
                    | Ok (preambleFunctions, typeMap) ->
                        let tcoFunctions = applyTco 0 preambleOptions sw preambleRegistries.RecursiveMembers preambleFunctions passTimingRecorder
                        let preambleExternalReturnTypes = preambleReturnTypes
                        match lowerToAllocatedLir
                            stdlib.Context.Target
                            0
                            preambleOptions
                            sw
                            passTimingRecorder
                            None
                            None
                            "preamble"
                            tcoFunctions
                            typeMap
                            preambleRegistries
                            None
                            preambleExternalReturnTypes with
                        | Error err ->
                            let msg = $"Preamble {err}"
                            Error msg
                        | Ok allocatedFuncs ->
                            let stdlibFuncNames =
                                stdlib.AllocatedFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let isStdlibFunction (name: string) : bool =
                                Set.contains name stdlibFuncNames
                            let preambleOnlyFuncs =
                                allocatedFuncs
                                |> List.filter (fun func -> not (isStdlibFunction func.Name))
                            let preambleSymbolicFuncs = preambleOnlyFuncs
                            let preambleLiftedFuncNames =
                                tcoFunctions
                                |> List.map (fun func -> func.Name)
                                |> Set.ofList
                            let baseFuncNames =
                                Set.union pipelineContext.BaseFuncNames preambleLiftedFuncNames
                            let pipelineContextWithLiftedNames = {
                                pipelineContext with
                                    BaseFuncNames = baseFuncNames
                                    LambdaLiftFunctions =
                                        buildLambdaLiftFunctionCatalog
                                            pipelineContext.Registries
                                            baseFuncNames
                                            pipelineContext.ReturnTypes
                            }

                            // Merge TypeMaps (stdlib + preamble)
                            let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                            let context = {
                                Context = pipelineContextWithLiftedNames
                                ANFFunctions = tcoFunctions
                                TypeMap = mergedTypeMap
                                SymbolicFunctions = preambleSymbolicFuncs
                                SymbolicCallGraph = DeadCodeElimination.buildCallGraph preambleSymbolicFuncs
                            }
                            Ok (stdlib, context)

/// Build preamble context from a typed preamble analysis and precomputed specializations
let buildPreambleContextFromAnalysis
    (stdlib: StdlibResult)
    (analysis: PreambleAnalysis)
    (specialization: SpecializationIdentity.SpecializationResult)
    (sourceFile: string)
    (_funcLineMap: Map<string, int>)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<StdlibResult * PreambleContext, string> =
    let combinedSpecRegistry = mergeSpecRegistries stdlib.Context.SpecRegistry specialization.SpecRegistry

    let mergedGenericDefs =
        Map.fold (fun acc k v -> Map.add k v acc) stdlib.Context.GenericFuncDefs analysis.GenericFuncDefs

    let (CheckedAST.Program (symbols, items)) = analysis.TypedAST
    let symbols, specializedFunctions =
        SpecializationIdentity.importSpecializedFunctions symbols specialization.SpecializedFuncs
    let specializedTopLevels = specializedFunctions |> List.map CheckedAST.FunctionDef
    let specializedAndOriginalTopLevels = specializedTopLevels @ items
    let symbols, materializedTopLevels =
        CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums
            symbols
            analysis.TypeCheckEnv.AliasReg
            analysis.TypeCheckEnv.IndexedTypeReg
            analysis.TypeCheckEnv.VariantLookup
            analysis.TypeCheckEnv.IndexedSumTypeReg
            specializedAndOriginalTopLevels
    let programWithSpecializations = CheckedAST.Program (symbols, materializedTopLevels)

    convertTypedDeclarationsWithTrace
        passTimingRecorder
        (Some stdlib.Context)
        (ReplaceTypeApps combinedSpecRegistry)
        programWithSpecializations
    |> Result.bind (fun preambleUserOnly ->
        let preambleRegistries = preambleUserOnly.Registries
        let preambleOptions = defaultOptions
        let sw = Stopwatch.StartNew()
        let preambleReturnTypes =
            mergeReturnTypes stdlib.Context.ReturnTypes preambleUserOnly.LocalReturnTypes
        let baseFuncNames =
            preambleUserOnly.Functions
            |> List.fold
                (fun names func -> Set.add func.Name names)
                stdlib.Context.BaseFuncNames
        let pipelineContext =
            let checkedValues =
                checkedValueArtifacts analysis.TypedAST
                |> Map.fold (fun values name value -> Map.add name value values) stdlib.Context.CheckedValues
            buildContext
                stdlib.Context.Target
                preambleUserOnly.Symbols
                analysis.TypeCheckEnv
                checkedValues
                mergedGenericDefs
                combinedSpecRegistry
                preambleRegistries
                baseFuncNames
                preambleReturnTypes
        match buildAnf 0 preambleOptions sw preambleRegistries ANF_Inlining.defaultConfig Map.empty Map.empty Set.empty preambleUserOnly.Functions Map.empty false passTimingRecorder with
        | Error err ->
            let rcPrefix = "Reference count insertion error: "
            let msg =
                if err.StartsWith(rcPrefix) then
                    let suffix = err.Substring(rcPrefix.Length)
                    $"Preamble RC insertion error: {suffix}"
                else
                    $"Preamble {err}"
            Error msg
        | Ok (preambleFunctions, typeMap) ->
            let tcoFunctions = applyTco 0 preambleOptions sw preambleRegistries.RecursiveMembers preambleFunctions passTimingRecorder
            let preambleExternalReturnTypes = preambleReturnTypes
            match lowerToAllocatedLir
                stdlib.Context.Target
                0
                preambleOptions
                sw
                passTimingRecorder
                None
                None
                "preamble"
                tcoFunctions
                typeMap
                preambleRegistries
                None
                preambleExternalReturnTypes with
            | Error err ->
                let msg = $"Preamble {err}"
                Error msg
            | Ok allocatedFuncs ->
                let stdlibFuncNames =
                    stdlib.AllocatedFunctions
                    |> List.map (fun func -> func.Name)
                    |> Set.ofList
                let isStdlibFunction (name: string) : bool =
                    Set.contains name stdlibFuncNames
                let preambleOnlyFuncs =
                    allocatedFuncs
                    |> List.filter (fun func -> not (isStdlibFunction func.Name))
                let preambleSymbolicFuncs = preambleOnlyFuncs

                let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                Ok (stdlib, {
                    Context = pipelineContext
                    ANFFunctions = tcoFunctions
                    TypeMap = mergedTypeMap
                    SymbolicFunctions = preambleSymbolicFuncs
                    SymbolicCallGraph = DeadCodeElimination.buildCallGraph preambleSymbolicFuncs
                }))
