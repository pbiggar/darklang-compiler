// ANFPipeline.fs - Orchestrate ANF optimization and reference counting.

module ANFPipeline

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open Output
open CompilerOptions
open CompilationSession
open PipelineDiagnostics

let internal buildConversionResult
    (program: ANF.Program)
    (registries: AST_to_ANF.Registries)
    (ownershipContracts: Map<AST.FunctionId, OwnedIR.CallSignature>)
    : AST_to_ANF.ConversionResult =
    let (ANF.Program (functions, _)) = program
    let funcReg =
        AST_to_ANF.extendFunctionRegistryWithConverted registries.FuncReg functions
    {
        Program = program
        OwnershipContracts = ownershipContracts
        RecursiveMembers = registries.RecursiveMembers
        TypeReg = registries.TypeReg
        RecordFieldsReg = registries.RecordFieldsReg
        RecordTypeParamsReg = registries.RecordTypeParamsReg
        VariantLookup = registries.VariantLookup
        RcSumShapeReg = registries.RcSumShapeReg
        FuncReg = funcReg
        FuncParams = registries.FuncParams
        ModuleRegistry = registries.ModuleRegistry
    }

// The stdlib contains enough mutually connected helpers that the general user
// program policy causes excessive compile-time and ANF growth. This policy is
// deliberately limited to shallow, very small ordinary helpers; the other
// specialized inlining modes remain available to user programs.
let internal stdlibInliningConfig : ANF_Inlining.InliningConfig = {
    MaxFunctionSize = 1
    MaxInlineDepth = 1
    MaxExternalInlineSites = 0
    MaxBoundedLoopIterations = 0
    MaxBoundedLoopExpansion = 0
    MaxProjectedTupleInlineSize = 0
    MaxProjectedTupleInlineSites = 0
}

/// Run ANF optimization, construct SSA, and elaborate function ownership on SSA.
let internal buildAnf
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (registries: AST_to_ANF.Registries)
    (inliningConfig: ANF_Inlining.InliningConfig)
    (externalInlineCandidates: Map<AST.FunctionId, ANF_Inlining.FunctionInfo>)
    (externalOptimizationFunctions: Map<string, ANF.Function>)
    (nonInlineableFunctionNames: Set<AST.FunctionId>)
    (functions: ANF.Function list)
    (ownershipContracts: Map<AST.FunctionId, OwnedIR.CallSignature>)
    (specializeInternalSignatures: bool)
    (passTimingRecorder: PassTimingRecorder option)
    : Result<ANF.Function list * SSAANF.Function list * ANF.TypeMap, string> =

    let anfOptions = buildANFOptimizeOptions options
    let anfPassLabel =
        formatPassGroup
            "ANF Optimizations"
            [
                ("const_folding", anfOptions.EnableConstFolding)
                ("const_prop", anfOptions.EnableConstProp)
                ("copy_prop", anfOptions.EnableCopyProp)
                ("dce", anfOptions.EnableDCE)
                ("cse", anfOptions.EnableCSE)
                ("strength_reduction", anfOptions.EnableStrengthReduction)
            ]
    if verbosity >= 1 then println $"  [anf.optimize] {anfPassLabel}..."
    let anfProgram =
        ANF.Program (functions, ANF.Return ANF.UnitLiteral)
        |> ANF_Intrinsics.canonicalizeProgram registries.FunctionIds registries.FuncReg
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram options "=== ANF (before optimization) ===" anfProgram
    let anfOptStart = sw.Elapsed.TotalMilliseconds
    let singletonRecursiveNames =
        registries.RecursiveMembers
        |> Map.toSeq
        |> Seq.choose (fun (name, memberInfo) ->
            match memberInfo.Typed.Resolved.Availability with
            | AST.SelfRecursiveMember -> Some name
            | _ -> None)
        |> Set.ofSeq
    let anfOptimizeContext : ANFConstants.OptimizeContext =
        { TypeReg = registries.RecordFieldsReg
          RecordTypeParams = registries.RecordTypeParamsReg
          SumShapeReg = registries.RcSumShapeReg
          FunctionNames = registries.FunctionNames }
    let anfOptimized =
        if shouldRunANFOptimize anfOptions then
            ANF_Optimize.optimizeProgramWithOptionsAndExternalFunctionsWithTrace
                (passTimingRecorder
                 |> Option.map (fun recorder name elapsed ->
                     recorder { Pass = name; Elapsed = elapsed }))
                anfOptimizeContext
                anfOptions
                singletonRecursiveNames
                externalOptimizationFunctions
                anfProgram
        else
            anfProgram
    let anfOptElapsed = sw.Elapsed.TotalMilliseconds - anfOptStart
    recordPassTiming passTimingRecorder "ANF Optimizations" anfOptElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(anfOptElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram options "=== ANF (after optimization) ===" anfOptimized

    if verbosity >= 1 then println "  [anf.inline] ANF Inlining..."
    let inlineStart = sw.Elapsed.TotalMilliseconds
    let anfInlined =
        if options.DisableInlining then
            anfOptimized
        else
            ANF_Inlining.inlineProgramWithExternalCandidatesAndExclusions
                inliningConfig
                externalInlineCandidates
                nonInlineableFunctionNames
                anfOptimized

    if verbosity >= 1 && specializeInternalSignatures then
        println "  [anf.specialize-closures] ANF Higher-Order Specialization..."
    let higherOrderStart = sw.Elapsed.TotalMilliseconds
    let anfKnownHigherOrder =
        if options.DisableInlining || not specializeInternalSignatures then
            anfInlined
        else
            let externalFunctions =
                externalInlineCandidates
                |> Map.values
                |> Seq.map (fun info -> info.Func)
                |> Seq.toList
            ANF_HigherOrderSpecialization.specializeProgramWithExternalFunctionsAndNames
                registries.FunctionNames
                externalFunctions
                anfInlined
    let higherOrderElapsed = sw.Elapsed.TotalMilliseconds - higherOrderStart
    if specializeInternalSignatures then
        recordPassTiming passTimingRecorder "ANF Higher-Order Specialization" higherOrderElapsed
    if verbosity >= 2 && specializeInternalSignatures then
        let t = System.Math.Round(higherOrderElapsed, 1)
        println $"        {t}ms"

    let inlineElapsed = sw.Elapsed.TotalMilliseconds - inlineStart
    recordPassTiming passTimingRecorder "ANF Inlining" inlineElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(inlineElapsed, 1)
        println $"        {t}ms"

    let convResult = buildConversionResult anfKnownHigherOrder registries ownershipContracts

    let preSpecializationContext = RcTypeFacts.createContext convResult
    let (ANF.Program (preRCFunctions, _)) = anfKnownHigherOrder
    let ssaBeforeSpecializationResult =
        RefCountInsertion.verifyOwnershipContracts
            preSpecializationContext ownershipContracts anfKnownHigherOrder
        |> Result.bind (fun () ->
            preRCFunctions
            |> List.fold (fun result func ->
                result
                |> Result.bind (fun accumulated ->
                    SSAANF.convertFunctionBeforeRC
                        (ANF_to_MIR.maxTempIdInFunction func)
                        preSpecializationContext
                        func
                    |> Result.map (fun ssa ->
                        ssa :: accumulated)))
                (Ok [])
            |> Result.map List.rev)
    match ssaBeforeSpecializationResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok ssaBeforeSpecialization ->
        if verbosity >= 1 && specializeInternalSignatures then
            println "  [ssa.specialize-calls] SSA Direct-Call Specialization..."
        let specializationStart = sw.Elapsed.TotalMilliseconds
        let specialization: SSADirectCallSpecialization.Specialization =
            if options.DisableInlining || not specializeInternalSignatures then
                { Functions = ssaBeforeSpecialization
                  CloneOrigins = Map.empty }
            else
                SSADirectCallSpecialization.specializeProgramWithFunctionNames
                    registries.FunctionNames ssaBeforeSpecialization
        let specializationElapsed = sw.Elapsed.TotalMilliseconds - specializationStart
        if specializeInternalSignatures then
            recordPassTiming
                passTimingRecorder "SSA Direct-Call Specialization" specializationElapsed
        if verbosity >= 2 && specializeInternalSignatures then
            let t = System.Math.Round(specializationElapsed, 1)
            println $"        {t}ms"

        if verbosity >= 1 && not options.DisableANFOpt then
            println "  [ssa.escape-analysis] SSA Escape Analysis..."
        let escapeStart = sw.Elapsed.TotalMilliseconds
        let ssaAfterEscape =
            specialization.Functions
            |> List.map (fun ssa ->
                if options.DisableANFOpt then ssa
                else
                    SSAEscapeAnalysis.optimizeFunction
                        registries.TypeReg
                        registries.RcSumShapeReg
                        ssa)
        let escapeElapsed = sw.Elapsed.TotalMilliseconds - escapeStart
        if not options.DisableANFOpt then
            recordPassTiming passTimingRecorder "SSA Escape Analysis" escapeElapsed
        if verbosity >= 2 && not options.DisableANFOpt then
            let t = System.Math.Round(escapeElapsed, 1)
            println $"        {t}ms"

        let specializedRegistry =
            ssaAfterEscape
            |> List.fold (fun registry func ->
                Map.add
                    func.Id
                    (func.Name,
                     AST.TFunction (
                         func.TypedParams |> List.map (fun parameter -> parameter.Type),
                         func.ReturnType))
                    registry) convResult.FuncReg
        let ctx =
            RcTypeFacts.createContext
                { convResult with FuncReg = specializedRegistry }
        let originalFrontiers =
            preRCFunctions
            |> List.map (fun func ->
                func.Id, RefCountInsertion.ownedDictionaryFrontierParams func)
            |> Map.ofList
        if verbosity >= 1 then println "  [anf.reference-counts] Reference Count Insertion..."
        let rcStart = sw.Elapsed.TotalMilliseconds
        let ssaAfterRC =
            ssaAfterEscape
            |> List.map (fun ssa ->
                let sourceId =
                    Map.tryFind ssa.Id specialization.CloneOrigins
                    |> Option.defaultValue ssa.Id
                let retainedParams =
                    ssa.TypedParams |> List.map (fun parameter -> parameter.Id) |> Set.ofList
                let frontierParams =
                    Map.tryFind sourceId originalFrontiers
                    |> Option.defaultValue Set.empty
                    |> Set.intersect retainedParams
                RcSSARefCountInsertion.insertBlockLocal ctx frontierParams ssa)
        let typeMap =
            ssaAfterRC
            |> List.fold (fun types func ->
                func.FreshValueTypes
                |> Map.fold (fun current id typ -> Map.add id typ current) types)
                Map.empty
        let rcElapsed = sw.Elapsed.TotalMilliseconds - rcStart
        recordPassTiming passTimingRecorder "Reference Count Insertion" rcElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(rcElapsed, 1)
            println $"        {t}ms"
        Ok (preRCFunctions, ssaAfterRC, typeMap)
