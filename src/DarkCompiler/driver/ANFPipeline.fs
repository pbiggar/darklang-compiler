// ANFPipeline.fs - Orchestrate ANF optimization, reference counting, and tail-call detection.

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

/// Run ANF optimization + RC insertion, returning a final ANF function list and type map
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
    : Result<ANF.Function list * ANF.TypeMap, string> =

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
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
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
          SumShapeReg = registries.RcSumShapeReg }
    let anfOptimized =
        if shouldRunANFOptimize anfOptions then
            ANF_Optimize.optimizeProgramWithOptionsAndExternalFunctions
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
            ANF_HigherOrderSpecialization.specializeProgramWithExternalFunctions
                externalFunctions
                anfInlined
    let higherOrderElapsed = sw.Elapsed.TotalMilliseconds - higherOrderStart
    if specializeInternalSignatures then
        recordPassTiming passTimingRecorder "ANF Higher-Order Specialization" higherOrderElapsed
    if verbosity >= 2 && specializeInternalSignatures then
        let t = System.Math.Round(higherOrderElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 && specializeInternalSignatures then
        println "  [anf.specialize-calls] ANF Direct-Call Specialization..."
    let specializationStart = sw.Elapsed.TotalMilliseconds
    let anfSpecialized =
        if options.DisableInlining || not specializeInternalSignatures then
            anfKnownHigherOrder
        else
            ANF_DirectCallSpecialization.specializeProgram anfKnownHigherOrder
    let specializationElapsed = sw.Elapsed.TotalMilliseconds - specializationStart
    if specializeInternalSignatures then
        recordPassTiming passTimingRecorder "ANF Direct-Call Specialization" specializationElapsed
    if verbosity >= 2 && specializeInternalSignatures then
        let t = System.Math.Round(specializationElapsed, 1)
        println $"        {t}ms"
    let inlineElapsed = sw.Elapsed.TotalMilliseconds - inlineStart
    recordPassTiming passTimingRecorder "ANF Inlining" inlineElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(inlineElapsed, 1)
        println $"        {t}ms"

    if verbosity >= 1 && not options.DisableANFOpt then
        println "  [anf.escape-analysis] ANF Escape Analysis..."
    let escapeAnalysisStart = sw.Elapsed.TotalMilliseconds
    let anfAfterEscapeAnalysis =
        if options.DisableANFOpt then
            anfSpecialized
        else
            ANF_EscapeAnalysis.scalarReplaceProgram registries.TypeReg anfSpecialized
    let escapeAnalysisElapsed = sw.Elapsed.TotalMilliseconds - escapeAnalysisStart
    if not options.DisableANFOpt then
        recordPassTiming passTimingRecorder "ANF Escape Analysis" escapeAnalysisElapsed
    if verbosity >= 2 && not options.DisableANFOpt then
        let t = System.Math.Round(escapeAnalysisElapsed, 1)
        println $"        {t}ms"

    let convResult = buildConversionResult anfAfterEscapeAnalysis registries ownershipContracts

    if verbosity >= 1 then println "  [anf.reference-counts] Reference Count Insertion..."
    let rcStart = sw.Elapsed.TotalMilliseconds
    let rcPhaseRecorder =
        passTimingRecorder
        |> Option.map (fun recorder ->
            fun name (elapsedMs: float) ->
                recorder {
                    Pass = name
                    Elapsed = TimeSpan.FromMilliseconds elapsedMs
                })
    let rcResult =
        RefCountInsertion.insertRCInProgramWithTrace rcPhaseRecorder convResult
    match rcResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok (anfAfterRC, typeMap) ->
        let rcElapsed = sw.Elapsed.TotalMilliseconds - rcStart
        recordPassTiming passTimingRecorder "Reference Count Insertion" rcElapsed
        if verbosity >= 2 then
            let t = System.Math.Round(rcElapsed, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram options "=== ANF (after RC insertion) ===" anfAfterRC

        let (ANF.Program (finalFunctions, _)) = anfAfterRC
        Ok (finalFunctions, typeMap)

/// Run tail call detection on a function list (for post-print insertion TCO)
let internal applyTco
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (recursiveMembers: Map<string, AST.LoweredRecursiveMember>)
    (functions: ANF.Function list)
    (passTimingRecorder: PassTimingRecorder option)
    : ANF.Function list =
    if verbosity >= 1 then println "  [anf.tail-calls] Tail Call Detection..."
    let tcoStart = sw.Elapsed.TotalMilliseconds
    let anfProgram = ANF.Program (functions, ANF.Return ANF.UnitLiteral)
    let anfAfterTCO =
        if options.DisableTCO then
            anfProgram
        else
            TailCallDetection.detectTailCallsInProgramWithRecursion recursiveMembers anfProgram
    let tcoElapsed = sw.Elapsed.TotalMilliseconds - tcoStart
    recordPassTiming passTimingRecorder "Tail Call Detection" tcoElapsed
    if verbosity >= 2 then
        let t = System.Math.Round(tcoElapsed, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram options "=== ANF (after Tail Call Detection) ===" anfAfterTCO
    let (ANF.Program (tcoFunctions, _)) = anfAfterTCO
    tcoFunctions
