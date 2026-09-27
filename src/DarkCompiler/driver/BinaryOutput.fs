// BinaryOutput.fs - Select a validated target backend and assemble executable output.

module BinaryOutput

open ARM64CodeGenTypes
open ARM64GenericReferenceCounts
open CodeGen
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open Output
open CompilerOptions
open CompilationCacheIdentity
open CompilationSession
open PipelineDiagnostics

/// Run codegen, encoding, and binary generation
let internal generateBinary
    (target: Platform.Target)
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (passTimingRecorder: PassTimingRecorder option)
    (codegenLabel: string)
    (emitLabel: string)
    (dumpAsm: bool)
    (dumpMachineCode: bool)
    (session: CompilationSession option)
    (programContextIdentity: obj)
    (functionGroups: CodeGen.FunctionGroup list)
    (metadataGroups: CodeGen.MetadataGroup list)
    (arm64SumShapeRegistry: MemoryModel.RcSumShapeRegistry)
    (allocatedProgram: LIR.Program)
    : Result<byte array, string> =

    match target with
    | Platform.LinuxX86_64 ->
        // x86-64 backend
        if verbosity >= 1 then println codegenLabel
        let codegenStart = sw.Elapsed.TotalMilliseconds
        let codegenResult = CodeGen_X86_64.translateProgram allocatedProgram options.EnableLeakCheck
        match codegenResult with
        | Error err -> Error $"x86-64 code generation error: {err}"
        | Ok x86Instructions ->
            let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
            recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(codegenElapsed, 1)
                println $"        {t}ms"

            if dumpAsm && verbosity >= 3 then
                println "=== x86-64 Assembly Instructions ==="
                for (i, instr) in List.indexed x86Instructions do
                    println $"  {i}: {instr}"
                println ""

            if verbosity >= 1 then println (emitLabel.Replace("{format}", "ELF"))
            let emitStart = sw.Elapsed.TotalMilliseconds
            let x64StaticStringPool = X86_64_Resolve.collectStringPool x86Instructions
            match X86_64_Resolve.resolveAndEncode x86Instructions with
            | Error err -> Error $"x86-64 resolve error: {err}"
            | Ok resolveResult ->
                // Patch data labels (e.g., leak counter) if there are deferred fixups
                let patchedResult =
                    if List.isEmpty resolveResult.DeferredFixups then
                        Ok resolveResult
                    else
                        let elfHeaderSize = 64
                        let programHeaderSize = 56
                        let codeFileOffset = elfHeaderSize + programHeaderSize
                        let codeSize = resolveResult.MachineCode.Length
                        let dataLabels =
                            X86_64_Resolve.dataLabelOffsets codeFileOffset codeSize x64StaticStringPool
                        X86_64_Resolve.patchDataLabels resolveResult dataLabels codeFileOffset
                match patchedResult with
                | Error err -> Error $"x86-64 data label error: {err}"
                | Ok resolveResult ->
                    match X86_64_Resolve.requireLabelPosition "_start" resolveResult.LabelPositions with
                    | Error err -> Error $"x86-64 resolve error: {err}"
                    | Ok entryOffset ->
                        let binary =
                            Binary_Generation_ELF_X86_64.createExecutableWithPools
                                resolveResult.MachineCode x64StaticStringPool LiteralPool.emptyFloatPool
                                options.EnableLeakCheck entryOffset
                        let emitElapsed = sw.Elapsed.TotalMilliseconds - emitStart
                        recordPassTiming passTimingRecorder "x86-64 Emit" emitElapsed
                        if verbosity >= 2 then
                            let t = System.Math.Round(emitElapsed, 1)
                            println $"        {t}ms"
                        Ok binary

    | Platform.ARM64Backend armTarget ->
        // ARM64 backend (original)
        if verbosity >= 1 then println codegenLabel
        let codegenStart = sw.Elapsed.TotalMilliseconds
        let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
        let codegenOptions : ARM64CodeGenTypes.CodeGenOptions = {
            DisableFreeList = options.DisableFreeList
            EnableCoverage = options.EnableCoverage
            CoverageExprCount = coverageExprCount
            EnableLeakCheck = options.EnableLeakCheck
        }
        let arm64Target = ARM64.targetConfigFor armTarget
        let functionContexts =
            let contexts = Dictionary<LIR.Function, obj>(LirFunctionReferenceComparer())
            for group in metadataGroups do
                for func in group.Functions do
                    contexts.[func] <- group.ContextIdentity
            contexts
        let functionCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun func generate ->
                    let contextIdentity =
                        if ARM64GenericReferenceCounts.isPlannedGenericRefCountDecHelperCacheKey func then
                            current.Arm64GenericReleaseHelperContextIdentity
                        else
                            match functionContexts.TryGetValue func with
                            | true, identity -> identity
                            | false, _ -> programContextIdentity
                    current.CodegenFunction
                        contextIdentity
                        arm64Target
                        codegenOptions
                        func
                        generate)
        let metadataGroupCache =
            session
            |> Option.map (fun current ->
                fun contextIdentity functions summarize ->
                    current.Arm64MetadataGroup
                        contextIdentity
                        functions
                        summarize)
        let functionGroupCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun contextIdentity functions generate ->
                    current.CodegenFunctionGroup
                        contextIdentity
                        arm64Target
                        codegenOptions
                        functions
                        generate)
        let refinementCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun func callees refine ->
                    current.RefineArm64LirFunction func callees refine)
        let helperCache =
            session
            |> Option.filter (fun _ -> not options.EnableCoverage)
            |> Option.map (fun current ->
                fun helperKey generate ->
                    current.Arm64Helpers
                        programContextIdentity
                        arm64Target
                        codegenOptions
                        helperKey
                        generate)
        let codegenPhaseRecorder =
            passTimingRecorder
            |> Option.map (fun record ->
                fun name (elapsedMs: float) ->
                    record {
                        Pass = name
                        Elapsed = TimeSpan.FromMilliseconds elapsedMs
                    })
        let lirOpExpansionRecorder =
            session
            |> Option.bind (fun current -> current.Arm64LirOpExpansionRecorder)
        let codegenResult =
            CodeGen.generateARM64WithOptionsAndCaches
                arm64Target
                codegenOptions
                (Some arm64SumShapeRegistry)
                functionCache
                refinementCache
                functionGroupCache
                functionGroups
                metadataGroupCache
                helperCache
                metadataGroups
                lirOpExpansionRecorder
                codegenPhaseRecorder
                allocatedProgram
        match codegenResult with
        | Error err -> Error $"Code generation error: {err}"
        | Ok arm64Program ->
            let codegenElapsed = sw.Elapsed.TotalMilliseconds - codegenStart
            recordPassTiming passTimingRecorder "Code Generation" codegenElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(codegenElapsed, 1)
                println $"        {t}ms"

            if dumpAsm && verbosity >= 3 then
                let arm64Instructions =
                    CodeGen.generatedProgramInstructions arm64Program
                println "=== ARM64 Assembly Instructions ==="
                for (i, instr) in List.indexed arm64Instructions do
                    println $"  {i}: {instr}"
                println ""

            let os = ARM64.targetOS arm64Target
            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
            if verbosity >= 1 then println (emitLabel.Replace("{format}", formatName))
            let emitStart = sw.Elapsed.TotalMilliseconds
            let prepareCachedChunk =
                session
                |> Option.map (fun current -> current.PrepareArm64EmissionChunk)
            let prepareCachedChunkGroup =
                session
                |> Option.map (fun current -> current.PrepareArm64EmissionChunkGroup)
            let emit =
                ARM64_Emit.emitBinary
                    arm64Program
                    os
                    options.EnableLeakCheck
                    prepareCachedChunk
                    prepareCachedChunkGroup
                    codegenPhaseRecorder
            let emitElapsed = sw.Elapsed.TotalMilliseconds - emitStart
            recordPassTiming passTimingRecorder "ARM64 Emit" emitElapsed
            if verbosity >= 2 then
                let t = System.Math.Round(emitElapsed, 1)
                println $"        {t}ms"

            if dumpMachineCode && verbosity >= 3 then
                println "=== Machine Code (hex) ==="
                for i in 0 .. 4 .. (emit.MachineCode.Length - 1) do
                    if i + 3 < emit.MachineCode.Length then
                        let bytes = sprintf "%02x %02x %02x %02x" emit.MachineCode.[i] emit.MachineCode.[i+1] emit.MachineCode.[i+2] emit.MachineCode.[i+3]
                        println $"  {i:X4}: {bytes}"
                println $"Total: {emit.MachineCode.Length} bytes\n"

            Ok emit.Binary
