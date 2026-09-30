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

let private finalizeArm64GenericHelperIds
    (LIR.Program (functions, variants, records))
    (functionGroups: CodeGen.FunctionGroup list)
    (metadataGroups: CodeGen.MetadataGroup list) =
    let sourceNames =
        functions
        |> List.fold (fun names (func: LIR.Function) ->
            match FunctionIdMap.tryFind func.Id names with
            | Some existing when existing <> func.Name ->
                Crash.crash
                    $"Executable assigns FunctionId {AST.functionIdValue func.Id} to both '{existing}' and '{func.Name}'"
            | _ -> FunctionIdMap.add func.Id func.Name names) FunctionIdMap.empty
    let labels =
        functions
        |> List.collect (fun func ->
            func.CodegenFacts
            |> Option.map (fun facts -> facts.Arm64GenericHelperIds |> Map.keys |> Seq.toList)
            |> Option.defaultValue [])
    let finalIds = AST.allocateFunctionIds (sourceNames |> FunctionIdMap.keys) labels
    let finalizedId label =
        Map.tryFind label finalIds
        |> Option.defaultWith (fun () -> Crash.crash $"ARM64 helper '{label}' has no final identity")
    let finalizeFunction (func: LIR.Function) =
        let localIds =
            func.CodegenFacts
            |> Option.map (fun facts -> facts.Arm64GenericHelperIds)
            |> Option.defaultValue Map.empty
        if Map.isEmpty localIds then func
        else
            let replacements =
                localIds
                |> Map.fold (fun ids label oldId ->
                    let newId = finalizedId label
                    match FunctionIdMap.tryFind oldId ids with
                    | Some existing when existing <> newId ->
                        Crash.crash $"ARM64 helper identity {AST.functionIdValue oldId} names two helpers in '{func.Name}'"
                    | _ -> FunctionIdMap.add oldId newId ids) FunctionIdMap.empty
            let rewrite = function
                | LIR.Call (dest, id, args) ->
                    match FunctionIdMap.tryFind id replacements with
                    | Some replacement -> LIR.Call (dest, replacement, args)
                    | None -> LIR.Call (dest, id, args)
                | LIR.TailCall (id, args) ->
                    match FunctionIdMap.tryFind id replacements with
                    | Some replacement -> LIR.TailCall (replacement, args)
                    | None -> LIR.TailCall (id, args)
                | instr -> instr
            let blocks =
                func.CFG.Blocks
                |> Map.map (fun _ block ->
                    { block with Instrs = block.Instrs |> List.map rewrite })
            let facts =
                func.CodegenFacts
                |> Option.map (fun facts ->
                    { facts with
                        Arm64GenericHelperIds =
                            localIds
                            |> Map.map (fun label _ -> finalizedId label) })
            { func with CFG = { func.CFG with Blocks = blocks }; CodegenFacts = facts }
    let finalized = functions |> List.map finalizeFunction
    let byId =
        List.zip functions finalized
        |> List.groupBy (fun (original, _) -> original.Id)
        |> FunctionIdMap.ofList
    let remap (func: LIR.Function) =
        FunctionIdMap.tryFind func.Id byId
        |> Option.bind (List.tryPick (fun (original, finalized) ->
            if obj.ReferenceEquals(original, func) then Some finalized else None))
        |> Option.defaultWith (fun () ->
            Crash.crash $"Executable group contains unknown function '{func.Name}'")
    let functionGroups =
        functionGroups
        |> List.map (fun group -> { group with Functions = group.Functions |> List.map remap })
    let metadataGroups =
        metadataGroups
        |> List.map (fun group -> { group with Functions = group.Functions |> List.map remap })
    LIR.Program (finalized, variants, records), functionGroups, metadataGroups

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
    (knownArm64CalleeWrites: FunctionIdMap<ARM64CalleeClobbers.Writes>)
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
        let allocatedProgram, functionGroups, metadataGroups =
            finalizeArm64GenericHelperIds allocatedProgram functionGroups metadataGroups
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
                (Some knownArm64CalleeWrites)
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
