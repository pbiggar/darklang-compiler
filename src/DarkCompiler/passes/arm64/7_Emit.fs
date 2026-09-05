// 7_Emit.fs - ARM64 Emission (Encoding + Binary Generation)
//
// Resolves symbolic data labels into literal pools, encodes ARM64 instructions,
// and produces a platform-specific binary in a single pass.

module ARM64_Emit

type EmitResult = {
    MachineCode: ARM64.MachineCode array
    Binary: byte array
}

/// Resolve label refs, encode machine code, and generate a binary for the target OS
let emitBinary
    (program: CodeGen.GeneratedProgram)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    (prepareCachedChunk:
        (ARM64Symbolic.Instr list
            -> (unit -> ARM64_Encoding.PreparedChunk)
            -> ARM64_Encoding.PreparedChunk) option)
    : EmitResult =
    let preparedChunks =
        program
        |> CodeGen.generatedProgramChunks
        |> List.map (fun chunk ->
            let prepare () =
                ARM64_Encoding.prepareSymbolicChunk chunk.Instructions
            match prepareCachedChunk with
            | Some cache when chunk.ReusableAcrossCompilations ->
                cache chunk.Instructions prepare
            | _ -> prepare ())

    let (stringPool, floatPool) =
        preparedChunks
        |> Seq.collect (fun chunk -> chunk.PoolLabelRefs)
        |> ARM64_Resolve.collectPoolsFromLabelRefs

    let machineCode =
        ARM64_Encoding.encodePreparedChunksWithPools
            preparedChunks
            stringPool
            floatPool
            os
            enableLeakCheck

    let binary =
        match os with
        | Platform.MacOS ->
            Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
        | Platform.Linux ->
            Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
    { MachineCode = machineCode; Binary = binary }
