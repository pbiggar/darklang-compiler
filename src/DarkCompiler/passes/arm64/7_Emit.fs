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
    (instructions: ARM64Symbolic.Instr list)
    (os: Platform.OS)
    (enableLeakCheck: bool)
    (phaseRecorder: (string -> float -> unit) option)
    : EmitResult =
    let timer = System.Diagnostics.Stopwatch.StartNew ()
    let recordPhase name startedAt =
        phaseRecorder
        |> Option.iter (fun record -> record name (timer.Elapsed.TotalMilliseconds - startedAt))

    let poolStart = timer.Elapsed.TotalMilliseconds
    let (stringPool, floatPool) = ARM64_Resolve.collectPools instructions
    recordPhase "ARM64 Emit Pool Collection" poolStart

    let encodingStart = timer.Elapsed.TotalMilliseconds
    let machineCode =
        ARM64_Encoding.encodeSymbolicWithPools
            instructions
            stringPool
            floatPool
            os
            enableLeakCheck
            phaseRecorder
    recordPhase "ARM64 Emit Encoding" encodingStart

    let binaryStart = timer.Elapsed.TotalMilliseconds
    let binary =
        match os with
        | Platform.MacOS ->
            Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
        | Platform.Linux ->
            Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool enableLeakCheck
    recordPhase "ARM64 Emit Binary Serialization" binaryStart
    { MachineCode = machineCode; Binary = binary }
