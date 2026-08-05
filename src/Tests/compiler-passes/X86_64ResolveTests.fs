// X86_64ResolveTests.fs - Tests for x86-64 label resolution
//
// Verifies that CALL/JMP/Jcc labels are resolved to correct relative offsets.

module X86_64ResolveTests

open X86_64

/// Test: entry labels should be required explicitly, not replaced with offset 0.
let testRequireLabelPositionRejectsMissingStart () : Result<unit, string> =
    match X86_64_Resolve.requireLabelPosition "_start" Map.empty with
    | Error msg when msg.Contains("Missing required label: _start") -> Ok ()
    | Error msg -> Error $"Expected missing _start label error, got: {msg}"
    | Ok offset -> Error $"Expected missing _start label to fail, got offset {offset}"

/// Test: generate and execute a program with a forward call
let testCallAndExecute () : Result<unit, string> =
    // main: call func; mov rax,60; syscall
    // func: mov rdi,42; ret
    // Result: exit(42)
    let instructions = [
        CALL "func"
        // func sets RDI to 42, then returns here
        MOV_imm32 (RAX, 60)     // sys_exit
        SYSCALL
        Label "func"
        MOV_imm32 (RDI, 42)     // exit code
        RET
    ]
    match X86_64_Resolve.resolveAndEncode instructions with
    | Error err -> Error $"Resolution failed: {err}"
    | Ok { MachineCode = machineCode } ->
        let binary =
            Binary_Generation_ELF_X86_64.createExecutableWithPools
                machineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0
        match X86_64BinaryTests.runElfBinary binary with
        | Error err -> Error err
        | Ok exitCode ->
            if exitCode = 42 then Ok ()
            else Error $"Expected exit code 42, got {exitCode}"

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("Require label position rejects missing _start", testRequireLabelPositionRejectsMissingStart)
    ("CALL + execute", testCallAndExecute)
]
