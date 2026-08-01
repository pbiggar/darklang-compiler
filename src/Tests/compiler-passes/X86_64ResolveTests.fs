// X86_64ResolveTests.fs - Tests for x86-64 label resolution
//
// Verifies that CALL/JMP/Jcc labels are resolved to correct relative offsets.

module X86_64ResolveTests

open X86_64

/// Test that a simple forward jump resolves correctly
let testForwardJump () : Result<unit, string> =
    // JMP skip; NOP-equivalent (MOV RAX,RAX); Label "skip"; RET
    let instructions = [
        JMP "skip"            // 5 bytes: E9 xx xx xx xx
        MOV_reg (RAX, RAX)    // 3 bytes: 48 89 C0
        Label "skip"
        RET                   // 1 byte: C3
    ]
    match X86_64_Resolve.resolveAndEncode instructions with
    | Error err -> Error $"Resolution failed: {err}"
    | Ok { MachineCode = bytes } ->
        // JMP offset should be +3 (skip the 3-byte MOV)
        // rel32 = target(8) - nextInstr(5) = 3
        if bytes.[0] <> 0xE9uy then
            Error $"Expected E9 (JMP), got {bytes.[0]:X2}"
        elif bytes.[1] <> 0x03uy || bytes.[2] <> 0x00uy || bytes.[3] <> 0x00uy || bytes.[4] <> 0x00uy then
            Error $"Expected rel32=3, got {bytes.[1]:X2} {bytes.[2]:X2} {bytes.[3]:X2} {bytes.[4]:X2}"
        else
            Ok ()

/// Test that a backward jump resolves correctly
let testBackwardJump () : Result<unit, string> =
    let instructions = [
        Label "loop"
        MOV_imm32 (RAX, 1)    // 7 bytes: 48 C7 C0 01 00 00 00
        JMP "loop"             // 5 bytes: E9 xx xx xx xx
    ]
    match X86_64_Resolve.resolveAndEncode instructions with
    | Error err -> Error $"Resolution failed: {err}"
    | Ok { MachineCode = bytes } ->
        // JMP offset: target(0) - nextInstr(12) = -12 = 0xFFFFFFF4
        let rel = int bytes.[8] ||| (int bytes.[9] <<< 8) ||| (int bytes.[10] <<< 16) ||| (int bytes.[11] <<< 24)
        if rel <> -12 then
            Error $"Expected rel32=-12, got {rel}"
        else
            Ok ()

/// Test CALL with forward reference
let testCallForward () : Result<unit, string> =
    let instructions = [
        CALL "func"           // 5 bytes: E8 xx xx xx xx
        RET                   // 1 byte: C3
        Label "func"
        MOV_imm32 (RAX, 42)  // 7 bytes
        RET                   // 1 byte
    ]
    match X86_64_Resolve.resolveAndEncode instructions with
    | Error err -> Error $"Resolution failed: {err}"
    | Ok { MachineCode = bytes } ->
        // CALL offset: target(6) - nextInstr(5) = 1
        if bytes.[0] <> 0xE8uy then
            Error $"Expected E8 (CALL), got {bytes.[0]:X2}"
        elif bytes.[1] <> 0x01uy || bytes.[2] <> 0x00uy || bytes.[3] <> 0x00uy || bytes.[4] <> 0x00uy then
            Error $"Expected rel32=1, got {bytes.[1]:X2} {bytes.[2]:X2} {bytes.[3]:X2} {bytes.[4]:X2}"
        else
            Ok ()

/// Test undefined label is deferred for data label patching
let testUndefinedLabelDeferredFixup () : Result<unit, string> =
    let instructions = [
        JMP "nonexistent"
    ]
    match X86_64_Resolve.resolveAndEncode instructions with
    | Error msg -> Error $"Unexpected error: {msg}"
    | Ok result ->
        if List.length result.DeferredFixups = 1
           && result.DeferredFixups.[0].TargetLabel = "nonexistent" then Ok ()
        else Error $"Expected 1 deferred fixup for 'nonexistent', got {result.DeferredFixups}"

/// Test duplicate labels are reported instead of silently choosing one target.
let testDuplicateLabel () : Result<unit, string> =
    let instructions = [
        Label "again"
        MOV_imm32 (RAX, 1)
        Label "again"
        RET
    ]
    match X86_64_Resolve.resolveAndEncode instructions with
    | Ok _ -> Error "Expected duplicate label to fail"
    | Error msg ->
        if msg.Contains "Duplicate label: again" then Ok ()
        else Error $"Expected duplicate label error, got: {msg}"

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
    ("Forward JMP", testForwardJump)
    ("Backward JMP", testBackwardJump)
    ("Forward CALL", testCallForward)
    ("Undefined label deferred fixup", testUndefinedLabelDeferredFixup)
    ("Duplicate label error", testDuplicateLabel)
    ("Require label position rejects missing _start", testRequireLabelPositionRejectsMissingStart)
    ("CALL + execute", testCallAndExecute)
]
