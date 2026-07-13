// X86_64BinaryTests.fs - End-to-end test for x86-64 binary generation
//
// Generates a minimal x86-64 ELF binary and verifies it can be executed.
// This proves the encoder + ELF generation pipeline works end-to-end.

module X86_64BinaryTests

open X86_64
open X86_64_Encoding

/// Generate machine code for "exit(42)" on x86-64 Linux:
///   MOV RAX, 60    (syscall number for exit)
///   MOV RDI, 42    (exit code)
///   SYSCALL
let private exitProgram (exitCode: int) : byte array =
    let instructions = [
        MOV_imm32 (RAX, 60)           // sys_exit = 60
        MOV_imm32 (RDI, exitCode)     // exit code
        SYSCALL
    ]
    instructions
    |> List.map encodeInstruction
    |> Array.concat

/// Test that we can generate a valid x86-64 ELF binary
let testGenerateElf () : Result<unit, string> =
    let machineCode = exitProgram 42
    let binary =
        Binary_Generation_ELF_X86_64.createExecutableWithPools
            machineCode
            LiteralPool.emptyStringPool
            LiteralPool.emptyFloatPool
            false 0

    // Verify ELF magic
    if binary.[0] <> 0x7Fuy || binary.[1] <> byte 'E' || binary.[2] <> byte 'L' || binary.[3] <> byte 'F' then
        Error "Missing ELF magic bytes"
    // Verify 64-bit
    elif binary.[4] <> 2uy then
        Error "Not ELF64"
    // Verify little-endian
    elif binary.[5] <> 1uy then
        Error "Not little-endian"
    // Verify machine type is x86-64 (0x3E = 62 at offset 18-19, little-endian)
    elif binary.[18] <> 0x3Euy || binary.[19] <> 0x00uy then
        Error $"Wrong machine type: expected 0x3E 0x00, got 0x{binary.[18]:X2} 0x{binary.[19]:X2}"
    else
        Ok ()

let testElfIdentHelper () : Result<unit, string> =
    let ident = Binary_ELF.createIdent ()
    let expected = [|
        Binary_ELF.EI_MAG0
        Binary_ELF.EI_MAG1
        Binary_ELF.EI_MAG2
        Binary_ELF.EI_MAG3
        Binary_ELF.ELFCLASS64
        Binary_ELF.ELFDATA2LSB
        Binary_ELF.EV_CURRENT
        Binary_ELF.ELFOSABI_NONE
        0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy
    |]

    if ident = expected then
        Ok ()
    else
        Error "ELF ident helper produced unexpected bytes"

/// Run an ELF binary, using qemu-user-static if on a different architecture.
/// Returns the exit code.
let internal runElfBinary (binary: byte array) : Result<int, string> =
    let tempPath = System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.Guid.NewGuid().ToString("N"))
    try
        do
            use stream = new System.IO.FileStream(tempPath, System.IO.FileMode.Create, System.IO.FileAccess.Write, System.IO.FileShare.None)
            stream.Write(binary, 0, binary.Length)
            stream.Flush(true)

        let permissions = System.IO.File.GetUnixFileMode(tempPath)
        System.IO.File.SetUnixFileMode(tempPath, permissions ||| System.IO.UnixFileMode.UserExecute)

        // On non-x86_64 hosts, use qemu-x86_64-static to run the binary
        let psi =
            match Platform.detectArch () with
            | Ok Platform.X86_64 ->
                System.Diagnostics.ProcessStartInfo(tempPath)
            | _ ->
                let p = System.Diagnostics.ProcessStartInfo("qemu-x86_64-static", tempPath)
                p
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true

        use proc = System.Diagnostics.Process.Start(psi)
        proc.WaitForExit(10000) |> ignore
        Ok proc.ExitCode
    with ex ->
        Error $"Failed to execute binary: {ex.Message}"
    |> fun result ->
        try System.IO.File.Delete(tempPath) with _ -> ()
        result

/// Test that the generated binary executes correctly
let testExecuteElf () : Result<unit, string> =
    let machineCode = exitProgram 42
    let binary =
        Binary_Generation_ELF_X86_64.createExecutableWithPools
            machineCode
            LiteralPool.emptyStringPool
            LiteralPool.emptyFloatPool
            false 0

    match runElfBinary binary with
    | Error err -> Error err
    | Ok exitCode ->
        if exitCode = 42 then Ok ()
        else Error $"Expected exit code 42, got {exitCode}"

let tests : (string * (unit -> Result<unit, string>)) list = [
    ("ELF ident helper", testElfIdentHelper)
    ("Generate x86-64 ELF", testGenerateElf)
    ("Execute x86-64 ELF", testExecuteElf)
]
