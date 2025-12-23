// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open System
open System.IO
open System.Diagnostics

/// Result of compilation
type CompileResult = {
    Binary: byte array
    Success: bool
    ErrorMessage: string option
}

/// Result of execution
type ExecutionResult = {
    ExitCode: int
    Stdout: string
    Stderr: string
}

/// Compile source code to binary (in-memory, no file I/O)
let compile (source: string) : CompileResult =
    try
        // Pass 1: Parse
        let ast = Parser.parseString source

        // Pass 2: AST → ANF
        let (AST.Program expr) = ast
        let (anfExpr, _) = AST_to_ANF.toANF expr (ANF.VarGen 0)
        let anfProgram = ANF.Program anfExpr

        // Pass 3: ANF → MIR
        let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0)

        // Pass 4: MIR → LIR
        let lirProgram = MIR_to_LIR.toLIR mirProgram

        // Pass 5: Register Allocation
        let (LIR.Program funcs) = lirProgram
        let func = List.head funcs
        let allocResult = RegisterAllocation.allocateRegisters func
        let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
        let allocatedProgram = LIR.Program [allocatedFunc]

        // Pass 6: Code Generation (LIR → ARM64)
        let arm64Instructions = CodeGen.generateARM64 allocatedProgram

        // Pass 7: ARM64 Encoding (ARM64 → machine code)
        let machineCode = arm64Instructions |> List.collect ARM64_Encoding.encode

        // Pass 8: Binary Generation (machine code → Mach-O)
        let binary = Binary_Generation.createExecutable machineCode

        { Binary = binary
          Success = true
          ErrorMessage = None }
    with
    | ex ->
        { Binary = Array.empty
          Success = false
          ErrorMessage = Some (sprintf "Compilation failed: %s" ex.Message) }

/// Execute compiled binary and capture output
let execute (binary: byte array) : ExecutionResult =
    // Write binary to temp file
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    File.WriteAllBytes(tempPath, binary)

    try
        // Make executable using Unix file mode
        let permissions = File.GetUnixFileMode(tempPath)
        File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)

        // Code sign with adhoc signature (required for macOS)
        let codesignInfo = ProcessStartInfo("codesign")
        codesignInfo.Arguments <- $"-s - \"{tempPath}\""
        codesignInfo.UseShellExecute <- false
        codesignInfo.RedirectStandardOutput <- true
        codesignInfo.RedirectStandardError <- true
        let codesignProc = Process.Start(codesignInfo)
        codesignProc.WaitForExit()

        // Execute
        let execInfo = ProcessStartInfo(tempPath)
        execInfo.RedirectStandardOutput <- true
        execInfo.RedirectStandardError <- true
        execInfo.UseShellExecute <- false

        let execProc = Process.Start(execInfo)
        let stdout = execProc.StandardOutput.ReadToEnd()
        let stderr = execProc.StandardError.ReadToEnd()
        execProc.WaitForExit()

        { ExitCode = execProc.ExitCode
          Stdout = stdout
          Stderr = stderr }
    finally
        // Cleanup
        try File.Delete(tempPath) with | _ -> ()

/// Compile and run source code (main entry point for E2E tests)
let compileAndRun (source: string) : ExecutionResult =
    let compileResult = compile source

    if not compileResult.Success then
        { ExitCode = 1
          Stdout = ""
          Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
    else
        execute compileResult.Binary
