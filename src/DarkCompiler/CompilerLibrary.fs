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
let compile (verbosity: int) (source: string) : CompileResult =
    let sw = Stopwatch.StartNew()
    try
        // Pass 1: Parse
        if verbosity >= 1 then printfn "  [1/8] Parse..."
        let ast = Parser.parseString source
        let parseTime = sw.Elapsed.TotalMilliseconds
        if verbosity >= 2 then printfn "        %.1fms" parseTime

        // Pass 2: AST → ANF
        if verbosity >= 1 then printfn "  [2/8] AST → ANF..."
        let (AST.Program expr) = ast
        let (anfExpr, _) = AST_to_ANF.toANF expr (ANF.VarGen 0)
        let anfProgram = ANF.Program anfExpr
        let anfTime = sw.Elapsed.TotalMilliseconds - parseTime
        if verbosity >= 2 then printfn "        %.1fms" anfTime

        // Pass 3: ANF → MIR
        if verbosity >= 1 then printfn "  [3/8] ANF → MIR..."
        let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0)
        let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - anfTime
        if verbosity >= 2 then printfn "        %.1fms" mirTime

        // Pass 4: MIR → LIR
        if verbosity >= 1 then printfn "  [4/8] MIR → LIR..."
        let lirProgram = MIR_to_LIR.toLIR mirProgram
        let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - anfTime - mirTime
        if verbosity >= 2 then printfn "        %.1fms" lirTime

        // Pass 5: Register Allocation
        if verbosity >= 1 then printfn "  [5/8] Register Allocation..."
        let (LIR.Program funcs) = lirProgram
        let func = List.head funcs
        let allocResult = RegisterAllocation.allocateRegisters func
        let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
        let allocatedProgram = LIR.Program [allocatedFunc]
        let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - anfTime - mirTime - lirTime
        if verbosity >= 2 then printfn "        %.1fms" allocTime

        // Pass 6: Code Generation (LIR → ARM64)
        if verbosity >= 1 then printfn "  [6/8] Code Generation..."
        let arm64Instructions = CodeGen.generateARM64 allocatedProgram
        let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - anfTime - mirTime - lirTime - allocTime
        if verbosity >= 2 then printfn "        %.1fms" codegenTime

        // Pass 7: ARM64 Encoding (ARM64 → machine code)
        if verbosity >= 1 then printfn "  [7/8] ARM64 Encoding..."
        let machineCode = arm64Instructions |> List.collect ARM64_Encoding.encode
        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - anfTime - mirTime - lirTime - allocTime - codegenTime
        if verbosity >= 2 then printfn "        %.1fms" encodeTime

        // Pass 8: Binary Generation (machine code → Mach-O)
        if verbosity >= 1 then printfn "  [8/8] Binary Generation..."
        let binary = Binary_Generation.createExecutable machineCode
        let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - anfTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
        if verbosity >= 2 then printfn "        %.1fms" binaryTime

        sw.Stop()

        if verbosity >= 1 then
            printfn "  ✓ Compilation complete (%.1fms)" sw.Elapsed.TotalMilliseconds

        { Binary = binary
          Success = true
          ErrorMessage = None }
    with
    | ex ->
        { Binary = Array.empty
          Success = false
          ErrorMessage = Some (sprintf "Compilation failed: %s" ex.Message) }

/// Execute compiled binary and capture output
let execute (verbosity: int) (binary: byte array) : ExecutionResult =
    let sw = Stopwatch.StartNew()

    if verbosity >= 1 then printfn ""
    if verbosity >= 1 then printfn "  Execution:"

    // Write binary to temp file
    if verbosity >= 1 then printfn "    • Writing binary to temp file..."
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    File.WriteAllBytes(tempPath, binary)
    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then printfn "      %.1fms" writeTime

    try
        // Make executable using Unix file mode
        if verbosity >= 1 then printfn "    • Setting executable permissions..."
        let permissions = File.GetUnixFileMode(tempPath)
        File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
        let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
        if verbosity >= 2 then printfn "      %.1fms" chmodTime

        // Code sign with adhoc signature (required for macOS)
        if verbosity >= 1 then printfn "    • Code signing (adhoc)..."
        let codesignStart = sw.Elapsed.TotalMilliseconds
        let codesignInfo = ProcessStartInfo("codesign")
        codesignInfo.Arguments <- $"-s - \"{tempPath}\""
        codesignInfo.UseShellExecute <- false
        codesignInfo.RedirectStandardOutput <- true
        codesignInfo.RedirectStandardError <- true
        let codesignProc = Process.Start(codesignInfo)
        codesignProc.WaitForExit()

        if codesignProc.ExitCode <> 0 then
            let stderr = codesignProc.StandardError.ReadToEnd()
            failwith (sprintf "Code signing failed: %s" stderr)

        let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
        if verbosity >= 2 then printfn "      %.1fms" codesignTime

        // Execute
        if verbosity >= 1 then printfn "    • Running binary..."
        let execStart = sw.Elapsed.TotalMilliseconds
        let execInfo = ProcessStartInfo(tempPath)
        execInfo.RedirectStandardOutput <- true
        execInfo.RedirectStandardError <- true
        execInfo.UseShellExecute <- false

        use execProc = Process.Start(execInfo)

        // Start async reads immediately to avoid blocking
        let stdoutTask = execProc.StandardOutput.ReadToEndAsync()
        let stderrTask = execProc.StandardError.ReadToEndAsync()

        // Wait for process to complete
        execProc.WaitForExit()

        // Now wait for output to be fully read
        let stdout = stdoutTask.Result
        let stderr = stderrTask.Result

        let execTime = sw.Elapsed.TotalMilliseconds - execStart
        if verbosity >= 2 then printfn "      %.1fms" execTime

        sw.Stop()

        if verbosity >= 1 then
            printfn "  ✓ Execution complete (%.1fms)" sw.Elapsed.TotalMilliseconds

        { ExitCode = execProc.ExitCode
          Stdout = stdout
          Stderr = stderr }
    finally
        // Cleanup
        try File.Delete(tempPath) with | _ -> ()

/// Compile and run source code (main entry point for E2E tests)
let compileAndRun (verbosity: int) (source: string) : ExecutionResult =
    let compileResult = compile verbosity source

    if not compileResult.Success then
        { ExitCode = 1
          Stdout = ""
          Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
    else
        execute verbosity compileResult.Binary
