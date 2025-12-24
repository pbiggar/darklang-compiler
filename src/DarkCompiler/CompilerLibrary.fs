// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open System
open System.IO
open System.Diagnostics
open Output

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
        if verbosity >= 1 then println "  [1/8] Parse..."
        let parseResult = Parser.parseString source
        let parseTime = sw.Elapsed.TotalMilliseconds
        if verbosity >= 2 then
            let t = System.Math.Round(parseTime, 1)
            println $"        {t}ms"

        match parseResult with
        | Error err ->
            { Binary = Array.empty
              Success = false
              ErrorMessage = Some $"Parse error: {err}" }
        | Ok ast ->
            // Show AST
            if verbosity >= 3 then
                println "\n=== AST ==="
                let (AST.Program expr) = ast
                println $"{expr}"
                println ""

            // Pass 1.5: Type Checking
            if verbosity >= 1 then println "  [1.5/8] Type Checking..."
            let typeCheckResult = TypeChecking.checkProgram ast
            let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
            if verbosity >= 2 then
                let t = System.Math.Round(typeCheckTime, 1)
                println $"        {t}ms"

            match typeCheckResult with
            | Error typeErr ->
                { Binary = Array.empty
                  Success = false
                  ErrorMessage = Some $"Type error: {TypeChecking.typeErrorToString typeErr}" }
            | Ok programType ->
                if verbosity >= 3 then
                    println $"Program type: {TypeChecking.typeToString programType}"
                    println ""

                // Pass 2: AST → ANF
                if verbosity >= 1 then println "  [2/8] AST → ANF..."
                let (AST.Program expr) = ast
                let anfResult = AST_to_ANF.toANF expr (ANF.VarGen 0) Map.empty
                let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                if verbosity >= 2 then
                    let t = System.Math.Round(anfTime, 1)
                    println $"        {t}ms"

                match anfResult with
                | Error err ->
                    { Binary = Array.empty
                      Success = false
                      ErrorMessage = Some $"ANF conversion error: {err}" }
                | Ok (anfExpr, _) ->
                    let anfProgram = ANF.Program anfExpr

                    // Show ANF
                    if verbosity >= 3 then
                        println "=== ANF ==="
                        println $"{anfExpr}"
                        println ""

                    // Pass 3: ANF → MIR
                    if verbosity >= 1 then println "  [3/8] ANF → MIR..."
                    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0)

                    // Show MIR
                    if verbosity >= 3 then
                        let (MIR.Program cfg) = mirProgram
                        println "=== MIR (Control Flow Graph) ==="
                        println $"Entry: {cfg.Entry}"
                        for kvp in cfg.Blocks do
                            let block = kvp.Value
                            println $"\n{block.Label}:"
                            for instr in block.Instrs do
                                println $"  {instr}"
                            println $"  {block.Terminator}"
                        println ""

                    let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(mirTime, 1)
                        println $"        {t}ms"

                    // Pass 4: MIR → LIR
                    if verbosity >= 1 then println "  [4/8] MIR → LIR..."
                    let lirProgram = MIR_to_LIR.toLIR mirProgram

                    // Show LIR
                    if verbosity >= 3 then
                        let (LIR.Program funcs) = lirProgram
                        println "=== LIR (Low-level IR with CFG) ==="
                        for func in funcs do
                            println $"Function: {func.Name}"
                            println $"Entry: {func.CFG.Entry}"
                            for kvp in func.CFG.Blocks do
                                let block = kvp.Value
                                println $"\n{block.Label}:"
                                for instr in block.Instrs do
                                    println $"  {instr}"
                                println $"  {block.Terminator}"
                        println ""

                    let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - mirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(lirTime, 1)
                        println $"        {t}ms"

                    // Pass 5: Register Allocation
                    if verbosity >= 1 then println "  [5/8] Register Allocation..."
                    let (LIR.Program funcs) = lirProgram
                    let func = List.head funcs
                    let allocatedFunc = RegisterAllocation.allocateRegisters func
                    let allocatedProgram = LIR.Program [allocatedFunc]

                    // Show LIR after allocation
                    if verbosity >= 3 then
                        println "=== LIR (After Register Allocation) ==="
                        println $"Function: {allocatedFunc.Name}"
                        println $"Entry: {allocatedFunc.CFG.Entry}"
                        for kvp in allocatedFunc.CFG.Blocks do
                            let block = kvp.Value
                            println $"\n{block.Label}:"
                            for instr in block.Instrs do
                                println $"  {instr}"
                            println $"  {block.Terminator}"
                        println ""

                    let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - mirTime - lirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocTime, 1)
                        println $"        {t}ms"

                    // Pass 6: Code Generation (LIR → ARM64)
                    if verbosity >= 1 then println "  [6/8] Code Generation..."
                    let codegenResult = CodeGen.generateARM64 allocatedProgram
                    let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - mirTime - lirTime - allocTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(codegenTime, 1)
                        println $"        {t}ms"

                    match codegenResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Code generation error: {err}" }
                    | Ok arm64Instructions ->
                        // Show ARM64
                        if verbosity >= 3 then
                            println "=== ARM64 Assembly Instructions ==="
                            for (i, instr) in List.indexed arm64Instructions do
                                println $"  {i}: {instr}"
                            println ""

                        // Pass 7: ARM64 Encoding (ARM64 → machine code)
                        if verbosity >= 1 then println "  [7/8] ARM64 Encoding..."
                        let machineCode = ARM64_Encoding.encodeAll arm64Instructions

                        // Show machine code
                        if verbosity >= 3 then
                            println "=== Machine Code (hex) ==="
                            for i in 0 .. 4 .. (machineCode.Length - 1) do
                                if i + 3 < machineCode.Length then
                                    let bytes = sprintf "%02x %02x %02x %02x" machineCode.[i] machineCode.[i+1] machineCode.[i+2] machineCode.[i+3]
                                    println $"  {i:X4}: {bytes}"
                            println $"Total: {machineCode.Length} bytes\n"

                        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - mirTime - lirTime - allocTime - codegenTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(encodeTime, 1)
                            println $"        {t}ms"

                        // Pass 8: Binary Generation (machine code → executable)
                        let osResult = Platform.detectOS ()
                        match osResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some $"Platform detection error: {err}" }
                        | Ok os ->
                            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                            if verbosity >= 1 then println $"  [8/8] Binary Generation ({formatName})..."
                            let binary =
                                match os with
                                | Platform.MacOS -> Binary_Generation_MachO.createExecutable machineCode
                                | Platform.Linux -> Binary_Generation_ELF.createExecutable machineCode
                            let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(binaryTime, 1)
                                println $"        {t}ms"

                            sw.Stop()

                            if verbosity >= 1 then
                                println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                            { Binary = binary
                              Success = true
                              ErrorMessage = None }
    with
    | ex ->
        { Binary = Array.empty
          Success = false
          ErrorMessage = Some $"Compilation failed: {ex.Message}" }

/// Execute compiled binary and capture output
let execute (verbosity: int) (binary: byte array) : ExecutionResult =
    let sw = Stopwatch.StartNew()

    if verbosity >= 1 then println ""
    if verbosity >= 1 then println "  Execution:"

    // Write binary to temp file
    if verbosity >= 1 then println "    • Writing binary to temp file..."
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    // Write and flush to disk to minimize (but not eliminate) "Text file busy" race
    do
        use stream = new IO.FileStream(tempPath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.None)
        stream.Write(binary, 0, binary.Length)
        stream.Flush(true)  // Flush both stream and OS buffers to disk

    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then println $"      {System.Math.Round(writeTime, 1)}ms"

    try
        // Make executable using Unix file mode
        if verbosity >= 1 then println "    • Setting executable permissions..."
        let permissions = File.GetUnixFileMode(tempPath)
        File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
        let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
        if verbosity >= 2 then println $"      {System.Math.Round(chmodTime, 1)}ms"

        // Code sign with adhoc signature (required for macOS only)
        let codesignResult =
            match Platform.detectOS () with
            | Error err ->
                // Platform detection failed
                Some $"Platform detection failed: {err}"
            | Ok os ->
                if Platform.requiresCodeSigning os then
                    if verbosity >= 1 then println "    • Code signing (adhoc)..."
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
                        Some $"Code signing failed: {stderr}"
                    else
                        let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
                        if verbosity >= 2 then println $"      {System.Math.Round(codesignTime, 1)}ms"
                        None
                else
                    if verbosity >= 1 then println "    • Code signing skipped (not required on Linux)"
                    None

        match codesignResult with
        | Some errorMsg ->
            // Code signing or platform detection failed - return error
            { ExitCode = -1
              Stdout = ""
              Stderr = errorMsg }
        | None ->
            // Execute (with retry for "Text file busy" race condition)
            // Even with flush, kernel may not have fully synced file/permissions in parallel tests
            if verbosity >= 1 then println "    • Running binary..."
            let execStart = sw.Elapsed.TotalMilliseconds
            let execInfo = ProcessStartInfo(tempPath)
            execInfo.RedirectStandardOutput <- true
            execInfo.RedirectStandardError <- true
            execInfo.UseShellExecute <- false

            // Retry up to 3 times with small delay if we get "Text file busy"
            let rec startWithRetry attempts =
                try
                    Process.Start(execInfo)
                with
                | :? System.ComponentModel.Win32Exception as ex when ex.Message.Contains("Text file busy") && attempts > 0 ->
                    Threading.Thread.Sleep(10)  // Wait 10ms before retry
                    startWithRetry (attempts - 1)

            use execProc = startWithRetry 3

            // Start async reads immediately to avoid blocking
            let stdoutTask = execProc.StandardOutput.ReadToEndAsync()
            let stderrTask = execProc.StandardError.ReadToEndAsync()

            // Wait for process to complete
            execProc.WaitForExit()

            // Now wait for output to be fully read
            let stdout = stdoutTask.Result
            let stderr = stderrTask.Result

            let execTime = sw.Elapsed.TotalMilliseconds - execStart
            if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"

            sw.Stop()

            if verbosity >= 1 then
                println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

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
