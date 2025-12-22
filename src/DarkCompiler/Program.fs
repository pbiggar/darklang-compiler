// Program.fs - Compiler CLI Entry Point
//
// The main entry point for the Darklang compiler CLI.
//
// This module:
// - Parses command-line arguments
// - Orchestrates the compilation pipeline through all passes
// - Handles errors and provides user feedback
//
// Compilation pipeline:
//   1. Parser: Source → AST
//   2. AST_to_ANF: AST → ANF
//   3. ANF_to_MIR: ANF → MIR
//   4. MIR_to_LIR: MIR → LIR
//   5. RegisterAllocation: LIR (virtual) → LIR (physical)
//   6. CodeGen: LIR → ARM64 instructions
//   7. ARM64_Encoding: ARM64 instructions → Machine code
//   8. Binary_Generation: Machine code → Mach-O executable

module Program

open System
open System.IO

/// Compile source expression to executable
let compile (source: string) (outputPath: string) (quiet: bool) : unit =
    if not quiet then
        printfn "Compiling: %s" source

    // Parse
    if not quiet then printfn "  [1/8] Parsing..."
    let ast = Parser.parseString source

    // Convert to ANF
    if not quiet then printfn "  [2/8] Converting to ANF..."
    let (AST.Program expr) = ast
    let (anfExpr, _) = AST_to_ANF.toANF expr (ANF.VarGen 0)
    let anfProgram = ANF.Program anfExpr

    // Convert to MIR
    if not quiet then printfn "  [3/8] Converting to MIR..."
    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0)

    // Convert to LIR
    if not quiet then printfn "  [4/8] Converting to LIR..."
    let lirProgram = MIR_to_LIR.toLIR mirProgram

    // Allocate registers
    if not quiet then printfn "  [5/8] Allocating registers..."
    let (LIR.Program funcs) = lirProgram
    let func = List.head funcs
    let allocResult = RegisterAllocation.allocateRegisters func
    let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
    let allocatedProgram = LIR.Program [allocatedFunc]

    // Generate ARM64 code
    if not quiet then printfn "  [6/8] Generating ARM64 code..."
    let arm64Code = CodeGen.generateARM64 allocatedProgram
    if not quiet then printfn "    Generated %d instructions" arm64Code.Length

    // Encode to machine code and generate binary
    if not quiet then printfn "  [7/8] Encoding ARM64 to machine code..."
    let machineCode = arm64Code |> List.collect ARM64_Encoding.encode

    if not quiet then printfn "  [8/8] Generating binary..."
    let binary = Binary_Generation.createExecutable machineCode

    // Write to file
    Binary_Generation.writeToFile outputPath binary
    if not quiet then printfn "Successfully wrote %d bytes to %s" binary.Length outputPath

/// Run an expression (compile to temp and execute)
let run (source: string) (quiet: bool) : int =
    // Create temp directory and file
    let tempDir = "/tmp/claude"
    if not (Directory.Exists tempDir) then
        Directory.CreateDirectory tempDir |> ignore

    let tempExe = Path.Combine(tempDir, sprintf "dark_%d" (System.Diagnostics.Process.GetCurrentProcess().Id))

    try
        // Compile
        compile source tempExe quiet

        if not quiet then
            printfn ""
            printfn "Running: %s" source
            printfn "---"

        // Execute
        let proc = new System.Diagnostics.Process()
        proc.StartInfo.FileName <- tempExe
        proc.StartInfo.UseShellExecute <- false
        proc.Start() |> ignore
        proc.WaitForExit()

        let exitCode = proc.ExitCode

        if not quiet then
            printfn "---"
            printfn "Exit code: %d" exitCode

        // Cleanup
        if File.Exists tempExe then
            File.Delete tempExe

        exitCode
    with
    | ex ->
        // Cleanup on error
        if File.Exists tempExe then
            File.Delete tempExe
        reraise()

/// Print usage information
let printUsage () =
    printfn "Darklang Compiler"
    printfn ""
    printfn "Usage:"
    printfn "  DarkCompiler <expression> <output>       Compile expression to executable"
    printfn "  DarkCompiler --file <file> <output>      Compile file to executable"
    printfn "  DarkCompiler --run <expression>          Compile and run expression"
    printfn "  DarkCompiler -r <expression>             Compile and run expression"
    printfn "  DarkCompiler --quiet --run <expression>  Run without compilation output"
    printfn "  DarkCompiler -q -r <expression>          Run without compilation output"
    printfn ""
    printfn "Examples:"
    printfn "  DarkCompiler \"2 + 3\" output              # Compile to 'output' file"
    printfn "  DarkCompiler --run \"2 + 3\"               # Run and show exit code (5)"
    printfn "  DarkCompiler -r \"6 * 7\"                  # Run and show exit code (42)"
    printfn "  DarkCompiler -q -r \"10 + 32\"             # Run quietly, exit code 42"
    printfn ""
    printfn "Note: Generated executables may require code signing to run on macOS"

[<EntryPoint>]
let main argv =
    try
        match argv with
        // Run with quiet mode (3 args - most specific first)
        | [| "--quiet"; "--run"; expr |] ->
            run expr true
        | [| "-q"; "-r"; expr |] ->
            run expr true

        // Run mode (2 args)
        | [| "--run"; expr |] ->
            run expr false
        | [| "-r"; expr |] ->
            run expr false

        // File input (3 args)
        | [| "--file"; inputFile; output |] ->
            if not (File.Exists inputFile) then
                printfn "Error: Input file '%s' not found" inputFile
                1
            else
                let source = File.ReadAllText inputFile
                compile source output false
                0

        // Quiet compile (3 args)
        | [| "--quiet"; expr; output |] ->
            compile expr output true
            0
        | [| "-q"; expr; output |] ->
            compile expr output true
            0

        // Normal compile (2 args - most general last)
        | [| expr; output |] ->
            compile expr output false
            0

        | _ ->
            printUsage ()
            1

    with
    | ex ->
        printfn "Error: %s" ex.Message
        printfn "%s" ex.StackTrace
        1
