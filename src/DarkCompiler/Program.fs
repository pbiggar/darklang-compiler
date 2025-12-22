// Program.fs - Compiler CLI Entry Point
//
// The main entry point for the Darklang compiler CLI.
//
// This module:
// - Parses command-line arguments using POSIX-style flags
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

/// Output verbosity level
type VerbosityLevel = Quiet | Normal | Verbose

/// Parsed CLI options
type CliOptions = {
    Run: bool                    // True = run, False = compile (default)
    IsExpression: bool           // True = expression, False = file (default)
    OutputFile: string option
    Verbosity: VerbosityLevel
    Help: bool
    Version: bool
    Argument: string option
}

/// Default empty options
let defaultOptions = {
    Run = false
    IsExpression = false
    OutputFile = None
    Verbosity = Normal
    Help = false
    Version = false
    Argument = None
}

/// Parse command-line flags into options
let parseArgs (argv: string array) : Result<CliOptions, string> =
    let rec parseFlags (args: string list) (opts: CliOptions) (lastVerbosity: VerbosityLevel) : Result<CliOptions, string> =
        match args with
        | [] ->
            // Apply last verbosity setting (last one wins)
            Ok { opts with Verbosity = lastVerbosity }

        | "-r" :: rest | "--run" :: rest ->
            if opts.Run then
                Error "Run flag specified multiple times"
            else
                parseFlags rest { opts with Run = true } lastVerbosity

        | "-e" :: rest | "--expression" :: rest ->
            if opts.IsExpression then
                Error "Expression flag specified multiple times"
            else
                parseFlags rest { opts with IsExpression = true } lastVerbosity

        | "-o" :: value :: rest | "--output" :: value :: rest ->
            if opts.OutputFile.IsSome then
                Error "Output file specified multiple times"
            else
                parseFlags rest { opts with OutputFile = Some value } lastVerbosity

        | flag :: rest when flag.StartsWith("-o") && flag.Length > 2 ->
            // Handle -ofile format
            let value = flag.Substring(2)
            if opts.OutputFile.IsSome then
                Error "Output file specified multiple times"
            else
                parseFlags rest { opts with OutputFile = Some value } lastVerbosity

        | flag :: rest when flag.StartsWith("--output=") ->
            // Handle --output=file format
            let value = flag.Substring(9)
            if opts.OutputFile.IsSome then
                Error "Output file specified multiple times"
            else
                parseFlags rest { opts with OutputFile = Some value } lastVerbosity

        | "-q" :: rest | "--quiet" :: rest ->
            parseFlags rest opts Quiet

        | "-v" :: rest | "--verbose" :: rest ->
            parseFlags rest opts Verbose

        | "-h" :: rest | "--help" :: rest ->
            parseFlags rest { opts with Help = true } lastVerbosity

        | "--version" :: rest ->
            parseFlags rest { opts with Version = true } lastVerbosity

        | "-" :: rest ->
            // Special case: "-" means stdin
            if opts.Argument.IsSome then
                Error "Cannot specify multiple input sources"
            else
                parseFlags rest { opts with Argument = Some "-" } lastVerbosity

        | flag :: rest when flag.StartsWith("-") && flag.Length > 1 ->
            // Handle combined short flags like -qr, -re, etc.
            let chars = flag.Substring(1).ToCharArray()
            let rec expandFlags (cs: char list) (acc: string list) =
                match cs with
                | [] -> acc
                | 'r' :: rest -> expandFlags rest ("-r" :: acc)
                | 'e' :: rest -> expandFlags rest ("-e" :: acc)
                | 'q' :: rest -> expandFlags rest ("-q" :: acc)
                | 'v' :: rest -> expandFlags rest ("-v" :: acc)
                | 'h' :: rest -> expandFlags rest ("-h" :: acc)
                | 'o' :: rest when rest.Length > 0 ->
                    // -ovalue format
                    let value = System.String(Array.ofList rest)
                    expandFlags [] (sprintf "-o%s" value :: acc)
                | c :: _ ->
                    // Invalid flag character
                    expandFlags [] (sprintf "-%c" c :: acc)

            let expandedFlags = expandFlags (Array.toList chars) [] |> List.rev
            parseFlags (expandedFlags @ rest) opts lastVerbosity

        | arg :: rest when not (arg.StartsWith("-")) ->
            // Non-flag argument - this is the filename or expression
            if opts.Argument.IsSome then
                Error (sprintf "Unexpected argument: %s" arg)
            else
                parseFlags rest { opts with Argument = Some arg } lastVerbosity

        | flag :: _ ->
            Error (sprintf "Unknown flag: %s" flag)

    parseFlags (Array.toList argv) defaultOptions Normal

/// Validate parsed options
let validateOptions (opts: CliOptions) : Result<CliOptions, string> =
    // Help and version override everything else
    if opts.Help || opts.Version then
        Ok opts
    else
        // Check for required argument
        if opts.Argument.IsNone then
            Error "Missing input (filename or expression with -e)"
        // Check for conflicting options
        else if opts.Run && opts.OutputFile.IsSome then
            Error "Cannot specify output file with run mode (-r)"
        else
            Ok opts

/// Compile source expression to executable
let compile (source: string) (outputPath: string) (verbosity: VerbosityLevel) : unit =
    let showNormal = verbosity = Normal || verbosity = Verbose
    let showVerbose = verbosity = Verbose

    if showNormal then
        printfn "Compiling: %s" source

    // Parse
    if showNormal then printfn "  [1/8] Parsing..."
    let ast = Parser.parseString source
    if showVerbose then printfn "      AST: %A" ast

    // Convert to ANF
    if showNormal then printfn "  [2/8] Converting to ANF..."
    let (AST.Program expr) = ast
    let (anfExpr, _) = AST_to_ANF.toANF expr (ANF.VarGen 0)
    let anfProgram = ANF.Program anfExpr
    if showVerbose then printfn "      ANF: %A" anfProgram

    // Convert to MIR
    if showNormal then printfn "  [3/8] Converting to MIR..."
    let (mirProgram, _) = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0)
    if showVerbose then printfn "      MIR: %A" mirProgram

    // Convert to LIR
    if showNormal then printfn "  [4/8] Converting to LIR..."
    let lirProgram = MIR_to_LIR.toLIR mirProgram
    if showVerbose then printfn "      LIR: %A" lirProgram

    // Allocate registers
    if showNormal then printfn "  [5/8] Allocating registers..."
    let (LIR.Program funcs) = lirProgram
    let func = List.head funcs
    let allocResult = RegisterAllocation.allocateRegisters func
    let allocatedFunc = { func with Body = allocResult.Instrs; StackSize = allocResult.StackSize }
    let allocatedProgram = LIR.Program [allocatedFunc]
    if showVerbose then printfn "      Allocated LIR: %A" allocatedProgram

    // Generate ARM64 code
    if showNormal then printfn "  [6/8] Generating ARM64 code..."
    let arm64Code = CodeGen.generateARM64 allocatedProgram
    if showNormal then printfn "    Generated %d instructions" arm64Code.Length
    if showVerbose then
        arm64Code |> List.iteri (fun i instr -> printfn "      [%d] %A" i instr)

    // Encode to machine code and generate binary
    if showNormal then printfn "  [7/8] Encoding ARM64 to machine code..."
    let machineCode = arm64Code |> List.collect ARM64_Encoding.encode
    if showVerbose then printfn "      Machine code: %d bytes" machineCode.Length

    if showNormal then printfn "  [8/8] Generating binary..."
    let binary = Binary_Generation.createExecutable machineCode

    // Write to file
    Binary_Generation.writeToFile outputPath binary
    if showNormal then printfn "Successfully wrote %d bytes to %s" binary.Length outputPath

/// Run an expression (compile to temp and execute)
let run (source: string) (verbosity: VerbosityLevel) : int =
    let showNormal = verbosity = Normal || verbosity = Verbose

    // Create temp directory and file
    let tempDir = "/tmp/claude"
    if not (Directory.Exists tempDir) then
        Directory.CreateDirectory tempDir |> ignore

    let tempExe = Path.Combine(tempDir, sprintf "dark_%d" (System.Diagnostics.Process.GetCurrentProcess().Id))

    try
        // Compile
        compile source tempExe verbosity

        if showNormal then
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

        if showNormal then
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

/// Print version information
let printVersion () =
    printfn "Dark Compiler v0.1.0"
    printfn "Darklang ARM64 compiler for macOS"

/// Print usage information
let printUsage () =
    printfn "Dark Compiler v0.1.0"
    printfn ""
    printfn "Usage:"
    printfn "  dark <file> [-o <output>]           Compile file to executable (default)"
    printfn "  dark -r <file>                      Compile and run file"
    printfn "  dark -e <expression> [-o <output>]  Compile expression to executable"
    printfn "  dark -r -e <expression>             Run expression"
    printfn "  dark -r -e -                        Read expression from stdin and run"
    printfn ""
    printfn "Flags:"
    printfn "  -r, --run            Run instead of compile (shows exit code)"
    printfn "  -e, --expression     Treat argument as expression (not filename)"
    printfn "  -o, --output FILE    Output file (default: dark.out)"
    printfn "  -q, --quiet          Suppress compilation output"
    printfn "  -v, --verbose        Show detailed compilation steps"
    printfn "  -h, --help           Show this help message"
    printfn "  --version            Show version information"
    printfn ""
    printfn "Flags can appear in any order and can be combined (e.g., -qr, -re)"
    printfn "When both -q and -v are specified, the last one wins."
    printfn ""
    printfn "Examples:"
    printfn "  dark prog.dark                     Compile file to 'dark.out'"
    printfn "  dark prog.dark -o output           Compile file to 'output'"
    printfn "  dark -r prog.dark                  Compile and run file"
    printfn "  dark -e \"2 + 3\"                    Compile expression to 'dark.out'"
    printfn "  dark -e \"2 + 3\" -o output          Compile expression to 'output'"
    printfn "  dark -r -e \"2 + 3\"                 Run and show exit code (5)"
    printfn "  dark -qr -e \"6 * 7\"                Run quietly (exit code: 42)"
    printfn "  dark -v prog.dark -o output        Compile with verbose output"
    printfn "  dark -r -e - < input.txt           Run expression from stdin"
    printfn ""
    printfn "Note: Generated executables may require code signing to run on macOS"

[<EntryPoint>]
let main argv =
    try
        match parseArgs argv |> Result.bind validateOptions with
        | Error msg ->
            printfn "Error: %s" msg
            printfn ""
            printUsage()
            1

        | Ok options when options.Help ->
            printUsage()
            0

        | Ok options when options.Version ->
            printVersion()
            0

        | Ok options ->
            // Get source code (from stdin, file, or inline expression)
            let getSource () : Result<string, string> =
                match options.Argument with
                | Some "-" ->
                    // Read from stdin
                    try
                        let source = Console.In.ReadToEnd()
                        if String.IsNullOrWhiteSpace source then
                            Error "No input provided on stdin"
                        else
                            Ok source
                    with ex ->
                        Error (sprintf "Failed to read from stdin: %s" ex.Message)

                | Some arg when options.IsExpression ->
                    // Inline expression
                    Ok arg

                | Some filepath ->
                    // Read from file
                    if not (File.Exists filepath) then
                        Error (sprintf "File not found: %s" filepath)
                    else
                        try
                            Ok (File.ReadAllText filepath)
                        with ex ->
                            Error (sprintf "Failed to read file: %s" ex.Message)

                | None ->
                    Error "No source provided"

            match getSource() with
            | Ok source ->
                if options.Run then
                    // Run mode
                    run source options.Verbosity
                else
                    // Compile mode (default)
                    let output = options.OutputFile |> Option.defaultValue "dark.out"
                    compile source output options.Verbosity
                    0

            | Error msg ->
                printfn "Error: %s" msg
                1

    with ex ->
        printfn "Error: %s" ex.Message
        printfn "%s" ex.StackTrace
        1
