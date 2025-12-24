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
open Output

/// Output verbosity level
/// 0 = Quiet (no output)
/// 1 = Normal (standard output)
/// 2 = Verbose (show pass names)
/// 3 = VeryVerbose (show pass names + timing)
type VerbosityLevel = Quiet | Normal | Verbose | VeryVerbose

/// Convert VerbosityLevel to integer for library
/// Library verbosity: 0=silent, 1=pass names, 2=pass names + timing
let verbosityToInt (level: VerbosityLevel) : int =
    match level with
    | Quiet -> 0      // No output
    | Normal -> 0     // CLI handles output, library silent
    | Verbose -> 1    // Library shows pass names
    | VeryVerbose -> 2 // Library shows pass names + timing

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
            // Stack -v flags: -v = Verbose, -vv = VeryVerbose
            let newVerbosity =
                match lastVerbosity with
                | Quiet -> Normal
                | Normal -> Verbose
                | Verbose -> VeryVerbose
                | VeryVerbose -> VeryVerbose
            parseFlags rest opts newVerbosity

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
                    expandFlags [] ($"-o{value}" :: acc)
                | c :: _ ->
                    // Invalid flag character
                    expandFlags [] ($"-{c}" :: acc)

            let expandedFlags = expandFlags (Array.toList chars) [] |> List.rev
            parseFlags (expandedFlags @ rest) opts lastVerbosity

        | arg :: rest when not (arg.StartsWith("-")) ->
            // Non-flag argument - this is the filename or expression
            if opts.Argument.IsSome then
                Error $"Unexpected argument: {arg}"
            else
                parseFlags rest { opts with Argument = Some arg } lastVerbosity

        | flag :: _ ->
            Error $"Unknown flag: {flag}"

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
let compile (source: string) (outputPath: string) (verbosity: VerbosityLevel) : int =
    let showNormal = verbosity = Normal || verbosity = Verbose

    if showNormal then
        println $"Compiling: {source}"

    // Use library for compilation
    let result = CompilerLibrary.compile (verbosityToInt verbosity) source

    if not result.Success then
        let errorMsg = result.ErrorMessage |> Option.defaultValue "Unknown error"
        eprintln $"Compilation failed: {errorMsg}"
        1
    else
        // Write to file
        match Platform.detectOS () with
        | Error err ->
            eprintln $"Platform detection failed: {err}"
            1
        | Ok os ->
            let writeResult =
                match os with
                | Platform.MacOS -> Binary_Generation_MachO.writeToFile outputPath result.Binary
                | Platform.Linux -> Binary_Generation_ELF.writeToFile outputPath result.Binary
            match writeResult with
            | Error err ->
                eprintln $"Failed to write binary: {err}"
                1
            | Ok () ->
                if showNormal then println $"Successfully wrote {result.Binary.Length} bytes to {outputPath}"
                0

/// Run an expression (compile to temp and execute)
let run (source: string) (verbosity: VerbosityLevel) : int =
    let showNormal = verbosity = Normal || verbosity = Verbose

    if showNormal then
        println $"Compiling and running: {source}"
        println "---"

    // Use library for compile and run
    let result = CompilerLibrary.compileAndRun (verbosityToInt verbosity) source

    if showNormal then
        if result.Stdout <> "" then
            println $"{result.Stdout}"
        if result.Stderr <> "" then
            eprintln $"{result.Stderr}"
        println "---"
        println $"Exit code: {result.ExitCode}"

    result.ExitCode

/// Print version information
let printVersion () =
    println "Dark Compiler v0.1.0"
    println "Darklang ARM64 compiler for macOS"

/// Print usage information
let printUsage () =
    println "Dark Compiler v0.1.0"
    println ""
    println "Usage:"
    println "  dark <file> [-o <output>]           Compile file to executable (default)"
    println "  dark -r <file>                      Compile and run file"
    println "  dark -e <expression> [-o <output>]  Compile expression to executable"
    println "  dark -r -e <expression>             Run expression"
    println "  dark -r -e -                        Read expression from stdin and run"
    println ""
    println "Flags:"
    println "  -r, --run            Run instead of compile (shows exit code)"
    println "  -e, --expression     Treat argument as expression (not filename)"
    println "  -o, --output FILE    Output file (default: dark.out)"
    println "  -q, --quiet          Suppress compilation output"
    println "  -v, --verbose        Show compilation pass names"
    println "  -vv                  Show pass names + timing details"
    println "  -h, --help           Show this help message"
    println "  --version            Show version information"
    println ""
    println "Flags can appear in any order and can be combined (e.g., -qr, -re, -vvre)"
    println "Verbosity levels: (none)=normal, -v=passes, -vv=passes+timing"
    println ""
    println "Examples:"
    println "  dark prog.dark                     Compile file to 'dark.out'"
    println "  dark prog.dark -o output           Compile file to 'output'"
    println "  dark -r prog.dark                  Compile and run file"
    printf "  dark -e \"2 + 3\"                    Compile expression to 'dark.out'\n"
    printf "  dark -e \"2 + 3\" -o output          Compile expression to 'output'\n"
    printf "  dark -r -e \"2 + 3\"                 Run and show exit code (5)\n"
    printf "  dark -qr -e \"6 * 7\"                Run quietly (exit code: 42)\n"
    println "  dark -v prog.dark -o output        Compile with verbose output"
    println "  dark -r -e - < input.txt           Run expression from stdin"
    println ""
    println "Note: Generated executables may require code signing to run on macOS"

[<EntryPoint>]
let main argv =
    try
        match parseArgs argv |> Result.bind validateOptions with
        | Error msg ->
            println $"Error: {msg}"
            println ""
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
                        Error $"Failed to read from stdin: {ex.Message}"

                | Some arg when options.IsExpression ->
                    // Inline expression
                    Ok arg

                | Some filepath ->
                    // Read from file
                    if not (File.Exists filepath) then
                        Error $"File not found: {filepath}"
                    else
                        try
                            Ok (File.ReadAllText filepath)
                        with ex ->
                            Error $"Failed to read file: {ex.Message}"

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

            | Error msg ->
                println $"Error: {msg}"
                1

    with ex ->
        println $"Error: {ex.Message}"
        println $"{ex.StackTrace}"
        1
