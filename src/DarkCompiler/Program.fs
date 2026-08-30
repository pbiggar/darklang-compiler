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
//   6. CodeGen: LIR → target ISA instructions
//   7. Encoding: target ISA instructions → machine code
//   8. Binary generation: machine code → platform executable

module Program

open System
open System.IO
open Output

/// Output verbosity level
/// 0 = Quiet (no output)
/// 1 = Normal (standard output)
/// 2 = Verbose (show pass names)
/// 3 = VeryVerbose (show pass names + timing)
/// 4 = DumpIR (show all intermediate representations)
type VerbosityLevel = Quiet | Normal | Verbose | VeryVerbose | DumpIR

/// Select the compiler backend independently from the process architecture.
/// Explicit targets are compile-only because the CLI does not emulate them.
type TargetSelection =
    | HostTarget
    | ExplicitTarget of Platform.Target

/// Convert VerbosityLevel to integer for library
/// Library verbosity: 0=silent, 1=pass names, 2=pass names + timing, 3=dump all IRs
let verbosityToInt (level: VerbosityLevel) : int =
    match level with
    | Quiet -> 0      // No output
    | Normal -> 0     // CLI handles output, library silent
    | Verbose -> 1    // Library shows pass names
    | VeryVerbose -> 2 // Library shows pass names + timing
    | DumpIR -> 3     // Library dumps all IRs

/// Determine whether the CLI should emit normal output for a verbosity level
let shouldShowNormal (level: VerbosityLevel) : bool =
    match level with
    | Quiet -> false
    | Normal | Verbose | VeryVerbose | DumpIR -> true

/// Parsed CLI options
type CliOptions = {
    Run: bool                    // True = run, False = compile (default)
    IsExpression: bool           // True = expression, False = file (default)
    SourceSyntax: CompilerLibrary.SourceSyntax
    OutputFile: string option
    Verbosity: VerbosityLevel
    Help: bool
    Version: bool
    Argument: string option
    LeakCheck: bool
    Target: TargetSelection
    EmitResult: bool
    // Compiler-owned sources may use private runtime and HAMT helpers.
    AllowInternal: bool
    // Optimization flags
    DisableFreeList: bool
    DisableANFOpt: bool
    DisableANFConstFolding: bool
    DisableANFConstProp: bool
    DisableANFCopyProp: bool
    DisableANFDCE: bool
    DisableANFStrengthReduction: bool
    DisableInlining: bool
    DisableTCO: bool
    DisableMIROpt: bool
    DisableMIRConstFolding: bool
    DisableMIRCSE: bool
    DisableMIRCopyProp: bool
    DisableMIRDCE: bool
    DisableMIRCFGSimplify: bool
    DisableMIRLICM: bool
    DisableLIROpt: bool
    DisableLIRPeephole: bool
    DisableFunctionTreeShaking: bool
    // IR dump flags
    DumpANF: bool
    DumpMIR: bool
    DumpLIR: bool
}

/// Default empty options
let defaultOptions = {
    Run = false
    IsExpression = false
    SourceSyntax = CompilerLibrary.CompilerSyntax
    OutputFile = None
    Verbosity = Normal
    Help = false
    Version = false
    Argument = None
    LeakCheck = false
    Target = HostTarget
    EmitResult = false
    AllowInternal = false
    DisableFreeList = false
    DisableANFOpt = false
    DisableANFConstFolding = false
    DisableANFConstProp = false
    DisableANFCopyProp = false
    DisableANFDCE = false
    DisableANFStrengthReduction = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableMIRConstFolding = false
    DisableMIRCSE = false
    DisableMIRCopyProp = false
    DisableMIRDCE = false
    DisableMIRCFGSimplify = false
    DisableMIRLICM = false
    DisableLIROpt = false
    DisableLIRPeephole = false
    DisableFunctionTreeShaking = false
    DumpANF = false
    DumpMIR = false
    DumpLIR = false
}

let private parseSourceSyntaxValue (value: string) : Result<CompilerLibrary.SourceSyntax, string> =
    match value.Trim().ToLowerInvariant() with
    | "compiler" -> Ok CompilerLibrary.CompilerSyntax
    | "interpreter" -> Ok CompilerLibrary.InterpreterSyntax
    | _ -> Error $"Invalid syntax '{value}' (expected 'compiler' or 'interpreter')"

let private parseTargetValue (value: string) : Result<TargetSelection, string> =
    match value.Trim().ToLowerInvariant() with
    | "linux-x86_64" -> Ok (ExplicitTarget Platform.LinuxX86_64)
    | _ -> Error $"Invalid target '{value}' (expected 'linux-x86_64')"

/// Build compiler options from CLI options
let buildCompilerOptions (cliOpts: CliOptions) : CompilerLibrary.CompilerOptions = {
    DisableFreeList = cliOpts.DisableFreeList
    DisableANFOpt = cliOpts.DisableANFOpt
    DisableANFConstFolding = cliOpts.DisableANFConstFolding
    DisableANFConstProp = cliOpts.DisableANFConstProp
    DisableANFCopyProp = cliOpts.DisableANFCopyProp
    DisableANFDCE = cliOpts.DisableANFDCE
    DisableANFStrengthReduction = cliOpts.DisableANFStrengthReduction
    DisableInlining = cliOpts.DisableInlining
    DisableTCO = cliOpts.DisableTCO
    DisableMIROpt = cliOpts.DisableMIROpt
    DisableMIRConstFolding = cliOpts.DisableMIRConstFolding
    DisableMIRCSE = cliOpts.DisableMIRCSE
    DisableMIRCopyProp = cliOpts.DisableMIRCopyProp
    DisableMIRDCE = cliOpts.DisableMIRDCE
    DisableMIRCFGSimplify = cliOpts.DisableMIRCFGSimplify
    DisableMIRLICM = cliOpts.DisableMIRLICM
    DisableLIROpt = cliOpts.DisableLIROpt
    DisableLIRPeephole = cliOpts.DisableLIRPeephole
    DisableFunctionTreeShaking = cliOpts.DisableFunctionTreeShaking
    EnableCoverage = false
    EnableLeakCheck = cliOpts.LeakCheck
    Warnings = CompilerLibrary.defaultWarningSettings
    DumpANF = cliOpts.DumpANF
    DumpMIR = cliOpts.DumpMIR
    DumpLIR = cliOpts.DumpLIR
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

        | "--syntax" :: value :: rest ->
            parseSourceSyntaxValue value
            |> Result.bind (fun sourceSyntax ->
                parseFlags rest { opts with SourceSyntax = sourceSyntax } lastVerbosity)

        | "--syntax" :: [] ->
            Error "Missing value for --syntax (expected 'compiler' or 'interpreter')"

        | flag :: rest when flag.StartsWith("--syntax=") ->
            let value = flag.Substring(9)
            parseSourceSyntaxValue value
            |> Result.bind (fun sourceSyntax ->
                parseFlags rest { opts with SourceSyntax = sourceSyntax } lastVerbosity)

        | "--compiler-syntax" :: rest ->
            parseFlags rest { opts with SourceSyntax = CompilerLibrary.CompilerSyntax } lastVerbosity

        | "--interpreter-syntax" :: rest ->
            parseFlags rest { opts with SourceSyntax = CompilerLibrary.InterpreterSyntax } lastVerbosity

        | "--target" :: value :: rest ->
            match opts.Target with
            | ExplicitTarget _ -> Error "Target specified multiple times"
            | HostTarget ->
                parseTargetValue value
                |> Result.bind (fun target ->
                    parseFlags rest { opts with Target = target } lastVerbosity)

        | "--target" :: [] ->
            Error "Missing value for --target (expected 'linux-x86_64')"

        | flag :: rest when flag.StartsWith("--target=") ->
            match opts.Target with
            | ExplicitTarget _ -> Error "Target specified multiple times"
            | HostTarget ->
                let value = flag.Substring(9)
                parseTargetValue value
                |> Result.bind (fun target ->
                    parseFlags rest { opts with Target = target } lastVerbosity)

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
            // Stack -v flags: -v = Verbose, -vv = VeryVerbose, -vvv = DumpIR
            let newVerbosity =
                match lastVerbosity with
                | Quiet -> Normal
                | Normal -> Verbose
                | Verbose -> VeryVerbose
                | VeryVerbose -> DumpIR
                | DumpIR -> DumpIR
            parseFlags rest opts newVerbosity

        | "--dump-anf" :: rest ->
            parseFlags rest { opts with DumpANF = true } lastVerbosity

        | "--dump-mir" :: rest ->
            parseFlags rest { opts with DumpMIR = true } lastVerbosity

        | "--dump-lir" :: rest ->
            parseFlags rest { opts with DumpLIR = true } lastVerbosity

        | "--leak-check" :: rest ->
            parseFlags rest { opts with LeakCheck = true } lastVerbosity

        | "--allow-internal" :: rest ->
            // Deliberately omitted from user-facing help. This mode exists for
            // compiler-owned stdlib, regression, and benchmark sources only.
            parseFlags rest { opts with AllowInternal = true } lastVerbosity

        | "--emit-result" :: rest ->
            parseFlags rest { opts with EmitResult = true } lastVerbosity

        | "-h" :: rest | "--help" :: rest ->
            parseFlags rest { opts with Help = true } lastVerbosity

        | "--version" :: rest ->
            parseFlags rest { opts with Version = true } lastVerbosity

        | "--no-free-list" :: rest | "--disable-opt-freelist" :: rest ->
            parseFlags rest { opts with DisableFreeList = true } lastVerbosity

        | "--disable-opt-anf" :: rest ->
            parseFlags rest { opts with DisableANFOpt = true } lastVerbosity

        | "--disable-opt-anf-const-folding" :: rest ->
            parseFlags rest { opts with DisableANFConstFolding = true } lastVerbosity

        | "--disable-opt-anf-const-prop" :: rest ->
            parseFlags rest { opts with DisableANFConstProp = true } lastVerbosity

        | "--disable-opt-anf-copy-prop" :: rest ->
            parseFlags rest { opts with DisableANFCopyProp = true } lastVerbosity

        | "--disable-opt-anf-dce" :: rest ->
            parseFlags rest { opts with DisableANFDCE = true } lastVerbosity

        | "--disable-opt-anf-strength-reduction" :: rest ->
            parseFlags rest { opts with DisableANFStrengthReduction = true } lastVerbosity

        | "--disable-opt-inline" :: rest ->
            parseFlags rest { opts with DisableInlining = true } lastVerbosity

        | "--disable-opt-tco" :: rest ->
            parseFlags rest { opts with DisableTCO = true } lastVerbosity

        | "--disable-opt-mir" :: rest ->
            parseFlags rest { opts with DisableMIROpt = true } lastVerbosity

        | "--disable-opt-mir-const-folding" :: rest ->
            parseFlags rest { opts with DisableMIRConstFolding = true } lastVerbosity

        | "--disable-opt-mir-cse" :: rest ->
            parseFlags rest { opts with DisableMIRCSE = true } lastVerbosity

        | "--disable-opt-mir-copy-prop" :: rest ->
            parseFlags rest { opts with DisableMIRCopyProp = true } lastVerbosity

        | "--disable-opt-mir-dce" :: rest ->
            parseFlags rest { opts with DisableMIRDCE = true } lastVerbosity

        | "--disable-opt-mir-cfg-simplify" :: rest ->
            parseFlags rest { opts with DisableMIRCFGSimplify = true } lastVerbosity

        | "--disable-opt-mir-licm" :: rest ->
            parseFlags rest { opts with DisableMIRLICM = true } lastVerbosity

        | "--disable-opt-lir" :: rest ->
            parseFlags rest { opts with DisableLIROpt = true } lastVerbosity

        | "--disable-opt-lir-peephole" :: rest ->
            parseFlags rest { opts with DisableLIRPeephole = true } lastVerbosity

        | "--disable-opt-function-tree-shaking" :: rest ->
            parseFlags rest { opts with DisableFunctionTreeShaking = true } lastVerbosity

        | "--disable-opt-dce" :: rest ->
            parseFlags rest { opts with DisableFunctionTreeShaking = true } lastVerbosity

        | "-" :: rest ->
            // Special case: "-" means stdin
            if opts.Argument.IsSome then
                Error "Cannot specify multiple input sources"
            else
                parseFlags rest { opts with Argument = Some "-" } lastVerbosity

        | flag :: rest when flag.StartsWith("-") && not (flag.StartsWith("--")) && flag.Length > 1 ->
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
        else if opts.Run && opts.Target <> HostTarget then
            Error "Explicit compiler targets are compile-only; remove --run"
        else
            Ok opts

let private sourceFileForDiagnostics (cliOpts: CliOptions) : string =
    match cliOpts.IsExpression, cliOpts.Argument with
    | true, _ -> ""
    | false, Some sourceFile -> sourceFile
    | false, None ->
        Crash.crash "sourceFileForDiagnostics: compile/run called without a validated input source"

/// Compile source expression to executable
let compile (source: string) (outputPath: string) (verbosity: VerbosityLevel) (cliOpts: CliOptions) : int =
    let showNormal = shouldShowNormal verbosity

    if showNormal then
        println $"Compiling: {source}"

    // Use library for compilation
    let options = buildCompilerOptions cliOpts
    let sourceFile = sourceFileForDiagnostics cliOpts

    let selectedTarget =
        match cliOpts.Target with
        | HostTarget -> Platform.detectHostTarget ()
        | ExplicitTarget target -> Ok target

    match selectedTarget with
    | Error err ->
        eprintln $"Target detection failed: {err}"
        1
    | Ok target ->
        match CompilerLibrary.buildStdlib target with
        | Error err ->
            eprintln $"Compilation failed: {err}"
            1
        | Ok stdlib ->
            let request : CompilerLibrary.CompileRequest = {
                Context = CompilerLibrary.StdlibOnly stdlib
                Mode =
                    if cliOpts.IsExpression || cliOpts.EmitResult then CompilerLibrary.CompileMode.TestExpression
                    else CompilerLibrary.CompileMode.FullProgram
                SourceSyntax = cliOpts.SourceSyntax
                Sources =
                    AST.NonEmptyList.singleton
                        { CompilerLibrary.SourceUnit.Name = sourceFile
                          Purpose = NameSyntax.SourceUnitPurpose.Executable
                          Source = source }
                AllowInternal = cliOpts.AllowInternal
                Verbosity = verbosityToInt verbosity
                Options = options
                PackageValues = CompilerLibrary.emptyPackageValueCatalog
                PassTimingRecorder = None
                Session = None
            }
            let compileReport = CompilerLibrary.compile request
            match compileReport.Result with
            | Error err ->
                eprintln $"Compilation failed: {err}"
                1
            | Ok binary ->
                let writeResult =
                    match compileReport.Target with
                    | Platform.ARM64Backend Platform.MacOSARM64 ->
                        Binary_Generation_MachO.writeToFile outputPath binary
                    | Platform.ARM64Backend Platform.LinuxARM64
                    | Platform.LinuxX86_64 ->
                        Binary_Generation_ELF.writeToFile outputPath binary
                match writeResult with
                | Error err ->
                    eprintln $"Failed to write binary: {err}"
                    1
                | Ok () ->
                    if showNormal then println $"Successfully wrote {binary.Length} bytes to {outputPath}"
                    0

/// Run an expression (compile to temp and execute)
let run (source: string) (verbosity: VerbosityLevel) (cliOpts: CliOptions) : int =
    let showNormal = shouldShowNormal verbosity

    if showNormal then
        println $"Compiling and running: {source}"
        println "---"

    // Use library for compile and run
    let options = buildCompilerOptions cliOpts
    let execResult : CompilerLibrary.ExecutionOutput =
        let sourceFile = sourceFileForDiagnostics cliOpts

        match Platform.detectHostTarget () with
        | Error err ->
            { ExitCode = 1
              Stdout = ""
              Stderr = $"Target detection failed: {err}"
              RuntimeTime = TimeSpan.Zero }
        | Ok target ->
            match CompilerLibrary.buildStdlib target with
            | Error err ->
                { ExitCode = 1
                  Stdout = ""
                  Stderr = err
                  RuntimeTime = TimeSpan.Zero }
            | Ok stdlib ->
                let request : CompilerLibrary.CompileRequest = {
                    Context = CompilerLibrary.StdlibOnly stdlib
                    Mode =
                        if cliOpts.IsExpression || cliOpts.EmitResult then CompilerLibrary.CompileMode.TestExpression
                        else CompilerLibrary.CompileMode.FullProgram
                    SourceSyntax = cliOpts.SourceSyntax
                    Sources =
                        AST.NonEmptyList.singleton
                            { CompilerLibrary.SourceUnit.Name = sourceFile
                              Purpose = NameSyntax.SourceUnitPurpose.Executable
                              Source = source }
                    AllowInternal = cliOpts.AllowInternal
                    Verbosity = verbosityToInt verbosity
                    Options = options
                    PackageValues = CompilerLibrary.emptyPackageValueCatalog
                    PassTimingRecorder = None
                    Session = None
                }
                let compileReport = CompilerLibrary.compile request
                match compileReport.Result with
                | Error err ->
                    { ExitCode = 1
                      Stdout = ""
                      Stderr = err
                      RuntimeTime = TimeSpan.Zero }
                | Ok binary ->
                    CompilerLibrary.executeAttached compileReport.Target (verbosityToInt verbosity) binary

    if showNormal then
        println "---"
        println $"Exit code: {execResult.ExitCode}"
    if execResult.Stderr <> "" then
        eprintln $"{execResult.Stderr}"

    execResult.ExitCode

/// Print version information
let versionLines () : string list =
    [
        "Dark Compiler v0.1.0"
        "Darklang native compiler for macOS and Linux"
    ]

let printVersion () =
    versionLines () |> List.iter println

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
    println "  --syntax MODE        Source syntax: compiler (default) or interpreter"
    println "  --interpreter-syntax Alias for --syntax=interpreter"
    println "  --target TARGET      Compile for linux-x86_64 instead of the host"
    println "  --emit-result        Print a file's final expression result when executed"
    println "  -o, --output FILE    Output file (default: dark.out)"
    println "  -q, --quiet          Suppress compilation output"
    println "  -v, --verbose        Show compilation pass names"
    println "  -vv                  Show pass names + timing details"
    println "  -vvv                 Dump all intermediate representations"
    println "  --dump-anf           Dump ANF (all ANF stages)"
    println "  --dump-mir           Dump MIR (control-flow graph)"
    println "  --dump-lir           Dump LIR (before and after register allocation)"
    println "  --leak-check         Enable leak checking (debug builds only)"
    println "  -h, --help           Show this help message"
    println "  --version            Show version information"
    println ""
    println "Optimization flags (for debugging):"
    println "  --disable-opt-anf       Disable ANF-level optimizations"
    println "  --disable-opt-inline    Disable function inlining"
    println "  --disable-opt-tco       Disable tail call optimization"
    println "  --disable-opt-mir       Disable MIR-level optimizations"
    println "  --disable-opt-lir       Disable LIR-level optimizations"
    println "  --disable-opt-function-tree-shaking  Disable function tree shaking"
    println "  --disable-opt-dce       Alias for --disable-opt-function-tree-shaking"
    println "  --disable-opt-freelist  Disable free list memory reuse"
    println ""
    println "Flags can appear in any order and can be combined (e.g., -qr, -re, -vvre)"
    println "Verbosity levels: (none)=normal, -v=passes, -vv=passes+timing, -vvv=dump IRs"
    println ""
    println "Examples:"
    println "  dark prog.dark                     Compile file to 'dark.out'"
    println "  dark prog.dark -o output           Compile file to 'output'"
    println "  dark -r prog.dark                  Compile and run file"
    printf "  dark -e \"2 + 3\"                    Compile expression to 'dark.out'\n"
    printf "  dark -e \"2 + 3\" -o output          Compile expression to 'output'\n"
    printf "  dark -r -e \"2 + 3\"                 Run and show exit code (5)\n"
    printf "  dark -qr -e \"6 * 7\"                Run quietly (exit code: 42)\n"
    println "  dark --syntax=interpreter -e \"let x = 5L in x\""
    println "  dark --target=linux-x86_64 prog.dark -o prog-x86_64"
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
                    run source options.Verbosity options
                else
                    // Compile mode (default)
                    let output = options.OutputFile |> Option.defaultValue "dark.out"
                    compile source output options.Verbosity options

            | Error msg ->
                println $"Error: {msg}"
                1

    with ex ->
        println $"Error: {ex.Message}"
        println $"{ex.StackTrace}"
        1
