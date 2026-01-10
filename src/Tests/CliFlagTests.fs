// CliFlagTests.fs - Unit tests for CLI flag parsing
//
// Verifies that IR dump flags are accepted and mapped into CLI options.

module CliFlagTests

open Program

/// Test result type
type TestResult = Result<unit, string>

/// Test that IR dump flags parse and set the expected options
let testDumpIrFlags () : TestResult =
    let args = [| "--dump-anf"; "--dump-mir"; "--dump-lir"; "input.dark" |]
    match parseArgs args with
    | Error msg -> Error $"Expected dump flags to parse, got error: {msg}"
    | Ok opts ->
        if not opts.DumpANF then
            Error "Expected DumpANF to be true"
        else if not opts.DumpMIR then
            Error "Expected DumpMIR to be true"
        else if not opts.DumpLIR then
            Error "Expected DumpLIR to be true"
        else
            Ok ()

/// Test that verbosity levels map to the expected normal output behavior
let testShouldShowNormal () : TestResult =
    if Program.shouldShowNormal Quiet then
        Error "Expected Quiet to suppress normal output"
    else if not (Program.shouldShowNormal Normal) then
        Error "Expected Normal to show output"
    else if not (Program.shouldShowNormal Verbose) then
        Error "Expected Verbose to show output"
    else if not (Program.shouldShowNormal VeryVerbose) then
        Error "Expected VeryVerbose to show output"
    else if not (Program.shouldShowNormal DumpIR) then
        Error "Expected DumpIR to show output"
    else
        Ok ()

/// Test compiler option mapping from CLI options
let testBuildCompilerOptions () : TestResult =
    let cliOpts = {
        defaultOptions with
            DisableFreeList = true
            DisableANFOpt = true
            DisableInlining = true
            DisableTCO = true
            DisableMIROpt = true
            DisableLIROpt = true
            DisableDCE = true
            DumpANF = true
            DumpMIR = true
            DumpLIR = true
    }
    let compilerOpts = Program.buildCompilerOptions cliOpts
    if not compilerOpts.DisableFreeList then
        Error "Expected DisableFreeList to map into CompilerOptions"
    else if not compilerOpts.DisableANFOpt then
        Error "Expected DisableANFOpt to map into CompilerOptions"
    else if not compilerOpts.DisableInlining then
        Error "Expected DisableInlining to map into CompilerOptions"
    else if not compilerOpts.DisableTCO then
        Error "Expected DisableTCO to map into CompilerOptions"
    else if not compilerOpts.DisableMIROpt then
        Error "Expected DisableMIROpt to map into CompilerOptions"
    else if not compilerOpts.DisableLIROpt then
        Error "Expected DisableLIROpt to map into CompilerOptions"
    else if not compilerOpts.DisableDCE then
        Error "Expected DisableDCE to map into CompilerOptions"
    else if not compilerOpts.DumpANF then
        Error "Expected DumpANF to map into CompilerOptions"
    else if not compilerOpts.DumpMIR then
        Error "Expected DumpMIR to map into CompilerOptions"
    else if not compilerOpts.DumpLIR then
        Error "Expected DumpLIR to map into CompilerOptions"
    else if compilerOpts.EnableCoverage then
        Error "Expected EnableCoverage to remain false"
    else
        Ok ()

let tests = [
    ("IR dump flags", testDumpIrFlags)
    ("show normal output", testShouldShowNormal)
    ("build compiler options", testBuildCompilerOptions)
]

/// Run all CLI flag unit tests
let runAll () : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
