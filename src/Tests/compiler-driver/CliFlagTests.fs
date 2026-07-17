// CliFlagTests.fs - Unit tests for CLI flag parsing
//
// Verifies that IR dump flags are accepted and mapped into CLI options.

module CliFlagTests

open Program

/// Test result type
type TestResult = Result<unit, string>

let private ensure (condition: bool) (message: string) : TestResult =
    if condition then Ok () else Error message

let private parseOk (label: string) (args: string array) =
    match parseArgs args with
    | Ok opts -> Ok opts
    | Error msg -> Error $"Expected {label} to parse, got error: {msg}"

let private parseRejected (label: string) (args: string array) : TestResult =
    match parseArgs args with
    | Ok _ -> Error $"Expected {label} to be rejected"
    | Error _ -> Ok ()

let testVersionText () : TestResult =
    Program.versionLines ()
    |> List.contains "Darklang native compiler for macOS and Linux"
    |> fun found ->
        ensure found "Expected version text to describe the supported macOS and Linux native compiler"

/// Single CLI test that validates all supported flag parsing behaviors.
let testCliFlags () : TestResult =
    parseOk "dump flags" [| "--dump-anf"; "--dump-mir"; "--dump-lir"; "input.dark" |]
    |> Result.bind (fun opts ->
        ensure opts.DumpANF "Expected DumpANF to be true"
        |> Result.bind (fun () -> ensure opts.DumpMIR "Expected DumpMIR to be true")
        |> Result.bind (fun () -> ensure opts.DumpLIR "Expected DumpLIR to be true"))
    |> Result.bind (fun () ->
        ensure (not (Program.shouldShowNormal Quiet)) "Expected Quiet to suppress normal output"
        |> Result.bind (fun () -> ensure (Program.shouldShowNormal Normal) "Expected Normal to show output")
        |> Result.bind (fun () -> ensure (Program.shouldShowNormal Verbose) "Expected Verbose to show output")
        |> Result.bind (fun () -> ensure (Program.shouldShowNormal VeryVerbose) "Expected VeryVerbose to show output")
        |> Result.bind (fun () -> ensure (Program.shouldShowNormal DumpIR) "Expected DumpIR to show output"))
    |> Result.bind (fun () ->
        parseOk "leak-check flag" [| "--leak-check"; "input.dark" |]
        |> Result.bind (fun opts -> ensure opts.LeakCheck "Expected LeakCheck to be true"))
    |> Result.bind (fun () ->
        parseOk
            "ANF optimization flags"
            [|
                "--disable-opt-anf-const-folding"
                "--disable-opt-anf-const-prop"
                "--disable-opt-anf-copy-prop"
                "--disable-opt-anf-dce"
                "--disable-opt-anf-strength-reduction"
                "input.dark"
            |]
        |> Result.bind (fun opts ->
            ensure opts.DisableANFConstFolding "Expected DisableANFConstFolding to be true"
            |> Result.bind (fun () -> ensure opts.DisableANFConstProp "Expected DisableANFConstProp to be true")
            |> Result.bind (fun () -> ensure opts.DisableANFCopyProp "Expected DisableANFCopyProp to be true")
            |> Result.bind (fun () -> ensure opts.DisableANFDCE "Expected DisableANFDCE to be true")
            |> Result.bind (fun () ->
                ensure opts.DisableANFStrengthReduction "Expected DisableANFStrengthReduction to be true")))
    |> Result.bind (fun () ->
        parseOk
            "MIR optimization flags"
            [|
                "--disable-opt-mir-const-folding"
                "--disable-opt-mir-cse"
                "--disable-opt-mir-copy-prop"
                "--disable-opt-mir-dce"
                "--disable-opt-mir-cfg-simplify"
                "--disable-opt-mir-licm"
                "input.dark"
            |]
        |> Result.bind (fun opts ->
            ensure opts.DisableMIRConstFolding "Expected DisableMIRConstFolding to be true"
            |> Result.bind (fun () -> ensure opts.DisableMIRCSE "Expected DisableMIRCSE to be true")
            |> Result.bind (fun () -> ensure opts.DisableMIRCopyProp "Expected DisableMIRCopyProp to be true")
            |> Result.bind (fun () -> ensure opts.DisableMIRDCE "Expected DisableMIRDCE to be true")
            |> Result.bind (fun () ->
                ensure opts.DisableMIRCFGSimplify "Expected DisableMIRCFGSimplify to be true")
            |> Result.bind (fun () -> ensure opts.DisableMIRLICM "Expected DisableMIRLICM to be true")))
    |> Result.bind (fun () ->
        parseOk "LIR peephole flag" [| "--disable-opt-lir-peephole"; "input.dark" |]
        |> Result.bind (fun opts ->
            ensure opts.DisableLIRPeephole "Expected DisableLIRPeephole to be true"))
    |> Result.bind (fun () ->
        parseOk "function tree shaking flag" [| "--disable-opt-function-tree-shaking"; "input.dark" |]
        |> Result.bind (fun opts ->
            ensure opts.DisableFunctionTreeShaking "Expected DisableFunctionTreeShaking to be true"))
    |> Result.bind (fun () -> parseRejected "--no-cache" [| "--no-cache"; "input.dark" |])
    |> Result.bind (fun () -> parseRejected "--cache-key" [| "--cache-key" |])
    |> Result.bind (fun () ->
        parseOk "interpreter syntax flag" [| "--syntax=interpreter"; "input.dark" |]
        |> Result.bind (fun opts ->
            match opts.SourceSyntax with
            | CompilerLibrary.InterpreterSyntax -> Ok ()
            | CompilerLibrary.CompilerSyntax ->
                Error "Expected SourceSyntax to be InterpreterSyntax"))
    |> Result.bind (fun () ->
        parseOk "interpreter syntax alias flag" [| "--interpreter-syntax"; "input.dark" |]
        |> Result.bind (fun opts ->
            match opts.SourceSyntax with
            | CompilerLibrary.InterpreterSyntax -> Ok ()
            | CompilerLibrary.CompilerSyntax ->
                Error "Expected SourceSyntax to be InterpreterSyntax when using --interpreter-syntax"))
    |> Result.bind (fun () ->
        parseRejected "invalid syntax value" [| "--syntax=banana"; "input.dark" |])

let tests = [
    ("CLI version text", testVersionText)
    ("CLI flags", testCliFlags)
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
