// ProgramCliTests.fs - Compiler CLI target-selection tests.

module ProgramCliTests

type TestResult = Result<unit, string>

let testExplicitLinuxX86_64Target () : TestResult =
    match Program.parseArgs [| "--target=linux-x86_64"; "program.dark" |] with
    | Ok options when options.Target = Program.ExplicitTarget Platform.LinuxX86_64 -> Ok ()
    | Ok options -> Error $"Expected explicit Linux x86_64 target, got {options.Target}"
    | Error error -> Error $"Expected target parsing to succeed, got: {error}"

let testUnknownTargetRejected () : TestResult =
    match Program.parseArgs [| "--target=windows-x86_64"; "program.dark" |] with
    | Error error when error.Contains "linux-x86_64" -> Ok ()
    | Error error -> Error $"Expected supported-target guidance, got: {error}"
    | Ok _ -> Error "Expected unknown target to be rejected"

let testCrossTargetRunRejected () : TestResult =
    match
        Program.parseArgs [| "--run"; "--target=linux-x86_64"; "program.dark" |]
        |> Result.bind Program.validateOptions
    with
    | Error error when error.Contains "compile-only" -> Ok ()
    | Error error -> Error $"Expected compile-only guidance, got: {error}"
    | Ok _ -> Error "Expected cross-target run mode to be rejected"

let testEmitResultModeIsExplicit () : TestResult =
    match Program.parseArgs [| "--emit-result"; "program.dark" |] with
    | Ok options when options.EmitResult -> Ok ()
    | Ok _ -> Error "Expected --emit-result to select observable file-result compilation"
    | Error error -> Error $"Expected --emit-result parsing to succeed, got: {error}"

let testPackageServerIsExplicit () : TestResult =
    match Program.parseArgs [| "--package-server=http://127.0.0.1:9090"; "program.dark" |] with
    | Ok options when
        options.PackageServer
        |> Option.exists (fun server -> server.AbsoluteUri = "http://127.0.0.1:9090/") ->
        Ok ()
    | Ok options -> Error $"Unexpected package server options: {options}"
    | Error error -> Error $"Expected package server parsing to succeed, got: {error}"

let testPackageServerRejectsNonHttpUrl () : TestResult =
    match Program.parseArgs [| "--package-server=file:///tmp/packages"; "program.dark" |] with
    | Error error when error.Contains "HTTP(S)" -> Ok ()
    | Error error -> Error $"Expected HTTP(S) package server guidance, got: {error}"
    | Ok _ -> Error "Expected a non-HTTP package server URL to be rejected"

let testScopedIRDumpOptions () : TestResult =
    match
        Program.parseArgs
            [| "--dump-anf"
               "--dump-function=map"
               "--dump-ir-summary"
               "--dump-ir-output=artifacts/map.ir"
               "program.dark" |]
        |> Result.bind Program.validateOptions
    with
    | Ok options when
        options.DumpANF
        && options.DumpFunction = Some "map"
        && options.DumpIRSummary
        && options.DumpIROutput = Some "artifacts/map.ir" ->
        Ok ()
    | Ok options -> Error $"Unexpected scoped IR dump options: {options}"
    | Error error -> Error $"Expected scoped IR dump options to parse, got: {error}"

let testIRDumpModifiersRequireDumpSelection () : TestResult =
    match
        Program.parseArgs [| "--dump-function=map"; "program.dark" |]
        |> Result.bind Program.validateOptions
    with
    | Error error when error.Contains "require an IR dump" -> Ok ()
    | Error error -> Error $"Expected IR dump selection guidance, got: {error}"
    | Ok _ -> Error "Expected a dump function filter without an IR selection to fail"

let testEmptyIRDumpValuesRejected () : TestResult =
    match Program.parseArgs [| "--dump-anf"; "--dump-function="; "program.dark" |] with
    | Error error when error.Contains "non-empty" -> Ok ()
    | Error error -> Error $"Expected non-empty dump filter guidance, got: {error}"
    | Ok _ -> Error "Expected an empty dump function filter to fail"

let testBatchCompileParsesIndependentOutputs () : TestResult =
    match
        Program.parseCommand
            [| "--batch"
               "--quiet"
               "--package-server=http://127.0.0.1:9090"
               "--"
               "first.dark"
               "first.out"
               "second.dark"
               "second.out" |]
    with
    | Ok (Program.BatchCommand options) ->
        match options.Input with
        | Program.CommandLineItems items ->
            match fst items :: snd items with
            | [ first; second ] when
                options.Verbosity = Program.Quiet
                && (options.PackageServer
                    |> Option.exists (fun server -> server.AbsoluteUri = "http://127.0.0.1:9090/"))
                && first.SourceFile = "first.dark"
                && first.OutputFile = "first.out"
                && second.SourceFile = "second.dark"
                && second.OutputFile = "second.out" ->
                Ok ()
            | items -> Error $"Unexpected batch compile items: {items}"
        | Program.ManifestFile path -> Error $"Expected command-line items, got manifest {path}"
    | Ok command -> Error $"Expected batch command, got: {command}"
    | Error error -> Error $"Expected batch command to parse, got: {error}"

let testBatchCompileRejectsMissingOutput () : TestResult =
    match Program.parseCommand [| "--batch"; "--"; "only-source.dark" |] with
    | Error error when error.Contains "output path" -> Ok ()
    | Error error -> Error $"Expected missing-output guidance, got: {error}"
    | Ok _ -> Error "Expected an unmatched batch source to be rejected"

let testBatchManifestKeepGoingParses () : TestResult =
    match
        Program.parseCommand
            [| "--batch"
               "--package-server=http://127.0.0.1:9090"
               "--manifest"
               "package-probes.json"
               "--keep-going"
               "--report"
               "package-report.jsonl" |]
    with
    | Ok (Program.BatchCommand options) ->
        match options.Input, options.ReportPath with
        | Program.ManifestFile "package-probes.json", Some "package-report.jsonl"
            when options.KeepGoing
                 && (options.PackageServer
                     |> Option.exists (fun server -> server.AbsoluteUri = "http://127.0.0.1:9090/")) ->
            Ok ()
        | _ -> Error $"Unexpected manifest batch options: {options}"
    | Ok command -> Error $"Expected batch command, got: {command}"
    | Error error -> Error $"Expected manifest batch compilation to parse, got: {error}"

let testBatchCompileAllowsCompilerOwnedSources () : TestResult =
    match
        Program.parseCommand
            [| "--batch"
               "--allow-internal"
               "--"
               "benchmark.dark"
               "benchmark.out" |]
    with
    | Ok (Program.BatchCommand options) when options.AllowInternal -> Ok ()
    | Ok command -> Error $"Expected internal batch compilation, got: {command}"
    | Error error -> Error $"Expected --allow-internal to parse in batch mode, got: {error}"

let tests = [
    ("parse explicit Linux x86_64 target", testExplicitLinuxX86_64Target)
    ("reject unknown compiler target", testUnknownTargetRejected)
    ("reject cross-target run mode", testCrossTargetRunRejected)
    ("parse explicit file-result mode", testEmitResultModeIsExplicit)
    ("parse explicit package server", testPackageServerIsExplicit)
    ("reject non-HTTP package server", testPackageServerRejectsNonHttpUrl)
    ("parse scoped IR dump options", testScopedIRDumpOptions)
    ("require an IR selection for dump modifiers", testIRDumpModifiersRequireDumpSelection)
    ("reject empty IR dump values", testEmptyIRDumpValuesRejected)
    ("parse independent batch compile outputs", testBatchCompileParsesIndependentOutputs)
    ("reject batch source without output", testBatchCompileRejectsMissingOutput)
    ("parse keep-going manifest batch", testBatchManifestKeepGoingParses)
    ("allow compiler-owned sources in batch mode", testBatchCompileAllowsCompilerOwnedSources)
]
