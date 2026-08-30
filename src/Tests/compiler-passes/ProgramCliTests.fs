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

let tests = [
    ("parse explicit Linux x86_64 target", testExplicitLinuxX86_64Target)
    ("reject unknown compiler target", testUnknownTargetRejected)
    ("reject cross-target run mode", testCrossTargetRunRejected)
    ("parse explicit file-result mode", testEmitResultModeIsExplicit)
]
