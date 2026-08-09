// PlatformTests.fs - Tests for validated compiler target classification.
//
// These tests keep supported OS/architecture pairs explicit without depending
// on the host running the test suite.

module PlatformTests

type TestResult = Result<unit, string>

let private expectTarget
    (os: Platform.OS)
    (arch: Platform.Arch)
    (expected: Platform.Target)
    : TestResult =
    match Platform.targetFor os arch with
    | Ok actual when actual = expected -> Ok ()
    | Ok actual -> Error $"Expected target {expected}, got {actual}"
    | Error err -> Error $"Expected target {expected}, got error: {err}"

let testTargetForRepresentsSupportedPairs () : TestResult =
    expectTarget Platform.MacOS Platform.ARM64 (Platform.ARM64Backend Platform.MacOSARM64)
    |> Result.bind (fun () ->
        expectTarget Platform.Linux Platform.ARM64 (Platform.ARM64Backend Platform.LinuxARM64))
    |> Result.bind (fun () ->
        expectTarget Platform.Linux Platform.X86_64 Platform.LinuxX86_64)

let testTargetForRejectsMacOSX86_64 () : TestResult =
    match Platform.targetFor Platform.MacOS Platform.X86_64 with
    | Error _ -> Ok ()
    | Ok target -> Error $"Expected macOS x86_64 to be rejected, got {target}"

let tests = [
    ("targetFor represents supported pairs", testTargetForRepresentsSupportedPairs)
    ("targetFor rejects macOS x86_64", testTargetForRejectsMacOSX86_64)
]
