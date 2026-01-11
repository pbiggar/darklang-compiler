// ScriptHelperTests.fs - Unit tests for shared test script helpers
//
// Ensures the test runner scripts rely on scripts/test-common.sh helpers.

module ScriptHelperTests

open System.IO

type TestResult = Result<unit, string>

let private repoRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let private readFile (path: string) : Result<string, string> =
    try Ok (File.ReadAllText(path))
    with ex -> Error $"Failed to read {path}: {ex.Message}"

let private requireContains (path: string) (needle: string) (text: string) : TestResult =
    if text.Contains needle then
        Ok ()
    else
        Error $"Expected {path} to contain \"{needle}\""

let private requireAll (path: string) (needles: string list) (text: string) : TestResult =
    let rec check remaining =
        match remaining with
        | [] -> Ok ()
        | needle :: rest ->
            match requireContains path needle text with
            | Ok () -> check rest
            | Error msg -> Error msg
    check needles

let private requireNotContains (path: string) (needle: string) (text: string) : TestResult =
    if text.Contains needle then
        Error $"Expected {path} to not contain \"{needle}\""
    else
        Ok ()

let private scriptPath (name: string) : string =
    Path.Combine(repoRoot, name)

let testRunTestsUsesCommonHelper () : TestResult =
    let path = scriptPath "run-tests"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        requireAll path [ "test-common.sh"; "build_tests"; "find_test_exe"; "run_tests" ] text

let testRunVerificationUsesCommonHelper () : TestResult =
    let path = scriptPath "run-verification-tests"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        requireAll path [ "test-common.sh"; "build_tests"; "find_test_exe"; "run_tests" ] text

let testRunVerificationDoesNotUseEnvVar () : TestResult =
    let path = scriptPath "run-verification-tests"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        requireNotContains path "ENABLE_VERIFICATION_TESTS" text

let testRunnerDoesNotUseEnvVar () : TestResult =
    let path = Path.Combine(repoRoot, "src", "Tests", "TestRunner.fs")
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        requireNotContains path "ENABLE_VERIFICATION_TESTS" text

let testCompilerCachingDoesNotUseEnvVar () : TestResult =
    let path = Path.Combine(repoRoot, "src", "Tests", "CompilerCachingTests.fs")
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        requireNotContains path "ENABLE_VERIFICATION_TESTS" text

let testRunDarkCoverageUsesCommonHelper () : TestResult =
    let path = scriptPath "run-dark-coverage"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        requireAll path [ "test-common.sh"; "build_tests"; "find_test_exe"; "run_tests" ] text

let tests = [
    ("run-tests uses test-common", testRunTestsUsesCommonHelper)
    ("run-verification-tests uses test-common", testRunVerificationUsesCommonHelper)
    ("run-verification-tests avoids env vars", testRunVerificationDoesNotUseEnvVar)
    ("test runner avoids env vars", testRunnerDoesNotUseEnvVar)
    ("compiler caching tests avoid env vars", testCompilerCachingDoesNotUseEnvVar)
    ("run-dark-coverage uses test-common", testRunDarkCoverageUsesCommonHelper)
]
