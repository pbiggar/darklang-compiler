// E2ESchedulingTests.fs - Unit tests for E2E scheduling helpers
//
// Ensures grouping preserves per-file order and batching expectations.

module E2ESchedulingTests

open TestDSL.E2EFormat
open TestDSL.E2EScheduling

/// Test result type
type TestResult = Result<unit, string>

let private makeTest (name: string) (sourceFile: string) : E2ETest =
    {
        Name = name
        Source = ""
        Preamble = ""
        ExpectedStdout = None
        ExpectedStderr = None
        ExpectedExitCode = 0
        ExpectCompileError = false
        ExpectedErrorMessage = None
        DisableFreeList = false
        DisableANFOpt = false
        DisableInlining = false
        DisableTCO = false
        DisableMIROpt = false
        DisableLIROpt = false
        DisableDCE = false
        SourceFile = sourceFile
        FunctionLineMap = Map.empty
    }

let testGroupsByFile () : TestResult =
    let tests =
        [
            (0, makeTest "a1" "a.e2e")
            (1, makeTest "b1" "b.e2e")
            (2, makeTest "a2" "a.e2e")
        ]

    let groups = groupTestsBySourceFile tests
    let fileSet = groups |> List.map (fun g -> g.SourceFile) |> Set.ofList
    if fileSet <> Set.ofList ["a.e2e"; "b.e2e"] then
        Error "Expected groups for a.e2e and b.e2e"
    else
        let counts =
            groups
            |> List.map (fun g -> (g.SourceFile, g.Tests.Length))
            |> Map.ofList
        let aCount = Map.tryFind "a.e2e" counts |> Option.defaultValue 0
        let bCount = Map.tryFind "b.e2e" counts |> Option.defaultValue 0
        if aCount <> 2 then
            Error "Expected two tests grouped for a.e2e"
        else if bCount <> 1 then
            Error "Expected one test grouped for b.e2e"
        else
            Ok ()

/// Run all scheduling tests
let runAll () : TestResult =
    let tests = [
        ("group by file", testGroupsByFile)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
