// TestRunnerSchedulingTests.fs - Unit tests for test runner scheduling helpers
//
// Validates E2E ordering and stdlib scheduling decisions for faster runs.

module TestRunnerSchedulingTests

open TestDSL.E2EFormat
open TestRunnerScheduling

type TestResult = Result<unit, string>

let private makeE2ETest (name: string) (source: string) (preamble: string) (sourceFile: string) : E2ETest =
    {
        Name = name
        Source = source
        Preamble = preamble
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

let testOrderE2ETestsByEstimatedCost () : TestResult =
    let highCost = makeE2ETest "high" "aaaa" "p" "c.e2e"
    let tieAlpha = makeE2ETest "alpha" "abc" "" "b.e2e"
    let tieBeta = makeE2ETest "beta" "def" "" "a.e2e"
    let lowCost = makeE2ETest "low" "x" "" "z.e2e"
    let ordered =
        [| tieAlpha; lowCost; highCost; tieBeta |]
        |> orderE2ETestsByEstimatedCost
    let orderedNames = ordered |> Array.map (fun test -> test.Name) |> Array.toList
    let expected = [ "high"; "beta"; "alpha"; "low" ]
    if orderedNames = expected then
        Ok ()
    else
        Error $"Expected E2E order {expected}, got {orderedNames}"

let testSplitUnitTestsByStdlibNeed () : TestResult =
    let suites : UnitTestSuite array = [|
        ("CLI Flags Tests", 1, fun () -> Ok ())
        ("Compiler Caching Tests", 1, fun () -> Ok ())
        ("Parallel Move Tests", 1, fun () -> Ok ())
    |]
    let (noStdlib, needsStdlib) =
        splitUnitTestsByStdlibNeed [ "Compiler Caching Tests" ] suites
    let noStdlibNames = noStdlib |> Array.map (fun (name, _, _) -> name) |> Array.toList
    let needsStdlibNames = needsStdlib |> Array.map (fun (name, _, _) -> name) |> Array.toList
    if noStdlibNames = [ "CLI Flags Tests"; "Parallel Move Tests" ] && needsStdlibNames = [ "Compiler Caching Tests" ] then
        Ok ()
    else
        Error $"Unexpected unit test split: no-stdlib={noStdlibNames}, stdlib={needsStdlibNames}"

let testShouldStartStdlibCompile () : TestResult =
    let cases = [
        (false, false, false, false)
        (true, false, false, true)
        (false, true, false, true)
        (false, false, true, true)
    ]
    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (hasE2E, hasVerification, needsUnitStdlib, expected) :: rest ->
            let actual = shouldStartStdlibCompile hasE2E hasVerification needsUnitStdlib
            if actual = expected then
                checkCases rest
            else
                Error $"Expected shouldStartStdlibCompile({hasE2E}, {hasVerification}, {needsUnitStdlib}) to be {expected}, got {actual}"
    checkCases cases

let testShouldRunUnitAndE2EInParallel () : TestResult =
    let cases = [
        (true, true, true)
        (true, false, false)
        (false, true, false)
        (false, false, false)
    ]
    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (hasUnit, hasE2E, expected) :: rest ->
            let actual = shouldRunUnitAndE2EInParallel hasUnit hasE2E
            if actual = expected then
                checkCases rest
            else
                Error $"Expected shouldRunUnitAndE2EInParallel({hasUnit}, {hasE2E}) to be {expected}, got {actual}"
    checkCases cases

let testStdlibWarmupPlan () : TestResult =
    match decideStdlibWarmupPlan true, decideStdlibWarmupPlan false with
    | CompileStdlibBeforeTests, SkipStdlibWarmup -> Ok ()
    | actual ->
        Error $"Expected CompileStdlibBeforeTests/SkipStdlibWarmup, got {actual}"

let testCalculateOptimalParallelism () : TestResult =
    let actual = calculateOptimalParallelism 8 64.0
    if actual = 14 then
        Ok ()
    else
        Error $"Expected calculateOptimalParallelism to return 14, got {actual}"

let runAll () : TestResult =
    let tests = [
        ("E2E ordering", testOrderE2ETestsByEstimatedCost)
        ("Unit test split", testSplitUnitTestsByStdlibNeed)
        ("Stdlib compile decision", testShouldStartStdlibCompile)
        ("Parallel suite decision", testShouldRunUnitAndE2EInParallel)
        ("Stdlib warmup plan", testStdlibWarmupPlan)
        ("Parallelism calculation", testCalculateOptimalParallelism)
    ]

    let rec runTests remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
