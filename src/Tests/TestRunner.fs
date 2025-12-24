// TestRunner.fs - Standalone test runner for DSL-based tests
//
// Discovers and runs test files from passes/ directories.

module TestRunner.Main

open System
open System.IO
open System.Reflection
open System.Diagnostics
open TestDSL.PassTestRunner
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner

// ANSI color codes
module Colors =
    let reset = "\x1b[0m"
    let green = "\x1b[32m"
    let red = "\x1b[31m"
    let yellow = "\x1b[33m"
    let cyan = "\x1b[36m"
    let gray = "\x1b[90m"
    let bold = "\x1b[1m"

// Format elapsed time
let formatTime (elapsed: TimeSpan) =
    if elapsed.TotalMilliseconds < 1000.0 then
        sprintf "%.0fms" elapsed.TotalMilliseconds
    else
        sprintf "%.2fs" elapsed.TotalSeconds

// Parallel map with limited degree of parallelism
let parallelMapWithLimit (maxDegree: int) (f: 'a -> 'b) (array: 'a array) : 'b array =
    let results = Array.zeroCreate array.Length
    let options = System.Threading.Tasks.ParallelOptions()
    options.MaxDegreeOfParallelism <- maxDegree

    System.Threading.Tasks.Parallel.For(
        0,
        array.Length,
        options,
        fun i ->
            results.[i] <- f array.[i]
    ) |> ignore

    results

[<EntryPoint>]
let main args =
    let totalTimer = Stopwatch.StartNew()

    printfn "%s%s🧪 Running DSL-based Tests%s%s" Colors.bold Colors.cyan Colors.reset ""
    printfn ""

    // Get the directory where the assembly is located (where test files are copied)
    let assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let mutable passed = 0
    let mutable failed = 0

    // Run ANF→MIR tests
    let anf2mirDir = Path.Combine(assemblyDir, "passes/anf2mir")
    if Directory.Exists anf2mirDir then
        let anf2mirTests = Directory.GetFiles(anf2mirDir, "*.anf2mir")
        if anf2mirTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            printfn "%s📦 ANF→MIR Tests%s" Colors.cyan Colors.reset
            printfn ""

            for testPath in anf2mirTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                printf "  %s... " testName

                match loadANF2MIRTest testPath with
                | Ok (input, expected) ->
                    let result = runANF2MIRTest input expected
                    testTimer.Stop()
                    if result.Success then
                        printfn "%s✓ PASS%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        passed <- passed + 1
                    else
                        printfn "%s✗ FAIL%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        printfn "    %s" result.Message
                        match result.Expected, result.Actual with
                        | Some exp, Some act ->
                            printfn "    Expected:"
                            for line in exp.Split('\n') do
                                printfn "      %s" line
                            printfn "    Actual:"
                            for line in act.Split('\n') do
                                printfn "      %s" line
                        | _ -> ()
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    printfn "%s✗ ERROR%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                    printfn "    Failed to load test: %s" msg
                    failed <- failed + 1

            sectionTimer.Stop()
            printfn "  %s└─ Completed in %s%s" Colors.gray (formatTime sectionTimer.Elapsed) Colors.reset
            printfn ""

    // Run MIR→LIR tests
    let mir2lirDir = Path.Combine(assemblyDir, "passes/mir2lir")
    if Directory.Exists mir2lirDir then
        let mir2lirTests = Directory.GetFiles(mir2lirDir, "*.mir2lir")
        if mir2lirTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            printfn "%s🔄 MIR→LIR Tests%s" Colors.cyan Colors.reset
            printfn ""

            for testPath in mir2lirTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                printf "  %s... " testName

                match loadMIR2LIRTest testPath with
                | Ok (input, expected) ->
                    let result = runMIR2LIRTest input expected
                    testTimer.Stop()
                    if result.Success then
                        printfn "%s✓ PASS%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        passed <- passed + 1
                    else
                        printfn "%s✗ FAIL%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        printfn "    %s" result.Message
                        match result.Expected, result.Actual with
                        | Some exp, Some act ->
                            printfn "    Expected:"
                            for line in exp.Split('\n') do
                                printfn "      %s" line
                            printfn "    Actual:"
                            for line in act.Split('\n') do
                                printfn "      %s" line
                        | _ -> ()
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    printfn "%s✗ ERROR%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                    printfn "    Failed to load test: %s" msg
                    failed <- failed + 1

            sectionTimer.Stop()
            printfn "  %s└─ Completed in %s%s" Colors.gray (formatTime sectionTimer.Elapsed) Colors.reset
            printfn ""

    // Run LIR→ARM64 tests
    let lir2arm64Dir = Path.Combine(assemblyDir, "passes/lir2arm64")
    if Directory.Exists lir2arm64Dir then
        let lir2arm64Tests = Directory.GetFiles(lir2arm64Dir, "*.lir2arm64")
        if lir2arm64Tests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            printfn "%s🎯 LIR→ARM64 Tests%s" Colors.cyan Colors.reset
            printfn ""

            for testPath in lir2arm64Tests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                printf "  %s... " testName

                match loadLIR2ARM64Test testPath with
                | Ok (input, expected) ->
                    let result = runLIR2ARM64Test input expected
                    testTimer.Stop()
                    if result.Success then
                        printfn "%s✓ PASS%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        passed <- passed + 1
                    else
                        printfn "%s✗ FAIL%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        printfn "    %s" result.Message
                        match result.Expected, result.Actual with
                        | Some exp, Some act ->
                            printfn "    Expected:"
                            for line in exp.Split('\n') do
                                printfn "      %s" line
                            printfn "    Actual:"
                            for line in act.Split('\n') do
                                printfn "      %s" line
                        | _ -> ()
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    printfn "%s✗ ERROR%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                    printfn "    Failed to load test: %s" msg
                    failed <- failed + 1

            sectionTimer.Stop()
            printfn "  %s└─ Completed in %s%s" Colors.gray (formatTime sectionTimer.Elapsed) Colors.reset
            printfn ""

    // Run ARM64 encoding tests
    let arm64encDir = Path.Combine(assemblyDir, "passes/arm64enc")
    if Directory.Exists arm64encDir then
        let arm64encTests = Directory.GetFiles(arm64encDir, "*.arm64enc")
        if arm64encTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            printfn "%s⚙️  ARM64 Encoding Tests%s" Colors.cyan Colors.reset
            printfn ""

            for testPath in arm64encTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                printf "  %s... " testName

                match TestDSL.ARM64EncodingTestRunner.loadARM64EncodingTest testPath with
                | Ok test ->
                    let result = TestDSL.ARM64EncodingTestRunner.runARM64EncodingTest test
                    testTimer.Stop()
                    if result.Success then
                        printfn "%s✓ PASS%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        passed <- passed + 1
                    else
                        printfn "%s✗ FAIL%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        printfn "    %s" result.Message
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    printfn "%s✗ ERROR%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                    printfn "    Failed to load test: %s" msg
                    failed <- failed + 1

            sectionTimer.Stop()
            printfn "  %s└─ Completed in %s%s" Colors.gray (formatTime sectionTimer.Elapsed) Colors.reset
            printfn ""

    // Run E2E tests (in parallel)
    let e2eDir = Path.Combine(assemblyDir, "e2e")
    if Directory.Exists e2eDir then
        let e2eTestFiles = Directory.GetFiles(e2eDir, "*.e2e", SearchOption.AllDirectories)
        if e2eTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            printfn "%s🚀 E2E Tests%s" Colors.cyan Colors.reset
            printfn ""

            // Parse all tests from .e2e files
            let allTests = ResizeArray<TestDSL.E2EFormat.E2ETest>()
            let mutable parseErrors = []

            for testFile in e2eTestFiles do
                match TestDSL.E2EFormat.parseE2ETestFile testFile with
                | Ok tests -> allTests.AddRange(tests)
                | Error msg ->
                    parseErrors <- (Path.GetFileName testFile, msg) :: parseErrors

            // Report parse errors
            for (fileName, msg) in parseErrors do
                printfn "%s✗ ERROR parsing %s%s" Colors.red fileName Colors.reset
                printfn "    %s" msg
                failed <- failed + 1

            // Run tests in parallel (max 6 concurrent) and print as they complete (in order)
            if allTests.Count > 0 then
                let testsArray = allTests.ToArray()
                let numTests = testsArray.Length
                let results = Array.zeroCreate<option<E2ETest * E2ETestResult * TimeSpan>> numTests
                let mutable nextToPrint = 0
                let lockObj = obj()

                // Helper function to print a test result
                let printTestResult (test: E2ETest) (result: E2ETestResult) (elapsed: TimeSpan) =
                    printf "  %s... " test.Name
                    if result.Success then
                        printfn "%s✓ PASS%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime elapsed) Colors.reset
                        lock lockObj (fun () -> passed <- passed + 1)
                    else
                        printfn "%s✗ FAIL%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime elapsed) Colors.reset
                        printfn "    %s" result.Message

                        // Show exit code mismatch
                        match result.ExitCode with
                        | Some code when code <> test.ExpectedExitCode ->
                            printfn "    Expected exit code: %d" test.ExpectedExitCode
                            printfn "    Actual exit code: %d" code
                        | _ -> ()

                        // Show stdout mismatch
                        match test.ExpectedStdout, result.Stdout with
                        | Some expected, Some actual when actual.Trim() <> expected.Trim() ->
                            printfn "    Expected stdout: %s" (expected.Replace("\n", "\\n"))
                            printfn "    Actual stdout: %s" (actual.Replace("\n", "\\n"))
                        | Some expected, None ->
                            printfn "    Expected stdout: %s" (expected.Replace("\n", "\\n"))
                            printfn "    Actual: no stdout captured"
                        | None, Some actual when actual.Trim() <> "" ->
                            // Unexpected stdout when none was expected
                            printfn "    Unexpected stdout: %s" (actual.Replace("\n", "\\n"))
                        | _ -> ()

                        // Show stderr mismatch
                        match test.ExpectedStderr, result.Stderr with
                        | Some expected, Some actual when actual.Trim() <> expected.Trim() ->
                            printfn "    Expected stderr: %s" (expected.Replace("\n", "\\n"))
                            printfn "    Actual stderr: %s" (actual.Replace("\n", "\\n"))
                        | Some expected, None ->
                            printfn "    Expected stderr: %s" (expected.Replace("\n", "\\n"))
                            printfn "    Actual: no stderr captured"
                        | None, Some actual when actual.Trim() <> "" ->
                            // Unexpected stderr when none was expected
                            printfn "    Unexpected stderr: %s" (actual.Replace("\n", "\\n"))
                        | _ -> ()

                        lock lockObj (fun () -> failed <- failed + 1)

                // Helper to print all consecutive completed tests starting from nextToPrint
                let printPendingResults () =
                    lock lockObj (fun () ->
                        while nextToPrint < numTests && results.[nextToPrint].IsSome do
                            let (test, result, elapsed) = results.[nextToPrint].Value
                            printTestResult test result elapsed
                            nextToPrint <- nextToPrint + 1
                    )

                // Run tests in parallel
                let options = System.Threading.Tasks.ParallelOptions()
                options.MaxDegreeOfParallelism <- 6
                System.Threading.Tasks.Parallel.For(
                    0,
                    numTests,
                    options,
                    fun i ->
                        let test = testsArray.[i]
                        let testTimer = Stopwatch.StartNew()
                        let result = runE2ETest test
                        testTimer.Stop()

                        // Store result
                        lock lockObj (fun () ->
                            results.[i] <- Some (test, result, testTimer.Elapsed)
                        )

                        // Try to print this and any subsequent completed tests
                        printPendingResults ()
                ) |> ignore

            sectionTimer.Stop()
            printfn "  %s└─ Completed in %s%s" Colors.gray (formatTime sectionTimer.Elapsed) Colors.reset
            printfn ""

    // Run unit tests
    let sectionTimer = Stopwatch.StartNew()
    printfn "%s🔧 Unit Tests%s" Colors.cyan Colors.reset
    printfn ""

    let unitTestTimer = Stopwatch.StartNew()
    try
        EncodingTests.runAll()
        unitTestTimer.Stop()
        printfn "  %s✓ Encoding Tests%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime unitTestTimer.Elapsed) Colors.reset
        passed <- passed + 1
    with
    | ex ->
        unitTestTimer.Stop()
        printfn "  %s✗ FAIL: Encoding tests%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime unitTestTimer.Elapsed) Colors.reset
        printfn "    %s" ex.Message
        failed <- failed + 1

    let binaryTestTimer = Stopwatch.StartNew()
    try
        BinaryTests.runAll()
        binaryTestTimer.Stop()
        printfn "  %s✓ Binary Tests%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime binaryTestTimer.Elapsed) Colors.reset
        passed <- passed + 11  // 11 tests in BinaryTests
    with
    | ex ->
        binaryTestTimer.Stop()
        printfn "  %s✗ FAIL: Binary tests%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime binaryTestTimer.Elapsed) Colors.reset
        printfn "    %s" ex.Message
        failed <- failed + 1

    sectionTimer.Stop()
    printfn "  %s└─ Completed in %s%s" Colors.gray (formatTime sectionTimer.Elapsed) Colors.reset
    printfn ""

    totalTimer.Stop()
    printfn "%s%s═══════════════════════════════════════%s" Colors.bold Colors.cyan Colors.reset
    printfn "%s%s📊 Test Results%s" Colors.bold Colors.cyan Colors.reset
    printfn "%s%s═══════════════════════════════════════%s" Colors.bold Colors.cyan Colors.reset
    if failed = 0 then
        printfn "  %s✓ All tests passed: %d/%d%s" Colors.green (passed) (passed + failed) Colors.reset
    else
        printfn "  %s✓ Passed: %d%s" Colors.green passed Colors.reset
        printfn "  %s✗ Failed: %d%s" Colors.red failed Colors.reset
    printfn "  %s⏱  Total time: %s%s" Colors.gray (formatTime totalTimer.Elapsed) Colors.reset
    printfn "%s%s═══════════════════════════════════════%s" Colors.bold Colors.cyan Colors.reset

    if failed = 0 then 0 else 1
