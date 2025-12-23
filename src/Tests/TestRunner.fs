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

    // Run E2E tests
    let e2eDir = Path.Combine(assemblyDir, "e2e")
    if Directory.Exists e2eDir then
        let e2eTests = Directory.GetFiles(e2eDir, "*.test", SearchOption.AllDirectories)
        if e2eTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            printfn "%s🚀 E2E Tests%s" Colors.cyan Colors.reset
            printfn ""

            // Find compiler executable - it's in the same bin directory as Tests
            // assemblyDir is: /path/to/bin/Tests/Debug/net9.0
            // compiler is at: /path/to/bin/DarkCompiler/Debug/net9.0/DarkCompiler.dll
            let binDir = Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName(assemblyDir)))
            let compilerPath = Path.Combine(binDir, "DarkCompiler/Debug/net9.0/DarkCompiler.dll")

            for testPath in e2eTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                printf "  %s... " testName

                match parseE2ETest testPath with
                | Ok test ->
                    let result = runE2ETest test compilerPath
                    testTimer.Stop()
                    if result.Success then
                        printfn "%s✓ PASS%s %s(%s)%s" Colors.green Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        passed <- passed + 1
                    else
                        printfn "%s✗ FAIL%s %s(%s)%s" Colors.red Colors.reset Colors.gray (formatTime testTimer.Elapsed) Colors.reset
                        printfn "    %s" result.Message
                        match result.ExitCode with
                        | Some code when code <> test.ExpectedExitCode ->
                            printfn "    Expected exit code: %d" test.ExpectedExitCode
                            printfn "    Actual exit code: %d" code
                        | _ -> ()
                        // Always show stdout/stderr on failure
                        match result.Stdout with
                        | Some stdout when stdout.Trim() <> "" ->
                            printfn "    Stdout: %s" (stdout.Trim())
                        | _ -> ()
                        match result.Stderr with
                        | Some stderr when stderr.Trim() <> "" ->
                            printfn "    Stderr: %s" (stderr.Trim())
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
