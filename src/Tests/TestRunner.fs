// TestRunner.fs - Standalone test runner for DSL-based tests
//
// Discovers and runs test files from passes/ directories.

module TestRunner.Main

open System
open System.IO
open System.Reflection
open System.Diagnostics
open Output
open TestDSL.PassTestRunner
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner
open TestDSL.OptimizationFormat
open TestDSL.OptimizationTestRunner
open TestRunnerScheduling
open StdlibTestHarness

// ANSI color codes
module Colors =
    let reset = "\x1b[0m"
    let green = "\x1b[32m"
    let red = "\x1b[31m"
    let yellow = "\x1b[33m"
    let cyan = "\x1b[36m"
    let gray = "\x1b[90m"
    let bold = "\x1b[1m"

// Failed test info for summary at end
type FailedTestInfo = {
    File: string
    Name: string
    Message: string
    Details: string list  // Additional details like expected/actual
}

// Test timing info for slowest tests report
type TestTiming = {
    Name: string
    TotalTime: TimeSpan
    CompileTime: TimeSpan option
    RuntimeTime: TimeSpan option
}

// Summary of per-file test suite results
type FileSuiteSummary = {
    Passed: int
    Failed: int
    FailedTests: FailedTestInfo list
}

// Progress bar for test execution
module ProgressBar =
    let private barWidth = 20
    let private lockObj = obj()

    type State = {
        mutable Total: int
        mutable Completed: int
        mutable Failed: int
        Label: string
    }

    let create label total = { Total = total; Completed = 0; Failed = 0; Label = label }

    let update (state: State) =
        lock lockObj (fun () ->
            let pct = if state.Total = 0 then 0.0 else float state.Completed / float state.Total
            let filled = int (pct * float barWidth)
            let bar = String.replicate filled "=" + String.replicate (barWidth - filled) " "
            let failStr = if state.Failed > 0 then $" ({Colors.red}{state.Failed} failed{Colors.reset})" else ""
            // Use \r to return to start of line, \x1b[K to clear to end of line
            eprint $"\r\x1b[K  {state.Label}: [{bar}] {state.Completed}/{state.Total}{failStr}"
        )

    let increment (state: State) (success: bool) =
        lock lockObj (fun () ->
            state.Completed <- state.Completed + 1
            if not success then state.Failed <- state.Failed + 1
        )
        update state

    let finish (state: State) =
        lock lockObj (fun () ->
            // Clear the progress line and print final summary
            eprint "\r\x1b[K"
        )

// Format elapsed time
let formatTime (elapsed: TimeSpan) =
    if elapsed.TotalMilliseconds < 1000.0 then
        sprintf "%.0fms" elapsed.TotalMilliseconds
    else
        sprintf "%.2fs" elapsed.TotalSeconds

// Get optimal parallelism based on system resources
let getOptimalParallelism () : int =
    // Get CPU core count
    let cpuCores = Environment.ProcessorCount

    // Get available memory in GB
    let gcMemoryInfo = System.GC.GetGCMemoryInfo()
    let totalMemoryGB = float gcMemoryInfo.TotalAvailableMemoryBytes / (1024.0 * 1024.0 * 1024.0)

    // Each E2E test needs roughly:
    // - Minimal CPU (tests are mostly I/O bound - compile + exec)
    // - ~100MB memory (compiler + generated binary + test overhead)
    let optimal = calculateOptimalParallelism cpuCores totalMemoryGB
    optimal

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

// Parse command line for --parallel=N option
let parseParallelArg (args: string array) : int option =
    args
    |> Array.tryFind (fun arg -> arg.StartsWith("--parallel="))
    |> Option.map (fun arg -> arg.Substring(11) |> int)

// Parse command line for --filter=PATTERN option
let parseFilterArg (args: string array) : string option =
    args
    |> Array.tryFind (fun arg -> arg.StartsWith("--filter="))
    |> Option.map (fun arg -> arg.Substring(9))

// Check if --coverage flag is present (show inline coverage after tests)
let hasCoverageArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--coverage")

// Check if --verification flag is present (enable verification/stress tests)
let hasVerificationArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--verification")

// Check if a test name matches the filter (case-insensitive substring match)
let matchesFilter (filter: string option) (testName: string) : bool =
    match filter with
    | None -> true
    | Some pattern -> testName.ToLowerInvariant().Contains(pattern.ToLowerInvariant())

// Check if --help flag is present
let hasHelpArg (args: string array) : bool =
    args |> Array.exists (fun arg -> arg = "--help" || arg = "-h")

// Print help message
let printHelp () =
    println "Usage: Tests [OPTIONS]"
    println ""
    println "Options:"
    println "  --filter=PATTERN   Run only tests matching PATTERN (case-insensitive substring)"
    println "  --parallel=N       Run with N parallel test workers"
    println "  --coverage         Show stdlib coverage percentage after running tests"
    println "  --verification     Enable verification/stress tests"
    println "  --help, -h         Show this help message"
    println ""
    println "Examples:"
    println "  Tests                      Run all tests"
    println "  Tests --filter=tuple       Run tests with 'tuple' in the name"
    println "  Tests --filter=string      Run tests with 'string' in the name"
    println "  Tests --parallel=4         Run with 4 parallel workers"
    println "  Tests --coverage           Run tests and show coverage percentage"
    println "  Tests --verification       Run verification/stress tests"

[<EntryPoint>]
let main args =
    // Check for --help flag
    if hasHelpArg args then
        printHelp ()
        0
    else

    let totalTimer = Stopwatch.StartNew()

    // Check for --parallel=N argument
    let overrideParallel = parseParallelArg args

    // Check for --filter=PATTERN argument
    let filter = parseFilterArg args

    // Check for --coverage flag (show inline coverage after tests)
    let showCoverage = hasCoverageArg args

    // Check for --verification flag (enable verification/stress tests)
    let verificationEnabled = hasVerificationArg args
    if verificationEnabled then
        Environment.SetEnvironmentVariable("ENABLE_VERIFICATION_TESTS", "true")

    println $"{Colors.bold}{Colors.cyan}🧪 Running DSL-based Tests{Colors.reset}"
    match filter with
    | Some pattern -> println $"{Colors.gray}  Filter: {pattern}{Colors.reset}"
    | None -> ()
    println ""

    // Get the directory where the assembly is located (where test files are copied)
    let assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let e2eDir = Path.Combine(assemblyDir, "e2e")
    let verificationDir = Path.Combine(assemblyDir, "verification")

    let unitStdlibSuites = [ "Compiler Caching Tests"; "Preamble Precompile Tests" ]
    let needsUnitStdlib = unitStdlibSuites |> List.exists (matchesFilter filter)
    let unitTestNames = [|
        "CLI Flags Tests"
        "Test Runner Scheduling Tests"
        "Compiler Caching Tests"
        "Preamble Precompile Tests"
        "Stdlib Test Harness Tests"
        "IR Symbol Tests"
        "IR Printer Tests"
        "Pass Test Runner Tests"
        "Parallel Utils Tests"
        "Encoding Tests"
        "Binary Tests"
        "Type Checking Tests"
        "Parallel Move Tests"
        "SSA Liveness Tests"
        "Phi Resolution Tests"
        "Chordal Graph Tests"
    |]

    let hasE2ETests =
        Directory.Exists e2eDir
        && Directory.GetFiles(e2eDir, "*.e2e", SearchOption.AllDirectories).Length > 0

    let hasVerificationTests =
        Directory.Exists verificationDir
        && Directory.GetFiles(verificationDir, "*.e2e", SearchOption.AllDirectories).Length > 0

    let shouldCompileStdlib =
        shouldStartStdlibCompile hasE2ETests (verificationEnabled && hasVerificationTests) needsUnitStdlib

    let stdlibWarmupPlan = decideStdlibWarmupPlan shouldCompileStdlib

    let enableVerification =
        verificationEnabled || Environment.GetEnvironmentVariable("ENABLE_VERIFICATION_TESTS") = "true"
    let hasUnitTests =
        unitTestNames |> Array.exists (matchesFilter filter)
    let hasE2EOrVerification =
        hasE2ETests || (enableVerification && hasVerificationTests)
    let runUnitAndE2EInParallel =
        shouldRunUnitAndE2EInParallel hasUnitTests hasE2EOrVerification

    let compileStdlibWithTiming () : Result<CompilerLibrary.StdlibResult, string> * TimeSpan =
        let timer = Stopwatch.StartNew()
        let result = TestDSL.E2ETestRunner.compileStdlib()
        timer.Stop()
        (result, timer.Elapsed)

    let mutable e2eStdlib : CompilerLibrary.StdlibResult option = None

    let mutable passed = 0
    let mutable failed = 0
    let failedTests = ResizeArray<FailedTestInfo>()
    let allTimings = ResizeArray<TestTiming>()
    let resultsLock = obj()

    let recordTiming (timing: TestTiming) : unit =
        lock resultsLock (fun () -> allTimings.Add timing)

    let recordResults (passedDelta: int) (failedDelta: int) (failedTestsDelta: FailedTestInfo list) : unit =
        lock resultsLock (fun () ->
            passed <- passed + passedDelta
            failed <- failed + failedDelta
            for test in failedTestsDelta do
                failedTests.Add test)

    let stdlibLock = obj()
    let mutable stdlibCompileResult : Result<CompilerLibrary.StdlibResult, string> option = None
    let mutable stdlibCompileElapsed : TimeSpan option = None

    let tryGetStdlibCompileElapsed () : TimeSpan option =
        lock stdlibLock (fun () -> stdlibCompileElapsed)

    let recordStdlibCompile
        (result: Result<CompilerLibrary.StdlibResult, string>)
        (elapsed: TimeSpan)
        : unit =
        lock stdlibLock (fun () ->
            stdlibCompileResult <- Some result
            stdlibCompileElapsed <- Some elapsed
            match result with
            | Ok stdlib -> e2eStdlib <- Some stdlib
            | Error _ -> ())
        recordTiming { Name = "Stdlib Compile"; TotalTime = elapsed; CompileTime = None; RuntimeTime = None }

    match stdlibWarmupPlan with
    | CompileStdlibBeforeTests ->
        let (result, elapsed) = compileStdlibWithTiming ()
        recordStdlibCompile result elapsed
    | SkipStdlibWarmup -> ()

    let getStdlibResult () : Result<CompilerLibrary.StdlibResult, string> =
        match e2eStdlib with
        | Some stdlib -> Ok stdlib
        | None ->
            match lock stdlibLock (fun () -> stdlibCompileResult) with
            | Some cached -> cached
            | None -> Error "Stdlib was not compiled before tests"

    let loadE2ETests (testFiles: string array) : E2ETest array * (string * string) list =
        let tests = ResizeArray<E2ETest>()
        let mutable parseErrors = []
        for testFile in testFiles do
            match parseE2ETestFile testFile with
            | Ok parsed -> tests.AddRange(parsed)
            | Error msg -> parseErrors <- (testFile, msg) :: parseErrors
        (tests.ToArray(), List.rev parseErrors)

    let reportParseErrors (suiteName: string) (parseErrors: (string * string) list) : unit =
        for (filePath, msg) in parseErrors do
            let fileName = Path.GetFileName filePath
            println $"  {Colors.red}✗ ERROR parsing {fileName}{Colors.reset}"
            println $"    {msg}"
            recordResults 0 1 [{ File = filePath; Name = $"{suiteName}: {fileName}"; Message = msg; Details = [] }]

    let addExpectedActualDetails (expected: string option) (actual: string option) : string list =
        let details = ResizeArray<string>()
        match expected, actual with
        | Some exp, Some act ->
            println "    Expected:"
            for line in exp.Split('\n') do
                println $"      {line}"
                details.Add($"Expected: {line}")
            println "    Actual:"
            for line in act.Split('\n') do
                println $"      {line}"
                details.Add($"Actual: {line}")
        | _ -> ()
        details |> Seq.toList

    let runFileSuite
        (suiteTitle: string)
        (progressLabel: string)
        (testFiles: string array)
        (getTestName: string -> string)
        (formatTimingName: string -> string)
        (runFile: string -> Result<'result, string>)
        (handleSuccess: ProgressBar.State -> string -> string -> TimeSpan -> 'result -> FileSuiteSummary)
        (handleError: ProgressBar.State -> string -> string -> TimeSpan -> string -> FileSuiteSummary)
        : unit =
        if testFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}{suiteTitle}{Colors.reset}"

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create progressLabel testFiles.Length
            ProgressBar.update progress

            for testPath in testFiles do
                let testName = getTestName testPath
                let testTimer = Stopwatch.StartNew()
                let summary =
                    match runFile testPath with
                    | Ok result ->
                        testTimer.Stop()
                        handleSuccess progress testPath testName testTimer.Elapsed result
                    | Error msg ->
                        testTimer.Stop()
                        handleError progress testPath testName testTimer.Elapsed msg
                allTimings.Add({ Name = formatTimingName testName; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                sectionPassed <- sectionPassed + summary.Passed
                sectionFailed <- sectionFailed + summary.Failed
                passed <- passed + summary.Passed
                failed <- failed + summary.Failed
                for failedTest in summary.FailedTests do
                    failedTests.Add failedTest

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    let runE2ESuite
        (suiteName: string)
        (progressLabel: string)
        (testsArray: E2ETest array)
        (stdlib: CompilerLibrary.StdlibResult)
        : unit =
        let numTests = testsArray.Length
        if numTests > 0 then
            let precompileTasks = TestDSL.E2ETestRunner.startPreamblePrecompileTasks stdlib testsArray

            let results = Array.zeroCreate<option<E2ETest * E2ETestResult>> numTests
            let lockObj = obj()
            let progress = ProgressBar.create progressLabel numTests
            ProgressBar.update progress

            let maxParallel = overrideParallel |> Option.defaultWith getOptimalParallelism

            let options = System.Threading.Tasks.ParallelOptions()
            options.MaxDegreeOfParallelism <- maxParallel
            System.Threading.Tasks.Parallel.For(
                0,
                numTests,
                options,
                fun i ->
                    let test = testsArray.[i]
                    let preambleResult = TestDSL.E2ETestRunner.awaitPreamblePrecompile precompileTasks test
                    let result =
                        match preambleResult with
                        | Ok () -> runE2ETest stdlib test
                        | Error err ->
                            { Success = false
                              Message = $"Preamble precompile failed: {err}"
                              Stdout = None
                              Stderr = None
                              ExitCode = Some 1
                              CompileTime = TimeSpan.Zero
                              RuntimeTime = TimeSpan.Zero }
                    let totalTime = result.CompileTime + result.RuntimeTime
                    lock lockObj (fun () ->
                        results.[i] <- Some (test, result)
                        recordTiming { Name = $"{suiteName}: {test.Name}"; TotalTime = totalTime; CompileTime = Some result.CompileTime; RuntimeTime = Some result.RuntimeTime }
                        ProgressBar.increment progress result.Success
                    )
            ) |> ignore

            ProgressBar.finish progress

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let failedTestsLocal = ResizeArray<FailedTestInfo>()
            for result in results do
                match result with
                | Some (test, testResult) ->
                    if testResult.Success then
                        sectionPassed <- sectionPassed + 1
                    else
                        let cleanName = test.Name.Replace("Stdlib.", "")
                        let displayName = if cleanName.Length > 60 then cleanName.Substring(0, 57) + "..." else cleanName
                        println $"  {displayName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}(compile: {formatTime testResult.CompileTime}, run: {formatTime testResult.RuntimeTime}){Colors.reset}"
                        println $"    {testResult.Message}"

                        let details = ResizeArray<string>()
                        match testResult.ExitCode with
                        | Some code when code <> test.ExpectedExitCode ->
                            println $"    Expected exit code: {test.ExpectedExitCode}, Actual: {code}"
                            details.Add($"Expected exit code: {test.ExpectedExitCode}, Actual: {code}")
                        | _ -> ()
                        match test.ExpectedStdout, testResult.Stdout with
                        | Some expected, Some actual when actual.Trim() <> expected.Trim() ->
                            let expectedDisp = expected.Replace("\n", "\\n")
                            let actualDisp = actual.Replace("\n", "\\n")
                            println $"    Expected stdout: {expectedDisp}"
                            println $"    Actual stdout: {actualDisp}"
                        | _ -> ()
                        match test.ExpectedStderr, testResult.Stderr with
                        | Some expected, Some actual when actual.Trim() <> expected.Trim() ->
                            let expectedDisp = expected.Replace("\n", "\\n")
                            let actualDisp = actual.Replace("\n", "\\n")
                            println $"    Expected stderr: {expectedDisp}"
                            println $"    Actual stderr: {actualDisp}"
                        | _ -> ()

                        failedTestsLocal.Add({ File = test.SourceFile; Name = $"{suiteName}: {test.Name}"; Message = testResult.Message; Details = details |> Seq.toList })
                        sectionFailed <- sectionFailed + 1
                | None -> ()

            recordResults sectionPassed sectionFailed (failedTestsLocal |> Seq.toList)

            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"

    let runPassTestFile
        (loadTest: string -> Result<'input, string>)
        (runTest: 'input -> PassTestResult)
        (testPath: string)
        : Result<PassTestResult, string> =
        match loadTest testPath with
        | Ok input -> Ok (runTest input)
        | Error msg -> Error msg

    let handlePassTestSuccess
        (suiteLabel: string)
        (progress: ProgressBar.State)
        (testPath: string)
        (testName: string)
        (elapsed: TimeSpan)
        (result: PassTestResult)
        : FileSuiteSummary =
        if result.Success then
            ProgressBar.increment progress true
            { Passed = 1; Failed = 0; FailedTests = [] }
        else
            ProgressBar.increment progress false
            ProgressBar.finish progress
            println $"  {testName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime elapsed}){Colors.reset}"
            println $"    {result.Message}"
            let details = addExpectedActualDetails result.Expected result.Actual
            let failedInfo =
                { File = testPath
                  Name = $"{suiteLabel}: {testName}"
                  Message = result.Message
                  Details = details }
            ProgressBar.update progress
            { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }

    let handlePassTestError
        (suiteLabel: string)
        (progress: ProgressBar.State)
        (testPath: string)
        (testName: string)
        (elapsed: TimeSpan)
        (msg: string)
        : FileSuiteSummary =
        ProgressBar.increment progress false
        ProgressBar.finish progress
        println $"  {testName}... {Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime elapsed}){Colors.reset}"
        println $"    Failed to load test: {msg}"
        let failedInfo =
            { File = testPath
              Name = $"{suiteLabel}: {testName}"
              Message = $"Failed to load test: {msg}"
              Details = [] }
        ProgressBar.update progress
        { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }

    // Run ANF→MIR tests
    let anf2mirDir = Path.Combine(assemblyDir, "passes/anf2mir")
    if Directory.Exists anf2mirDir then
        let anf2mirTests = Directory.GetFiles(anf2mirDir, "*.anf2mir") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        let runANF2MIRFile =
            runPassTestFile loadANF2MIRTest (fun (input, expected) -> runANF2MIRTest input expected)
        runFileSuite
            "📦 ANF→MIR Tests"
            "ANF→MIR"
            anf2mirTests
            Path.GetFileName
            (fun testName -> $"ANF→MIR: {testName}")
            runANF2MIRFile
            (handlePassTestSuccess "ANF→MIR")
            (handlePassTestError "ANF→MIR")

    // Run MIR→LIR tests
    let mir2lirDir = Path.Combine(assemblyDir, "passes/mir2lir")
    if Directory.Exists mir2lirDir then
        let mir2lirTests = Directory.GetFiles(mir2lirDir, "*.mir2lir") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        let runMIR2LIRFile =
            runPassTestFile loadMIR2LIRTest (fun (input, expected) -> runMIR2LIRTest input expected)
        runFileSuite
            "🔄 MIR→LIR Tests"
            "MIR→LIR"
            mir2lirTests
            Path.GetFileName
            (fun testName -> $"MIR→LIR: {testName}")
            runMIR2LIRFile
            (handlePassTestSuccess "MIR→LIR")
            (handlePassTestError "MIR→LIR")

    // Run LIR→ARM64 tests
    let lir2arm64Dir = Path.Combine(assemblyDir, "passes/lir2arm64")
    if Directory.Exists lir2arm64Dir then
        let lir2arm64Tests = Directory.GetFiles(lir2arm64Dir, "*.lir2arm64") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        let runLIR2ARM64File =
            runPassTestFile loadLIR2ARM64Test (fun (input, expected) -> runLIR2ARM64Test input expected)
        runFileSuite
            "🎯 LIR→ARM64 Tests"
            "LIR→ARM64"
            lir2arm64Tests
            Path.GetFileName
            (fun testName -> $"LIR→ARM64: {testName}")
            runLIR2ARM64File
            (handlePassTestSuccess "LIR→ARM64")
            (handlePassTestError "LIR→ARM64")

    // Run ARM64 encoding tests
    let arm64encDir = Path.Combine(assemblyDir, "passes/arm64enc")
    if Directory.Exists arm64encDir then
        let arm64encTests = Directory.GetFiles(arm64encDir, "*.arm64enc") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        let runARM64EncodingFile =
            runPassTestFile
                TestDSL.ARM64EncodingTestRunner.loadARM64EncodingTest
                TestDSL.ARM64EncodingTestRunner.runARM64EncodingTest
        runFileSuite
            "⚙️  ARM64 Encoding Tests"
            "ARM64 Enc"
            arm64encTests
            Path.GetFileName
            (fun testName -> $"ARM64 Encoding: {testName}")
            runARM64EncodingFile
            (handlePassTestSuccess "ARM64 Encoding")
            (handlePassTestError "ARM64 Encoding")

    // Run Type Checking tests
    let typecheckDir = Path.Combine(assemblyDir, "typecheck")
    if Directory.Exists typecheckDir then
        let typecheckTestFiles = Directory.GetFiles(typecheckDir, "*.typecheck", SearchOption.AllDirectories) |> Array.filter (fun p -> matchesFilter filter (Path.GetFileNameWithoutExtension p))
        let runTypecheckFile testFile =
            TestDSL.TypeCheckingTestRunner.runTypeCheckingTestFile testFile
        let handleTypecheckSuccess
            (progress: ProgressBar.State)
            (testPath: string)
            (fileName: string)
            (_: TimeSpan)
            (results: TestDSL.TypeCheckingTestRunner.TypeCheckingTestResult list)
            : FileSuiteSummary =
            let fileSuccess = results |> List.forall (fun r -> r.Success)
            let filePassCount = results |> List.filter (fun r -> r.Success) |> List.length
            let fileFailCount = results |> List.filter (fun r -> not r.Success) |> List.length
            if fileSuccess then
                ProgressBar.increment progress true
                { Passed = filePassCount; Failed = 0; FailedTests = [] }
            else
                let failures = ResizeArray<FailedTestInfo>()
                for result in results do
                    if not result.Success then
                        ProgressBar.increment progress false
                        ProgressBar.finish progress
                        let typeDesc =
                            match result.ExpectedType with
                            | Some t -> TypeChecking.typeToString t
                            | None -> "error"
                        println $"  {typeDesc} ({fileName})... {Colors.red}✗ FAIL{Colors.reset}"
                        println $"    {result.Message}"
                        failures.Add({ File = testPath; Name = $"Type Checking: {typeDesc} ({fileName})"; Message = result.Message; Details = [] })
                        ProgressBar.update progress
                { Passed = filePassCount; Failed = fileFailCount; FailedTests = failures |> Seq.toList }
        let handleTypecheckError
            (progress: ProgressBar.State)
            (testPath: string)
            (_: string)
            (_: TimeSpan)
            (msg: string)
            : FileSuiteSummary =
            ProgressBar.increment progress false
            ProgressBar.finish progress
            println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName testPath}{Colors.reset}"
            println $"    {msg}"
            let failedInfo =
                { File = testPath
                  Name = $"Type Checking: {Path.GetFileName testPath}"
                  Message = msg
                  Details = [] }
            ProgressBar.update progress
            { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }
        runFileSuite
            "📋 Type Checking Tests"
            "TypeCheck"
            typecheckTestFiles
            (fun testPath -> Path.GetFileNameWithoutExtension (testPath: string))
            (fun fileName -> $"TypeCheck: {fileName}")
            runTypecheckFile
            handleTypecheckSuccess
            handleTypecheckError

    // Run Optimization Tests (ANF, MIR, LIR)
    let optDir = Path.Combine(assemblyDir, "optimization")
    if Directory.Exists optDir then
        let optTestFiles = Directory.GetFiles(optDir, "*.opt", SearchOption.AllDirectories)
        let runOptimizationFile testFile =
            let fileName = Path.GetFileNameWithoutExtension (testFile: string)
            let stage =
                if fileName.ToLower().Contains("anf") then TestDSL.OptimizationFormat.ANF
                elif fileName.ToLower().Contains("mir") then TestDSL.OptimizationFormat.MIR
                elif fileName.ToLower().Contains("lir") then TestDSL.OptimizationFormat.LIR
                else TestDSL.OptimizationFormat.ANF
            TestDSL.OptimizationTestRunner.runTestFile stage testFile
        let handleOptimizationSuccess
            (progress: ProgressBar.State)
            (testPath: string)
            (_: string)
            (_: TimeSpan)
            (results: (TestDSL.OptimizationFormat.OptimizationTest * TestDSL.OptimizationTestRunner.OptimizationTestResult) list)
            : FileSuiteSummary =
            let filteredResults = results |> List.filter (fun (test, _) -> matchesFilter filter test.Name)
            let fileSuccess = filteredResults |> List.forall (fun (_, r) -> r.Success)
            let filePassCount = filteredResults |> List.filter (fun (_, r) -> r.Success) |> List.length
            let fileFailCount = filteredResults |> List.filter (fun (_, r) -> not r.Success) |> List.length
            if fileSuccess then
                ProgressBar.increment progress true
                { Passed = filePassCount; Failed = 0; FailedTests = [] }
            else
                let failures = ResizeArray<FailedTestInfo>()
                for (test, result) in filteredResults do
                    if not result.Success then
                        ProgressBar.increment progress false
                        ProgressBar.finish progress
                        println $"  {test.Name}... {Colors.red}✗ FAIL{Colors.reset}"
                        println $"    {result.Message}"
                        let details = addExpectedActualDetails result.Expected result.Actual
                        failures.Add({ File = testPath; Name = $"Optimization: {test.Name}"; Message = result.Message; Details = details })
                        ProgressBar.update progress
                { Passed = filePassCount; Failed = fileFailCount; FailedTests = failures |> Seq.toList }
        let handleOptimizationError
            (progress: ProgressBar.State)
            (testPath: string)
            (_: string)
            (_: TimeSpan)
            (msg: string)
            : FileSuiteSummary =
            ProgressBar.increment progress false
            ProgressBar.finish progress
            println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName testPath}{Colors.reset}"
            println $"    {msg}"
            let failedInfo =
                { File = testPath
                  Name = $"Optimization: {Path.GetFileName testPath}"
                  Message = msg
                  Details = [] }
            ProgressBar.update progress
            { Passed = 0; Failed = 1; FailedTests = [ failedInfo ] }
        runFileSuite
            "⚡ Optimization Tests"
            "Optimization"
            optTestFiles
            (fun testPath -> Path.GetFileNameWithoutExtension (testPath: string))
            (fun fileName -> $"Optimization: {fileName}")
            runOptimizationFile
            handleOptimizationSuccess
            handleOptimizationError

    let runWithStdlib
        (suiteName: string)
        (runner: CompilerLibrary.StdlibResult -> Result<unit, string>)
        : Result<unit, string> =
        StdlibTestHarness.withStdlib getStdlibResult (fun stdlib -> runner stdlib)

    let wrapStdlibTests
        (suiteName: string)
        (tests: (string * (CompilerLibrary.StdlibResult -> Result<unit, string>)) list)
        : (string * (unit -> Result<unit, string>)) list =
        tests
        |> List.map (fun (name, test) ->
            (name, fun () -> runWithStdlib suiteName (fun stdlib -> test stdlib)))

    // Define unit test suites and their per-test runners
    let allUnitTests : UnitTestSuite array = [|
        { Name = "CLI Flags Tests"; Tests = CliFlagTests.tests }
        { Name = "Test Runner Scheduling Tests"; Tests = TestRunnerSchedulingTests.tests }
        { Name = "Compiler Library Tests"; Tests = CompilerLibraryTests.tests }
        { Name = "Stdlib Test Harness Tests"; Tests = StdlibTestHarnessTests.tests }
        { Name = "Compiler Caching Tests"; Tests = wrapStdlibTests "Compiler Caching Tests" CompilerCachingTests.tests }
        { Name = "Preamble Precompile Tests"; Tests = wrapStdlibTests "Preamble Precompile Tests" PreamblePrecompileTests.tests }
        { Name = "IR Symbol Tests"; Tests = IRSymbolTests.tests }
        { Name = "IR Printer Tests"; Tests = IRPrinterTests.tests }
        { Name = "Script Helper Tests"; Tests = ScriptHelperTests.tests }
        { Name = "Pass Test Runner Tests"; Tests = PassTestRunnerTests.tests }
        { Name = "Parallel Utils Tests"; Tests = ParallelUtilsTests.tests }
        { Name = "Encoding Tests"; Tests = EncodingTests.tests }
        { Name = "Binary Tests"; Tests = BinaryTests.tests }
        { Name = "Type Checking Tests"; Tests = TypeCheckingTests.tests }
        { Name = "Parallel Move Tests"; Tests = ParallelMoveTests.tests }
        { Name = "SSA Liveness Tests"; Tests = SSALivenessTests.tests }
        { Name = "Phi Resolution Tests"; Tests = PhiResolutionTests.tests }
        { Name = "Chordal Graph Tests"; Tests = ChordalGraphTests.tests }
    |]
    let unitTests = allUnitTests |> Array.filter (fun suite -> matchesFilter filter suite.Name)

    let (unitTestsNoStdlib, unitTestsWithStdlib) =
        splitUnitTestsByStdlibNeed unitStdlibSuites unitTests
    let unitTestsOrdered = Array.append unitTestsNoStdlib unitTestsWithStdlib

    let runUnitTests () : unit =
        let unitSectionTimer = Stopwatch.StartNew()
        println $"{Colors.cyan}🔧 Unit Tests{Colors.reset}"

        let mutable unitSectionPassed = 0
        let mutable unitSectionFailed = 0
        let unitFailedTests = ResizeArray<FailedTestInfo>()

        let totalUnitTests =
            unitTestsOrdered
            |> Array.sumBy (fun suite -> suite.Tests.Length)
        let unitProgress = ProgressBar.create "Unit" totalUnitTests
        ProgressBar.update unitProgress

        for suite in unitTestsOrdered do
            for (testName, runTest) in suite.Tests do
                let timer = Stopwatch.StartNew()
                match runTest() with
                | Ok () ->
                    timer.Stop()
                    let displayName = formatUnitTestName suite.Name testName
                    recordTiming { Name = $"Unit: {displayName}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None }
                    unitSectionPassed <- unitSectionPassed + 1
                    ProgressBar.increment unitProgress true
                | Error msg ->
                    timer.Stop()
                    let displayName = formatUnitTestName suite.Name testName
                    recordTiming { Name = $"Unit: {displayName}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None }
                    ProgressBar.increment unitProgress false
                    ProgressBar.finish unitProgress
                    println $"  {displayName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime timer.Elapsed}){Colors.reset}"
                    println $"    {msg}"
                    unitFailedTests.Add({ File = ""; Name = $"Unit: {displayName}"; Message = msg; Details = [] })
                    unitSectionFailed <- unitSectionFailed + 1
                    ProgressBar.update unitProgress

        ProgressBar.finish unitProgress
        unitSectionTimer.Stop()
        if unitSectionFailed = 0 then
            println $"  {Colors.green}✓ {unitSectionPassed} passed{Colors.reset}"
        else
            println $"  {Colors.green}✓ {unitSectionPassed} passed{Colors.reset}, {Colors.red}✗ {unitSectionFailed} failed{Colors.reset}"
        println $"  {Colors.gray}└─ Completed in {formatTime unitSectionTimer.Elapsed}{Colors.reset}"
        println ""
        recordResults unitSectionPassed unitSectionFailed (unitFailedTests |> Seq.toList)

    let runE2EAndVerification () : unit =
        if Directory.Exists e2eDir then
            let e2eTestFiles = Directory.GetFiles(e2eDir, "*.e2e", SearchOption.AllDirectories)
            if e2eTestFiles.Length > 0 then
                let sectionTimer = Stopwatch.StartNew()
                println $"{Colors.cyan}🚀 E2E Tests{Colors.reset}"

                let (allE2ETests, parseErrors) = loadE2ETests e2eTestFiles
                reportParseErrors "E2E" parseErrors

                if allE2ETests.Length > 0 then
                    match getStdlibResult () with
                    | Error e ->
                        println $"  {Colors.red}Stdlib compilation failed: {e}{Colors.reset}"
                        println $"  {Colors.red}Skipping all E2E tests{Colors.reset}"
                    | Ok stdlib ->
                        let timingText =
                            match tryGetStdlibCompileElapsed () with
                            | Some elapsed -> $"(Stdlib compiled in {formatTime elapsed})"
                            | None -> "(Stdlib compiled)"
                        println $"  {Colors.gray}{timingText}{Colors.reset}"

                        let testsArray =
                            allE2ETests
                            |> Array.filter (fun test -> matchesFilter filter test.Name)
                            |> orderE2ETestsByEstimatedCost
                        runE2ESuite "E2E" "E2E" testsArray stdlib

                sectionTimer.Stop()
                println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
                println ""

        if enableVerification && Directory.Exists verificationDir then
            let verificationTestFiles = Directory.GetFiles(verificationDir, "*.e2e", SearchOption.AllDirectories)
            if verificationTestFiles.Length > 0 then
                let sectionTimer = Stopwatch.StartNew()
                println $"{Colors.cyan}🔬 Verification Tests{Colors.reset}"

                let (allVerifTests, parseErrors) = loadE2ETests verificationTestFiles
                reportParseErrors "Verification" parseErrors

                if allVerifTests.Length > 0 then
                    match getStdlibResult () with
                    | Error err ->
                        println $"  {Colors.red}Stdlib compilation failed: {err}{Colors.reset}"
                        println $"  {Colors.red}Skipping verification tests{Colors.reset}"
                    | Ok stdlib ->
                        let testsArray =
                            allVerifTests
                            |> Array.filter (fun test -> matchesFilter filter test.Name)
                            |> orderE2ETestsByEstimatedCost
                        runE2ESuite "Verification" "Verification" testsArray stdlib

                sectionTimer.Stop()
                println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
                println ""

    if runUnitAndE2EInParallel then
        let unitTask = System.Threading.Tasks.Task.Run(fun () -> runUnitTests ())
        let e2eTask = System.Threading.Tasks.Task.Run(fun () -> runE2EAndVerification ())
        System.Threading.Tasks.Task.WaitAll [| unitTask; e2eTask |]
    else
        runUnitTests ()
        runE2EAndVerification ()

    // Compute stdlib coverage only if --coverage flag is set
    let coveragePercent =
        if not showCoverage then None
        else
            match e2eStdlib with
            | None -> None  // No stdlib compiled (no E2E tests ran)
            | Some stdlib ->
                let allStdlibFuncs = CompilerLibrary.getAllStdlibFunctionNamesFromStdlib stdlib
                let coveredFuncs = System.Collections.Generic.HashSet<string>()
                let e2eDir = Path.Combine(assemblyDir, "e2e")
                if Directory.Exists e2eDir then
                    let e2eTestFiles = Directory.GetFiles(e2eDir, "*.e2e", SearchOption.AllDirectories)
                    for testFile in e2eTestFiles do
                        match TestDSL.E2EFormat.parseE2ETestFile testFile with
                        | Error _ -> ()
                        | Ok tests ->
                            for test in tests do
                                match CompilerLibrary.getReachableStdlibFunctionsFromStdlib stdlib test.Source with
                                | Error _ -> ()
                                | Ok reachable ->
                                    for func in reachable do
                                        if Set.contains func allStdlibFuncs then
                                            coveredFuncs.Add(func) |> ignore
                    let totalFuncs = Set.count allStdlibFuncs
                    let coveredCount = coveredFuncs.Count
                    if totalFuncs > 0 then Some (float coveredCount / float totalFuncs * 100.0) else None
                else None

    totalTimer.Stop()

    // Print slowest tests
    if allTimings.Count > 0 then
        println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
        println $"{Colors.bold}{Colors.gray}🐢 Slowest Tests{Colors.reset}"
        println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
        let slowest = allTimings |> Seq.sortByDescending (fun t -> t.TotalTime) |> Seq.truncate 5 |> Seq.toList
        for (i, timing) in slowest |> List.indexed do
            let timingStr =
                match timing.CompileTime, timing.RuntimeTime with
                | Some ct, Some rt ->
                    $"compile: {formatTime ct}  run: {formatTime rt}  total: {formatTime timing.TotalTime}"
                | _ ->
                    $"total: {formatTime timing.TotalTime}"
            let displayName = if timing.Name.Length > 45 then timing.Name.Substring(0, 42) + "..." else timing.Name
            println $"  {Colors.gray}{i + 1}. {displayName,-45} {timingStr}{Colors.reset}"
        println ""

    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}📊 Test Results{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    if failed = 0 then
        println $"  {Colors.green}✓ All tests passed: {passed}/{passed + failed}{Colors.reset}"
    else
        println $"  {Colors.green}✓ Passed: {passed}{Colors.reset}"
        println $"  {Colors.red}✗ Failed: {failed}{Colors.reset}"
    match coveragePercent with
    | Some pct -> println $"  {Colors.gray}📊 Stdlib coverage: {pct:F1}%%{Colors.reset}"
    | None -> ()
    println $"  {Colors.gray}⏱  Total time: {formatTime totalTimer.Elapsed}{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"

    // Print first 10 failing tests summary
    if failedTests.Count > 0 then
        println ""
        println $"{Colors.bold}{Colors.red}═══════════════════════════════════════{Colors.reset}"
        let displayCount = min 10 failedTests.Count
        let moreCount = failedTests.Count - displayCount
        if moreCount > 0 then
            println $"{Colors.bold}{Colors.red}❌ First {displayCount} Failing Tests (of {failedTests.Count} total){Colors.reset}"
        else
            println $"{Colors.bold}{Colors.red}❌ Failing Tests ({failedTests.Count}){Colors.reset}"
        println $"{Colors.bold}{Colors.red}═══════════════════════════════════════{Colors.reset}"
        println ""

        for i in 0 .. displayCount - 1 do
            let test = failedTests.[i]
            let fileName = if test.File <> "" then Path.GetFileName test.File else ""
            // Format: "1. E2E: file.e2e:L43: expression" with file:line in cyan
            let displayName =
                if fileName <> "" && test.Name.StartsWith("E2E: L") then
                    let afterPrefix = test.Name.Substring(5)  // "L43: expression" (skip "E2E: ")
                    $"E2E: {Colors.cyan}{fileName}:{afterPrefix}{Colors.reset}"
                elif fileName <> "" then
                    $"{Colors.cyan}{fileName}: {Colors.reset}{Colors.red}{test.Name}"
                else
                    test.Name
            println $"{Colors.red}{i + 1}. {displayName}{Colors.reset}"
            println $"   {Colors.gray}{test.Message}{Colors.reset}"
            for detail in test.Details do
                println $"   {Colors.gray}{detail}{Colors.reset}"
            println ""

        if moreCount > 0 then
            println $"{Colors.gray}... and {moreCount} more failing test(s){Colors.reset}"
            println ""

    (if failed = 0 then 0 else 1)
    |> fun exitCode ->
        exitCode
