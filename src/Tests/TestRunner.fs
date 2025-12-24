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

// Get optimal parallelism based on system resources
let getOptimalParallelism () : int =
    // Get CPU core count
    let cpuCores = Environment.ProcessorCount

    // Get available memory in GB
    let gcMemoryInfo = System.GC.GetGCMemoryInfo()
    let totalMemoryGB = float gcMemoryInfo.TotalAvailableMemoryBytes / (1024.0 * 1024.0 * 1024.0)

    // Each E2E test needs roughly:
    // - 1 CPU core for compilation + execution
    // - ~200MB memory (compiler + generated binary + test overhead)
    let estimatedMemoryPerTestGB = 0.2

    // Calculate max based on memory
    let maxByMemory = int (totalMemoryGB / estimatedMemoryPerTestGB)

    // Calculate max based on CPU (leave 1 core free for system)
    let maxByCPU = max 1 (cpuCores - 1)

    // Use the smaller of the two, with a minimum of 2 and maximum of 16
    let optimal = min (min maxByMemory maxByCPU) 16 |> max 2

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

[<EntryPoint>]
let main args =
    let totalTimer = Stopwatch.StartNew()

    println $"{Colors.bold}{Colors.cyan}🧪 Running DSL-based Tests{Colors.reset}"
    println ""

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
            println $"{Colors.cyan}📦 ANF→MIR Tests{Colors.reset}"
            println ""

            for testPath in anf2mirTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                print $"  {testName}... "

                match loadANF2MIRTest testPath with
                | Ok (input, expected) ->
                    let result = runANF2MIRTest input expected
                    testTimer.Stop()
                    if result.Success then
                        println $"{Colors.green}✓ PASS{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        passed <- passed + 1
                    else
                        println $"{Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        match result.Expected, result.Actual with
                        | Some exp, Some act ->
                            println "    Expected:"
                            for line in exp.Split('\n') do
                                println $"      {line}"
                            println "    Actual:"
                            for line in act.Split('\n') do
                                println $"      {line}"
                        | _ -> ()
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    println $"{Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failed <- failed + 1

            sectionTimer.Stop()
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run MIR→LIR tests
    let mir2lirDir = Path.Combine(assemblyDir, "passes/mir2lir")
    if Directory.Exists mir2lirDir then
        let mir2lirTests = Directory.GetFiles(mir2lirDir, "*.mir2lir")
        if mir2lirTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🔄 MIR→LIR Tests{Colors.reset}"
            println ""

            for testPath in mir2lirTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                print $"  {testName}... "

                match loadMIR2LIRTest testPath with
                | Ok (input, expected) ->
                    let result = runMIR2LIRTest input expected
                    testTimer.Stop()
                    if result.Success then
                        println $"{Colors.green}✓ PASS{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        passed <- passed + 1
                    else
                        println $"{Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        match result.Expected, result.Actual with
                        | Some exp, Some act ->
                            println "    Expected:"
                            for line in exp.Split('\n') do
                                println $"      {line}"
                            println "    Actual:"
                            for line in act.Split('\n') do
                                println $"      {line}"
                        | _ -> ()
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    println $"{Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failed <- failed + 1

            sectionTimer.Stop()
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run LIR→ARM64 tests
    let lir2arm64Dir = Path.Combine(assemblyDir, "passes/lir2arm64")
    if Directory.Exists lir2arm64Dir then
        let lir2arm64Tests = Directory.GetFiles(lir2arm64Dir, "*.lir2arm64")
        if lir2arm64Tests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🎯 LIR→ARM64 Tests{Colors.reset}"
            println ""

            for testPath in lir2arm64Tests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                print $"  {testName}... "

                match loadLIR2ARM64Test testPath with
                | Ok (input, expected) ->
                    let result = runLIR2ARM64Test input expected
                    testTimer.Stop()
                    if result.Success then
                        println $"{Colors.green}✓ PASS{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        passed <- passed + 1
                    else
                        println $"{Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        match result.Expected, result.Actual with
                        | Some exp, Some act ->
                            println "    Expected:"
                            for line in exp.Split('\n') do
                                println $"      {line}"
                            println "    Actual:"
                            for line in act.Split('\n') do
                                println $"      {line}"
                        | _ -> ()
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    println $"{Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failed <- failed + 1

            sectionTimer.Stop()
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run ARM64 encoding tests
    let arm64encDir = Path.Combine(assemblyDir, "passes/arm64enc")
    if Directory.Exists arm64encDir then
        let arm64encTests = Directory.GetFiles(arm64encDir, "*.arm64enc")
        if arm64encTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}⚙️  ARM64 Encoding Tests{Colors.reset}"
            println ""

            for testPath in arm64encTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()
                print $"  {testName}... "

                match TestDSL.ARM64EncodingTestRunner.loadARM64EncodingTest testPath with
                | Ok test ->
                    let result = TestDSL.ARM64EncodingTestRunner.runARM64EncodingTest test
                    testTimer.Stop()
                    if result.Success then
                        println $"{Colors.green}✓ PASS{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        passed <- passed + 1
                    else
                        println $"{Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        failed <- failed + 1
                | Error msg ->
                    testTimer.Stop()
                    println $"{Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failed <- failed + 1

            sectionTimer.Stop()
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run Type Checking tests
    let typecheckDir = Path.Combine(assemblyDir, "typecheck")
    if Directory.Exists typecheckDir then
        let typecheckTestFiles = Directory.GetFiles(typecheckDir, "*.typecheck", SearchOption.AllDirectories)
        if typecheckTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}📋 Type Checking Tests{Colors.reset}"
            println ""

            // Parse and run all tests from .typecheck files
            let mutable sectionPassed = 0
            let mutable sectionFailed = 0

            for testFile in typecheckTestFiles do
                let fileName = Path.GetFileNameWithoutExtension testFile
                match TestDSL.TypeCheckingTestRunner.runTypeCheckingTestFile testFile with
                | Ok results ->
                    for result in results do
                        let testTimer = Stopwatch.StartNew()
                        match result.Success with
                        | true ->
                            sectionPassed <- sectionPassed + 1
                            passed <- passed + 1
                        | false ->
                            let typeDesc =
                                match result.ExpectedType with
                                | Some t -> TypeChecking.typeToString t
                                | None -> "error"
                            print $"  {typeDesc} ({fileName})... "
                            println $"{Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                            println $"    {result.Message}"
                            sectionFailed <- sectionFailed + 1
                            failed <- failed + 1
                | Error msg ->
                    println $"{Colors.red}✗ ERROR parsing {Path.GetFileName testFile}{Colors.reset}"
                    println $"    {msg}"
                    sectionFailed <- sectionFailed + 1
                    failed <- failed + 1

            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ All {sectionPassed} type checking tests passed{Colors.reset} {Colors.gray}({formatTime sectionTimer.Elapsed}){Colors.reset}"
            else
                println $"  {Colors.yellow}✓ {sectionPassed} passed, ✗ {sectionFailed} failed{Colors.reset} {Colors.gray}({formatTime sectionTimer.Elapsed}){Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run E2E tests (in parallel)
    let e2eDir = Path.Combine(assemblyDir, "e2e")
    if Directory.Exists e2eDir then
        let e2eTestFiles = Directory.GetFiles(e2eDir, "*.e2e", SearchOption.AllDirectories)
        if e2eTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🚀 E2E Tests{Colors.reset}"
            println ""

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
                println $"{Colors.red}✗ ERROR parsing {fileName}{Colors.reset}"
                println $"    {msg}"
                failed <- failed + 1

            // Run tests in parallel (dynamically determined based on system resources) and print as they complete (in order)
            if allTests.Count > 0 then
                let testsArray = allTests.ToArray()
                let numTests = testsArray.Length
                let results = Array.zeroCreate<option<E2ETest * E2ETestResult * TimeSpan>> numTests
                let mutable nextToPrint = 0
                let lockObj = obj()

                // Determine optimal parallelism based on system resources
                let maxParallel = getOptimalParallelism()
                println $"  {Colors.gray}(Running with {maxParallel} parallel tests based on system resources){Colors.reset}"
                println ""

                // Helper function to print a test result
                let printTestResult (test: E2ETest) (result: E2ETestResult) (elapsed: TimeSpan) =
                    print $"  {test.Name}... "
                    if result.Success then
                        println $"{Colors.green}✓ PASS{Colors.reset} {Colors.gray}({formatTime elapsed}){Colors.reset}"
                        lock lockObj (fun () -> passed <- passed + 1)
                    else
                        println $"{Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime elapsed}){Colors.reset}"
                        println $"    {result.Message}"

                        // Show exit code mismatch
                        match result.ExitCode with
                        | Some code when code <> test.ExpectedExitCode ->
                            println $"    Expected exit code: {test.ExpectedExitCode}"
                            println $"    Actual exit code: {code}"
                        | _ -> ()

                        // Show stdout mismatch
                        match test.ExpectedStdout, result.Stdout with
                        | Some expected, Some actual when actual.Trim() <> expected.Trim() ->
                            let expectedDisplay = expected.Replace("\n", "\\n")
                            let actualDisplay = actual.Replace("\n", "\\n")
                            println $"    Expected stdout: {expectedDisplay}"
                            println $"    Actual stdout: {actualDisplay}"
                        | Some expected, None ->
                            let expectedDisplay = expected.Replace("\n", "\\n")
                            println $"    Expected stdout: {expectedDisplay}"
                            println "    Actual: no stdout captured"
                        | None, Some actual when actual.Trim() <> "" ->
                            // Unexpected stdout when none was expected
                            let actualDisplay = actual.Replace("\n", "\\n")
                            println $"    Unexpected stdout: {actualDisplay}"
                        | _ -> ()

                        // Show stderr mismatch
                        match test.ExpectedStderr, result.Stderr with
                        | Some expected, Some actual when actual.Trim() <> expected.Trim() ->
                            let expectedDisplay = expected.Replace("\n", "\\n")
                            let actualDisplay = actual.Replace("\n", "\\n")
                            println $"    Expected stderr: {expectedDisplay}"
                            println $"    Actual stderr: {actualDisplay}"
                        | Some expected, None ->
                            let expectedDisplay = expected.Replace("\n", "\\n")
                            println $"    Expected stderr: {expectedDisplay}"
                            println "    Actual: no stderr captured"
                        | None, Some actual when actual.Trim() <> "" ->
                            // Unexpected stderr when none was expected
                            let actualDisplay = actual.Replace("\n", "\\n")
                            println $"    Unexpected stderr: {actualDisplay}"
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
                options.MaxDegreeOfParallelism <- maxParallel
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
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run unit tests
    let sectionTimer = Stopwatch.StartNew()
    println $"{Colors.cyan}🔧 Unit Tests{Colors.reset}"
    println ""

    let unitTestTimer = Stopwatch.StartNew()
    match EncodingTests.runAll() with
    | Ok () ->
        unitTestTimer.Stop()
        println $"  {Colors.green}✓ Encoding Tests{Colors.reset} {Colors.gray}({formatTime unitTestTimer.Elapsed}){Colors.reset}"
        passed <- passed + 1
    | Error msg ->
        unitTestTimer.Stop()
        println $"  {Colors.red}✗ FAIL: Encoding tests{Colors.reset} {Colors.gray}({formatTime unitTestTimer.Elapsed}){Colors.reset}"
        println $"    {msg}"
        failed <- failed + 1

    let binaryTestTimer = Stopwatch.StartNew()
    match BinaryTests.runAll() with
    | Ok () ->
        binaryTestTimer.Stop()
        println $"  {Colors.green}✓ Binary Tests{Colors.reset} {Colors.gray}({formatTime binaryTestTimer.Elapsed}){Colors.reset}"
        passed <- passed + 11  // 11 tests in BinaryTests
    | Error msg ->
        binaryTestTimer.Stop()
        println $"  {Colors.red}✗ FAIL: Binary tests{Colors.reset} {Colors.gray}({formatTime binaryTestTimer.Elapsed}){Colors.reset}"
        println $"    {msg}"
        failed <- failed + 1

    let typeCheckingTestTimer = Stopwatch.StartNew()
    match TypeCheckingTests.runAll() with
    | Ok () ->
        typeCheckingTestTimer.Stop()
        println $"  {Colors.green}✓ Type Checking Tests{Colors.reset} {Colors.gray}({formatTime typeCheckingTestTimer.Elapsed}){Colors.reset}"
        passed <- passed + 8  // 8 tests in TypeCheckingTests
    | Error msg ->
        typeCheckingTestTimer.Stop()
        println $"  {Colors.red}✗ FAIL: Type checking tests{Colors.reset} {Colors.gray}({formatTime typeCheckingTestTimer.Elapsed}){Colors.reset}"
        println $"    {msg}"
        failed <- failed + 1

    sectionTimer.Stop()
    println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
    println ""

    totalTimer.Stop()
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}📊 Test Results{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    if failed = 0 then
        println $"  {Colors.green}✓ All tests passed: {passed}/{passed + failed}{Colors.reset}"
    else
        println $"  {Colors.green}✓ Passed: {passed}{Colors.reset}"
        println $"  {Colors.red}✗ Failed: {failed}{Colors.reset}"
    println $"  {Colors.gray}⏱  Total time: {formatTime totalTimer.Elapsed}{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"

    if failed = 0 then 0 else 1
