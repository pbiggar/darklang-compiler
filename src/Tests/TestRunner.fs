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
open TestDSL.Profiler

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
    let estimatedMemoryPerTestGB = 0.1

    // Calculate max based on memory
    let maxByMemory = int (totalMemoryGB / estimatedMemoryPerTestGB)

    // Empirically, optimal parallelism is CPU cores + 1 (slight oversubscription)
    // Benchmarking showed cores+1 is ~4% faster than exact core count
    let maxByCPU = cpuCores + 1

    // Use the smaller of the two, with a minimum of 4
    let optimal = min maxByMemory maxByCPU |> max 4

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
    println "  --help, -h         Show this help message"
    println ""
    println "Examples:"
    println "  Tests                      Run all tests"
    println "  Tests --filter=tuple       Run tests with 'tuple' in the name"
    println "  Tests --filter=string      Run tests with 'string' in the name"
    println "  Tests --parallel=4         Run with 4 parallel workers"
    println "  Tests --coverage           Run tests and show coverage percentage"

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

    println $"{Colors.bold}{Colors.cyan}🧪 Running DSL-based Tests{Colors.reset}"
    match filter with
    | Some pattern -> println $"{Colors.gray}  Filter: {pattern}{Colors.reset}"
    | None -> ()
    println ""

    // Get the directory where the assembly is located (where test files are copied)
    let assemblyDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

    let mutable passed = 0
    let mutable failed = 0
    let failedTests = ResizeArray<FailedTestInfo>()
    let allTimings = ResizeArray<TestTiming>()

    // Run ANF→MIR tests
    let anf2mirDir = Path.Combine(assemblyDir, "passes/anf2mir")
    if Directory.Exists anf2mirDir then
        let anf2mirTests = Directory.GetFiles(anf2mirDir, "*.anf2mir") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        if anf2mirTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}📦 ANF→MIR Tests{Colors.reset}"

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create "ANF→MIR" anf2mirTests.Length
            ProgressBar.update progress

            for testPath in anf2mirTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()

                match loadANF2MIRTest testPath with
                | Ok (input, expected) ->
                    let result = runANF2MIRTest input expected
                    testTimer.Stop()
                    allTimings.Add({ Name = $"ANF→MIR: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    if result.Success then
                        passed <- passed + 1
                        sectionPassed <- sectionPassed + 1
                        ProgressBar.increment progress true
                    else
                        ProgressBar.increment progress false
                        ProgressBar.finish progress
                        println $"  {testName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        let details = ResizeArray<string>()
                        match result.Expected, result.Actual with
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
                        failedTests.Add({ File = testPath; Name = $"ANF→MIR: {testName}"; Message = result.Message; Details = details |> Seq.toList })
                        failed <- failed + 1
                        sectionFailed <- sectionFailed + 1
                        ProgressBar.update progress
                | Error msg ->
                    testTimer.Stop()
                    allTimings.Add({ Name = $"ANF→MIR: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    println $"  {testName}... {Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failedTests.Add({ File = testPath; Name = $"ANF→MIR: {testName}"; Message = $"Failed to load test: {msg}"; Details = [] })
                    failed <- failed + 1
                    sectionFailed <- sectionFailed + 1
                    ProgressBar.update progress

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run MIR→LIR tests
    let mir2lirDir = Path.Combine(assemblyDir, "passes/mir2lir")
    if Directory.Exists mir2lirDir then
        let mir2lirTests = Directory.GetFiles(mir2lirDir, "*.mir2lir") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        if mir2lirTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🔄 MIR→LIR Tests{Colors.reset}"

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create "MIR→LIR" mir2lirTests.Length
            ProgressBar.update progress

            for testPath in mir2lirTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()

                match loadMIR2LIRTest testPath with
                | Ok (input, expected) ->
                    let result = runMIR2LIRTest input expected
                    testTimer.Stop()
                    allTimings.Add({ Name = $"MIR→LIR: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    if result.Success then
                        passed <- passed + 1
                        sectionPassed <- sectionPassed + 1
                        ProgressBar.increment progress true
                    else
                        ProgressBar.increment progress false
                        ProgressBar.finish progress
                        println $"  {testName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        let details = ResizeArray<string>()
                        match result.Expected, result.Actual with
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
                        failedTests.Add({ File = testPath; Name = $"MIR→LIR: {testName}"; Message = result.Message; Details = details |> Seq.toList })
                        failed <- failed + 1
                        sectionFailed <- sectionFailed + 1
                        ProgressBar.update progress
                | Error msg ->
                    testTimer.Stop()
                    allTimings.Add({ Name = $"MIR→LIR: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    println $"  {testName}... {Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failedTests.Add({ File = testPath; Name = $"MIR→LIR: {testName}"; Message = $"Failed to load test: {msg}"; Details = [] })
                    failed <- failed + 1
                    sectionFailed <- sectionFailed + 1
                    ProgressBar.update progress

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run LIR→ARM64 tests
    let lir2arm64Dir = Path.Combine(assemblyDir, "passes/lir2arm64")
    if Directory.Exists lir2arm64Dir then
        let lir2arm64Tests = Directory.GetFiles(lir2arm64Dir, "*.lir2arm64") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        if lir2arm64Tests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🎯 LIR→ARM64 Tests{Colors.reset}"

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create "LIR→ARM64" lir2arm64Tests.Length
            ProgressBar.update progress

            for testPath in lir2arm64Tests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()

                match loadLIR2ARM64Test testPath with
                | Ok (input, expected) ->
                    let result = runLIR2ARM64Test input expected
                    testTimer.Stop()
                    allTimings.Add({ Name = $"LIR→ARM64: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    if result.Success then
                        passed <- passed + 1
                        sectionPassed <- sectionPassed + 1
                        ProgressBar.increment progress true
                    else
                        ProgressBar.increment progress false
                        ProgressBar.finish progress
                        println $"  {testName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        let details = ResizeArray<string>()
                        match result.Expected, result.Actual with
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
                        failedTests.Add({ File = testPath; Name = $"LIR→ARM64: {testName}"; Message = result.Message; Details = details |> Seq.toList })
                        failed <- failed + 1
                        sectionFailed <- sectionFailed + 1
                        ProgressBar.update progress
                | Error msg ->
                    testTimer.Stop()
                    allTimings.Add({ Name = $"LIR→ARM64: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    println $"  {testName}... {Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failedTests.Add({ File = testPath; Name = $"LIR→ARM64: {testName}"; Message = $"Failed to load test: {msg}"; Details = [] })
                    failed <- failed + 1
                    sectionFailed <- sectionFailed + 1
                    ProgressBar.update progress

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run ARM64 encoding tests
    let arm64encDir = Path.Combine(assemblyDir, "passes/arm64enc")
    if Directory.Exists arm64encDir then
        let arm64encTests = Directory.GetFiles(arm64encDir, "*.arm64enc") |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
        if arm64encTests.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}⚙️  ARM64 Encoding Tests{Colors.reset}"

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create "ARM64 Enc" arm64encTests.Length
            ProgressBar.update progress

            for testPath in arm64encTests do
                let testName = Path.GetFileName testPath
                let testTimer = Stopwatch.StartNew()

                match TestDSL.ARM64EncodingTestRunner.loadARM64EncodingTest testPath with
                | Ok test ->
                    let result = TestDSL.ARM64EncodingTestRunner.runARM64EncodingTest test
                    testTimer.Stop()
                    allTimings.Add({ Name = $"ARM64 Encoding: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    if result.Success then
                        passed <- passed + 1
                        sectionPassed <- sectionPassed + 1
                        ProgressBar.increment progress true
                    else
                        ProgressBar.increment progress false
                        ProgressBar.finish progress
                        println $"  {testName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                        println $"    {result.Message}"
                        failedTests.Add({ File = testPath; Name = $"ARM64 Encoding: {testName}"; Message = result.Message; Details = [] })
                        failed <- failed + 1
                        sectionFailed <- sectionFailed + 1
                        ProgressBar.update progress
                | Error msg ->
                    testTimer.Stop()
                    allTimings.Add({ Name = $"ARM64 Encoding: {testName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    println $"  {testName}... {Colors.red}✗ ERROR{Colors.reset} {Colors.gray}({formatTime testTimer.Elapsed}){Colors.reset}"
                    println $"    Failed to load test: {msg}"
                    failedTests.Add({ File = testPath; Name = $"ARM64 Encoding: {testName}"; Message = $"Failed to load test: {msg}"; Details = [] })
                    failed <- failed + 1
                    sectionFailed <- sectionFailed + 1
                    ProgressBar.update progress

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run Type Checking tests
    let typecheckDir = Path.Combine(assemblyDir, "typecheck")
    if Directory.Exists typecheckDir then
        let typecheckTestFiles = Directory.GetFiles(typecheckDir, "*.typecheck", SearchOption.AllDirectories) |> Array.filter (fun p -> matchesFilter filter (Path.GetFileNameWithoutExtension p))
        if typecheckTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}📋 Type Checking Tests{Colors.reset}"

            // Parse and run all tests from .typecheck files
            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create "TypeCheck" typecheckTestFiles.Length
            ProgressBar.update progress

            for testFile in typecheckTestFiles do
                let fileName = Path.GetFileNameWithoutExtension testFile
                let testTimer = Stopwatch.StartNew()
                match TestDSL.TypeCheckingTestRunner.runTypeCheckingTestFile testFile with
                | Ok results ->
                    testTimer.Stop()
                    let fileSuccess = results |> List.forall (fun r -> r.Success)
                    let filePassCount = results |> List.filter (fun r -> r.Success) |> List.length
                    let fileFailCount = results |> List.filter (fun r -> not r.Success) |> List.length
                    allTimings.Add({ Name = $"TypeCheck: {fileName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    sectionPassed <- sectionPassed + filePassCount
                    passed <- passed + filePassCount
                    if fileSuccess then
                        ProgressBar.increment progress true
                    else
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
                                failedTests.Add({ File = testFile; Name = $"Type Checking: {typeDesc} ({fileName})"; Message = result.Message; Details = [] })
                                sectionFailed <- sectionFailed + 1
                                failed <- failed + 1
                                ProgressBar.update progress
                | Error msg ->
                    testTimer.Stop()
                    allTimings.Add({ Name = $"TypeCheck: {fileName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName testFile}{Colors.reset}"
                    println $"    {msg}"
                    failedTests.Add({ File = testFile; Name = $"Type Checking: {Path.GetFileName testFile}"; Message = msg; Details = [] })
                    sectionFailed <- sectionFailed + 1
                    failed <- failed + 1
                    ProgressBar.update progress

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run Optimization Tests (ANF, MIR, LIR)
    let optDir = Path.Combine(assemblyDir, "optimization")
    if Directory.Exists optDir then
        let optTestFiles = Directory.GetFiles(optDir, "*.opt", SearchOption.AllDirectories)
        if optTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}⚡ Optimization Tests{Colors.reset}"

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let progress = ProgressBar.create "Optimization" optTestFiles.Length
            ProgressBar.update progress

            for testFile in optTestFiles do
                let fileName = Path.GetFileNameWithoutExtension testFile
                let testTimer = Stopwatch.StartNew()
                // Determine stage from filename (anf.opt -> ANF, mir.opt -> MIR, lir.opt -> LIR)
                let stage =
                    if fileName.ToLower().Contains("anf") then TestDSL.OptimizationFormat.ANF
                    elif fileName.ToLower().Contains("mir") then TestDSL.OptimizationFormat.MIR
                    elif fileName.ToLower().Contains("lir") then TestDSL.OptimizationFormat.LIR
                    else TestDSL.OptimizationFormat.ANF  // Default to ANF

                match TestDSL.OptimizationTestRunner.runTestFile stage testFile with
                | Error msg ->
                    testTimer.Stop()
                    allTimings.Add({ Name = $"Optimization: {fileName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    ProgressBar.increment progress false
                    ProgressBar.finish progress
                    println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName testFile}{Colors.reset}"
                    println $"    {msg}"
                    failedTests.Add({ File = testFile; Name = $"Optimization: {Path.GetFileName testFile}"; Message = msg; Details = [] })
                    sectionFailed <- sectionFailed + 1
                    failed <- failed + 1
                    ProgressBar.update progress
                | Ok results ->
                    testTimer.Stop()
                    let filteredResults = results |> List.filter (fun (test, _) -> matchesFilter filter test.Name)
                    let fileSuccess = filteredResults |> List.forall (fun (_, r) -> r.Success)
                    let filePassCount = filteredResults |> List.filter (fun (_, r) -> r.Success) |> List.length
                    allTimings.Add({ Name = $"Optimization: {fileName}"; TotalTime = testTimer.Elapsed; CompileTime = None; RuntimeTime = None })
                    sectionPassed <- sectionPassed + filePassCount
                    passed <- passed + filePassCount
                    if fileSuccess then
                        ProgressBar.increment progress true
                    else
                        for (test, result) in filteredResults do
                            if not result.Success then
                                ProgressBar.increment progress false
                                ProgressBar.finish progress
                                println $"  {test.Name}... {Colors.red}✗ FAIL{Colors.reset}"
                                println $"    {result.Message}"
                                let details = ResizeArray<string>()
                                match result.Expected, result.Actual with
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
                                failedTests.Add({ File = testFile; Name = $"Optimization: {test.Name}"; Message = result.Message; Details = details |> Seq.toList })
                                sectionFailed <- sectionFailed + 1
                                failed <- failed + 1
                                ProgressBar.update progress

            ProgressBar.finish progress
            sectionTimer.Stop()
            if sectionFailed = 0 then
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
            else
                println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run E2E tests (in parallel)
    // Store stdlib for reuse in coverage analysis (avoids recompiling)
    let mutable e2eStdlib : CompilerLibrary.StdlibResult option = None
    let e2eDir = Path.Combine(assemblyDir, "e2e")
    if Directory.Exists e2eDir then
        let e2eTestFiles = Directory.GetFiles(e2eDir, "*.e2e", SearchOption.AllDirectories)
        if e2eTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🚀 E2E Tests{Colors.reset}"

            // Parse all tests from .e2e files
            let allE2ETests = ResizeArray<TestDSL.E2EFormat.E2ETest>()
            let mutable parseErrors = []

            for testFile in e2eTestFiles do
                match TestDSL.E2EFormat.parseE2ETestFile testFile with
                | Ok tests -> allE2ETests.AddRange(tests)
                | Error msg ->
                    parseErrors <- (testFile, msg) :: parseErrors

            // Report parse errors
            for (filePath, msg) in parseErrors do
                let fileName = Path.GetFileName filePath
                println $"  {Colors.red}✗ ERROR parsing {fileName}{Colors.reset}"
                println $"    {msg}"
                failedTests.Add({ File = filePath; Name = $"E2E: {fileName}"; Message = msg; Details = [] })
                failed <- failed + 1

            // Run tests in parallel with progress bar
            if allE2ETests.Count > 0 then
                match TestDSL.E2ETestRunner.compileStdlib() with
                | Error e ->
                    println $"  {Colors.red}Stdlib compilation failed: {e}{Colors.reset}"
                    println $"  {Colors.red}Skipping all E2E tests{Colors.reset}"
                | Ok stdlib ->
                    e2eStdlib <- Some stdlib
                    println $"  {Colors.gray}(Stdlib compiled){Colors.reset}"

                    let testsArray = allE2ETests.ToArray() |> Array.filter (fun test -> matchesFilter filter test.Name)
                    let numTests = testsArray.Length

                    let precompileTasks =
                        testsArray
                        |> Array.toList
                        |> List.groupBy (fun test -> (test.SourceFile, test.Preamble))
                        |> List.map (fun ((sourceFile, preamble), group) ->
                            let funcLineMap =
                                group
                                |> List.tryHead
                                |> Option.map (fun test -> test.FunctionLineMap)
                                |> Option.defaultValue Map.empty
                            let task =
                                System.Threading.Tasks.Task.Run(fun () ->
                                    TestDSL.E2ETestRunner.precompilePreamble stdlib sourceFile preamble funcLineMap)
                            ((sourceFile, preamble), task))
                        |> Map.ofList

                    let results = Array.zeroCreate<option<E2ETest * E2ETestResult>> numTests
                    let lockObj = obj()
                    let progress = ProgressBar.create "E2E" numTests
                    ProgressBar.update progress

                    let maxParallel = overrideParallel |> Option.defaultWith getOptimalParallelism

                    // Run tests in parallel (wait on preamble precompile per file)
                    let options = System.Threading.Tasks.ParallelOptions()
                    options.MaxDegreeOfParallelism <- maxParallel
                    System.Threading.Tasks.Parallel.For(
                        0,
                        numTests,
                        options,
                        fun i ->
                            let test = testsArray.[i]
                            let preambleResult =
                                match Map.tryFind (test.SourceFile, test.Preamble) precompileTasks with
                                | Some task ->
                                    let meta =
                                        [ ("file", test.SourceFile)
                                          ("test", test.Name) ]
                                    time "preamble_wait" meta (fun () -> task.Result)
                                | None -> Ok ()
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
                                allTimings.Add({ Name = $"E2E: {test.Name}"; TotalTime = totalTime; CompileTime = Some result.CompileTime; RuntimeTime = Some result.RuntimeTime })
                                ProgressBar.increment progress result.Success
                            )
                    ) |> ignore

                    ProgressBar.finish progress

                    // Print failures after progress bar
                    let mutable sectionPassed = 0
                    let mutable sectionFailed = 0
                    for result in results do
                        match result with
                        | Some (test, testResult) ->
                            if testResult.Success then
                                passed <- passed + 1
                                sectionPassed <- sectionPassed + 1
                            else
                                let totalTime = testResult.CompileTime + testResult.RuntimeTime
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

                                failedTests.Add({ File = test.SourceFile; Name = $"E2E: {test.Name}"; Message = testResult.Message; Details = details |> Seq.toList })
                                failed <- failed + 1
                                sectionFailed <- sectionFailed + 1
                        | None -> ()

                    if sectionFailed = 0 then
                        println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
                    else
                        println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"

            sectionTimer.Stop()
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run Verification tests (only if ENABLE_VERIFICATION_TESTS=true)
    let verificationDir = Path.Combine(assemblyDir, "verification")
    let enableVerification = Environment.GetEnvironmentVariable("ENABLE_VERIFICATION_TESTS") = "true"
    if enableVerification && Directory.Exists verificationDir then
        let verificationTestFiles = Directory.GetFiles(verificationDir, "*.e2e", SearchOption.AllDirectories)
        if verificationTestFiles.Length > 0 then
            let sectionTimer = Stopwatch.StartNew()
            println $"{Colors.cyan}🔬 Verification Tests{Colors.reset}"

            let allVerifTests = ResizeArray<TestDSL.E2EFormat.E2ETest>()
            let mutable parseErrors = []
            for testFile in verificationTestFiles do
                match TestDSL.E2EFormat.parseE2ETestFile testFile with
                | Ok tests -> allVerifTests.AddRange(tests)
                | Error msg -> parseErrors <- (testFile, msg) :: parseErrors

            for (filePath, msg) in parseErrors do
                println $"  {Colors.red}✗ ERROR parsing {Path.GetFileName filePath}{Colors.reset}"
                println $"    {msg}"
                failedTests.Add({ File = filePath; Name = $"Verification: {Path.GetFileName filePath}"; Message = msg; Details = [] })
                failed <- failed + 1

            if allVerifTests.Count > 0 then
                let stdlib = match e2eStdlib with Some s -> s | None -> match TestDSL.E2ETestRunner.compileStdlib() with Ok s -> s | Error _ -> failwith "Stdlib compilation failed"
                let testsArray = allVerifTests.ToArray() |> Array.filter (fun test -> matchesFilter filter test.Name)
                let numTests = testsArray.Length

                let precompileTasks =
                    testsArray
                    |> Array.toList
                    |> List.groupBy (fun test -> (test.SourceFile, test.Preamble))
                    |> List.map (fun ((sourceFile, preamble), group) ->
                        let funcLineMap =
                            group
                            |> List.tryHead
                            |> Option.map (fun test -> test.FunctionLineMap)
                            |> Option.defaultValue Map.empty
                        let task =
                            System.Threading.Tasks.Task.Run(fun () ->
                                TestDSL.E2ETestRunner.precompilePreamble stdlib sourceFile preamble funcLineMap)
                        ((sourceFile, preamble), task))
                    |> Map.ofList

                let results = Array.zeroCreate<option<E2ETest * E2ETestResult>> numTests
                let lockObj = obj()
                let progress = ProgressBar.create "Verification" numTests
                ProgressBar.update progress
                let maxParallel = overrideParallel |> Option.defaultWith getOptimalParallelism

                let options = System.Threading.Tasks.ParallelOptions()
                options.MaxDegreeOfParallelism <- maxParallel
                System.Threading.Tasks.Parallel.For(0, numTests, options, fun i ->
                    let test = testsArray.[i]
                    let preambleResult =
                        match Map.tryFind (test.SourceFile, test.Preamble) precompileTasks with
                        | Some task ->
                            let meta =
                                [ ("file", test.SourceFile)
                                  ("test", test.Name) ]
                            time "preamble_wait" meta (fun () -> task.Result)
                        | None -> Ok ()
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
                        allTimings.Add({ Name = $"Verification: {test.Name}"; TotalTime = totalTime; CompileTime = Some result.CompileTime; RuntimeTime = Some result.RuntimeTime })
                        ProgressBar.increment progress result.Success
                    )
                ) |> ignore

                ProgressBar.finish progress

                let mutable sectionPassed = 0
                let mutable sectionFailed = 0
                for result in results do
                    match result with
                    | Some (test, testResult) ->
                        if testResult.Success then
                            passed <- passed + 1
                            sectionPassed <- sectionPassed + 1
                        else
                            let cleanName = test.Name.Replace("Stdlib.", "")
                            let displayName = if cleanName.Length > 60 then cleanName.Substring(0, 57) + "..." else cleanName
                            println $"  {displayName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}(compile: {formatTime testResult.CompileTime}, run: {formatTime testResult.RuntimeTime}){Colors.reset}"
                            println $"    {testResult.Message}"
                            failedTests.Add({ File = test.SourceFile; Name = $"Verification: {test.Name}"; Message = testResult.Message; Details = [] })
                            failed <- failed + 1
                            sectionFailed <- sectionFailed + 1
                    | None -> ()

                if sectionFailed = 0 then
                    println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}"
                else
                    println $"  {Colors.green}✓ {sectionPassed} passed{Colors.reset}, {Colors.red}✗ {sectionFailed} failed{Colors.reset}"

            sectionTimer.Stop()
            println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
            println ""

    // Run unit tests
    let unitSectionTimer = Stopwatch.StartNew()
    println $"{Colors.cyan}🔧 Unit Tests{Colors.reset}"

    let mutable unitSectionPassed = 0
    let mutable unitSectionFailed = 0

    // Define unit test suites with their names and test counts
    let allUnitTests : (string * int * (unit -> Result<unit, string>)) array = [|
        ("CLI Flags Tests", 1, fun () -> CliFlagTests.runAll())
        ("Compiler Caching Tests", 1, fun () -> CompilerCachingTests.runAll())
        ("Profiler Tests", 1, fun () -> ProfilerTests.runAll())
        ("Preamble Precompile Tests", 1, fun () -> PreamblePrecompileTests.runAll())
        ("Encoding Tests", 1, fun () -> EncodingTests.runAll())
        ("Binary Tests", 11, fun () -> BinaryTests.runAll())
        ("Type Checking Tests", 8, fun () -> TypeCheckingTests.runAll())
        ("Parallel Move Tests", 8, fun () -> ParallelMoveTests.runAll())
        ("SSA Liveness Tests", 4, fun () -> SSALivenessTests.runAll())
        ("Phi Resolution Tests", 5, fun () -> PhiResolutionTests.runAll())
    |]
    let unitTests = allUnitTests |> Array.filter (fun (name, _, _) -> matchesFilter filter name)

    let unitProgress = ProgressBar.create "Unit" (unitTests.Length + 1)  // +1 for Chordal Graph
    ProgressBar.update unitProgress

    for (name, count, runTest) in unitTests do
        let timer = Stopwatch.StartNew()
        match runTest() with
        | Ok () ->
            timer.Stop()
            allTimings.Add({ Name = $"Unit: {name}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None })
            passed <- passed + count
            unitSectionPassed <- unitSectionPassed + count
            ProgressBar.increment unitProgress true
        | Error msg ->
            timer.Stop()
            allTimings.Add({ Name = $"Unit: {name}"; TotalTime = timer.Elapsed; CompileTime = None; RuntimeTime = None })
            ProgressBar.increment unitProgress false
            ProgressBar.finish unitProgress
            println $"  {name}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime timer.Elapsed}){Colors.reset}"
            println $"    {msg}"
            failedTests.Add({ File = ""; Name = $"Unit: {name}"; Message = msg; Details = [] })
            failed <- failed + 1
            unitSectionFailed <- unitSectionFailed + 1
            ProgressBar.update unitProgress

    // Handle Chordal Graph Tests separately (has different structure)
    if matchesFilter filter "Chordal Graph Tests" then
        let chordalTimer = Stopwatch.StartNew()
        let results = ChordalGraphTests.runAllTests()
        chordalTimer.Stop()
        allTimings.Add({ Name = "Unit: Chordal Graph Tests"; TotalTime = chordalTimer.Elapsed; CompileTime = None; RuntimeTime = None })
        let failures = results |> List.filter (fun (_, r) -> match r with Error _ -> true | Ok _ -> false)
        let passedCount = results |> List.filter (fun (_, r) -> match r with Ok _ -> true | Error _ -> false) |> List.length
        passed <- passed + passedCount
        unitSectionPassed <- unitSectionPassed + passedCount
        if List.isEmpty failures then
            ProgressBar.increment unitProgress true
        else
            ProgressBar.increment unitProgress false
            ProgressBar.finish unitProgress
            println $"  Chordal Graph Tests... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}({formatTime chordalTimer.Elapsed}){Colors.reset}"
            for (name, result) in failures do
                match result with
                | Error msg ->
                    println $"    {name}: {msg}"
                    failedTests.Add({ File = ""; Name = $"Unit: Chordal Graph - {name}"; Message = msg; Details = [] })
                    failed <- failed + 1
                    unitSectionFailed <- unitSectionFailed + 1
                | Ok _ -> ()
            ProgressBar.update unitProgress

    ProgressBar.finish unitProgress
    unitSectionTimer.Stop()
    if unitSectionFailed = 0 then
        println $"  {Colors.green}✓ {unitSectionPassed} passed{Colors.reset}"
    else
        println $"  {Colors.green}✓ {unitSectionPassed} passed{Colors.reset}, {Colors.red}✗ {unitSectionFailed} failed{Colors.reset}"
    println $"  {Colors.gray}└─ Completed in {formatTime unitSectionTimer.Elapsed}{Colors.reset}"
    println ""

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
        writeReport "profiles/test-profile.csv"
        CompilerProfiler.writeReport "profiles/compiler-profile.csv"
        exitCode
