// TestRunner.fs - Test runner entrypoint and suite orchestration.
//
// Defines Dark compiler test suites and their execution order.

module TestRunner.Main

open System
open System.IO
open System.Diagnostics
open System.Globalization
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Output
open TestDSL.PassTestRunner
open TestDSL.E2EFormat
open TestDSL.E2ETestRunner
open TestDSL.OptimizationFormat
open TestDSL.OptimizationTestRunner
open TestRunnerArgs
open TestFramework

// Print help message
let printHelp () =
    println "Usage: Tests [OPTIONS]"
    println ""
    println "Options:"
    println "  --filter=PATTERN   Run only tests matching PATTERN (case-insensitive substring)"
    println "  --coverage         Show stdlib coverage percentage after running tests"
    println "  --verification     Enable verification/stress tests"
    println "  --parser-pretty-roundtrip  Legacy no-op (parser/pretty corpus roundtrip runs by default)"
    println "  --roundtrip-all-dark  Include all upstream .dark files in parser/pretty corpus roundtrip"
    println "  --all-test-timings  Print timing for every test in final timing summary"
    println "  --timings-json=PATH  Write machine-readable timing data to PATH"
    println "  --quiet            Quiet mode: print 'success' or list failed tests"
    println "  --ai               AI mode: compact output with a dot every 250 completed tests"
    println "  --verbose, -v      Print failing tests as soon as they occur"
    println "  --help, -h         Show this help message"
    println ""
    println "Examples:"
    println "  Tests                      Run all tests"
    println "  Tests --filter=tuple       Run tests with 'tuple' in the name"
    println "  Tests --filter=string      Run tests with 'string' in the name"
    println "  Tests --coverage           Run tests and show coverage percentage"
    println "  Tests --verification       Run verification/stress tests"
    println "  Tests --parser-pretty-roundtrip  Legacy no-op (corpus roundtrip already enabled)"
    println "  Tests --roundtrip-all-dark  Roundtrip all upstream .dark files and stop on first error"
    println "  Tests --all-test-timings  Show timing for every test at the end"
    println "  Tests --timings-json=/tmp/timings.json  Write timing data as JSON"

type private TestRunResult =
    { ExitCode: int
      State: TestRunState
      TotalTime: TimeSpan
      UnaccountedBreakdown: UnaccountedTimeBreakdown }

let private aiProgressTestInterval = 250

let private emptyRunResult (exitCode: int) : TestRunResult =
    { ExitCode = exitCode
      State = TestFramework.createState ()
      TotalTime = TimeSpan.Zero
      UnaccountedBreakdown =
        { Unaccounted = TimeSpan.Zero
          Runtime = TimeSpan.Zero
          Overhead = TimeSpan.Zero } }

type TimingJsonSummary =
    { passed: int
      failed: int
      total: int
      total_ms: float
      unaccounted_ms: float
      runtime_unaccounted_ms: float
      overhead_unaccounted_ms: float }

type TimingJsonTest =
    { name: string
      total_ms: float
      compile_ms: Nullable<float>
      runtime_ms: Nullable<float> }

type TimingJsonPass =
    { name: string
      elapsed_ms: float }

type TimingJsonPayload =
    { summary: TimingJsonSummary
      tests: TimingJsonTest array
      passes: TimingJsonPass array }

let private milliseconds (elapsed: TimeSpan) : float =
    Math.Round(elapsed.TotalMilliseconds, 3)

let private optionalMilliseconds (elapsed: TimeSpan option) : Nullable<float> =
    match elapsed with
    | Some value -> Nullable(milliseconds value)
    | None -> Nullable()

let private runTestsWithProgressReporter (completedTestReporter: (int -> unit) option) args =
    // Check for --help flag
    if hasHelpArg args then
        printHelp ()
        emptyRunResult 0
    else

    let totalTimer = Stopwatch.StartNew()

    // Check for --filter=PATTERN argument
    let filter = parseFilterArg args

    // Check for --coverage flag (show inline coverage after tests)
    let showCoverage = hasCoverageArg args

    // Check for --verification flag (enable verification/stress tests)
    let verificationEnabled = hasVerificationArg args

    // Parser/pretty corpus roundtrip always runs by default.
    // Keep --parser-pretty-roundtrip accepted as a compatibility no-op.
    let _parserPrettyRoundtripFlagPresent = hasParserPrettyRoundtripArg args

    // Include every upstream .dark file in corpus roundtrip mode.
    let roundtripAllDark = hasRoundtripAllDarkArg args

    // Print timing for every test in the final timing section.
    let showAllTestTimings = hasAllTestTimingsArg args

    // Check for --verbose flag (print failing tests immediately)
    let verbose = hasVerboseArg args

    // Optionally write machine-readable timing data to JSON.
    let timingsJsonPath =
        match parseTimingsJsonArg args with
        | Ok path -> path
        | Error msg -> Crash.crash msg

    println $"{Colors.bold}{Colors.cyan}🧪 Running DSL-based Tests{Colors.reset}"
    match filter with
    | Some pattern -> println $"{Colors.gray}  Filter: {pattern}{Colors.reset}"
    | None -> ()
    println $"{Colors.gray}  Parser/pretty corpus roundtrip: enabled (default){Colors.reset}"
    if roundtripAllDark then
        println $"{Colors.gray}  Roundtrip upstream .dark coverage: all files (mode enabled){Colors.reset}"
    else
        println $"{Colors.gray}  Roundtrip upstream .dark coverage: default subset{Colors.reset}"
    if showAllTestTimings then
        println $"{Colors.gray}  Per-test timings: all tests (mode enabled){Colors.reset}"
    match timingsJsonPath with
    | Some path ->
        println $"{Colors.gray}  Timing JSON output: {path}{Colors.reset}"
    | None -> ()
    println ""

    // Use the source tree for test data to avoid copying files into the build output.
    let testDataRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
    let getTestFiles (dir : string) (suffix: string) =
        let fulldir = Path.Combine(testDataRoot, dir)
        Directory.GetFiles(fulldir, $"*.{suffix}", SearchOption.AllDirectories)

    let eifUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "flow-control", "eif.dark")
    let ematchUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "flow-control", "ematch.dark")
    let eapplyUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "apply", "eapply.dark")
    let aliasesUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "custom-data", "aliases.dark")
    let recordsUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "custom-data", "records.dark")
    let recordFieldAccessUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "custom-data", "record-field-acess.dark")
    let einfixUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "apply", "einfix.dark")
    let eandUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "basic", "eand.dark")
    let eorUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "basic", "eor.dark")
    let evariableUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "basic", "evariable.dark")
    let dfloatUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "basic", "dfloat.dark")
    let estringUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "basic", "estring.dark")
    let eletUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "basic", "elet.dark")
    let epipeUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "flow-control", "epipe.dark")
    let derrorUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "derror.dark")
    let elambdaUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "elambda.dark")
    let dtupleUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "collections", "dtuple.dark")
    let edictUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "language", "collections", "edict.dark")
    let dateUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "date.dark")
    let durationUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "duration.dark")
    let listUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "list.dark")
    let dictUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "dict.dark")
    let uuidUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "uuid.dark")
    let charToAsciiCodeUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "char-to-ascii-code.dark")
    let charUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "char.dark")
    let stringUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "string.dark")
    let regexUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "regex.dark")
    let optionUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "option.dark")
    let resultUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "result.dark")
    let mathUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "math.dark")
    let floatUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "float.dark")
    let altJsonUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "alt-json.dark")
    let jsonUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "json.dark")
    let int64UpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "ints", "int64.dark")
    let intUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "ints", "int.dark")
    let blobUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "bytes.dark")
    let base64UpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "base64.dark")
    let cryptoUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "crypto.dark")
    let x509UpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "x509.dark")
    let htmlUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "html.dark")
    let httpUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "http.dark")
    let streamUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "stream.dark")
    let cliPathUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "cli-path.dark")
    let cliGlobUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "cli-glob.dark")
    let cliProcessUpstreamDarkPath =
        Path.Combine(testDataRoot, "e2e", "upstream", "stdlib", "cli-process.dark")
    let upstreamDarkPaths =
        [| eifUpstreamDarkPath
           ematchUpstreamDarkPath
           eapplyUpstreamDarkPath
           aliasesUpstreamDarkPath
           recordsUpstreamDarkPath
           recordFieldAccessUpstreamDarkPath
           einfixUpstreamDarkPath
           eandUpstreamDarkPath
           eorUpstreamDarkPath
           evariableUpstreamDarkPath
           dfloatUpstreamDarkPath
           estringUpstreamDarkPath
           eletUpstreamDarkPath
           epipeUpstreamDarkPath
           derrorUpstreamDarkPath
           elambdaUpstreamDarkPath
           dtupleUpstreamDarkPath
           edictUpstreamDarkPath
           dateUpstreamDarkPath
           durationUpstreamDarkPath
           listUpstreamDarkPath
           dictUpstreamDarkPath
           uuidUpstreamDarkPath
           charToAsciiCodeUpstreamDarkPath
           charUpstreamDarkPath
           stringUpstreamDarkPath
           regexUpstreamDarkPath
           optionUpstreamDarkPath
           resultUpstreamDarkPath
           mathUpstreamDarkPath
           floatUpstreamDarkPath
           altJsonUpstreamDarkPath
           jsonUpstreamDarkPath
           int64UpstreamDarkPath
           intUpstreamDarkPath
           blobUpstreamDarkPath
           base64UpstreamDarkPath
           cryptoUpstreamDarkPath
           x509UpstreamDarkPath
           htmlUpstreamDarkPath
           httpUpstreamDarkPath
           streamUpstreamDarkPath
           cliPathUpstreamDarkPath
           cliGlobUpstreamDarkPath
           cliProcessUpstreamDarkPath |]
    let defaultUpstreamDarkPaths =
        [| eifUpstreamDarkPath
           ematchUpstreamDarkPath
           eapplyUpstreamDarkPath
           aliasesUpstreamDarkPath
           recordsUpstreamDarkPath
           recordFieldAccessUpstreamDarkPath
           einfixUpstreamDarkPath
           eandUpstreamDarkPath
           eorUpstreamDarkPath
           evariableUpstreamDarkPath
           dfloatUpstreamDarkPath
           estringUpstreamDarkPath
           eletUpstreamDarkPath
           epipeUpstreamDarkPath
           derrorUpstreamDarkPath
           dtupleUpstreamDarkPath
           edictUpstreamDarkPath
           dateUpstreamDarkPath
           durationUpstreamDarkPath
           listUpstreamDarkPath
           dictUpstreamDarkPath
           uuidUpstreamDarkPath
           charToAsciiCodeUpstreamDarkPath
           charUpstreamDarkPath
           stringUpstreamDarkPath
           regexUpstreamDarkPath
           optionUpstreamDarkPath
           resultUpstreamDarkPath
           mathUpstreamDarkPath
           floatUpstreamDarkPath
           altJsonUpstreamDarkPath
           jsonUpstreamDarkPath
           int64UpstreamDarkPath
           intUpstreamDarkPath
           htmlUpstreamDarkPath
           httpUpstreamDarkPath
           streamUpstreamDarkPath
           cliPathUpstreamDarkPath
           cliGlobUpstreamDarkPath
           cliProcessUpstreamDarkPath |]
    for path in upstreamDarkPaths do
        if not (File.Exists path) then
            Crash.crash $"Missing required upstream dark test file: {path}"

    let allUpstreamDarkPaths =
        Directory.GetFiles(
            Path.Combine(testDataRoot, "e2e", "upstream"),
            "*.dark",
            SearchOption.AllDirectories
        )
        |> Array.sort

    let filterUpstreamDarkPaths (paths: string array) : string array =
        match filter with
        | None -> paths
        | Some pattern ->
            let loweredPattern = pattern.Trim().ToLowerInvariant()
            paths
            |> Array.filter (fun path -> path.ToLowerInvariant().Contains(loweredPattern))

    let includeUpstreamDarkPathsForE2E =
        match filter with
        | None -> defaultUpstreamDarkPaths
        | Some _ ->
            let pathMatches = filterUpstreamDarkPaths upstreamDarkPaths
            // A filter may name an expression rather than its source file.
            // Load the enabled upstream inventory so matchesE2EFilter below can
            // select those expression names instead of incorrectly reporting 0/0.
            if Array.isEmpty pathMatches then upstreamDarkPaths else pathMatches

    let includeUpstreamDarkPathsForRoundtrip =
        if roundtripAllDark then
            filterUpstreamDarkPaths allUpstreamDarkPaths
        else
            includeUpstreamDarkPathsForE2E

    let e2eFiles = getTestFiles "e2e" "e2e"

    let e2eTestFiles =
        Array.append
            e2eFiles
            includeUpstreamDarkPathsForE2E
    let roundtripCorpusTestFiles =
        Array.append
            e2eFiles
            includeUpstreamDarkPathsForRoundtrip
    let verificationTestFiles = getTestFiles "verification" "e2e"
    let optTestFiles = getTestFiles "optimization" "opt"
    let typecheckTestFiles = getTestFiles "typecheck" "typecheck"
    let anf2mirTestFiles  = getTestFiles "passes/anf2mir" "anf2mir"
    let mir2lirTestFiles  = getTestFiles "passes/mir2lir" "mir2lir"
    let lir2arm64TestFiles  = getTestFiles "passes/lir2arm64" "lir2arm64"
    let arm64encTestFiles  = getTestFiles "passes/arm64enc" "arm64enc"
    let x64encTestFiles = getTestFiles "passes/x64enc" "x64enc"
    let graphColorTestFiles = getTestFiles "algorithms/graph-color" "graphcolor"
    let parallelMoveTestFiles = getTestFiles "algorithms/parallel-moves" "parallelmoves"
    let irFormatSnapshotTestFiles = getTestFiles "formatting/ir" "irformat"
    let lirExecutionTestFiles = getTestFiles "backend/x64" "lirexec"
    let formattingRoundtripTestFiles = getTestFiles "formatting-roundtrip" "roundtrip"
    let syntaxTestFiles = getTestFiles "syntax" "syntax"

    let unitStdlibSuites = [ "Stdlib Compile Tests"; "Preamble Build Tests" ]
    let buildUnitTests (stdlib: CompilerLibrary.StdlibResult) : UnitTestSuite array = [|
        { Name = "Platform Tests"; Tests = PlatformTests.tests }
        { Name = "ValueSearch Catalog Tests"; Tests = ValueSearchCatalogTests.tests stdlib }
        { Name = "Program Structure Tests"; Tests = ProgramStructureTests.tests stdlib }
        { Name = "IR Symbol Tests"; Tests = IRSymbolTests.tests }
        { Name = "IR Printer Tests"; Tests = IRPrinterTests.tests }
        { Name = "ANF to MIR Tests"; Tests = ANFToMIRTests.tests }
        { Name = "MIR Optimize Tests"; Tests = MIROptimizeTests.tests }
        { Name = "LIR Peephole Tests"; Tests = LIRPeepholeTests.tests }
        { Name = "LIR Layout Tests"; Tests = LIRLayoutTests.tests }
        { Name = "Dead Code Elimination Tests"; Tests = DeadCodeEliminationTests.tests }
        { Name = "ANF Optimize Tests"; Tests = ANFOptimizeTests.tests }
        { Name = "Script Helper Tests"; Tests = ScriptHelperTests.tests }
        { Name = "Stdlib Source Tests"; Tests = StdlibSourceTests.tests }
        { Name = "Test Runner Args Tests"; Tests = TestRunnerArgsTests.tests }
        { Name = "Type Checking Test Runner Tests"; Tests = TypeCheckingTestRunnerTests.tests }
        { Name = "Pass Test Runner Tests"; Tests = PassTestRunnerTests.tests }
        { Name = "Optimization Format Tests"; Tests = OptimizationFormatTests.tests }
        { Name = "Type Checking Format Tests"; Tests = TypeCheckingFormatTests.tests }
        { Name = "Progress Bar Tests"; Tests = ProgressBarTests.tests }
        { Name = "Syntax DSL Tests"; Tests = SyntaxDSLTests.tests }
        { Name = "Encoding DSL Tests"; Tests = EncodingDSLTests.tests }
        { Name = "Graph Color DSL Tests"; Tests = GraphColorDSLTests.tests }
        { Name = "Parallel Move DSL Tests"; Tests = ParallelMoveDSLTests.tests }
        { Name = "IR Format Snapshot DSL Tests"; Tests = IRFormatSnapshotDSLTests.tests }
        { Name = "IR Format Snapshot Fixture Tests"; Tests = TestDSL.IRFormatSnapshotTestRunner.tests irFormatSnapshotTestFiles }
        { Name = "LIR Execution DSL Tests"; Tests = LIRExecutionDSLTests.tests }
        { Name = "LIR Execution Fixture Tests"; Tests = TestDSL.LIRExecutionTestRunner.tests lirExecutionTestFiles }
        { Name = "ARM64 Encoding Tests"; Tests = ARM64EncodingTests.tests }
        { Name = "ARM64 Binary Tests"; Tests = ARM64BinaryTests.tests }
        { Name = "Parser Tests"; Tests = ParserTests.tests }
        { Name = "ARM64 CodeGen Tests"; Tests = ARM64CodeGenTests.tests }
        { Name = "x64 Encoding Fixture Tests"; Tests = TestDSL.X86_64EncodingTestRunner.tests x64encTestFiles }
        { Name = "x64 Binary Tests"; Tests = X86_64BinaryTests.tests }
        { Name = "x64 Resolve Tests"; Tests = X86_64ResolveTests.tests }
        { Name = "x64 CodeGen Tests"; Tests = X86_64CodeGenTests.tests }
        { Name = "Type Checking Tests"; Tests = TypeCheckingTests.tests }
        { Name = "Parallel Move Fixture Tests"; Tests = TestDSL.ParallelMoveTestRunner.tests parallelMoveTestFiles }
        { Name = "Bitset Tests"; Tests = BitsetTests.tests }
        { Name = "SSA Construction Tests"; Tests = SSAConstructionTests.tests }
        { Name = "SSA Liveness Tests"; Tests = SSALivenessTests.tests }
        { Name = "Phi Resolution Tests"; Tests = PhiResolutionTests.tests }
        { Name = "Graph Coloring Fixture Tests"; Tests = TestDSL.GraphColorTestRunner.tests graphColorTestFiles }
        { Name = "Chordal Graph Integration Tests"; Tests = ChordalGraphTests.tests }
        { Name = "AST to ANF Tests"; Tests = ASTToANFTests.tests }
        { Name = "RefCount Insertion Tests"; Tests = RefCountInsertionTests.tests }
        { Name = "TailCall Detection Tests"; Tests = TailCallDetectionTests.tests }
        { Name = "ANF Inlining Tests"; Tests = ANFInliningTests.tests }
        { Name = "ANF Direct-Call Specialization Tests"; Tests = ANFDirectCallSpecializationTests.tests }
        { Name = "ANF Higher-Order Specialization Tests"; Tests = ANFHigherOrderSpecializationTests.tests }
        { Name = "Monomorphization Tests"; Tests = MonomorphizationTests.tests }
        { Name = "Lambda Lifting Tests"; Tests = LambdaLiftingTests.tests }
        { Name = "Syntax Interop Tests"; Tests = SyntaxInteropTests.tests }
        { Name = "Formatting Roundtrip Tests"; Tests = FormattingRoundtripTests.tests formattingRoundtripTestFiles }
        { Name = "Syntax Fixture Tests"; Tests = TestDSL.SyntaxTestRunner.tests syntaxTestFiles }
        { Name = "E2E Format Tests"; Tests = E2EFormatTests.tests }
    |]

    let enableVerification = verificationEnabled

    let symbols : OutputSymbols =
        { Pass = "✓"
          Fail = "✗"
          SectionPrefix = "└─" }

    let runState = TestFramework.createStateWithProgressReporter completedTestReporter
    let recordTiming = TestFramework.recordTiming runState
    let recordResults = TestFramework.recordResults runState
    let recordPassTiming = TestFramework.recordPassTiming runState
    let passTimingTotal () = TestFramework.calculatePassTimingsTotalForOverhead runState.PassTimings
    let mergeRunState (target: TestRunState) (source: TestRunState) : unit =
        let previousCompleted = target.Passed + target.Failed
        target.Passed <- target.Passed + source.Passed
        target.Failed <- target.Failed + source.Failed
        for failedTest in source.FailedTests do
            target.FailedTests.Add failedTest
        for timing in source.Timings do
            target.Timings.Add timing
        for passName in source.PassTimingOrder do
            match Map.tryFind passName source.PassTimings with
            | Some elapsed ->
                if not (Map.containsKey passName target.PassTimings) then
                    target.PassTimingOrder.Add passName
                let existing =
                    Map.tryFind passName target.PassTimings
                    |> Option.defaultValue TimeSpan.Zero
                target.PassTimings <- Map.add passName (existing + elapsed) target.PassTimings
            | None -> ()
        match target.CompletedTestReporter with
        | Some reportCompletedTest ->
            let completedDelta = source.Passed + source.Failed
            for completed in previousCompleted + 1 .. previousCompleted + completedDelta do
                reportCompletedTest completed
        | None -> ()
    let recordNonPassTiming (name: string) (elapsed: TimeSpan) : unit =
        if elapsed > TimeSpan.Zero then
            recordPassTiming { Pass = name; Elapsed = elapsed }
    let recordPhaseOverhead
        (name: string)
        (elapsed: TimeSpan)
        (passTimingStart: TimeSpan)
        (passTimingEnd: TimeSpan)
        : unit =
        let passDelta = passTimingEnd - passTimingStart
        if passDelta < TimeSpan.Zero then
            Crash.crash $"recordPhaseOverhead: pass timing delta ({passDelta}) is negative for {name}"
        let overhead = elapsed - passDelta
        recordNonPassTiming name overhead
    let runSuiteWithExecutionTiming (timingName: string) (runSuite: unit -> unit) : unit =
        let suitePassTimingStart = passTimingTotal ()
        let suiteTimer = Stopwatch.StartNew()
        runSuite ()
        suiteTimer.Stop()
        let suitePassTimingEnd = passTimingTotal ()
        recordPhaseOverhead
            timingName
            suiteTimer.Elapsed
            suitePassTimingStart
            suitePassTimingEnd

    let stdlibPassTimingStart = passTimingTotal ()
    let timer = Stopwatch.StartNew()
    let target =
        match Platform.detectHostTarget () with
        | Ok target -> target
        | Error err -> Crash.crash $"Host target detection failed: {err}"
    let stdlib =
        match CompilerLibrary.buildStdlibWithTrace target (Some recordPassTiming) with
        | Ok stdlib -> stdlib
        | Error err -> Crash.crash $"Stdlib did not build with error: {err}"
    let elapsed = timer.Elapsed
    let stdlibPassTimingEnd = passTimingTotal ()
    recordPhaseOverhead "Stdlib Build Overhead" elapsed stdlibPassTimingStart stdlibPassTimingEnd
    let optionalRoundtripSuites : UnitTestSuite array =
        [|
            {
                Name = "Syntax Roundtrip Corpus Tests"
                Tests = SyntaxRoundtripCorpusTests.tests roundtripCorpusTestFiles
            }
        |]

    let allUnitTests = Array.append (buildUnitTests stdlib) optionalRoundtripSuites

    // Upstream enablement is intentionally incremental: keep tests discoverable, but
    // only run line-allowlisted cases for files currently being enabled.
    let upstreamEnablementLineAllowlist : Map<string, Set<int>> =
        Map.ofList
            [
                ("src/Tests/e2e/upstream/language/apply/eapply.dark", Set.ofList [ 1; 4; 7; 10; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 75; 76; 78; 79; 83; 84; 88; 92; 93; 95; 96; 98; 100; 102; 106; 108; 133; 134; 136; 139 ])
                ("src/Tests/e2e/upstream/language/custom-data/aliases.dark", Set.ofList [ 6; 9; 15; 16; 19; 89; 91; 108 ])
                ("src/Tests/e2e/upstream/language/custom-data/records.dark", Set.ofList [ 6; 8; 15; 23; 47; 49; 58; 65; 68; 72; 76; 80; 84 ])
                ("src/Tests/e2e/upstream/language/custom-data/record-field-acess.dark", Set.singleton 3)
                ("src/Tests/e2e/upstream/language/flow-control/epipe.dark", Set.ofList [ 3; 4; 7; 27; 53; 93; 96; 102 ])
                ("src/Tests/e2e/upstream/language/derror.dark", Set.ofList [ 14; 20 ])
                ("src/Tests/e2e/upstream/stdlib/list.dark", Set.ofList [ 1; 3; 5; 6; 8; 9; 12; 15; 16; 17; 18; 25; 26; 27; 28; 29; 31; 32; 33; 34; 35; 36; 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 54; 93; 99; 100; 111; 112; 114; 115; 116; 118; 119; 120; 121; 122; 123; 124; 127; 132; 133; 144; 145; 146; 147; 149; 151; 152; 155; 158; 163; 166; 176; 182; 185; 188; 190; 195; 196; 203; 204; 205; 208; 213; 215; 218; 229; 230; 232; 233; 235; 236; 239; 240; 241; 242; 243; 244; 245; 246; 247; 248; 249; 252; 253; 254; 256; 257; 259; 260; 261; 265; 270; 274; 277; 287; 289; 290; 292; 293; 294; 295; 296; 297; 298; 303; 308; 309; 310; 311; 312; 314; 315; 316; 317; 321; 324; 339; 342; 351; 354; 357; 360; 363; 364; 365; 367; 368; 369; 370; 371; 372; 373; 378 ])
                ("src/Tests/e2e/upstream/stdlib/uuid.dark", Set.ofList [ 2; 6 ])
                ("src/Tests/e2e/upstream/stdlib/option.dark", Set.ofList [ 251; 281; 288; 291; 294; 297; 319 ])
                ("src/Tests/e2e/upstream/stdlib/result.dark", Set.ofList [ 37; 176; 200; 223; 227; 231; 235; 240; 244; 248; 252; 262 ])
                ("src/Tests/e2e/upstream/stdlib/math.dark", Set.ofList [ 5; 6; 8; 9; 47; 48 ])
                ("src/Tests/e2e/upstream/stdlib/float.dark", Set.ofList [ 60; 105; 116; 118; 120; 122; 128; 137; 193; 195 ])
                ("src/Tests/e2e/upstream/stdlib/ints/int64.dark", Set.ofList [ 185; 194; 204 ])
                ("src/Tests/e2e/upstream/stdlib/cli-color.dark", Set.ofList [ 1 .. 18 ])
            ]

    let normalizePath (path: string) : string =
        path.Replace('\\', '/')

    let enabledLinesForSourceFile (sourceFile: string) : Set<int> option =
        let normalizedSourceFile = normalizePath sourceFile
        upstreamEnablementLineAllowlist
        |> Map.tryPick (fun suffix lines ->
            if normalizedSourceFile.EndsWith(suffix) then Some lines else None)

    let tryParseTestLineNumber (testName: string) : int option =
        if testName.StartsWith("L") then
            let colonIndex = testName.IndexOf(':')
            if colonIndex > 1 then
                let lineText = testName.Substring(1, colonIndex - 1)
                match Int32.TryParse(lineText) with
                | true, value -> Some value
                | false, _ -> None
            else
                None
        else
            None

    let applyUpstreamEnablementGate (test: E2ETest) : E2ETest =
        if Option.isSome test.SkipReason then
            test
        else
            match enabledLinesForSourceFile test.SourceFile with
            | None ->
                test
            | Some enabledLines ->
                match tryParseTestLineNumber test.Name with
                | Some lineNumber when Set.contains lineNumber enabledLines ->
                    test
                | Some _ ->
                    { test with
                        SkipReason = Some "pending upstream enablement" }
                | None ->
                    test

    let loadE2ETests (testFiles: string array) : E2ETest array * (string * string) list =
        let tests = ResizeArray<E2ETest>()
        let mutable parseErrors = []
        for testFile in testFiles do
            match parseE2ETestFile testFile with
            | Ok parsed ->
                parsed
                |> List.map applyUpstreamEnablementGate
                |> tests.AddRange
            | Error msg -> parseErrors <- (testFile, msg) :: parseErrors
        (tests.ToArray(), List.rev parseErrors)

    let matchesE2EFilter (test: E2ETest) : bool =
        matchesFilter filter test.Name
        || matchesFilter filter test.SourceFile

    let reportParseErrors (suiteName: string) (parseErrors: (string * string) list) : unit =
        for (filePath, msg) in parseErrors do
            let fileName = Path.GetFileName filePath
            println $"  {Colors.red}✗ ERROR parsing {fileName}{Colors.reset}"
            println $"    {msg}"
            recordResults 0 1 [{ File = filePath; Name = $"{suiteName}: {fileName}"; Message = msg; Details = [] }]

    let runE2ESuite
        (baseStdlib: CompilerLibrary.StdlibResult)
        (suiteName: string)
        (progressLabel: string)
        (testsArray: E2ETest array)
        : unit =
        let stdlib = baseStdlib
        let numTests = testsArray.Length
        if numTests > 0 then
            use compilationSession = new CompilerLibrary.CompilationSession()
            let suitePassTimingStart = passTimingTotal ()
            let suiteContextTimer = Stopwatch.StartNew()
            let suiteContextsResult =
                TestDSL.E2ETestRunner.buildSuiteContexts
                    stdlib
                    testsArray
                    (Some recordPassTiming)
            suiteContextTimer.Stop()
            let suitePassTimingEnd = passTimingTotal ()
            recordPhaseOverhead
                $"{suiteName} Suite Context Overhead"
                suiteContextTimer.Elapsed
                suitePassTimingStart
                suitePassTimingEnd
            let mutable suiteContextsOpt =
                match suiteContextsResult with
                | Ok suiteContexts -> Some suiteContexts
                | Error _ -> None

            let results = Array.zeroCreate<option<E2ETest * E2ETestResult>> numTests
            let progress = ProgressBar.create progressLabel numTests
            ProgressBar.update progress

            let unpackRun (run: E2ERun) : int * string * string * TimeSpan * TimeSpan =
                match run with
                | CompileFailed (exitCode, error, compileTime) ->
                    (exitCode, "", error, compileTime, TimeSpan.Zero)
                | Ran (exitCode, stdout, stderr, compileTime, runtimeTime) ->
                    (exitCode, stdout, stderr, compileTime, runtimeTime)

            let runFromTestResult (result: E2ETestResult) : E2ERun =
                match result with
                | Ok run -> run
                | Error failure -> failure.Run

            let preambleFailureResult (message: string) : E2ETestResult =
                Error { Run = CompileFailed (1, message, TimeSpan.Zero); Message = message }

            let collectExitCodeDetails (test: E2ETest) (run: E2ERun) : string list =
                let (exitCode, _, _, _, _) = unpackRun run
                if exitCode <> test.ExpectedExitCode then
                    [ $"Expected exit code: {test.ExpectedExitCode}, Actual: {exitCode}" ]
                else
                    []

            let printE2EFailure (test: E2ETest) (failure: E2EFailure) : string list =
                let run = failure.Run
                let (_, stdout, stderr, compileTime, runtimeTime) = unpackRun run
                let cleanName = test.Name.Replace("Stdlib.", "")
                let displayName = if cleanName.Length > 60 then cleanName.Substring(0, 57) + "..." else cleanName
                println $"  {displayName}... {Colors.red}✗ FAIL{Colors.reset} {Colors.gray}(compile: {formatTime compileTime}, run: {formatTime runtimeTime}){Colors.reset}"
                println $"    {failure.Message}"

                let details = collectExitCodeDetails test run
                for detail in details do
                    println $"    {detail}"

                match test.ExpectedStdout with
                | Some expected when stdout.Trim() <> expected.Trim() ->
                    let expectedDisp = expected.Replace("\n", "\\n")
                    let actualDisp = stdout.Replace("\n", "\\n")
                    println $"    Expected stdout: {expectedDisp}"
                    println $"    Actual stdout: {actualDisp}"
                | _ -> ()
                match test.ExpectedStderr with
                | Some expected when stderr.Trim() <> expected.Trim() ->
                    let expectedDisp = expected.Replace("\n", "\\n")
                    let actualDisp = stderr.Replace("\n", "\\n")
                    println $"    Expected stderr: {expectedDisp}"
                    println $"    Actual stderr: {actualDisp}"
                | _ -> ()
                details

            for i in 0 .. numTests - 1 do
                let test = testsArray.[i]
                let result, passTimingDelta =
                    match suiteContextsResult with
                    | Error err ->
                        preambleFailureResult $"Preamble build failed: {err}", TimeSpan.Zero
                    | Ok _ ->
                        match suiteContextsOpt with
                        | None ->
                            preambleFailureResult "Missing built suite contexts", TimeSpan.Zero
                        | Some currentSuiteContexts ->
                            let contextKey = TestDSL.E2ETestRunner.preambleContextKeyForTest test
                            match Map.tryFind contextKey currentSuiteContexts.PreambleContexts with
                            | None ->
                                preambleFailureResult $"Missing built preamble context for {test.SourceFile}", TimeSpan.Zero
                            | Some ctx ->
                                let passTimingStart = passTimingTotal ()
                                let testResult =
                                    TestDSL.E2ETestRunner.runE2ETestWithPreambleContext
                                        currentSuiteContexts.Stdlib
                                        ctx
                                        (Some compilationSession)
                                        test
                                        (Some recordPassTiming)
                                let passTimingEnd = passTimingTotal ()
                                let passTimingDelta = passTimingEnd - passTimingStart
                                testResult, passTimingDelta

                let run = runFromTestResult result
                let (_, _, _, compileTime, runtimeTime) = unpackRun run
                let compileOverhead = compileTime - passTimingDelta
                recordNonPassTiming "Compile Overhead" compileOverhead
                recordNonPassTiming TestFramework.testRuntimeTimingName runtimeTime
                let totalTime = compileTime + runtimeTime
                results.[i] <- Some (test, result)
                recordTiming {
                    Name = $"{suiteName}: {test.Name}"
                    TotalTime = totalTime
                    CompileTime = Some compileTime
                    RuntimeTime = Some runtimeTime
                }
                let success =
                    match result with
                    | Ok _ -> true
                    | Error _ -> false
                ProgressBar.increment progress success
                match result with
                | Error failure when verbose ->
                    ProgressBar.finish progress
                    let _ = printE2EFailure test failure
                    ProgressBar.update progress
                | _ -> ()

            ProgressBar.finish progress

            let mutable sectionPassed = 0
            let mutable sectionFailed = 0
            let failedTestsLocal = ResizeArray<FailedTestInfo>()
            for result in results do
                match result with
                | Some (test, testResult) ->
                    match testResult with
                    | Ok _ ->
                        sectionPassed <- sectionPassed + 1
                    | Error failure ->
                        let details =
                            if verbose then collectExitCodeDetails test failure.Run
                            else printE2EFailure test failure
                        failedTestsLocal.Add({ File = test.SourceFile; Name = $"{suiteName}: {test.Name}"; Message = failure.Message; Details = details })
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
    let anf2mirTests = anf2mirTestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runANF2MIRFile =
        runPassTestFile loadANF2MIRTest (fun (input, expected) -> runANF2MIRTest input expected)
    runSuiteWithExecutionTiming
        "Pass Test Suite Execution"
        (fun () ->
            runFileSuite
                runState
                symbols
                "📦 ANF→MIR Tests"
                "ANF→MIR"
                anf2mirTests
                Path.GetFileName
                (fun testName -> $"ANF→MIR: {testName}")
                runANF2MIRFile
                (handlePassTestSuccess "ANF→MIR")
                (handlePassTestError "ANF→MIR"))

    // Run MIR→LIR tests
    let mir2lirTests = mir2lirTestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runMIR2LIRFile =
        runPassTestFile loadMIR2LIRTest (fun (input, expected) -> runMIR2LIRTest input expected)
    runSuiteWithExecutionTiming
        "Pass Test Suite Execution"
        (fun () ->
            runFileSuite
                runState
                symbols
                "🔄 MIR→LIR Tests"
                "MIR→LIR"
                mir2lirTests
                Path.GetFileName
                (fun testName -> $"MIR→LIR: {testName}")
                runMIR2LIRFile
                (handlePassTestSuccess "MIR→LIR")
                (handlePassTestError "MIR→LIR"))

    // Run LIR→ARM64 tests
    let lir2arm64Tests = lir2arm64TestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runLIR2ARM64File =
        runPassTestFile loadLIR2ARM64Test (fun (input, expected) -> runLIR2ARM64Test input expected)
    runSuiteWithExecutionTiming
        "Pass Test Suite Execution"
        (fun () ->
            runFileSuite
                runState
                symbols
                "🎯 LIR→ARM64 Tests"
                "LIR→ARM64"
                lir2arm64Tests
                Path.GetFileName
                (fun testName -> $"LIR→ARM64: {testName}")
                runLIR2ARM64File
                (handlePassTestSuccess "LIR→ARM64")
                (handlePassTestError "LIR→ARM64"))

    // Run ARM64 encoding tests
    let arm64encTests = arm64encTestFiles |> Array.filter (fun p -> matchesFilter filter (Path.GetFileName p))
    let runARM64EncodingFile =
        runPassTestFile
            TestDSL.ARM64EncodingTestRunner.loadARM64EncodingTest
            TestDSL.ARM64EncodingTestRunner.runARM64EncodingTest
    runSuiteWithExecutionTiming
        "Pass Test Suite Execution"
        (fun () ->
            runFileSuite
                runState
                symbols
                "⚙️  ARM64 Encoding Tests"
                "ARM64 Enc"
                arm64encTests
                Path.GetFileName
                (fun testName -> $"ARM64 Encoding: {testName}")
                runARM64EncodingFile
                (handlePassTestSuccess "ARM64 Encoding")
                (handlePassTestError "ARM64 Encoding"))

    // Run Type Checking tests
    let typecheckTests =
        typecheckTestFiles
        |> Array.filter (fun p -> matchesFilter filter (Path.GetFileNameWithoutExtension p))
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
    runSuiteWithExecutionTiming
        "Pass Test Suite Execution"
        (fun () ->
            runFileSuite
                runState
                symbols
                "📋 Type Checking Tests"
                "TypeCheck"
                typecheckTests
                (fun testPath -> Path.GetFileNameWithoutExtension (testPath: string))
                (fun fileName -> $"TypeCheck: {fileName}")
                runTypecheckFile
                handleTypecheckSuccess
                handleTypecheckError)

    // Run Optimization Tests (ANF, MIR, LIR)
    if optTestFiles.Length > 0 then
        let runOptimizationFile testFile =
            let fileName = Path.GetFileNameWithoutExtension (testFile: string)
            let loweredFileName = fileName.ToLowerInvariant()
            let stage =
                if loweredFileName.Contains("anf") then TestDSL.OptimizationFormat.ANF
                elif loweredFileName.Contains("mir") then TestDSL.OptimizationFormat.MIR
                elif loweredFileName.Contains("lir") then TestDSL.OptimizationFormat.LIR
                else TestDSL.OptimizationFormat.ANF
            TestDSL.OptimizationTestRunner.runTestFile stdlib stage testFile
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
                ProgressBar.increment progress false
                ProgressBar.finish progress
                let failures = ResizeArray<FailedTestInfo>()
                for (test, result) in filteredResults do
                    if not result.Success then
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
        runSuiteWithExecutionTiming
            "Pass Test Suite Execution"
            (fun () ->
                runFileSuite
                    runState
                    symbols
                    "⚡ Optimization Tests"
                    "Optimization"
                    optTestFiles
                    (fun testPath -> Path.GetFileNameWithoutExtension (testPath: string))
                    (fun fileName -> $"Optimization: {fileName}")
                    runOptimizationFile
                    handleOptimizationSuccess
                    handleOptimizationError)

    // Order unit test suites so non-stdlib tests run first.
    let splitUnitTestsByStdlibNeed
        (needsStdlib: string list)
        (suites: UnitTestSuite array)
        : UnitTestSuite array * UnitTestSuite array =
        let needsStdlibSet = Set.ofList needsStdlib
        let needsStdlibSuites, noStdlibSuites =
            suites
            |> Array.partition (fun suite -> Set.contains suite.Name needsStdlibSet)
        (noStdlibSuites, needsStdlibSuites)

    let unitTests =
        allUnitTests
        |> Array.choose (fun suite ->
            if matchesFilter filter suite.Name then
                Some suite
            else
                let filteredTests =
                    suite.Tests
                    |> List.filter (fun (testName, _) -> matchesFilter filter testName)
                if List.isEmpty filteredTests then None else Some { suite with Tests = filteredTests })

    let (unitTestsNoStdlib, unitTestsWithStdlib) =
        splitUnitTestsByStdlibNeed unitStdlibSuites unitTests
    let unitTestsOrdered = Array.append unitTestsNoStdlib unitTestsWithStdlib

    let runE2EAndVerification
        (baseStdlib: CompilerLibrary.StdlibResult)
        : unit =
        if e2eTestFiles.Length > 0 then
                let sectionTimer = Stopwatch.StartNew()
                println $"{Colors.cyan}🚀 E2E Tests{Colors.reset}"

                let parseTimer = Stopwatch.StartNew()
                let (allE2ETests, parseErrors) = loadE2ETests e2eTestFiles
                parseTimer.Stop()
                recordNonPassTiming "E2E Test Parse" parseTimer.Elapsed
                reportParseErrors "E2E" parseErrors

                if allE2ETests.Length > 0 then
                    let filteredTests =
                        allE2ETests
                        |> Array.filter matchesE2EFilter
                    let skippedCount =
                        filteredTests
                        |> Array.filter (fun test -> Option.isSome test.SkipReason)
                        |> Array.length
                    let testsArray =
                        filteredTests
                        |> Array.filter (fun test -> Option.isNone test.SkipReason)
                    if skippedCount > 0 then
                        println $"  {Colors.gray}Skipping {skippedCount} test(s) marked with skip=\"...\"{Colors.reset}"
                    if testsArray.Length > 0 then
                        let timingText = $"(Stdlib compiled in {formatTime elapsed})"
                        println $"  {Colors.gray}{timingText}{Colors.reset}"
                        runE2ESuite baseStdlib "E2E" "E2E" testsArray

                sectionTimer.Stop()
                println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
                println ""

        if enableVerification && verificationTestFiles.Length > 0 then
                let sectionTimer = Stopwatch.StartNew()
                println $"{Colors.cyan}🔬 Verification Tests{Colors.reset}"

                let parseTimer = Stopwatch.StartNew()
                let (allVerifTests, parseErrors) = loadE2ETests verificationTestFiles
                parseTimer.Stop()
                recordNonPassTiming "Verification Test Parse" parseTimer.Elapsed
                reportParseErrors "Verification" parseErrors

                if allVerifTests.Length > 0 then
                    let filteredTests =
                        allVerifTests
                        |> Array.filter matchesE2EFilter
                    let skippedCount =
                        filteredTests
                        |> Array.filter (fun test -> Option.isSome test.SkipReason)
                        |> Array.length
                    let testsArray =
                        filteredTests
                        |> Array.filter (fun test -> Option.isNone test.SkipReason)
                    if skippedCount > 0 then
                        println $"  {Colors.gray}Skipping {skippedCount} test(s) marked with skip=\"...\"{Colors.reset}"
                    if testsArray.Length > 0 then
                        runE2ESuite baseStdlib "Verification" "Verification" testsArray

                sectionTimer.Stop()
                println $"  {Colors.gray}└─ Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
                println ""

    let runUnitSuitesInParallel () : Task<TestRunState> =
        Task.Run(fun () ->
            let unitState = TestFramework.createState ()
            runUnitTestSuites unitState symbols "🔧 Unit Tests" "Unit" unitTestsOrdered
            unitState)

    println $"{Colors.gray}  Unit and E2E suites: running in parallel{Colors.reset}"
    let unitTask = runUnitSuitesInParallel ()
    runE2EAndVerification stdlib
    let unitState = unitTask.GetAwaiter().GetResult()
    mergeRunState runState unitState

    // Compute stdlib coverage only if --coverage flag is set
    let coveragePercent =
        if not showCoverage then None
        else
            let allStdlibFuncs = CompilerLibrary.getAllStdlibFunctionNamesFromStdlib stdlib
            let coveredFuncs = System.Collections.Generic.HashSet<string>()
            if e2eTestFiles.Length > 0 then
                for testFile in e2eTestFiles do
                    match TestDSL.E2EFormat.parseE2ETestFile testFile with
                    | Error _ -> ()
                    | Ok tests ->
                        for test in tests do
                            if Option.isNone test.SkipReason then
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
    let unaccountedBreakdown =
        TestFramework.calculateUnaccountedTimeBreakdown
            totalTimer.Elapsed
            runState.PassTimings
            runState.Timings

    let orderedPassTimingNames : string list =
        let knownOrder = runState.PassTimingOrder |> Seq.toList
        let extras =
            runState.PassTimings
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.filter (fun name -> not (List.contains name knownOrder))
            |> Seq.sort
            |> Seq.toList
        knownOrder @ extras

    let writeTimingsJson (path: string) : unit =
        let testEntries : TimingJsonTest array =
            runState.Timings
            |> Seq.sortByDescending (fun timing -> timing.TotalTime)
            |> Seq.map (fun timing ->
                { name = timing.Name
                  total_ms = milliseconds timing.TotalTime
                  compile_ms = optionalMilliseconds timing.CompileTime
                  runtime_ms = optionalMilliseconds timing.RuntimeTime })
            |> Seq.toArray
        let passEntries : TimingJsonPass array =
            orderedPassTimingNames
            |> List.choose (fun name ->
                Map.tryFind name runState.PassTimings
                |> Option.map (fun elapsed ->
                    { name = name
                      elapsed_ms = milliseconds elapsed }))
            |> List.toArray
        let payload =
            { summary =
                { passed = runState.Passed
                  failed = runState.Failed
                  total = runState.Passed + runState.Failed
                  total_ms = milliseconds totalTimer.Elapsed
                  unaccounted_ms = milliseconds unaccountedBreakdown.Unaccounted
                  runtime_unaccounted_ms = milliseconds unaccountedBreakdown.Runtime
                  overhead_unaccounted_ms = milliseconds unaccountedBreakdown.Overhead }
              tests = testEntries
              passes = passEntries }
        let options =
            JsonSerializerOptions(
                DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
            )
        let directory = Path.GetDirectoryName(path)
        if not (String.IsNullOrWhiteSpace(directory)) then
            Directory.CreateDirectory(directory) |> ignore
        File.WriteAllText(path, JsonSerializer.Serialize(payload, options))

    match timingsJsonPath with
    | Some path ->
        writeTimingsJson path
        println $"  {Colors.gray}⏱  Wrote timing JSON: {path}{Colors.reset}"
    | None -> ()

    // Print slowest tests
    if runState.Timings.Count > 0 then
        let title =
            if showAllTestTimings then
                "⏱ All Test Timings"
            else
                "🐢 Slowest Tests"
        let timingsToDisplay =
            runState.Timings
            |> Seq.sortByDescending (fun t -> t.TotalTime)
            |> (if showAllTestTimings then Seq.toList else Seq.truncate 5 >> Seq.toList)
        println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
        println $"{Colors.bold}{Colors.gray}{title}{Colors.reset}"
        println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
        for (i, timing) in timingsToDisplay |> List.indexed do
            let baseTimingStr =
                match timing.CompileTime, timing.RuntimeTime with
                | Some ct, Some rt ->
                    $"compile: {formatTime ct}  run: {formatTime rt}  total: {formatTime timing.TotalTime}"
                | _ ->
                    $"total: {formatTime timing.TotalTime}"
            let timingStr = baseTimingStr
            let displayName = if timing.Name.Length > 45 then timing.Name.Substring(0, 42) + "..." else timing.Name
            println $"  {Colors.gray}{i + 1}. {displayName,-45} {timingStr}{Colors.reset}"
        println ""

    println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
    println $"{Colors.bold}{Colors.gray}⏱ Suite Timings{Colors.reset}"
    println $"{Colors.bold}{Colors.gray}═══════════════════════════════════════{Colors.reset}"
    let columns =
        TestFramework.buildPassTimingColumns
            runState.PassTimings
            (runState.PassTimingOrder |> Seq.toList)
            unaccountedBreakdown.Unaccounted
    let formatColumn (sections: TestFramework.PassTimingSection list) : string list =
        let entries = sections |> List.collect (fun section -> section.Entries)
        let formatSeconds (elapsed: TimeSpan) : string =
            if elapsed.TotalMilliseconds < 100.0 then
                ">0.1s"
            else
                let seconds = elapsed.TotalSeconds.ToString("0.0")
                $"{seconds}s"
        let numberText (entry: TestFramework.PassTimingEntry) : string =
            match entry.Number with
            | Some number -> number
            | None -> ""
        let numberWidth =
            entries
            |> List.map (fun entry -> (numberText entry).Length)
            |> List.fold max 0
        let numberPadWidth = if numberWidth > 0 then numberWidth + 2 else 0
        let labelFor (entry: TestFramework.PassTimingEntry) : string =
            let numberPadded = (numberText entry).PadRight numberPadWidth
            if numberPadWidth > 0 then $"{numberPadded}{entry.Name}" else entry.Name
        let labelWidth =
            entries
            |> List.map (fun entry -> (labelFor entry).Length)
            |> List.fold max 0
        let timeWidth =
            entries
            |> List.map (fun entry -> (formatSeconds entry.Elapsed).Length)
            |> List.fold max 0
        let formatEntry (entry: TestFramework.PassTimingEntry) : string =
            let label = labelFor entry
            let timeText = formatSeconds entry.Elapsed
            let paddedTime = timeText.PadLeft timeWidth
            let timeWithColor =
                if entry.Elapsed.TotalSeconds > 3.0 then
                    $"{Colors.red}{paddedTime}{Colors.gray}"
                elif entry.Elapsed.TotalSeconds > 2.0 then
                    $"{Colors.yellow}{paddedTime}{Colors.gray}"
                elif entry.Elapsed.TotalSeconds > 1.0 then
                    $"{Colors.white}{paddedTime}{Colors.gray}"
                else
                    paddedTime
            $"  {label.PadRight labelWidth}  {timeWithColor}"
        let sectionCount = List.length sections
        sections
        |> List.mapi (fun idx section ->
            let entryLines =
                if List.isEmpty section.Entries then
                    [ "  (none)" ]
                else
                    section.Entries |> List.map formatEntry
            let lines = section.Title :: entryLines
            if idx < sectionCount - 1 then lines @ [ "" ] else lines)
        |> List.collect id
    let leftLines = formatColumn columns.Ordered
    let rightLines = formatColumn columns.ByTime
    let visibleLength (text: string) : int =
        [ Colors.red; Colors.yellow; Colors.white; Colors.gray; Colors.bold; Colors.reset ]
        |> List.fold (fun (acc: string) (code: string) -> acc.Replace(code, "")) text
        |> fun stripped -> stripped.Length
    let padRightVisible (text: string) (width: int) : string =
        let visible = visibleLength text
        if visible >= width then text else text + String(' ', width - visible)
    let leftWidth =
        leftLines
        |> List.map visibleLength
        |> List.fold max 0
    let gap = "  "
    let lineCount = max leftLines.Length rightLines.Length
    for idx in 0 .. lineCount - 1 do
        let left = if idx < leftLines.Length then leftLines.[idx] else ""
        let right = if idx < rightLines.Length then rightLines.[idx] else ""
        let paddedLeft = padRightVisible left leftWidth
        println $"  {Colors.gray}{paddedLeft}{gap}{right}{Colors.reset}"
    println ""

    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}📊 Test Results{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"
    if runState.Failed = 0 then
        println $"  {Colors.green}✓ All tests passed: {runState.Passed}/{runState.Passed + runState.Failed}{Colors.reset}"
    else
        println $"  {Colors.green}✓ Passed: {runState.Passed}{Colors.reset}"
        println $"  {Colors.red}✗ Failed: {runState.Failed}{Colors.reset}"
    match coveragePercent with
    | Some pct -> println $"  {Colors.gray}📊 Stdlib coverage: {pct:F1}%%{Colors.reset}"
    | None -> ()
    println
        $"  {Colors.gray}⏱  Unaccounted time: {formatTime unaccountedBreakdown.Unaccounted} (runtime: {formatTime unaccountedBreakdown.Runtime}, overhead: {formatTime unaccountedBreakdown.Overhead}){Colors.reset}"
    println $"  {Colors.gray}⏱  Total time: {formatTime totalTimer.Elapsed}{Colors.reset}"
    println $"{Colors.bold}{Colors.cyan}═══════════════════════════════════════{Colors.reset}"

    // Print first 10 failing tests summary
    if runState.FailedTests.Count > 0 then
        println ""
        println $"{Colors.bold}{Colors.red}═══════════════════════════════════════{Colors.reset}"
        let displayCount = min 10 runState.FailedTests.Count
        let moreCount = runState.FailedTests.Count - displayCount
        if moreCount > 0 then
            println $"{Colors.bold}{Colors.red}❌ First {displayCount} Failing Tests (of {runState.FailedTests.Count} total){Colors.reset}"
        else
            println $"{Colors.bold}{Colors.red}❌ Failing Tests ({runState.FailedTests.Count}){Colors.reset}"
        println $"{Colors.bold}{Colors.red}═══════════════════════════════════════{Colors.reset}"
        println ""

        for i in 0 .. displayCount - 1 do
            let test = runState.FailedTests.[i]
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

    let exitCode = if runState.Failed = 0 then 0 else 1
    { ExitCode = exitCode
      State = runState
      TotalTime = totalTimer.Elapsed
      UnaccountedBreakdown = unaccountedBreakdown }

let private runTests args =
    runTestsWithProgressReporter None args

let private captureOutput (run: unit -> TestRunResult) : TestRunResult =
    let originalOut = Console.Out
    let originalError = Console.Error
    use buffer = new StringWriter(CultureInfo.InvariantCulture)
    Console.SetOut(buffer)
    Console.SetError(buffer)
    try
        let result = run ()
        Console.Out.Flush()
        Console.Error.Flush()
        result
    finally
        Console.SetOut(originalOut)
        Console.SetError(originalError)

let private formatFailureDisplayName (test: FailedTestInfo) : string =
    let fileName = if test.File <> "" then Path.GetFileName test.File else ""
    if fileName <> "" && test.Name.StartsWith("E2E: L") then
        let afterPrefix = test.Name.Substring(5)
        $"E2E: {fileName}:{afterPrefix}"
    elif fileName <> "" then
        $"{fileName}: {test.Name}"
    else
        test.Name

let private printStructuredFailures (state: TestRunState) (limit: int) : unit =
    let displayCount = min limit state.FailedTests.Count
    for i in 0 .. displayCount - 1 do
        let test = state.FailedTests.[i]
        println $"{i + 1}. {formatFailureDisplayName test}"
        println $"   {test.Message}"
        for detail in test.Details do
            println $"   {detail}"
    let moreCount = state.FailedTests.Count - displayCount
    if moreCount > 0 then
        println $"... and {moreCount} more failing test(s)"

let private printQuietResult (result: TestRunResult) : int =
    if result.ExitCode = 0 then
        println "success"
        0
    else
        println "failed tests:"
        if result.State.FailedTests.Count = 0 then
            println $"(test runner exited with code {result.ExitCode})"
        else
            printStructuredFailures result.State 20
        1

let private runAiMode (args: string array) : int =
    let originalOut = Console.Out
    let originalError = Console.Error
    let reportCompletedTest completed =
        if completed % aiProgressTestInterval = 0 then
            originalOut.Write(".")
            originalOut.Flush()

    Console.SetOut(TextWriter.Null)
    Console.SetError(TextWriter.Null)
    originalOut.WriteLine("running tests (AI mode)")
    let task =
        Task.Run(fun () ->
            runTestsWithProgressReporter (Some reportCompletedTest) args)

    let result =
        try
            task.GetAwaiter().GetResult()
        finally
            Console.SetOut(originalOut)
            Console.SetError(originalError)

    let total = result.State.Passed + result.State.Failed
    if total >= aiProgressTestInterval then
        originalOut.WriteLine()

    let totalSecondsText =
        result.TotalTime.TotalSeconds.ToString("0.0", CultureInfo.InvariantCulture)
    if result.ExitCode = 0 then
        originalOut.WriteLine($"success: {result.State.Passed}/{total} passed in {totalSecondsText}s")
    else
        originalOut.WriteLine("failed")
        originalOut.WriteLine($"summary: {result.State.Passed} passed, {result.State.Failed} failed, {totalSecondsText}s")
        printStructuredFailures result.State 20

    result.ExitCode

[<EntryPoint>]
let main args =
    if hasHelpArg args then
        printHelp ()
        0
    elif hasQuietArg args then
        captureOutput (fun () -> runTests args)
        |> printQuietResult
    elif hasAiArg args then
        runAiMode args
    else
        let result = runTests args
        result.ExitCode
