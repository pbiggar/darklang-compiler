// TestFramework.fs - Generic test runner helpers.
//
// Provides shared types and utilities that are independent of any specific test suite.

module TestFramework

open System
open System.Diagnostics
open Output
type UnitTestCase = string * (unit -> Result<unit, string>)

type UnitTestSuite = {
    Name: string
    Tests: UnitTestCase list
}

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

type PassTimingEntry = {
    Number: string option
    Name: string
    Elapsed: TimeSpan
}

type PassTimingSection = {
    Title: string
    Entries: PassTimingEntry list
}

type PassTimingColumns = {
    Ordered: PassTimingSection list
    ByTime: PassTimingSection list
}

type UnaccountedTimeBreakdown = {
    Unaccounted: TimeSpan
    Runtime: TimeSpan
    Overhead: TimeSpan
}

let testRuntimeTimingName = "Test Runtime"

// Summary of per-file test suite results
type FileSuiteSummary = {
    Passed: int
    Failed: int
    FailedTests: FailedTestInfo list
}

type TestRunState = {
    mutable Passed: int
    mutable Failed: int
    FailedTests: ResizeArray<FailedTestInfo>
    Timings: ResizeArray<TestTiming>
    mutable PassTimings: Map<string, TimeSpan>
    PassTimingOrder: ResizeArray<string>
    CompletedTestReporter: (int -> unit) option
}

type OutputSymbols = {
    Pass: string
    Fail: string
    SectionPrefix: string
}

let createStateWithProgressReporter (completedTestReporter: (int -> unit) option) : TestRunState =
    { Passed = 0
      Failed = 0
      FailedTests = ResizeArray()
      Timings = ResizeArray()
      PassTimings = Map.empty
      PassTimingOrder = ResizeArray()
      CompletedTestReporter = completedTestReporter }

let createState () : TestRunState =
    createStateWithProgressReporter None

let recordTiming (state: TestRunState) (timing: TestTiming) : unit =
    state.Timings.Add timing

let recordPassTiming (state: TestRunState) (timing: CompilerLibrary.PassTiming) : unit =
    if not (Map.containsKey timing.Pass state.PassTimings) then
        state.PassTimingOrder.Add timing.Pass
    let existing =
        Map.tryFind timing.Pass state.PassTimings
        |> Option.defaultValue TimeSpan.Zero
    let updated = existing + timing.Elapsed
    state.PassTimings <- Map.add timing.Pass updated state.PassTimings

let recordResults
    (state: TestRunState)
    (passedDelta: int)
    (failedDelta: int)
    (failedTestsDelta: FailedTestInfo list)
    : unit =
    let previousCompleted = state.Passed + state.Failed
    state.Passed <- state.Passed + passedDelta
    state.Failed <- state.Failed + failedDelta
    for test in failedTestsDelta do
        state.FailedTests.Add test
    match state.CompletedTestReporter with
    | Some reportCompletedTest ->
        let completedDelta = passedDelta + failedDelta
        if completedDelta < 0 then
            Crash.crash $"recordResults: completed test delta cannot be negative ({completedDelta})"
        else
            for completed in previousCompleted + 1 .. previousCompleted + completedDelta do
                reportCompletedTest completed
    | None -> ()

// Format elapsed time
let formatTime (elapsed: TimeSpan) =
    if elapsed.TotalMilliseconds < 1000.0 then
        let ms = elapsed.TotalMilliseconds.ToString("0")
        $"{ms}ms"
    else
        let seconds = elapsed.TotalSeconds.ToString("0.00")
        $"{seconds}s"

let calculatePassTimingsTotal (passTimings: Map<string, TimeSpan>) : TimeSpan =
    passTimings
    |> Map.fold (fun acc _ elapsed -> acc + elapsed) TimeSpan.Zero

let filterPassTimingsForOverhead (passTimings: Map<string, TimeSpan>) : Map<string, TimeSpan> =
    let overlapTimingNames =
        Set.ofList [
            "Start Function Compilation"
        ]
    passTimings
    |> Map.filter (fun name _ -> not (Set.contains name overlapTimingNames))

let calculatePassTimingsTotalForOverhead (passTimings: Map<string, TimeSpan>) : TimeSpan =
    passTimings
    |> filterPassTimingsForOverhead
    |> Map.fold (fun acc _ elapsed -> acc + elapsed) TimeSpan.Zero

let calculateUnaccountedTimeBreakdown
    (totalTime: TimeSpan)
    (passTimings: Map<string, TimeSpan>)
    (timings: seq<TestTiming>)
    : UnaccountedTimeBreakdown =
    let passTimingTotal = calculatePassTimingsTotalForOverhead passTimings
    let unaccounted = totalTime - passTimingTotal
    let runtimeTotal =
        timings
        |> Seq.choose (fun timing -> timing.RuntimeTime)
        |> Seq.fold (fun acc runtime -> acc + runtime) TimeSpan.Zero
    let runtimeAccounted =
        Map.tryFind testRuntimeTimingName passTimings
        |> Option.defaultValue TimeSpan.Zero
    let runtimeUnaccounted = runtimeTotal - runtimeAccounted
    let overhead = unaccounted - runtimeUnaccounted
    { Unaccounted = unaccounted; Runtime = runtimeUnaccounted; Overhead = overhead }

let buildPassTimingColumns
    (passTimings: Map<string, TimeSpan>)
    (_passTimingOrder: string list)
    (unaccountedTime: TimeSpan)
    : PassTimingColumns =
    let hiddenTimingNames =
        Set.ofList [
            "Unit Test Suite Execution"
        ]
    let consolidated =
        passTimings
        |> Map.filter (fun name _ -> not (Set.contains name hiddenTimingNames))

    let passDefinitions : (string * string * string) list =
        [
            ("Parse", "1", "Parser")
            ("Type Checking", "1.5", "Type Checking")
            ("AST -> ANF", "2", "AST to ANF")
            ("ANF Optimizations", "2.3", "ANF Optimizations")
            ("ANF Inlining", "2.4", "ANF Inlining")
            ("Reference Count Insertion", "2.5", "Ref Count Insertion")
            ("Print Insertion", "2.6", "Print Insertion")
            ("Tail Call Detection", "2.7", "Tail Call Optimization")
            ("ANF -> MIR", "3", "ANF to MIR")
            ("SSA Construction", "3.1", "SSA Construction")
            ("MIR Optimizations", "3.5", "MIR Optimizations")
            ("MIR -> LIR", "4", "MIR to LIR")
            ("LIR Peephole", "4.5", "LIR Peephole")
            ("Register Allocation", "5", "Register Allocation")
            ("Function Tree Shaking", "5.5", "Function Tree Shaking")
            ("Code Generation", "6", "Code Generation")
            ("ARM64 Emit", "7", "ARM64 Emit")
        ]

    let orderedDefinitions : (string * string option * string) list =
        (passDefinitions
        |> List.map (fun (timingKey, number, name) ->
            (timingKey, Some number, name)))
        @ [
            (testRuntimeTimingName, None, testRuntimeTimingName)
        ]

    let overheadDefinitions : (string * string) list =
        [
            ("Pass Test Suite Execution", "Pass Test Suite Execution")
            ("Stdlib Build Overhead", "Stdlib Build Overhead")
            ("E2E Test Parse", "E2E Test Parse")
            ("E2E Suite Context Overhead", "E2E Suite Context Overhead")
            ("Verification Test Parse", "Verification Test Parse")
            ("Verification Suite Context Overhead", "Verification Suite Context Overhead")
            ("Compile Overhead", "Compile Overhead")
        ]

    let minDisplayDuration = TimeSpan.FromMilliseconds(50.0)

    let shouldDisplay (elapsed: TimeSpan) : bool =
        elapsed >= minDisplayDuration

    let orderedEntries =
        orderedDefinitions
        |> List.choose (fun (timingKey, number, name) ->
            match Map.tryFind timingKey consolidated with
            | Some elapsed when shouldDisplay elapsed ->
                Some { Number = number; Name = name; Elapsed = elapsed }
            | _ -> None)

    let overheadEntries =
        overheadDefinitions
        |> List.choose (fun (timingKey, name) ->
            match Map.tryFind timingKey consolidated with
            | Some elapsed when shouldDisplay elapsed ->
                Some { Number = None; Name = name; Elapsed = elapsed }
            | _ -> None)

    let knownTotal =
        consolidated
        |> Map.fold (fun acc _ elapsed -> acc + elapsed) TimeSpan.Zero
    let displayedTotal =
        orderedEntries @ overheadEntries
        |> List.fold (fun acc entry -> acc + entry.Elapsed) TimeSpan.Zero
    let otherKnown = knownTotal - displayedTotal
    if otherKnown < TimeSpan.Zero then
        Crash.crash $"buildPassTimingColumns: known timings ({knownTotal}) smaller than displayed ({displayedTotal})"

    let otherKnownEntries =
        if shouldDisplay otherKnown then
            [ { Number = None; Name = "Other (known)"; Elapsed = otherKnown } ]
        else
            []

    let otherUnknownEntries =
        if shouldDisplay unaccountedTime then
            [ { Number = None; Name = "Other (unknown)"; Elapsed = unaccountedTime } ]
        else
            []

    let overheadEntriesWithOther = overheadEntries @ otherKnownEntries @ otherUnknownEntries
    let orderedEntriesByTime =
        orderedEntries |> List.sortByDescending (fun entry -> entry.Elapsed)
    let overheadSection =
        if List.isEmpty overheadEntriesWithOther then
            None
        else
            Some { Title = "Overhead"; Entries = overheadEntriesWithOther }

    let combinedByTimeEntries =
        orderedEntries @ overheadEntriesWithOther
        |> List.sortByDescending (fun entry -> entry.Elapsed)

    {
        Ordered =
            [
                { Title = "Passes"; Entries = orderedEntries }
            ]
            @ (overheadSection |> Option.toList)
        ByTime =
            [
                { Title = "By Time"; Entries = combinedByTimeEntries }
            ]
    }

let addExpectedActualDetails (expected: string option) (actual: string option) : string list =
    match expected, actual with
    | Some exp, Some act ->
        println "    Expected:"
        let expectedDetails =
            exp.Split('\n')
            |> Array.toList
            |> List.map (fun line ->
                println $"      {line}"
                $"Expected: {line}")
        println "    Actual:"
        let actualDetails =
            act.Split('\n')
            |> Array.toList
            |> List.map (fun line ->
                println $"      {line}"
                $"Actual: {line}")
        expectedDetails @ actualDetails
    | _ -> []

let runFileSuite
    (state: TestRunState)
    (symbols: OutputSymbols)
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
            recordTiming state {
                Name = formatTimingName testName
                TotalTime = testTimer.Elapsed
                CompileTime = None
                RuntimeTime = None
            }
            sectionPassed <- sectionPassed + summary.Passed
            sectionFailed <- sectionFailed + summary.Failed
            recordResults state summary.Passed summary.Failed summary.FailedTests

        ProgressBar.finish progress
        sectionTimer.Stop()
        if sectionFailed = 0 then
            println $"  {Colors.green}{symbols.Pass} {sectionPassed} passed{Colors.reset}"
        else
            println $"  {Colors.green}{symbols.Pass} {sectionPassed} passed{Colors.reset}, {Colors.red}{symbols.Fail} {sectionFailed} failed{Colors.reset}"
        println $"  {Colors.gray}{symbols.SectionPrefix} Completed in {formatTime sectionTimer.Elapsed}{Colors.reset}"
        println ""

let runUnitTestSuites
    (state: TestRunState)
    (symbols: OutputSymbols)
    (suiteTitle: string)
    (progressLabel: string)
    (suites: UnitTestSuite array)
    : unit =
    let unitSectionTimer = Stopwatch.StartNew()
    println $"{Colors.cyan}{suiteTitle}{Colors.reset}"

    let mutable unitSectionPassed = 0
    let mutable unitSectionFailed = 0
    let unitFailedTests = ResizeArray<FailedTestInfo>()

    let totalUnitTests =
        suites
        |> Array.sumBy (fun suite -> suite.Tests.Length)
    let unitProgress = ProgressBar.create progressLabel totalUnitTests
    ProgressBar.update unitProgress

    for suite in suites do
        for (testName, runTest) in suite.Tests do
            let timer = Stopwatch.StartNew()
            let displayName = $"{suite.Name}: {testName}: "
            match runTest() with
            | Ok () ->
                timer.Stop()
                recordTiming state {
                    Name = $"Unit: {displayName}"
                    TotalTime = timer.Elapsed
                    CompileTime = None
                    RuntimeTime = None
                }
                unitSectionPassed <- unitSectionPassed + 1
                ProgressBar.increment unitProgress true
            | Error msg ->
                timer.Stop()
                recordTiming state {
                    Name = $"Unit: {suite.Name}: {testName}"
                    TotalTime = timer.Elapsed
                    CompileTime = None
                    RuntimeTime = None
                }
                recordTiming state {
                    Name = $"Unit: {displayName}"
                    TotalTime = timer.Elapsed
                    CompileTime = None
                    RuntimeTime = None
                }
                ProgressBar.increment unitProgress false
                ProgressBar.finish unitProgress
                println $"  {displayName}... {Colors.red}{symbols.Fail} FAIL{Colors.reset} {Colors.gray}({formatTime timer.Elapsed}){Colors.reset}"
                println $"    {msg}"
                unitFailedTests.Add({ File = ""; Name = $"Unit: {displayName}"; Message = msg; Details = [] })
                unitSectionFailed <- unitSectionFailed + 1
                ProgressBar.update unitProgress

    ProgressBar.finish unitProgress
    unitSectionTimer.Stop()
    if unitSectionFailed = 0 then
        println $"  {Colors.green}{symbols.Pass} {unitSectionPassed} passed{Colors.reset}"
    else
        println $"  {Colors.green}{symbols.Pass} {unitSectionPassed} passed{Colors.reset}, {Colors.red}{symbols.Fail} {unitSectionFailed} failed{Colors.reset}"
    println $"  {Colors.gray}{symbols.SectionPrefix} Completed in {formatTime unitSectionTimer.Elapsed}{Colors.reset}"
    println ""
    recordResults state unitSectionPassed unitSectionFailed (unitFailedTests |> Seq.toList)
