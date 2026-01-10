// TestRunnerScheduling.fs - Helpers for test runner scheduling decisions
//
// Provides deterministic ordering and stdlib-related scheduling helpers.

module TestRunnerScheduling

open TestDSL.E2EFormat

type UnitTestSuite = string * int * (unit -> Result<unit, string>)

type StdlibWarmupPlan =
    | CompileStdlibBeforeTests
    | SkipStdlibWarmup

let estimateE2ETestCost (test: E2ETest) : int =
    test.Source.Length + test.Preamble.Length

let orderE2ETestsByEstimatedCost (tests: E2ETest array) : E2ETest array =
    tests
    |> Array.map (fun test -> test, estimateE2ETestCost test)
    |> Array.sortBy (fun (test, cost) -> (-cost, test.SourceFile, test.Name))
    |> Array.map fst

let splitUnitTestsByStdlibNeed
    (needsStdlib: string list)
    (suites: UnitTestSuite array)
    : UnitTestSuite array * UnitTestSuite array =
    let needsStdlibSet = Set.ofList needsStdlib
    let needsStdlibSuites, noStdlibSuites =
        suites
        |> Array.partition (fun (name, _, _) -> Set.contains name needsStdlibSet)
    (noStdlibSuites, needsStdlibSuites)

let shouldStartStdlibCompile (hasE2E: bool) (hasVerification: bool) (needsUnitStdlib: bool) : bool =
    hasE2E || hasVerification || needsUnitStdlib

let shouldRunUnitAndE2EInParallel (hasUnit: bool) (hasE2E: bool) : bool =
    hasUnit && hasE2E

let decideStdlibWarmupPlan (shouldCompileStdlib: bool) : StdlibWarmupPlan =
    if shouldCompileStdlib then
        CompileStdlibBeforeTests
    else
        SkipStdlibWarmup

let calculateOptimalParallelism (cpuCores: int) (totalMemoryGB: float) : int =
    // Each E2E test needs roughly ~100MB of memory (compiler + generated binary + overhead)
    let estimatedMemoryPerTestGB = 0.1
    let maxByMemory = int (totalMemoryGB / estimatedMemoryPerTestGB)
    let oversubscriptionFactor = 1.5
    let maxByCPU =
        float (cpuCores + 1) * oversubscriptionFactor
        |> System.Math.Ceiling
        |> int
    let optimal = min maxByMemory maxByCPU |> max 4
    optimal
