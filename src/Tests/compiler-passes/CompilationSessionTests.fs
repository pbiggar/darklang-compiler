// CompilationSessionTests.fs - Cache-contract tests for bounded compiler reuse.

module CompilationSessionTests

open AST

type TestResult = Result<unit, string>

let private compile
    (stdlib: CompilerLibrary.StdlibResult)
    (session: CompilerLibrary.CompilationSession)
    (options: CompilerLibrary.CompilerOptions)
    (source: string)
    : CompilerLibrary.CompileReport =
    CompilerLibrary.compile {
        Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.TestExpression
        Sources =
            NonEmptyList.singleton {
                CompilerLibrary.SourceUnit.Name = "CompilationSessionTests.dark"
                Purpose = NameSyntax.SourceUnitPurpose.Executable
                Source = source
            }
        AllowInternal = false
        Verbosity = 0
        Options = options
        PackageValues = CompilerLibrary.emptyPackageValueCatalog
        PassTimingRecorder = None
        Session = Some session
    }

let private expectCompiled (report: CompilerLibrary.CompileReport) : TestResult =
    match report.Result with
    | Ok _ -> Ok ()
    | Error error -> Error error

let private fakeFunction : LIR.Function =
    let entry = LIR.Label "cached_function_entry"
    {
        Name = "cached_function"
        TypedParams = []
        CFG = {
            Entry = entry
            Blocks = Map.ofList [
                entry, { Label = entry; Instrs = []; Terminator = LIR.Ret }
            ]
        }
        StackSize = 0
        UsedCalleeSaved = []
        CodegenFacts = None
    }

let testArm64HitWithNestedJson (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<List<List<Int64>>>(\"[[1,2],[3]]\")"
    match expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source),
          expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source) with
    | Ok (), Ok () when
        session.Arm64CodegenHitCount > 0
        && session.Arm64CodegenMissCount > 0
        && session.Arm64ReleasePlanSummaryHitCount > 0
        && session.Arm64ReleasePlanSummaryMissCount > 0
        && session.JsonPlanHitCount = 1
        && session.JsonPlanMissCount = 1 ->
        Ok ()
    | Ok (), Ok () ->
        Error $"Expected repeated nested JSON compilation to hit all caches, got ARM64 hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}; release-plan hits={session.Arm64ReleasePlanSummaryHitCount}, misses={session.Arm64ReleasePlanSummaryMissCount}; JSON hits={session.JsonPlanHitCount}, misses={session.JsonPlanMissCount}"
    | Error error, _
    | _, Error error -> Error error

let testArm64CodegenCacheSegregatesTargetOptionsAndCoverage (_: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let macOS = ARM64.targetConfigFor Platform.MacOSARM64
    let linux = ARM64.targetConfigFor Platform.LinuxARM64
    let changedOptions = { CodeGen.defaultOptions with DisableFreeList = true }
    let coverageOptions = { CodeGen.defaultOptions with EnableCoverage = true; CoverageExprCount = 1 }
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let _ = session.CodegenFunction macOS CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction macOS CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction linux CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction macOS changedOptions fakeFunction generate
    let _ = session.CodegenFunction macOS coverageOptions fakeFunction generate
    if calls.Count = 4 && session.CachedArm64FunctionCount = 3 && session.Arm64CodegenHitCount = 1 && session.Arm64CodegenMissCount = 3 then
        Ok ()
    else
        Error $"Expected target/options entries to segregate and coverage to bypass cache, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64CodegenMetricsAreOptIn (_: CompilerLibrary.StdlibResult) () : TestResult =
    use ordinary = new CompilerLibrary.CompilationSession()
    use profiled = new CompilerLibrary.CompilationSession(true)
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let generate () = Ok [ARM64Symbolic.RET]
    let _ = ordinary.CodegenFunction target CodeGen.defaultOptions fakeFunction generate
    let _ = profiled.CodegenFunction target CodeGen.defaultOptions fakeFunction generate
    match ordinary.Arm64CodegenMetrics, profiled.Arm64CodegenMetrics with
    | [], [metric] when
        metric.FunctionName = fakeFunction.Name
        && metric.LirInstructionCount = 1
        && metric.SymbolicInstructionCount = 1 ->
        Ok ()
    | ordinaryMetrics, profiledMetrics ->
        Error $"Expected only the opted-in session to retain one function metric, got ordinary={ordinaryMetrics.Length}, profiled={profiledMetrics.Length}"

let testArm64ReleasePlanSummaryCacheConfirmsPlanShape (_: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let firstPlan = ANF.NoReleasePlan
    let secondPlan = ANF.DynamicBufferRelease ANF.DynamicStringBuffer
    let summary needsClosure needsStream : LIR.Arm64ReleasePlanSummary = {
        ListDecHelperLabels = Set.empty
        PlannedListDecHelpers = Map.empty
        PlannedGenericDecHelpers = Map.empty
        DictDecHelperLabels = Set.empty
        PlannedDictDecHelpers = Map.empty
        NeedsClosureRcDecHelper = needsClosure
        NeedsStreamRcDecHelper = needsStream
    }
    let firstSummary = summary false false
    let secondSummary = summary true false
    let staticSummary = summary false true
    let generated = ResizeArray<unit>()
    let generate result () =
        generated.Add ()
        result
    let first = session.Arm64ReleasePlanSummary false "shared-key" firstPlan (generate firstSummary)
    let second = session.Arm64ReleasePlanSummary false "shared-key" secondPlan (generate secondSummary)
    let firstAgain = session.Arm64ReleasePlanSummary false "shared-key" firstPlan (generate secondSummary)
    let staticResult = session.Arm64ReleasePlanSummary true "shared-key" firstPlan (generate staticSummary)
    if first = firstSummary
       && second = secondSummary
       && firstAgain = firstSummary
       && staticResult = staticSummary
       && generated.Count = 3
       && session.CachedArm64ReleasePlanSummaryCount = 3
       && session.Arm64ReleasePlanSummaryHitCount = 1
       && session.Arm64ReleasePlanSummaryMissCount = 3 then
        Ok ()
    else
        Error $"Expected release-plan cache to confirm complete shapes and segregate static dependencies, got generated={generated.Count}, cached={session.CachedArm64ReleasePlanSummaryCount}, hits={session.Arm64ReleasePlanSummaryHitCount}, misses={session.Arm64ReleasePlanSummaryMissCount}"

let testSessionIsolationAndDisposal (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    let first = new CompilerLibrary.CompilationSession()
    let second = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<Int64>(\"42\")"
    let firstResult = expectCompiled (compile stdlib first CompilerLibrary.defaultOptions source)
    let secondResult = expectCompiled (compile stdlib second CompilerLibrary.defaultOptions source)
    (first :> System.IDisposable).Dispose()
    match firstResult, secondResult with
    | Ok (), Ok () when
        first.CachedArm64FunctionCount = 0
        && first.CachedArm64ReleasePlanSummaryCount = 0
        && first.CachedJsonPlanCount = 0
        && second.CachedArm64FunctionCount > 0
        && second.CachedArm64ReleasePlanSummaryCount > 0
        && second.CachedJsonPlanCount > 0 -> Ok ()
    | Ok (), Ok () ->
        Error $"Expected isolated sessions and disposal to release only the first registry, got first={first.CachedArm64FunctionCount}, second={second.CachedArm64FunctionCount}"
    | Error error, _
    | _, Error error -> Error error

let testJsonPlanCacheSegregatesNominalShapes (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let first =
        "type CachedJsonShape = { value: Int64 }\n"
        + "Stdlib.Json.parse<CachedJsonShape>(\"{\\\"value\\\":1}\")"
    let second =
        "type CachedJsonShape = { text: String }\n"
        + "Stdlib.Json.parse<CachedJsonShape>(\"{\\\"text\\\":\\\"ok\\\"}\")"
    match expectCompiled (compile stdlib session CompilerLibrary.defaultOptions first),
          expectCompiled (compile stdlib session CompilerLibrary.defaultOptions second),
          expectCompiled (compile stdlib session CompilerLibrary.defaultOptions first) with
    | Ok (), Ok (), Ok () when
        session.CachedJsonPlanCount = 2
        && session.JsonPlanMissCount = 2
        && session.JsonPlanHitCount = 1 ->
        Ok ()
    | Ok (), Ok (), Ok () ->
        Error $"Expected same-named distinct record shapes to use separate JSON plans, got cached={session.CachedJsonPlanCount}, hits={session.JsonPlanHitCount}, misses={session.JsonPlanMissCount}"
    | Error error, _, _
    | _, Error error, _
    | _, _, Error error -> Error error

let tests (stdlib: CompilerLibrary.StdlibResult) = [
    ("compilation session reuses ARM64 code for nested JSON", testArm64HitWithNestedJson stdlib)
    ("compilation session segregates ARM64 target options and coverage", testArm64CodegenCacheSegregatesTargetOptionsAndCoverage stdlib)
    ("compilation session codegen metrics are opt-in", testArm64CodegenMetricsAreOptIn stdlib)
    ("compilation session confirms ARM64 release-plan cache shapes", testArm64ReleasePlanSummaryCacheConfirmsPlanShape stdlib)
    ("compilation session isolates and disposes registries", testSessionIsolationAndDisposal stdlib)
    ("compilation session segregates canonical JSON declaration shapes", testJsonPlanCacheSegregatesNominalShapes stdlib)
]
