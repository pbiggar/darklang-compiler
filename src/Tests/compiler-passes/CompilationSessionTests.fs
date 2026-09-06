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
        && session.JsonPlanMissCount = 1
        && session.MirRegistryProjectionHitCount = 1
        && session.MirRegistryProjectionMissCount = 1
        && session.Arm64FunctionGroupHitCount > 0
        && session.Arm64FunctionGroupMissCount > 0 ->
        Ok ()
    | Ok (), Ok () ->
        Error $"Expected repeated nested JSON compilation to hit all caches, got ARM64 hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}; function-group hits={session.Arm64FunctionGroupHitCount}, misses={session.Arm64FunctionGroupMissCount}; release-plan hits={session.Arm64ReleasePlanSummaryHitCount}, misses={session.Arm64ReleasePlanSummaryMissCount}; JSON hits={session.JsonPlanHitCount}, misses={session.JsonPlanMissCount}; MIR registry hits={session.MirRegistryProjectionHitCount}, misses={session.MirRegistryProjectionMissCount}"
    | Error error, _
    | _, Error error -> Error error

let testArm64CodegenCacheSegregatesTargetOptionsAndCoverage (_: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let macOS = ARM64.targetConfigFor Platform.MacOSARM64
    let linux = ARM64.targetConfigFor Platform.LinuxARM64
    let changedOptions = { CodeGen.defaultOptions with DisableFreeList = true }
    let coverageOptions = { CodeGen.defaultOptions with EnableCoverage = true; CoverageExprCount = 1 }
    let contextIdentity = System.Object()
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let structurallyEquivalentFunction =
        { fakeFunction with Name = fakeFunction.Name }
    let _ = session.CodegenFunction contextIdentity macOS CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS CodeGen.defaultOptions structurallyEquivalentFunction generate
    let _ = session.CodegenFunction contextIdentity linux CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS changedOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS coverageOptions fakeFunction generate
    if calls.Count = 4 && session.CachedArm64FunctionCount = 3 && session.Arm64CodegenHitCount = 2 && session.Arm64CodegenMissCount = 3 then
        Ok ()
    else
        Error $"Expected reference and structural hits while target/options entries segregate and coverage bypasses the cache, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64CodegenMetricsAreOptIn (_: CompilerLibrary.StdlibResult) () : TestResult =
    use ordinary = new CompilerLibrary.CompilationSession()
    use profiled = new CompilerLibrary.CompilationSession(true)
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let contextIdentity = System.Object()
    let generate () = Ok [ARM64Symbolic.RET]
    let _ = ordinary.CodegenFunction contextIdentity target CodeGen.defaultOptions fakeFunction generate
    let _ = profiled.CodegenFunction contextIdentity target CodeGen.defaultOptions fakeFunction generate
    match ordinary.Arm64CodegenMetrics, profiled.Arm64CodegenMetrics with
    | [], [metric] when
        metric.FunctionName = fakeFunction.Name
        && metric.LirInstructionCount = 1
        && metric.SymbolicInstructionCount = 1 ->
        Ok ()
    | ordinaryMetrics, profiledMetrics ->
        Error $"Expected only the opted-in session to retain one function metric, got ordinary={ordinaryMetrics.Length}, profiled={profiledMetrics.Length}"

let testArm64CodegenCacheSegregatesCompilationContexts (_: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let firstContext = System.Object()
    let secondContext = System.Object()
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let structurallyEquivalentFunction =
        { fakeFunction with Name = fakeFunction.Name }
    let _ = session.CodegenFunction firstContext target CodeGen.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction firstContext target CodeGen.defaultOptions structurallyEquivalentFunction generate
    let _ = session.CodegenFunction secondContext target CodeGen.defaultOptions structurallyEquivalentFunction generate
    if calls.Count = 2
       && session.CachedArm64FunctionCount = 2
       && session.Arm64CodegenHitCount = 1
       && session.Arm64CodegenMissCount = 2 then
        Ok ()
    else
        Error $"Expected structurally equal functions to reuse only within one registry context, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64EmissionChunkCacheUsesChunkIdentity (_: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let instructions = [ARM64Symbolic.MOVZ (ARM64.X0, 42us, 0)]
    let structurallyEquivalentInstructions =
        [ARM64Symbolic.MOVZ (ARM64.X0, 42us, 0)]
    let preparations = ResizeArray<unit>()
    let prepare chunk () =
        preparations.Add ()
        ARM64_Encoding.prepareSymbolicChunk chunk
    let first =
        session.PrepareArm64EmissionChunk instructions (prepare instructions)
    let repeated =
        session.PrepareArm64EmissionChunk instructions (prepare instructions)
    let structurallyEquivalent =
        session.PrepareArm64EmissionChunk
            structurallyEquivalentInstructions
            (prepare structurallyEquivalentInstructions)
    if preparations.Count = 2
       && System.Object.ReferenceEquals(first, repeated)
       && not (System.Object.ReferenceEquals(first, structurallyEquivalent))
       && session.CachedArm64EmissionChunkCount = 2 then
        Ok ()
    else
        Error $"Expected identity-based prepared chunk reuse, got preparations={preparations.Count}, cached={session.CachedArm64EmissionChunkCount}, repeated={System.Object.ReferenceEquals(first, repeated)}, structural={System.Object.ReferenceEquals(first, structurallyEquivalent)}"

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

let testExpressionTypeCheckingReusesBaseRegistries
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    let baseEnv = stdlib.Context.TypeCheckEnv
    let source =
        "Stdlib.List.map<Int64, Int64>([1L, 2L], fun x -> x + 1L) == [2L, 3L]"
    CompilerLibrary.parseProgram false source
    |> Result.bind (fun program ->
        TypeChecking.checkProgramWithBaseEnvAndSettings
            baseEnv
            true
            CompilerLibrary.defaultWarningSettings
            program
        |> Result.mapError TypeChecking.typeErrorToString)
    |> Result.bind (fun (programType, Program topLevels, checkedEnv) ->
        let hasEqualityHelper =
            topLevels
            |> List.exists (function
                | FunctionDef fn -> fn.Name.StartsWith("__dark_eq_")
                | _ -> false)
        let reusesBaseRegistries =
            obj.ReferenceEquals(checkedEnv.TypeReg, baseEnv.TypeReg)
            && obj.ReferenceEquals(checkedEnv.IndexedTypeReg, baseEnv.IndexedTypeReg)
            && obj.ReferenceEquals(checkedEnv.VariantLookup, baseEnv.VariantLookup)
            && obj.ReferenceEquals(checkedEnv.FuncEnv, baseEnv.FuncEnv)
            && obj.ReferenceEquals(checkedEnv.FuncParamNames, baseEnv.FuncParamNames)
            && obj.ReferenceEquals(checkedEnv.GenericFuncDefs, baseEnv.GenericFuncDefs)
            && obj.ReferenceEquals(checkedEnv.AliasReg, baseEnv.AliasReg)
        if programType = TBool && hasEqualityHelper && reusesBaseRegistries then
            Ok ()
        else
            Error $"Expected expression-only checking to preserve generic/equality processing while reusing base registries, got type={TypeChecking.typeToString programType}, equalityHelper={hasEqualityHelper}, reused={reusesBaseRegistries}")

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
        && first.CachedAnfDependencyCount = 0
        && first.CachedCompiledDependencyCount = 0
        && first.CachedMirRegistryProjectionCount = 0
        && first.CachedArm64MetadataGroupCount = 0
        && first.CachedArm64FunctionGroupCount = 0
        && first.CachedArm64ReleasePlanSummaryCount = 0
        && first.CachedJsonPlanCount = 0
        && second.CachedArm64FunctionCount > 0
        && second.CachedAnfDependencyCount > 0
        && second.CachedCompiledDependencyCount > 0
        && second.CachedMirRegistryProjectionCount > 0
        && second.CachedArm64MetadataGroupCount > 0
        && second.CachedArm64FunctionGroupCount > 0
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
        && session.JsonPlanHitCount = 1
        && session.AnfDependencyMissCount = 2
        && session.AnfDependencyHitCount = 1
        && session.CompiledDependencyMissCount = 2
        && session.CompiledDependencyHitCount = 1 ->
        Ok ()
    | Ok (), Ok (), Ok () ->
        Error $"Expected same-named distinct record shapes to segregate every dependency cache, got JSON cached={session.CachedJsonPlanCount}, hits={session.JsonPlanHitCount}, misses={session.JsonPlanMissCount}; ANF hits={session.AnfDependencyHitCount}, misses={session.AnfDependencyMissCount}; compiled hits={session.CompiledDependencyHitCount}, misses={session.CompiledDependencyMissCount}"
    | Error error, _, _
    | _, Error error, _
    | _, _, Error error -> Error error

let testJsonDependenciesAreReusedBeforeLowering
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<List<List<Int64>>>(\"[[1,2],[3]]\")"
    expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source))
    |> Result.bind (fun () ->
        if session.AnfDependencyHitCount > 0
           && session.CompiledDependencyHitCount > 0 then
            Ok ()
        else
            Error $"Expected repeated JSON dependencies to bypass conversion and lowering, got ANF hits={session.AnfDependencyHitCount}, compiled hits={session.CompiledDependencyHitCount}")

let testStableStartTrampolineIsReused
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    expectCompiled (compile stdlib session CompilerLibrary.defaultOptions "1L + 1L")
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerLibrary.defaultOptions "2L + 2L"))
    |> Result.bind (fun () ->
        if session.CompiledStartHitCount > 0
           && session.CompiledStartMissCount = 1
           && session.Arm64StartCodegenHitCount > 0 then
            Ok ()
        else
            Error $"Expected source-independent _start lowering and codegen to be reused, got lowering hits={session.CompiledStartHitCount}, misses={session.CompiledStartMissCount}, codegen hits={session.Arm64StartCodegenHitCount}")

let testDependencyMetadataIsReusedCompositionally
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<List<Int64>>(\"[1,2,3]\")"
    expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source))
    |> Result.bind (fun () ->
        if session.Arm64MetadataGroupHitCount > 0 then Ok ()
        else Error "Expected cached dependency metadata to be merged without rescanning its functions")

let testStdlibReachabilityIsReused
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<List<Int64>>(\"[1,2,3]\")"
    expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source))
    |> Result.bind (fun () ->
        if session.StdlibReachabilityHitCount > 0
           && session.StdlibReachabilityMissCount > 0 then
            Ok ()
        else
            Error $"Expected identical stdlib roots to reuse reachability, got hits={session.StdlibReachabilityHitCount}, misses={session.StdlibReachabilityMissCount}")

let testArm64HelpersAreReused
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<List<Int64>>(\"[1,2,3]\")"
    expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source))
    |> Result.bind (fun () ->
        if session.Arm64HelperHitCount > 0
           && session.Arm64HelperMissCount > 0 then
            Ok ()
        else
            Error $"Expected identical helper programs to be reused, got hits={session.Arm64HelperHitCount}, misses={session.Arm64HelperMissCount}")

let tests (stdlib: CompilerLibrary.StdlibResult) = [
    ("compilation session reuses ARM64 code for nested JSON", testArm64HitWithNestedJson stdlib)
    ("compilation session segregates ARM64 target options and coverage", testArm64CodegenCacheSegregatesTargetOptionsAndCoverage stdlib)
    ("compilation session codegen metrics are opt-in", testArm64CodegenMetricsAreOptIn stdlib)
    ("compilation session segregates ARM64 registry contexts", testArm64CodegenCacheSegregatesCompilationContexts stdlib)
    ("compilation session reuses prepared ARM64 chunks by identity", testArm64EmissionChunkCacheUsesChunkIdentity stdlib)
    ("compilation session confirms ARM64 release-plan cache shapes", testArm64ReleasePlanSummaryCacheConfirmsPlanShape stdlib)
    ("expression-only type checking reuses base registries", testExpressionTypeCheckingReusesBaseRegistries stdlib)
    ("compilation session isolates and disposes registries", testSessionIsolationAndDisposal stdlib)
    ("compilation session segregates canonical JSON declaration shapes", testJsonPlanCacheSegregatesNominalShapes stdlib)
    ("compilation session reuses JSON dependencies before lowering", testJsonDependenciesAreReusedBeforeLowering stdlib)
    ("compilation session reuses the stable start trampoline", testStableStartTrampolineIsReused stdlib)
    ("compilation session composes cached dependency metadata", testDependencyMetadataIsReusedCompositionally stdlib)
    ("compilation session reuses stdlib reachability", testStdlibReachabilityIsReused stdlib)
    ("compilation session reuses identical ARM64 helper programs", testArm64HelpersAreReused stdlib)
]
