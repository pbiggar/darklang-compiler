// CompilationSessionTests.fs - Cache-contract tests for bounded compiler reuse.

module CompilationSessionTests

open AST

type TestResult = Result<unit, string>

let private compile
    (stdlib: CompilationContexts.StdlibResult)
    (session: CompilationSession.CompilationSession)
    (options: CompilerOptions.CompilerOptions)
    (source: string)
    : CompilerOptions.CompileReport =
    CompilerLibrary.compile {
        Context = CompilationContexts.StdlibOnly stdlib
        Mode = CompilerOptions.TestExpression
        Sources =
            NonEmptyList.singleton {
                CompilationContexts.SourceUnit.Name = "CompilationSessionTests.dark"
                Purpose = NameSyntax.SourceUnitPurpose.Executable
                Source = source
            }
        AllowInternal = false
        Verbosity = 0
        Options = options
        PackageValues = CompilationContexts.emptyPackageValueCatalog
        PassTimingRecorder = None
        Session = Some session
    }

let private expectCompiled (report: CompilerOptions.CompileReport) : TestResult =
    match report.Result with
    | Ok _ -> Ok ()
    | Error error -> Error error

let private fakeFunction : LIR.Function =
    let entry = LIR.Label "cached_function_entry"
    {
        Id = AST.functionIdForName "cached_function"
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

let private fakeMirFunction : MIR.Function =
    let entry = MIR.Label "cached_mir_function_entry"
    {
        Id = AST.functionIdForName "cached_mir_function"
        Name = "cached_mir_function"
        TypedParams = []
        ReturnType = TUnit
        CFG = {
            Entry = entry
            Blocks =
                Map.ofList [
                    entry,
                    {
                        Label = entry
                        Instrs = []
                        Terminator = MIR.Ret (MIR.Int64Const 0L)
                    }
                ]
        }
        FloatRegs = Set.empty
    }

let testSsaFunctionCacheReusesStructuralFunctions
    (_: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let conversions = ResizeArray<unit>()
    let convert () =
        conversions.Add ()
        SSA_Construction.convertFunctionToSSA fakeMirFunction
    let first = session.ConvertMirFunctionToSsa fakeMirFunction convert
    let structurallyEquivalent = { fakeMirFunction with Name = fakeMirFunction.Name }
    let second = session.ConvertMirFunctionToSsa structurallyEquivalent convert
    if conversions.Count = 1
       && obj.ReferenceEquals(first, second)
       && session.CachedSsaFunctionCount = 1
       && session.SsaFunctionHitCount = 1
       && session.SsaFunctionMissCount = 1 then
        Ok ()
    else
        Error $"Expected structurally identical MIR functions to share SSA conversion, got conversions={conversions.Count}, cached={session.CachedSsaFunctionCount}, hits={session.SsaFunctionHitCount}, misses={session.SsaFunctionMissCount}"

let testSsaFunctionCacheIgnoresFunctionLocalRegisterOffsets
    (_: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let makeFunction registerOffset : MIR.Function =
        let entry = MIR.Label "offset_mir_function_entry"
        let parameter = MIR.VReg registerOffset
        let result = MIR.VReg (registerOffset + 1)
        {
            Id = AST.functionIdForName "offset_mir_function"
            Name = "offset_mir_function"
            TypedParams = [{ Reg = parameter; Type = TInt64 }]
            ReturnType = TInt64
            CFG = {
                Entry = entry
                Blocks =
                    Map.ofList [
                        entry,
                        {
                            Label = entry
                            Instrs = [MIR.Mov (result, MIR.Register parameter, Some TInt64)]
                            Terminator = MIR.Ret (MIR.Register result)
                        }
                    ]
            }
            FloatRegs = Set.empty
        }
    let firstInput = makeFunction 100
    let secondInput = makeFunction 400
    let conversions = ResizeArray<unit>()
    let convert func () =
        conversions.Add ()
        SSA_Construction.convertFunctionToSSA func
    let first =
        session.ConvertMirFunctionToSsa firstInput (convert firstInput)
    let second =
        session.ConvertMirFunctionToSsa secondInput (convert secondInput)
    if conversions.Count = 1
       && obj.ReferenceEquals(first, second)
       && session.CachedSsaFunctionCount = 1
       && session.SsaFunctionHitCount = 1
       && session.SsaFunctionMissCount = 1 then
        Ok ()
    else
        Error $"Expected function-local MIR register offsets to share SSA conversion, got conversions={conversions.Count}, cached={session.CachedSsaFunctionCount}, hits={session.SsaFunctionHitCount}, misses={session.SsaFunctionMissCount}"

let testMirOptimizationCacheReusesStructuralFunctions
    (_: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let optimizations = ResizeArray<unit>()
    let key : CompilationCacheIdentity.MirOptimizationKey = {
        Function = fakeMirFunction
        Options = MIROptimizationFacts.defaultOptimizeOptions
        EffectFreeCalls = Set.empty
    }
    let optimize () =
        optimizations.Add ()
        MIR_Optimize.optimizeFunctionWithEffectFreeCallsAndTickTrace
            None
            Set.empty
            key.Options
            fakeMirFunction
    let first = session.OptimizeMirFunction key optimize
    let equivalentKey = {
        key with
            Function = { fakeMirFunction with Name = fakeMirFunction.Name }
    }
    let second = session.OptimizeMirFunction equivalentKey optimize
    if optimizations.Count = 1
       && obj.ReferenceEquals(first, second)
       && session.CachedMirOptimizationCount = 1
       && session.MirOptimizationHitCount = 1
       && session.MirOptimizationMissCount = 1 then
        Ok ()
    else
        Error $"Expected structurally identical SSA functions to share MIR optimization, got optimizations={optimizations.Count}, cached={session.CachedMirOptimizationCount}, hits={session.MirOptimizationHitCount}, misses={session.MirOptimizationMissCount}"

let testAllocatedLirFunctionCacheReusesStructuralFunctions
    (_: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let allocations = ResizeArray<unit>()
    let allocate () =
        allocations.Add ()
        { fakeFunction with StackSize = 16 }
    let first =
        session.AllocateLirFunction Platform.ARM64 fakeFunction allocate
    let structurallyEquivalent = { fakeFunction with Name = fakeFunction.Name }
    let second =
        session.AllocateLirFunction
            Platform.ARM64
            structurallyEquivalent
            allocate
    if allocations.Count = 1
       && obj.ReferenceEquals(first, second)
       && session.CachedAllocatedLirFunctionCount = 1
       && session.AllocatedLirFunctionHitCount = 1
       && session.AllocatedLirFunctionMissCount = 1 then
        Ok ()
    else
        Error $"Expected structurally identical LIR functions to share register allocation, got allocations={allocations.Count}, cached={session.CachedAllocatedLirFunctionCount}, hits={session.AllocatedLirFunctionHitCount}, misses={session.AllocatedLirFunctionMissCount}"

let testArm64HitWithNestedJson (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
    let source = "Stdlib.Json.parse<List<List<Int64>>>(\"[[1,2],[3]]\")"
    match expectCompiled (compile stdlib session CompilerOptions.defaultOptions source),
          expectCompiled (compile stdlib session CompilerOptions.defaultOptions source) with
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

let testArm64CodegenCacheSegregatesTargetOptionsAndCoverage (_: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
    let macOS = ARM64.targetConfigFor Platform.MacOSARM64
    let linux = ARM64.targetConfigFor Platform.LinuxARM64
    let changedOptions = { ARM64CodeGenTypes.defaultOptions with DisableFreeList = true }
    let coverageOptions = { ARM64CodeGenTypes.defaultOptions with EnableCoverage = true; CoverageExprCount = 1 }
    let contextIdentity = System.Object()
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let structurallyEquivalentFunction =
        { fakeFunction with Name = fakeFunction.Name }
    let _ = session.CodegenFunction contextIdentity macOS ARM64CodeGenTypes.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS ARM64CodeGenTypes.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS ARM64CodeGenTypes.defaultOptions structurallyEquivalentFunction generate
    let _ = session.CodegenFunction contextIdentity linux ARM64CodeGenTypes.defaultOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS changedOptions fakeFunction generate
    let _ = session.CodegenFunction contextIdentity macOS coverageOptions fakeFunction generate
    if calls.Count = 4 && session.CachedArm64FunctionCount = 3 && session.Arm64CodegenHitCount = 2 && session.Arm64CodegenMissCount = 3 then
        Ok ()
    else
        Error $"Expected reference and structural hits while target/options entries segregate and coverage bypasses the cache, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64CodegenMetricsAreOptIn (_: CompilationContexts.StdlibResult) () : TestResult =
    use ordinary = new CompilationSession.CompilationSession()
    use profiled = new CompilationSession.CompilationSession(true)
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let contextIdentity = System.Object()
    let generate () = Ok [ARM64Symbolic.RET]
    let _ = ordinary.CodegenFunction contextIdentity target ARM64CodeGenTypes.defaultOptions fakeFunction generate
    let _ = profiled.CodegenFunction contextIdentity target ARM64CodeGenTypes.defaultOptions fakeFunction generate
    match ordinary.Arm64CodegenMetrics, profiled.Arm64CodegenMetrics with
    | [], [metric] when
        metric.FunctionName = fakeFunction.Name
        && metric.LirInstructionCount = 1
        && metric.SymbolicInstructionCount = 1 ->
        Ok ()
    | ordinaryMetrics, profiledMetrics ->
        Error $"Expected only the opted-in session to retain one function metric, got ordinary={ordinaryMetrics.Length}, profiled={profiledMetrics.Length}"

let testArm64CodegenCacheSegregatesCompilationContexts (_: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let firstContext = System.Object()
    let secondContext = System.Object()
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let registryDependentFunction =
        let entry = LIR.Label "registry_dependent_entry"
        {
            fakeFunction with
                Id = AST.functionIdForName "registry_dependent_function"
                Name = "registry_dependent_function"
                CFG = {
                    Entry = entry
                    Blocks =
                        Map.ofList [
                            entry,
                            {
                                Label = entry
                                Instrs =
                                    [
                                        LIR.RawSlotInit (
                                            LIR.Physical LIR.X0,
                                            LIR.Physical LIR.X1,
                                            LIR.Physical LIR.X2,
                                            TRecord ("UserRecord", []))
                                    ]
                                Terminator = LIR.Ret
                            }
                        ]
                }
        }
        |> LIR.attachFunctionCodegenFacts
    let structurallyEquivalentFunction =
        { registryDependentFunction with Name = registryDependentFunction.Name }
    let _ = session.CodegenFunction firstContext target ARM64CodeGenTypes.defaultOptions registryDependentFunction generate
    let _ = session.CodegenFunction firstContext target ARM64CodeGenTypes.defaultOptions structurallyEquivalentFunction generate
    let _ = session.CodegenFunction secondContext target ARM64CodeGenTypes.defaultOptions structurallyEquivalentFunction generate
    if calls.Count = 2
       && session.CachedArm64FunctionCount = 2
       && session.Arm64CodegenHitCount = 1
       && session.Arm64CodegenMissCount = 2 then
        Ok ()
    else
        Error $"Expected structurally equal functions to reuse only within one registry context, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64CodegenCacheReusesContextIndependentFunctions
    (_: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let firstContext = System.Object()
    let secondContext = System.Object()
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let preparedFunction = LIR.attachFunctionCodegenFacts fakeFunction
    let structurallyEquivalentFunction =
        { preparedFunction with Name = preparedFunction.Name }
    let _ =
        session.CodegenFunction
            firstContext
            target
            ARM64CodeGenTypes.defaultOptions
            preparedFunction
            generate
    let _ =
        session.CodegenFunction
            secondContext
            target
            ARM64CodeGenTypes.defaultOptions
            structurallyEquivalentFunction
            generate
    if calls.Count = 1
       && session.CachedArm64FunctionCount = 1
       && session.Arm64CodegenHitCount = 1
       && session.Arm64CodegenMissCount = 1 then
        Ok ()
    else
        Error $"Expected registry-independent functions to reuse ARM64 code across compilation contexts, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64CodegenCacheReusesPlannedSlotInitFunctions
    (_: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let target = ARM64.targetConfigFor Platform.MacOSARM64
    let firstContext = System.Object()
    let secondContext = System.Object()
    let calls = ResizeArray<unit>()
    let generate () =
        calls.Add ()
        Ok []
    let slotInitFunction =
        let entry = LIR.Label "planned_slot_init_entry"
        {
            fakeFunction with
                Id = AST.functionIdForName "planned_slot_init_function"
                Name = "planned_slot_init_function"
                CFG = {
                    Entry = entry
                    Blocks =
                        Map.ofList [
                            entry,
                            {
                                Label = entry
                                Instrs =
                                    [
                                        LIR.RawSlotInit (
                                            LIR.Physical LIR.X0,
                                            LIR.Physical LIR.X1,
                                            LIR.Physical LIR.X2,
                                            TRecord ("UserRecord", []))
                                    ]
                                Terminator = LIR.Ret
                            }
                        ]
                }
        }
    let preparedFunction =
        LIR.Program (
            [slotInitFunction],
            Map.empty,
            Map.ofList [("UserRecord", [("value", TString)])])
        |> ARM64PrepareFunctions.prepareARM64Program
        |> fun (LIR.Program (functions, _, _)) -> List.head functions
    let structurallyEquivalentFunction =
        { preparedFunction with Name = preparedFunction.Name }
    let _ =
        session.CodegenFunction
            firstContext
            target
            ARM64CodeGenTypes.defaultOptions
            preparedFunction
            generate
    let _ =
        session.CodegenFunction
            secondContext
            target
            ARM64CodeGenTypes.defaultOptions
            structurallyEquivalentFunction
            generate
    if calls.Count = 1
       && session.CachedArm64FunctionCount = 1
       && session.Arm64CodegenHitCount = 1
       && session.Arm64CodegenMissCount = 1 then
        Ok ()
    else
        Error $"Expected planned RawSlotInit functions to reuse ARM64 code across compilation contexts, got calls={calls.Count}, cached={session.CachedArm64FunctionCount}, hits={session.Arm64CodegenHitCount}, misses={session.Arm64CodegenMissCount}"

let testArm64EmissionChunkCacheUsesChunkIdentity (_: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
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

let testArm64EmissionChunkGroupCacheUsesGroupIdentity (_: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
    let instructionParts =
        [[ARM64Symbolic.MOVZ (ARM64.X0, 42us, 0)]; [ARM64Symbolic.RET]]
    let structurallyEquivalentParts =
        [[ARM64Symbolic.MOVZ (ARM64.X0, 42us, 0)]; [ARM64Symbolic.RET]]
    let preparations = ResizeArray<unit>()
    let prepare parts () =
        preparations.Add ()
        parts
        |> List.map ARM64_Encoding.prepareSymbolicChunk
        |> ARM64_Encoding.combinePreparedChunks
    let first =
        session.PrepareArm64EmissionChunkGroup
            instructionParts
            (prepare instructionParts)
    let repeated =
        session.PrepareArm64EmissionChunkGroup
            instructionParts
            (prepare instructionParts)
    let structurallyEquivalent =
        session.PrepareArm64EmissionChunkGroup
            structurallyEquivalentParts
            (prepare structurallyEquivalentParts)
    if preparations.Count = 2
       && System.Object.ReferenceEquals(first, repeated)
       && not (System.Object.ReferenceEquals(first, structurallyEquivalent))
       && session.CachedArm64EmissionChunkCount = 2 then
        Ok ()
    else
        Error $"Expected identity-based prepared chunk-group reuse, got preparations={preparations.Count}, cached={session.CachedArm64EmissionChunkCount}, repeated={System.Object.ReferenceEquals(first, repeated)}, structural={System.Object.ReferenceEquals(first, structurallyEquivalent)}"

let testArm64ReleasePlanSummaryCacheConfirmsPlanShape (_: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
    let firstPlan = MemoryModel.NoReleasePlan
    let secondPlan = MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer
    let summary needsClosure needsStream : LIR.Arm64ReleasePlanSummary = {
        ListDecHelperLabels = Set.empty
        PlannedListDecHelpers = Map.empty
        ExpensiveGenericDecHelper = None
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
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    let baseEnv = stdlib.Context.TypeCheckEnv
    let source =
        "Stdlib.List.map<Int64, Int64> [1L, 2L] (fun x -> x + 1L) == [2L, 3L]"
    PackageCatalog.parseProgram false source
    |> Result.bind (fun program ->
        TypeChecking.checkProgramWithBaseEnvAndSettings
            baseEnv
            true
            CompilerOptions.defaultWarningSettings
            program
        |> Result.mapError CheckingDiagnostics.typeErrorToString)
    |> Result.bind (fun (programType, CheckedAST.Program (_, topLevels), checkedEnv) ->
        let hasEqualityHelper =
            topLevels
            |> List.exists (function
                | CheckedAST.FunctionDef fn -> fn.Name.StartsWith("__dark_eq_")
                | _ -> false)
        let reusesBaseRegistries =
            obj.ReferenceEquals(checkedEnv.TypeReg, baseEnv.TypeReg)
            && obj.ReferenceEquals(checkedEnv.IndexedTypeReg, baseEnv.IndexedTypeReg)
            && obj.ReferenceEquals(checkedEnv.VariantLookup, baseEnv.VariantLookup)
            && obj.ReferenceEquals(checkedEnv.IndexedSumTypeReg, baseEnv.IndexedSumTypeReg)
            && obj.ReferenceEquals(checkedEnv.FuncEnv, baseEnv.FuncEnv)
            && obj.ReferenceEquals(checkedEnv.FuncParamNames, baseEnv.FuncParamNames)
            && obj.ReferenceEquals(checkedEnv.GenericFuncDefs, baseEnv.GenericFuncDefs)
            && obj.ReferenceEquals(checkedEnv.AliasReg, baseEnv.AliasReg)
        if programType = TBool && hasEqualityHelper && reusesBaseRegistries then
            Ok ()
        else
            Error $"Expected expression-only checking to preserve generic/equality processing while reusing base registries, got type={CheckingDiagnostics.typeToString programType}, equalityHelper={hasEqualityHelper}, reused={reusesBaseRegistries}")

let testSessionIsolationAndDisposal (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    let first = new CompilationSession.CompilationSession()
    let second = new CompilationSession.CompilationSession()
    let source = "Stdlib.Json.parse<Int64>(\"42\")"
    let firstResult = expectCompiled (compile stdlib first CompilerOptions.defaultOptions source)
    let secondResult = expectCompiled (compile stdlib second CompilerOptions.defaultOptions source)
    (first :> System.IDisposable).Dispose()
    match firstResult, secondResult with
    | Ok (), Ok () when
        first.CachedArm64FunctionCount = 0
        && first.CachedSsaFunctionCount = 0
        && first.CachedMirOptimizationCount = 0
        && first.CachedAllocatedLirFunctionCount = 0
        && first.CachedAnfDependencyCount = 0
        && first.CachedCompiledDependencyCount = 0
        && first.CachedMirRegistryProjectionCount = 0
        && first.CachedArm64MetadataGroupCount = 0
        && first.CachedArm64FunctionGroupCount = 0
        && first.CachedArm64ReleasePlanSummaryCount = 0
        && first.CachedJsonPlanCount = 0
        && second.CachedArm64FunctionCount > 0
        && second.CachedSsaFunctionCount > 0
        && second.CachedMirOptimizationCount > 0
        && second.CachedAllocatedLirFunctionCount > 0
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

let testJsonPlanCacheSegregatesNominalShapes (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    use session = new CompilationSession.CompilationSession()
    let first =
        "type CachedJsonShape = { value: Int64 }\n"
        + "Stdlib.Json.parse<CachedJsonShape>(\"{\\\"value\\\":1}\")"
    let second =
        "type CachedJsonShape = { text: String }\n"
        + "Stdlib.Json.parse<CachedJsonShape>(\"{\\\"text\\\":\\\"ok\\\"}\")"
    match expectCompiled (compile stdlib session CompilerOptions.defaultOptions first),
          expectCompiled (compile stdlib session CompilerOptions.defaultOptions second),
          expectCompiled (compile stdlib session CompilerOptions.defaultOptions first) with
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
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let source = "Stdlib.Json.parse<List<List<Int64>>>(\"[[1,2],[3]]\")"
    expectCompiled (compile stdlib session CompilerOptions.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerOptions.defaultOptions source))
    |> Result.bind (fun () ->
        if session.AnfDependencyHitCount > 0
           && session.CompiledDependencyHitCount > 0 then
            Ok ()
        else
            Error $"Expected repeated JSON dependencies to bypass conversion and lowering, got ANF hits={session.AnfDependencyHitCount}, compiled hits={session.CompiledDependencyHitCount}")

let testStableStartTrampolineIsReused
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    expectCompiled (compile stdlib session CompilerOptions.defaultOptions "1L + 1L")
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerOptions.defaultOptions "2L + 2L"))
    |> Result.bind (fun () ->
        if session.CompiledStartHitCount > 0
           && session.CompiledStartMissCount = 1
           && session.Arm64StartCodegenHitCount > 0 then
            Ok ()
        else
            Error $"Expected source-independent _start lowering and codegen to be reused, got lowering hits={session.CompiledStartHitCount}, misses={session.CompiledStartMissCount}, codegen hits={session.Arm64StartCodegenHitCount}")

let testDependencyMetadataIsReusedCompositionally
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let source = "Stdlib.Json.parse<List<Int64>>(\"[1,2,3]\")"
    expectCompiled (compile stdlib session CompilerOptions.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerOptions.defaultOptions source))
    |> Result.bind (fun () ->
        if session.Arm64MetadataGroupHitCount > 0 then Ok ()
        else Error "Expected cached dependency metadata to be merged without rescanning its functions")

let testStdlibReachabilityIsReused
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let source functionName =
        $"let {functionName} (value: Int64) : String =\n"
        + $"    if value <= 0L then Stdlib.Int64.toString value\n"
        + $"    else {functionName} (value - 1L)\n\n"
        + $"{functionName} 1L"
    expectCompiled (compile stdlib session CompilerOptions.defaultOptions (source "first_user_function"))
    |> Result.bind (fun () ->
        expectCompiled (compile stdlib session CompilerOptions.defaultOptions (source "second_user_function")))
    |> Result.bind (fun () ->
        if session.StdlibReachabilityHitCount > 0
           && session.StdlibReachabilityMissCount > 0 then
            Ok ()
        else
            Error $"Expected equivalent stdlib roots with different user-local calls to reuse reachability, got hits={session.StdlibReachabilityHitCount}, misses={session.StdlibReachabilityMissCount}")

let testArm64HelpersAreReused
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    use session = new CompilationSession.CompilationSession()
    let source = "Stdlib.Json.parse<List<Int64>>(\"[1,2,3]\")"
    expectCompiled (compile stdlib session CompilerOptions.defaultOptions source)
    |> Result.bind (fun () -> expectCompiled (compile stdlib session CompilerOptions.defaultOptions source))
    |> Result.bind (fun () ->
        if session.Arm64HelperHitCount > 0
           && session.Arm64HelperMissCount > 0 then
            Ok ()
        else
            Error $"Expected identical helper programs to be reused, got hits={session.Arm64HelperHitCount}, misses={session.Arm64HelperMissCount}")

let tests (target: Platform.Target) (stdlib: CompilationContexts.StdlibResult) =
    let allTests = [
        ("compilation session reuses ARM64 code for nested JSON", testArm64HitWithNestedJson stdlib)
        ("compilation session segregates ARM64 target options and coverage", testArm64CodegenCacheSegregatesTargetOptionsAndCoverage stdlib)
        ("compilation session codegen metrics are opt-in", testArm64CodegenMetricsAreOptIn stdlib)
        ("compilation session reuses structural SSA functions", testSsaFunctionCacheReusesStructuralFunctions stdlib)
        ("compilation session ignores function-local MIR register offsets", testSsaFunctionCacheIgnoresFunctionLocalRegisterOffsets stdlib)
        ("compilation session reuses structural MIR optimizations", testMirOptimizationCacheReusesStructuralFunctions stdlib)
        ("compilation session reuses structural LIR allocation", testAllocatedLirFunctionCacheReusesStructuralFunctions stdlib)
        ("compilation session segregates ARM64 registry contexts", testArm64CodegenCacheSegregatesCompilationContexts stdlib)
        ("compilation session reuses registry-independent ARM64 functions", testArm64CodegenCacheReusesContextIndependentFunctions stdlib)
        ("compilation session reuses planned ARM64 slot-init functions", testArm64CodegenCacheReusesPlannedSlotInitFunctions stdlib)
        ("compilation session reuses prepared ARM64 chunks by identity", testArm64EmissionChunkCacheUsesChunkIdentity stdlib)
        ("compilation session reuses prepared ARM64 chunk groups by identity", testArm64EmissionChunkGroupCacheUsesGroupIdentity stdlib)
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
    match target with
    | Platform.ARM64Backend _ -> allTests
    | Platform.LinuxX86_64 ->
        let arm64OnlyInfrastructure =
            Set.ofList [
                "compilation session isolates and disposes registries"
                "compilation session reuses the stable start trampoline"
                "compilation session composes cached dependency metadata"
            ]
        allTests
        |> List.filter (fun (name, _) ->
            not (name.Contains("ARM64"))
            && not (Set.contains name arm64OnlyInfrastructure))
