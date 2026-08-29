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
        SourceSyntax = CompilerLibrary.CompilerSyntax
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
    }

let testDecoderHitAndNestedReuse (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<List<List<Int64>>>(\"[[1,2],[3]]\")"
    match expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source),
          expectCompiled (compile stdlib session CompilerLibrary.defaultOptions source) with
    | Ok (), Ok () when
        session.CachedJsonPlanCount = 1
        && session.JsonPlanHitCount = 1
        && session.JsonPlanMissCount = 1
        && session.Arm64CodegenHitCount > 0
        && session.Arm64CodegenMissCount > 0 ->
        Ok ()
    | Ok (), Ok () ->
        Error $"Expected repeated nested JSON compilation to hit both caches, got JSON plans={session.CachedJsonPlanCount}, JSON hits={session.JsonPlanHitCount}, JSON misses={session.JsonPlanMissCount}, ARM64 hits={session.Arm64CodegenHitCount}, ARM64 misses={session.Arm64CodegenMissCount}"
    | Error error, _
    | _, Error error -> Error error

let testDecoderDeclarationContextIsolation (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    use session = new CompilerLibrary.CompilationSession()
    let first = "type SessionRecord = { value: Int64 }\nStdlib.Json.parse<SessionRecord>(\"{\\\"value\\\":1}\")"
    let second = "type SessionRecord = { label: String }\nStdlib.Json.parse<SessionRecord>(\"{\\\"label\\\":\\\"one\\\"}\")"
    match expectCompiled (compile stdlib session CompilerLibrary.defaultOptions first),
          expectCompiled (compile stdlib session CompilerLibrary.defaultOptions second) with
    | Ok (), Ok () when session.CachedJsonPlanCount = 2 && session.JsonPlanHitCount = 0 && session.JsonPlanMissCount = 2 -> Ok ()
    | Ok (), Ok () ->
        Error $"Expected isolated declaration contexts to create two decoder plans, got plans={session.CachedJsonPlanCount}, hits={session.JsonPlanHitCount}, misses={session.JsonPlanMissCount}"
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

let testSessionIsolationAndDisposal (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    let first = new CompilerLibrary.CompilationSession()
    let second = new CompilerLibrary.CompilationSession()
    let source = "Stdlib.Json.parse<Int64>(\"42\")"
    let firstResult = expectCompiled (compile stdlib first CompilerLibrary.defaultOptions source)
    let secondResult = expectCompiled (compile stdlib second CompilerLibrary.defaultOptions source)
    (first :> System.IDisposable).Dispose()
    match firstResult, secondResult with
    | Ok (), Ok () when first.CachedJsonPlanCount = 0 && second.CachedJsonPlanCount = 1 && second.JsonPlanMissCount = 1 -> Ok ()
    | Ok (), Ok () ->
        Error $"Expected isolated sessions and disposal to release only the first registry, got first={first.CachedJsonPlanCount}, second={second.CachedJsonPlanCount}"
    | Error error, _
    | _, Error error -> Error error

let tests (stdlib: CompilerLibrary.StdlibResult) = [
    ("compilation session reuses nested JSON decoders", testDecoderHitAndNestedReuse stdlib)
    ("compilation session isolates JSON declaration contexts", testDecoderDeclarationContextIsolation stdlib)
    ("compilation session segregates ARM64 target options and coverage", testArm64CodegenCacheSegregatesTargetOptionsAndCoverage stdlib)
    ("compilation session isolates and disposes registries", testSessionIsolationAndDisposal stdlib)
]
