// CompilerCachingTests.fs - Unit tests for compiler caching behavior
//
// Ensures specialized functions are cached after compilation so they are not
// recompiled for every test expression.

module CompilerCachingTests

open CompilerLibrary

/// Test result type
type TestResult = Result<unit, string>

let private withFreshCaches (stdlib: StdlibResult) : StdlibResult =
    { stdlib with
        SpecCache = SpecializationCache()
        CompiledFuncCache = createCompiledFunctionCache ()
        ANFFuncCache = ANFFunctionCache()
        PreambleCache = PreambleCache()
        CodegenCache = CodegenCache() }

let private isVerificationEnabled () : bool =
    System.Environment.GetEnvironmentVariable("ENABLE_VERIFICATION_TESTS") = "true"

let private tryGetCompiledLazy
    (stdlib: StdlibResult)
    (key: CompiledFunctionKey)
    : Lazy<Result<CachedUserFunction, string>> option =
    match stdlib.CompiledFuncCache.TryGetValue key with
    | true, lazyEntry -> Some lazyEntry
    | false, _ -> None

let private makeTestSymbolicFunction (name: string) : LIRSymbolic.Function =
    let label = LIR.Label "entry"
    let block: LIRSymbolic.BasicBlock = { Label = label; Instrs = []; Terminator = LIRSymbolic.Ret }
    let cfg: LIRSymbolic.CFG = { Entry = label; Blocks = Map.ofList [ (label, block) ] }
    { Name = name
      TypedParams = []
      CFG = cfg
      StackSize = 0
      UsedCalleeSaved = [] }

/// Test that specialized functions are cached after compilation
let testSpecializedFunctionCaching (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = withFreshCaches sharedStdlib
    let source = "Stdlib.List.length<Int64>(Stdlib.List.map<Int64, Int64>([1, 2, 3], (x: Int64) => x + 1))"
    match Parser.parseString source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        match AST_to_ANF.convertUserOnlyCached stdlib.SpecCache stdlib.ANFFuncCache "" Map.empty stdlib.GenericFuncDefs stdlib.ANFResult userAst with
        | Error err -> Error $"ANF conversion error: {err}"
        | Ok userOnly ->
            if Set.isEmpty userOnly.SpecializedFuncNames then
                Error "Expected specialized functions but none were generated"
            else
                match CompilerLibrary.compilePreamble stdlib "" "cache_test" Map.empty with
                | Error err -> Error $"Preamble compilation failed: {err}"
                | Ok preambleCtx ->
                    let compileResult =
                        CompilerLibrary.compileTestWithPreamble 0 CompilerLibrary.defaultOptions stdlib preambleCtx "cache_test" source
                    if not compileResult.Success then
                        let errorMessage = compileResult.ErrorMessage |> Option.defaultValue "(no message)"
                        Error $"Compilation failed: {errorMessage}"
                    else
                        let missing =
                            userOnly.SpecializedFuncNames
                            |> Set.filter (fun name ->
                                not (stdlib.CompiledFuncCache.ContainsKey (CompiledFunctionKey.Stdlib name)))
                        if Set.isEmpty missing then
                            Ok ()
                        else
                            let missingList = System.String.Join(", ", Set.toList missing)
                            Error $"Missing cached specialized functions: {missingList}"

/// Test that stdlib compilation uses parallel mode
let testStdlibCompileModeIsParallel (sharedStdlib: StdlibResult) : TestResult =
    match sharedStdlib.CompileMode with
    | StdlibCompileMode.Parallel -> Ok ()
    | StdlibCompileMode.Sequential -> Error "Expected stdlib to compile in parallel mode"

/// Test that preamble functions are compiled once across tests in the same file
let testPreambleFunctionCachedOnce (sharedStdlib: StdlibResult) : TestResult =
    let stdlib = withFreshCaches sharedStdlib
    let preamble = "def foo(x: Int64): Int64 = x + 1"
    let sourceFile = "cache_preamble_test.e2e"
    let cacheKey = CompiledFunctionKey.Preamble (sourceFile, "foo")
    match CompilerLibrary.compilePreamble stdlib preamble sourceFile Map.empty with
    | Error err -> Error $"Preamble compilation failed: {err}"
    | Ok preambleCtx ->
        match tryGetCompiledLazy stdlib cacheKey with
        | None -> Error "Expected preamble function to be cached"
        | Some firstLazy ->
            let first =
                CompilerLibrary.compileTestWithPreamble
                    0
                    CompilerLibrary.defaultOptions
                    stdlib
                    preambleCtx
                    sourceFile
                    "foo(1)"
            let second =
                CompilerLibrary.compileTestWithPreamble
                    0
                    CompilerLibrary.defaultOptions
                    stdlib
                    preambleCtx
                    sourceFile
                    "foo(2)"
            if not first.Success then
                let message = first.ErrorMessage |> Option.defaultValue "(no message)"
                Error $"First compilation failed: {message}"
            elif not second.Success then
                let message = second.ErrorMessage |> Option.defaultValue "(no message)"
                Error $"Second compilation failed: {message}"
            else
                match tryGetCompiledLazy stdlib cacheKey with
                | Some secondLazy when obj.ReferenceEquals(firstLazy, secondLazy) -> Ok ()
                | Some _ -> Error "Expected preamble cache to reuse the same Lazy"
                | None -> Error "Expected preamble cache entry to remain available"

/// Test that the compiled function cache returns the same Lazy for a repeated key
let testCompiledFunctionCacheIsLazy () : TestResult =
    let cache = CompilerLibrary.createCompiledFunctionCache ()
    let key = CompilerLibrary.CompiledFunctionKey.Stdlib "cache_lazy"
    let entry1 : CachedUserFunction = {
        SymbolicFunction = makeTestSymbolicFunction "cache_lazy"
    }
    let entry2 : CachedUserFunction = {
        SymbolicFunction = makeTestSymbolicFunction "cache_lazy_alt"
    }
    let lazy1 =
        CompilerLibrary.getOrAddCompiledFunctionLazy cache key (fun () -> Ok entry1)
    let lazy2 =
        CompilerLibrary.getOrAddCompiledFunctionLazy cache key (fun () -> Ok entry2)
    if obj.ReferenceEquals(lazy1, lazy2) then
        Ok ()
    else
        Error "Expected compiled function cache to return the same Lazy for the same key"


let private runSpecializationCacheParallelDedup () : TestResult =
    let cache = SpecializationCache()
    let funcDef : AST.FunctionDef = {
        Name = "cache_parallel"
        TypeParams = [ "T" ]
        Params = [ ("x", AST.TVar "T") ]
        ReturnType = AST.TVar "T"
        Body = AST.Var "x"
    }
    let typeArgs = [ AST.TInt64 ]
    let workerCount = 16
    let barrier = new System.Threading.Barrier(workerCount + 1)
    let run () =
        barrier.SignalAndWait()
        AST_to_ANF.specializeFunctionCached cache funcDef typeArgs
    let tasks =
        [ 1 .. workerCount ]
        |> List.map (fun _ -> System.Threading.Tasks.Task.Run(fun () -> run ()))
    barrier.SignalAndWait()
    let waitTasks =
        tasks
        |> List.map (fun task -> task :> System.Threading.Tasks.Task)
        |> List.toArray
    System.Threading.Tasks.Task.WaitAll(waitTasks)
    let results = tasks |> List.map (fun task -> task.Result)
    match results with
    | [] -> Error "Expected specialization results"
    | first :: rest ->
        if rest |> List.forall (fun item -> obj.ReferenceEquals(first, item)) then
            Ok ()
        else
            Error "Expected specialization cache to return the same instance across threads"

/// Test that specialization caching only runs once per function when called in parallel
let testSpecializationCacheParallelDedup () : TestResult =
    if isVerificationEnabled () then
        runSpecializationCacheParallelDedup ()
    else
        Ok ()

let tests : (string * (StdlibResult -> TestResult)) list = [
    ("stdlib compile mode", testStdlibCompileModeIsParallel)
    ("specialized function caching", testSpecializedFunctionCaching)
    ("preamble function caching", testPreambleFunctionCachedOnce)
    ("compiled function cache is lazy", fun _ -> testCompiledFunctionCacheIsLazy ())
    ("specialization cache parallel dedup", fun _ -> testSpecializationCacheParallelDedup ())
]

let testsWithStdlib (sharedStdlib: StdlibResult) : (string * (unit -> TestResult)) list =
    tests |> List.map (fun (name, test) -> (name, fun () -> test sharedStdlib))

/// Run all compiler caching unit tests
let runAllWithStdlib (sharedStdlib: StdlibResult) : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests (testsWithStdlib sharedStdlib)

let runAll () : TestResult =
    match CompilerLibrary.compileStdlib () with
    | Error err -> Error $"Stdlib compile failed: {err}"
    | Ok stdlib -> runAllWithStdlib stdlib
