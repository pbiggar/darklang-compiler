// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Concurrent
open Output

/// Result of compilation
type CompileResult = {
    Binary: byte array
    Success: bool
    ErrorMessage: string option
}

/// Result of execution
type ExecutionResult = {
    ExitCode: int
    Stdout: string
    Stderr: string
}

/// Result of execution with timing breakdown
type TimedExecutionResult = {
    ExitCode: int
    Stdout: string
    Stderr: string
    CompileTime: TimeSpan
    RuntimeTime: TimeSpan
}

/// Compiler options for controlling optimization behavior
type CompilerOptions = {
    /// Disable free list memory reuse (always bump allocate)
    DisableFreeList: bool
    /// Disable ANF-level optimizations (constant folding, propagation, etc.)
    DisableANFOpt: bool
    /// Disable ANF function inlining
    DisableInlining: bool
    /// Disable tail call optimization
    DisableTCO: bool
    /// Disable MIR-level optimizations (DCE, copy/constant propagation on SSA)
    DisableMIROpt: bool
    /// Disable LIR-level optimizations (peephole optimizations)
    DisableLIROpt: bool
    /// Disable dead code elimination (tree shaking of unused stdlib)
    DisableDCE: bool
    /// Enable runtime expression coverage tracking
    EnableCoverage: bool
    /// Dump ANF representations to stdout
    DumpANF: bool
    /// Dump MIR representations to stdout
    DumpMIR: bool
    /// Dump LIR representations to stdout (before and after register allocation)
    DumpLIR: bool
}

/// Default compiler options
let defaultOptions : CompilerOptions = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableInlining = false
    DisableTCO = false
    DisableMIROpt = false
    DisableLIROpt = false
    DisableDCE = false
    EnableCoverage = false
    DumpANF = false
    DumpMIR = false
    DumpLIR = false
}

/// Determine whether to dump a specific IR, based on verbosity or explicit option
let shouldDumpIR (verbosity: int) (enabled: bool) : bool =
    verbosity >= 3 || enabled

/// Print ANF program in a consistent, human-readable format
let printANFProgram (title: string) (program: ANF.Program) : unit =
    println title
    let (ANF.Program (funcs, mainExpr)) = program
    for func in funcs do
        println $"Function: {func.Name}"
        println $"  TypedParams: {func.TypedParams}"
        println $"  Body: {func.Body}"
        println ""
    println $"Main: {mainExpr}"
    println ""

/// Print MIR program (with CFG) in a consistent format
let printMIRProgram (title: string) (program: MIR.Program) : unit =
    let (MIR.Program (functions, _, _, _, _)) = program
    println title
    for func in functions do
        println $"\nFunction: {func.Name}"
        println $"Entry: {func.CFG.Entry}"
        for kvp in func.CFG.Blocks do
            let block = kvp.Value
            println $"\n{block.Label}:"
            for instr in block.Instrs do
                println $"  {instr}"
            println $"  {block.Terminator}"
    println ""

/// Print LIR program (with CFG) in a consistent format
let printLIRProgram (title: string) (program: LIR.Program) : unit =
    let (LIR.Program (funcs, _, _)) = program
    println title
    for func in funcs do
        println $"Function: {func.Name}"
        println $"Entry: {func.CFG.Entry}"
        for kvp in func.CFG.Blocks do
            let block = kvp.Value
            println $"\n{block.Label}:"
            for instr in block.Instrs do
                println $"  {instr}"
            println $"  {block.Terminator}"
    println ""

/// Cache for specialized (monomorphized) functions - thread-safe for parallel test execution
/// Key: (funcName, typeArgs as strings), Value: specialized AST.FunctionDef
type SpecializationCache = ConcurrentDictionary<string * string list, AST.FunctionDef>

/// Cached fully-compiled user function (after all passes through register allocation)
/// Includes the strings/floats it references so pools can be reconstructed
type CachedUserFunction = {
    /// The compiled LIR function (after register allocation)
    LIRFunction: LIR.Function
    /// Strings referenced by this function (for pool reconstruction)
    Strings: (int * string) list  // (original pool id, string value)
    /// Floats referenced by this function (for pool reconstruction)
    Floats: (int * float) list    // (original pool id, float value)
}

/// Cache for compiled specialized stdlib functions - avoids recompiling same specialization across tests
/// Key: function name (e.g., "FingerTree.empty_i64")
/// Value: Fully compiled LIR function with pool entries
type CompiledFunctionCache = ConcurrentDictionary<string, CachedUserFunction>

/// Cache for ANF-level user functions - avoids re-converting same function across tests
/// Key: (filename, line_number, function_name)
/// Value: (ANF.Function, ANF.VarGen) - VarGen tracks next available TempId after this function
type ANFFunctionCache = ConcurrentDictionary<string * int * string, ANF.Function * ANF.VarGen>

/// Compiled preamble context - extends stdlib for a test file
/// Preamble functions are compiled ONCE per file, then reused for all tests in that file
type PreambleContext = {
    /// Extended type checking environment (stdlib + preamble types/functions)
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    /// Preamble's generic function definitions for monomorphization
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    /// Preamble's ANF functions (after mono, inline, lift, ANF, RC, TCO)
    ANFFunctions: ANF.Function list
    /// Type map from RC insertion (merged with stdlib's TypeMap)
    TypeMap: ANF.TypeMap
    /// Preamble's registries for ANF conversion
    ANFResult: AST_to_ANF.ConversionResult
}

/// Cache for compiled preambles - thread-safe for parallel test execution
/// Key: (sourceFile, preambleHash), Value: compiled PreambleContext
type PreambleCache = ConcurrentDictionary<string * int, PreambleContext>

/// Result of compiling stdlib - can be reused across compilations
type StdlibResult = {
    /// Parsed stdlib AST (for merging with user AST)
    AST: AST.Program
    /// Type-checked stdlib with inferred types
    TypedAST: AST.Program
    /// Type checking environment (registries for types, variants, functions)
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    /// ANF conversion result
    ANFResult: AST_to_ANF.ConversionResult
    /// Generic function definitions for on-demand monomorphization
    /// (e.g., Stdlib.List.length<t> needs to be specialized when user calls it with Int64)
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    /// Module registry for stdlib intrinsics (built once, reused)
    ModuleRegistry: AST.ModuleRegistry
    /// Cached MIR program (avoids re-converting stdlib ANF to MIR each compilation)
    MIRProgram: MIR.Program
    /// Cached LIR program (avoids re-converting stdlib MIR to LIR each compilation)
    LIRProgram: LIR.Program
    /// Pre-allocated stdlib functions (physical registers assigned, ready for merge)
    AllocatedFunctions: LIR.Function list
    /// Call graph for dead code elimination (which stdlib funcs call which other funcs)
    StdlibCallGraph: Map<string, Set<string>>
    /// Cache for specialized functions (shared across test compilations)
    SpecCache: SpecializationCache
    /// Stdlib ANF functions indexed by name (for coverage analysis)
    StdlibANFFunctions: Map<string, ANF.Function>
    /// Call graph at ANF level (for coverage analysis reachability)
    StdlibANFCallGraph: Map<string, Set<string>>
    /// TypeMap from RC insertion (needed for getReachableStdlibFunctions)
    StdlibTypeMap: ANF.TypeMap
    /// Cache for compiled specialized stdlib functions (shared across test compilations)
    CompiledFuncCache: CompiledFunctionCache
    /// Cache for ANF-level user functions (shared across test compilations)
    ANFFuncCache: ANFFunctionCache
    /// Cache for compiled preambles (shared across test compilations)
    PreambleCache: PreambleCache
}

/// Cache for lazily compiled LIR functions - thread-safe for parallel test execution
/// Uses Lazy<T> to ensure each function is compiled exactly once even under contention
type LIRCache = ConcurrentDictionary<string, Lazy<LIR.Function option>>

/// Lazy stdlib result - defers expensive compilation until we know what's needed
/// Only compiles stdlib functions that user code actually calls
type LazyStdlibResult = {
    /// Type checking environment (registries for types, variants, functions)
    TypeCheckEnv: TypeChecking.TypeCheckEnv
    /// Generic function definitions for on-demand monomorphization
    GenericFuncDefs: AST_to_ANF.GenericFuncDefs
    /// Module registry for stdlib intrinsics
    ModuleRegistry: AST.ModuleRegistry
    /// ANF conversion result (for registries used in user conversion)
    ANFResult: AST_to_ANF.ConversionResult
    /// Stdlib ANF functions after RC insertion, indexed by name for lazy compilation
    StdlibANFFunctions: Map<string, ANF.Function>
    /// Call graph at ANF level for early DCE
    StdlibANFCallGraph: Map<string, Set<string>>
    /// TypeMap from RC insertion - needed for MIR conversion (tail call arg types, etc.)
    StdlibTypeMap: ANF.TypeMap
    /// Cache for specialized functions (shared across test compilations)
    SpecCache: SpecializationCache
    /// Cache for ANF-level user functions (shared across test compilations)
    ANFFuncCache: ANFFunctionCache
    /// Stdlib MIR functions (after SSA + optimization), indexed by name for lazy LIR compilation
    StdlibMIRFunctions: Map<string, MIR.Function>
    /// Lazy cache for compiled LIR functions (compiled on-demand, thread-safe)
    LIRCache: LIRCache
    /// String pool for all stdlib functions (built during MIR conversion)
    StdlibStringPool: MIR.StringPool
    /// Float pool for all stdlib functions (built during MIR conversion)
    StdlibFloatPool: MIR.FloatPool
}

// Helper functions for exception-to-Result conversion (Darklang compatibility)

/// Compare two LIR functions and return differences
let compareLIRFunctions (name: string) (cached: LIR.Function) (fresh: LIR.Function) : string list =
    let differences = ResizeArray<string>()

    // Compare basic properties
    if cached.Name <> fresh.Name then
        differences.Add($"Name: cached={cached.Name}, fresh={fresh.Name}")
    if cached.StackSize <> fresh.StackSize then
        differences.Add($"StackSize: cached={cached.StackSize}, fresh={fresh.StackSize}")
    if cached.UsedCalleeSaved <> fresh.UsedCalleeSaved then
        differences.Add($"UsedCalleeSaved: cached={cached.UsedCalleeSaved}, fresh={fresh.UsedCalleeSaved}")
    if cached.TypedParams <> fresh.TypedParams then
        differences.Add($"TypedParams: cached={cached.TypedParams}, fresh={fresh.TypedParams}")

    // Compare CFG
    let cachedBlocks = cached.CFG.Blocks |> Map.toList |> List.sortBy fst
    let freshBlocks = fresh.CFG.Blocks |> Map.toList |> List.sortBy fst

    if cached.CFG.Entry <> fresh.CFG.Entry then
        differences.Add($"CFG.Entry: cached={cached.CFG.Entry}, fresh={fresh.CFG.Entry}")

    if cachedBlocks.Length <> freshBlocks.Length then
        differences.Add($"Block count: cached={cachedBlocks.Length}, fresh={freshBlocks.Length}")
    else
        for ((cLabel, cBlock), (fLabel, fBlock)) in List.zip cachedBlocks freshBlocks do
            if cLabel <> fLabel then
                differences.Add($"Block label mismatch: cached={cLabel}, fresh={fLabel}")
            if cBlock.Terminator <> fBlock.Terminator then
                differences.Add($"Block {cLabel} terminator: cached={cBlock.Terminator}, fresh={fBlock.Terminator}")
            if cBlock.Instrs.Length <> fBlock.Instrs.Length then
                differences.Add($"Block {cLabel} instr count: cached={cBlock.Instrs.Length}, fresh={fBlock.Instrs.Length}")
            else
                for i, (cInstr, fInstr) in List.indexed (List.zip cBlock.Instrs fBlock.Instrs) do
                    if cInstr <> fInstr then
                        differences.Add($"Block {cLabel} instr {i}: cached={cInstr}, fresh={fInstr}")

    differences |> Seq.toList

/// Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
/// This is needed because buildReturnTypeReg only includes functions in the current program,
/// but we need return types for all callable functions (including stdlib)
let extractReturnTypes (funcReg: Map<string, AST.Type>) : Map<string, AST.Type> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (name, typ) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (name, retType)
        | other -> failwith $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

/// Try to delete a file, ignoring any errors
let tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message

/// Load a .dark file from possible paths
let loadDarkFile (filename: string) : Result<AST.Program, string> =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    let filePath =
        possiblePaths
        |> List.tryFind File.Exists
    match filePath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find {filename} in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        Parser.parseString source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")

/// Load the stdlib.dark and unicode_data.dark files
/// Returns the merged stdlib AST or an error message
let loadStdlib () : Result<AST.Program, string> =
    // Load stdlib.dark first
    match loadDarkFile "stdlib.dark" with
    | Error e -> Error e
    | Ok (AST.Program stdlibItems) ->
        // Try to load unicode_data.dark (optional, may not exist in all environments)
        match loadDarkFile "unicode_data.dark" with
        | Error _ ->
            // Unicode data not available, return stdlib only
            Ok (AST.Program stdlibItems)
        | Ok (AST.Program unicodeItems) ->
            // Merge stdlib and unicode data
            Ok (AST.Program (stdlibItems @ unicodeItems))

/// Merge two programs - stdlib functions come first
let mergePrograms (stdlib: AST.Program) (userProgram: AST.Program) : AST.Program =
    let (AST.Program stdlibItems) = stdlib
    let (AST.Program userItems) = userProgram
    AST.Program (stdlibItems @ userItems)

/// Merge typed stdlib with typed user program for ANF conversion
/// Excludes stdlib's dummy expression - only user's main expression is kept
let mergeTypedPrograms (stdlibTyped: AST.Program) (userTyped: AST.Program) : AST.Program =
    let (AST.Program stdlibItems) = stdlibTyped
    let (AST.Program userItems) = userTyped
    // Filter out any Expression items from stdlib (dummy main)
    let stdlibDefsOnly = stdlibItems |> List.filter (function AST.Expression _ -> false | _ -> true)
    AST.Program (stdlibDefsOnly @ userItems)

/// Compile stdlib in isolation, returning reusable result
/// This can be called once and the result reused for multiple user program compilations
let compileStdlib () : Result<StdlibResult, string> =
    match loadStdlib() with
    | Error e -> Error e
    | Ok stdlibAst ->
        // Add dummy main expression for type checking (stdlib has no main)
        let (AST.Program items) = stdlibAst
        let withMain = AST.Program (items @ [AST.Expression AST.UnitLiteral])

        match TypeChecking.checkProgramWithEnv withMain with
        | Error e -> Error (TypeChecking.typeErrorToString e)
        | Ok (_, typedStdlib, typeCheckEnv) ->
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = AST_to_ANF.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match AST_to_ANF.convertProgramWithTypes typedStdlib with
            | Error e -> Error e
            | Ok anfResult ->
                // Run RC insertion on stdlib ANF (stdlib functions need ref counting too)
                match RefCountInsertion.insertRCInProgram anfResult with
                | Error e -> Error e
                | Ok (anfAfterRC, typeMap) ->
                    // Pass 2.7: Tail Call Detection
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC
                    // Extract stdlib ANF functions for coverage analysis
                    let (ANF.Program (stdlibFuncs, _)) = anfAfterTCO
                    let stdlibFuncMap =
                        stdlibFuncs
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    // Build ANF-level call graph for coverage analysis
                    let stdlibANFCallGraph = ANFDeadCodeElimination.buildCallGraph stdlibFuncs
                    // Convert stdlib ANF to MIR (functions only, no _start)
                    // Use FuncParams for typeReg (needed for tail call argument types)
                    // Coverage is false here since stdlib is precompiled for caching
                    let externalReturnTypes = extractReturnTypes anfResult.FuncReg
                    match ANF_to_MIR.toMIRFunctionsOnly anfAfterTCO typeMap anfResult.FuncParams anfResult.VariantLookup Map.empty false externalReturnTypes with
                    | Error e -> Error e
                    | Ok (mirFuncs, stringPool, floatPool, variantRegistry, recordRegistry) ->
                        // Wrap in MIR.Program for LIR conversion
                        let mirProgram = MIR.Program (mirFuncs, stringPool, floatPool, variantRegistry, recordRegistry)
                        // Convert stdlib MIR to LIR (cached for reuse)
                        match MIR_to_LIR.toLIR mirProgram with
                        | Error e -> Error e
                        | Ok lirProgram ->
                            // Pre-allocate stdlib functions (cached for reuse)
                            let (LIR.Program (lirFuncs, lirStrings, lirFloats)) = lirProgram
                            let allocatedFuncs = lirFuncs |> List.map RegisterAllocation.allocateRegisters
                            // Build call graph for dead code elimination
                            let stdlibCallGraph = DeadCodeElimination.buildCallGraph allocatedFuncs
                            Ok {
                                AST = stdlibAst
                                TypedAST = typedStdlib
                                TypeCheckEnv = typeCheckEnv
                                ANFResult = anfResult
                                GenericFuncDefs = genericFuncDefs
                                ModuleRegistry = moduleRegistry
                                MIRProgram = mirProgram
                                LIRProgram = lirProgram
                                AllocatedFunctions = allocatedFuncs
                                StdlibCallGraph = stdlibCallGraph
                                SpecCache = SpecializationCache()
                                StdlibANFFunctions = stdlibFuncMap
                                StdlibANFCallGraph = stdlibANFCallGraph
                                StdlibTypeMap = typeMap
                                CompiledFuncCache = CompiledFunctionCache()
                                ANFFuncCache = ANFFunctionCache()
                                PreambleCache = PreambleCache()
                            }

/// Prepare stdlib for lazy compilation - stops at MIR level
/// LIR conversion and register allocation are done lazily when functions are needed
let prepareStdlibForLazyCompile () : Result<LazyStdlibResult, string> =
    match loadStdlib() with
    | Error e -> Error e
    | Ok stdlibAst ->
        // Add dummy main expression for type checking (stdlib has no main)
        let (AST.Program items) = stdlibAst
        let withMain = AST.Program (items @ [AST.Expression AST.UnitLiteral])

        match TypeChecking.checkProgramWithEnv withMain with
        | Error e -> Error (TypeChecking.typeErrorToString e)
        | Ok (_, typedStdlib, typeCheckEnv) ->
            // Extract generic function definitions for on-demand monomorphization
            let genericFuncDefs = AST_to_ANF.extractGenericFuncDefs typedStdlib
            // Build module registry once (reused across all compilations)
            let moduleRegistry = Stdlib.buildModuleRegistry ()
            match AST_to_ANF.convertProgramWithTypes typedStdlib with
            | Error e -> Error e
            | Ok anfResult ->
                // Run RC insertion on stdlib ANF
                match RefCountInsertion.insertRCInProgram anfResult with
                | Error e -> Error e
                | Ok (anfAfterRC, typeMap) ->
                    // Pass 2.7: Tail Call Detection
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC
                    // Extract stdlib functions into a map for lazy lookup
                    let (ANF.Program (stdlibFuncs, _)) = anfAfterTCO
                    let stdlibFuncMap =
                        stdlibFuncs
                        |> List.map (fun f -> f.Name, f)
                        |> Map.ofList
                    // Build call graph at ANF level for early DCE
                    let stdlibCallGraph = ANFDeadCodeElimination.buildCallGraph stdlibFuncs

                    // Convert ALL stdlib to MIR (builds string/float pools once)
                    // LIR conversion is deferred until functions are actually needed
                    let stdlibAnfProgram = ANF.Program (stdlibFuncs, ANF.Return ANF.UnitLiteral)
                    let stdlibTypeReg = anfResult.FuncParams
                    let stdlibVariantLookup = anfResult.VariantLookup

                    // Convert stdlib to MIR
                    let stdlibExternalReturnTypes = extractReturnTypes anfResult.FuncReg
                    match ANF_to_MIR.toMIRFunctionsOnly stdlibAnfProgram typeMap stdlibTypeReg stdlibVariantLookup Map.empty false stdlibExternalReturnTypes with
                    | Error e -> Error $"Stdlib MIR error: {e}"
                    | Ok (stdlibMirFuncs, stdlibStrings, stdlibFloats, stdlibVariants, stdlibRecords) ->
                        // SSA Construction
                        let stdlibMirProgram = MIR.Program (stdlibMirFuncs, stdlibStrings, stdlibFloats, stdlibVariants, stdlibRecords)
                        let stdlibSsaProgram = SSA_Construction.convertToSSA stdlibMirProgram

                        // MIR Optimizations
                        let stdlibOptimized = MIR_Optimize.optimizeProgram stdlibSsaProgram

                        // Extract optimized MIR functions into a map for lazy LIR compilation
                        let (MIR.Program (optimizedMirFuncs, mirStrings, mirFloats, _, _)) = stdlibOptimized
                        let stdlibMirMap =
                            optimizedMirFuncs
                            |> List.map (fun f -> f.Name, f)
                            |> Map.ofList

                        Ok {
                            TypeCheckEnv = typeCheckEnv
                            GenericFuncDefs = genericFuncDefs
                            ModuleRegistry = moduleRegistry
                            ANFResult = anfResult
                            StdlibANFFunctions = stdlibFuncMap
                            StdlibANFCallGraph = stdlibCallGraph
                            StdlibTypeMap = typeMap
                            SpecCache = SpecializationCache()
                            ANFFuncCache = ANFFunctionCache()
                            StdlibMIRFunctions = stdlibMirMap
                            LIRCache = LIRCache()
                            StdlibStringPool = mirStrings
                            StdlibFloatPool = mirFloats
                        }

/// Internal: Compile user code with stdlib AST (shared implementation)
/// This is the core compilation pipeline that does one pass of type-checking and ANF conversion
let private compileWithStdlibAST (verbosity: int) (options: CompilerOptions) (stdlibAst: AST.Program) (source: string) : CompileResult =
    let sw = Stopwatch.StartNew()
    try
        // Pass 1: Parse user code
        if verbosity >= 1 then println "  [1/8] Parse..."
        let parseResult = Parser.parseString source
        let parseTime = sw.Elapsed.TotalMilliseconds
        if verbosity >= 2 then
            let t = System.Math.Round(parseTime, 1)
            println $"        {t}ms"

        match parseResult with
        | Error err ->
            { Binary = Array.empty
              Success = false
              ErrorMessage = Some $"Parse error: {err}" }
        | Ok userAst ->
            // Merge user AST with stdlib AST
            let ast = mergePrograms stdlibAst userAst

            // Show AST
            if verbosity >= 3 then
                println "\n=== AST ==="
                let (AST.Program expr) = ast
                println $"{expr}"
                println ""

            // Pass 1.5: Type Checking
            if verbosity >= 1 then println "  [1.5/8] Type Checking..."
            let typeCheckResult = TypeChecking.checkProgram ast
            let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
            if verbosity >= 2 then
                let t = System.Math.Round(typeCheckTime, 1)
                println $"        {t}ms"

            match typeCheckResult with
            | Error typeErr ->
                { Binary = Array.empty
                  Success = false
                  ErrorMessage = Some $"Type error: {TypeChecking.typeErrorToString typeErr}" }
            | Ok (programType, transformedAst) ->
                if verbosity >= 3 then
                    println $"Program type: {TypeChecking.typeToString programType}"
                    println ""

                // Pass 2: AST → ANF (use transformed AST with type inference applied)
                if verbosity >= 1 then println "  [2/8] AST → ANF..."
                let anfResult = AST_to_ANF.convertProgramWithTypes transformedAst
                let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                if verbosity >= 2 then
                    let t = System.Math.Round(anfTime, 1)
                    println $"        {t}ms"

                match anfResult with
                | Error err ->
                    { Binary = Array.empty
                      Success = false
                      ErrorMessage = Some $"ANF conversion error: {err}" }
                | Ok convResult ->
                    // Show ANF before optimization
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (before optimization) ===" convResult.Program

                    // Pass 2.3: ANF Optimization
                    if verbosity >= 1 then println "  [2.3/8] ANF Optimization..."
                    let anfOptimized =
                        if options.DisableANFOpt then convResult.Program
                        else ANF_Optimize.optimizeProgram convResult.Program
                    let anfOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(anfOptTime, 1)
                        println $"        {t}ms"

                    // Show ANF after optimization
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after optimization) ===" anfOptimized

                    // Pass 2.4: ANF Inlining (optional)
                    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
                    let anfInlined =
                        if options.DisableInlining then anfOptimized
                        else ANF_Inlining.inlineProgramDefault anfOptimized
                    let inlineTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(inlineTime, 1)
                        println $"        {t}ms"

                    // Update convResult with optimized program for RC insertion
                    let convResultOptimized = { convResult with Program = anfInlined }

                    // Pass 2.5: Reference Count Insertion
                    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
                    let rcResult = RefCountInsertion.insertRCInProgram convResultOptimized
                    let rcTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - inlineTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(rcTime, 1)
                        println $"        {t}ms"

                    match rcResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Reference count insertion error: {err}" }
                    | Ok (anfAfterRC, typeMap) ->

                    // Show ANF after RC insertion
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after RC insertion) ===" anfAfterRC

                    // Pass 2.7: Tail Call Detection
                    if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
                    let anfAfterTCO =
                        if options.DisableTCO then anfAfterRC
                        else TailCallDetection.detectTailCallsInProgram anfAfterRC
                    let tcoTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(tcoTime, 1)
                        println $"        {t}ms"

                    // Show ANF after TCO
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after Tail Call Detection) ===" anfAfterTCO

                    // Pass 2.8: Print Insertion (for main expression)
                    if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                    let (ANF.Program (functions, mainExpr)) = anfAfterTCO
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType
                    let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - tcoTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(printTime, 1)
                        println $"        {t}ms"

                    // Show ANF after Print insertion
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after Print insertion) ===" anfProgram

                    // Pass 3: ANF → MIR
                    if verbosity >= 1 then println "  [3/8] ANF → MIR..."
                    // Build func params from function defs (for float param handling)
                    let funcParams =
                        let (AST.Program topLevels) = transformedAst
                        topLevels
                        |> List.choose (function
                            | AST.FunctionDef funcDef -> Some (funcDef.Name, funcDef.Params)
                            | _ -> None)
                        |> Map.ofList
                    // Use funcParams for MIR conversion (float param handling)
                    // TypeReg from ConversionResult contains record type definitions for printing
                    // typeMap contains TempId -> Type mappings from RC insertion
                    let externalReturnTypes = extractReturnTypes convResultOptimized.FuncReg
                    let mirResult = ANF_to_MIR.toMIR anfProgram typeMap funcParams programType convResultOptimized.VariantLookup convResultOptimized.TypeReg options.EnableCoverage externalReturnTypes

                    match mirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"MIR conversion error: {err}" }
                    | Ok mirProgram ->

                    // Show MIR
                    if shouldDumpIR verbosity options.DumpMIR then
                        printMIRProgram "=== MIR (Control Flow Graph) ===" mirProgram

                    let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(mirTime, 1)
                        println $"        {t}ms"

                    // Pass 3.1: SSA Construction (with liveness-aware phi insertion)
                    if verbosity >= 1 then println "  [3.1/8] SSA Construction..."
                    let ssaProgram = SSA_Construction.convertToSSA mirProgram

                    let ssaTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(ssaTime, 1)
                        println $"        {t}ms"

                    // Pass 3.5: MIR Optimizations (on SSA form)
                    if verbosity >= 1 then println "  [3.5/8] MIR Optimizations..."
                    let optimizedProgram =
                        if options.DisableMIROpt then ssaProgram
                        else MIR_Optimize.optimizeProgram ssaProgram

                    let mirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - ssaTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(mirOptTime, 1)
                        println $"        {t}ms"

                    // Pass 3.9 (SSA Destruction) has been removed.
                    // Phi resolution is now handled in RegisterAllocation.allocateRegisters

                    // Pass 4: MIR → LIR (SSA form is preserved)
                    if verbosity >= 1 then println "  [4/7] MIR → LIR..."
                    let lirResult = MIR_to_LIR.toLIR optimizedProgram

                    match lirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"LIR conversion error: {err}" }
                    | Ok lirProgram ->

                    // Show LIR
                    if shouldDumpIR verbosity options.DumpLIR then
                        printLIRProgram "=== LIR (Low-level IR with CFG) ===" lirProgram

                    let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(lirTime, 1)
                        println $"        {t}ms"

                    // Pass 4.5: LIR Optimizations (peephole)
                    if verbosity >= 1 then println "  [4.5/7] LIR Optimizations..."
                    let optimizedLirProgram =
                        if options.DisableLIROpt then lirProgram
                        else LIR_Optimize.optimizeProgram lirProgram

                    let lirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - ssaTime - mirOptTime - lirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(lirOptTime, 1)
                        println $"        {t}ms"

                    // Pass 5: Register Allocation (includes phi resolution)
                    if verbosity >= 1 then println "  [5/7] Register Allocation..."
                    let (LIR.Program (funcs, stringPool, floatPool)) = optimizedLirProgram
                    let allocatedFuncs = funcs |> List.map RegisterAllocation.allocateRegisters
                    let allocatedProgram = LIR.Program (allocatedFuncs, stringPool, floatPool)

                    // Show LIR after allocation
                    if shouldDumpIR verbosity options.DumpLIR then
                        printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                    let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocTime, 1)
                        println $"        {t}ms"

                    // Pass 6: Code Generation (LIR → ARM64)
                    if verbosity >= 1 then println "  [6/7] Code Generation..."
                    let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
                    let codegenOptions : CodeGen.CodeGenOptions = {
                        DisableFreeList = options.DisableFreeList
                        EnableCoverage = options.EnableCoverage
                        CoverageExprCount = coverageExprCount
                    }
                    let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
                    let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(codegenTime, 1)
                        println $"        {t}ms"

                    match codegenResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Code generation error: {err}" }
                    | Ok arm64Instructions ->
                        // Show ARM64
                        if verbosity >= 3 then
                            println "=== ARM64 Assembly Instructions ==="
                            for (i, instr) in List.indexed arm64Instructions do
                                println $"  {i}: {instr}"
                            println ""

                        // Detect platform for encoding and binary generation
                        let osResult = Platform.detectOS ()
                        match osResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some $"Platform detection error: {err}" }
                        | Ok os ->

                            // Pass 7: ARM64 Encoding (ARM64 → machine code)
                            // Use encodeAllWithStrings to handle ADRP/ADD_label for string addresses
                            if verbosity >= 1 then println "  [7/7] ARM64 Encoding..."

                            // Compute code file offset based on platform
                            let codeFileOffset =
                                match os with
                                | Platform.Linux ->
                                    // ELF: header (64) + 1 program header (56) = 120
                                    64 + 56
                                | Platform.MacOS ->
                                    // Mach-O: header (32) + load commands + padding
                                    // This needs to match 8_Binary_Generation_MachO.fs calculation
                                    let headerSize = 32
                                    // Same calculation as in createExecutableWithStrings
                                    let pageZeroCommandSize = 72
                                    let numTextSections = if stringPool.Strings.IsEmpty then 1 else 2
                                    let textSegmentCommandSize = 72 + (80 * numTextSections)
                                    let linkeditSegmentCommandSize = 72
                                    let dylinkerCommandSize = 32
                                    let dylibCommandSize = 56
                                    let symtabCommandSize = 24
                                    let dysymtabCommandSize = 80
                                    let uuidCommandSize = 24
                                    let buildVersionCommandSize = 24
                                    let mainCommandSize = 24
                                    let commandsSize = pageZeroCommandSize + textSegmentCommandSize + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize + symtabCommandSize + dysymtabCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize
                                    // Round up: (headerSize + commandsSize + 200 + 7) &&& ~~~7
                                    (headerSize + commandsSize + 200 + 7) &&& (~~~7)
                            let machineCode = ARM64_Encoding.encodeAllWithPools arm64Instructions stringPool floatPool codeFileOffset

                            // Show machine code
                            if verbosity >= 3 then
                                println "=== Machine Code (hex) ==="
                                for i in 0 .. 4 .. (machineCode.Length - 1) do
                                    if i + 3 < machineCode.Length then
                                        let bytes = sprintf "%02x %02x %02x %02x" machineCode.[i] machineCode.[i+1] machineCode.[i+2] machineCode.[i+3]
                                        println $"  {i:X4}: {bytes}"
                                println $"Total: {machineCode.Length} bytes\n"

                            let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(encodeTime, 1)
                                println $"        {t}ms"

                            // Pass 8: Binary Generation (machine code → executable)
                            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                            if verbosity >= 1 then println $"  [7/7] Binary Generation ({formatName})..."
                            let binary =
                                match os with
                                | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool
                                | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool
                            let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(binaryTime, 1)
                                println $"        {t}ms"

                            sw.Stop()

                            if verbosity >= 1 then
                                println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                            { Binary = binary
                              Success = true
                              ErrorMessage = None }
        with
        | ex ->
            { Binary = Array.empty
              Success = false
              ErrorMessage = Some $"Compilation failed: {ex.Message}" }

/// Compile user code with pre-compiled stdlib (separate compilation)
/// Uses stdlib.TypeCheckEnv for type checking user code, avoiding re-type-checking stdlib
/// Still merges typed ASTs for ANF conversion (Phase 2 will optimize that)
let compileWithStdlib (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (source: string) (sourceFile: string) (funcLineMap: Map<string, int>) : CompileResult =
    let sw = Stopwatch.StartNew()
    try
        // Pass 1: Parse user code only
        if verbosity >= 1 then println "  [1/8] Parse..."
        let parseResult = Parser.parseString source
        let parseTime = sw.Elapsed.TotalMilliseconds
        if verbosity >= 2 then
            let t = System.Math.Round(parseTime, 1)
            println $"        {t}ms"

        match parseResult with
        | Error err ->
            { Binary = Array.empty
              Success = false
              ErrorMessage = Some $"Parse error: {err}" }
        | Ok userAst ->
            // Pass 1.5: Type Checking (separate - user code with stdlib's TypeCheckEnv)
            if verbosity >= 1 then println "  [1.5/8] Type Checking (incremental)..."
            let typeCheckResult = TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv userAst
            let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
            if verbosity >= 2 then
                let t = System.Math.Round(typeCheckTime, 1)
                println $"        {t}ms"

            match typeCheckResult with
            | Error typeErr ->
                { Binary = Array.empty
                  Success = false
                  ErrorMessage = Some $"Type error: {TypeChecking.typeErrorToString typeErr}" }
            | Ok (programType, typedUserAst, _userEnv) ->
                if verbosity >= 3 then
                    println $"Program type: {TypeChecking.typeToString programType}"
                    println ""

                // Pass 2: AST → ANF (user only, separate from stdlib)
                // User code is converted with access to stdlib's registries for lookups.
                // User ANF is kept separate for MIR-level caching optimization.
                // Use cached version to avoid re-monomorphizing same generic functions.
                if verbosity >= 1 then println "  [2/8] AST → ANF (user only)..."
                let userOnlyResult =
                    AST_to_ANF.convertUserOnlyCached
                        stdlib.SpecCache
                        stdlib.ANFFuncCache
                        sourceFile
                        funcLineMap
                        stdlib.GenericFuncDefs
                        stdlib.ANFResult
                        typedUserAst
                let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                if verbosity >= 2 then
                    let t = System.Math.Round(anfTime, 1)
                    println $"        {t}ms"

                match userOnlyResult with
                | Error err ->
                    { Binary = Array.empty
                      Success = false
                      ErrorMessage = Some $"ANF conversion error: {err}" }
                | Ok userOnly ->
                    // Pass 2.3: ANF Optimization (user code only)
                    if verbosity >= 1 then println "  [2.3/8] ANF Optimization..."
                    let userProgram = ANF.Program (userOnly.UserFunctions, userOnly.MainExpr)
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (before optimization) ===" userProgram
                    let anfOptimized =
                        if options.DisableANFOpt then userProgram
                        else ANF_Optimize.optimizeProgram userProgram
                    let anfOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(anfOptTime, 1)
                        println $"        {t}ms"
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after optimization) ===" anfOptimized

                    // Pass 2.4: ANF Inlining (optional)
                    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
                    let anfInlined =
                        if options.DisableInlining then anfOptimized
                        else ANF_Inlining.inlineProgramDefault anfOptimized
                    let inlineTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(inlineTime, 1)
                        println $"        {t}ms"

                    // Create ConversionResult for RC insertion (user functions only)
                    let userConvResult : AST_to_ANF.ConversionResult = {
                        Program = anfInlined
                        TypeReg = userOnly.TypeReg
                        VariantLookup = userOnly.VariantLookup
                        FuncReg = userOnly.FuncReg
                        FuncParams = userOnly.FuncParams
                        ModuleRegistry = userOnly.ModuleRegistry
                    }

                    // Pass 2.5: Reference Count Insertion (user code only)
                    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
                    let rcResult = RefCountInsertion.insertRCInProgram userConvResult
                    let rcTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - inlineTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(rcTime, 1)
                        println $"        {t}ms"

                    match rcResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Reference count insertion error: {err}" }
                    | Ok (userAnfAfterRC, typeMap) ->
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after RC insertion) ===" userAnfAfterRC

                        // Pass 2.7: Tail Call Detection
                        if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
                        let userAnfAfterTCO =
                            if options.DisableTCO then userAnfAfterRC
                            else TailCallDetection.detectTailCallsInProgram userAnfAfterRC
                        let tcoTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(tcoTime, 1)
                            println $"        {t}ms"
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after Tail Call Detection) ===" userAnfAfterTCO

                        // Pass 2.8: Print Insertion (user code only)
                        if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                        let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterTCO
                        let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                        let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - tcoTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(printTime, 1)
                            println $"        {t}ms"
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after Print insertion) ===" userAnfProgram

                        // Pass 3: ANF → MIR (user code only)
                        if verbosity >= 1 then println "  [3/8] ANF → MIR (user only)..."
                        // Use FuncParams (maps function names to param types) for correct float param handling
                        // Use TypeReg from ConversionResult for record type definitions (for record printing)
                        // typeMap contains TempId -> Type mappings from RC insertion
                        let externalReturnTypes = extractReturnTypes userOnly.FuncReg
                        let userMirResult = ANF_to_MIR.toMIR userAnfProgram typeMap userOnly.FuncParams programType userConvResult.VariantLookup userConvResult.TypeReg options.EnableCoverage externalReturnTypes

                        match userMirResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some $"MIR conversion error: {err}" }
                        | Ok userMirProgram ->
                            if shouldDumpIR verbosity options.DumpMIR then
                                printMIRProgram "=== MIR (Control Flow Graph) ===" userMirProgram

                            let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(mirTime, 1)
                                println $"        {t}ms"

                            // Pass 3.1: SSA Construction (user code only)
                            if verbosity >= 1 then println "  [3.1/8] SSA Construction (user only)..."
                            let userSsaProgram = SSA_Construction.convertToSSA userMirProgram

                            let ssaTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(ssaTime, 1)
                                println $"        {t}ms"

                            // Pass 3.5: MIR Optimizations (user code only)
                            if verbosity >= 1 then println "  [3.5/8] MIR Optimizations (user only)..."
                            let userOptimizedProgram =
                                if options.DisableMIROpt then userSsaProgram
                                else MIR_Optimize.optimizeProgram userSsaProgram

                            let mirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - ssaTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(mirOptTime, 1)
                                println $"        {t}ms"

                            // Pass 3.9 (SSA Destruction) removed - phi resolution handled in RegisterAllocation

                            // Pass 4: MIR → LIR (user code only, phi nodes flow through)
                            if verbosity >= 1 then println "  [4/8] MIR → LIR (user only)..."
                            let userLirResult = MIR_to_LIR.toLIR userOptimizedProgram

                            match userLirResult with
                            | Error err ->
                                { Binary = Array.empty
                                  Success = false
                                  ErrorMessage = Some $"LIR conversion error: {err}" }
                            | Ok userLirProgram ->
                                if shouldDumpIR verbosity options.DumpLIR then
                                    printLIRProgram "=== LIR (Low-level IR with CFG) ===" userLirProgram

                                let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(lirTime, 1)
                                    println $"        {t}ms"

                                // Pass 4.5: LIR Optimizations (peephole - user code)
                                if verbosity >= 1 then println "  [4.5/8] LIR Optimizations (user only)..."
                                let userOptimizedLirProgram =
                                    if options.DisableLIROpt then userLirProgram
                                    else LIR_Optimize.optimizeProgram userLirProgram

                                let lirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(lirOptTime, 1)
                                    println $"        {t}ms"

                                // Pass 5: Register Allocation (user functions only, stdlib pre-allocated)
                                if verbosity >= 1 then println "  [5/8] Register Allocation (user only + cached stdlib)..."
                                let (LIR.Program (userFuncs, userStrings, userFloats)) = userOptimizedLirProgram
                                let (LIR.Program (_, stdlibStrings, stdlibFloats)) = stdlib.LIRProgram

                                // Allocate only user functions
                                let allocatedUserFuncs = userFuncs |> List.map RegisterAllocation.allocateRegisters

                                // Merge pools (stdlib first, user appended with offset)
                                let stringOffset = stdlibStrings.NextId
                                let floatOffset = stdlibFloats.NextId
                                let mergedStrings = ANF_to_MIR.appendStringPools stdlibStrings userStrings
                                let mergedFloats = ANF_to_MIR.appendFloatPools stdlibFloats userFloats

                                // Offset pool refs in allocated user functions
                                let offsetUserFuncs = allocatedUserFuncs |> List.map (MIR_to_LIR.offsetLIRFunction stringOffset floatOffset)

                                // Filter stdlib functions to only include reachable ones (dead code elimination)
                                let reachableStdlib =
                                    if options.DisableDCE then stdlib.AllocatedFunctions
                                    else
                                        DeadCodeElimination.filterFunctions
                                            stdlib.StdlibCallGraph
                                            offsetUserFuncs
                                            stdlib.AllocatedFunctions

                                // Combine reachable stdlib functions with user functions
                                let allFuncs = reachableStdlib @ offsetUserFuncs
                                let allocatedProgram = LIR.Program (allFuncs, mergedStrings, mergedFloats)
                                if shouldDumpIR verbosity options.DumpLIR then
                                    printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(allocTime, 1)
                                    println $"        {t}ms"

                                // Pass 6: Code Generation
                                if verbosity >= 1 then println "  [6/8] Code Generation..."
                                let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
                                let codegenOptions : CodeGen.CodeGenOptions = {
                                    DisableFreeList = options.DisableFreeList
                                    EnableCoverage = options.EnableCoverage
                                    CoverageExprCount = coverageExprCount
                                }
                                let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
                                let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(codegenTime, 1)
                                    println $"        {t}ms"

                                match codegenResult with
                                | Error err ->
                                    { Binary = Array.empty
                                      Success = false
                                      ErrorMessage = Some $"Code generation error: {err}" }
                                | Ok arm64Instructions ->
                                    let osResult = Platform.detectOS ()
                                    match osResult with
                                    | Error err ->
                                        { Binary = Array.empty
                                          Success = false
                                          ErrorMessage = Some $"Platform detection error: {err}" }
                                    | Ok os ->

                                        // Pass 7: ARM64 Encoding
                                        if verbosity >= 1 then println "  [7/7] ARM64 Encoding..."
                                        let codeFileOffset =
                                            match os with
                                            | Platform.Linux -> 64 + 56
                                            | Platform.MacOS ->
                                                let headerSize = 32
                                                let pageZeroCommandSize = 72
                                                let numTextSections = if mergedStrings.Strings.IsEmpty then 1 else 2
                                                let textSegmentCommandSize = 72 + (80 * numTextSections)
                                                let linkeditSegmentCommandSize = 72
                                                let dylinkerCommandSize = 32
                                                let dylibCommandSize = 56
                                                let symtabCommandSize = 24
                                                let dysymtabCommandSize = 80
                                                let uuidCommandSize = 24
                                                let buildVersionCommandSize = 24
                                                let mainCommandSize = 24
                                                let commandsSize = pageZeroCommandSize + textSegmentCommandSize + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize + symtabCommandSize + dysymtabCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize
                                                (headerSize + commandsSize + 200 + 7) &&& (~~~7)
                                        let machineCode = ARM64_Encoding.encodeAllWithPools arm64Instructions mergedStrings mergedFloats codeFileOffset

                                        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime
                                        if verbosity >= 2 then
                                            let t = System.Math.Round(encodeTime, 1)
                                            println $"        {t}ms"

                                        // Pass 8: Binary Generation
                                        let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                                        if verbosity >= 1 then println $"  [7/7] Binary Generation ({formatName})..."
                                        let binary =
                                            match os with
                                            | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode mergedStrings mergedFloats
                                            | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode mergedStrings mergedFloats
                                        let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
                                        if verbosity >= 2 then
                                            let t = System.Math.Round(binaryTime, 1)
                                            println $"        {t}ms"

                                        sw.Stop()
                                        if verbosity >= 1 then
                                            println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                                        { Binary = binary
                                          Success = true
                                          ErrorMessage = None }
                    with
                    | ex ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Compilation failed: {ex.Message}" }

                /// Compile preamble with stdlib as base, returning extended context for test compilation
                /// Preamble functions go through the full pipeline (parse → typecheck → mono → inline → lift → ANF → RC → TCO)
                /// The result is cached and reused for all tests in the same file
let compilePreamble (stdlib: StdlibResult) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : Result<PreambleContext, string> =
    // Handle empty preamble - return a context that just wraps stdlib
    if String.IsNullOrWhiteSpace(preamble) then
        Ok {
            TypeCheckEnv = stdlib.TypeCheckEnv
            GenericFuncDefs = stdlib.GenericFuncDefs
            ANFFunctions = []
            TypeMap = stdlib.StdlibTypeMap
            ANFResult = stdlib.ANFResult
        }
    else
        // Parse preamble with dummy expression (parser requires a main expression)
        let preambleSource = preamble + "\n0"
        match Parser.parseString preambleSource with
        | Error err -> Error $"Preamble parse error: {err}"
        | Ok preambleAst ->
            // Type-check preamble with stdlib.TypeCheckEnv
            match TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv preambleAst with
            | Error typeErr -> Error $"Preamble type error: {TypeChecking.typeErrorToString typeErr}"
            | Ok (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
                // Extract generic function definitions from preamble
                let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
                // Merge stdlib generics with preamble generics
                let mergedGenericDefs = Map.fold (fun acc k v -> Map.add k v acc) stdlib.GenericFuncDefs preambleGenericDefs

                // Convert preamble to ANF (mono → inline → lift → ANF)
                match AST_to_ANF.convertUserOnlyCached stdlib.SpecCache stdlib.ANFFuncCache sourceFile funcLineMap mergedGenericDefs stdlib.ANFResult typedPreambleAst with
                | Error err -> Error $"Preamble ANF conversion error: {err}"
                | Ok preambleUserOnly ->
                    // ANF Optimization (preamble functions)
                    let preambleProgram = ANF.Program (preambleUserOnly.UserFunctions, preambleUserOnly.MainExpr)
                    let anfOptimized = ANF_Optimize.optimizeProgram preambleProgram

                    // ANF Inlining (keep functions separate for now, can inline later)
                    let anfInlined = ANF_Inlining.inlineProgramDefault anfOptimized

                    // Create ConversionResult for RC insertion
                    let preambleConvResult : AST_to_ANF.ConversionResult = {
                        Program = anfInlined
                        TypeReg = preambleUserOnly.TypeReg
                        VariantLookup = preambleUserOnly.VariantLookup
                        FuncReg = preambleUserOnly.FuncReg
                        FuncParams = preambleUserOnly.FuncParams
                        ModuleRegistry = preambleUserOnly.ModuleRegistry
                    }

                    // RC Insertion
                    match RefCountInsertion.insertRCInProgram preambleConvResult with
                    | Error err -> Error $"Preamble RC insertion error: {err}"
                    | Ok (preambleAnfAfterRC, typeMap) ->
                        // Tail Call Detection
                        let preambleAnfAfterTCO = TailCallDetection.detectTailCallsInProgram preambleAnfAfterRC

                        // Extract just the functions (ignore main expr which is dummy `0`)
                        let (ANF.Program (preambleFunctions, _dummyMainExpr)) = preambleAnfAfterTCO

                        // Merge TypeMaps (stdlib + preamble)
                        let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                        Ok {
                            TypeCheckEnv = preambleTypeCheckEnv
                            GenericFuncDefs = mergedGenericDefs
                            ANFFunctions = preambleFunctions
                            TypeMap = mergedTypeMap
                            ANFResult = preambleConvResult
                        }

/// Compile test expression with pre-compiled preamble context
/// Only the tiny test expression is parsed/compiled - preamble functions are merged in
let compileTestWithPreamble (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult)
                            (preambleCtx: PreambleContext) (testExpr: string) : CompileResult =
    let sw = Stopwatch.StartNew()
    try
        // Pass 1: Parse test expression only (tiny)
        if verbosity >= 1 then println "  [1/8] Parse (test expr only)..."
        let parseResult = Parser.parseString testExpr
        let parseTime = sw.Elapsed.TotalMilliseconds
        if verbosity >= 2 then
            let t = System.Math.Round(parseTime, 1)
            println $"        {t}ms"

        match parseResult with
        | Error err ->
            { Binary = Array.empty
              Success = false
              ErrorMessage = Some $"Parse error: {err}" }
        | Ok testAst ->
            // Pass 1.5: Type Checking (test expr with preamble's TypeCheckEnv)
            if verbosity >= 1 then println "  [1.5/8] Type Checking (with preamble env)..."
            let typeCheckResult = TypeChecking.checkProgramWithBaseEnv preambleCtx.TypeCheckEnv testAst
            let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
            if verbosity >= 2 then
                let t = System.Math.Round(typeCheckTime, 1)
                println $"        {t}ms"

            match typeCheckResult with
            | Error typeErr ->
                { Binary = Array.empty
                  Success = false
                  ErrorMessage = Some $"Type error: {TypeChecking.typeErrorToString typeErr}" }
            | Ok (programType, typedTestAst, _testEnv) ->
                if verbosity >= 3 then
                    println $"Program type: {TypeChecking.typeToString programType}"
                    println ""

                // Pass 2: AST → ANF (test expr only, with preamble's generics and registries)
                if verbosity >= 1 then println "  [2/8] AST → ANF (test expr only)..."
                let testOnlyResult =
                    AST_to_ANF.convertUserOnlyCached
                        stdlib.SpecCache
                        stdlib.ANFFuncCache
                        ""  // No source file for test expr
                        Map.empty  // No function line map for test expr
                        preambleCtx.GenericFuncDefs
                        preambleCtx.ANFResult
                        typedTestAst
                let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                if verbosity >= 2 then
                    let t = System.Math.Round(anfTime, 1)
                    println $"        {t}ms"

                match testOnlyResult with
                | Error err ->
                    { Binary = Array.empty
                      Success = false
                      ErrorMessage = Some $"ANF conversion error: {err}" }
                | Ok testOnly ->
                    // With per-function RegGen, all functions are compiled fresh (no caching needed
                    // for determinism). Specialized stdlib functions get deterministic VRegs.
                    //
                    // NOTE: There's a known pre-existing compiler bug that causes crashes with:
                    // - (String, Int64) tuples extracted via Tuple2.first/second
                    // - Both values passed to Dict.set
                    // - Result passed to tail call
                    // See the Dict.fromList tests that are expected to fail until this is fixed.
                    let functionsToCompile = testOnly.UserFunctions
                    let cachedFuncs : ANF.Function list = []
                    let cachedFuncNames : string list = []

                    if verbosity >= 3 then
                        println $"  [COMPILE] All {functionsToCompile.Length} user functions compiled fresh"
                        for f in functionsToCompile do
                            println $"    - {f.Name}"

                    // No cached functions, so no cached return types needed
                    let cachedReturnTypes : Map<string, AST.Type> = Map.empty
                    let specializedNeedingCache : Set<string> = Set.empty

                    // Pass 2.3: ANF Optimization (only non-cached functions)
                    if verbosity >= 1 then println "  [2.3/8] ANF Optimization..."
                    let testProgram = ANF.Program (functionsToCompile, testOnly.MainExpr)
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (before optimization) ===" testProgram
                    let anfOptimized =
                        if options.DisableANFOpt then testProgram
                        else ANF_Optimize.optimizeProgram testProgram
                    let anfOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(anfOptTime, 1)
                        println $"        {t}ms"
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after optimization) ===" anfOptimized

                    // Pass 2.4: ANF Inlining (test expr only)
                    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
                    let anfInlined =
                        if options.DisableInlining then anfOptimized
                        else ANF_Inlining.inlineProgramDefault anfOptimized
                    let inlineTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(inlineTime, 1)
                        println $"        {t}ms"

                    // Create ConversionResult for RC insertion (uses preamble's registries)
                    let testConvResult : AST_to_ANF.ConversionResult = {
                        Program = anfInlined
                        TypeReg = testOnly.TypeReg
                        VariantLookup = testOnly.VariantLookup
                        FuncReg = testOnly.FuncReg
                        FuncParams = testOnly.FuncParams
                        ModuleRegistry = testOnly.ModuleRegistry
                    }

                    // Pass 2.5: Reference Count Insertion (test expr only)
                    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
                    let rcResult = RefCountInsertion.insertRCInProgram testConvResult
                    let rcTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - inlineTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(rcTime, 1)
                        println $"        {t}ms"

                    match rcResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Reference count insertion error: {err}" }
                    | Ok (testAnfAfterRC, testTypeMap) ->
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after RC insertion) ===" testAnfAfterRC

                        // Pass 2.7: Tail Call Detection (test expr only)
                        if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
                        let testAnfAfterTCO =
                            if options.DisableTCO then testAnfAfterRC
                            else TailCallDetection.detectTailCallsInProgram testAnfAfterRC
                        let tcoTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(tcoTime, 1)
                            println $"        {t}ms"
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after Tail Call Detection) ===" testAnfAfterTCO

                        // Merge preamble's ANF functions with test expr's ANF
                        let (ANF.Program (testFunctions, testMainExpr)) = testAnfAfterTCO
                        let mergedFunctions = preambleCtx.ANFFunctions @ testFunctions
                        let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) preambleCtx.TypeMap testTypeMap

                        // Pass 2.8: Print Insertion (on merged program)
                        if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                        let mergedAnfProgram = PrintInsertion.insertPrint mergedFunctions testMainExpr programType
                        let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - tcoTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(printTime, 1)
                            println $"        {t}ms"
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after Print insertion) ===" mergedAnfProgram

                        // Pass 3: ANF → MIR (merged program)
                        // Use FuncReg to get return types for all callable functions (including stdlib and cached)
                        if verbosity >= 1 then println "  [3/8] ANF → MIR..."
                        let allReturnTypes = extractReturnTypes testOnly.FuncReg
                        let userMirResult = ANF_to_MIR.toMIR mergedAnfProgram mergedTypeMap testOnly.FuncParams programType testConvResult.VariantLookup testConvResult.TypeReg options.EnableCoverage allReturnTypes

                        match userMirResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some $"MIR conversion error: {err}" }
                        | Ok userMirProgram ->
                            if shouldDumpIR verbosity options.DumpMIR then
                                printMIRProgram "=== MIR (Control Flow Graph) ===" userMirProgram

                            let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(mirTime, 1)
                                println $"        {t}ms"

                            // Pass 3.1: SSA Construction
                            if verbosity >= 1 then println "  [3.1/8] SSA Construction..."
                            let userSsaProgram = SSA_Construction.convertToSSA userMirProgram
                            let ssaTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(ssaTime, 1)
                                println $"        {t}ms"

                            // Pass 3.5: MIR Optimizations
                            if verbosity >= 1 then println "  [3.5/8] MIR Optimizations..."
                            let userOptimizedProgram =
                                if options.DisableMIROpt then userSsaProgram
                                else MIR_Optimize.optimizeProgram userSsaProgram
                            let mirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - ssaTime
                            if verbosity >= 2 then
                                let t = System.Math.Round(mirOptTime, 1)
                                println $"        {t}ms"

                            // Pass 4: MIR → LIR
                            if verbosity >= 1 then println "  [4/8] MIR → LIR..."
                            let userLirResult = MIR_to_LIR.toLIR userOptimizedProgram

                            match userLirResult with
                            | Error err ->
                                { Binary = Array.empty
                                  Success = false
                                  ErrorMessage = Some $"LIR conversion error: {err}" }
                            | Ok userLirProgram ->
                                if shouldDumpIR verbosity options.DumpLIR then
                                    printLIRProgram "=== LIR (Low-level IR with CFG) ===" userLirProgram

                                let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(lirTime, 1)
                                    println $"        {t}ms"

                                // Pass 4.5: LIR Optimizations (peephole)
                                if verbosity >= 1 then println "  [4.5/8] LIR Optimizations..."
                                let userOptimizedLirProgram =
                                    if options.DisableLIROpt then userLirProgram
                                    else LIR_Optimize.optimizeProgram userLirProgram

                                let lirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(lirOptTime, 1)
                                    println $"        {t}ms"

                                // Pass 5: Register Allocation
                                if verbosity >= 1 then println "  [5/8] Register Allocation..."
                                let (LIR.Program (userFuncs, userStrings, userFloats)) = userOptimizedLirProgram
                                let (LIR.Program (_, stdlibStrings, stdlibFloats)) = stdlib.LIRProgram

                                // Allocate user functions
                                let allocatedUserFuncs = userFuncs |> List.map RegisterAllocation.allocateRegisters

                                // Merge pools (stdlib first, user appended with offset)
                                let stringOffset = stdlibStrings.NextId
                                let floatOffset = stdlibFloats.NextId
                                let mergedStrings = ANF_to_MIR.appendStringPools stdlibStrings userStrings
                                let mergedFloats = ANF_to_MIR.appendFloatPools stdlibFloats userFloats

                                // Offset pool refs in allocated user functions
                                let offsetUserFuncs = allocatedUserFuncs |> List.map (MIR_to_LIR.offsetLIRFunction stringOffset floatOffset)

                                // LIR comparison code disabled since caching is disabled
                                // TODO: Re-enable when caching bug is fixed

                                // Cache newly-compiled specialized functions BEFORE offset adjustment.
                                // We cache un-offset functions so they can be re-offset correctly when
                                // retrieved in different test contexts (each test has different user pools).
                                for func in allocatedUserFuncs do
                                    if Set.contains func.Name specializedNeedingCache then
                                        let entry : CachedUserFunction = {
                                            LIRFunction = func  // UN-OFFSET version
                                            Strings = []
                                            Floats = []
                                        }
                                        let added = stdlib.CompiledFuncCache.TryAdd(func.Name, entry)
                                        if verbosity >= 3 then
                                            let blockCount = func.CFG.Blocks.Count
                                            let instrCount = func.CFG.Blocks |> Map.toList |> List.sumBy (fun (_, b) -> b.Instrs.Length)
                                            println $"  [CACHE STORE] {func.Name}: {blockCount} blocks, {instrCount} instrs, added={added}"

                                // Retrieve cached specialized functions and apply current test's offsets
                                let cachedSpecializedFuncs =
                                    cachedFuncNames
                                    |> List.choose (fun name ->
                                        match stdlib.CompiledFuncCache.TryGetValue(name) with
                                        | true, cached ->
                                            // Apply current test's pool offsets to cached function
                                            let func = MIR_to_LIR.offsetLIRFunction stringOffset floatOffset cached.LIRFunction
                                            if verbosity >= 3 then
                                                let blockCount = func.CFG.Blocks.Count
                                                let instrCount = func.CFG.Blocks |> Map.toList |> List.sumBy (fun (_, b) -> b.Instrs.Length)
                                                println $"  [CACHE HIT] {name}: {blockCount} blocks, {instrCount} instrs"
                                            Some func
                                        | false, _ -> None)

                                // Combine newly-compiled and cached user functions
                                let allUserFuncs = cachedSpecializedFuncs @ offsetUserFuncs

                                if verbosity >= 3 then
                                    println $"  [COMBINED] cached: {cachedSpecializedFuncs.Length}, fresh: {offsetUserFuncs.Length}, total: {allUserFuncs.Length}"
                                    for f in allUserFuncs do
                                        println $"    - {f.Name}"

                                // Filter stdlib functions to only include reachable ones (dead code elimination)
                                let reachableStdlib =
                                    if options.DisableDCE then stdlib.AllocatedFunctions
                                    else
                                        DeadCodeElimination.filterFunctions
                                            stdlib.StdlibCallGraph
                                            allUserFuncs
                                            stdlib.AllocatedFunctions

                                // Combine reachable stdlib functions with user functions
                                let allFuncs = reachableStdlib @ allUserFuncs
                                let allocatedProgram = LIR.Program (allFuncs, mergedStrings, mergedFloats)
                                if shouldDumpIR verbosity options.DumpLIR then
                                    printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(allocTime, 1)
                                    println $"        {t}ms"

                                // Pass 6: Code Generation
                                if verbosity >= 1 then println "  [6/8] Code Generation..."
                                let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
                                let codegenOptions : CodeGen.CodeGenOptions = {
                                    DisableFreeList = options.DisableFreeList
                                    EnableCoverage = options.EnableCoverage
                                    CoverageExprCount = coverageExprCount
                                }
                                let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
                                let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(codegenTime, 1)
                                    println $"        {t}ms"

                                match codegenResult with
                                | Error err ->
                                    { Binary = Array.empty
                                      Success = false
                                      ErrorMessage = Some $"Code generation error: {err}" }
                                | Ok arm64Instructions ->
                                    let osResult = Platform.detectOS ()
                                    match osResult with
                                    | Error err ->
                                        { Binary = Array.empty
                                          Success = false
                                          ErrorMessage = Some $"Platform detection error: {err}" }
                                    | Ok os ->

                                        // Pass 7: ARM64 Encoding
                                        if verbosity >= 1 then println "  [7/7] ARM64 Encoding..."
                                        let codeFileOffset =
                                            match os with
                                            | Platform.Linux -> 64 + 56
                                            | Platform.MacOS ->
                                                let headerSize = 32
                                                let pageZeroCommandSize = 72
                                                let numTextSections = if mergedStrings.Strings.IsEmpty then 1 else 2
                                                let textSegmentCommandSize = 72 + (80 * numTextSections)
                                                let linkeditSegmentCommandSize = 72
                                                let dylinkerCommandSize = 32
                                                let dylibCommandSize = 56
                                                let symtabCommandSize = 24
                                                let dysymtabCommandSize = 80
                                                let uuidCommandSize = 24
                                                let buildVersionCommandSize = 24
                                                let mainCommandSize = 24
                                                let commandsSize = pageZeroCommandSize + textSegmentCommandSize + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize + symtabCommandSize + dysymtabCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize
                                                (headerSize + commandsSize + 200 + 7) &&& (~~~7)
                                        let machineCode = ARM64_Encoding.encodeAllWithPools arm64Instructions mergedStrings mergedFloats codeFileOffset

                                        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime
                                        if verbosity >= 2 then
                                            let t = System.Math.Round(encodeTime, 1)
                                            println $"        {t}ms"

                                        // Pass 8: Binary Generation
                                        let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                                        if verbosity >= 1 then println $"  [7/7] Binary Generation ({formatName})..."
                                        let binary =
                                            match os with
                                            | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode mergedStrings mergedFloats
                                            | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode mergedStrings mergedFloats
                                        let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
                                        if verbosity >= 2 then
                                            let t = System.Math.Round(binaryTime, 1)
                                            println $"        {t}ms"

                                        sw.Stop()
                                        if verbosity >= 1 then
                                            println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                                        { Binary = binary
                                          Success = true
                                          ErrorMessage = None }
                    with
                    | ex ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Compilation failed: {ex.Message}" }

                /// Compile a single MIR function to LIR (used by cache factory)
let private compileMIRToLIR (stdlib: LazyStdlibResult) (mirFunc: MIR.Function) : LIR.Function option =
    // Create a mini MIR program with just this function
    let miniProgram = MIR.Program ([mirFunc], stdlib.StdlibStringPool, stdlib.StdlibFloatPool, Map.empty, Map.empty)

    // Convert to LIR
    match MIR_to_LIR.toLIR miniProgram with
    | Error _ -> None  // Conversion failed
    | Ok lirProgram ->
        // LIR Optimizations
        let optimizedLir = LIR_Optimize.optimizeProgram lirProgram
        let (LIR.Program (lirFuncs, _, _)) = optimizedLir

        // Register allocation
        match lirFuncs with
        | [lirFunc] -> Some (RegisterAllocation.allocateRegisters lirFunc)
        | funcs -> failwith $"compileMIRToLIR: Expected exactly 1 function, got {List.length funcs}"

/// Lazily compile a stdlib function from MIR to LIR with caching
/// Returns the compiled LIR function (from cache if available, or compiles and caches)
/// Uses Lazy<T> to ensure compilation happens exactly once even under parallel contention
let private getOrCompileStdlibLIR (stdlib: LazyStdlibResult) (funcName: string) : LIR.Function option =
    match Map.tryFind funcName stdlib.StdlibMIRFunctions with
    | None -> None  // Function not found in stdlib
    | Some mirFunc ->
        // GetOrAdd with Lazy ensures the factory is called at most once per key
        // Even if multiple threads call GetOrAdd simultaneously, only one Lazy is stored
        // and its Value is computed exactly once
        let lazyResult = stdlib.LIRCache.GetOrAdd(funcName, fun _ ->
            lazy (compileMIRToLIR stdlib mirFunc))
        lazyResult.Value

/// Get multiple stdlib functions, lazily compiling as needed
let private getStdlibLIRFunctions (stdlib: LazyStdlibResult) (funcNames: string list) : LIR.Function list =
    funcNames |> List.choose (getOrCompileStdlibLIR stdlib)

/// Compile user code with lazy stdlib - only compiles stdlib functions that are actually called
/// This avoids compiling unused stdlib functions through expensive passes (MIR, LIR, RegAlloc)
let compileWithLazyStdlib (verbosity: int) (options: CompilerOptions) (stdlib: LazyStdlibResult) (source: string) : CompileResult =
    let sw = Stopwatch.StartNew()
    try
        // Pass 1: Parse user code only
        if verbosity >= 1 then println "  [1/8] Parse..."
        let parseResult = Parser.parseString source
        let parseTime = sw.Elapsed.TotalMilliseconds
        if verbosity >= 2 then
            let t = System.Math.Round(parseTime, 1)
            println $"        {t}ms"

        match parseResult with
        | Error err ->
            { Binary = Array.empty
              Success = false
              ErrorMessage = Some $"Parse error: {err}" }
        | Ok userAst ->
            // Pass 1.5: Type Checking (user code with stdlib's TypeCheckEnv)
            if verbosity >= 1 then println "  [1.5/8] Type Checking (incremental)..."
            let typeCheckResult = TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv userAst
            let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
            if verbosity >= 2 then
                let t = System.Math.Round(typeCheckTime, 1)
                println $"        {t}ms"

            match typeCheckResult with
            | Error typeErr ->
                { Binary = Array.empty
                  Success = false
                  ErrorMessage = Some $"Type error: {TypeChecking.typeErrorToString typeErr}" }
            | Ok (programType, typedUserAst, _userEnv) ->
                if verbosity >= 3 then
                    println $"Program type: {TypeChecking.typeToString programType}"
                    println ""

                // Pass 2: AST → ANF (user only)
                // Use cached version to avoid re-monomorphizing same generic functions.
                if verbosity >= 1 then println "  [2/8] AST → ANF (user only)..."
                let userOnlyResult =
                    AST_to_ANF.convertUserOnlyCached
                        stdlib.SpecCache
                        stdlib.ANFFuncCache
                        ""  // No source file for non-test compilation
                        Map.empty  // No function line map
                        stdlib.GenericFuncDefs
                        stdlib.ANFResult
                        typedUserAst
                let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                if verbosity >= 2 then
                    let t = System.Math.Round(anfTime, 1)
                    println $"        {t}ms"

                match userOnlyResult with
                | Error err ->
                    { Binary = Array.empty
                      Success = false
                      ErrorMessage = Some $"ANF conversion error: {err}" }
                | Ok userOnly ->
                    // Pass 2.3: ANF Optimization (user code only)
                    if verbosity >= 1 then println "  [2.3/8] ANF Optimization..."
                    let userProgram = ANF.Program (userOnly.UserFunctions, userOnly.MainExpr)
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (before optimization) ===" userProgram
                    let anfOptimized =
                        if options.DisableANFOpt then userProgram
                        else ANF_Optimize.optimizeProgram userProgram
                    let anfOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(anfOptTime, 1)
                        println $"        {t}ms"
                    if shouldDumpIR verbosity options.DumpANF then
                        printANFProgram "=== ANF (after optimization) ===" anfOptimized

                    // Pass 2.4: ANF Inlining (optional)
                    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
                    let anfInlined =
                        if options.DisableInlining then anfOptimized
                        else ANF_Inlining.inlineProgramDefault anfOptimized
                    let inlineTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(inlineTime, 1)
                        println $"        {t}ms"

                    // Create ConversionResult for RC insertion (user functions only)
                    let userConvResult : AST_to_ANF.ConversionResult = {
                        Program = anfInlined
                        TypeReg = userOnly.TypeReg
                        VariantLookup = userOnly.VariantLookup
                        FuncReg = userOnly.FuncReg
                        FuncParams = userOnly.FuncParams
                        ModuleRegistry = userOnly.ModuleRegistry
                    }

                    // Pass 2.5: Reference Count Insertion (user code only)
                    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
                    let rcResult = RefCountInsertion.insertRCInProgram userConvResult
                    let rcTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - inlineTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(rcTime, 1)
                        println $"        {t}ms"

                    match rcResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Reference count insertion error: {err}" }
                    | Ok (userAnfAfterRC, typeMap) ->
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after RC insertion) ===" userAnfAfterRC

                        // Pass 2.7: Tail Call Detection
                        if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
                        let userAnfAfterTCO =
                            if options.DisableTCO then userAnfAfterRC
                            else TailCallDetection.detectTailCallsInProgram userAnfAfterRC
                        let tcoTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(tcoTime, 1)
                            println $"        {t}ms"
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after Tail Call Detection) ===" userAnfAfterTCO

                        // Pass 2.8: Print Insertion (user code only)
                        if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                        let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterTCO
                        let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                        let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - tcoTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(printTime, 1)
                            println $"        {t}ms"
                        if shouldDumpIR verbosity options.DumpANF then
                            printANFProgram "=== ANF (after Print insertion) ===" userAnfProgram

                        // Early DCE: Determine which stdlib functions are actually needed
                        // Lazily compile needed functions from MIR to LIR (cached for future use)
                        let (allocatedStdlibFuncs, stdlibLirStrings, stdlibLirFloats) =
                            if options.DisableDCE then
                                // Include all stdlib functions when DCE is disabled
                                let allNames = stdlib.StdlibMIRFunctions |> Map.keys |> List.ofSeq
                                let allFuncs = getStdlibLIRFunctions stdlib allNames
                                (allFuncs, stdlib.StdlibStringPool, stdlib.StdlibFloatPool)
                            else
                                let (ANF.Program (userFuncsForDCE, userMainForDCE)) = userAnfProgram
                                let userANFFuncs = { ANF.Name = "_start"; ANF.TypedParams = []; ANF.ReturnType = AST.TUnit; ANF.Body = userMainForDCE } :: userFuncsForDCE
                                let reachableStdlibNames = ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph userANFFuncs
                                if verbosity >= 2 then
                                    println $"        DCE: {reachableStdlibNames.Count} stdlib functions needed"
                                // Lazily compile needed LIR functions (from MIR, cached for future use)
                                let reachableFuncs = getStdlibLIRFunctions stdlib (Set.toList reachableStdlibNames)
                                (reachableFuncs, stdlib.StdlibStringPool, stdlib.StdlibFloatPool)

                        // Pass 3: ANF → MIR (user code only)
                        if verbosity >= 1 then println "  [3/8] ANF → MIR (user only)..."
                        // Use FuncParams for correct float param handling, VariantLookup for enum printing, TypeReg for record printing
                        // typeMap contains TempId -> Type mappings from RC insertion
                        let externalReturnTypes = extractReturnTypes userOnly.FuncReg
                        let userMirResult = ANF_to_MIR.toMIR userAnfProgram typeMap userOnly.FuncParams programType userConvResult.VariantLookup userConvResult.TypeReg options.EnableCoverage externalReturnTypes
                        let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(mirTime, 1)
                            println $"        {t}ms"

                        match userMirResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some $"MIR conversion error: {err}" }
                        | Ok userMirProgram ->
                            if shouldDumpIR verbosity options.DumpMIR then
                                printMIRProgram "=== MIR (Control Flow Graph) ===" userMirProgram

                            // Pass 3.1: SSA Construction (user code only)
                            if verbosity >= 1 then println "  [3.1/8] SSA Construction (user only)..."
                            let userSsaProgram = SSA_Construction.convertToSSA userMirProgram

                            // Pass 3.5: MIR Optimizations (user code only)
                            if verbosity >= 1 then println "  [3.5/8] MIR Optimizations (user only)..."
                            let userOptimizedProgram =
                                if options.DisableMIROpt then userSsaProgram
                                else MIR_Optimize.optimizeProgram userSsaProgram

                            // Pass 3.9 (SSA Destruction) removed - phi resolution handled in RegisterAllocation

                            // Pass 4: MIR → LIR (user code only, phi nodes flow through)
                            if verbosity >= 1 then println "  [4/8] MIR → LIR (user only)..."
                            let userLirResult = MIR_to_LIR.toLIR userOptimizedProgram

                            match userLirResult with
                            | Error err ->
                                { Binary = Array.empty
                                  Success = false
                                  ErrorMessage = Some $"LIR conversion error: {err}" }
                            | Ok userLirProgram ->
                                if shouldDumpIR verbosity options.DumpLIR then
                                    printLIRProgram "=== LIR (Low-level IR with CFG) ===" userLirProgram

                                let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(lirTime, 1)
                                    println $"        {t}ms"

                                // Pass 4.5: LIR Optimizations (peephole - user code)
                                if verbosity >= 1 then println "  [4.5/8] LIR Optimizations (user only)..."
                                let userOptimizedLirProgram =
                                    if options.DisableLIROpt then userLirProgram
                                    else LIR_Optimize.optimizeProgram userLirProgram

                                let lirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(lirOptTime, 1)
                                    println $"        {t}ms"

                                // Pass 5: Register Allocation (user functions)
                                if verbosity >= 1 then println "  [5/8] Register Allocation..."
                                let (LIR.Program (userFuncs, userStrings, userFloats)) = userOptimizedLirProgram

                                // Allocate user functions
                                let allocatedUserFuncs = userFuncs |> List.map RegisterAllocation.allocateRegisters

                                // Merge pools (stdlib first, user appended with offset)
                                let stringOffset = stdlibLirStrings.NextId
                                let floatOffset = stdlibLirFloats.NextId
                                let mergedStrings = ANF_to_MIR.appendStringPools stdlibLirStrings userStrings
                                let mergedFloats = ANF_to_MIR.appendFloatPools stdlibLirFloats userFloats

                                // Offset pool refs in allocated user functions
                                let offsetUserFuncs = allocatedUserFuncs |> List.map (MIR_to_LIR.offsetLIRFunction stringOffset floatOffset)

                                // Combine stdlib + user functions
                                let allFuncs = allocatedStdlibFuncs @ offsetUserFuncs
                                let allocatedProgram = LIR.Program (allFuncs, mergedStrings, mergedFloats)
                                if shouldDumpIR verbosity options.DumpLIR then
                                    printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                let regAllocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(regAllocTime, 1)
                                    println $"        {t}ms"

                                // Pass 6: Code Generation
                                if verbosity >= 1 then println "  [6/8] Code Generation..."
                                let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
                                let codegenOptions : CodeGen.CodeGenOptions = {
                                    DisableFreeList = options.DisableFreeList
                                    EnableCoverage = options.EnableCoverage
                                    CoverageExprCount = coverageExprCount
                                }
                                let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
                                let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - regAllocTime
                                if verbosity >= 2 then
                                    let t = System.Math.Round(codegenTime, 1)
                                    println $"        {t}ms"

                                match codegenResult with
                                | Error err ->
                                    { Binary = Array.empty
                                      Success = false
                                      ErrorMessage = Some $"Code generation error: {err}" }
                                | Ok arm64Instructions ->

                                    let osResult = Platform.detectOS ()
                                    match osResult with
                                    | Error err ->
                                        { Binary = Array.empty
                                          Success = false
                                          ErrorMessage = Some $"Platform detection error: {err}" }
                                    | Ok os ->

                                        // Pass 7: ARM64 Encoding
                                        if verbosity >= 1 then println "  [7/7] ARM64 Encoding..."
                                        let codeFileOffset =
                                            match os with
                                            | Platform.Linux -> 64 + 56
                                            | Platform.MacOS ->
                                                let headerSize = 32
                                                let pageZeroCommandSize = 72
                                                let numTextSections = if mergedStrings.Strings.IsEmpty then 1 else 2
                                                let textSegmentCommandSize = 72 + (80 * numTextSections)
                                                let linkeditSegmentCommandSize = 72
                                                let dylinkerCommandSize = 32
                                                let dylibCommandSize = 56
                                                let symtabCommandSize = 24
                                                let dysymtabCommandSize = 80
                                                let uuidCommandSize = 24
                                                let buildVersionCommandSize = 24
                                                let mainCommandSize = 24
                                                let commandsSize = pageZeroCommandSize + textSegmentCommandSize + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize + symtabCommandSize + dysymtabCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize
                                                (headerSize + commandsSize + 200 + 7) &&& (~~~7)
                                        let machineCode = ARM64_Encoding.encodeAllWithPools arm64Instructions mergedStrings mergedFloats codeFileOffset
                                        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - regAllocTime - codegenTime
                                        if verbosity >= 2 then
                                            let t = System.Math.Round(encodeTime, 1)
                                            println $"        {t}ms"

                                        // Pass 8: Binary Generation
                                        let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                                        if verbosity >= 1 then println $"  [7/7] Binary Generation ({formatName})..."
                                        let binary =
                                            match os with
                                            | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode mergedStrings mergedFloats
                                            | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode mergedStrings mergedFloats
                                        let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime - mirTime - lirTime - regAllocTime - codegenTime - encodeTime
                                        if verbosity >= 2 then
                                            let t = System.Math.Round(binaryTime, 1)
                                            println $"        {t}ms"

                                        sw.Stop()
                                        if verbosity >= 1 then
                                            println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                                        { Binary = binary
                                          Success = true
                                          ErrorMessage = None }
                        with
                        | ex ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some $"Compilation failed: {ex.Message}" }

                    /// Compile source code to binary using lazy stdlib compilation
                    /// Only compiles stdlib functions that are actually called by user code
let compileWithOptionsLazy (verbosity: int) (options: CompilerOptions) (source: string) : CompileResult =
    match prepareStdlibForLazyCompile() with
    | Error err ->
        { Binary = Array.empty
          Success = false
          ErrorMessage = Some err }
    | Ok lazyStdlib ->
        compileWithLazyStdlib verbosity options lazyStdlib source

/// Compile source code to binary (in-memory, no file I/O)
/// Uses lazy stdlib compilation - only compiles stdlib functions that are actually called
let compileWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : CompileResult =
    compileWithOptionsLazy verbosity options source

/// Compile source code to binary (uses default options)
let compile (verbosity: int) (source: string) : CompileResult =
    compileWithOptions verbosity defaultOptions source

/// Execute compiled binary and capture output
let execute (verbosity: int) (binary: byte array) : ExecutionResult =
    let sw = Stopwatch.StartNew()

    if verbosity >= 1 then println ""
    if verbosity >= 1 then println "  Execution:"

    // Write binary to temp file
    if verbosity >= 1 then println "    • Writing binary to temp file..."
    let tempPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

    // Write and flush to disk to minimize (but not eliminate) "Text file busy" race
    do
        use stream = new IO.FileStream(tempPath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.None)
        stream.Write(binary, 0, binary.Length)
        stream.Flush(true)  // Flush both stream and OS buffers to disk

    let writeTime = sw.Elapsed.TotalMilliseconds
    if verbosity >= 2 then println $"      {System.Math.Round(writeTime, 1)}ms"

    try
        // Make executable using Unix file mode
        if verbosity >= 1 then println "    • Setting executable permissions..."
        let permissions = File.GetUnixFileMode(tempPath)
        File.SetUnixFileMode(tempPath, permissions ||| IO.UnixFileMode.UserExecute)
        let chmodTime = sw.Elapsed.TotalMilliseconds - writeTime
        if verbosity >= 2 then println $"      {System.Math.Round(chmodTime, 1)}ms"

        // Code sign with adhoc signature (required for macOS only)
        let codesignResult =
            match Platform.detectOS () with
            | Error err ->
                // Platform detection failed
                Some $"Platform detection failed: {err}"
            | Ok os ->
                if Platform.requiresCodeSigning os then
                    if verbosity >= 1 then println "    • Code signing (adhoc)..."
                    let codesignStart = sw.Elapsed.TotalMilliseconds
                    let codesignInfo = ProcessStartInfo("codesign")
                    codesignInfo.Arguments <- $"-s - \"{tempPath}\""
                    codesignInfo.UseShellExecute <- false
                    codesignInfo.RedirectStandardOutput <- true
                    codesignInfo.RedirectStandardError <- true
                    let codesignProc = Process.Start(codesignInfo)
                    codesignProc.WaitForExit()

                    if codesignProc.ExitCode <> 0 then
                        let stderr = codesignProc.StandardError.ReadToEnd()
                        Some $"Code signing failed: {stderr}"
                    else
                        let codesignTime = sw.Elapsed.TotalMilliseconds - codesignStart
                        if verbosity >= 2 then println $"      {System.Math.Round(codesignTime, 1)}ms"
                        None
                else
                    if verbosity >= 1 then println "    • Code signing skipped (not required on Linux)"
                    None

        match codesignResult with
        | Some errorMsg ->
            // Code signing or platform detection failed - return error
            { ExitCode = -1
              Stdout = ""
              Stderr = errorMsg }
        | None ->
            // Execute (with retry for "Text file busy" race condition)
            // Even with flush, kernel may not have fully synced file/permissions in parallel tests
            if verbosity >= 1 then println "    • Running binary..."
            let execStart = sw.Elapsed.TotalMilliseconds
            let execInfo = ProcessStartInfo(tempPath)
            execInfo.RedirectStandardOutput <- true
            execInfo.RedirectStandardError <- true
            execInfo.UseShellExecute <- false

            // Retry up to 3 times with small delay if we get "Text file busy"
            let rec startWithRetry attempts =
                match tryStartProcess execInfo with
                | Ok proc -> Ok proc
                | Error msg when msg.Contains("Text file busy") && attempts > 0 ->
                    Threading.Thread.Sleep(10)  // Wait 10ms before retry
                    startWithRetry (attempts - 1)
                | Error msg -> Error msg

            match startWithRetry 3 with
            | Error msg ->
                { ExitCode = -1
                  Stdout = ""
                  Stderr = $"Failed to start process: {msg}" }
            | Ok execProc ->
                use proc = execProc
                // Start async reads immediately to avoid blocking
                let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                let stderrTask = proc.StandardError.ReadToEndAsync()

                // Wait for process to complete
                proc.WaitForExit()

                // Now wait for output to be fully read
                let stdout = stdoutTask.Result
                let stderr = stderrTask.Result

                let execTime = sw.Elapsed.TotalMilliseconds - execStart
                if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"

                sw.Stop()

                if verbosity >= 1 then
                    println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

                { ExitCode = proc.ExitCode
                  Stdout = stdout
                  Stderr = stderr }
    finally
        // Cleanup - ignore deletion errors
        tryDeleteFile tempPath

/// Compile and run source code with options
let compileAndRunWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : ExecutionResult =
    let compileResult = compileWithOptions verbosity options source

    if not compileResult.Success then
        { ExitCode = 1
          Stdout = ""
          Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
    else
        execute verbosity compileResult.Binary

/// Compile and run source code with pre-compiled stdlib
let compileAndRunWithStdlib (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (source: string) : ExecutionResult =
    let compileResult = compileWithStdlib verbosity options stdlib source "" Map.empty

    if not compileResult.Success then
        { ExitCode = 1
          Stdout = ""
          Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
    else
        execute verbosity compileResult.Binary

/// Compile and run source code with pre-compiled stdlib and user function caching
/// sourceFile and funcLineMap are used to build cache keys for user functions
/// Cache key: (sourceFile, lineNumber, functionName)
let compileAndRunWithStdlibCached (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (testExpr: string) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : ExecutionResult =
    // Check preamble cache
    let preambleHash = preamble.GetHashCode()
    let cacheKey = (sourceFile, preambleHash)

    let preambleCtxResult =
        match stdlib.PreambleCache.TryGetValue(cacheKey) with
        | true, cached ->
            if verbosity >= 2 then println $"  [Preamble cache hit for {sourceFile}]"
            Ok cached
        | false, _ ->
            if verbosity >= 2 then println $"  [Preamble cache miss for {sourceFile}]"
            match compilePreamble stdlib preamble sourceFile funcLineMap with
            | Ok ctx ->
                stdlib.PreambleCache.TryAdd(cacheKey, ctx) |> ignore
                Ok ctx
            | Error err -> Error err

    match preambleCtxResult with
    | Error err ->
        { ExitCode = 1
          Stdout = ""
          Stderr = err }
    | Ok preambleCtx ->
        let compileResult = compileTestWithPreamble verbosity options stdlib preambleCtx testExpr

        if not compileResult.Success then
            { ExitCode = 1
              Stdout = ""
              Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
        else
            execute verbosity compileResult.Binary

/// Compile and run with timing breakdown (for test output)
let compileAndRunWithStdlibCachedTimed (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (testExpr: string) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : TimedExecutionResult =
    // Check preamble cache
    let preambleHash = preamble.GetHashCode()
    let cacheKey = (sourceFile, preambleHash)

    let preambleCtxResult =
        match stdlib.PreambleCache.TryGetValue(cacheKey) with
        | true, cached ->
            if verbosity >= 2 then println $"  [Preamble cache hit for {sourceFile}]"
            Ok cached
        | false, _ ->
            if verbosity >= 2 then println $"  [Preamble cache miss for {sourceFile}]"
            match compilePreamble stdlib preamble sourceFile funcLineMap with
            | Ok ctx ->
                stdlib.PreambleCache.TryAdd(cacheKey, ctx) |> ignore
                Ok ctx
            | Error err -> Error err

    match preambleCtxResult with
    | Error err ->
        { ExitCode = 1
          Stdout = ""
          Stderr = err
          CompileTime = TimeSpan.Zero
          RuntimeTime = TimeSpan.Zero }
    | Ok preambleCtx ->
        let compileTimer = Stopwatch.StartNew()
        let compileResult = compileTestWithPreamble verbosity options stdlib preambleCtx testExpr
        compileTimer.Stop()
        let compileTime = compileTimer.Elapsed

        if not compileResult.Success then
            { ExitCode = 1
              Stdout = ""
              Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed"
              CompileTime = compileTime
              RuntimeTime = TimeSpan.Zero }
        else
            let runtimeTimer = Stopwatch.StartNew()
            let execResult = execute verbosity compileResult.Binary
            runtimeTimer.Stop()
            { ExitCode = execResult.ExitCode
              Stdout = execResult.Stdout
              Stderr = execResult.Stderr
              CompileTime = compileTime
              RuntimeTime = runtimeTimer.Elapsed }

/// Compile and run source code with lazy stdlib (caches LIR compilation across tests)
let compileAndRunWithLazyStdlib (verbosity: int) (options: CompilerOptions) (stdlib: LazyStdlibResult) (source: string) : ExecutionResult =
    let compileResult = compileWithLazyStdlib verbosity options stdlib source

    if not compileResult.Success then
        { ExitCode = 1
          Stdout = ""
          Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
    else
        execute verbosity compileResult.Binary

/// Compile and run source code (main entry point for E2E tests)
let compileAndRun (verbosity: int) (source: string) : ExecutionResult =
    compileAndRunWithOptions verbosity defaultOptions source

/// Get the set of stdlib function names reachable from user code
/// Used for coverage analysis without full compilation
let getReachableStdlibFunctions (stdlib: LazyStdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match Parser.parseString source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv userAst with
        | Error typeErr -> Error $"Type error: {TypeChecking.typeErrorToString typeErr}"
        | Ok (programType, typedUserAst, _) ->
            // Convert to ANF
            match AST_to_ANF.convertUserOnlyCached stdlib.SpecCache stdlib.ANFFuncCache "" Map.empty stdlib.GenericFuncDefs stdlib.ANFResult typedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                // Create ConversionResult for RC insertion
                let userConvResult : AST_to_ANF.ConversionResult = {
                    Program = ANF.Program (userOnly.UserFunctions, userOnly.MainExpr)
                    TypeReg = userOnly.TypeReg
                    VariantLookup = userOnly.VariantLookup
                    FuncReg = userOnly.FuncReg
                    FuncParams = userOnly.FuncParams
                    ModuleRegistry = userOnly.ModuleRegistry
                }
                // RC insertion to get full ANF
                match RefCountInsertion.insertRCInProgram userConvResult with
                | Error err -> Error $"RC insertion error: {err}"
                | Ok (userAnfAfterRC, _typeMap) ->
                    // Tail call detection
                    let userAnfAfterTCO = TailCallDetection.detectTailCallsInProgram userAnfAfterRC
                    // Print insertion
                    let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterTCO
                    let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                    // Extract reachable stdlib functions using DCE infrastructure
                    let (ANF.Program (userFuncsForDCE, userMainForDCE)) = userAnfProgram
                    let userANFFuncs = { ANF.Name = "_start"; ANF.TypedParams = []; ANF.ReturnType = AST.TUnit; ANF.Body = userMainForDCE } :: userFuncsForDCE
                    let reachableStdlibNames = ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph userANFFuncs
                    Ok reachableStdlibNames

/// Get all stdlib function names from the lazy stdlib
let getAllStdlibFunctionNames (stdlib: LazyStdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get all stdlib function names from the pre-compiled stdlib
let getAllStdlibFunctionNamesFromStdlib (stdlib: StdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get the set of stdlib function names reachable from user code (using pre-compiled stdlib)
/// Used for coverage analysis without re-compiling stdlib
let getReachableStdlibFunctionsFromStdlib (stdlib: StdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match Parser.parseString source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkProgramWithBaseEnv stdlib.TypeCheckEnv userAst with
        | Error typeErr -> Error $"Type error: {TypeChecking.typeErrorToString typeErr}"
        | Ok (programType, typedUserAst, _) ->
            // Convert to ANF
            match AST_to_ANF.convertUserOnlyCached stdlib.SpecCache stdlib.ANFFuncCache "" Map.empty stdlib.GenericFuncDefs stdlib.ANFResult typedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                // Create ConversionResult for RC insertion
                let userConvResult : AST_to_ANF.ConversionResult = {
                    Program = ANF.Program (userOnly.UserFunctions, userOnly.MainExpr)
                    TypeReg = userOnly.TypeReg
                    VariantLookup = userOnly.VariantLookup
                    FuncReg = userOnly.FuncReg
                    FuncParams = userOnly.FuncParams
                    ModuleRegistry = userOnly.ModuleRegistry
                }
                // RC insertion to get full ANF
                match RefCountInsertion.insertRCInProgram userConvResult with
                | Error err -> Error $"RC insertion error: {err}"
                | Ok (userAnfAfterRC, _typeMap) ->
                    // Tail call detection
                    let userAnfAfterTCO = TailCallDetection.detectTailCallsInProgram userAnfAfterRC
                    // Print insertion
                    let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterTCO
                    let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                    // Extract reachable stdlib functions using DCE infrastructure
                    let (ANF.Program (userFuncsForDCE, userMainForDCE)) = userAnfProgram
                    let userANFFuncs = { ANF.Name = "_start"; ANF.TypedParams = []; ANF.ReturnType = AST.TUnit; ANF.Body = userMainForDCE } :: userFuncsForDCE
                    let reachableStdlibNames = ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph userANFFuncs
                    Ok reachableStdlibNames
