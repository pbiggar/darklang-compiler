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
}

/// Cache for specialized (monomorphized) functions - thread-safe for parallel test execution
/// Key: (funcName, typeArgs as strings), Value: specialized AST.FunctionDef
type SpecializationCache = ConcurrentDictionary<string * string list, AST.FunctionDef>

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
}

/// Cache for lazily compiled LIR functions - thread-safe for parallel test execution
type LIRCache = ConcurrentDictionary<string, LIR.Function>

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
                    // Convert stdlib ANF to MIR (functions only, no _start)
                    // Use FuncParams for typeReg (needed for tail call argument types)
                    // Coverage is false here since stdlib is precompiled for caching
                    match ANF_to_MIR.toMIRFunctionsOnly anfAfterTCO typeMap anfResult.FuncParams anfResult.VariantLookup Map.empty false with
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
                    match ANF_to_MIR.toMIRFunctionsOnly stdlibAnfProgram typeMap stdlibTypeReg stdlibVariantLookup Map.empty false with
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
                    if verbosity >= 3 then
                        println "=== ANF (before optimization) ==="
                        let (ANF.Program (funcs, mainExprDbg)) = convResult.Program
                        for func in funcs do
                            println $"Function: {func.Name}"
                            println $"  Params: {func.Params}"
                            println $"  Body: {func.Body}"
                            println ""
                        println $"Main: {mainExprDbg}"
                        println ""

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
                    if verbosity >= 3 then
                        println "=== ANF (after optimization) ==="
                        let (ANF.Program (funcs, mainExprDbg)) = anfOptimized
                        for func in funcs do
                            println $"Function: {func.Name}"
                            println $"  Params: {func.Params}"
                            println $"  Body: {func.Body}"
                            println ""
                        println $"Main: {mainExprDbg}"
                        println ""

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
                    if verbosity >= 3 then
                        println "=== ANF (after RC insertion) ==="
                        let (ANF.Program (funcs, mainExprDbg)) = anfAfterRC
                        for func in funcs do
                            println $"Function: {func.Name}"
                            println $"  Params: {func.Params}"
                            println $"  Body: {func.Body}"
                            println ""
                        println $"Main: {mainExprDbg}"
                        println ""

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
                    if verbosity >= 3 then
                        println "=== ANF (after Tail Call Detection) ==="
                        let (ANF.Program (funcs, mainExprDbg)) = anfAfterTCO
                        for func in funcs do
                            println $"Function: {func.Name}"
                            println $"  Params: {func.Params}"
                            println $"  Body: {func.Body}"
                            println ""
                        println $"Main: {mainExprDbg}"
                        println ""

                    // Pass 2.8: Print Insertion (for main expression)
                    if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                    let (ANF.Program (functions, mainExpr)) = anfAfterTCO
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType
                    let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - tcoTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(printTime, 1)
                        println $"        {t}ms"

                    // Show ANF after Print insertion
                    if verbosity >= 3 then
                        println "=== ANF (after Print insertion) ==="
                        let (ANF.Program (funcs, mainExprDbg)) = anfProgram
                        for func in funcs do
                            println $"Function: {func.Name}"
                            println $"  Params: {func.Params}"
                            println $"  Body: {func.Body}"
                            println ""
                        println $"Main: {mainExprDbg}"
                        println ""

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
                    let mirResult = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0) typeMap funcParams programType convResultOptimized.VariantLookup convResultOptimized.TypeReg options.EnableCoverage

                    match mirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"MIR conversion error: {err}" }
                    | Ok (mirProgram, _) ->

                    // Show MIR
                    if verbosity >= 3 then
                        let (MIR.Program (functions, _, _, _, _)) = mirProgram
                        println "=== MIR (Control Flow Graph) ==="
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
                    if verbosity >= 3 then
                        let (LIR.Program (funcs, _, _)) = lirProgram
                        println "=== LIR (Low-level IR with CFG) ==="
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
                    if verbosity >= 3 then
                        println "=== LIR (After Register Allocation) ==="
                        for allocatedFunc in allocatedFuncs do
                            println $"Function: {allocatedFunc.Name}"
                            println $"Entry: {allocatedFunc.CFG.Entry}"
                            for kvp in allocatedFunc.CFG.Blocks do
                                let block = kvp.Value
                                println $"\n{block.Label}:"
                                for instr in block.Instrs do
                                    println $"  {instr}"
                                println $"  {block.Terminator}"
                        println ""

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
let compileWithStdlib (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (source: string) : CompileResult =
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
                    let anfOptimized =
                        if options.DisableANFOpt then userProgram
                        else ANF_Optimize.optimizeProgram userProgram
                    let anfOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(anfOptTime, 1)
                        println $"        {t}ms"

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

                    // Pass 2.7: Tail Call Detection
                    if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
                    let userAnfAfterTCO =
                        if options.DisableTCO then userAnfAfterRC
                        else TailCallDetection.detectTailCallsInProgram userAnfAfterRC
                    let tcoTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(tcoTime, 1)
                        println $"        {t}ms"

                    // Pass 2.8: Print Insertion (user code only)
                    if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                    let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterTCO
                    let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                    let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - tcoTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(printTime, 1)
                        println $"        {t}ms"

                    // Pass 3: ANF → MIR (user code only)
                    if verbosity >= 1 then println "  [3/8] ANF → MIR (user only)..."
                    // Use FuncParams (maps function names to param types) for correct float param handling
                    // Use TypeReg from ConversionResult for record type definitions (for record printing)
                    // typeMap contains TempId -> Type mappings from RC insertion
                    let userMirResult = ANF_to_MIR.toMIR userAnfProgram (MIR.RegGen 0) typeMap userOnly.FuncParams programType userConvResult.VariantLookup userConvResult.TypeReg options.EnableCoverage

                    match userMirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"MIR conversion error: {err}" }
                    | Ok (userMirProgram, _) ->

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

/// Lazily compile a stdlib function from MIR to LIR with caching
/// Returns the compiled LIR function (from cache if available, or compiles and caches)
let private getOrCompileStdlibLIR (stdlib: LazyStdlibResult) (funcName: string) : LIR.Function option =
    // Check cache first
    match stdlib.LIRCache.TryGetValue(funcName) with
    | true, cached -> Some cached
    | false, _ ->
        // Not in cache - compile from MIR
        match Map.tryFind funcName stdlib.StdlibMIRFunctions with
        | None -> None  // Function not found in stdlib
        | Some mirFunc ->
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
                | [lirFunc] ->
                    let allocated = RegisterAllocation.allocateRegisters lirFunc
                    // Cache the result (thread-safe: if another thread beat us, that's fine)
                    stdlib.LIRCache.TryAdd(funcName, allocated) |> ignore
                    Some allocated
                | _ -> None  // Unexpected: should have exactly one function

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
                    let anfOptimized =
                        if options.DisableANFOpt then userProgram
                        else ANF_Optimize.optimizeProgram userProgram
                    let anfOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(anfOptTime, 1)
                        println $"        {t}ms"

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

                    // Pass 2.7: Tail Call Detection
                    if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
                    let userAnfAfterTCO =
                        if options.DisableTCO then userAnfAfterRC
                        else TailCallDetection.detectTailCallsInProgram userAnfAfterRC
                    let tcoTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(tcoTime, 1)
                        println $"        {t}ms"

                    // Pass 2.8: Print Insertion (user code only)
                    if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
                    let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterTCO
                    let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                    let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - tcoTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(printTime, 1)
                        println $"        {t}ms"

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
                            let userANFFuncs = { ANF.Name = "_start"; ANF.Params = []; ANF.Body = userMainForDCE } :: userFuncsForDCE
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
                    let userMirResult = ANF_to_MIR.toMIR userAnfProgram (MIR.RegGen 0) typeMap userOnly.FuncParams programType userConvResult.VariantLookup userConvResult.TypeReg options.EnableCoverage
                    let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime - rcTime - printTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(mirTime, 1)
                        println $"        {t}ms"

                    match userMirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"MIR conversion error: {err}" }
                    | Ok (userMirProgram, _) ->

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
    let compileResult = compileWithStdlib verbosity options stdlib source

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
            match AST_to_ANF.convertUserOnlyCached stdlib.SpecCache stdlib.GenericFuncDefs stdlib.ANFResult typedUserAst with
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
                    let userANFFuncs = { ANF.Name = "_start"; ANF.Params = []; ANF.Body = userMainForDCE } :: userFuncsForDCE
                    let reachableStdlibNames = ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph userANFFuncs
                    Ok reachableStdlibNames

/// Get all stdlib function names from the lazy stdlib
let getAllStdlibFunctionNames (stdlib: LazyStdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq
