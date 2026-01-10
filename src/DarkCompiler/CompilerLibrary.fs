// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open CodeGen

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
    let (MIR.Program (functions, _, _)) = program
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

/// Print symbolic LIR program (with CFG) in a consistent format
let printLIRSymbolicProgram (title: string) (program: LIRSymbolic.Program) : unit =
    let (LIRSymbolic.Program funcs) = program
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

/// Compute the platform-specific code file offset for encoding
let private computeCodeFileOffset (os: Platform.OS) (stringPool: MIR.StringPool) : int =
    match os with
    | Platform.Linux ->
        // ELF: header (64) + 1 program header (56) = 120
        64 + 56
    | Platform.MacOS ->
        // Mach-O: header (32) + load commands + padding
        // Must match 8_Binary_Generation_MachO.fs calculation
        let headerSize = 32
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
        (headerSize + commandsSize + 200 + 7) &&& (~~~7)

/// Run MIR+LIR passes from ANF, returning an optimized LIR program
let private compileAnfToLir
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (stageSuffix: string)
    (anfProgram: ANF.Program)
    (typeMap: ANF.TypeMap)
    (convResult: AST_to_ANF.ConversionResult)
    (programType: AST.Type)
    (externalReturnTypes: Map<string, AST.Type>)
    : Result<LIRSymbolic.Program, string> =

    let suffix = if stageSuffix = "" then "" else $" ({stageSuffix})"

    if verbosity >= 1 then println $"  [3/8] ANF → MIR{suffix}..."
    let mirStart = sw.Elapsed.TotalMilliseconds
    let mirResult =
        ANF_to_MIR.toMIR
            anfProgram
            typeMap
            convResult.FuncParams
            programType
            convResult.VariantLookup
            convResult.TypeReg
            options.EnableCoverage
            externalReturnTypes
    match mirResult with
    | Error err -> Error $"MIR conversion error: {err}"
    | Ok mirProgram ->
        if shouldDumpIR verbosity options.DumpMIR then
            printMIRProgram "=== MIR (Control Flow Graph) ===" mirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - mirStart, 1)
            println $"        {t}ms"

        if verbosity >= 1 then println $"  [3.1/8] SSA Construction{suffix}..."
        let ssaStart = sw.Elapsed.TotalMilliseconds
        let ssaProgram = SSA_Construction.convertToSSA mirProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - ssaStart, 1)
            println $"        {t}ms"

        if verbosity >= 1 then println $"  [3.5/8] MIR Optimizations{suffix}..."
        let mirOptStart = sw.Elapsed.TotalMilliseconds
        let optimizedProgram =
            if options.DisableMIROpt then ssaProgram
            else MIR_Optimize.optimizeProgram ssaProgram
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - mirOptStart, 1)
            println $"        {t}ms"

        if verbosity >= 1 then println $"  [4/8] MIR → LIR{suffix}..."
        let lirStart = sw.Elapsed.TotalMilliseconds
        let lirResult = MIR_to_LIR.toLIR optimizedProgram
        match lirResult with
        | Error err -> Error $"LIR conversion error: {err}"
        | Ok lirProgram ->
            if shouldDumpIR verbosity options.DumpLIR then
                printLIRSymbolicProgram "=== LIR (Low-level IR with CFG) ===" lirProgram
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - lirStart, 1)
                println $"        {t}ms"

            if verbosity >= 1 then println $"  [4.5/8] LIR Optimizations{suffix}..."
            let lirOptStart = sw.Elapsed.TotalMilliseconds
            let optimizedLir =
                if options.DisableLIROpt then lirProgram
                else LIR_Optimize.optimizeProgram lirProgram
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - lirOptStart, 1)
                println $"        {t}ms"
            Ok optimizedLir

/// Run ANF optimization + RC/TCO/print insertion, returning a final ANF program and type map
let private buildAnfProgram
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (programType: AST.Type)
    (userFunctions: ANF.Function list)
    (mainExpr: ANF.AExpr)
    (userOnly: AST_to_ANF.UserOnlyResult)
    (extraFunctions: ANF.Function list)
    (extraTypeMap: ANF.TypeMap)
    : Result<ANF.Program * ANF.TypeMap * AST_to_ANF.ConversionResult, string> =

    if verbosity >= 1 then println "  [2.3/8] ANF Optimization..."
    let anfProgram = ANF.Program (userFunctions, mainExpr)
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (before optimization) ===" anfProgram
    let anfOptStart = sw.Elapsed.TotalMilliseconds
    let anfOptimized =
        if options.DisableANFOpt then anfProgram
        else ANF_Optimize.optimizeProgram anfProgram
    if verbosity >= 2 then
        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - anfOptStart, 1)
        println $"        {t}ms"
    if shouldDumpIR verbosity options.DumpANF then
        printANFProgram "=== ANF (after optimization) ===" anfOptimized

    if verbosity >= 1 then println "  [2.4/8] ANF Inlining..."
    let inlineStart = sw.Elapsed.TotalMilliseconds
    let anfInlined =
        if options.DisableInlining then anfOptimized
        else ANF_Inlining.inlineProgramDefault anfOptimized
    if verbosity >= 2 then
        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - inlineStart, 1)
        println $"        {t}ms"

    let convResult : AST_to_ANF.ConversionResult = {
        Program = anfInlined
        TypeReg = userOnly.TypeReg
        VariantLookup = userOnly.VariantLookup
        FuncReg = userOnly.FuncReg
        FuncParams = userOnly.FuncParams
        ModuleRegistry = userOnly.ModuleRegistry
    }

    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
    let rcStart = sw.Elapsed.TotalMilliseconds
    match RefCountInsertion.insertRCInProgram convResult with
    | Error err -> Error $"Reference count insertion error: {err}"
    | Ok (anfAfterRC, typeMap) ->
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - rcStart, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after RC insertion) ===" anfAfterRC

        if verbosity >= 1 then println "  [2.7/8] Tail Call Detection..."
        let tcoStart = sw.Elapsed.TotalMilliseconds
        let anfAfterTCO =
            if options.DisableTCO then anfAfterRC
            else TailCallDetection.detectTailCallsInProgram anfAfterRC
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - tcoStart, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after Tail Call Detection) ===" anfAfterTCO

        let (ANF.Program (postTcoFunctions, postTcoMain)) = anfAfterTCO
        let mergedFunctions = extraFunctions @ postTcoFunctions
        let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) extraTypeMap typeMap

        if verbosity >= 1 then println "  [2.8/8] Print Insertion..."
        let printStart = sw.Elapsed.TotalMilliseconds
        let mergedProgram =
            PrintInsertion.insertPrint mergedFunctions postTcoMain programType
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - printStart, 1)
            println $"        {t}ms"
        if shouldDumpIR verbosity options.DumpANF then
            printANFProgram "=== ANF (after Print insertion) ===" mergedProgram
        Ok (mergedProgram, mergedTypeMap, convResult)

/// Run codegen, encoding, and binary generation
let private generateBinary
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (codegenLabel: string)
    (encodeLabel: string)
    (binaryLabel: string)
    (dumpAsm: bool)
    (dumpMachineCode: bool)
    (allocatedProgram: LIR.Program)
    : Result<byte array, string> =

    if verbosity >= 1 then println codegenLabel
    let codegenStart = sw.Elapsed.TotalMilliseconds
    let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
    let codegenOptions : CodeGen.CodeGenOptions = {
        DisableFreeList = options.DisableFreeList
        EnableCoverage = options.EnableCoverage
        CoverageExprCount = coverageExprCount
    }
    let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
    match codegenResult with
    | Error err -> Error $"Code generation error: {err}"
    | Ok arm64Instructions ->
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - codegenStart, 1)
            println $"        {t}ms"

        if dumpAsm && verbosity >= 3 then
            println "=== ARM64 Assembly Instructions ==="
            for (i, instr) in List.indexed arm64Instructions do
                println $"  {i}: {instr}"
            println ""

        match Platform.detectOS () with
        | Error err -> Error $"Platform detection error: {err}"
        | Ok os ->
            if verbosity >= 1 then println encodeLabel
            let encodeStart = sw.Elapsed.TotalMilliseconds
            let (LIR.Program (_, stringPool, floatPool)) = allocatedProgram
            let codeFileOffset = computeCodeFileOffset os stringPool
            let machineCode =
                ARM64_Encoding.encodeAllWithPools arm64Instructions stringPool floatPool codeFileOffset
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - encodeStart, 1)
                println $"        {t}ms"

            if dumpMachineCode && verbosity >= 3 then
                println "=== Machine Code (hex) ==="
                for i in 0 .. 4 .. (machineCode.Length - 1) do
                    if i + 3 < machineCode.Length then
                        let bytes = sprintf "%02x %02x %02x %02x" machineCode.[i] machineCode.[i+1] machineCode.[i+2] machineCode.[i+3]
                        println $"  {i:X4}: {bytes}"
                println $"Total: {machineCode.Length} bytes\n"

            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
            if verbosity >= 1 then println (binaryLabel.Replace("{format}", formatName))
            let binaryStart = sw.Elapsed.TotalMilliseconds
            let binary =
                match os with
                | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool
                | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - binaryStart, 1)
                println $"        {t}ms"

            Ok binary


/// Cache for specialized (monomorphized) functions - thread-safe for parallel test execution
/// Key: (funcName, typeArgs as strings), Value: specialized AST.FunctionDef (lazy for concurrency)
type SpecializationCache = ConcurrentDictionary<string * string list, Lazy<AST.FunctionDef>>

/// Cached fully-compiled user function (after all passes through register allocation)
/// Stored in symbolic form so pool indices can be resolved late.
type CachedUserFunction = {
    /// The compiled function in symbolic LIR form
    SymbolicFunction: LIRSymbolic.Function
}

/// Unique key for a compiled function entry
type CompiledFunctionKey =
    | Stdlib of name:string
    | Preamble of sourceFile:string * name:string
    | User of sourceFile:string * lineNumber:int * name:string

/// Cache for compiled functions - avoids recompiling same function across tests
/// Key: CompiledFunctionKey, Value: Lazy compiled entry
type CompiledFunctionCache = ConcurrentDictionary<CompiledFunctionKey, Lazy<Result<CachedUserFunction, string>>>

/// Create a new compiled function cache
let createCompiledFunctionCache () : CompiledFunctionCache =
    CompiledFunctionCache()

/// Get or add a compiled function entry, ensuring the build runs at most once per key
let getOrAddCompiledFunctionLazy
    (cache: CompiledFunctionCache)
    (key: CompiledFunctionKey)
    (build: unit -> Result<CachedUserFunction, string>)
    : Lazy<Result<CachedUserFunction, string>> =
    cache.GetOrAdd(
        key,
        fun _ ->
            Lazy<Result<CachedUserFunction, string>>(
                build,
                System.Threading.LazyThreadSafetyMode.ExecutionAndPublication))

/// Try to get a compiled function entry from the cache
let private tryGetCompiledFunction
    (cache: CompiledFunctionCache)
    (key: CompiledFunctionKey)
    : Result<CachedUserFunction option, string> =
    match cache.TryGetValue key with
    | true, lazyEntry ->
        match lazyEntry.Value with
        | Ok entry -> Ok (Some entry)
        | Error err -> Error err
    | false, _ -> Ok None

/// Cache for codegen output (ARM64 instruction lists) per function
/// Key: (function name, disableFreeList flag)
type CodegenCache = ConcurrentDictionary<LIR.Function * bool, Lazy<Result<ARM64.Instr list, string>>>

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
    /// Preamble's symbolic LIR functions after register allocation
    SymbolicFunctions: LIRSymbolic.Function list
    /// Names of specialized stdlib functions needed by the preamble
    StdlibSpecializedNames: Set<string>
}

/// Cache for compiled preambles - thread-safe for parallel test execution
/// Key: (sourceFile, preambleHash), Value: compiled PreambleContext
type PreambleCache = ConcurrentDictionary<string * int, Lazy<Result<PreambleContext, string>>>

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
    /// Cache for compiled functions (shared across test compilations)
    CompiledFuncCache: CompiledFunctionCache
    /// Cache for ANF-level user functions (shared across test compilations)
    ANFFuncCache: ANFFunctionCache
    /// Cache for compiled preambles (shared across test compilations)
    PreambleCache: PreambleCache
    /// Cache for codegen output (shared across test compilations)
    CodegenCache: CodegenCache
}

/// Generate binary with codegen caching for selected functions
let private generateBinaryWithCodegenCache
    (verbosity: int)
    (options: CompilerOptions)
    (sw: Stopwatch)
    (codegenLabel: string)
    (encodeLabel: string)
    (binaryLabel: string)
    (dumpAsm: bool)
    (dumpMachineCode: bool)
    (codegenCache: CodegenCache)
    (cacheableNames: Set<string>)
    (allocatedProgram: LIR.Program)
    : Result<byte array, string> =

    let (LIR.Program (functions, stringPool, _floatPool)) = allocatedProgram
    if verbosity >= 1 then println codegenLabel
    let codegenStart = sw.Elapsed.TotalMilliseconds
    let coverageExprCount = if options.EnableCoverage then LIR.countCoverageHits allocatedProgram else 0
    let codegenOptions : CodeGen.CodeGenOptions = {
        DisableFreeList = options.DisableFreeList
        EnableCoverage = options.EnableCoverage
        CoverageExprCount = coverageExprCount
    }
    let ctx : CodeGenContext = {
        Options = codegenOptions
        StringPool = stringPool
        StackSize = 0
        UsedCalleeSaved = []
    }
    let sortedFunctions : LIR.Function list =
        match List.tryFind (fun (f: LIR.Function) -> f.Name = "_start") functions with
        | Some startFunc ->
            let otherFuncs = List.filter (fun (f: LIR.Function) -> f.Name <> "_start") functions
            startFunc :: otherFuncs
        | None -> functions
    let rec buildInstrs
        (acc: ARM64.Instr list list)
        (remaining: LIR.Function list)
        : Result<ARM64.Instr list, string> =
        match remaining with
        | [] -> Ok (acc |> List.rev |> List.collect id |> CodeGen.peepholeOptimize)
        | func :: rest ->
            if func.Name <> "_start" && cacheableNames.Contains func.Name && not options.EnableCoverage then
                let key = (func, options.DisableFreeList)
                let lazyResult =
                    codegenCache.GetOrAdd(
                        key,
                        fun _ ->
                            Lazy<Result<ARM64.Instr list, string>>(
                                (fun () ->
                                    match CodeGen.convertFunction ctx func with
                                    | Error err -> Error err
                                    | Ok instrs -> Ok instrs),
                                System.Threading.LazyThreadSafetyMode.ExecutionAndPublication))
                match lazyResult.Value with
                | Error err -> Error err
                | Ok newInstrs -> buildInstrs (newInstrs :: acc) rest
            else
                match CodeGen.convertFunction ctx func with
                | Error err -> Error err
                | Ok newInstrs -> buildInstrs (newInstrs :: acc) rest

    let codegenResult = buildInstrs [] sortedFunctions
    match codegenResult with
    | Error err -> Error $"Code generation error: {err}"
    | Ok arm64Instructions ->
        if verbosity >= 2 then
            let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - codegenStart, 1)
            println $"        {t}ms"

        if dumpAsm && verbosity >= 3 then
            println "=== ARM64 Assembly Instructions ==="
            for (i, instr) in List.indexed arm64Instructions do
                println $"  {i}: {instr}"
            println ""

        match Platform.detectOS () with
        | Error err -> Error $"Platform detection error: {err}"
        | Ok os ->
            if verbosity >= 1 then println encodeLabel
            let encodeStart = sw.Elapsed.TotalMilliseconds
            let (LIR.Program (_, stringPool, floatPool)) = allocatedProgram
            let codeFileOffset = computeCodeFileOffset os stringPool
            let machineCode =
                ARM64_Encoding.encodeAllWithPools arm64Instructions stringPool floatPool codeFileOffset
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - encodeStart, 1)
                println $"        {t}ms"

            if dumpMachineCode && verbosity >= 3 then
                println "=== Machine Code (hex) ==="
                for i in 0 .. 4 .. (machineCode.Length - 1) do
                    if i + 3 < machineCode.Length then
                        let bytes = sprintf "%02x %02x %02x %02x" machineCode.[i] machineCode.[i+1] machineCode.[i+2] machineCode.[i+3]
                        println $"  {i:X4}: {bytes}"
                println $"Total: {machineCode.Length} bytes\n"

            let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
            if verbosity >= 1 then println (binaryLabel.Replace("{format}", formatName))
            let binaryStart = sw.Elapsed.TotalMilliseconds
            let binary =
                match os with
                | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool
                | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool
            if verbosity >= 2 then
                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - binaryStart, 1)
                println $"        {t}ms"

            Ok binary

/// Cache for lazily compiled LIR functions - thread-safe for parallel test execution
/// Uses Lazy<T> to ensure each function is compiled exactly once even under contention
type LIRCache = ConcurrentDictionary<string, Lazy<LIRSymbolic.Function option>>

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
}

/// Helper: map a function returning Result over a list, returning Error on first failure
let private mapResults (f: 'a -> Result<'b, string>) (items: 'a list) : Result<'b list, string> =
    let rec loop acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc)
        | item :: rest ->
            match f item with
            | Error err -> Error err
            | Ok result -> loop (result :: acc) rest
    loop [] items

/// Cache a symbolic function using the shared compiled function cache
let private cacheSymbolicFunction
    (stdlib: StdlibResult)
    (cacheKey: CompiledFunctionKey)
    (func: LIRSymbolic.Function)
    : Result<unit, string> =
    let build () : Result<CachedUserFunction, string> =
        Ok { SymbolicFunction = func }
    let lazyEntry = getOrAddCompiledFunctionLazy stdlib.CompiledFuncCache cacheKey build
    match lazyEntry.Value with
    | Ok _ -> Ok ()
    | Error err -> Error err

/// Cache a specialized function using the shared compiled function cache
let private cacheSpecializedFunction (stdlib: StdlibResult) (func: LIRSymbolic.Function) : Result<unit, string> =
    cacheSymbolicFunction stdlib (CompiledFunctionKey.Stdlib func.Name) func

/// Cache a preamble function using the shared compiled function cache
let private cachePreambleFunction
    (stdlib: StdlibResult)
    (sourceFile: string)
    (func: LIRSymbolic.Function)
    : Result<unit, string> =
    cacheSymbolicFunction stdlib (CompiledFunctionKey.Preamble (sourceFile, func.Name)) func

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
                    | Ok (mirFuncs, variantRegistry, recordRegistry) ->
                        // Wrap in MIR.Program for LIR conversion
                        let mirProgram = MIR.Program (mirFuncs, variantRegistry, recordRegistry)
                        // SSA construction + MIR optimizations for stdlib (keeps RA assumptions consistent)
                        let stdlibSsaProgram =
                            SSA_Construction.convertToSSA mirProgram
                        let stdlibOptimizedProgram =
                            MIR_Optimize.optimizeProgram stdlibSsaProgram
                        // Convert stdlib MIR to LIR (cached for reuse)
                        match MIR_to_LIR.toLIR stdlibOptimizedProgram with
                        | Error e -> Error e
                        | Ok lirProgram ->
                            let optimizedLir = LIR_Optimize.optimizeProgram lirProgram
                            // Pre-allocate stdlib functions (cached for reuse)
                            let (LIRSymbolic.Program lirFuncs) = optimizedLir
                            let allocatedFuncs = lirFuncs |> List.map RegisterAllocation.allocateRegisters
                            let allocatedSymbolic = LIRSymbolic.Program allocatedFuncs
                            match LIRSymbolic.toLIR allocatedSymbolic with
                            | Error err -> Error err
                            | Ok allocatedProgram ->
                                let (LIR.Program (resolvedFuncs, _, _)) = allocatedProgram
                                // Build call graph for dead code elimination
                                let stdlibCallGraph = DeadCodeElimination.buildCallGraph resolvedFuncs
                                Ok {
                                    AST = stdlibAst
                                    TypedAST = typedStdlib
                                    TypeCheckEnv = typeCheckEnv
                                    ANFResult = anfResult
                                    GenericFuncDefs = genericFuncDefs
                                    ModuleRegistry = moduleRegistry
                                    MIRProgram = mirProgram
                                    LIRProgram = allocatedProgram
                                    AllocatedFunctions = resolvedFuncs
                                    StdlibCallGraph = stdlibCallGraph
                                    SpecCache = SpecializationCache()
                                    StdlibANFFunctions = stdlibFuncMap
                                    StdlibANFCallGraph = stdlibANFCallGraph
                                    StdlibTypeMap = typeMap
                                    CompiledFuncCache = createCompiledFunctionCache ()
                                    ANFFuncCache = ANFFunctionCache()
                                    PreambleCache = PreambleCache()
                                    CodegenCache = CodegenCache()
                                }

/// Prepare stdlib for lazy compilation - stops at MIR level
/// LIR conversion and register allocation are done lazily when functions are needed
let private prepareStdlibForLazyCompile () : Result<LazyStdlibResult, string> =
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

                    // Convert ALL stdlib to MIR (LIR conversion is deferred until needed)
                    let stdlibAnfProgram = ANF.Program (stdlibFuncs, ANF.Return ANF.UnitLiteral)
                    let stdlibTypeReg = anfResult.FuncParams
                    let stdlibVariantLookup = anfResult.VariantLookup

                    // Convert stdlib to MIR
                    let stdlibExternalReturnTypes = extractReturnTypes anfResult.FuncReg
                    match ANF_to_MIR.toMIRFunctionsOnly stdlibAnfProgram typeMap stdlibTypeReg stdlibVariantLookup Map.empty false stdlibExternalReturnTypes with
                    | Error e -> Error $"Stdlib MIR error: {e}"
                    | Ok (stdlibMirFuncs, stdlibVariants, stdlibRecords) ->
                        // SSA Construction
                        let stdlibMirProgram = MIR.Program (stdlibMirFuncs, stdlibVariants, stdlibRecords)
                        let stdlibSsaProgram = SSA_Construction.convertToSSA stdlibMirProgram

                        // MIR Optimizations
                        let stdlibOptimized = MIR_Optimize.optimizeProgram stdlibSsaProgram

                        // Extract optimized MIR functions into a map for lazy LIR compilation
                        let (MIR.Program (optimizedMirFuncs, _, _)) = stdlibOptimized
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

                    // Pass 3-4.5: ANF → MIR → LIR (shared pipeline)
                    // Build func params from function defs (for float param handling)
                    let funcParams =
                        let (AST.Program topLevels) = transformedAst
                        topLevels
                        |> List.choose (function
                            | AST.FunctionDef funcDef -> Some (funcDef.Name, funcDef.Params)
                            | _ -> None)
                        |> Map.ofList
                    let convResultWithParams = { convResultOptimized with FuncParams = funcParams }
                    let externalReturnTypes = extractReturnTypes convResultWithParams.FuncReg
                    let lirResult =
                        compileAnfToLir
                            verbosity
                            options
                            sw
                            ""
                            anfProgram
                            typeMap
                            convResultWithParams
                            programType
                            externalReturnTypes
                    match lirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some err }
                    | Ok optimizedLirProgram ->
                    // Pass 5: Register Allocation (includes phi resolution)
                    if verbosity >= 1 then println "  [5/7] Register Allocation..."
                    let allocStart = sw.Elapsed.TotalMilliseconds
                    let (LIRSymbolic.Program funcs) = optimizedLirProgram
                    let allocatedFuncs = funcs |> List.map RegisterAllocation.allocateRegisters
                    let allocatedSymbolic = LIRSymbolic.Program allocatedFuncs

                    if shouldDumpIR verbosity options.DumpLIR then
                        printLIRSymbolicProgram "=== LIR (After Register Allocation) ===" allocatedSymbolic

                    if verbosity >= 2 then
                        let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - allocStart, 1)
                        println $"        {t}ms"

                    match LIRSymbolic.toLIR allocatedSymbolic with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"LIR pool resolution error: {err}" }
                    | Ok allocatedProgram ->
                        let binaryResult =
                            generateBinary
                                verbosity
                                options
                                sw
                                "  [6/7] Code Generation..."
                                "  [7/7] ARM64 Encoding..."
                                "  [7/7] Binary Generation ({format})..."
                                true
                                true
                                allocatedProgram
                        match binaryResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some err }
                        | Ok binary ->
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
            SymbolicFunctions = []
            StdlibSpecializedNames = Set.empty
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

                        let preambleExternalReturnTypes = extractReturnTypes preambleConvResult.FuncReg
                        match ANF_to_MIR.toMIRFunctionsOnly preambleAnfAfterTCO typeMap preambleConvResult.FuncParams preambleConvResult.VariantLookup Map.empty false preambleExternalReturnTypes with
                        | Error err -> Error $"Preamble MIR conversion error: {err}"
                        | Ok (mirFuncs, variantRegistry, recordRegistry) ->
                            let mirProgram = MIR.Program (mirFuncs, variantRegistry, recordRegistry)
                            let ssaProgram = SSA_Construction.convertToSSA mirProgram
                            let optimizedProgram = MIR_Optimize.optimizeProgram ssaProgram
                            match MIR_to_LIR.toLIR optimizedProgram with
                            | Error err -> Error $"Preamble LIR conversion error: {err}"
                            | Ok lirProgram ->
                                let optimizedLir = LIR_Optimize.optimizeProgram lirProgram
                                let (LIRSymbolic.Program lirFuncs) = optimizedLir
                                let isStdlibSpecialized (name: string) : bool =
                                    name.StartsWith("Stdlib.") || name.StartsWith("__")
                                let allocatedFuncs: LIRSymbolic.Function list =
                                    lirFuncs |> List.map RegisterAllocation.allocateRegisters
                                let preambleOnlyFuncs =
                                    allocatedFuncs
                                    |> List.filter (fun func -> not (isStdlibSpecialized func.Name))
                                let allocatedSymbolic = LIRSymbolic.Program allocatedFuncs
                                let preambleSymbolicFuncs = preambleOnlyFuncs

                                let rec cachePreambleFuncs (remaining: LIRSymbolic.Function list) : Result<unit, string> =
                                    match remaining with
                                    | [] -> Ok ()
                                    | func :: rest ->
                                        if isStdlibSpecialized func.Name then
                                            match cacheSpecializedFunction stdlib func with
                                            | Error err -> Error $"Cached preamble function {func.Name}: {err}"
                                            | Ok () -> cachePreambleFuncs rest
                                        else
                                            cachePreambleFuncs rest

                                let rec cachePreambleUserFuncs (remaining: LIRSymbolic.Function list) : Result<unit, string> =
                                    match remaining with
                                    | [] -> Ok ()
                                    | func :: rest ->
                                        match cachePreambleFunction stdlib sourceFile func with
                                        | Error err -> Error $"Cached preamble user function {func.Name}: {err}"
                                        | Ok () -> cachePreambleUserFuncs rest

                                match cachePreambleFuncs allocatedFuncs with
                                | Error err -> Error err
                                | Ok () ->
                                    match cachePreambleUserFuncs preambleSymbolicFuncs with
                                    | Error err -> Error err
                                    | Ok () ->

                                    // Merge TypeMaps (stdlib + preamble)
                                    let mergedTypeMap = Map.fold (fun acc k v -> Map.add k v acc) stdlib.StdlibTypeMap typeMap

                                    Ok {
                                        TypeCheckEnv = preambleTypeCheckEnv
                                        GenericFuncDefs = mergedGenericDefs
                                        ANFFunctions = preambleFunctions
                                        TypeMap = mergedTypeMap
                                        ANFResult = preambleConvResult
                                        SymbolicFunctions = preambleSymbolicFuncs
                                        StdlibSpecializedNames = preambleUserOnly.SpecializedFuncNames
                                    }

/// Compile test expression with pre-compiled preamble context
/// Only the tiny test expression is parsed/compiled - preamble functions are merged in
let compileTestWithPreamble (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult)
                            (preambleCtx: PreambleContext) (sourceFile: string) (testExpr: string) : CompileResult =
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
                    let isStdlibSpecialized (name: string) : bool =
                        name.StartsWith("Stdlib.") || name.StartsWith("__")
                    let stdlibSpecializedNames =
                        testOnly.SpecializedFuncNames
                        |> Set.filter isStdlibSpecialized
                    let preambleSpecializedNames =
                        preambleCtx.StdlibSpecializedNames
                        |> Set.filter isStdlibSpecialized
                    let allStdlibSpecializedNames =
                        Set.union stdlibSpecializedNames preambleSpecializedNames
                    let preambleFuncs = preambleCtx.SymbolicFunctions
                    let preambleFuncNameSet =
                        preambleFuncs |> List.map (fun f -> f.Name) |> Set.ofList
                    let preambleCachedNameSet =
                        preambleFuncNameSet
                        |> Set.filter (fun name ->
                            stdlib.CompiledFuncCache.ContainsKey (CompiledFunctionKey.Preamble (sourceFile, name)))
                    let cachedFuncNameSet =
                        allStdlibSpecializedNames
                        |> Set.filter (fun name ->
                            stdlib.CompiledFuncCache.ContainsKey (CompiledFunctionKey.Stdlib name))
                    let functionsToCompile =
                        testOnly.UserFunctions
                        |> List.filter (fun f ->
                            not (Set.contains f.Name cachedFuncNameSet)
                            && not (Set.contains f.Name preambleFuncNameSet))
                    let cachedFuncNames : string list =
                        cachedFuncNameSet
                        |> Set.filter (fun name -> not (Set.contains name preambleFuncNameSet))
                        |> Set.toList

                    if verbosity >= 3 then
                        println $"  [COMPILE] {functionsToCompile.Length} user functions compiled fresh"
                        for f in functionsToCompile do
                            println $"    - {f.Name}"

                    // No cached functions, so no cached return types needed
                    let specializedNeedingCache =
                        allStdlibSpecializedNames
                        |> Set.filter (fun name -> not (Set.contains name cachedFuncNameSet))

                    let anfResult =
                        buildAnfProgram
                            verbosity
                            options
                            sw
                            programType
                            functionsToCompile
                            testOnly.MainExpr
                            testOnly
                            []
                            Map.empty
                    match anfResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some err }
                    | Ok (mergedAnfProgram, mergedTypeMap, testConvResult) ->
                        let allReturnTypes = extractReturnTypes testOnly.FuncReg
                        let userLirResult =
                            compileAnfToLir
                                verbosity
                                options
                                sw
                                ""
                                mergedAnfProgram
                                mergedTypeMap
                                testConvResult
                                programType
                                allReturnTypes
                        match userLirResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some err }
                        | Ok userOptimizedLirProgram ->
                                // Pass 5: Register Allocation
                                if verbosity >= 1 then println "  [5/8] Register Allocation..."
                                let allocStart = sw.Elapsed.TotalMilliseconds
                                let (LIRSymbolic.Program userFuncs) = userOptimizedLirProgram
                                let (LIR.Program (_, stdlibStrings, stdlibFloats)) = stdlib.LIRProgram

                                // Allocate user functions
                                let allocatedUserFuncs: LIRSymbolic.Function list =
                                    userFuncs |> List.map RegisterAllocation.allocateRegisters

                                // Cache newly-compiled specialized functions using symbolic refs.
                                let rec cacheSpecializedFuncs (remaining: LIRSymbolic.Function list) : Result<unit, string> =
                                    match remaining with
                                    | [] -> Ok ()
                                    | func :: rest ->
                                        if Set.contains func.Name specializedNeedingCache then
                                            match cacheSpecializedFunction stdlib func with
                                            | Error err -> Error $"Cached function {func.Name}: {err}"
                                            | Ok () ->
                                                if verbosity >= 3 then
                                                    let blockCount = func.CFG.Blocks.Count
                                                    let instrCount =
                                                        func.CFG.Blocks
                                                        |> Map.toList
                                                        |> List.sumBy (fun (_, b) -> b.Instrs.Length)
                                                    println $"  [CACHE STORE] {func.Name}: {blockCount} blocks, {instrCount} instrs"
                                                cacheSpecializedFuncs rest
                                        else
                                            cacheSpecializedFuncs rest

                                match cacheSpecializedFuncs allocatedUserFuncs with
                                | Error err ->
                                    { Binary = Array.empty
                                      Success = false
                                      ErrorMessage = Some err }
                                | Ok () ->
                                    let cachedKeys =
                                        let stdlibKeys =
                                            cachedFuncNames
                                            |> List.map (fun name -> (name, CompiledFunctionKey.Stdlib name))
                                        let preambleKeys =
                                            preambleCachedNameSet
                                            |> Set.toList
                                            |> List.map (fun name -> (name, CompiledFunctionKey.Preamble (sourceFile, name)))
                                        stdlibKeys @ preambleKeys

                                    let rec collectCachedEntries acc remaining =
                                        match remaining with
                                        | [] -> Ok (List.rev acc)
                                        | (name, key) :: rest ->
                                            match tryGetCompiledFunction stdlib.CompiledFuncCache key with
                                            | Error err -> Error err
                                            | Ok None -> collectCachedEntries acc rest
                                            | Ok (Some cached) -> collectCachedEntries ((name, cached.SymbolicFunction) :: acc) rest

                                    match collectCachedEntries [] cachedKeys with
                                    | Error err ->
                                        { Binary = Array.empty
                                          Success = false
                                          ErrorMessage = Some err }
                                    | Ok cachedEntries ->
                                        if verbosity >= 3 then
                                            for (name, func) in cachedEntries do
                                                let blockCount = func.CFG.Blocks.Count
                                                let instrCount =
                                                    func.CFG.Blocks
                                                    |> Map.toList
                                                    |> List.sumBy (fun (_, b) -> b.Instrs.Length)
                                                println $"  [CACHE HIT] {name}: {blockCount} blocks, {instrCount} instrs"

                                        let preambleSymbolicFuncs =
                                            preambleFuncs
                                            |> List.filter (fun f -> not (Set.contains f.Name preambleCachedNameSet))
                                        let cachedSymbolicFuncs =
                                            cachedEntries |> List.map snd
                                        let allSymbolicUserFuncs =
                                            preambleSymbolicFuncs @ cachedSymbolicFuncs @ allocatedUserFuncs

                                        match LIRSymbolic.toLIRWithPools stdlibStrings stdlibFloats allSymbolicUserFuncs with
                                        | Error err ->
                                            { Binary = Array.empty
                                              Success = false
                                              ErrorMessage = Some $"LIR pool resolution error: {err}" }
                                        | Ok (LIR.Program (allUserFuncs, mergedStrings, mergedFloats)) ->
                                            // Prune unused user functions (keeps `_start` and anything reachable from it).
                                            let reachableUserFuncs =
                                                if options.DisableDCE then allUserFuncs
                                                else
                                                    let roots =
                                                        if allUserFuncs |> List.exists (fun f -> f.Name = "_start") then
                                                            Set.ofList ["_start"]
                                                        else
                                                            allUserFuncs |> List.map (fun f -> f.Name) |> Set.ofList
                                                    let userCallGraph = DeadCodeElimination.buildCallGraph allUserFuncs
                                                    let reachableNames = DeadCodeElimination.findReachable userCallGraph roots
                                                    allUserFuncs |> List.filter (fun f -> Set.contains f.Name reachableNames)

                                            if verbosity >= 3 then
                                                println $"  [COMBINED] cached: {cachedSymbolicFuncs.Length}, fresh: {allocatedUserFuncs.Length}, total: {allUserFuncs.Length}"
                                                for f in allUserFuncs do
                                                    println $"    - {f.Name}"
                                                println $"  [DCE] user funcs: {reachableUserFuncs.Length}"

                                            // Filter stdlib functions to only include reachable ones (dead code elimination)
                                            let reachableStdlib =
                                                if options.DisableDCE then stdlib.AllocatedFunctions
                                                else
                                                    DeadCodeElimination.filterFunctions
                                                        stdlib.StdlibCallGraph
                                                        reachableUserFuncs
                                                        stdlib.AllocatedFunctions

                                            // Combine reachable stdlib functions with user functions
                                            let allFuncs = reachableStdlib @ reachableUserFuncs
                                            let allocatedProgram = LIR.Program (allFuncs, mergedStrings, mergedFloats)
                                            if shouldDumpIR verbosity options.DumpLIR then
                                                printLIRProgram "=== LIR (After Register Allocation) ===" allocatedProgram

                                            if verbosity >= 2 then
                                                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - allocStart, 1)
                                                println $"        {t}ms"

                                            let binaryResult =
                                                let cacheableNames =
                                                    let stdlibNames =
                                                        reachableStdlib
                                                        |> List.map (fun f -> f.Name)
                                                    let preambleNames =
                                                        reachableUserFuncs
                                                        |> List.choose (fun f ->
                                                            if Set.contains f.Name preambleFuncNameSet then Some f.Name else None)
                                                    let allNames = stdlibNames @ preambleNames @ cachedFuncNames
                                                    allNames
                                                    |> List.filter (fun name -> name <> "_start")
                                                    |> Set.ofList
                                                if cacheableNames.IsEmpty then
                                                    generateBinary
                                                        verbosity
                                                        options
                                                        sw
                                                        "  [6/8] Code Generation..."
                                                        "  [7/7] ARM64 Encoding..."
                                                        "  [7/7] Binary Generation ({format})..."
                                                        false
                                                        false
                                                        allocatedProgram
                                                else
                                                    generateBinaryWithCodegenCache
                                                        verbosity
                                                        options
                                                        sw
                                                        "  [6/8] Code Generation..."
                                                        "  [7/7] ARM64 Encoding..."
                                                        "  [7/7] Binary Generation ({format})..."
                                                        false
                                                        false
                                                        stdlib.CodegenCache
                                                        cacheableNames
                                                        allocatedProgram
                                            match binaryResult with
                                            | Error err ->
                                                { Binary = Array.empty
                                                  Success = false
                                                  ErrorMessage = Some err }
                                            | Ok binary ->
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
let private compileMIRToLIR (_stdlib: LazyStdlibResult) (mirFunc: MIR.Function) : LIRSymbolic.Function option =
    // Create a mini MIR program with just this function
    let miniProgram = MIR.Program ([mirFunc], Map.empty, Map.empty)

    // Convert to symbolic LIR
    match MIR_to_LIR.toLIR miniProgram with
    | Error _ -> None  // Conversion failed
    | Ok lirProgram ->
        // LIR Optimizations
        let optimizedLir = LIR_Optimize.optimizeProgram lirProgram
        let (LIRSymbolic.Program lirFuncs) = optimizedLir

        // Register allocation
        match lirFuncs with
        | [lirFunc] -> Some (RegisterAllocation.allocateRegisters lirFunc)
        | _ -> None

/// Lazily compile a stdlib function from MIR to LIR with caching
/// Returns the compiled LIR function (from cache if available, or compiles and caches)
/// Uses Lazy<T> to ensure compilation happens exactly once even under parallel contention
let private getOrCompileStdlibLIR (stdlib: LazyStdlibResult) (funcName: string) : LIRSymbolic.Function option =
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
let private getStdlibLIRFunctions (stdlib: LazyStdlibResult) (funcNames: string list) : LIRSymbolic.Function list =
    funcNames |> List.choose (getOrCompileStdlibLIR stdlib)

/// Compile user code with lazy stdlib - only compiles stdlib functions that are actually called
/// This avoids compiling unused stdlib functions through expensive passes (MIR, LIR, RegAlloc)
let private compileWithLazyStdlib (verbosity: int) (options: CompilerOptions) (stdlib: LazyStdlibResult) (source: string) : CompileResult =
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
                    let anfResult =
                        buildAnfProgram
                            verbosity
                            options
                            sw
                            programType
                            userOnly.UserFunctions
                            userOnly.MainExpr
                            userOnly
                            []
                            Map.empty
                    match anfResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some err }
                    | Ok (userAnfProgram, typeMap, userConvResult) ->
                        // Early DCE: Determine which stdlib functions are actually needed
                        // Lazily compile needed functions from MIR to LIR (cached for future use)
                        let allocatedStdlibFuncs =
                            if options.DisableDCE then
                                let allNames = stdlib.StdlibMIRFunctions |> Map.keys |> List.ofSeq
                                let allFuncs = getStdlibLIRFunctions stdlib allNames
                                allFuncs
                            else
                                let (ANF.Program (userFuncsForDCE, userMainForDCE)) = userAnfProgram
                                let userANFFuncs = { ANF.Name = "_start"; ANF.TypedParams = []; ANF.ReturnType = AST.TUnit; ANF.Body = userMainForDCE } :: userFuncsForDCE
                                let reachableStdlibNames = ANFDeadCodeElimination.getReachableStdlib stdlib.StdlibANFCallGraph userANFFuncs
                                if verbosity >= 2 then
                                    println $"        DCE: {reachableStdlibNames.Count} stdlib functions needed"
                                let reachableFuncs = getStdlibLIRFunctions stdlib (Set.toList reachableStdlibNames)
                                reachableFuncs

                        let externalReturnTypes = extractReturnTypes userOnly.FuncReg
                        let userLirResult =
                            compileAnfToLir
                                verbosity
                                options
                                sw
                                "user only"
                                userAnfProgram
                                typeMap
                                userConvResult
                                programType
                                externalReturnTypes
                        match userLirResult with
                        | Error err ->
                            { Binary = Array.empty
                              Success = false
                              ErrorMessage = Some err }
                        | Ok userOptimizedLirProgram ->
                            // Pass 5: Register Allocation (user functions)
                            if verbosity >= 1 then println "  [5/8] Register Allocation..."
                            let allocStart = sw.Elapsed.TotalMilliseconds
                            let (LIRSymbolic.Program userFuncs) = userOptimizedLirProgram

                            let allocatedUserFuncs = userFuncs |> List.map RegisterAllocation.allocateRegisters
                            let allocatedSymbolic = LIRSymbolic.Program (allocatedStdlibFuncs @ allocatedUserFuncs)
                            if shouldDumpIR verbosity options.DumpLIR then
                                printLIRSymbolicProgram "=== LIR (After Register Allocation) ===" allocatedSymbolic

                            if verbosity >= 2 then
                                let t = System.Math.Round(sw.Elapsed.TotalMilliseconds - allocStart, 1)
                                println $"        {t}ms"

                            match LIRSymbolic.toLIR allocatedSymbolic with
                            | Error err ->
                                { Binary = Array.empty
                                  Success = false
                                  ErrorMessage = Some $"LIR pool resolution error: {err}" }
                            | Ok allocatedProgram ->
                                let binaryResult =
                                    generateBinary
                                        verbosity
                                        options
                                        sw
                                        "  [6/8] Code Generation..."
                                        "  [7/7] ARM64 Encoding..."
                                        "  [7/7] Binary Generation ({format})..."
                                        false
                                        false
                                        allocatedProgram
                                match binaryResult with
                                | Error err ->
                                    { Binary = Array.empty
                                      Success = false
                                      ErrorMessage = Some err }
                                | Ok binary ->
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

/// Compile source code to binary (in-memory, no file I/O)
/// Uses lazy stdlib compilation - only compiles stdlib functions that are actually called
let compileWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : CompileResult =
    match prepareStdlibForLazyCompile() with
    | Error err ->
        { Binary = Array.empty
          Success = false
          ErrorMessage = Some err }
    | Ok lazyStdlib ->
        compileWithLazyStdlib verbosity options lazyStdlib source

/// Compile source code to binary (uses default options)
/// Execute compiled binary and capture output
let private execute (verbosity: int) (binary: byte array) : ExecutionResult =
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

let private compileResultToExecution (verbosity: int) (compileResult: CompileResult) : ExecutionResult =
    if not compileResult.Success then
        { ExitCode = 1
          Stdout = ""
          Stderr = compileResult.ErrorMessage |> Option.defaultValue "Compilation failed" }
    else
        execute verbosity compileResult.Binary

let private getOrCompilePreambleContext
    (verbosity: int)
    (stdlib: StdlibResult)
    (preamble: string)
    (sourceFile: string)
    (funcLineMap: Map<string, int>)
    : Result<PreambleContext, string> =
    let preambleHash = preamble.GetHashCode()
    let cacheKey = (sourceFile, preambleHash)

    let cacheHit = stdlib.PreambleCache.ContainsKey cacheKey
    if verbosity >= 2 then
        let status = if cacheHit then "hit" else "miss"
        println $"  [Preamble cache {status} for {sourceFile}]"
    let lazyCtx =
        stdlib.PreambleCache.GetOrAdd(
            cacheKey,
            fun _ ->
                Lazy<Result<PreambleContext, string>>(
                    (fun () -> compilePreamble stdlib preamble sourceFile funcLineMap),
                    System.Threading.LazyThreadSafetyMode.ExecutionAndPublication))
    lazyCtx.Value

/// Compile and run source code with options
let compileAndRunWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : ExecutionResult =
    compileWithOptions verbosity options source |> compileResultToExecution verbosity

/// Compile and run with timing breakdown (for test output)
let compileAndRunWithStdlibCachedTimed (verbosity: int) (options: CompilerOptions) (stdlib: StdlibResult) (testExpr: string) (preamble: string) (sourceFile: string) (funcLineMap: Map<string, int>) : TimedExecutionResult =
    match getOrCompilePreambleContext verbosity stdlib preamble sourceFile funcLineMap with
    | Error err ->
        { ExitCode = 1
          Stdout = ""
          Stderr = err
          CompileTime = TimeSpan.Zero
          RuntimeTime = TimeSpan.Zero }
    | Ok preambleCtx ->
        let compileTimer = Stopwatch.StartNew()
        let compileResult = compileTestWithPreamble verbosity options stdlib preambleCtx sourceFile testExpr
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
