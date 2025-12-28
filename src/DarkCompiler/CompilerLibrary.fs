// CompilerLibrary.fs - Library API for the Dark compiler
//
// Exposes the compiler as a library for use in tests and other tools.
// Provides clean functions that can be called without spawning processes.

module CompilerLibrary

open System
open System.IO
open System.Diagnostics
open System.Reflection
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

/// Compiler options for controlling code generation behavior
type CompilerOptions = {
    /// Disable free list memory reuse (always bump allocate)
    /// Useful for testing that free list is necessary
    DisableFreeList: bool
}

/// Default compiler options
let defaultOptions : CompilerOptions = {
    DisableFreeList = false
}

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
}

/// Load the stdlib.dark file
/// Returns the stdlib AST or an error message
let loadStdlib () : Result<AST.Program, string> =
    // Find stdlib.dark relative to the executable
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    // Try multiple locations for stdlib.dark
    let possiblePaths = [
        Path.Combine(exeDir, "stdlib.dark")
        Path.Combine(exeDir, "..", "..", "..", "..", "..", "src", "DarkCompiler", "stdlib.dark")
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", "stdlib.dark")
    ]
    let stdlibPath =
        possiblePaths
        |> List.tryFind File.Exists
    match stdlibPath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find stdlib.dark in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        Parser.parseString source
        |> Result.mapError (fun err -> $"Error parsing stdlib.dark: {err}")

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
                | Ok anfAfterRC ->
                    // Convert stdlib ANF to MIR (cached for reuse)
                    let emptyTypeMap : ANF.TypeMap = Map.empty
                    let emptyTypeReg : Map<string, (string * AST.Type) list> = Map.empty
                    match ANF_to_MIR.toMIR anfAfterRC (MIR.RegGen 0) emptyTypeMap emptyTypeReg with
                    | Error e -> Error e
                    | Ok (mirProgram, _) ->
                        // Convert stdlib MIR to LIR (cached for reuse)
                        match MIR_to_LIR.toLIR mirProgram with
                        | Error e -> Error e
                        | Ok lirProgram ->
                            Ok {
                                AST = stdlibAst
                                TypedAST = typedStdlib
                                TypeCheckEnv = typeCheckEnv
                                ANFResult = anfResult
                                GenericFuncDefs = genericFuncDefs
                                ModuleRegistry = moduleRegistry
                                MIRProgram = mirProgram
                                LIRProgram = lirProgram
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
                    let anfOptimized = ANF_Optimize.optimizeProgram convResult.Program
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

                    // Update convResult with optimized program for RC insertion
                    let convResultOptimized = { convResult with Program = anfOptimized }

                    // Pass 2.5: Reference Count Insertion
                    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
                    let rcResult = RefCountInsertion.insertRCInProgram convResultOptimized
                    let rcTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - anfOptTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(rcTime, 1)
                        println $"        {t}ms"

                    match rcResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Reference count insertion error: {err}" }
                    | Ok anfAfterRC ->

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

                    // Pass 2.6: Print Insertion (for main expression)
                    if verbosity >= 1 then println "  [2.6/8] Print Insertion..."
                    let (ANF.Program (functions, mainExpr)) = anfAfterRC
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType
                    let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime
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
                    // Pass empty TypeMap and TypeReg since payload sizes are now stored in instructions
                    let emptyTypeMap : ANF.TypeMap = Map.empty
                    let emptyTypeReg : Map<string, (string * AST.Type) list> = Map.empty
                    let mirResult = ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0) emptyTypeMap emptyTypeReg

                    match mirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"MIR conversion error: {err}" }
                    | Ok (mirProgram, _) ->

                    // Show MIR
                    if verbosity >= 3 then
                        let (MIR.Program (functions, _, _)) = mirProgram
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

                    let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime
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
                    let optimizedProgram = MIR_Optimize.optimizeProgram ssaProgram

                    let mirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - ssaTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(mirOptTime, 1)
                        println $"        {t}ms"

                    // Pass 3.9: SSA Destruction
                    if verbosity >= 1 then println "  [3.9/8] SSA Destruction..."
                    let mirAfterSSA = SSA_Destruction.destructSSA optimizedProgram

                    let ssaDestructTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - ssaTime - mirOptTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(ssaDestructTime, 1)
                        println $"        {t}ms"

                    // Pass 4: MIR → LIR
                    if verbosity >= 1 then println "  [4/8] MIR → LIR..."
                    let lirResult = MIR_to_LIR.toLIR mirAfterSSA

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

                    let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(lirTime, 1)
                        println $"        {t}ms"

                    // Pass 4.5: LIR Optimizations (peephole)
                    if verbosity >= 1 then println "  [4.5/8] LIR Optimizations..."
                    let optimizedLirProgram = LIR_Optimize.optimizeProgram lirProgram

                    let lirOptTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - ssaTime - mirOptTime - ssaDestructTime - lirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(lirOptTime, 1)
                        println $"        {t}ms"

                    // Pass 5: Register Allocation
                    if verbosity >= 1 then println "  [5/8] Register Allocation..."
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

                    let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocTime, 1)
                        println $"        {t}ms"

                    // Pass 6: Code Generation (LIR → ARM64)
                    if verbosity >= 1 then println "  [6/8] Code Generation..."
                    let codegenOptions : CodeGen.CodeGenOptions = {
                        DisableFreeList = options.DisableFreeList
                    }
                    let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
                    let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime - allocTime
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
                        if verbosity >= 1 then println "  [7/8] ARM64 Encoding..."

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

                        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(encodeTime, 1)
                            println $"        {t}ms"

                        // Pass 8: Binary Generation (machine code → executable)
                        let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                        if verbosity >= 1 then println $"  [8/8] Binary Generation ({formatName})..."
                        let binary =
                            match os with
                            | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool
                            | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool
                        let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
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
                if verbosity >= 1 then println "  [2/8] AST → ANF (user only)..."
                let userOnlyResult =
                    AST_to_ANF.convertUserOnly
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
                    // Create ConversionResult for RC insertion (user functions only)
                    let userConvResult : AST_to_ANF.ConversionResult = {
                        Program = ANF.Program (userOnly.UserFunctions, userOnly.MainExpr)
                        TypeReg = userOnly.TypeReg
                        VariantLookup = userOnly.VariantLookup
                        FuncReg = userOnly.FuncReg
                        FuncParams = userOnly.FuncParams
                        ModuleRegistry = userOnly.ModuleRegistry
                    }

                    // Pass 2.5: Reference Count Insertion (user code only)
                    if verbosity >= 1 then println "  [2.5/8] Reference Count Insertion..."
                    let rcResult = RefCountInsertion.insertRCInProgram userConvResult
                    let rcTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(rcTime, 1)
                        println $"        {t}ms"

                    match rcResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"Reference count insertion error: {err}" }
                    | Ok userAnfAfterRC ->

                    // Pass 2.6: Print Insertion (user code only)
                    if verbosity >= 1 then println "  [2.6/8] Print Insertion..."
                    let (ANF.Program (userFunctions, userMainExpr)) = userAnfAfterRC
                    let userAnfProgram = PrintInsertion.insertPrint userFunctions userMainExpr programType
                    let printTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(printTime, 1)
                        println $"        {t}ms"

                    // Pass 3: ANF → MIR (user code only)
                    if verbosity >= 1 then println "  [3/8] ANF → MIR (user only)..."
                    let emptyTypeMap : ANF.TypeMap = Map.empty
                    let emptyTypeReg : Map<string, (string * AST.Type) list> = Map.empty
                    let userMirResult = ANF_to_MIR.toMIR userAnfProgram (MIR.RegGen 0) emptyTypeMap emptyTypeReg

                    match userMirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"MIR conversion error: {err}" }
                    | Ok (userMirProgram, _) ->

                    let mirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(mirTime, 1)
                        println $"        {t}ms"

                    // Pass 4: MIR → LIR (user code only, then merge with cached stdlib LIR)
                    if verbosity >= 1 then println "  [4/8] MIR → LIR (user + cached stdlib)..."
                    let userLirResult = MIR_to_LIR.toLIR userMirProgram

                    match userLirResult with
                    | Error err ->
                        { Binary = Array.empty
                          Success = false
                          ErrorMessage = Some $"LIR conversion error: {err}" }
                    | Ok userLirProgram ->

                    // Merge user LIR with cached stdlib LIR (offsets pool indices)
                    let lirProgram = MIR_to_LIR.mergeLIRPrograms stdlib.LIRProgram userLirProgram

                    let lirTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(lirTime, 1)
                        println $"        {t}ms"

                    // Pass 5: Register Allocation
                    if verbosity >= 1 then println "  [5/8] Register Allocation..."
                    let (LIR.Program (funcs, stringPool, floatPool)) = lirProgram
                    let allocatedFuncs = funcs |> List.map RegisterAllocation.allocateRegisters
                    let allocatedProgram = LIR.Program (allocatedFuncs, stringPool, floatPool)

                    let allocTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime
                    if verbosity >= 2 then
                        let t = System.Math.Round(allocTime, 1)
                        println $"        {t}ms"

                    // Pass 6: Code Generation
                    if verbosity >= 1 then println "  [6/8] Code Generation..."
                    let codegenOptions : CodeGen.CodeGenOptions = { DisableFreeList = options.DisableFreeList }
                    let codegenResult = CodeGen.generateARM64WithOptions codegenOptions allocatedProgram
                    let codegenTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime - allocTime
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
                        if verbosity >= 1 then println "  [7/8] ARM64 Encoding..."
                        let codeFileOffset =
                            match os with
                            | Platform.Linux -> 64 + 56
                            | Platform.MacOS ->
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
                        let machineCode = ARM64_Encoding.encodeAllWithPools arm64Instructions stringPool floatPool codeFileOffset

                        let encodeTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime
                        if verbosity >= 2 then
                            let t = System.Math.Round(encodeTime, 1)
                            println $"        {t}ms"

                        // Pass 8: Binary Generation
                        let formatName = match os with | Platform.MacOS -> "Mach-O" | Platform.Linux -> "ELF"
                        if verbosity >= 1 then println $"  [8/8] Binary Generation ({formatName})..."
                        let binary =
                            match os with
                            | Platform.MacOS -> Binary_Generation_MachO.createExecutableWithPools machineCode stringPool floatPool
                            | Platform.Linux -> Binary_Generation_ELF.createExecutableWithPools machineCode stringPool floatPool
                        let binaryTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime - anfTime - rcTime - printTime - mirTime - lirTime - allocTime - codegenTime - encodeTime
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

/// Compile source code to binary (in-memory, no file I/O)
/// This loads stdlib and compiles it together with user code in one pass
let compileWithOptions (verbosity: int) (options: CompilerOptions) (source: string) : CompileResult =
    // Load stdlib AST and compile directly (no separate stdlib compilation)
    match loadStdlib() with
    | Error err ->
        { Binary = Array.empty
          Success = false
          ErrorMessage = Some err }
    | Ok stdlibAst ->
        compileWithStdlibAST verbosity options stdlibAst source

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
                try
                    Process.Start(execInfo)
                with
                | :? System.ComponentModel.Win32Exception as ex when ex.Message.Contains("Text file busy") && attempts > 0 ->
                    Threading.Thread.Sleep(10)  // Wait 10ms before retry
                    startWithRetry (attempts - 1)

            use execProc = startWithRetry 3

            // Start async reads immediately to avoid blocking
            let stdoutTask = execProc.StandardOutput.ReadToEndAsync()
            let stderrTask = execProc.StandardError.ReadToEndAsync()

            // Wait for process to complete
            execProc.WaitForExit()

            // Now wait for output to be fully read
            let stdout = stdoutTask.Result
            let stderr = stderrTask.Result

            let execTime = sw.Elapsed.TotalMilliseconds - execStart
            if verbosity >= 2 then println $"      {System.Math.Round(execTime, 1)}ms"

            sw.Stop()

            if verbosity >= 1 then
                println $"  ✓ Execution complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"

            { ExitCode = execProc.ExitCode
              Stdout = stdout
              Stderr = stderr }
    finally
        // Cleanup
        try File.Delete(tempPath) with | _ -> ()

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
