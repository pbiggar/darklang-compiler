// OptimizationTestRunner.fs - Test runner for optimization verification
//
// Compiles source code, captures IR at specific stages, and compares
// against expected output to verify optimizations work correctly.

module TestDSL.OptimizationTestRunner

open System
open AST
open TestDSL.OptimizationFormat
open IRPrinter

/// Result of running an optimization test
type OptimizationTestResult = {
    Success: bool
    Message: string
    Expected: string option
    Actual: string option
}

let private externalReturnTypes : Map<string, AST.Type> =
    Map.ofList [
        ("__hash_i64", TInt64)
        ("__hash_str", TInt64)
        ("__hash_bool", TInt64)
        ("__key_eq_i64", TBool)
        ("__key_eq_str", TBool)
        ("__key_eq_bool", TBool)
        ("__string_hash", TInt64)
        ("__string_eq", TBool)
    ]

let private typeCheckWithStdlib (stdlib: CompilerLibrary.StdlibResult) (ast: AST.Program) : Result<AST.Type * AST.Program, string> =
    match TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv ast with
    | Error e -> Error $"Type error: {TypeChecking.typeErrorToString e}"
    | Ok (programType, typedAst, _env) -> Ok (programType, typedAst)

let private convertTypedProgram (typedAst: AST.Program) : Result<AST_to_ANF.ConversionResult, string> =
    let moduleRegistry = Stdlib.buildModuleRegistry ()
    let monomorphized = AST_to_ANF.monomorphize typedAst
    let inlined = AST_to_ANF.inlineLambdasInProgram monomorphized
    AST_to_ANF.liftLambdasInProgram Map.empty Map.empty Map.empty Map.empty inlined
    |> Result.bind (fun lifted ->
        AST_to_ANF.splitTopLevels lifted
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
            let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
            let registries = AST_to_ANF.buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    {
                        Program = ANF.Program (anfFuncs, anfExpr)
                        TypeReg = registries.TypeReg
                        VariantLookup = registries.VariantLookup
                        FuncReg = registries.FuncReg
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                    }))))

let private optimizeContextFromConversionResult (convResult: AST_to_ANF.ConversionResult) : ANF_Optimize.OptimizeContext =
    { TypeReg = convResult.TypeReg
      SumShapeReg = AST_to_ANF.rcSumShapeRegistryFromVariantLookup convResult.VariantLookup }

/// Normalize IR output for comparison
/// - Trim whitespace
/// - Normalize line endings
/// - Remove trailing whitespace from each line
let normalizeIR (ir: string) : string =
    ir.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.TrimEnd())
    |> Array.filter (fun line -> line.Length > 0)
    |> String.concat "\n"

/// Compile source and get ANF after optimization
let getOptimizedANF (stdlib: CompilerLibrary.StdlibResult) (source: string) : Result<string, string> =
    // Parse source
    match Parser.parseString true source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast ->
        // Type check
        match typeCheckWithStdlib stdlib ast with
        | Error e -> Error e
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match convertTypedProgram typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized =
                    ANF_Optimize.optimizeProgramWithContextAndOptions
                        (optimizeContextFromConversionResult convResult)
                        ANF_Optimize.defaultOptimizeOptions
                        convResult.Program

                // Pretty-print the result
                Ok (formatANF optimized)

/// Compile source and get MIR after optimization
let getOptimizedMIR (stdlib: CompilerLibrary.StdlibResult) (source: string) : Result<string, string> =
    // Parse source
    match Parser.parseString true source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast ->
        // Type check
        match typeCheckWithStdlib stdlib ast with
        | Error e -> Error e
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match convertTypedProgram typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized =
                    ANF_Optimize.optimizeProgramWithContextAndOptions
                        (optimizeContextFromConversionResult convResult)
                        ANF_Optimize.defaultOptimizeOptions
                        convResult.Program

                // Reference counting and print insertion
                let convResultOptimized = { convResult with Program = optimized }
                match RefCountInsertion.insertRCInProgram convResultOptimized with
                | Error e -> Error $"RC insertion error: {e}"
                | Ok (anfAfterRC, typeMap) ->
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC
                    let (ANF.Program (functions, mainExpr)) = anfAfterTCO
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType

                    // Convert to MIR
                    match ANF_to_MIR.toMIR anfProgram typeMap Map.empty programType convResultOptimized.VariantLookup convResultOptimized.TypeReg false externalReturnTypes with
                    | Error e -> Error $"MIR conversion error: {e}"
                    | Ok mirProgram ->
                        // SSA construction
                        let ssaProgram = SSA_Construction.convertToSSA mirProgram

                        // MIR optimization
                        let optimizedMir = MIR_Optimize.optimizeProgram ssaProgram

                        // SSA form is now preserved (phi resolution happens in register allocation)
                        // Pretty-print the optimized MIR (still in SSA form)
                        Ok (formatMIR optimizedMir)

/// Compile source and get LIR after optimization
let getOptimizedLIR (stdlib: CompilerLibrary.StdlibResult) (source: string) : Result<string, string> =
    // Parse source
    match Parser.parseString true source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast ->
        // Type check
        match typeCheckWithStdlib stdlib ast with
        | Error e -> Error e
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match convertTypedProgram typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized =
                    ANF_Optimize.optimizeProgramWithContextAndOptions
                        (optimizeContextFromConversionResult convResult)
                        ANF_Optimize.defaultOptimizeOptions
                        convResult.Program

                // Reference counting and print insertion
                let convResultOptimized = { convResult with Program = optimized }
                match RefCountInsertion.insertRCInProgram convResultOptimized with
                | Error e -> Error $"RC insertion error: {e}"
                | Ok (anfAfterRC, typeMap) ->
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC
                    let (ANF.Program (functions, mainExpr)) = anfAfterTCO
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType

                    // Convert to MIR
                    match ANF_to_MIR.toMIR anfProgram typeMap Map.empty programType convResultOptimized.VariantLookup convResultOptimized.TypeReg false externalReturnTypes with
                    | Error e -> Error $"MIR conversion error: {e}"
                    | Ok mirProgram ->
                        // SSA construction and optimization
                        let ssaProgram = SSA_Construction.convertToSSA mirProgram
                        let optimizedMir = MIR_Optimize.optimizeProgram ssaProgram

                        // SSA form is now preserved (phi resolution happens in register allocation)
                        // Convert to LIR
                        match MIR_to_LIR.toLIR optimizedMir with
                        | Error e -> Error $"LIR conversion error: {e}"
                        | Ok lirProgram ->
                            // LIR optimization
                            let optimizedLir = LIR_Peephole.optimizeProgram lirProgram
                            // Pretty-print
                            Ok (formatLIR optimizedLir)

/// Run a single optimization test
let runOptimizationTest (stdlib: CompilerLibrary.StdlibResult) (test: OptimizationTest) : OptimizationTestResult =
    let irResult =
        match test.Stage with
        | ANF -> getOptimizedANF stdlib test.Source
        | MIR -> getOptimizedMIR stdlib test.Source
        | LIR -> getOptimizedLIR stdlib test.Source

    match irResult with
    | Error e ->
        { Success = false
          Message = e
          Expected = Some test.ExpectedIR
          Actual = None }
    | Ok actualIR ->
        let normalizedExpected = normalizeIR test.ExpectedIR
        let normalizedActual = normalizeIR actualIR

        if normalizedExpected = normalizedActual then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            { Success = false
              Message = "IR mismatch"
              Expected = Some normalizedExpected
              Actual = Some normalizedActual }

/// Load and run tests from a file
let runTestFile (stdlib: CompilerLibrary.StdlibResult) (stage: IRStage) (path: string) : Result<(OptimizationTest * OptimizationTestResult) list, string> =
    match parseTestFile stage path with
    | Error e -> Error e
    | Ok tests ->
        let results = tests |> List.map (fun test -> (test, runOptimizationTest stdlib test))
        Ok results
