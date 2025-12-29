// OptimizationTestRunner.fs - Test runner for optimization verification
//
// Compiles source code, captures IR at specific stages, and compares
// against expected output to verify optimizations work correctly.

module TestDSL.OptimizationTestRunner

open System
open TestDSL.OptimizationFormat
open TestDSL.PassTestRunner

/// Result of running an optimization test
type OptimizationTestResult = {
    Success: bool
    Message: string
    Expected: string option
    Actual: string option
}

/// Normalize IR output for comparison
/// - Trim whitespace
/// - Normalize line endings
/// - Remove trailing whitespace from each line
let normalizeIR (ir: string) : string =
    ir.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.TrimEnd())
    |> Array.filter (fun line -> line.Length > 0)
    |> String.concat "\n"

/// Pretty-print MIR program with CFG structure
let prettyPrintMIRProgram (MIR.Program (functions, _, _, _, _)) : string =
    let funcStrs =
        functions
        |> List.map (fun func ->
            let blockStrs =
                func.CFG.Blocks
                |> Map.toList
                |> List.sortBy fst
                |> List.map (fun (label, block) ->
                    let instrStrs =
                        block.Instrs
                        |> List.map (sprintf "    %A")
                        |> String.concat "\n"
                    let termStr = sprintf "    %A" block.Terminator
                    $"  {label}:\n{instrStrs}\n{termStr}")
                |> String.concat "\n"
            $"{func.Name}:\n{blockStrs}")
        |> String.concat "\n\n"
    funcStrs

/// Pretty-print LIR program with CFG structure
let prettyPrintLIRProgram (LIR.Program (functions, _, _)) : string =
    let funcStrs =
        functions
        |> List.map (fun func ->
            let blockStrs =
                func.CFG.Blocks
                |> Map.toList
                |> List.sortBy fst
                |> List.map (fun (label, block) ->
                    let instrStrs =
                        block.Instrs
                        |> List.map prettyPrintLIRInstr
                        |> List.map (sprintf "    %s")
                        |> String.concat "\n"
                    let termStr = sprintf "    %s" (prettyPrintLIRTerminator block.Terminator)
                    $"  {label}:\n{instrStrs}\n{termStr}")
                |> String.concat "\n"
            $"{func.Name}:\n{blockStrs}")
        |> String.concat "\n\n"
    funcStrs

/// Compile source and get ANF after optimization
let getOptimizedANF (source: string) : Result<string, string> =
    // Parse source
    match Parser.parseString source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast ->
        // Type check
        match TypeChecking.checkProgram ast with
        | Error e -> Error $"Type error: {TypeChecking.typeErrorToString e}"
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match AST_to_ANF.convertProgramWithTypes typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized = ANF_Optimize.optimizeProgram convResult.Program

                // Pretty-print the result
                Ok (prettyPrintANF optimized)

/// Compile source and get MIR after optimization
let getOptimizedMIR (source: string) : Result<string, string> =
    // Parse source
    match Parser.parseString source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast ->
        // Type check
        match TypeChecking.checkProgram ast with
        | Error e -> Error $"Type error: {TypeChecking.typeErrorToString e}"
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match AST_to_ANF.convertProgramWithTypes typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized = ANF_Optimize.optimizeProgram convResult.Program

                // Reference counting and print insertion
                let convResultOptimized = { convResult with Program = optimized }
                match RefCountInsertion.insertRCInProgram convResultOptimized with
                | Error e -> Error $"RC insertion error: {e}"
                | Ok anfAfterRC ->
                    let (ANF.Program (functions, mainExpr)) = anfAfterRC
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType

                    // Convert to MIR
                    let emptyTypeMap : ANF.TypeMap = Map.empty
                    match ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0) emptyTypeMap Map.empty programType convResultOptimized.VariantLookup convResultOptimized.TypeReg with
                    | Error e -> Error $"MIR conversion error: {e}"
                    | Ok (mirProgram, _) ->
                        // SSA construction
                        let ssaProgram = SSA_Construction.convertToSSA mirProgram

                        // MIR optimization
                        let optimizedMir = MIR_Optimize.optimizeProgram ssaProgram

                        // SSA destruction
                        let mirAfterSSA = SSA_Destruction.destructSSA optimizedMir

                        // Pretty-print
                        Ok (prettyPrintMIRProgram mirAfterSSA)

/// Compile source and get LIR after optimization
let getOptimizedLIR (source: string) : Result<string, string> =
    // Parse source
    match Parser.parseString source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast ->
        // Type check
        match TypeChecking.checkProgram ast with
        | Error e -> Error $"Type error: {TypeChecking.typeErrorToString e}"
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match AST_to_ANF.convertProgramWithTypes typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized = ANF_Optimize.optimizeProgram convResult.Program

                // Reference counting and print insertion
                let convResultOptimized = { convResult with Program = optimized }
                match RefCountInsertion.insertRCInProgram convResultOptimized with
                | Error e -> Error $"RC insertion error: {e}"
                | Ok anfAfterRC ->
                    let (ANF.Program (functions, mainExpr)) = anfAfterRC
                    let anfProgram = PrintInsertion.insertPrint functions mainExpr programType

                    // Convert to MIR
                    let emptyTypeMap : ANF.TypeMap = Map.empty
                    match ANF_to_MIR.toMIR anfProgram (MIR.RegGen 0) emptyTypeMap Map.empty programType convResultOptimized.VariantLookup convResultOptimized.TypeReg with
                    | Error e -> Error $"MIR conversion error: {e}"
                    | Ok (mirProgram, _) ->
                        // SSA construction and optimization
                        let ssaProgram = SSA_Construction.convertToSSA mirProgram
                        let optimizedMir = MIR_Optimize.optimizeProgram ssaProgram
                        let mirAfterSSA = SSA_Destruction.destructSSA optimizedMir

                        // Convert to LIR
                        match MIR_to_LIR.toLIR mirAfterSSA with
                        | Error e -> Error $"LIR conversion error: {e}"
                        | Ok lirProgram ->
                            // LIR optimization
                            let optimizedLir = LIR_Optimize.optimizeProgram lirProgram

                            // Pretty-print
                            Ok (prettyPrintLIRProgram optimizedLir)

/// Run a single optimization test
let runOptimizationTest (test: OptimizationTest) : OptimizationTestResult =
    let irResult =
        match test.Stage with
        | ANF -> getOptimizedANF test.Source
        | MIR -> getOptimizedMIR test.Source
        | LIR -> getOptimizedLIR test.Source

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
let runTestFile (stage: IRStage) (path: string) : Result<(OptimizationTest * OptimizationTestResult) list, string> =
    match parseTestFile stage path with
    | Error e -> Error e
    | Ok tests ->
        let results = tests |> List.map (fun test -> (test, runOptimizationTest test))
        Ok results
