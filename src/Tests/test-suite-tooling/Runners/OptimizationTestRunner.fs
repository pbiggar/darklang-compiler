// OptimizationTestRunner.fs - Test runner for optimization verification
//
// Compiles source code, captures IR at specific stages, and compares
// against expected output to verify optimizations work correctly.

module TestDSL.OptimizationTestRunner

open System
open AST
open TestDSL.OptimizationFormat
open ANFPrinter
open MIRPrinter
open LIRPrinter
open TestDSL.LIRParser
open TestDSL.ARM64SymbolicParser
open TestDSL.X86_64Parser
/// Result of running an optimization test
type OptimizationTestResult = {
    Success: bool
    Message: string
    Expected: string option
    Actual: string option
}

let private externalReturnTypes : Map<AST.FunctionId, string * AST.Type> =
    Map.ofList [
        (TestIds.functionIdForName "__hash_i64", ("__hash_i64", TInt64))
        (TestIds.functionIdForName "__hash_str", ("__hash_str", TInt64))
        (TestIds.functionIdForName "__hash_bool", ("__hash_bool", TInt64))
        (TestIds.functionIdForName "__key_eq_i64", ("__key_eq_i64", TBool))
        (TestIds.functionIdForName "__key_eq_str", ("__key_eq_str", TBool))
        (TestIds.functionIdForName "__key_eq_bool", ("__key_eq_bool", TBool))
        (TestIds.functionIdForName "__string_hash", ("__string_hash", TInt64))
    ]

let private externalFunctionNames =
    externalReturnTypes
    |> Map.toList
    |> List.map (fun (id, (name, _)) -> (id, name))
    |> Map.ofList

let private returnTypesFor (stdlib: CompilationContexts.StdlibResult) =
    externalReturnTypes
    |> Map.fold (fun returnTypes id value -> Map.add id value returnTypes) stdlib.Context.ReturnTypes

let private typeCheckWithStdlib (stdlib: CompilationContexts.StdlibResult) (ast: AST.Program) : Result<AST.Type * CheckedAST.Program, string> =
    match TypeChecking.checkProgramWithBaseEnv stdlib.Context.TypeCheckEnv ast with
    | Error e -> Error $"Type error: {CheckingDiagnostics.typeErrorToString e}"
    | Ok (programType, typedAst, _env) -> Ok (programType, typedAst)

let private hasTopLevelExpression (AST.Program topLevels: AST.Program) : bool =
    topLevels
    |> List.exists (function
        | AST.Expression _ -> true
        | AST.FunctionDef _ | AST.TypeDef _ | AST.ValueDef _ -> false)

let private addSyntheticMainExpressionIfNeeded (AST.Program topLevels: AST.Program) : AST.Program * bool =
    if hasTopLevelExpression (AST.Program topLevels) then
        (AST.Program topLevels, false)
    else
        (AST.Program (topLevels @ [ AST.Expression ([], AST.Int64Literal 0L) ]), true)

let private parseOptimizationSource (source: string) : Result<AST.Program * bool, string> =
    match Parser.parseString true source with
    | Error e -> Error $"Parse error: {e}"
    | Ok ast -> Ok (addSyntheticMainExpressionIfNeeded ast)

let private convertTypedProgram
    (stdlib: CompilationContexts.StdlibResult)
    (typedAst: CheckedAST.Program)
    : Result<AST_to_ANF.ConversionResult, string> =
    SourcePreparation.convertTypedProgramToUserOnly stdlib.Context typedAst
    |> Result.map (fun converted ->
        {
            Program = ANF.Program (converted.UserFunctions, converted.MainExpr)
            OwnershipContracts = converted.OwnershipContracts
            RecursiveMembers = converted.RecursiveMembers
            TypeReg = converted.TypeReg
            RecordFieldsReg = converted.RecordFieldsReg
            RecordTypeParamsReg = converted.RecordTypeParamsReg
            VariantLookup = converted.VariantLookup
            RcSumShapeReg = converted.RcSumShapeReg
            FuncReg = converted.FuncReg
            FuncParams = converted.FuncParams
            ModuleRegistry = converted.ModuleRegistry
        })

let private optimizeContextFromConversionResult (convResult: AST_to_ANF.ConversionResult) : ANFConstants.OptimizeContext =
    { TypeReg = convResult.RecordFieldsReg
      RecordTypeParams = convResult.RecordTypeParamsReg
      SumShapeReg = convResult.RcSumShapeReg
      FunctionNames = convResult.FuncReg |> Map.map (fun _ (name, _) -> name) }

/// Normalize IR output for comparison
/// - Trim whitespace
/// - Normalize line endings
/// - Remove trailing whitespace from each line
let normalizeIR (ir: string) : string =
    ir.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.TrimEnd())
    |> Array.filter (fun line -> line.Length > 0)
    |> String.concat "\n"

let private withoutSyntheticANFMain (ir: string) : string =
    let suffix = "\n\nMain:\nreturn 0"
    if ir.EndsWith(suffix, StringComparison.Ordinal) then
        ir.Substring(0, ir.Length - suffix.Length)
    else
        ir

let private formatANFForOptimizationTest (syntheticMain: bool) (program: ANF.Program) : string =
    let formatted = formatANF program
    if syntheticMain then
        withoutSyntheticANFMain formatted
    else
        formatted

let private removeSyntheticMIREntry (MIR.Program (functions, variants, records)) : MIR.Program =
    MIR.Program (
        functions |> List.filter (fun func -> func.Name <> "_start"),
        variants,
        records
    )

let private formatMIRForOptimizationTest
    (functionNames: Map<AST.FunctionId, string>)
    (syntheticMain: bool)
    (program: MIR.Program)
    : string =
    let functionNames =
        Map.fold (fun names id name -> Map.add id name names) functionNames externalFunctionNames
    if syntheticMain then
        formatMIRWithFunctionNames functionNames (removeSyntheticMIREntry program)
    else
        formatMIRWithFunctionNames functionNames program

let private removeSyntheticLIREntry (LIR.Program (functions, variants, records)) : LIR.Program =
    LIR.Program (
        functions |> List.filter (fun func -> func.Name <> "_start"),
        variants,
        records
    )

let private formatLIRForOptimizationTest (syntheticMain: bool) (program: LIR.Program) : string =
    if syntheticMain then
        formatLIR (removeSyntheticLIREntry program)
    else
        formatLIR program

/// Compile source and get ANF after optimization
let getOptimizedANF (stdlib: CompilationContexts.StdlibResult) (source: string) : Result<string, string> =
    match parseOptimizationSource source with
    | Error e -> Error e
    | Ok (ast, syntheticMain) ->
        // Type check
        match typeCheckWithStdlib stdlib ast with
        | Error e -> Error e
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match convertTypedProgram stdlib typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized =
                    ANF_Optimize.optimizeProgramWithOptions
                        (optimizeContextFromConversionResult convResult)
                        ANFConstants.defaultOptimizeOptions
                        convResult.Program

                // Pretty-print the result
                Ok (formatANFForOptimizationTest syntheticMain optimized)

let getOptimizedStdlibANF (stdlib: CompilationContexts.StdlibResult) (functionName: string) : Result<string, string> =
    match Map.tryFind functionName stdlib.StdlibANFFunctions with
    | None -> Error $"Prebuilt stdlib ANF function not found: {functionName}"
    | Some func ->
        let functionNames =
            stdlib.StdlibANFFunctions
            |> Map.values
            |> Seq.map (fun candidate -> (candidate.Id, candidate.Name))
            |> Map.ofSeq
        Ok (formatANFFunction functionNames func)

/// Compile source and get MIR after optimization
let getOptimizedMIR (stdlib: CompilationContexts.StdlibResult) (source: string) : Result<string, string> =
    match parseOptimizationSource source with
    | Error e -> Error e
    | Ok (ast, syntheticMain) ->
        // Type check
        match typeCheckWithStdlib stdlib ast with
        | Error e -> Error e
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match convertTypedProgram stdlib typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized =
                    ANF_Optimize.optimizeProgramWithOptions
                        (optimizeContextFromConversionResult convResult)
                        ANFConstants.defaultOptimizeOptions
                        convResult.Program

                // Generated output participates in reference-count insertion.
                let (ANF.Program (functions, mainExpr)) = optimized
                let printed = PrintInsertion.insertPrint functions mainExpr programType
                let convResultOptimized = { convResult with Program = printed }
                match RefCountInsertion.insertRCInProgram convResultOptimized with
                | Error e -> Error $"RC insertion error: {e}"
                | Ok (anfAfterRC, typeMap) ->
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC

                    // Convert to MIR
                    match ANF_to_MIR.toMIR anfAfterTCO typeMap Map.empty programType convResultOptimized.VariantLookup (TypeRegistries.recordFieldsRegistry convResultOptimized.TypeReg) false (returnTypesFor stdlib) with
                    | Error e -> Error $"MIR conversion error: {e}"
                    | Ok mirProgram ->
                        // SSA construction
                        let ssaProgram = SSA_Construction.convertToSSA mirProgram

                        // MIR optimization
                        let optimizedMir = MIR_Optimize.optimizeProgram ssaProgram

                        // SSA form is now preserved (phi resolution happens in register allocation)
                        // Pretty-print the optimized MIR (still in SSA form)
                        let functionNames =
                            convResultOptimized.FuncReg |> Map.map (fun _ (name, _) -> name)
                        Ok (formatMIRForOptimizationTest functionNames syntheticMain optimizedMir)

/// Compile source and get LIR after optimization
let getOptimizedLIR (stdlib: CompilationContexts.StdlibResult) (source: string) : Result<string, string> =
    match parseOptimizationSource source with
    | Error e -> Error e
    | Ok (ast, syntheticMain) ->
        // Type check
        match typeCheckWithStdlib stdlib ast with
        | Error e -> Error e
        | Ok (programType, typedAst) ->
            // Convert to ANF
            match convertTypedProgram stdlib typedAst with
            | Error e -> Error $"ANF conversion error: {e}"
            | Ok convResult ->
                // Optimize ANF
                let optimized =
                    ANF_Optimize.optimizeProgramWithOptions
                        (optimizeContextFromConversionResult convResult)
                        ANFConstants.defaultOptimizeOptions
                        convResult.Program

                // Generated output participates in reference-count insertion.
                let (ANF.Program (functions, mainExpr)) = optimized
                let printed = PrintInsertion.insertPrint functions mainExpr programType
                let convResultOptimized = { convResult with Program = printed }
                match RefCountInsertion.insertRCInProgram convResultOptimized with
                | Error e -> Error $"RC insertion error: {e}"
                | Ok (anfAfterRC, typeMap) ->
                    let anfAfterTCO = TailCallDetection.detectTailCallsInProgram anfAfterRC

                    // Convert to MIR
                    match ANF_to_MIR.toMIR anfAfterTCO typeMap Map.empty programType convResultOptimized.VariantLookup (TypeRegistries.recordFieldsRegistry convResultOptimized.TypeReg) false (returnTypesFor stdlib) with
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
                            Ok (formatLIRForOptimizationTest syntheticMain optimizedLir)

/// Run a single optimization test
let runOptimizationTest (stdlib: CompilationContexts.StdlibResult) (test: OptimizationTest) : OptimizationTestResult =
    let sourceIRResult =
        match test.Stage, test.Input with
        | ANF, Source source -> getOptimizedANF stdlib source
        | ANF, StdlibFunction functionName -> getOptimizedStdlibANF stdlib functionName
        | MIR, Source source -> getOptimizedMIR stdlib source
        | LIR, Source source -> getOptimizedLIR stdlib source
        | MIR, StdlibFunction _ | LIR, StdlibFunction _ -> Error "STDLIB-FUNCTION is supported only for ANF optimization tests"
        | DirectLIR, _ | DirectARM64, _ | DirectLIR2X64, _ -> Error "Direct optimization stages use structural comparison"

    let structuralResult =
        match test.Stage, test.Input with
        | DirectLIR, Source source ->
            match parseLIR source, parseLIR test.ExpectedIR with
            | Error e, _ -> Some (Error $"Failed to parse INPUT LIR: {e}")
            | _, Error e -> Some (Error $"Failed to parse EXPECTED LIR: {e}")
            | Ok input, Ok expected ->
                let actual = LIR_Peephole.optimizeProgram input
                if actual = expected then Some (Ok ())
                else
                    Some (Error $"LIR mismatch\nExpected:\n{formatLIR expected}\nActual:\n{formatLIR actual}")
        | DirectARM64, Source source ->
            match parseARM64Symbolic source, parseARM64Symbolic test.ExpectedIR with
            | Error e, _ -> Some (Error $"Failed to parse INPUT ARM64: {e}")
            | _, Error e -> Some (Error $"Failed to parse EXPECTED ARM64: {e}")
            | Ok input, Ok expected ->
                let actual = ARM64Peephole.peepholeOptimize input
                if actual = expected then Some (Ok ())
                else
                    let render instrs =
                        instrs
                        |> List.map TestDSL.PassTestRunner.prettyPrintARM64Instr
                        |> String.concat "\n"
                    Some (Error $"ARM64 mismatch\nExpected:\n{render expected}\nActual:\n{render actual}")
        | DirectLIR2X64, Source source ->
            match parseLIR source, parseX64 test.ExpectedIR with
            | Error e, _ -> Some (Error $"Failed to parse INPUT LIR: {e}")
            | _, Error e -> Some (Error $"Failed to parse EXPECTED x64: {e}")
            | Ok (LIR.Program ([func], _, _) as input), Ok expected ->
                let rec containsSequence remaining =
                    if List.length remaining < List.length expected then false
                    elif List.take (List.length expected) remaining = expected then true
                    else containsSequence (List.tail remaining)
                match CodeGen_X86_64.translateProgram input false with
                | Error e -> Some (Error $"x64 lowering failed: {e}")
                | Ok emitted ->
                    let functionBody =
                        emitted
                        |> List.skipWhile ((<>) (X86_64.Label func.Name))
                        |> List.takeWhile ((<>) (X86_64.Label $"_epilogue_{func.Name}"))
                    if containsSequence functionBody then Some (Ok ())
                    else Some (Error $"Expected x64 instruction sequence was not selected in {func.Name}\nExpected sequence: {expected}\nActual function: {functionBody}")
            | Ok _, Ok _ -> Some (Error "INPUT LIR must contain exactly one function")
        | DirectLIR, StdlibFunction _ | DirectARM64, StdlibFunction _ | DirectLIR2X64, StdlibFunction _ ->
            Some (Error "Direct optimization stages require an INPUT section")
        | (ANF | MIR | LIR), _ -> None

    match structuralResult with
    | Some (Ok ()) ->
        { Success = true; Message = "Test passed"; Expected = None; Actual = None }
    | Some (Error e) ->
        { Success = false; Message = e; Expected = Some test.ExpectedIR; Actual = None }
    | None ->
        match sourceIRResult with
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
let runTestFile (stdlib: CompilationContexts.StdlibResult) (stage: IRStage) (path: string) : Result<(OptimizationTest * OptimizationTestResult) list, string> =
    match parseTestFile stage path with
    | Error e -> Error e
    | Ok tests ->
        let results = tests |> List.map (fun test -> (test, runOptimizationTest stdlib test))
        Ok results
