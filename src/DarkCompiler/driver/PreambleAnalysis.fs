// PreambleAnalysis.fs - Check reusable source preambles against explicit base environments.

module PreambleAnalysis

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationSession
open CompilationContexts

let internal checkProgramWithBaseEnv
    (passTimingRecorder: PassTimingRecorder option)
    (warningSettings: AST.WarningSettings)
    (baseEnv: CheckingTypes.TypeCheckEnv)
    (program: AST.Program)
    : Result<AST.Type * CheckedAST.Program * CheckingTypes.TypeCheckEnv, CheckingDiagnostics.TypeError> =
    match passTimingRecorder with
    | None ->
        TypeChecking.checkProgramWithBaseEnvAndSettings baseEnv true warningSettings program
    | Some recorder ->
        TypeChecking.checkProgramWithBaseEnvAndSettingsWithTrace
            (fun phase elapsedMs ->
                recorder {
                    Pass = phase
                    Elapsed = TimeSpan.FromMilliseconds elapsedMs
                })
            baseEnv
            true
            warningSettings
            program

let private checkSyntheticPreambleWithBaseEnv
    (warningSettings: AST.WarningSettings)
    (baseEnv: CheckingTypes.TypeCheckEnv)
    (program: AST.Program)
    : Result<AST.Type * CheckedAST.Program * CheckingTypes.TypeCheckEnv, CheckingDiagnostics.TypeError> =
    TypeChecking.checkSyntheticPreambleWithBaseEnvAndSettings
        baseEnv
        true
        warningSettings
        program

/// Parse and typecheck a preamble, returning typed AST + preamble typecheck env
let analyzePreamble
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    : Result<PreambleAnalysis, string> =
    Parser.parseString allowInternal preamble
    |> Result.mapError (fun err -> $"Preamble parse error: {err}")
    |> Result.bind (fun preambleAst ->
        checkSyntheticPreambleWithBaseEnv
            defaultWarningSettings
            stdlib.Context.TypeCheckEnv
            preambleAst
        |> Result.mapError (fun typeErr -> $"Preamble type error: {CheckingDiagnostics.typeErrorToString typeErr}")
        |> Result.map (fun (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
            let preambleGenericDefs = SpecializationIdentity.extractGenericFuncDefs typedPreambleAst
            {
                TypedAST = typedPreambleAst
                TypeCheckEnv = preambleTypeCheckEnv
                GenericFuncDefs = preambleGenericDefs
            }))

/// Load a .dark file allowing internal identifiers (for stdlib sources)
let internal loadDarkFileAllowInternal (filename: string) : Result<AST.Program, string> =
    let exePath = Assembly.GetExecutingAssembly().Location
    let exeDir = Path.GetDirectoryName(exePath)
    let possiblePaths = [
        Path.Combine(exeDir, filename)
        Path.Combine(exeDir, "..", "..", "..", "..", "src", "DarkCompiler", filename)
        Path.Combine(Environment.CurrentDirectory, "src", "DarkCompiler", filename)
    ]
    let filePath = possiblePaths |> List.tryFind File.Exists
    match filePath with
    | None ->
        let pathsStr = String.Join(", ", possiblePaths)
        Error $"Could not find {filename} in any of: {pathsStr}"
    | Some path ->
        let source = File.ReadAllText(path)
        Parser.parseString true source
        |> Result.mapError (fun err -> $"Error parsing {filename}: {err}")
