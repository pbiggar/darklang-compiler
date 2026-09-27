// PreambleAnalysis.fs - Check reusable source preambles against explicit base environments.

module PreambleAnalysis

open CompilationContexts

/// Parse and check a preamble directly from interpreter syntax.
let analyzePreamble
    (allowInternal: bool)
    (stdlib: StdlibResult)
    (preamble: string)
    : Result<PreambleAnalysis, string> =
    WrittenParsing.parse LibParser.Validation.Script preamble
    |> Result.mapError (fun err -> $"Preamble parse error: {err}")
    |> Result.bind (fun preambleAst ->
        WrittenChecking.checkSourceUnitsWithBase
            stdlib.Context.WrittenEnvironment
            allowInternal
            false
            [preambleAst]
        |> Result.mapError (fun typeErr -> $"Preamble type error: {typeErr}")
        |> Result.map (fun (_programType, typedPreambleAst, writtenEnvironment) ->
            let preambleTypeCheckEnv =
                CheckingTypes.mergeTypeCheckEnv
                    stdlib.Context.TypeCheckEnv
                    (WrittenChecking.typeCheckEnvironment typedPreambleAst)
            let preambleGenericDefs =
                SpecializationIdentity.extractGenericFuncDefs typedPreambleAst
            {
                TypedAST = typedPreambleAst
                TypeCheckEnv = preambleTypeCheckEnv
                WrittenEnvironment = Some writtenEnvironment
                GenericFuncDefs = preambleGenericDefs
            }))
