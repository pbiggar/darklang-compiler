// Reachability.fs - Query standard-library reachability through the compilation pipeline.

module CompilerReachability

open ARM64CodeGenTypes
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationSession
open ANFPipeline
open CompilationContexts
open SourcePreparation
open PackageCatalog

/// Get all stdlib function names from the prebuilt stdlib
let getAllStdlibFunctionNamesFromStdlib (stdlib: StdlibResult) : Set<string> =
    stdlib.StdlibANFFunctions |> Map.keys |> Set.ofSeq

/// Get the set of stdlib function names reachable from user code (using prebuilt stdlib)
/// Used for coverage analysis without re-compiling stdlib
let getReachableStdlibFunctionsFromStdlib (stdlib: StdlibResult) (source: string) : Result<Set<string>, string> =
    // Parse user code
    match Parser.parseString false source with
    | Error err -> Error $"Parse error: {err}"
    | Ok userAst ->
        // Type check with stdlib environment
        match TypeChecking.checkPublicProgramWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            false
            defaultWarningSettings
            userAst with
        | Error typeErr -> Error (CheckingDiagnostics.typeErrorToString typeErr)
        | Ok (programType, typedUserAst, userEnv) ->
            let plannedUserAst = JsonPlanning.rewriteProgram userEnv typedUserAst
            let plannedProgramType = CheckingTypes.resolveType userEnv.AliasReg programType
            let renderedUserAst, boundaryProgramType =
                if plannedProgramType = AST.TUnit then
                    (plannedUserAst, AST.TUnit)
                else
                    (ValueRendering.rewriteProgram
                        userEnv.IndexedTypeReg
                        userEnv.IndexedSumTypeReg
                        stdlib.Context.Registries.FuncReg
                        plannedProgramType
                        plannedUserAst,
                     AST.TString)
            // Convert to ANF
            match convertTypedProgramToUserOnly stdlib.Context renderedUserAst with
            | Error err -> Error $"ANF conversion error: {err}"
            | Ok userOnly ->
                let coverageOptions = { defaultOptions with DisableANFOpt = true; DisableInlining = true }
                let sw = Stopwatch.StartNew()
                let entryFunction =
                    AST_to_ANF.synthesizeEntryFunction "_start" boundaryProgramType userOnly.MainExpr
                let userRegistries : AST_to_ANF.Registries = {
                    ScopeContracts = userOnly.ScopeContracts
                    TypeReg = userOnly.TypeReg
                    RecordFieldsReg = userOnly.RecordFieldsReg
                    RecordTypeParamsReg = userOnly.RecordTypeParamsReg
                    VariantLookup = userOnly.VariantLookup
                    SumTypeNames = userOnly.SumTypeNames
                    RcSumShapeReg = userOnly.RcSumShapeReg
                    FuncReg = userOnly.FuncReg
                    FuncParams = userOnly.FuncParams
                    ModuleRegistry = userOnly.ModuleRegistry
                    RecursiveMembers = userOnly.RecursiveMembers
                }
                PrintInsertion.insertPrintInEntry
                    "_start"
                    boundaryProgramType
                    (entryFunction :: userOnly.UserFunctions)
                |> Result.mapError (fun err -> $"Print insertion error: {err}")
                |> Result.bind (fun printedFunctions ->
                    buildAnf
                        0
                        coverageOptions
                        sw
                        userRegistries
                        ANF_Inlining.defaultConfig
                        Map.empty
                        userOnly.NonInlineableFunctionNames
                        printedFunctions
                        false
                        None)
                |> Result.map (fun (userFunctions, _typeMap) ->
                    let tcoFunctions =
                        applyTco 0 coverageOptions sw userRegistries.RecursiveMembers userFunctions None
                    ANFDeadCodeElimination.getReachableStdlib
                        stdlib.StdlibANFCallGraph
                        tcoFunctions)
