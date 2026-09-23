// CompilerLibrary.fs - Compile a validated request using its explicit source-context plan.

module CompilerLibrary

open ARM64CodeGenTypes
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationCacheIdentity
open CompilationSession
open CompilationContexts
open SourcePreparation
open PackageCatalog
open UserCompilation

let private labelsForMode (mode: CompileMode) : UserCompileLabels =
    match mode with
    | FullProgram ->
        {
            Parse = "  [frontend.parse] Parse..."
            TypeCheck = "  [frontend.type-check] Type Checking (with stdlib env)..."
            Anf = "  [anf.lower] AST → ANF (user only)..."
            StageSuffix = "user only"
        }
    | TestExpression ->
        {
            Parse = "  [frontend.parse] Parse (test expr only)..."
            TypeCheck = "  [frontend.type-check] Type Checking (with preamble env)..."
            Anf = "  [anf.lower] AST → ANF (test expr only)..."
            StageSuffix = ""
        }

let private buildCompilePlan (request: CompileRequest) : UserCompilePlan =
    let (stdlib, baseContext, prebuiltSymbolic, prebuiltCallGraph, skipNames) =
        match request.Context with
        | StdlibOnly stdlib ->
            stdlib, stdlib.Context, [], Map.empty, Set.empty
        | StdlibWithPreamble (stdlib, preambleCtx) ->
            let preambleFuncs = preambleCtx.SymbolicFunctions
            let preambleFuncNameSet =
                preambleFuncs |> List.map (fun f -> f.Name) |> Set.ofList
            stdlib, preambleCtx.Context, preambleFuncs, preambleCtx.SymbolicCallGraph, preambleFuncNameSet

    let emitFunctionEvents, treeShakeUserFunctions =
        match request.Mode with
        | FullProgram -> false, false
        | TestExpression -> true, true

    let monomorphization =
        match request.Mode with
        | FullProgram -> Monomorphize (Some baseContext.GenericFuncDefs)
        | TestExpression -> SpecializeLocalAndReplace baseContext.SpecRegistry

    {
        AllowInternal = request.AllowInternal
        Mode = request.Mode
        Verbosity = request.Verbosity
        Options = request.Options
        PackageValues = request.PackageValues
        PackageManager = request.PackageManager
        PassTimingRecorder = request.PassTimingRecorder
        Session = request.Session
        Stdlib = stdlib
        BaseContext = baseContext
        Monomorphization = monomorphization
        ExternalInlineCandidates = stdlib.StdlibInlineCandidates
        PrebuiltSymbolicFunctions = prebuiltSymbolic
        PrebuiltCallGraph = prebuiltCallGraph
        SkipFunctionNames = skipNames
        EmitFunctionEvents = emitFunctionEvents
        TreeShakeUserFunctions = treeShakeUserFunctions
        Labels = labelsForMode request.Mode
        Sources = request.Sources
    }

/// Compile source code to binary (in-memory, no file I/O)
let compile (request: CompileRequest) : CompileReport =
    let plan = buildCompilePlan request
    compileUserWithPlan plan
