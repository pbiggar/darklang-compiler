// UserCompilation.fs - Compile a user source unit through the typed pipeline stages.

module UserCompilation

open ARM64CodeGenTypes
open CodeGen
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open Output
open CompilerOptions
open CompilationCacheIdentity
open CompilationSession
open PipelineDiagnostics
open NativePipeline
open ANFPipeline
open BinaryOutput
open CompilationContexts
open SourcePreparation
open PreambleAnalysis
open PackageCatalog

/// Compile a user/test program against a prebuilt stdlib/preamble context
let internal compileUserWithPlan (plan: UserCompilePlan) : CompileReport =
    let sw = Stopwatch.StartNew()
    let result =
        try
            // Pass 1: Parse user code only
            if plan.Verbosity >= 1 then println plan.Labels.Parse
            let parseResult =
                parseSourceProgram plan.AllowInternal plan.Sources
                |> Result.map snd
            let parseTime = sw.Elapsed.TotalMilliseconds
            recordPassTiming plan.PassTimingRecorder "Parse" parseTime
            if plan.Verbosity >= 2 then
                let t = System.Math.Round(parseTime, 1)
                println $"        {t}ms"

            match parseResult with
            | Error err -> Error $"Parse error: {err}"
            | Ok userAst ->
                // Pass 1.5: Type Checking (user code with base TypeCheckEnv)
                if plan.Verbosity >= 1 then println plan.Labels.TypeCheck
                let typeCheckResult =
                    checkProgramWithBaseEnv
                        plan.PassTimingRecorder
                        plan.Options.Warnings
                        plan.BaseContext.TypeCheckEnv
                        userAst
                let typeCheckTime = sw.Elapsed.TotalMilliseconds - parseTime
                recordPassTiming plan.PassTimingRecorder "Type Checking" typeCheckTime
                if plan.Verbosity >= 2 then
                    let t = System.Math.Round(typeCheckTime, 1)
                    println $"        {t}ms"

                match typeCheckResult with
                | Error typeErr -> Error (CheckingDiagnostics.typeErrorToString typeErr)
                | Ok (programType, _, _) when
                    plan.Mode = FullProgram
                    && programType <> AST.TUnit
                    && programType <> AST.TInt64
                    && programType <> AST.TInt ->
                    Error
                        $"File entry expression must return Unit, Int, or Int64; got {CheckingDiagnostics.typeToString programType}"
                | Ok (programType, typedUserAst, userEnv) ->
                    let jsonPlanningStart = sw.Elapsed.TotalMilliseconds
                    let plannedUserAst =
                        JsonPlanning.rewriteProgramWithSession
                            (plan.Session |> Option.map (fun session -> session.JsonPlanning))
                            userEnv
                            typedUserAst
                    let jsonPlanningElapsed = sw.Elapsed.TotalMilliseconds - jsonPlanningStart
                    recordPassTiming plan.PassTimingRecorder "JSON Planning" jsonPlanningElapsed
                    let valueRenderingStart = Stopwatch.StartNew()
                    let plannedProgramType = CheckingTypes.resolveType userEnv.AliasReg programType
                    let renderedUserAst, boundaryProgramType =
                        if plan.Mode = FullProgram then
                            (plannedUserAst, plannedProgramType)
                        else if plannedProgramType = AST.TUnit then
                            (plannedUserAst, AST.TUnit)
                        else
                            (ValueRendering.rewriteProgram
                                userEnv.IndexedTypeReg
                                userEnv.IndexedSumTypeReg
                                plan.BaseContext.Registries.FuncReg
                                plannedProgramType
                                plannedUserAst,
                             AST.TString)
                    valueRenderingStart.Stop()
                    recordPassTiming
                        plan.PassTimingRecorder
                        "Value Rendering"
                        valueRenderingStart.Elapsed.TotalMilliseconds
                    if plan.Verbosity >= 3 then
                        println $"Program type: {CheckingDiagnostics.typeToString programType}"
                        println ""

                    // Pass 2: AST → ANF (user only)
                    if plan.Verbosity >= 1 then println plan.Labels.Anf
                    let catalogStart = Stopwatch.StartNew()
                    let materializedProgramResult =
                        materializePackageValueCatalog
                            plan.BaseContext
                            plan.Options.Warnings
                            plan.PackageValues
                            renderedUserAst
                    catalogStart.Stop()
                    recordPassTiming
                        plan.PassTimingRecorder
                        "AST -> ANF Package Catalog"
                        catalogStart.Elapsed.TotalMilliseconds
                    let userOnlyResult =
                        materializedProgramResult
                        |> Result.bind (fun materializedProgram ->
                            convertTypedProgramToUserOnlyWithMode
                                plan.BaseContext
                                plan.Monomorphization
                                userEnv
                                plan.Session
                                plan.PassTimingRecorder
                                materializedProgram)
                    let anfTime = sw.Elapsed.TotalMilliseconds - parseTime - typeCheckTime
                    recordPassTiming plan.PassTimingRecorder "AST -> ANF" anfTime
                    if plan.Verbosity >= 2 then
                        let t = System.Math.Round(anfTime, 1)
                        println $"        {t}ms"

                    match userOnlyResult with
                    | Error err -> Error $"ANF conversion error: {err}"
                    | Ok (userOnly, dependencyIdentity) ->
                        let functionsToCompile =
                            userOnly.UserFunctions
                            |> List.filter (fun f -> not (Set.contains f.Name plan.SkipFunctionNames))

                        let dependencyRoots =
                            functionsToCompile
                            |> List.choose (fun func ->
                                if func.Name.StartsWith("__dark_json_")
                                   || func.Name.StartsWith("__dark_eq_")
                                   || Set.contains func.Name userOnly.NonInlineableFunctionNames then
                                    Some func.Name
                                else
                                    None)
                            |> Set.ofList
                        let dependencyNames =
                            CallGraphReachability.findReachable
                                (ANFDeadCodeElimination.buildCallGraph functionsToCompile)
                                dependencyRoots
                        let dependencyFunctions, programFunctions =
                            functionsToCompile
                            |> List.partition (fun func -> Set.contains func.Name dependencyNames)

                        if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                            println $"  [COMPILE] {programFunctions.Length} program functions compiled fresh"
                            for f in functionsToCompile do
                                println $"    - {f.Name}"

                        let programEntryName = "__dark_compiler_program_entry"
                        let hasReservedName =
                            functionsToCompile
                            |> List.exists (fun func -> func.Name = programEntryName)
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
                        let externalReturnTypes =
                            mergeReturnTypes plan.BaseContext.ReturnTypes userOnly.LocalReturnTypes
                        let releasePlanSummaryCache =
                            plan.Session
                            |> Option.map (fun current ->
                                fun includeStaticRootDependencies releasePlanCacheKey releasePlan generate ->
                                    current.Arm64ReleasePlanSummary
                                        includeStaticRootDependencies
                                        releasePlanCacheKey
                                        releasePlan
                                        generate)
                        let dependencyFunctionCaches : FunctionCompilationCaches option =
                            plan.Session
                            |> Option.filter (fun _ -> not plan.Options.EnableCoverage)
                            |> Option.map (fun current ->
                                {
                                    ConvertSsa =
                                        fun func convert ->
                                            current.ConvertMirFunctionToSsa func convert
                                    OptimizeMir =
                                        fun key optimize ->
                                            current.OptimizeMirFunction key optimize
                                    AllocateLir =
                                        fun arch func allocate ->
                                            current.AllocateLirFunction arch func allocate
                                })
                        let mirRegistryTimer = Stopwatch.StartNew()
                        let projectedMirRegistries =
                            match plan.Session with
                            | Some current ->
                                current.ProjectMirRegistries
                                    dependencyIdentity
                                    plan.BaseContext.ProjectedMirRegistries
                                    userOnly.LocalVariantLookup
                                    userOnly.LocalRecordFieldsReg
                            | None ->
                                projectMirRegistryOverlay
                                    plan.BaseContext.ProjectedMirRegistries
                                    userOnly.LocalVariantLookup
                                    userOnly.LocalRecordFieldsReg
                        mirRegistryTimer.Stop()
                        recordPassTiming
                            plan.PassTimingRecorder
                            "MIR Registry Projection Preparation"
                            mirRegistryTimer.Elapsed.TotalMilliseconds

                        let compileDependencyFunctions () =
                            buildAnf
                                plan.Verbosity
                                plan.Options
                                sw
                                userRegistries
                                ANF_Inlining.defaultConfig
                                plan.ExternalInlineCandidates
                                userOnly.NonInlineableFunctionNames
                                dependencyFunctions
                                true
                                plan.PassTimingRecorder
                            |> Result.bind (fun (anfDependencies, dependencyTypeMap) ->
                                let tcoDependencies =
                                    applyTco
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        userRegistries.RecursiveMembers
                                        anfDependencies
                                        plan.PassTimingRecorder
                                lowerToAllocatedLir
                                    plan.BaseContext.Target
                                    plan.Verbosity
                                    plan.Options
                                    sw
                                    plan.PassTimingRecorder
                                    dependencyFunctionCaches
                                    releasePlanSummaryCache
                                    plan.Labels.StageSuffix
                                    tcoDependencies
                                    dependencyTypeMap
                                    userRegistries
                                    (Some projectedMirRegistries)
                                    externalReturnTypes)

                        let dependencyLirResult =
                            if List.isEmpty dependencyFunctions then
                                Ok []
                            else
                                match plan.Session with
                                | Some current ->
                                    current.CompileDependencies
                                        dependencyIdentity
                                        {
                                            Target = plan.BaseContext.Target
                                            Options = plan.Options
                                            NonInlineableFunctionNames = userOnly.NonInlineableFunctionNames
                                        }
                                        compileDependencyFunctions
                                | None ->
                                    compileDependencyFunctions ()

                        let programEntry =
                            AST_to_ANF.synthesizeEntryFunction
                                programEntryName
                                boundaryProgramType
                                userOnly.MainExpr
                        let programAnfResult =
                            if hasReservedName then
                                Error $"Function name '{programEntryName}' is reserved"
                            else
                                buildAnf
                                    plan.Verbosity
                                    plan.Options
                                    sw
                                    userRegistries
                                    ANF_Inlining.defaultConfig
                                    plan.ExternalInlineCandidates
                                    userOnly.NonInlineableFunctionNames
                                    (programEntry :: programFunctions)
                                    true
                                    plan.PassTimingRecorder
                        match dependencyLirResult, programAnfResult with
                        | Error err, _
                        | _, Error err -> Error err
                        | Ok allocatedDependencyFuncs, Ok (programAnfFunctions, programTypeMap) ->
                            if plan.Verbosity >= 1 then println "  [anf.print-result] Print Insertion..."
                            let printStart = sw.Elapsed.TotalMilliseconds
                            let printResult =
                                match plan.Mode with
                                | FullProgram -> Ok programAnfFunctions
                                | TestExpression ->
                                    PrintInsertion.insertPrintInEntry
                                        programEntryName
                                        boundaryProgramType
                                        programAnfFunctions
                            match printResult with
                            | Error err -> Error $"Print insertion error: {err}"
                            | Ok printedFunctions ->
                                let printElapsed = sw.Elapsed.TotalMilliseconds - printStart
                                recordPassTiming plan.PassTimingRecorder "Print Insertion" printElapsed
                                if plan.Verbosity >= 2 then
                                    let t = System.Math.Round(printElapsed, 1)
                                    println $"        {t}ms"
                                if shouldDumpIR plan.Verbosity plan.Options.DumpANF then
                                    let printProgram = ANF.Program (printedFunctions, ANF.Return ANF.UnitLiteral)
                                    printANFProgram plan.Options "=== ANF (after Print insertion) ===" printProgram

                                let tcoProgramFunctions =
                                    applyTco
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        userRegistries.RecursiveMembers
                                        printedFunctions
                                        plan.PassTimingRecorder
                                let programLirResult =
                                    lowerToAllocatedLir
                                        plan.BaseContext.Target
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        plan.PassTimingRecorder
                                        None
                                        releasePlanSummaryCache
                                        plan.Labels.StageSuffix
                                        tcoProgramFunctions
                                        programTypeMap
                                        userRegistries
                                        (Some projectedMirRegistries)
                                        externalReturnTypes
                                let startResultId = ANF.TempId 0
                                let startFunction =
                                    AST_to_ANF.synthesizeEntryFunction
                                        "_start"
                                        boundaryProgramType
                                        (ANF.Let (
                                            startResultId,
                                            ANF.Call (programEntryName, []),
                                            ANF.Return (ANF.Var startResultId)))
                                let startRegistries = {
                                    userRegistries with
                                        FuncReg =
                                            Map.add
                                                programEntryName
                                                (AST.TFunction ([], boundaryProgramType))
                                                userRegistries.FuncReg
                                        FuncParams =
                                            Map.add programEntryName [] userRegistries.FuncParams
                                }
                                let compileStart () =
                                    buildAnf
                                        plan.Verbosity
                                        plan.Options
                                        sw
                                        startRegistries
                                        ANF_Inlining.defaultConfig
                                        Map.empty
                                        Set.empty
                                        [startFunction]
                                        false
                                        plan.PassTimingRecorder
                                    |> Result.bind (fun (startAnf, startTypeMap) ->
                                        let tcoStart =
                                            applyTco
                                                plan.Verbosity
                                                plan.Options
                                                sw
                                                startRegistries.RecursiveMembers
                                                startAnf
                                                plan.PassTimingRecorder
                                        lowerToAllocatedLir
                                            plan.BaseContext.Target
                                            plan.Verbosity
                                            plan.Options
                                            sw
                                            plan.PassTimingRecorder
                                            None
                                            releasePlanSummaryCache
                                            plan.Labels.StageSuffix
                                            tcoStart
                                            startTypeMap
                                            startRegistries
                                            (Some projectedMirRegistries)
                                            (Map.add programEntryName boundaryProgramType externalReturnTypes))
                                let startLirResult =
                                    match plan.Session with
                                    | Some current ->
                                        current.CompileStart
                                            {
                                                Target = plan.BaseContext.Target
                                                Options = plan.Options
                                                BoundaryProgramType = boundaryProgramType
                                            }
                                            compileStart
                                    | None ->
                                        compileStart ()
                                match programLirResult, startLirResult with
                                | Error err, _
                                | _, Error err -> Error err
                                | Ok allocatedProgramFuncs, Ok allocatedStartFuncs ->
                                    let allocatedUserFuncs =
                                        allocatedStartFuncs
                                        @ allocatedProgramFuncs
                                        @ allocatedDependencyFuncs
                                    let allSymbolicUserFuncs = plan.PrebuiltSymbolicFunctions @ allocatedUserFuncs
                                    let userCallGraphStart = sw.Elapsed.TotalMilliseconds
                                    let userCallGraph =
                                        if plan.Options.DisableFunctionTreeShaking then Map.empty
                                        else DeadCodeElimination.buildCallGraph allSymbolicUserFuncs
                                    let userCallGraphElapsed =
                                        sw.Elapsed.TotalMilliseconds - userCallGraphStart
                                    recordPassTiming
                                        plan.PassTimingRecorder
                                        "Function Tree Shaking"
                                        userCallGraphElapsed
                                    let finalUserFuncs =
                                        if plan.TreeShakeUserFunctions then
                                            if plan.Verbosity >= 1 then println "  [lir.tree-shake] Function Tree Shaking..."
                                            let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                            let shakenUserFuncs =
                                                if plan.Options.DisableFunctionTreeShaking then
                                                    allSymbolicUserFuncs
                                                else
                                                    FunctionTreeShaking.filterUserFunctionsWithCallGraph
                                                        (Some "_start")
                                                        userCallGraph
                                                        allSymbolicUserFuncs
                                            let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                            recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                            shakenUserFuncs
                                        else
                                            allSymbolicUserFuncs

                                    if plan.EmitFunctionEvents && plan.Verbosity >= 3 then
                                        println $"  [COMBINED] fresh: {allocatedUserFuncs.Length}, total: {allSymbolicUserFuncs.Length}"
                                        for f in allSymbolicUserFuncs do
                                            println $"    - {f.Name}"
                                        println $"  [TreeShaking] user funcs: {finalUserFuncs.Length}"

                                    // Filter stdlib functions to only include reachable ones (dead code elimination)
                                    let reachableStdlib =
                                        if plan.Options.DisableFunctionTreeShaking then plan.Stdlib.AllocatedFunctions
                                        else
                                            let treeShakeStart = sw.Elapsed.TotalMilliseconds
                                            let filtered =
                                                match plan.Session with
                                                | Some current ->
                                                    current.ReachableStdlibFunctions
                                                        (box plan.Stdlib)
                                                        userCallGraph
                                                        finalUserFuncs
                                                        plan.Stdlib.StdlibCallGraph
                                                        plan.Stdlib.AllocatedFunctions
                                                | None ->
                                                    FunctionTreeShaking.filterStdlibFunctionsWithUserCallGraph
                                                        plan.Stdlib.StdlibCallGraph
                                                        userCallGraph
                                                        finalUserFuncs
                                                        plan.Stdlib.AllocatedFunctions
                                            filtered
                                            |> fun shakenStdlib ->
                                                let treeShakeElapsed = sw.Elapsed.TotalMilliseconds - treeShakeStart
                                                recordPassTiming plan.PassTimingRecorder "Function Tree Shaking" treeShakeElapsed
                                                shakenStdlib

                                    // Concrete helper and Stdlib specialization names encode their
                                    // complete type arguments. A user unit can request a function already
                                    // supplied by the prebuilt stdlib, with context-specific lowering
                                    // making the allocated bodies differ. Keep the stdlib copy, which is
                                    // first and has the complete stdlib registries. The same holds for a
                                    // user unit that declares a function the stdlib carries under a
                                    // non-Stdlib name (Darklang.LanguageTools.* is in both).
                                    let prebuiltStdlibNames =
                                        reachableStdlib |> List.map (fun func -> func.Name) |> Set.ofList
                                    let mergeFunctionsByName (functions: LIR.Function list) : LIR.Function list =
                                        functions
                                        |> List.fold
                                            (fun (names, retainedRev) func ->
                                                match Map.tryFind func.Name names with
                                                | None ->
                                                    (Map.add func.Name func names, func :: retainedRev)
                                                | Some existing when existing = func ->
                                                    (names, retainedRev)
                                                | Some _
                                                    when func.Name.StartsWith("__dark_eq_")
                                                         || func.Name.StartsWith("__dark_compare_")
                                                         || func.Name.StartsWith("Stdlib.")
                                                         || Set.contains func.Name prebuiltStdlibNames ->
                                                    (names, retainedRev)
                                                | Some _ ->
                                                    Crash.crash $"Conflicting allocated LIR functions named '{func.Name}'")
                                            (Map.empty, [])
                                        |> snd
                                        |> List.rev

                                    // x64 emits one ELF symbol per function name, so discard duplicate
                                    // specializations there. ARM64 emission has historically retained the
                                    // user copies; preserving that selection also preserves specialization.
                                    let allFuncs, retainedUserFuncs =
                                        match plan.BaseContext.Target with
                                        | Platform.LinuxX86_64 ->
                                            let retainedStdlibNames =
                                                reachableStdlib
                                                |> List.map (fun func -> func.Name)
                                                |> Set.ofList
                                            (reachableStdlib @ finalUserFuncs |> mergeFunctionsByName,
                                             finalUserFuncs
                                             |> List.filter (fun func ->
                                                 not (Set.contains func.Name retainedStdlibNames)))
                                        | Platform.ARM64Backend _ ->
                                            (reachableStdlib @ finalUserFuncs, finalUserFuncs)
                                    let reachableDependencyFuncs, reachableProgramFuncs =
                                        retainedUserFuncs
                                        |> List.partition (fun func ->
                                            Set.contains func.Name dependencyNames)
                                    let lirVariantRegistry : LIR.VariantRegistry =
                                        userEnv.IndexedSumTypeReg
                                        |> Map.map (fun _ info ->
                                            ({ TypeParams = info.TypeParams
                                               Variants =
                                                info.Variants
                                                |> List.map (fun variant ->
                                                    ({ Name = variant.Name
                                                       Tag = variant.Tag
                                                       Payload = variant.Payload }
                                                        : LIR.VariantInfo)) }
                                                : LIR.TypeVariants))
                                    let allocatedProgram =
                                        LIR.Program (
                                            allFuncs,
                                            lirVariantRegistry,
                                            userRegistries.RecordFieldsReg
                                        )
                                    let freshProgramContextIdentity = box allocatedProgramFuncs
                                    let startProgramFuncs, otherProgramFuncs =
                                        reachableProgramFuncs
                                        |> List.partition (fun func -> func.Name = "_start")
                                    let functionGroups : CodeGen.FunctionGroup list =
                                        [
                                            {
                                                ContextIdentity = freshProgramContextIdentity
                                                ReusableAcrossCompilations = false
                                                Functions = startProgramFuncs
                                            }
                                            {
                                                ContextIdentity = box reachableStdlib
                                                ReusableAcrossCompilations = true
                                                Functions = reachableStdlib
                                            }
                                            {
                                                ContextIdentity = freshProgramContextIdentity
                                                ReusableAcrossCompilations = false
                                                Functions = otherProgramFuncs
                                            }
                                            {
                                                ContextIdentity = dependencyIdentity
                                                ReusableAcrossCompilations = false
                                                Functions = reachableDependencyFuncs
                                            }
                                        ]
                                        |> List.filter (fun group -> not (List.isEmpty group.Functions))
                                    if shouldDumpIR plan.Verbosity plan.Options.DumpLIR then
                                        printLIRProgram plan.Options "=== LIR (After Register Allocation) ===" allocatedProgram

                                    let binaryResult =
                                        generateBinary
                                            plan.BaseContext.Target
                                            plan.Verbosity
                                            plan.Options
                                            sw
                                            plan.PassTimingRecorder
                                            "  [backend.codegen] Code Generation..."
                                            "  [backend.emit] ARM64 Emit ({format})..."
                                            false
                                            false
                                            plan.Session
                                            dependencyIdentity
                                            functionGroups
                                            ([
                                                {
                                                    CodeGen.ContextIdentity = box plan.BaseContext
                                                    Functions = reachableStdlib
                                                }
                                                {
                                                    CodeGen.ContextIdentity = dependencyIdentity
                                                    Functions = reachableProgramFuncs
                                                }
                                                {
                                                    CodeGen.ContextIdentity = dependencyIdentity
                                                    Functions = reachableDependencyFuncs
                                                }
                                             ]
                                             |> List.filter (fun group -> not (List.isEmpty group.Functions)))
                                            userRegistries.RcSumShapeReg
                                            allocatedProgram
                                    match binaryResult with
                                    | Error err -> Error err
                                    | Ok binary ->
                                        Ok binary
        with
        | ex ->
            Error $"Compilation failed: {ex.Message}"
    sw.Stop()
    match result with
    | Ok _ when plan.Verbosity >= 1 ->
        println $"  ✓ Compilation complete ({System.Math.Round(sw.Elapsed.TotalMilliseconds, 1)}ms)"
    | _ -> ()
    { Target = plan.BaseContext.Target; Result = result; CompileTime = sw.Elapsed }
