// SourcePreparation.fs - Prepare checked declarations through specialization and closure lowering.

module SourcePreparation

open CodeGen
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions
open CompilationCacheIdentity
open CompilationSession
open PipelineDiagnostics
open ANFPipeline
open CompilationContexts

// Helper functions for exception-to-Result conversion (Darklang compatibility)

/// Extract return types from a FuncReg (FunctionRegistry maps func name -> full type)
/// This is needed because buildReturnTypeReg only includes functions in the current program,
/// but we need return types for all callable functions (including stdlib)
let internal extractReturnTypes
    (funcReg: TypeRegistries.FunctionRegistry)
    : Map<AST.FunctionId, string * AST.SemanticType> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (id, (name, typ)) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (id, (name, retType))
        | other -> Crash.crash $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

let private emptyRegistries (moduleRegistry: AST.ModuleRegistry) : AST_to_ANF.Registries =
    {
        ScopeContracts = Map.empty
        InertFunctionScopes = Set.empty
        TypeReg = Map.empty
        TypeNames = TypeRegistries.emptyTypeNames
        RecordFieldsReg = Map.empty
        RecordTypeParamsReg = Map.empty
        VariantLookup = Map.empty
        SumTypeNames = Set.empty
        RcSumShapeReg = Map.empty
        FuncReg = Map.empty
        FunctionIds = Map.empty
        FunctionNames = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = moduleRegistry
        RecursiveMembers = Map.empty
    }

let private liftLambdasWithBase
    (baseTypeReg: TypeRegistries.TypeRegistry)
    (baseVariantLookup: LoweringPrimitives.VariantLookup)
    (baseFunctions: LiftFunctions.FunctionCatalog)
    (passTimingRecorder: PassTimingRecorder option)
    (program: CheckedAST.Program)
    : Result<CheckedAST.Program, string> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordPassTiming passTimingRecorder name timer.Elapsed.TotalMilliseconds
        result
    measure "AST -> ANF Preparation: Lambda Lifting" (fun () ->
        LiftFunctions.liftLambdasInProgram
            baseTypeReg
            baseVariantLookup
            baseFunctions
            program)

let internal mergeSpecRegistries
    (baseRegistry: SpecializationIdentity.SpecRegistry)
    (overlayRegistry: SpecializationIdentity.SpecRegistry)
    : SpecializationIdentity.SpecRegistry =
    Map.fold (fun acc key value -> Map.add key value acc) baseRegistry overlayRegistry

let private collectLocalSpecs
    (genericDefs: SpecializationIdentity.GenericFuncDefs)
    (program: CheckedAST.Program)
    : Set<SpecializationIdentity.SpecKey> =
    let (CheckedAST.Program (symbols, topLevels)) = program
    let allSpecs =
        topLevels
        |> List.map (function
            | CheckedAST.FunctionDef f when List.isEmpty f.TypeParams -> Monomorphization.collectTypeAppsFromFunc symbols f
            | CheckedAST.ValueDef valueDef -> Monomorphization.collectTypeApps symbols (CheckedAST.valueDefBody valueDef)
            | CheckedAST.Expression e -> Monomorphization.collectTypeApps symbols e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty
    allSpecs
    |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

type internal MonomorphizationMode =
    | Monomorphize of SpecializationIdentity.GenericFuncDefs option
    | ReplaceTypeApps of SpecializationIdentity.SpecRegistry
    | SpecializeLocalAndReplace of SpecializationIdentity.SpecRegistry

/// Import inherited checked values before specialization so their bodies cross
/// every preparation boundary together with local declarations.
let private importInheritedValues
    (passTimingRecorder: PassTimingRecorder option)
    (inheritedValues: Map<string, CheckedValueArtifact>)
    (CheckedAST.Program (symbols, topLevels))
    : CheckedAST.Program =
    let currentNames =
        topLevels
        |> List.choose (function
            | CheckedAST.ValueDef valueDef -> Some valueDef.Name
            | _ -> None)
        |> Set.ofList
    let inheritedEntries =
        inheritedValues
        |> Map.toList
        |> List.filter (fun (name, _) -> not (Set.contains name currentNames))
    let inheritedDefinitions, symbols, compositionTicks, definitionTicks =
        inheritedEntries
        |> List.fold (fun (collected, symbols, compositionTicks, definitionTicks) (name, artifact) ->
            let compositionStart =
                if Option.isSome passTimingRecorder then Stopwatch.GetTimestamp() else 0L
            // Checked value bodies carry canonical IDs. Their reusable
            // artifacts retain only the cursor needed for later fresh binders.
            let symbols =
                CheckedAST.includeBindingCursor artifact.BindingCursor symbols
            let compositionTicks =
                if Option.isSome passTimingRecorder then
                    compositionTicks + Stopwatch.GetTimestamp() - compositionStart
                else compositionTicks
            let definitionStart =
                if Option.isSome passTimingRecorder then Stopwatch.GetTimestamp() else 0L
            let id, symbols = CheckedAST.internValue name symbols
            let definition =
                CheckedAST.ValueDef {
                    Id = id
                    Name = name
                    Type = CheckedAST.checkedType artifact.Type
                    Body = artifact.Body
                }
            let definitionTicks =
                if Option.isSome passTimingRecorder then
                    definitionTicks + Stopwatch.GetTimestamp() - definitionStart
                else definitionTicks
            (definition :: collected, symbols, compositionTicks, definitionTicks))
            ([], symbols, 0L, 0L)
        |> fun (definitions, symbols, compositionTicks, definitionTicks) ->
            (List.rev definitions, symbols, compositionTicks, definitionTicks)
    let milliseconds ticks = float ticks * 1000.0 / float Stopwatch.Frequency
    recordPassTiming
        passTimingRecorder
        "AST -> ANF Value Import: Binding Cursor Composition"
        (milliseconds compositionTicks)
    recordPassTiming
        passTimingRecorder
        "AST -> ANF Value Import: Definition Construction"
        (milliseconds definitionTicks)
    CheckedAST.Program (symbols, inheritedDefinitions @ topLevels)

/// Materialize checked module values as one lexical binding per execution
/// scope. This gives every reference ordinary value semantics through the
/// existing ANF ownership pipeline and leaves no value-only lowering cases.
let private materializeProgramValues
    (CheckedAST.Program (symbols, topLevels))
    : CheckedAST.Program =
    let currentValues =
        topLevels
        |> List.choose (function
            | CheckedAST.ValueDef valueDef ->
                Some (valueDef.Name, (valueDef.Id, valueDef.Type, valueDef.Body))
            | _ -> None)
    let rawBindings =
        currentValues
        |> List.map (fun (name, (id, _, body)) -> (name, (id, body)))
        |> List.map (fun (name, (id, body)) -> (name, id, body))
    // Checked top-level value references already carry canonical BindingIds.
    // The old name-based repair walked every body again, including programs
    // with no references requiring repair.
    let bindings = rawBindings
    let valueIds = bindings |> List.map (fun (_, id, _) -> id) |> Set.ofList
    let dependencies =
        bindings
        |> List.map (fun (_, id, body) ->
            id, Set.intersect valueIds (ClosureAnalysis.freeVars body Set.empty))
        |> Map.ofList
    // Outlining a literal replaces one cheap instruction with a call and can
    // regress runtime code. Share bodies that have actual lowering work.
    let cheapValueIds =
        bindings
        |> List.choose (fun (_, id, body) ->
            match body with
            | CheckedAST.UnitLiteral | CheckedAST.Int64Literal _ | CheckedAST.Int128Literal _
            | CheckedAST.Int8Literal _ | CheckedAST.Int16Literal _ | CheckedAST.Int32Literal _
            | CheckedAST.UInt8Literal _ | CheckedAST.UInt16Literal _ | CheckedAST.UInt32Literal _
            | CheckedAST.UInt64Literal _ | CheckedAST.UInt128Literal _ | CheckedAST.BigIntLiteral _
            | CheckedAST.BoolLiteral _ | CheckedAST.StringLiteral _ | CheckedAST.BlobLiteral _
            | CheckedAST.CharLiteral _ | CheckedAST.FloatLiteral _ | CheckedAST.Local _
            | CheckedAST.ListLiteral []
            | CheckedAST.DictLiteral (_, _, []) | CheckedAST.Constructor (_, []) -> Some id
            | _ -> None)
        |> Set.ofList
    let valueTypes = currentValues |> List.map (fun (_, (id, typ, _)) -> id, typ) |> Map.ofList
    let helperName name = $"__dark_value_materializer_{name}"
    let helperIds =
        bindings
        |> List.map (fun (name, id, _) -> id, AST.functionIdForName (helperName name))
        |> Map.ofList
    let bindingOrder =
        bindings
        |> List.mapi (fun index (_, id, _) -> id, index)
        |> Map.ofList
    let dependencyArgs =
        dependencies
        |> Map.map (fun _ required ->
            required
            |> Set.toList
            |> List.sortBy (fun dependencyId ->
                Map.tryFind dependencyId bindingOrder
                |> Option.defaultWith (fun () -> Crash.crash "Missing checked value dependency")))
    let wrap excluded body =
        let direct =
            Set.intersect valueIds (ClosureAnalysis.freeVars body Set.empty)
            |> fun values -> Set.difference values excluded
        if Set.isEmpty direct then (Set.empty, body)
        else
            let rec required pending needed =
                match pending with
                | [] -> needed
                | id :: rest when Set.contains id needed -> required rest needed
                | id :: rest ->
                    let next =
                        Map.tryFind id dependencies
                        |> Option.defaultValue Set.empty
                        |> fun values -> Set.difference values excluded
                        |> Set.toList
                    required (next @ rest) (Set.add id needed)
            let needed = required (Set.toList direct) Set.empty
            let selected = bindings |> List.filter (fun (_, id, _) -> Set.contains id needed)
            let rec orderByDependencies ordered remaining =
                match remaining with
                | [] -> ordered
                | _ ->
                    let remainingNames = remaining |> List.map (fun (_, id, _) -> id) |> Set.ofList
                    let ready =
                        remaining
                        |> List.filter (fun (_, id, _) ->
                            remainingNames
                            |> Set.remove id
                            |> Set.intersect (Map.tryFind id dependencies |> Option.defaultValue Set.empty)
                            |> Set.isEmpty)
                    match ready with
                    | [] ->
                        Crash.crash "Checked top-level values contain a cyclic materialization dependency"
                    | _ ->
                        let readyNames = ready |> List.map (fun (_, id, _) -> id) |> Set.ofList
                        let pending = remaining |> List.filter (fun (_, id, _) -> not (Set.contains id readyNames))
                        orderByDependencies (ordered @ ready) pending
            let ordered = orderByDependencies [] selected
            let materialized =
                List.foldBack (fun (_, id, value) result ->
                    let initializer =
                        if Set.contains id cheapValueIds then value
                        else
                            let arguments =
                                Map.tryFind id dependencyArgs
                                |> Option.defaultValue []
                                |> List.map CheckedAST.Local
                            let arguments =
                                match arguments with
                                | [] -> AST.NonEmptyList.singleton CheckedAST.UnitLiteral
                                | first :: rest -> { Head = first; Tail = rest }
                            let helperId =
                                Map.tryFind id helperIds
                                |> Option.defaultWith (fun () -> Crash.crash "Missing checked value materializer")
                            CheckedAST.Call (helperId, arguments)
                    CheckedAST.Let (CheckedAST.LPVariable id, initializer, result)) ordered body
            (needed, materialized)
    let materialized, usedValues =
        topLevels
        |> List.fold (fun (items, used) item ->
            match item with
            | CheckedAST.ValueDef _ -> (items, used)
            | CheckedAST.FunctionDef funcDef ->
                let parameters =
                    funcDef.Params
                    |> AST.NonEmptyList.toList
                    |> List.map fst
                    |> Set.ofList
                let needed, body = wrap parameters funcDef.Body
                (CheckedAST.FunctionDef { funcDef with Body = body } :: items, Set.union used needed)
            | CheckedAST.Expression expr ->
                let needed, body = wrap Set.empty expr
                (CheckedAST.Expression body :: items, Set.union used needed)
            | CheckedAST.TypeDef (id, typeDef) ->
                (CheckedAST.TypeDef (id, typeDef) :: items, used)) ([], Set.empty)
    // A helper owns each checked value body once. Calls still bind its result
    // in each execution scope, in dependency order, so effects are not cached.
    let helpers =
        currentValues
        |> List.choose (fun (name, (id, typ, body)) ->
            if not (Set.contains id usedValues) || Set.contains id cheapValueIds then None
            else
                let parameters =
                    Map.tryFind id dependencyArgs
                    |> Option.defaultValue []
                    |> List.map (fun dependencyId ->
                        let typ =
                            Map.tryFind dependencyId valueTypes
                            |> Option.defaultWith (fun () -> Crash.crash "Missing checked value type")
                        (dependencyId, CheckedAST.semanticType typ))
                let parameters =
                    match parameters with
                    | [] ->
                        AST.NonEmptyList.singleton (AST.topLevelValueId $"{name}#unit", AST.TUnit)
                    | first :: rest -> { Head = first; Tail = rest }
                Some (CheckedAST.FunctionDef {
                    Id =
                        Map.tryFind id helperIds
                        |> Option.defaultWith (fun () -> Crash.crash "Missing checked value materializer")
                    Name = helperName name
                    TypeParams = []
                    Params = CheckedAST.checkedParams parameters
                    ReturnType = typ
                    Body = body
                    Recursion = None
                }))
    let symbols =
        currentValues
        |> List.fold (fun symbols (name, (id, _, _)) ->
            if Set.contains id usedValues && not (Set.contains id cheapValueIds) then
                CheckedAST.internFunction (helperName name) symbols |> snd
            else symbols) symbols
    CheckedAST.Program (symbols, List.rev materialized @ helpers)

let internal prepareProgramForAnf
    (monomorphization: MonomorphizationMode)
    (baseTypeReg: TypeRegistries.TypeRegistry)
    (baseVariantLookup: LoweringPrimitives.VariantLookup)
    (baseFuncNames: Set<string>)
    (baseFunctions: LiftFunctions.FunctionCatalog)
    (inheritedValues: Map<string, CheckedValueArtifact>)
    (passTimingRecorder: PassTimingRecorder option)
    (program: CheckedAST.Program)
    : Result<CheckedAST.Program, string> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordPassTiming passTimingRecorder name timer.Elapsed.TotalMilliseconds
        result
    let program =
        measure "AST -> ANF Preparation: Value Import" (fun () ->
            importInheritedValues passTimingRecorder inheritedValues program)
    let monomorphizedResult =
        measure "AST -> ANF Preparation: Monomorphization" (fun () ->
            match monomorphization with
            | Monomorphize None ->
                Ok (PrepareFunctions.monomorphize program)
            | Monomorphize (Some defs) ->
                Ok (PrepareFunctions.monomorphizeWithExternalDefs defs program)
            | ReplaceTypeApps specRegistry ->
                Monomorphization.replaceTypeAppsInProgramWithRegistry specRegistry program
            | SpecializeLocalAndReplace specRegistry ->
                let localGenericDefs = SpecializationIdentity.extractGenericFuncDefs program
                if Map.isEmpty localGenericDefs then
                    Monomorphization.replaceTypeAppsInProgramWithRegistry specRegistry program
                else
                    let localSpecs = collectLocalSpecs localGenericDefs program
                    let specialization = Monomorphization.specializeFromSpecs localGenericDefs localSpecs
                    let combinedSpecRegistry =
                        mergeSpecRegistries specRegistry specialization.SpecRegistry
                    let (CheckedAST.Program (symbols, items)) = program
                    let symbols, specializedFunctions =
                        SpecializationIdentity.importSpecializedFunctions symbols specialization.SpecializedFuncs
                    let specializedTopLevels = specializedFunctions |> List.map CheckedAST.FunctionDef
                    let programWithSpecializations = CheckedAST.Program (symbols, specializedTopLevels @ items)
                    Monomorphization.replaceTypeAppsInProgramWithRegistry combinedSpecRegistry programWithSpecializations)
    match monomorphizedResult with
    | Error err -> Error err
    | Ok monomorphized ->
        let monomorphized =
            measure "AST -> ANF Preparation: Value Materialization" (fun () ->
                materializeProgramValues monomorphized)
        let needsLowering =
            measure "AST -> ANF Preparation: Lambda Analysis" (fun () ->
                let (CheckedAST.Program (_, topLevels)) = monomorphized
                let localFuncNames =
                    topLevels
                    |> List.choose (function CheckedAST.FunctionDef f -> Some f.Name | _ -> None)
                    |> Set.ofList
                let knownFuncNames = Set.union baseFuncNames localFuncNames
                Monomorphization.programNeedsLambdaLowering knownFuncNames monomorphized)
        if needsLowering then
            measure "AST -> ANF Preparation: Lambda Lowering" (fun () ->
                let inlined =
                    measure "AST -> ANF Preparation: Lambda Inlining" (fun () ->
                        InlineLambdas.inlineLambdasInProgram monomorphized)
                liftLambdasWithBase
                    baseTypeReg
                    baseVariantLookup
                    baseFunctions
                    passTimingRecorder
                    inlined)
        else
            Ok monomorphized

let internal buildRegistriesForProgram
    (passTimingRecorder: PassTimingRecorder option)
    (symbols: CheckedAST.Symbols)
    (baseProvidesModuleFunctionParams: bool)
    (moduleRegistry: AST.ModuleRegistry)
    (baseRegistries: AST_to_ANF.Registries)
    (typeDefs: AST.TypeDef list)
    (functions: CheckedAST.FunctionDef list)
    : AST_to_ANF.Registries * AST_to_ANF.Registries * CheckedAST.FunctionDef list =
    let measure name operation =
        match passTimingRecorder with
        | None -> operation ()
        | Some _ ->
            let start = Stopwatch.GetTimestamp()
            let result = operation ()
            let elapsed =
                float (Stopwatch.GetTimestamp() - start)
                * 1000.0 / float Stopwatch.Frequency
            recordPassTiming passTimingRecorder name elapsed
            result
    let aliasReg, resolvedFunctions =
        measure "AST -> ANF Registry: Alias Resolution" (fun () ->
            let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
            (aliasReg, AST_to_ANF.resolveAliasesInFunctions aliasReg functions))
    let phaseRecorder =
        passTimingRecorder
        |> Option.map (fun recorder ->
            fun name (elapsed: float) ->
                recorder { Pass = name; Elapsed = TimeSpan.FromMilliseconds elapsed })
    let localRegistries =
        measure "AST -> ANF Registry: Local Construction" (fun () ->
            if baseProvidesModuleFunctionParams then
                AST_to_ANF.buildOverlayRegistriesWithTrace
                    phaseRecorder symbols moduleRegistry typeDefs aliasReg resolvedFunctions
            else
                AST_to_ANF.buildRegistriesWithTrace
                    phaseRecorder symbols moduleRegistry typeDefs aliasReg resolvedFunctions)
    let mergedRegistries =
        measure "AST -> ANF Registry: Base Overlay Merge" (fun () ->
            AST_to_ANF.mergeRegistriesWithTrace
                phaseRecorder baseRegistries localRegistries)
    (mergedRegistries, localRegistries, resolvedFunctions)

type internal DeclarationConversion = {
    Symbols: CheckedAST.Symbols
    Functions: ANF.Function list
    Registries: AST_to_ANF.Registries
    LocalReturnTypes: Map<AST.FunctionId, string * AST.SemanticType>
}

let internal splitDeclarations
    (CheckedAST.Program (_, topLevels))
    : Result<AST.TypeDef list * CheckedAST.FunctionDef list, string> =
    let expressions =
        topLevels |> List.choose (function CheckedAST.Expression expression -> Some expression | _ -> None)
    if not (List.isEmpty expressions) then
        Error $"Declaration-only program must not contain entry expressions; found {expressions.Length}"
    else
        Ok (
            topLevels |> List.choose (function CheckedAST.TypeDef (_, definition) -> Some definition | _ -> None),
            topLevels |> List.choose (function CheckedAST.FunctionDef definition -> Some definition | _ -> None)
        )

let internal convertTypedDeclarationsWithTrace
    (passTimingRecorder: PassTimingRecorder option)
    (baseContext: PipelineContext option)
    (monomorphization: MonomorphizationMode)
    (typedProgram: CheckedAST.Program)
    : Result<DeclarationConversion, string> =
    let typedProgram =
        match baseContext with
        | None -> typedProgram
        | Some context ->
            let sourceSymbols = CheckedAST.programSymbols typedProgram
            let symbols, topLevels =
                CheckedAST.composeTopLevels
                    sourceSymbols
                    context.Symbols
                    (CheckedAST.programTopLevels typedProgram)
            CheckedAST.Program (symbols, topLevels)
    let moduleRegistry =
        baseContext
        |> Option.map (fun context -> context.Registries.ModuleRegistry)
        |> Option.defaultWith Stdlib.buildModuleRegistry
    let baseRegistries =
        baseContext
        |> Option.map (fun context -> context.Registries)
        |> Option.defaultValue (emptyRegistries moduleRegistry)
    let baseFuncNames =
        baseContext
        |> Option.map (fun context -> context.BaseFuncNames)
        |> Option.defaultValue (buildBaseFuncNames baseRegistries)
    let baseFunctions =
        baseContext
        |> Option.map (fun context -> context.LambdaLiftFunctions)
        |> Option.defaultWith (fun () ->
            buildLambdaLiftFunctionCatalog
                baseRegistries
                baseFuncNames
                (extractReturnTypes baseRegistries.FuncReg))
    let (baseTypeReg, baseVariantLookup) =
        match baseContext with
        | Some context ->
            (context.LambdaLiftTypeReg, context.LambdaLiftVariantLookup)
        | None ->
            LiftFunctions.prepareLambdaLiftBaseTypes
                baseRegistries.TypeReg
                baseRegistries.VariantLookup
    prepareProgramForAnf
        monomorphization
        baseTypeReg
        baseVariantLookup
        baseFuncNames
        baseFunctions
        (baseContext |> Option.map (fun context -> context.CheckedValues) |> Option.defaultValue Map.empty)
        passTimingRecorder
        typedProgram
    |> Result.bind (fun liftedProgram ->
        splitDeclarations liftedProgram
        |> Result.bind (fun (typeDefs, functions) ->
            let (registries, localRegistries, resolvedFunctions) =
                buildRegistriesForProgram
                    passTimingRecorder
                    (CheckedAST.programSymbols liftedProgram)
                    (Option.isSome baseContext)
                    moduleRegistry
                    baseRegistries
                    typeDefs
                    functions
            let ownershipTiming name (elapsed: TimeSpan) =
                recordPassTiming passTimingRecorder name elapsed.TotalMilliseconds
            AST_to_ANF.convertFunctionsWithOwnershipWithTrace
                (Some ownershipTiming)
                (CheckedAST.programSymbols liftedProgram)
                registries
                (ANF.VarGen 0)
                resolvedFunctions
            |> Result.map (fun converted ->
                { Symbols = CheckedAST.programSymbols liftedProgram
                  Functions = converted.Functions
                  Registries = registries
                  LocalReturnTypes = extractReturnTypes localRegistries.FuncReg })))

let internal convertTypedDeclarations baseContext monomorphization typedProgram =
    convertTypedDeclarationsWithTrace None baseContext monomorphization typedProgram

let private convertTypedProgramToConversionResult
    (moduleRegistry: AST.ModuleRegistry)
    (typedProgram: CheckedAST.Program)
    : Result<AST_to_ANF.ConversionResult, string> =
    let baseRegistries = emptyRegistries moduleRegistry
    let baseFuncNames = buildBaseFuncNames baseRegistries
    let baseFunctions =
        buildLambdaLiftFunctionCatalog baseRegistries baseFuncNames Map.empty
    prepareProgramForAnf
        (Monomorphize None)
        Map.empty
        Map.empty
        baseFuncNames
        baseFunctions
        Map.empty
        None
        typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, _localRegistries, resolvedFunctions) =
                buildRegistriesForProgram
                    None
                    (CheckedAST.programSymbols liftedProgram)
                    false
                    moduleRegistry
                    baseRegistries
                    typeDefs
                    functions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctionsWithOwnership
                (CheckedAST.programSymbols liftedProgram)
                registries
                varGen
                resolvedFunctions
            |> Result.bind (fun converted ->
                AST_to_ANF.convertExprToAnf registries converted.VarGen expr
                |> Result.map (fun (anfExpr, _) ->
                    buildConversionResult
                        (ANF.Program (converted.Functions, anfExpr))
                        registries
                        converted.OwnershipContracts))))

let internal convertTypedProgramToUserOnlyWithMode
    (baseContext: PipelineContext)
    (monomorphization: MonomorphizationMode)
    (typeCheckEnv: CheckingTypes.TypeCheckEnv)
    (session: CompilationSession option)
    (passTimingRecorder: PassTimingRecorder option)
    (typedProgram: CheckedAST.Program)
    : Result<AST_to_ANF.UserOnlyResult * obj, string> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordPassTiming passTimingRecorder name timer.Elapsed.TotalMilliseconds
        result
    let sourceSymbols = CheckedAST.programSymbols typedProgram
    let symbols, topLevels =
        measure "AST -> ANF Symbol Import" (fun () ->
            CheckedAST.composeTopLevels
                sourceSymbols
                baseContext.Symbols
                (CheckedAST.programTopLevels typedProgram))
    let typedProgram = CheckedAST.Program (symbols, topLevels)

    // Late AOT plans (notably Json) may introduce concrete calls to generic
    // stdlib functions after the suite preamble registry was built. Materialize
    // just those missing specializations into the user compilation unit.
    let (typedProgram, monomorphization, nonInlineableFunctionNames) =
        measure "AST -> ANF Dependency Planning" (fun () ->
            let addMissing baseRegistry rebuildMode =
                // A local generic function's body may instantiate a stdlib generic
                // only once the local one is itself specialized (a fold over
                // Parser<a> inside choice<'a>, called as choice<String>), so
                // specialize the local generics first and request what their
                // specialized bodies reach: specializeFromSpecs reports those as
                // ExternalSpecs. The later local specialization repeats this work
                // on the same input and lands on the same names.
                let localGenericDefs = SpecializationIdentity.extractGenericFuncDefs typedProgram
                let reachedThroughLocalGenerics =
                    if Map.isEmpty localGenericDefs then
                        Set.empty
                    else
                        let localSpecs = collectLocalSpecs localGenericDefs typedProgram
                        (Monomorphization.specializeFromSpecs localGenericDefs localSpecs).ExternalSpecs
                        |> Set.filter (fun (funcName, _) ->
                            Map.containsKey funcName baseContext.GenericFuncDefs)
                let requested =
                    Set.union
                        (collectLocalSpecs baseContext.GenericFuncDefs typedProgram)
                        reachedThroughLocalGenerics
                let (CheckedAST.Program (initialSymbols, items)) = typedProgram
                let localFunctionNames =
                    items
                    |> List.choose (function
                        | CheckedAST.FunctionDef fn -> Some fn.Name
                        | _ -> None)
                    |> Set.ofList
                let isKnownFunctionName localNames name =
                    Set.contains name localNames
                    || Set.contains name baseContext.BaseFuncNames
                let rec materialize
                    (symbols: CheckedAST.Symbols)
                    (specRegistry: SpecializationIdentity.SpecRegistry)
                    (localFunctionNames: Set<string>)
                    (pendingSpecs: Set<SpecializationIdentity.SpecKey>)
                    (accFunctions: CheckedAST.FunctionDef list)
                    : SpecializationIdentity.SpecRegistry * CheckedAST.FunctionDef list * CheckedAST.Symbols =
                    let missingSpecs =
                        pendingSpecs
                        |> Set.filter (fun key -> not (Map.containsKey key specRegistry))
                    if Set.isEmpty missingSpecs then
                        (specRegistry, accFunctions, symbols)
                    else
                        let specialization =
                            Monomorphization.specializeFromSpecs baseContext.GenericFuncDefs missingSpecs
                        let combinedRegistry =
                            mergeSpecRegistries specRegistry specialization.SpecRegistry
                        let symbols, specializedFunctions =
                            SpecializationIdentity.importSpecializedFunctions symbols specialization.SpecializedFuncs
                        let symbols, materializedTopLevels =
                            specializedFunctions
                            |> List.filter (fun fn ->
                                not (isKnownFunctionName localFunctionNames fn.Name))
                            |> List.map CheckedAST.FunctionDef
                            |> CheckedMaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums
                                symbols
                                typeCheckEnv.AliasReg
                                typeCheckEnv.IndexedTypeReg
                                typeCheckEnv.VariantLookup
                                typeCheckEnv.IndexedSumTypeReg
                        let newFunctions =
                            materializedTopLevels
                            |> List.choose (function
                                | CheckedAST.FunctionDef fn
                                    when not (isKnownFunctionName localFunctionNames fn.Name) ->
                                    Some fn
                                | _ -> None)
                        let nextLocalFunctionNames =
                            newFunctions
                            |> List.fold (fun names fn -> Set.add fn.Name names) localFunctionNames
                        let nextSpecs =
                            CheckedAST.Program (symbols, materializedTopLevels)
                            |> collectLocalSpecs baseContext.GenericFuncDefs
                        materialize
                            symbols
                            combinedRegistry
                            nextLocalFunctionNames
                            nextSpecs
                            (accFunctions @ newFunctions)

                let (combinedRegistry, newFunctions, symbols) =
                    materialize initialSymbols baseRegistry localFunctionNames requested []
                let programWithSpecializations =
                    CheckedAST.Program (symbols, (newFunctions |> List.map CheckedAST.FunctionDef) @ items)
                let specializedFunctionNames =
                    newFunctions |> List.map (fun fn -> fn.Id) |> Set.ofList
                (programWithSpecializations, rebuildMode combinedRegistry, specializedFunctionNames)
            match monomorphization with
            | ReplaceTypeApps registry -> addMissing registry ReplaceTypeApps
            | SpecializeLocalAndReplace registry -> addMissing registry SpecializeLocalAndReplace
            | Monomorphize _ -> (typedProgram, monomorphization, Set.empty))
    let baseFuncNames = baseContext.BaseFuncNames
    measure "AST -> ANF Program Preparation" (fun () ->
        prepareProgramForAnf
            monomorphization
            baseContext.LambdaLiftTypeReg
            baseContext.LambdaLiftVariantLookup
            baseFuncNames
            baseContext.LambdaLiftFunctions
            baseContext.CheckedValues
            passTimingRecorder
            typedProgram)
    |> Result.bind (fun liftedProgram ->
        measure "AST -> ANF Registry Construction" (fun () ->
            AST_to_ANF.splitTopLevels liftedProgram
            |> Result.map (fun (typeDefs, functions, expr) ->
                let (registries, localRegistries, resolvedFunctions) =
                    buildRegistriesForProgram
                        passTimingRecorder
                        (CheckedAST.programSymbols liftedProgram)
                        true
                        baseContext.Registries.ModuleRegistry
                        baseContext.Registries
                        typeDefs
                        functions
                let localReturnTypes = extractReturnTypes localRegistries.FuncReg
                (CheckedAST.programSymbols liftedProgram, registries, localRegistries, resolvedFunctions, localReturnTypes, expr)))
        |> Result.bind (fun (symbols, registries, localRegistries, resolvedFunctions, localReturnTypes, expr) ->
            let varGen = ANF.VarGen 0
            let conversionKey = {
                Functions = resolvedFunctions
                LocalRegistries = localRegistries
                NonInlineableFunctionNames = nonInlineableFunctionNames
            }
            let convert () =
                measure "AST -> ANF Dependency Conversion" (fun () ->
                    let ownershipTiming name (elapsed: TimeSpan) =
                        recordPassTiming
                            passTimingRecorder
                            name
                            elapsed.TotalMilliseconds
                    AST_to_ANF.convertFunctionsWithOwnershipWithTrace
                        (Some ownershipTiming)
                        symbols
                        registries
                        varGen
                        resolvedFunctions)
            let convertedDependencies =
                measure "AST -> ANF Dependency Lookup" (fun () ->
                    match session with
                    | Some current ->
                        current.ConvertAnfDependencies
                            (box baseContext)
                            conversionKey
                            convert
                    | None ->
                        convert ()
                        |> Result.map (fun converted ->
                            (converted, System.Object())))
            convertedDependencies
            |> Result.bind (fun (converted, dependencyIdentity) ->
                measure "AST -> ANF Expression Conversion" (fun () ->
                    AST_to_ANF.convertExprToAnf registries converted.VarGen expr)
                |> Result.map (fun (anfExpr, _) ->
                    let convertedFuncReg =
                        AST_to_ANF.extendFunctionRegistryWithConverted
                            registries.FuncReg
                            converted.Functions
                    let convertedReturnTypes =
                        converted.Functions
                        |> List.fold (fun returnTypes functionDefinition ->
                            Map.add
                                functionDefinition.Id
                                (functionDefinition.Name, functionDefinition.ReturnType)
                                returnTypes) localReturnTypes
                    ({
                        Symbols = symbols
                        UserFunctions = converted.Functions
                        OwnershipContracts = converted.OwnershipContracts
                        ScopeContracts = registries.ScopeContracts
                        InertFunctionScopes = registries.InertFunctionScopes
                        NonInlineableFunctionNames =
                            Set.union
                                nonInlineableFunctionNames
                                (converted.OwnershipContracts |> Map.keys |> Set.ofSeq)
                        MainExpr = anfExpr
                        TypeReg = registries.TypeReg
                        TypeNames = registries.TypeNames
                        RecordFieldsReg = registries.RecordFieldsReg
                        RecordTypeParamsReg = registries.RecordTypeParamsReg
                        VariantLookup = registries.VariantLookup
                        SumTypeNames = registries.SumTypeNames
                        LocalRecordFieldsReg = localRegistries.RecordFieldsReg
                        LocalVariantLookup = localRegistries.VariantLookup
                        RcSumShapeReg = registries.RcSumShapeReg
                        FuncReg = convertedFuncReg
                        FunctionIds = registries.FunctionIds
                        FunctionNames = registries.FunctionNames
                        LocalReturnTypes = convertedReturnTypes
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                        RecursiveMembers = registries.RecursiveMembers
                     },
                     dependencyIdentity)))))

let convertTypedProgramToUserOnly
    (baseContext: PipelineContext)
    (typedProgram: CheckedAST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    convertTypedProgramToUserOnlyWithMode
        baseContext
        (Monomorphize (Some baseContext.GenericFuncDefs))
        baseContext.TypeCheckEnv
        None
        None
        typedProgram
    |> Result.map fst

let convertTypedProgramToUserOnlyWithTrace
    (baseContext: PipelineContext)
    (passTimingRecorder: PassTimingRecorder option)
    (typedProgram: CheckedAST.Program)
    : Result<AST_to_ANF.UserOnlyResult, string> =
    convertTypedProgramToUserOnlyWithMode
        baseContext
        (Monomorphize (Some baseContext.GenericFuncDefs))
        baseContext.TypeCheckEnv
        None
        passTimingRecorder
        typedProgram
    |> Result.map fst

/// Try to delete a file, ignoring any errors
let internal tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let internal tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message
