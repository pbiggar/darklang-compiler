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

let internal returnTypesByName
    (returnTypes: Map<AST.FunctionId, string * AST.SemanticType>)
    : Map<string, AST.SemanticType> =
    returnTypes |> Map.toSeq |> Seq.map snd |> Map.ofSeq

let private emptyRegistries (moduleRegistry: AST.ModuleRegistry) : AST_to_ANF.Registries =
    {
        ScopeContracts = Map.empty
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
    (baseFuncParams: Map<string, (string * AST.SemanticType) list>)
    (baseFuncReturnTypes: Map<string, AST.SemanticType>)
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
            baseFuncParams
            baseFuncReturnTypes
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
    let rec groupBySymbolNamespace entries =
        match entries with
        | [] -> []
        | (_, (first: CheckedValueArtifact)) :: _ ->
            let same, rest =
                entries
                |> List.partition (fun (_, (artifact: CheckedValueArtifact)) ->
                    CheckedAST.sameSymbolNamespace first.Symbols artifact.Symbols)
            same :: groupBySymbolNamespace rest
    let inheritedDefinitions, symbols =
        groupBySymbolNamespace inheritedEntries
        |> List.fold (fun (collected, symbols) group ->
            let sourceSymbols = (group |> List.head |> snd).Symbols
            let symbols, imported =
                group
                |> List.map (fun (_, artifact) -> CheckedAST.Expression artifact.Body)
                |> CheckedAST.importTopLevels sourceSymbols symbols
            let importedBodies =
                imported
                |> List.map (function
                    | CheckedAST.Expression body -> body
                    | _ -> Crash.crash "Checked value import changed its top-level shape")
            let importedEntries =
                List.zip3
                    (group |> List.map fst)
                    (group |> List.map (fun (_, artifact) -> artifact.Type))
                    importedBodies
            let definitions, symbols =
                importedEntries
                |> List.mapFold (fun symbols (name, typ, body) ->
                    let (id, symbols) = CheckedAST.internValue name symbols
                    (CheckedAST.ValueDef { Id = id; Name = name; Type = typ; Body = body }, symbols)) symbols
            (collected @ definitions, symbols)) ([], symbols)
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
    let valueIds =
        rawBindings |> List.map (fun (name, id, _) -> name, id) |> Map.ofList
    let bindings =
        rawBindings
        |> List.map (fun (name, id, body) ->
            (name, id, CheckedAST.resolveUnboundValueLocals symbols valueIds body))
    let wrap excluded body =
        let body = CheckedAST.resolveUnboundValueLocals symbols valueIds body
        let eligible = bindings |> List.filter (fun (_, id, _) -> not (Set.contains id excluded))
        let rec required fixedPoint =
            let next =
                eligible
                |> List.fold (fun names (_, id, value) ->
                    if Set.contains id names then
                        eligible
                        |> List.fold (fun dependencies (_, candidateId, _) ->
                            if InlineLambdas.varOccursInExpr candidateId value then Set.add candidateId dependencies
                            else dependencies) names
                    else names) fixedPoint
            if Set.count next = Set.count fixedPoint then next else required next
        let direct =
            eligible
            |> List.fold (fun names (_, id, _) ->
                if InlineLambdas.varOccursInExpr id body then Set.add id names else names) Set.empty
        let needed = required direct
        let selected = eligible |> List.filter (fun (_, id, _) -> Set.contains id needed)
        let rec orderByDependencies ordered remaining =
            match remaining with
            | [] -> ordered
            | _ ->
                let remainingNames = remaining |> List.map (fun (_, id, _) -> id) |> Set.ofList
                let ready =
                    remaining
                    |> List.filter (fun (_, id, value) ->
                        remainingNames
                        |> Set.remove id
                        |> Set.forall (fun candidate ->
                            not (InlineLambdas.varOccursInExpr candidate value)))
                match ready with
                | [] ->
                    Crash.crash "Checked top-level values contain a cyclic materialization dependency"
                | _ ->
                    let readyNames = ready |> List.map (fun (_, id, _) -> id) |> Set.ofList
                    let pending = remaining |> List.filter (fun (_, id, _) -> not (Set.contains id readyNames))
                    orderByDependencies (ordered @ ready) pending
        let ordered = orderByDependencies [] selected
        List.foldBack (fun (_, id, value) result ->
            if Set.contains id excluded then result
            else CheckedAST.Let (CheckedAST.LPVariable id, value, result)) ordered body
    let materialized =
        topLevels
        |> List.choose (function
            | CheckedAST.ValueDef _ -> None
            | CheckedAST.FunctionDef funcDef ->
                let parameters =
                    funcDef.Params
                    |> AST.NonEmptyList.toList
                    |> List.map fst
                    |> Set.ofList
                Some (CheckedAST.FunctionDef { funcDef with Body = wrap parameters funcDef.Body })
            | CheckedAST.Expression expr -> Some (CheckedAST.Expression (wrap Set.empty expr))
            | CheckedAST.TypeDef (id, typeDef) -> Some (CheckedAST.TypeDef (id, typeDef)))
    CheckedAST.Program (symbols, materialized)

let internal prepareProgramForAnf
    (monomorphization: MonomorphizationMode)
    (baseTypeReg: TypeRegistries.TypeRegistry)
    (baseVariantLookup: LoweringPrimitives.VariantLookup)
    (baseFuncNames: Set<string>)
    (baseFuncParams: Map<string, (string * AST.SemanticType) list>)
    (baseFuncReturnTypes: Map<string, AST.SemanticType>)
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
    let program = importInheritedValues inheritedValues program
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
        let monomorphized = materializeProgramValues monomorphized
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
                    baseFuncParams
                    baseFuncReturnTypes
                    passTimingRecorder
                    inlined)
        else
            Ok monomorphized

let internal buildRegistriesForProgram
    (symbols: CheckedAST.Symbols)
    (baseProvidesModuleFunctionParams: bool)
    (moduleRegistry: AST.ModuleRegistry)
    (baseRegistries: AST_to_ANF.Registries)
    (typeDefs: AST.TypeDef list)
    (functions: CheckedAST.FunctionDef list)
    : AST_to_ANF.Registries * AST_to_ANF.Registries * CheckedAST.FunctionDef list =
    let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
    let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
    let localRegistries =
        if baseProvidesModuleFunctionParams then
            AST_to_ANF.buildOverlayRegistries symbols moduleRegistry typeDefs aliasReg resolvedFunctions
        else
            AST_to_ANF.buildRegistries symbols moduleRegistry typeDefs aliasReg resolvedFunctions
    let mergedRegistries = AST_to_ANF.mergeRegistries baseRegistries localRegistries
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
                CheckedAST.importTopLevels
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
    let baseFuncParams =
        baseContext
        |> Option.map (fun context -> context.LambdaLiftFuncParams)
        |> Option.defaultWith (fun () ->
            reserveBaseFunctionParams baseRegistries.FuncParams baseFuncNames)
    let baseFuncReturnTypes =
        baseContext
        |> Option.map (fun context ->
            returnTypesByName context.ReturnTypes)
        |> Option.defaultWith (fun () ->
            extractReturnTypes baseRegistries.FuncReg |> returnTypesByName)
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
        baseFuncParams
        baseFuncReturnTypes
        (baseContext |> Option.map (fun context -> context.CheckedValues) |> Option.defaultValue Map.empty)
        passTimingRecorder
        typedProgram
    |> Result.bind (fun liftedProgram ->
        splitDeclarations liftedProgram
        |> Result.bind (fun (typeDefs, functions) ->
            let (registries, localRegistries, resolvedFunctions) =
                buildRegistriesForProgram
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
    prepareProgramForAnf
        (Monomorphize None)
        Map.empty
        Map.empty
        baseFuncNames
        baseRegistries.FuncParams
        Map.empty
        Map.empty
        None
        typedProgram
    |> Result.bind (fun liftedProgram ->
        AST_to_ANF.splitTopLevels liftedProgram
        |> Result.bind (fun (typeDefs, functions, expr) ->
            let (registries, _localRegistries, resolvedFunctions) =
                buildRegistriesForProgram
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
        CheckedAST.importTopLevels
            sourceSymbols
            baseContext.Symbols
            (CheckedAST.programTopLevels typedProgram)
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
            baseContext.LambdaLiftFuncParams
            (baseContext.ReturnTypes
             |> Map.toSeq
             |> Seq.map snd
             |> Map.ofSeq)
            baseContext.CheckedValues
            passTimingRecorder
            typedProgram)
    |> Result.bind (fun liftedProgram ->
        measure "AST -> ANF Registry Construction" (fun () ->
            AST_to_ANF.splitTopLevels liftedProgram
            |> Result.map (fun (typeDefs, functions, expr) ->
                let (registries, localRegistries, resolvedFunctions) =
                    buildRegistriesForProgram
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

/// Try to delete a file, ignoring any errors
let internal tryDeleteFile (path: string) : unit =
    try File.Delete(path) with _ -> ()

/// Try to start a process, returning Result instead of throwing
let internal tryStartProcess (info: ProcessStartInfo) : Result<Process, string> =
    try Ok (Process.Start(info))
    with ex -> Error ex.Message
