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
let internal extractReturnTypes (funcReg: Map<string, AST.Type>) : Map<string, AST.Type> =
    funcReg
    |> Map.toSeq
    |> Seq.choose (fun (name, typ) ->
        match typ with
        | AST.TFunction (_, retType) -> Some (name, retType)
        | other -> Crash.crash $"extractReturnTypes: Non-function type '{other}' found in FuncReg for '{name}'")
    |> Map.ofSeq

let private emptyRegistries (moduleRegistry: AST.ModuleRegistry) : AST_to_ANF.Registries =
    {
        ScopeContracts = Map.empty
        TypeReg = Map.empty
        RecordFieldsReg = Map.empty
        RecordTypeParamsReg = Map.empty
        VariantLookup = Map.empty
        SumTypeNames = Set.empty
        RcSumShapeReg = Map.empty
        FuncReg = Map.empty
        FuncParams = Map.empty
        ModuleRegistry = moduleRegistry
        RecursiveMembers = Map.empty
    }

let private liftLambdasWithBase
    (baseTypeReg: TypeRegistries.TypeRegistry)
    (baseVariantLookup: LoweringPrimitives.VariantLookup)
    (baseFuncParams: Map<string, (string * AST.Type) list>)
    (baseFuncReturnTypes: Map<string, AST.Type>)
    (passTimingRecorder: PassTimingRecorder option)
    (program: AST.Program)
    : Result<AST.Program, string> =
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
    (program: AST.Program)
    : Set<SpecializationIdentity.SpecKey> =
    let (AST.Program topLevels) = program
    let allSpecs =
        topLevels
        |> List.map (function
            | AST.FunctionDef f when List.isEmpty f.TypeParams -> Monomorphization.collectTypeAppsFromFunc f
            | AST.ValueDef valueDef -> Monomorphization.collectTypeApps (AST.valueDefBody valueDef)
            | AST.Expression e -> Monomorphization.collectTypeApps e
            | _ -> Set.empty)
        |> List.fold Set.union Set.empty
    allSpecs
    |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

type internal MonomorphizationMode =
    | Monomorphize of SpecializationIdentity.GenericFuncDefs option
    | ReplaceTypeApps of SpecializationIdentity.SpecRegistry
    | SpecializeLocalAndReplace of SpecializationIdentity.SpecRegistry

/// Materialize checked module values as one lexical binding per execution
/// scope. This gives every reference ordinary value semantics through the
/// existing ANF ownership pipeline and leaves no value-only lowering cases.
let private materializeProgramValues
    (inheritedValues: Map<string, AST.Type * AST.Expr>)
    (AST.Program topLevels)
    : AST.Program =
    let currentValues =
        topLevels
        |> List.choose (function
            | AST.ValueDef (AST.CheckedValueDef (name, typ, body)) -> Some (name, (typ, body))
            | AST.ValueDef (AST.UncheckedValueDef (name, _)) ->
                Crash.crash $"Unchecked value '{name}' reached ANF preparation"
            | _ -> None)
    let currentNames = currentValues |> List.map fst |> Set.ofList
    let bindings =
        (inheritedValues
         |> Map.toList
         |> List.filter (fun (name, _) -> not (Set.contains name currentNames)))
        @ currentValues
        |> List.map (fun (name, (_, body)) -> (name, body))
    let wrap excluded body =
        let eligible = bindings |> List.filter (fun (name, _) -> not (Set.contains name excluded))
        let rec required fixedPoint =
            let next =
                eligible
                |> List.fold (fun names (name, value) ->
                    if Set.contains name names then
                        eligible
                        |> List.fold (fun dependencies (candidate, _) ->
                            if InlineLambdas.varOccursInExpr candidate value then Set.add candidate dependencies
                            else dependencies) names
                    else names) fixedPoint
            if Set.count next = Set.count fixedPoint then next else required next
        let direct =
            eligible
            |> List.fold (fun names (name, _) ->
                if InlineLambdas.varOccursInExpr name body then Set.add name names else names) Set.empty
        let needed = required direct
        let selected = eligible |> List.filter (fun (name, _) -> Set.contains name needed)
        let rec orderByDependencies ordered remaining =
            match remaining with
            | [] -> ordered
            | _ ->
                let remainingNames = remaining |> List.map fst |> Set.ofList
                let ready =
                    remaining
                    |> List.filter (fun (name, value) ->
                        remainingNames
                        |> Set.remove name
                        |> Set.forall (fun candidate ->
                            not (InlineLambdas.varOccursInExpr candidate value)))
                match ready with
                | [] ->
                    Crash.crash "Checked top-level values contain a cyclic materialization dependency"
                | _ ->
                    let readyNames = ready |> List.map fst |> Set.ofList
                    let pending = remaining |> List.filter (fun (name, _) -> not (Set.contains name readyNames))
                    orderByDependencies (ordered @ ready) pending
        let ordered = orderByDependencies [] selected
        List.foldBack (fun (name, value) result ->
            if Set.contains name excluded then result
            else AST.Let (AST.LPVariable name, value, result)) ordered body
    let materialized =
        topLevels
        |> List.choose (function
            | AST.ValueDef _ -> None
            | AST.FunctionDef funcDef ->
                let parameters =
                    funcDef.Params
                    |> AST.NonEmptyList.toList
                    |> List.map fst
                    |> Set.ofList
                Some (AST.FunctionDef { funcDef with Body = wrap parameters funcDef.Body })
            | AST.Expression expr -> Some (AST.Expression (wrap Set.empty expr))
            | AST.TypeDef typeDef -> Some (AST.TypeDef typeDef))
    AST.Program materialized

let internal prepareProgramForAnf
    (monomorphization: MonomorphizationMode)
    (baseTypeReg: TypeRegistries.TypeRegistry)
    (baseVariantLookup: LoweringPrimitives.VariantLookup)
    (baseFuncNames: Set<string>)
    (baseFuncParams: Map<string, (string * AST.Type) list>)
    (baseFuncReturnTypes: Map<string, AST.Type>)
    (inheritedValues: Map<string, AST.Type * AST.Expr>)
    (passTimingRecorder: PassTimingRecorder option)
    (program: AST.Program)
    : Result<AST.Program, string> =
    let program = materializeProgramValues inheritedValues program
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordPassTiming passTimingRecorder name timer.Elapsed.TotalMilliseconds
        result
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
                    let (AST.Program items) = program
                    let specializedTopLevels = specialization.SpecializedFuncs |> List.map AST.FunctionDef
                    let programWithSpecializations = AST.Program (specializedTopLevels @ items)
                    Monomorphization.replaceTypeAppsInProgramWithRegistry combinedSpecRegistry programWithSpecializations)
    match monomorphizedResult with
    | Error err -> Error err
    | Ok monomorphized ->
        let needsLowering =
            measure "AST -> ANF Preparation: Lambda Analysis" (fun () ->
                let (AST.Program topLevels) = monomorphized
                let localFuncNames =
                    topLevels
                    |> List.choose (function AST.FunctionDef f -> Some f.Name | _ -> None)
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
    (baseProvidesModuleFunctionParams: bool)
    (moduleRegistry: AST.ModuleRegistry)
    (baseRegistries: AST_to_ANF.Registries)
    (typeDefs: AST.TypeDef list)
    (functions: AST.FunctionDef list)
    : AST_to_ANF.Registries * AST_to_ANF.Registries * AST.FunctionDef list =
    let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
    let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
    let localRegistries =
        if baseProvidesModuleFunctionParams then
            AST_to_ANF.buildOverlayRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
        else
            AST_to_ANF.buildRegistries moduleRegistry typeDefs aliasReg resolvedFunctions
    let mergedRegistries = AST_to_ANF.mergeRegistries baseRegistries localRegistries
    (mergedRegistries, localRegistries, resolvedFunctions)

type internal DeclarationConversion = {
    Functions: ANF.Function list
    Registries: AST_to_ANF.Registries
    LocalReturnTypes: Map<string, AST.Type>
}

let internal splitDeclarations
    (AST.Program topLevels)
    : Result<AST.TypeDef list * AST.FunctionDef list, string> =
    let expressions =
        topLevels |> List.choose (function AST.Expression expression -> Some expression | _ -> None)
    if not (List.isEmpty expressions) then
        Error $"Declaration-only program must not contain entry expressions; found {expressions.Length}"
    else
        Ok (
            topLevels |> List.choose (function AST.TypeDef definition -> Some definition | _ -> None),
            topLevels |> List.choose (function AST.FunctionDef definition -> Some definition | _ -> None)
        )

let internal convertTypedDeclarations
    (baseContext: PipelineContext option)
    (monomorphization: MonomorphizationMode)
    (typedProgram: AST.Program)
    : Result<DeclarationConversion, string> =
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
        |> Option.map (fun context -> context.ReturnTypes)
        |> Option.defaultWith (fun () -> extractReturnTypes baseRegistries.FuncReg)
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
        (baseContext |> Option.map (fun context -> context.TypeCheckEnv.Values) |> Option.defaultValue Map.empty)
        None
        typedProgram
    |> Result.bind (fun liftedProgram ->
        splitDeclarations liftedProgram
        |> Result.bind (fun (typeDefs, functions) ->
            let (registries, localRegistries, resolvedFunctions) =
                buildRegistriesForProgram
                    (Option.isSome baseContext)
                    moduleRegistry
                    baseRegistries
                    typeDefs
                    functions
            AST_to_ANF.convertFunctions registries (ANF.VarGen 0) resolvedFunctions
            |> Result.map (fun (anfFunctions, _) ->
                { Functions = anfFunctions
                  Registries = registries
                  LocalReturnTypes = extractReturnTypes localRegistries.FuncReg })))

let private convertTypedProgramToConversionResult
    (moduleRegistry: AST.ModuleRegistry)
    (typedProgram: AST.Program)
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
                buildRegistriesForProgram false moduleRegistry baseRegistries typeDefs functions
            let varGen = ANF.VarGen 0
            AST_to_ANF.convertFunctions registries varGen resolvedFunctions
            |> Result.bind (fun (anfFuncs, varGen1) ->
                AST_to_ANF.convertExprToAnf registries varGen1 expr
                |> Result.map (fun (anfExpr, _) ->
                    buildConversionResult (ANF.Program (anfFuncs, anfExpr)) registries))))

let internal convertTypedProgramToUserOnlyWithMode
    (baseContext: PipelineContext)
    (monomorphization: MonomorphizationMode)
    (typeCheckEnv: CheckingTypes.TypeCheckEnv)
    (session: CompilationSession option)
    (passTimingRecorder: PassTimingRecorder option)
    (typedProgram: AST.Program)
    : Result<AST_to_ANF.UserOnlyResult * obj, string> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordPassTiming passTimingRecorder name timer.Elapsed.TotalMilliseconds
        result

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
                let (AST.Program items) = typedProgram
                let localFunctionNames =
                    items
                    |> List.choose (function
                        | AST.FunctionDef fn -> Some fn.Name
                        | _ -> None)
                    |> Set.ofList
                let isKnownFunctionName localNames name =
                    Set.contains name localNames
                    || Set.contains name baseContext.BaseFuncNames
                let rec materialize
                    (specRegistry: SpecializationIdentity.SpecRegistry)
                    (localFunctionNames: Set<string>)
                    (pendingSpecs: Set<SpecializationIdentity.SpecKey>)
                    (accFunctions: AST.FunctionDef list)
                    : SpecializationIdentity.SpecRegistry * AST.FunctionDef list =
                    let missingSpecs =
                        pendingSpecs
                        |> Set.filter (fun key -> not (Map.containsKey key specRegistry))
                    if Set.isEmpty missingSpecs then
                        (specRegistry, accFunctions)
                    else
                        let specialization =
                            Monomorphization.specializeFromSpecs baseContext.GenericFuncDefs missingSpecs
                        let combinedRegistry =
                            mergeSpecRegistries specRegistry specialization.SpecRegistry
                        let materializedTopLevels =
                            specialization.SpecializedFuncs
                            |> List.filter (fun fn ->
                                not (isKnownFunctionName localFunctionNames fn.Name))
                            |> List.map AST.FunctionDef
                            |> MaterializeHelpers.materializeEqHelpersInTopLevelsWithIndexedSums
                                typeCheckEnv.AliasReg
                                typeCheckEnv.IndexedTypeReg
                                typeCheckEnv.VariantLookup
                                typeCheckEnv.IndexedSumTypeReg
                        let newFunctions =
                            materializedTopLevels
                            |> List.choose (function
                                | AST.FunctionDef fn
                                    when not (isKnownFunctionName localFunctionNames fn.Name) ->
                                    Some fn
                                | _ -> None)
                        let nextLocalFunctionNames =
                            newFunctions
                            |> List.fold (fun names fn -> Set.add fn.Name names) localFunctionNames
                        let nextSpecs =
                            materializedTopLevels
                            |> AST.Program
                            |> collectLocalSpecs baseContext.GenericFuncDefs
                        materialize
                            combinedRegistry
                            nextLocalFunctionNames
                            nextSpecs
                            (accFunctions @ newFunctions)

                let (combinedRegistry, newFunctions) =
                    materialize baseRegistry localFunctionNames requested []
                let programWithSpecializations =
                    AST.Program ((newFunctions |> List.map AST.FunctionDef) @ items)
                let specializedFunctionNames =
                    newFunctions |> List.map (fun fn -> fn.Name) |> Set.ofList
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
            baseContext.ReturnTypes
            baseContext.TypeCheckEnv.Values
            passTimingRecorder
            typedProgram)
    |> Result.bind (fun liftedProgram ->
        measure "AST -> ANF Registry Construction" (fun () ->
            AST_to_ANF.splitTopLevels liftedProgram
            |> Result.map (fun (typeDefs, functions, expr) ->
                let (registries, localRegistries, resolvedFunctions) =
                    buildRegistriesForProgram
                        true
                        baseContext.Registries.ModuleRegistry
                        baseContext.Registries
                        typeDefs
                        functions
                let localReturnTypes = extractReturnTypes localRegistries.FuncReg
                (registries, localRegistries, resolvedFunctions, localReturnTypes, expr)))
        |> Result.bind (fun (registries, localRegistries, resolvedFunctions, localReturnTypes, expr) ->
            let varGen = ANF.VarGen 0
            let conversionKey = {
                Functions = resolvedFunctions
                LocalRegistries = localRegistries
                NonInlineableFunctionNames = nonInlineableFunctionNames
            }
            let convert () =
                measure "AST -> ANF Dependency Conversion" (fun () ->
                    AST_to_ANF.convertFunctions registries varGen resolvedFunctions)
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
                        |> Result.map (fun (anfFuncs, varGen1) ->
                            (anfFuncs, varGen1, System.Object())))
            convertedDependencies
            |> Result.bind (fun (anfFuncs, varGen1, dependencyIdentity) ->
                measure "AST -> ANF Expression Conversion" (fun () ->
                    AST_to_ANF.convertExprToAnf registries varGen1 expr)
                |> Result.map (fun (anfExpr, _) ->
                    ({
                        UserFunctions = anfFuncs
                        ScopeContracts = registries.ScopeContracts
                        NonInlineableFunctionNames = nonInlineableFunctionNames
                        MainExpr = anfExpr
                        TypeReg = registries.TypeReg
                        RecordFieldsReg = registries.RecordFieldsReg
                        RecordTypeParamsReg = registries.RecordTypeParamsReg
                        VariantLookup = registries.VariantLookup
                        SumTypeNames = registries.SumTypeNames
                        LocalRecordFieldsReg = localRegistries.RecordFieldsReg
                        LocalVariantLookup = localRegistries.VariantLookup
                        RcSumShapeReg = registries.RcSumShapeReg
                        FuncReg = registries.FuncReg
                        LocalReturnTypes = localReturnTypes
                        FuncParams = registries.FuncParams
                        ModuleRegistry = registries.ModuleRegistry
                        RecursiveMembers = registries.RecursiveMembers
                     },
                     dependencyIdentity)))))

let internal convertTypedProgramToUserOnly
    (baseContext: PipelineContext)
    (typedProgram: AST.Program)
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
