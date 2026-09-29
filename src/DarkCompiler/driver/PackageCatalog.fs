// PackageCatalog.fs - Materialize reachable package values and source compilation plans.

module PackageCatalog

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

type internal UserCompileLabels = {
    Parse: string
    TypeCheck: string
    Anf: string
    StageSuffix: string
}

type internal UserCompilePlan = {
    AllowInternal: bool
    Mode: CompileMode
    Verbosity: int
    Options: CompilerOptions
    PackageValues: PackageValueCatalog
    PackageManager: PackageManager.Config option
    PassTimingRecorder: PassTimingRecorder option
    Session: CompilationSession option
    Stdlib: StdlibResult
    BaseContext: PipelineContext
    Monomorphization: MonomorphizationMode
    ExternalInlineCandidates: Map<AST.FunctionId, InliningCommon.FunctionInfo>
    PrebuiltSymbolicFunctions: LIR.Function list
    PrebuiltCallGraphSummaries: Map<AST.FunctionId, CompilationCacheIdentity.FunctionSummary>
    PrebuiltCallGraph: Map<AST.FunctionId, Set<AST.FunctionId>>
    SkipFunctionNames: Set<string>
    EmitFunctionEvents: bool
    TreeShakeUserFunctions: bool
    Labels: UserCompileLabels
    Sources: AST.NonEmptyList<SourceUnit>
}

/// Parse each source unit with the copied interpreter parser and enforce entry ownership.
let parseWrittenSourceProgram
    (allowInternal: bool)
    (requireEntry: bool)
    (sources: AST.NonEmptyList<SourceUnit>)
    : Result<LibParser.Validation.ValidatedSourceFile list, string> =
    sources
    |> AST.NonEmptyList.toList
    |> ResultList.traverse (fun sourceUnit ->
        NameSyntax.sourceUnitName sourceUnit.Name
        |> Result.bind (fun name ->
            WrittenParsing.parse LibParser.Validation.Script sourceUnit.Source
            |> Result.map (fun parsed ->
                NameSyntax.sourceUnitNameText name, sourceUnit.Purpose, parsed)))
    |> Result.bind (WrittenSource.validateSourceUnits requireEntry)

let private packageHashType =
    AST.TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])

let private packageLocationType =
    AST.TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])

let private runtimeValueType =
    AST.TSum ("Darklang.LanguageTools.RuntimeTypes.ValueType", [])

let private optionType (innerType: AST.SemanticType) =
    AST.TSum ("Darklang.Stdlib.Option.Option", [innerType])

let private constructor
    (typeName: string)
    (caseName: string)
    (payload: AST.Expr option)
    : AST.Expr =
    AST.Constructor (AST.UnresolvedConstructor (Some typeName), caseName, Option.toList payload)

let private packageHashExpr (hash: string) : AST.Expr =
    constructor
        "Darklang.LanguageTools.ProgramTypes.Hash"
        "Hash"
        (Some (AST.StringLiteral hash))

let private optionNoneExpr : AST.Expr =
    constructor "Darklang.Stdlib.Option.Option" "None" None

let private optionSomeExpr (value: AST.Expr) : AST.Expr =
    constructor "Darklang.Stdlib.Option.Option" "Some" (Some value)

let private call (name: string) (args: AST.Expr list) : AST.Expr =
    AST.applyNamed name (AST.NonEmptyList.fromList args)

let private orderedGroups
    (entries: ('key * 'value) list)
    : ('key * 'value list) list
    when 'key: comparison =
    let order, groups =
        entries
        |> List.fold (fun (order, groups) (key, value) ->
            match Map.tryFind key groups with
            | Some values -> order, Map.add key (value :: values) groups
            | None -> key :: order, Map.add key [value] groups) ([], Map.empty)
    order
    |> List.rev
    |> List.map (fun key ->
        let values =
            Map.tryFind key groups
            |> Option.defaultWith (fun () -> Crash.crash "Ordered package group is missing")
        key, List.rev values)

let private nestedIf
    (cases: (AST.Expr * AST.Expr) list)
    (fallback: AST.Expr)
    : AST.Expr =
    List.foldBack
        (fun (condition, result) remaining -> AST.If (condition, result, remaining))
        cases
        fallback

let private catalogFunction
    (name: string)
    (parameters: (string * AST.SemanticType) list)
    (returnType: AST.SemanticType)
    (body: AST.Expr)
    : AST.FunctionDef =
    {
        Name = name
        TypeParams = []
        Params = AST.NonEmptyList.fromList parameters
        ReturnType = returnType
        Body = body
        Recursion = None
    }

let private collectProgramSpecs (program: CheckedAST.Program) : Set<SpecializationIdentity.SpecKey> =
    let (CheckedAST.Program (symbols, topLevels)) = program
    topLevels
    |> List.map (function
        | CheckedAST.FunctionDef func when List.isEmpty func.TypeParams ->
            Monomorphization.collectTypeAppsFromFunc symbols func
        | CheckedAST.Expression expr -> Monomorphization.collectTypeApps symbols expr
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private collectProgramCalls (program: CheckedAST.Program) : Set<AST.FunctionId> =
    let (CheckedAST.Program (_, topLevels)) = program
    topLevels
    |> List.map (function
        | CheckedAST.FunctionDef func -> Monomorphization.collectCalledFunctions func.Body
        | CheckedAST.ValueDef valueDef -> Monomorphization.collectCalledFunctions valueDef.Body
        | CheckedAST.Expression expr -> Monomorphization.collectCalledFunctions expr
        | CheckedAST.TypeDef _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private calledFunctionNames
    (symbols: CheckedAST.Symbols)
    (calls: Set<AST.FunctionId>)
    : Set<string> =
    calls
    |> Set.toList
    |> List.choose (fun id -> CheckedAST.functionName id symbols)
    |> Set.ofList

let private validateDistinctCatalogHashes
    (entries: PackageValueCatalogEntry list)
    : Result<unit, string> =
    let folder (state: Result<Set<string>, string>) entry =
        state
        |> Result.bind (fun hashes ->
            if Set.contains entry.ValueHash hashes then
                Error $"Package value catalog contains duplicate value hash '{entry.ValueHash}'"
            else
                Ok (Set.add entry.ValueHash hashes))
    entries
    |> List.fold folder (Ok Set.empty)
    |> Result.map (fun _ -> ())

let private materializeReachablePackageValueCatalog
    (baseContext: PipelineContext)
    (warningSettings: AST.WarningSettings)
    (catalog: PackageValueCatalog)
    (typedProgram: CheckedAST.Program)
    : Result<CheckedAST.Program, string> =
    let (PackageValueCatalog entries) = catalog
    validateDistinctCatalogHashes entries
    |> Result.bind (fun () ->
        let localGenericDefs = SpecializationIdentity.extractGenericFuncDefs typedProgram
        let genericDefs =
            Map.fold
                (fun current name definition -> Map.add name definition current)
                baseContext.GenericFuncDefs
                localGenericDefs
        let specialization =
            typedProgram
            |> collectProgramSpecs
            |> Monomorphization.specializeFromSpecs
                (CheckedAST.programSymbols typedProgram)
                genericDefs
        let requestedEvaluatorTypes =
            specialization.ExternalSpecs
            |> Set.toList
            |> List.choose (function
                | "Builtin.pmEvaluateValue", [resultType] -> Some resultType
                | _ -> None)
            |> Set.ofList
        let specializedCallNames =
            specialization.SpecializedFuncs
            |> List.map (fun artifact ->
                Monomorphization.collectCalledFunctions artifact.Function.Body
                |> calledFunctionNames artifact.Symbols)
            |> List.fold Set.union Set.empty
        let symbols = CheckedAST.programSymbols typedProgram
        let reachableCallNames =
            Set.union
                (collectProgramCalls typedProgram |> calledFunctionNames symbols)
                specializedCallNames
        let isReachable name = Set.contains name reachableCallNames
        let needsFind = isReachable "Builtin.pmFindValuesByValueType"
        let needsLocations = isReachable "Builtin.pmGetLocationsByValue"
        let needsEvaluators = not (Set.isEmpty requestedEvaluatorTypes)

        if not needsFind && not needsLocations && not needsEvaluators then
            Ok typedProgram
        else
            let reachableEntries =
                entries
                |> List.filter (fun entry ->
                    Set.contains entry.Evaluator.ResultType requestedEvaluatorTypes)

            let findGroups =
                reachableEntries
                |> List.filter (fun entry -> List.isEmpty entry.RuntimeType.TypeArguments)
                |> List.map (fun entry -> entry.RuntimeType, entry.ValueHash)
                |> orderedGroups
            let findCases =
                findGroups
                |> List.map (fun (catalogType, hashes) ->
                    let condition =
                        call
                            "Darklang.LanguageTools.RuntimeTypes.__isCustomTypeWithNoTypeArguments"
                            [AST.Var "valueType"; AST.StringLiteral catalogType.Hash]
                    let result = hashes |> List.map packageHashExpr |> AST.ListLiteral
                    (condition, result))
            let findFunction =
                catalogFunction
                    "Builtin.pmFindValuesByValueType"
                    [("valueType", runtimeValueType)]
                    (AST.TList packageHashType)
                    (nestedIf findCases (AST.ListLiteral []))

            let visibleLocations =
                reachableEntries
                |> List.collect (fun entry ->
                    entry.Locations
                    |> List.collect (fun location ->
                        location.VisibleInBranches
                        |> List.map (fun branchId ->
                            ((branchId, entry.ValueHash), location))))
            let locationGroups =
                visibleLocations
                |> orderedGroups
            let locationExpr (location: CatalogPackageLocation) =
                AST.RecordLiteral (
                    AST.unresolvedRecordReference "Darklang.LanguageTools.ProgramTypes.PackageLocation" [],
                    [
                        (AST.unresolvedRecordFieldReference "owner", AST.StringLiteral location.Owner)
                        (AST.unresolvedRecordFieldReference "modules", location.Modules |> List.map AST.StringLiteral |> AST.ListLiteral)
                        (AST.unresolvedRecordFieldReference "name", AST.StringLiteral location.Name)
                    ]
                )
            let locationCases =
                locationGroups
                |> List.map (fun ((branchId, valueHash), locations) ->
                    let branchMatches =
                        AST.BinOp (AST.Eq, AST.Var "branchId", AST.StringLiteral branchId)
                    let hashMatches =
                        AST.BinOp (AST.Eq, AST.Var "hashText", AST.StringLiteral valueHash)
                    let result = locations |> List.map locationExpr |> AST.ListLiteral
                    (AST.BinOp (AST.And, branchMatches, hashMatches), result))
            let locationsBody =
                AST.Let (
                    AST.LPVariable "hashText",
                    call "Darklang.LanguageTools.ProgramTypes.hashToString" [AST.Var "valueHash"],
                    nestedIf locationCases (AST.ListLiteral [])
                )
            let locationsFunction =
                catalogFunction
                    "Builtin.pmGetLocationsByValue"
                    [("branchId", AST.TString); ("valueHash", packageHashType)]
                    (AST.TList packageLocationType)
                    locationsBody

            let evaluatorFunction (resultType: AST.SemanticType) =
                let name = SpecializationIdentity.specName "Builtin.pmEvaluateValue" [resultType]
                let cases =
                    reachableEntries
                    |> List.choose (fun entry ->
                        if entry.Evaluator.ResultType <> resultType then
                            None
                        else
                            match entry.Evaluator.State with
                            | Available value ->
                                let condition =
                                    AST.BinOp (
                                        AST.Eq,
                                        AST.Var "hashText",
                                        AST.StringLiteral entry.ValueHash
                                    )
                                Some (condition, optionSomeExpr value)
                            | Unavailable
                            | EvaluationFailure -> None)
                let body =
                    AST.Let (
                        AST.LPVariable "hashText",
                        call "Darklang.LanguageTools.ProgramTypes.hashToString" [AST.Var "valueHash"],
                        nestedIf cases optionNoneExpr
                    )
                catalogFunction
                    name
                    [("valueHash", packageHashType)]
                    (optionType resultType)
                    body

            let generatedFunctions =
                (if needsFind then [findFunction] else [])
                @ (if needsLocations then [locationsFunction] else [])
                @ (requestedEvaluatorTypes |> Set.toList |> List.map evaluatorFunction)
            let syntheticProgram =
                AST.Program (
                    (generatedFunctions |> List.map AST.FunctionDef)
                )
            TypeChecking.checkDeclarationProgramWithBaseEnvAndSettings
                { baseContext.TypeCheckEnv with
                    FunctionCatalog =
                        CheckedAST.functionCatalog (CheckedAST.programSymbols typedProgram) }
                false
                warningSettings
                syntheticProgram
            |> Result.mapError (fun error ->
                $"Package value catalog validation failed: {CheckingDiagnostics.typeErrorToString error}")
            |> Result.map (fun (_, CheckedAST.Program (generatedSymbols, generatedTopLevels), _) ->
                let (CheckedAST.Program (userSymbols, userTopLevels)) = typedProgram
                let symbols, importedGenerated =
                    CheckedAST.composeTopLevels generatedSymbols userSymbols generatedTopLevels
                CheckedAST.programFromCheckedParts (symbols, importedGenerated @ userTopLevels)))

let internal materializePackageValueCatalog
    (baseContext: PipelineContext)
    (warningSettings: AST.WarningSettings)
    (catalog: PackageValueCatalog)
    (typedProgram: CheckedAST.Program)
    : Result<CheckedAST.Program, string> =
    let programCalls = collectProgramCalls typedProgram
    let symbols = CheckedAST.programSymbols typedProgram
    let programCallNames = calledFunctionNames symbols programCalls
    let mightReachCatalog =
        programCallNames
        |> Set.exists (fun called ->
            Set.contains called packageCatalogFunctionNames
            || Set.contains called baseContext.PackageCatalogGenericCallers)
    if mightReachCatalog then
        materializeReachablePackageValueCatalog
            baseContext
            warningSettings
            catalog
            typedProgram
    else
        let (PackageValueCatalog entries) = catalog
        validateDistinctCatalogHashes entries
        |> Result.map (fun () -> typedProgram)
