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
    PassTimingRecorder: PassTimingRecorder option
    Session: CompilationSession option
    Stdlib: StdlibResult
    BaseContext: PipelineContext
    Monomorphization: MonomorphizationMode
    ExternalInlineCandidates: Map<string, ANF_Inlining.FunctionInfo>
    PrebuiltSymbolicFunctions: LIR.Function list
    SkipFunctionNames: Set<string>
    EmitFunctionEvents: bool
    TreeShakeUserFunctions: bool
    Labels: UserCompileLabels
    Sources: AST.NonEmptyList<SourceUnit>
}

/// Parse canonical Dark source text into the compiler AST.
let parseProgram
    (allowInternal: bool)
    (source: string)
    : Result<AST.Program, string> =
    Parser.parseString allowInternal source

let private parseSourceTree
    (allowInternal: bool)
    (source: string)
    : Result<NameSyntax.ParsedSource, string> =
    Parser.parseSourceString allowInternal source

let private applyDeclarationOverlays (topLevels: AST.TopLevel list) : AST.TopLevel list =
    let declarationKey topLevel =
        match topLevel with
        | AST.FunctionDef definition -> Some ("function", definition.Name)
        | AST.ValueDef definition -> Some ("value", AST.valueDefName definition)
        | AST.TypeDef (AST.RecordDef (name, _, _))
        | AST.TypeDef (AST.SumTypeDef (name, _, _))
        | AST.TypeDef (AST.TypeAlias (name, _, _)) -> Some ("type", name)
        | AST.Expression _ -> None
    let winningIndices =
        topLevels
        |> List.indexed
        |> List.choose (fun (index, topLevel) ->
            declarationKey topLevel |> Option.map (fun key -> (key, index)))
        |> Map.ofList
    topLevels
    |> List.indexed
    |> List.choose (fun (index, topLevel) ->
        declarationKey topLevel
        |> Option.map (fun key -> if Map.tryFind key winningIndices = Some index then Some topLevel else None)
        |> Option.defaultValue (Some topLevel))

/// Parse every source unit independently and validate entry ownership before
/// crossing into the expression-oriented lowering AST.
let parseSourceProgram
    (allowInternal: bool)
    (sources: AST.NonEmptyList<SourceUnit>)
    : Result<NameSyntax.ValidatedExecutableProgram * AST.Program, string> =
    let rec parseUnits remaining parsedUnits loweredTopLevels =
        match remaining with
        | [] ->
            let sourceProgram =
                parsedUnits
                |> List.rev
                |> AST.NonEmptyList.fromList
                |> NameSyntax.createSourceProgram
            NameSyntax.validateExecutableProgram sourceProgram
            |> Result.map (fun validated ->
                let composedTopLevels = List.rev loweredTopLevels |> List.collect id
                (validated, AST.Program (applyDeclarationOverlays composedTopLevels)))
        | sourceUnit :: rest ->
            NameSyntax.sourceUnitName sourceUnit.Name
            |> Result.bind (fun name ->
                parseSourceTree allowInternal sourceUnit.Source
                |> Result.bind (fun parsed ->
                    Parser.lowerParsedSource allowInternal parsed
                    |> Result.bind (fun (AST.Program topLevels) ->
                        let parsedUnit : NameSyntax.ParsedSourceUnit =
                            { Name = name
                              Purpose = sourceUnit.Purpose
                              Source = parsed }
                        parseUnits rest (parsedUnit :: parsedUnits) (topLevels :: loweredTopLevels))))
    parseUnits (AST.NonEmptyList.toList sources) [] []

let private packageHashType =
    AST.TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])

let private packageLocationType =
    AST.TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])

let private runtimeValueType =
    AST.TSum ("Darklang.LanguageTools.RuntimeTypes.ValueType", [])

let private optionType (innerType: AST.Type) =
    AST.TSum ("Stdlib.Option.Option", [innerType])

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
    constructor "Stdlib.Option.Option" "None" None

let private optionSomeExpr (value: AST.Expr) : AST.Expr =
    constructor "Stdlib.Option.Option" "Some" (Some value)

let private call (name: string) (args: AST.Expr list) : AST.Expr =
    AST.Call (name, AST.NonEmptyList.fromList args)

let private addOrderedGroup
    (key: 'key)
    (value: 'value)
    (groups: ('key * 'value list) list)
    : ('key * 'value list) list
    when 'key: equality =
    let rec add remaining =
        match remaining with
        | [] -> [(key, [value])]
        | (existingKey, values) :: rest when existingKey = key ->
            (existingKey, values @ [value]) :: rest
        | group :: rest -> group :: add rest
    add groups

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
    (parameters: (string * AST.Type) list)
    (returnType: AST.Type)
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
    let (CheckedAST.Program topLevels) = program
    topLevels
    |> List.map (function
        | CheckedAST.FunctionDef func when List.isEmpty func.TypeParams ->
            Monomorphization.collectTypeAppsFromFunc func
        | CheckedAST.Expression expr -> Monomorphization.collectTypeApps expr
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private collectProgramCalls (program: CheckedAST.Program) : Set<string> =
    let (CheckedAST.Program topLevels) = program
    topLevels
    |> List.map (function
        | CheckedAST.FunctionDef func -> Monomorphization.collectCalledFunctions func.Body
        | CheckedAST.ValueDef valueDef -> Monomorphization.collectCalledFunctions valueDef.Body
        | CheckedAST.Expression expr -> Monomorphization.collectCalledFunctions expr
        | CheckedAST.TypeDef _ -> Set.empty)
    |> List.fold Set.union Set.empty

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
            |> Monomorphization.specializeFromSpecs genericDefs
        let requestedEvaluatorTypes =
            specialization.ExternalSpecs
            |> Set.toList
            |> List.choose (function
                | "Builtin.pmEvaluateValue", [resultType] -> Some resultType
                | _ -> None)
            |> Set.ofList
        let specializedCalls =
            specialization.SpecializedFuncs
            |> List.map (fun func -> Monomorphization.collectCalledFunctions func.Body)
            |> List.fold Set.union Set.empty
        let reachableCalls = Set.union (collectProgramCalls typedProgram) specializedCalls
        let needsFind = Set.contains "Builtin.pmFindValuesByValueType" reachableCalls
        let needsLocations = Set.contains "Builtin.pmGetLocationsByValue" reachableCalls
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
                |> List.fold
                    (fun groups entry ->
                        addOrderedGroup entry.RuntimeType entry.ValueHash groups)
                    []
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
                |> List.fold
                    (fun groups (key, location) -> addOrderedGroup key location groups)
                    []
            let locationExpr (location: CatalogPackageLocation) =
                AST.RecordLiteral (
                    AST.unresolvedRecordReference "Darklang.LanguageTools.ProgramTypes.PackageLocation" [],
                    [
                        ("owner", AST.StringLiteral location.Owner)
                        ("modules", location.Modules |> List.map AST.StringLiteral |> AST.ListLiteral)
                        ("name", AST.StringLiteral location.Name)
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

            let evaluatorFunction (resultType: AST.Type) =
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
                baseContext.TypeCheckEnv
                false
                warningSettings
                syntheticProgram
            |> Result.mapError (fun error ->
                $"Package value catalog validation failed: {CheckingDiagnostics.typeErrorToString error}")
            |> Result.map (fun (_, CheckedAST.Program generatedTopLevels, _) ->
                let (CheckedAST.Program userTopLevels) = typedProgram
                CheckedAST.Program (generatedTopLevels @ userTopLevels)))

let internal materializePackageValueCatalog
    (baseContext: PipelineContext)
    (warningSettings: AST.WarningSettings)
    (catalog: PackageValueCatalog)
    (typedProgram: CheckedAST.Program)
    : Result<CheckedAST.Program, string> =
    let programCalls = collectProgramCalls typedProgram
    let mightReachCatalog =
        programCalls
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
