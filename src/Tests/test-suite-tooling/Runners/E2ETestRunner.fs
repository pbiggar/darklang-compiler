// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open AST
open AST_to_ANF
open TestDSL.E2EFormat

// Internal identifiers are only allowed for stdlib-internal tests.
let private isInternalTestFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/") || normalized.Contains("/verification/")

// Build the source expression to execute for a test.
// For `lhs = rhs` value tests, run a synthesized equality assertion.

let private asSingleExpression (program: Program) : Expr option =
    let (Program topLevels) = program
    match topLevels with
    | [Expression expr] -> Some expr
    | _ -> None

let private valueFloatEpsilon : float = 0.00000000001

let private isFloatExpectedExpr (expr: Expr) : bool =
    match expr with
    | FloatLiteral _ -> true
    | UnaryOp (Neg, FloatLiteral _) -> true
    | _ -> false

let private buildValueComparisonExpr (lhsExpr: Expr) (rhsExpr: Expr) : Expr =
    if isFloatExpectedExpr rhsExpr then
        // For float value tests, compare with epsilon tolerance.
        let absDiff =
            Call ("Stdlib.Float.absoluteValue", NonEmptyList.singleton (BinOp (Sub, lhsExpr, rhsExpr)))
        BinOp (Lt, absDiff, FloatLiteral valueFloatEpsilon)
    else
        BinOp (Eq, lhsExpr, rhsExpr)

let private tryFormatProgramIfStable
    (allowInternal: bool)
    (program: Program)
    : string option =
    let formatted = ASTPrettyPrinter.formatProgram ASTPrettyPrinter.InterpreterSyntax program
    match CompilerLibrary.parseProgram allowInternal formatted with
    | Ok _ ->
        // Stable recursive identities include structural declaration paths.
        // Test synthesis inserts a checker declaration, so a valid reparse can
        // deliberately receive different identities while retaining the same
        // source semantics. The dedicated syntax corpus owns exact roundtrips.
        Some formatted
    | Error _ -> None

let private pickValueCheckFuncName (topLevels: TopLevel list) : string =
    let existingNames =
        topLevels
        |> List.choose (function
            | FunctionDef fn -> Some fn.Name
            | _ -> None)
        |> Set.ofList
    let rec loop idx =
        let candidate =
            if idx = 0 then
                "e2eValueCheck"
            else
                $"e2eValueCheck{idx}"
        if Set.contains candidate existingNames then
            loop (idx + 1)
        else
            candidate
    loop 0

let private trySynthesizeValueEqualitySource
    (allowInternal: bool)
    (source: string)
    (rhsExpr: string)
    : string option =
    let sourceProgramResult = CompilerLibrary.parseProgram allowInternal source
    let rhsProgramResult = CompilerLibrary.parseProgram allowInternal rhsExpr

    match sourceProgramResult, rhsProgramResult with
    | Ok (Program sourceTopLevels), Ok rhsProgram ->
        match List.rev sourceTopLevels, asSingleExpression rhsProgram with
        | Expression lhsExpr :: sourceRestRev, Some rhsAst ->
            let comparisonExpr = buildValueComparisonExpr lhsExpr rhsAst
            let directEqProgram =
                Program (List.rev (Expression comparisonExpr :: sourceRestRev))

            tryFormatProgramIfStable allowInternal directEqProgram
        | _ ->
            None
    | _ ->
        None

let private sourceToExecute
    (allowInternal: bool)
    (test: E2ETest)
    : Result<string, string> =
    match test.ExpectedValueExpr with
    | Some rhsExpr ->
        match trySynthesizeValueEqualitySource allowInternal test.Source rhsExpr with
        | Some rewritten -> Ok rewritten
        | None ->
            // Some interpreter-specific forms (for example operator sections) can fail
            // AST pretty-print roundtrips even though the direct source is valid.
            // Fall back to textual wrapping and parse-validate before execution.
            let fallbackSource = $"({test.Source}) == ({rhsExpr})"
            match CompilerLibrary.parseProgram allowInternal fallbackSource with
            | Ok _ -> Ok fallbackSource
            | Error _ ->
                Error (
                    $"Failed to synthesize expected-value source for test '{test.Name}' in {test.SourceFile}.\n"
                    + "Expected-value tests must parse as a program whose last top-level is an expression,\n"
                    + "and RHS must parse as a single expression."
                )
    | None -> Ok test.Source

/// Result of running an E2E test
type E2ERun =
    | CompileFailed of exitCode:int * error:string * compileTime:TimeSpan
    | Ran of exitCode:int * stdout:string * stderr:string * compileTime:TimeSpan * runtimeTime:TimeSpan

type E2EFailure = {
    Run: E2ERun
    Message: string
}

type E2ETestResult = Result<E2ERun, E2EFailure>

/// A value-equality test whose synthesized checker is a single expression and
/// can therefore share one compiler invocation with other compatible checks.
type PreparedE2EBatchTest = {
    Test: E2ETest
    EqualitySource: string
}

/// One physical compile/run with one logical result for every test in it.
type E2EBatchExecution = {
    AggregateRun: E2ERun
    Results: (E2ETest * E2ETestResult) list
}

let maxSupportedBatchSize = 62

/// Only value-equality tests with no process contract can share a process. The
/// compiler path and options remain production-identical; only the synthesized
/// caller contains several independent checks.
let tryPrepareBatchTest (test: E2ETest) : PreparedE2EBatchTest option =
    let eligibleExpectation =
        Option.isSome test.ExpectedValueExpr
        && Option.isNone test.ExpectedStdout
        && Option.isNone test.ExpectedStderr
        && List.isEmpty test.Arguments
        && test.Stdin = TestDSL.E2EFormat.Closed
        && test.ExpectedExitCode = 0
        && not test.ExpectCompileError
        && Option.isNone test.SkipReason

    if not eligibleExpectation then
        None
    else
        let allowInternal = isInternalTestFile test.SourceFile
        match sourceToExecute allowInternal test with
        | Error _ -> None
        | Ok equalitySource ->
            match CompilerLibrary.parseProgram allowInternal equalitySource with
            | Ok (Program [Expression _]) ->
                Some { Test = test; EqualitySource = equalitySource }
            | Ok _
            | Error _ -> None

type PreambleContextKey = string * string

let preambleContextKeyForTest (test: E2ETest) : PreambleContextKey =
    (test.SourceFile, test.Preamble)

type private PreambleBuildSpec = {
    SourceFile: string
    Preamble: string
    FunctionLineMap: Map<string, int>
    AllowInternal: bool
}

/// Map of built preamble contexts and their matching stdlib specialization set,
/// keyed by source file + preamble text.
type PreambleContextMap =
    Map<PreambleContextKey, CompilerLibrary.StdlibResult * CompilerLibrary.PreambleContext>

type SuiteContext = {
    PreambleContexts: PreambleContextMap
}

type private PreamblePlan = {
    Spec: PreambleBuildSpec
    Analysis: CompilerLibrary.PreambleAnalysis option
    Specialization: SpecializationResult
    StdlibSpecs: Set<SpecKey>
    ExternalTypeReg: AST_to_ANF.TypeRegistry
    ExternalVariantLookup: VariantLookup
}

let private buildPreambleBuildSpec (sourceFile: string) (tests: E2ETest list) : Result<PreambleBuildSpec, string> =
    let preambles =
        tests
        |> List.map (fun test -> test.Preamble)
        |> List.distinct
    match preambles with
    | [preamble] ->
        let funcLineMaps =
            tests
            |> List.map (fun test -> test.FunctionLineMap)
            |> List.distinct
        match funcLineMaps with
        | [funcLineMap] ->
            Ok {
                SourceFile = sourceFile
                Preamble = preamble
                FunctionLineMap = funcLineMap
                AllowInternal = isInternalTestFile sourceFile
            }
        | _ ->
            Error $"Multiple function line maps found for {sourceFile}"
    | _ ->
        Error $"Multiple preambles found for {sourceFile}"

let private collectTypeAppsFromProgram (program: Program) : Set<SpecKey> =
    let (Program topLevels) = program
    topLevels
    |> List.map (function
        | FunctionDef f when List.isEmpty f.TypeParams -> collectTypeAppsFromFunc f
        | Expression e -> collectTypeApps e
        | _ -> Set.empty)
    |> List.fold Set.union Set.empty

let private filterSpecsByDefs (genericDefs: GenericFuncDefs) (specs: Set<SpecKey>) : Set<SpecKey> =
    specs |> Set.filter (fun (funcName, _) -> Map.containsKey funcName genericDefs)

let private isUpstreamDarkTestFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/e2e/upstream/")
    && normalized.EndsWith(".dark", StringComparison.OrdinalIgnoreCase)

let rec private collectPatternBoundNames (pattern: Pattern) : Set<string> =
    match pattern with
    | PVar name ->
        Set.singleton name
    | PConstructor (_, payloadOpt) ->
        payloadOpt
        |> Option.map collectPatternBoundNames
        |> Option.defaultValue Set.empty
    | PTuple patterns ->
        patterns
        |> List.map collectPatternBoundNames
        |> List.fold Set.union Set.empty
    | PList patterns ->
        patterns
        |> List.map collectPatternBoundNames
        |> List.fold Set.union Set.empty
    | PListCons (headPatterns, tailPattern) ->
        let headBound =
            headPatterns
            |> List.map collectPatternBoundNames
            |> List.fold Set.union Set.empty
        Set.union headBound (collectPatternBoundNames tailPattern)
    | _ ->
        Set.empty

let rec private collectLetPatternBoundNames (pattern: LetPattern) : Set<string> =
    match pattern with
    | LPVariable name -> Set.singleton name
    | LPTuple (first, second, rest) ->
        first :: second :: rest
        |> List.map collectLetPatternBoundNames
        |> List.fold Set.union Set.empty
    | LPUnit | LPWildcard -> Set.empty

let rec private collectExprReferencedPreambleFuncsWithBound
    (knownPreambleFunctions: Set<string>)
    (boundVars: Set<string>)
    (expr: Expr)
    : Set<string> =
    let combineMany (sets: Set<string> list) : Set<string> =
        sets |> List.fold Set.union Set.empty

    let collectCallLike (funcName: string) (args: NonEmptyList<Expr>) : Set<string> =
        let fromFuncName =
            if Set.contains funcName knownPreambleFunctions
               && not (Set.contains funcName boundVars) then
                Set.singleton funcName
            else
                Set.empty
        let fromArgs =
            args
            |> NonEmptyList.toList
            |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
            |> combineMany
        Set.union fromFuncName fromArgs

    match expr with
    | BoundaryRender (_, value) ->
        collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars value
    | UnitLiteral
    | Int64Literal _
    | Int128Literal _
    | BigIntLiteral _
    | Int8Literal _
    | Int16Literal _
    | Int32Literal _
    | UInt8Literal _
    | UInt16Literal _
    | UInt32Literal _
    | UInt64Literal _
    | UInt128Literal _
    | BoolLiteral _
    | StringLiteral _
    | CharLiteral _
    | FloatLiteral _
    | RuntimeError _ ->
        Set.empty
    | InterpolatedString parts ->
        parts
        |> List.map (function
            | StringText _ -> Set.empty
            | StringExpr partExpr ->
                collectExprReferencedPreambleFuncsWithBound
                    knownPreambleFunctions
                    boundVars
                    partExpr)
        |> combineMany
    | BinOp (_, left, right) ->
        Set.union
            (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars left)
            (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars right)
    | UnaryOp (_, inner) ->
        collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars inner
    | Let (pattern, valueExpr, bodyExpr) ->
        let valueRefs =
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars valueExpr
        let bodyRefs =
            collectExprReferencedPreambleFuncsWithBound
                knownPreambleFunctions
                (Set.union boundVars (collectLetPatternBoundNames pattern))
                bodyExpr
        Set.union valueRefs bodyRefs
    | RecursiveLet (recursion, valueExpr, bodyExpr) ->
        let recursiveBound = Set.add (recursiveBindingName recursion) boundVars
        Set.union
            (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions recursiveBound valueExpr)
            (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions recursiveBound bodyExpr)
    | Var name ->
        if Set.contains name knownPreambleFunctions
           && not (Set.contains name boundVars) then
            Set.singleton name
        else
            Set.empty
    | If (condExpr, thenExpr, elseExpr) ->
        combineMany [
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars condExpr
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars thenExpr
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars elseExpr
        ]
    | Sequence (firstExpr, nextExpr) ->
        Set.union
            (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars firstExpr)
            (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars nextExpr)
    | Call (funcName, args) ->
        collectCallLike funcName args
    | TypeApp (funcName, _typeArgs, args) ->
        collectCallLike funcName args
    | TupleLiteral elements ->
        elements
        |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
        |> combineMany
    | TupleAccess (tupleExpr, _index) ->
        collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars tupleExpr
    | DictLiteral (_, entries) ->
        entries
        |> List.map snd
        |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
        |> combineMany
    | RecordLiteral (_typeName, fields) ->
        fields
        |> List.map snd
        |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
        |> combineMany
    | RecordUpdate (recordExpr, updates) ->
        let recordRefs =
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars recordExpr
        let updateRefs =
            updates
            |> List.map snd
            |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
            |> combineMany
        Set.union recordRefs updateRefs
    | RecordAccess (recordExpr, _fieldName) ->
        collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars recordExpr
    | Constructor (_typeName, _variantName, payloadOpt) ->
        payloadOpt
        |> Option.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
        |> Option.defaultValue Set.empty
    | Match (scrutineeExpr, cases) ->
        let scrutineeRefs =
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars scrutineeExpr
        let caseRefs =
            cases
            |> List.map (fun case ->
                let caseBoundNames =
                    case.Patterns
                    |> NonEmptyList.toList
                    |> List.map collectPatternBoundNames
                    |> List.fold Set.union Set.empty
                let guardRefs =
                    case.Guard
                    |> Option.map (
                        collectExprReferencedPreambleFuncsWithBound
                            knownPreambleFunctions
                            boundVars
                    )
                    |> Option.defaultValue Set.empty
                let bodyRefs =
                    collectExprReferencedPreambleFuncsWithBound
                        knownPreambleFunctions
                        (Set.union boundVars caseBoundNames)
                        case.Body
                Set.union guardRefs bodyRefs)
            |> combineMany
        Set.union scrutineeRefs caseRefs
    | ListLiteral elements ->
        elements
        |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
        |> combineMany
    | Lambda (parameters, _, bodyExpr) ->
        let lambdaBoundVars =
            parameters
            |> NonEmptyList.toList
            |> List.map (fun parameter -> collectLetPatternBoundNames parameter.Pattern)
            |> List.fold Set.union Set.empty
            |> Set.union boundVars
        collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions lambdaBoundVars bodyExpr
    | Apply (funcExpr, args)
    | IndirectApply (funcExpr, args) ->
        let funcRefs =
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars funcExpr
        let argRefs =
            args
            |> NonEmptyList.toList
            |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
            |> combineMany
        Set.union funcRefs argRefs
    | FuncRef funcName ->
        if Set.contains funcName knownPreambleFunctions
           && not (Set.contains funcName boundVars) then
            Set.singleton funcName
        else
            Set.empty
    | Closure (funcName, captures) ->
        let fromFunc =
            if Set.contains funcName knownPreambleFunctions
               && not (Set.contains funcName boundVars) then
                Set.singleton funcName
            else
                Set.empty
        let fromCaptures =
            captures
            |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
            |> combineMany
        Set.union fromFunc fromCaptures

let private collectExprReferencedPreambleFuncs
    (knownPreambleFunctions: Set<string>)
    (expr: Expr)
    : Set<string> =
    collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions Set.empty expr

let private collectProgramReferencedPreambleFuncs
    (knownPreambleFunctions: Set<string>)
    (program: Program)
    : Set<string> =
    let (Program topLevels) = program
    topLevels
    |> List.map (function
        | FunctionDef funcDef ->
            let paramBoundVars =
                funcDef.Params
                |> NonEmptyList.toList
                |> List.map fst
                |> Set.ofList
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions paramBoundVars funcDef.Body
        | Expression expr ->
            collectExprReferencedPreambleFuncs knownPreambleFunctions expr
        | TypeDef _ ->
            Set.empty)
    |> List.fold Set.union Set.empty

let private collectFunctionReferencedPreambleFuncs
    (knownPreambleFunctions: Set<string>)
    (funcDef: FunctionDef)
    : Set<string> =
    let paramBoundVars =
        funcDef.Params
        |> NonEmptyList.toList
        |> List.map fst
        |> Set.ofList
    collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions paramBoundVars funcDef.Body

let private buildPreambleFunctionDependencyMap
    (preambleFunctionNames: Set<string>)
    (preambleFunctionDefs: FunctionDef list)
    : Map<string, Set<string>> =
    preambleFunctionDefs
    |> List.map (fun funcDef ->
        let deps = collectFunctionReferencedPreambleFuncs preambleFunctionNames funcDef
        (funcDef.Name, deps))
    |> Map.ofList

let private expandRequiredPreambleFunctions
    (dependencyMap: Map<string, Set<string>>)
    (initial: Set<string>)
    : Set<string> =
    let rec loop (pending: Set<string>) (required: Set<string>) : Set<string> =
        if Set.isEmpty pending then
            required
        else
            let discovered =
                pending
                |> Set.fold
                    (fun acc funcName ->
                        let deps = Map.tryFind funcName dependencyMap |> Option.defaultValue Set.empty
                        Set.union acc deps)
                    Set.empty
                |> Set.filter (fun name -> not (Set.contains name required))
            loop discovered (Set.union required discovered)
    loop initial initial

let private reducePreambleTopLevelsToRequiredFunctions
    (requiredFunctions: Set<string>)
    (preambleTopLevels: TopLevel list)
    : TopLevel list =
    preambleTopLevels
    |> List.filter (function
        | TypeDef _ -> true
        | FunctionDef funcDef -> Set.contains funcDef.Name requiredFunctions
        | Expression _ -> false)

let private parsePreambleAsProgram
    (allowInternal: bool)
    (preamble: string)
    : Result<Program, string> =
    CompilerLibrary.parseProgram allowInternal preamble

let private countLeadingSpaces (lineText: string) : int =
    lineText
    |> Seq.takeWhile (fun c -> c = ' ')
    |> Seq.length

let private isTopLevelPreambleDefinitionStart (trimmedLine: string) : bool =
    trimmedLine.StartsWith("let ")
    || trimmedLine.StartsWith("type ")
    || trimmedLine.StartsWith("def ")

/// Keep only top-level definitions and their continuation lines.
/// This is used by upstream reduced-preamble fallback when the raw per-test
/// preamble includes non-definition noise that cannot be parsed standalone.
let private sanitizePreambleForReducedFallback (preamble: string) : string =
    let lines = preamble.Split([| '\n' |], StringSplitOptions.None) |> Array.toList

    let rec loop
        (activeDefIndent: int option)
        (acc: string list)
        (remaining: string list)
        : string list =
        match remaining with
        | [] -> List.rev acc
        | line :: rest ->
            let trimmed = line.Trim()
            let indent = countLeadingSpaces line

            match activeDefIndent with
            | Some defIndent when trimmed = "" ->
                loop activeDefIndent (line :: acc) rest
            | Some defIndent when indent > defIndent ->
                loop activeDefIndent (line :: acc) rest
            | _ ->
                if indent = 0 && isTopLevelPreambleDefinitionStart trimmed then
                    loop (Some indent) (line :: acc) rest
                else
                    loop None acc rest

    loop None [] lines |> String.concat "\n"

let private analyzePreambleWithReducedFunctionSet
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    (tests: E2ETest list)
    : Result<CompilerLibrary.PreambleAnalysis, string> =
    let parseResult =
        match parsePreambleAsProgram spec.AllowInternal spec.Preamble with
        | Ok program -> Ok program
        | Error primaryErr ->
            let sanitizedPreamble = sanitizePreambleForReducedFallback spec.Preamble
            if sanitizedPreamble = spec.Preamble then
                Error primaryErr
            else
                parsePreambleAsProgram spec.AllowInternal sanitizedPreamble
                |> Result.mapError (fun reducedParseErr ->
                    $"{primaryErr}\nSanitized preamble parse failed: {reducedParseErr}")

    parseResult
    |> Result.bind (fun preambleProgram ->
        let (Program preambleTopLevels) = preambleProgram
        let preambleFunctionDefs =
            preambleTopLevels
            |> List.choose (function
                | FunctionDef funcDef -> Some funcDef
                | _ -> None)
        let preambleFunctionNames =
            preambleFunctionDefs
            |> List.map (fun funcDef -> funcDef.Name)
            |> Set.ofList
        let dependencyMap =
            buildPreambleFunctionDependencyMap preambleFunctionNames preambleFunctionDefs

        let runnableTests =
            tests
            |> List.filter (fun test -> not test.ExpectCompileError && Option.isNone test.SkipReason)

        let hasUnparsableTestSource =
            runnableTests
            |> List.exists (fun test ->
                sourceToExecute spec.AllowInternal test
                |> Result.bind (CompilerLibrary.parseProgram spec.AllowInternal)
                |> Result.isError)

        let seedFunctions =
            if hasUnparsableTestSource then
                preambleFunctionNames
            else
                runnableTests
                |> List.map (fun test ->
                    sourceToExecute spec.AllowInternal test
                    |> Result.bind (CompilerLibrary.parseProgram spec.AllowInternal)
                    |> Result.map (collectProgramReferencedPreambleFuncs preambleFunctionNames))
                |> List.choose Result.toOption
                |> List.fold Set.union Set.empty

        let requiredFunctions =
            seedFunctions
            |> Set.filter (fun name -> Set.contains name preambleFunctionNames)
            |> expandRequiredPreambleFunctions dependencyMap

        let reducedTopLevels =
            reducePreambleTopLevelsToRequiredFunctions requiredFunctions preambleTopLevels

        let reducedProgram = Program reducedTopLevels

        TypeChecking.checkSyntheticPreambleWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            true
            CompilerLibrary.defaultWarningSettings
            reducedProgram
        |> Result.mapError TypeChecking.typeErrorToString
        |> Result.map (fun (_programType, typedPreambleAst, preambleTypeCheckEnv) ->
            let preambleGenericDefs = AST_to_ANF.extractGenericFuncDefs typedPreambleAst
            {
                TypedAST = typedPreambleAst
                TypeCheckEnv = preambleTypeCheckEnv
                GenericFuncDefs = preambleGenericDefs
            }))

let private analyzePreambleForPlan
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    (tests: E2ETest list)
    : Result<CompilerLibrary.PreambleAnalysis option, string> =
    if String.IsNullOrWhiteSpace spec.Preamble then
        Ok None
    else
        match CompilerLibrary.analyzePreamble spec.AllowInternal stdlib spec.Preamble with
        | Ok analysis ->
            Ok (Some analysis)
        | Error primaryErr when isUpstreamDarkTestFile spec.SourceFile ->
            analyzePreambleWithReducedFunctionSet stdlib spec tests
            |> Result.map Some
            |> Result.mapError (fun reducedErr ->
                $"Preamble parse error in {spec.SourceFile}: {primaryErr}\nReduced preamble fallback failed: {reducedErr}")
        | Error primaryErr ->
            Error $"Preamble parse error in {spec.SourceFile}: {primaryErr}"

let private buildPreamblePlan
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    (tests: E2ETest list)
    : Result<PreamblePlan, string> =
    let analysisResult = analyzePreambleForPlan stdlib spec tests

    analysisResult
    |> Result.map (fun analysisOpt ->
        let preambleSpecs =
            match analysisOpt with
            | None -> Set.empty
            | Some analysis -> collectTypeAppsFromProgram analysis.TypedAST
        let (preambleTypeReg, preambleVariantLookup) =
            match analysisOpt with
            | None -> (Map.empty, Map.empty)
            | Some analysis ->
                match AST_to_ANF.splitDeclarations analysis.TypedAST with
                | Ok (typeDefs, functions) ->
                    let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
                    let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
                    let registries = AST_to_ANF.buildRegistries Map.empty typeDefs aliasReg resolvedFunctions
                    (registries.TypeReg, registries.VariantLookup)
                | Error _ ->
                    (Map.empty, Map.empty)
        let preambleGenericDefs =
            match analysisOpt with
            | None -> Map.empty
            | Some analysis -> analysis.GenericFuncDefs
        let preambleSpecsForDefs = filterSpecsByDefs preambleGenericDefs preambleSpecs
        let stdlibSpecsFromPreamble = filterSpecsByDefs stdlib.Context.GenericFuncDefs preambleSpecs
        let specialization =
            if Map.isEmpty preambleGenericDefs then
                {
                    SpecializedFuncs = []
                    SpecRegistry = Map.empty
                    ExternalSpecs = Set.empty
                }
            else
                specializeFromSpecs preambleGenericDefs preambleSpecsForDefs
        let stdlibSpecsFromSpecialization =
            filterSpecsByDefs stdlib.Context.GenericFuncDefs specialization.ExternalSpecs
        let stdlibSpecs = Set.union stdlibSpecsFromPreamble stdlibSpecsFromSpecialization
        let plan = {
            Spec = spec
            Analysis = analysisOpt
            Specialization = specialization
            StdlibSpecs = stdlibSpecs
            ExternalTypeReg = preambleTypeReg
            ExternalVariantLookup = preambleVariantLookup
        }
        plan)

/// Build suite stdlib specializations and per-file/per-preamble contexts
let buildSuiteContexts
    (stdlib: CompilerLibrary.StdlibResult)
    (tests: E2ETest array)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : Result<SuiteContext, string> =
    let recordTiming name (elapsed: TimeSpan) =
        passTimingRecorder
        |> Option.iter (fun record -> record { Pass = name; Elapsed = elapsed })

    let overlappingTimingNames =
        Set.ofList [
            "Start Function Compilation"
            "JSON Planning"
            "ARM64 Codegen Metadata"
            "ARM64 Codegen Functions"
            "ARM64 Codegen Helpers"
            "ARM64 Codegen Assembly"
            "ARM64 Codegen Peephole"
        ]

    let measureWithCompilerPasses name operation =
        let mutable nestedPassTime = TimeSpan.Zero
        let nestedRecorder =
            passTimingRecorder
            |> Option.map (fun outer ->
                fun (timing: CompilerLibrary.PassTiming) ->
                    if not (Set.contains timing.Pass overlappingTimingNames) then
                        nestedPassTime <- nestedPassTime + timing.Elapsed
                    outer timing)
        let timer = Diagnostics.Stopwatch.StartNew()
        let result = operation nestedRecorder
        timer.Stop()
        let overhead = timer.Elapsed - nestedPassTime
        if overhead > TimeSpan.Zero then recordTiming name overhead
        result

    let planningTimer = Diagnostics.Stopwatch.StartNew()
    let groupedTests =
        tests
        |> Array.toList
        |> List.groupBy preambleContextKeyForTest

    let plansResult =
        groupedTests
        |> List.fold
            (fun acc (contextKey, group) ->
                acc
                |> Result.bind (fun plans ->
                    let (sourceFile, _) = contextKey
                    buildPreambleBuildSpec sourceFile group
                    |> Result.bind (fun spec ->
                        buildPreamblePlan stdlib spec group
                        |> Result.map (fun plan -> (contextKey, plan) :: plans))))
            (Ok [])
    planningTimer.Stop()
    recordTiming "Suite Context Planning" planningTimer.Elapsed

    plansResult
    |> Result.bind (fun plans ->
        let specializedPlansResult =
            measureWithCompilerPasses
                "Suite Context Stdlib Specialization Overhead"
                (fun nestedRecorder ->
                    plans
                    |> List.fold
                        (fun acc (contextKey, plan) ->
                            acc
                            |> Result.bind (fun specializedPlans ->
                                CompilerLibrary.buildStdlibSpecializations
                                    stdlib
                                    plan.StdlibSpecs
                                    plan.ExternalTypeReg
                                    plan.ExternalVariantLookup
                                    nestedRecorder
                                |> Result.map (fun specializedStdlib ->
                                    (contextKey, plan, specializedStdlib) :: specializedPlans)))
                        (Ok []))
        specializedPlansResult
        |> Result.bind (fun specializedPlans ->
            measureWithCompilerPasses
                "Suite Context Preamble Build Overhead"
                (fun nestedRecorder ->
                    specializedPlans
                    |> List.fold
                        (fun acc (contextKey, plan, specializedStdlib) ->
                            acc
                            |> Result.bind (fun contexts ->
                                let ctxResult =
                                    match plan.Analysis with
                                    | None ->
                                        Ok ({
                                                Context = specializedStdlib.Context
                                                ANFFunctions = []
                                                TypeMap = specializedStdlib.StdlibTypeMap
                                                SymbolicFunctions = []
                                            } : CompilerLibrary.PreambleContext)
                                    | Some analysis ->
                                        CompilerLibrary.buildPreambleContextFromAnalysis
                                            specializedStdlib
                                            analysis
                                            plan.Specialization
                                            plan.Spec.SourceFile
                                            plan.Spec.FunctionLineMap
                                            nestedRecorder
                                        |> Result.map snd
                                        |> Result.mapError (fun err ->
                                            $"Preamble build error ({plan.Spec.SourceFile}): {err}")
                                ctxResult
                                |> Result.map (fun ctx ->
                                    Map.add contextKey (specializedStdlib, ctx) contexts)))
                        (Ok Map.empty))
            |> Result.map (fun contexts -> { PreambleContexts = contexts })))

let private exitCodeFromRun (run: E2ERun) : int =
    match run with
    | CompileFailed (exitCode, _, _) -> exitCode
    | Ran (exitCode, _, _, _, _) -> exitCode

let private stdoutFromRun (run: E2ERun) : string =
    match run with
    | CompileFailed _ -> ""
    | Ran (_, stdout, _, _, _) -> stdout

let private stderrFromRun (run: E2ERun) : string =
    match run with
    | CompileFailed (_, error, _) -> error
    | Ran (_, _, stderr, _, _) -> stderr

let private failRun (run: E2ERun) (message: string) : E2ETestResult =
    Error { Run = run; Message = message }

let private visibleOutput (value: string) : string =
    value.Replace("\\", "\\\\").Replace("\r", "\\r").Replace("\n", "\\n")

let private didValueEqualityPass (run: E2ERun) : bool =
    if exitCodeFromRun run <> 0 then
        false
    else
        let lastStdoutLine =
            (stdoutFromRun run).Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryLast
        lastStdoutLine = Some "true"

// Interpreter execution fixtures spell a returned Result.Error as `error=...`.
// Native execution renders the value instead of converting it to a process
// failure, so recognize that canonical Result presentation at this boundary.
let private isRenderedResultError (expectedMessage: string option) (run: E2ERun) : bool =
    if exitCodeFromRun run <> 0 then
        false
    else
        let output = stdoutFromRun run
        output.Contains(".Error(")
        && (match expectedMessage with
            | None -> true
            | Some message -> output.Contains(message))

let private evaluateExpectations (test: E2ETest) (run: E2ERun) : E2ETestResult =
    if test.ExpectCompileError then
        let signalExitCode =
            match run with
            | Ran (exitCode, _, _, _, _) when exitCode >= 128 -> Some exitCode
            | _ -> None
        match signalExitCode with
        | Some exitCode ->
            failRun run $"Expected a compiler or language error, but the generated program terminated by signal (exit {exitCode})"
        | None when exitCodeFromRun run = 0 && not (isRenderedResultError test.ExpectedErrorMessage run) ->
            failRun run "Expected compilation error but compilation succeeded"
        | None ->
            match test.ExpectedErrorMessage with
            | Some expectedMsg ->
                let output =
                    if exitCodeFromRun run = 0 then stdoutFromRun run else stderrFromRun run
                if output.Contains(expectedMsg) then
                    Ok run
                else
                    failRun run $"Expected error message '{expectedMsg}' not found in stderr. Actual stderr: {output}"
            | None ->
                Ok run
    elif Option.isSome test.ExpectedValueExpr then
        if didValueEqualityPass run then
            Ok run
        else
            let stderr = stderrFromRun run
            let detail = if String.IsNullOrWhiteSpace stderr then "" else $"\n{stderr.Trim()}"
            failRun run $"Value mismatch{detail}"
    else
        let stdoutMatches =
            match test.ExpectedStdout with
            | None -> true
            | Some expected ->
                let actual = stdoutFromRun run
                match test.OutputMatch with
                | TestDSL.E2EFormat.ExactBytes -> actual = expected
                | TestDSL.E2EFormat.NormalizedText -> actual.Trim() = expected.Trim()

        let stderrMatches =
            match test.ExpectedStderr with
            | None -> true
            | Some expected ->
                let actual = stderrFromRun run
                match test.OutputMatch with
                | TestDSL.E2EFormat.ExactBytes -> actual = expected
                | TestDSL.E2EFormat.NormalizedText -> actual.Trim() = expected.Trim()

        let exitCodeMatches = exitCodeFromRun run = test.ExpectedExitCode

        if stdoutMatches && stderrMatches && exitCodeMatches then
            Ok run
        else
            let expectedStdout = test.ExpectedStdout |> Option.defaultValue "<not asserted>"
            let expectedStderr = test.ExpectedStderr |> Option.defaultValue "<not asserted>"
            failRun run
                $"Output mismatch. stdout expected '{visibleOutput expectedStdout}', actual '{visibleOutput (stdoutFromRun run)}'; stderr expected '{visibleOutput expectedStderr}', actual '{visibleOutput (stderrFromRun run)}'"

let private buildCompilerOptions (test: E2ETest)
    : CompilerLibrary.CompilerOptions =
    { CompilerLibrary.defaultOptions with
        DisableFreeList = test.DisableFreeList
        DisableANFOpt = test.DisableANFOpt
        DisableANFConstFolding = test.DisableANFConstFolding
        DisableANFConstProp = test.DisableANFConstProp
        DisableANFCopyProp = test.DisableANFCopyProp
        DisableANFDCE = test.DisableANFDCE
        DisableANFStrengthReduction = test.DisableANFStrengthReduction
        DisableInlining = test.DisableInlining
        DisableTCO = test.DisableTCO
        DisableMIROpt = test.DisableMIROpt
        DisableMIRConstFolding = test.DisableMIRConstFolding
        DisableMIRCSE = test.DisableMIRCSE
        DisableMIRCopyProp = test.DisableMIRCopyProp
        DisableMIRDCE = test.DisableMIRDCE
        DisableMIRCFGSimplify = test.DisableMIRCFGSimplify
        DisableMIRLICM = test.DisableMIRLICM
        DisableLIROpt = test.DisableLIROpt
        DisableLIRPeephole = test.DisableLIRPeephole
        DisableFunctionTreeShaking = test.DisableFunctionTreeShaking
        EnableCoverage = false
        EnableLeakCheck = not test.DisableLeakCheck
        Warnings = CompilerLibrary.defaultWarningSettings
        DumpANF = false
        DumpMIR = false
        DumpLIR = false
    }

let private tryExecuteBinary
    (target: Platform.Target)
    (arguments: string list)
    (stdin: TestDSL.E2EFormat.TestStdin)
    (binary: byte array)
    : Result<CompilerLibrary.ExecutionOutput, string> =
    let input =
        match stdin with
        | TestDSL.E2EFormat.Closed -> CompilerLibrary.Closed
        | TestDSL.E2EFormat.Bytes value ->
            value |> System.Text.Encoding.UTF8.GetBytes |> CompilerLibrary.Bytes
    try Ok (CompilerLibrary.executeCapturedWithArguments target 0 arguments input binary)
    with ex -> Error ex.Message

let private compileAndRun
    (arguments: string list)
    (stdin: TestDSL.E2EFormat.TestStdin)
    (request: CompilerLibrary.CompileRequest)
    : E2ERun =
    let compileReport = CompilerLibrary.compile request
    match compileReport.Result with
    | Error err ->
        CompileFailed (1, err, compileReport.CompileTime)
    | Ok binary ->
        match tryExecuteBinary compileReport.Target arguments stdin binary with
        | Ok execResult ->
            Ran (execResult.ExitCode, execResult.Stdout, execResult.Stderr, compileReport.CompileTime, execResult.RuntimeTime)
        | Error err ->
            Ran (-1, "", $"Execution failed: {err}", compileReport.CompileTime, TimeSpan.Zero)

let canBatchTogether
    (left: PreparedE2EBatchTest)
    (right: PreparedE2EBatchTest)
    : bool =
    preambleContextKeyForTest left.Test = preambleContextKeyForTest right.Test
    && isInternalTestFile left.Test.SourceFile = isInternalTestFile right.Test.SourceFile
    && buildCompilerOptions left.Test = buildCompilerOptions right.Test

let private indentBatchBody (source: string) : string =
    source.Replace("\r\n", "\n").Split('\n')
    |> Array.map (fun line -> $"  {line}")
    |> String.concat "\n"

let private batchBindingPrefix (tests: PreparedE2EBatchTest list) : string =
    let existingNames =
        tests
        |> List.collect (fun prepared -> prepared.Test.FunctionLineMap |> Map.toList |> List.map fst)
        |> Set.ofList

    let rec pick attempt =
        let prefix =
            if attempt = 0 then "e2eBatchCase"
            else $"e2eBatchCase{attempt}_"
        let collides =
            tests
            |> List.indexed
            |> List.exists (fun (index, _) -> Set.contains $"{prefix}Result{index}" existingNames)
        if collides then pick (attempt + 1) else prefix
    pick 0

let buildBatchSource (tests: PreparedE2EBatchTest list) : string =
    let prefix = batchBindingPrefix tests
    let resultBindings =
        tests
        |> List.mapi (fun index prepared ->
            $"let {prefix}Result{index} =\n{indentBatchBody prepared.EqualitySource} in")
        |> String.concat "\n"
    let mask =
        tests
        |> List.mapi (fun index _ ->
            let bit = 1L <<< index
            $"(if {prefix}Result{index} then {bit}L else 0L)")
        |> String.concat "\n+ "
    $"{resultBindings}\n{mask}"

let tryParseBatchBoolResults
    (expectedCount: int)
    (stdout: string)
    : bool list option =
    let lastLine =
        stdout.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryLast
        |> Option.map (fun line -> line.Trim())

    match lastLine with
    | Some line ->
        match Int64.TryParse line with
        | true, mask when expectedCount > 0 && expectedCount <= maxSupportedBatchSize ->
            let allowedBits = (1L <<< expectedCount) - 1L
            if mask < 0L || (mask &&& (~~~allowedBits)) <> 0L then
                None
            else
                List.init expectedCount (fun index -> (mask &&& (1L <<< index)) <> 0L)
                |> Some
        | _ -> None
    | None -> None

let private splitDuration
    (count: int)
    (index: int)
    (duration: TimeSpan)
    : TimeSpan =
    let count64 = int64 count
    let quotient = duration.Ticks / count64
    let remainder = duration.Ticks % count64
    let ticks = quotient + (if int64 index < remainder then 1L else 0L)
    TimeSpan.FromTicks ticks

let private splitRun
    (count: int)
    (index: int)
    (stdout: string)
    (run: E2ERun)
    : E2ERun =
    match run with
    | CompileFailed (exitCode, error, compileTime) ->
        CompileFailed (exitCode, error, splitDuration count index compileTime)
    | Ran (exitCode, _, stderr, compileTime, runtimeTime) ->
        Ran (
            exitCode,
            stdout,
            stderr,
            splitDuration count index compileTime,
            splitDuration count index runtimeTime
        )

let runE2ETestBatchWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (session: CompilerLibrary.CompilationSession option)
    (tests: PreparedE2EBatchTest list)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2EBatchExecution =
    match tests with
    | [] ->
        let run = CompileFailed (1, "Cannot execute an empty E2E batch", TimeSpan.Zero)
        { AggregateRun = run; Results = [] }
    | first :: _ ->
        let count = tests.Length
        let request : CompilerLibrary.CompileRequest = {
            Context = CompilerLibrary.StdlibWithPreamble (stdlib, preambleCtx)
            Mode = CompilerLibrary.CompileMode.TestExpression
            Sources =
                NonEmptyList.singleton
                    { CompilerLibrary.SourceUnit.Name = first.Test.SourceFile
                      Purpose = NameSyntax.SourceUnitPurpose.Executable
                      Source = buildBatchSource tests }
            AllowInternal = isInternalTestFile first.Test.SourceFile
            Verbosity = 0
            Options = buildCompilerOptions first.Test
            PackageValues = CompilerLibrary.emptyPackageValueCatalog
            PassTimingRecorder = passTimingRecorder
            Session = session
        }
        let aggregateRun =
            compileAndRun [] TestDSL.E2EFormat.Closed request

        let results =
            match aggregateRun with
            | Ran (0, stdout, _, _, _) ->
                match tryParseBatchBoolResults count stdout with
                | Some values ->
                    List.map3 (fun index prepared passed ->
                        let caseStdout = if passed then "true\n" else "false\n"
                        let caseRun = splitRun count index caseStdout aggregateRun
                        (prepared.Test, evaluateExpectations prepared.Test caseRun))
                        [0 .. count - 1]
                        tests
                        values
                | None ->
                    tests
                    |> List.mapi (fun index prepared ->
                        let caseRun = splitRun count index stdout aggregateRun
                        (prepared.Test,
                         failRun caseRun $"Batch returned an invalid result vector for {count} tests. Last stdout: {visibleOutput stdout}"))
            | _ ->
                let message =
                    match aggregateRun with
                    | CompileFailed (_, error, _) -> $"Batch compilation failed: {error}"
                    | Ran (exitCode, _, stderr, _, _) ->
                        let detail =
                            if String.IsNullOrWhiteSpace stderr then ""
                            else $": {stderr.Trim()}"
                        $"Batch execution failed with exit code {exitCode}{detail}"
                tests
                |> List.mapi (fun index prepared ->
                    let caseRun = splitRun count index "" aggregateRun
                    (prepared.Test, failRun caseRun message))

        { AggregateRun = aggregateRun; Results = results }

let private tryBuildReducedPreambleForTest
    (allowInternal: bool)
    (preamble: string)
    (testSource: string)
    : string option =
    let parsePreambleResult = parsePreambleAsProgram allowInternal preamble
    let parseTestResult = CompilerLibrary.parseProgram allowInternal testSource

    match parsePreambleResult, parseTestResult with
    | Ok (Program preambleTopLevels), Ok testProgram ->
        let preambleFunctionDefs =
            preambleTopLevels
            |> List.choose (function
                | FunctionDef funcDef -> Some funcDef
                | _ -> None)

        let preambleFunctionNames =
            preambleFunctionDefs
            |> List.map (fun funcDef -> funcDef.Name)
            |> Set.ofList

        let dependencyMap =
            buildPreambleFunctionDependencyMap preambleFunctionNames preambleFunctionDefs

        let seedFunctions =
            collectProgramReferencedPreambleFuncs preambleFunctionNames testProgram

        let requiredFunctions =
            seedFunctions
            |> Set.filter (fun name -> Set.contains name preambleFunctionNames)
            |> expandRequiredPreambleFunctions dependencyMap

        let reducedTopLevels =
            reducePreambleTopLevelsToRequiredFunctions requiredFunctions preambleTopLevels

        let reducedPreambleSource = ASTPrettyPrinter.formatProgram ASTPrettyPrinter.InterpreterSyntax (Program reducedTopLevels)
        Some reducedPreambleSource
    | _ ->
        None

let private runE2ETestSourceWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (session: CompilerLibrary.CompilationSession option)
    (test: E2ETest)
    (source: string)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2ETestResult =
    let allowInternal = isInternalTestFile test.SourceFile
    let options = buildCompilerOptions test
    let request : CompilerLibrary.CompileRequest = {
        Context = CompilerLibrary.StdlibWithPreamble (stdlib, preambleCtx)
        Mode = CompilerLibrary.CompileMode.TestExpression
        Sources =
            NonEmptyList.singleton
                { CompilerLibrary.SourceUnit.Name = test.SourceFile
                  Purpose = NameSyntax.SourceUnitPurpose.Executable
                  Source = source }
        AllowInternal = allowInternal
        Verbosity = 0
        Options = options
        PackageValues = CompilerLibrary.emptyPackageValueCatalog
        PassTimingRecorder = passTimingRecorder
        Session = session
    }
    let run = compileAndRun test.Arguments test.Stdin request
    let primaryResult = evaluateExpectations test run

    let shouldTryRawPreambleFallback =
        match primaryResult with
        | Ok _ ->
            false
        | Error failure ->
            test.ExpectCompileError
            && Option.isSome test.ExpectedErrorMessage
            && isUpstreamDarkTestFile test.SourceFile
            && failure.Message.StartsWith("Expected error message", StringComparison.Ordinal)

    if shouldTryRawPreambleFallback then
        let fallbackPreamble =
            tryBuildReducedPreambleForTest allowInternal test.Preamble source
            |> Option.defaultValue test.Preamble
        let fallbackRequest : CompilerLibrary.CompileRequest = {
            Context = CompilerLibrary.StdlibOnly stdlib
            Mode = CompilerLibrary.CompileMode.FullProgram
            Sources =
                NonEmptyList.fromList
                    [{ CompilerLibrary.SourceUnit.Name = $"{test.SourceFile}:preamble"
                       Purpose = NameSyntax.SourceUnitPurpose.Library
                       Source = fallbackPreamble }
                     { CompilerLibrary.SourceUnit.Name = test.SourceFile
                       Purpose = NameSyntax.SourceUnitPurpose.Executable
                       Source = source }]
            AllowInternal = allowInternal
            Verbosity = 0
            Options = options
            PackageValues = CompilerLibrary.emptyPackageValueCatalog
            PassTimingRecorder = passTimingRecorder
            Session = session
        }
        let fallbackRun = compileAndRun test.Arguments test.Stdin fallbackRequest
        match evaluateExpectations test fallbackRun with
        | Ok _ as success ->
            success
        | Error _ ->
            primaryResult
    else
        primaryResult

/// Run E2E test using a prebuilt preamble context.
let runE2ETestWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (session: CompilerLibrary.CompilationSession option)
    (test: E2ETest)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2ETestResult =
    let allowInternal = isInternalTestFile test.SourceFile
    match sourceToExecute allowInternal test with
    | Error msg ->
        let run = CompileFailed (1, msg, TimeSpan.Zero)
        failRun run msg
    | Ok source ->
        runE2ETestSourceWithPreambleContext
            stdlib
            preambleCtx
            session
            test
            source
            passTimingRecorder

/// Run a prepared equality test singularly without reparsing its synthesized
/// source. This keeps batch-size comparisons from charging preparation twice.
let runPreparedE2ETestWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (session: CompilerLibrary.CompilationSession option)
    (prepared: PreparedE2EBatchTest)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2ETestResult =
    runE2ETestSourceWithPreambleContext
        stdlib
        preambleCtx
        session
        prepared.Test
        prepared.EqualitySource
        passTimingRecorder
