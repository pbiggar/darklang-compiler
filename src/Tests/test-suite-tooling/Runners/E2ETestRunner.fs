// E2ETestRunner.fs - End-to-end test runner
//
// Compiles source code, executes it, and validates output/exit code.

module TestDSL.E2ETestRunner

open System
open AST
open AST_to_ANF
open TypeChecking
open TestDSL.E2EFormat

// Internal identifiers are only allowed for stdlib-internal tests.
let private isInternalTestFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/") || normalized.Contains("/verification/")

// Choose parser syntax from the test suite directory.
let private sourceSyntaxForTestFile (sourceFile: string) : CompilerLibrary.SourceSyntax =
    let normalized = sourceFile.Replace('\\', '/')
    let isUpstreamDark =
        normalized.Contains("/e2e/upstream/")
        && normalized.EndsWith(".dark", StringComparison.OrdinalIgnoreCase)

    if normalized.Contains("/interpreter/") || isUpstreamDark then
        CompilerLibrary.InterpreterSyntax
    else
        CompilerLibrary.CompilerSyntax

let private typeCheckProgramForSourceSyntax
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (typeCheckEnv: TypeCheckEnv)
    (program: Program)
    : Result<Type * Program * TypeCheckEnv, TypeError> =
    let warningSettings = CompilerLibrary.defaultWarningSettings
    match sourceSyntax with
    | CompilerLibrary.InterpreterSyntax ->
        TypeChecking.checkProgramWithBaseEnvAndSettings typeCheckEnv true warningSettings program
    | CompilerLibrary.CompilerSyntax ->
        TypeChecking.checkProgramWithBaseEnvAndSettings typeCheckEnv false warningSettings program

// Build the source expression to execute for a test.
// For `lhs = rhs` value tests, run a synthesized equality assertion.

let private prettySyntaxForSourceSyntax (sourceSyntax: CompilerLibrary.SourceSyntax) : ASTPrettyPrinter.Syntax =
    match sourceSyntax with
    | CompilerLibrary.CompilerSyntax -> ASTPrettyPrinter.CompilerSyntax
    | CompilerLibrary.InterpreterSyntax -> ASTPrettyPrinter.InterpreterSyntax

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
            Call ("Stdlib.Float.abs", NonEmptyList.singleton (BinOp (Sub, lhsExpr, rhsExpr)))
        BinOp (Lt, absDiff, FloatLiteral valueFloatEpsilon)
    else
        BinOp (Eq, lhsExpr, rhsExpr)

let private tryFormatProgramIfStable
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (program: Program)
    : string option =
    let prettySyntax = prettySyntaxForSourceSyntax sourceSyntax
    let formatted = ASTPrettyPrinter.formatProgram prettySyntax program
    match CompilerLibrary.parseProgram sourceSyntax allowInternal formatted with
    | Ok reparsed ->
        match sourceSyntax with
        | CompilerLibrary.CompilerSyntax ->
            if reparsed = program then Some formatted else None
        | CompilerLibrary.InterpreterSyntax ->
            // Interpreter syntax intentionally normalizes some AST details
            // (for example around lambda forms), so parse-success is enough.
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
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (source: string)
    (rhsExpr: string)
    : string option =
    let sourceProgramResult = CompilerLibrary.parseProgram sourceSyntax allowInternal source
    let rhsProgramResult = CompilerLibrary.parseProgram sourceSyntax allowInternal rhsExpr

    match sourceProgramResult, rhsProgramResult with
    | Ok (Program sourceTopLevels), Ok rhsProgram ->
        match List.rev sourceTopLevels, asSingleExpression rhsProgram with
        | Expression lhsExpr :: sourceRestRev, Some rhsAst ->
            let comparisonExpr = buildValueComparisonExpr lhsExpr rhsAst
            let directEqProgram =
                match sourceSyntax with
                | CompilerLibrary.CompilerSyntax ->
                    // Keep the value check isolated in its own top-level def so
                    // the parser cannot attach the synthesized expression to the
                    // previous definition body.
                    let valueCheckFuncName = pickValueCheckFuncName sourceTopLevels
                    let valueCheckFuncDef : AST.FunctionDef = {
                        Name = valueCheckFuncName
                        TypeParams = []
                        // Match parser-generated synthetic unit parameter naming so
                        // pretty-print/parse roundtrips stay structurally stable.
                        Params = NonEmptyList.singleton ("$unit0", TUnit)
                        ReturnType = TBool
                        Body = comparisonExpr
                    }
                    Program (
                        List.rev (
                            Expression (Call (valueCheckFuncName, NonEmptyList.singleton UnitLiteral))
                            :: FunctionDef valueCheckFuncDef
                            :: sourceRestRev
                        )
                    )
                | CompilerLibrary.InterpreterSyntax ->
                    Program (List.rev (Expression comparisonExpr :: sourceRestRev))

            tryFormatProgramIfStable sourceSyntax allowInternal directEqProgram
        | _ ->
            None
    | _ ->
        None

let private sourceToExecute
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (test: E2ETest)
    : Result<string, string> =
    match test.ExpectedValueExpr with
    | Some rhsExpr ->
        match trySynthesizeValueEqualitySource sourceSyntax allowInternal test.Source rhsExpr with
        | Some rewritten -> Ok rewritten
        | None ->
            // Some interpreter-specific forms (for example operator sections) can fail
            // AST pretty-print roundtrips even though the direct source is valid.
            // Fall back to textual wrapping and parse-validate before execution.
            let fallbackSource = $"({test.Source}) == ({rhsExpr})"
            match CompilerLibrary.parseProgram sourceSyntax allowInternal fallbackSource with
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
type PreambleContextKey = string * string

let preambleContextKeyForTest (test: E2ETest) : PreambleContextKey =
    (test.SourceFile, test.Preamble)

type private PreambleBuildSpec = {
    SourceFile: string
    Preamble: string
    FunctionLineMap: Map<string, int>
    AllowInternal: bool
    SourceSyntax: CompilerLibrary.SourceSyntax
}

/// Map of built preamble contexts keyed by source file + preamble text
type PreambleContextMap = Map<PreambleContextKey, CompilerLibrary.PreambleContext>

type SuiteContext = {
    Stdlib: CompilerLibrary.StdlibResult
    PreambleContexts: PreambleContextMap
}

type private PreamblePlan = {
    Spec: PreambleBuildSpec
    Analysis: CompilerLibrary.PreambleAnalysis option
    Specialization: SpecializationResult
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
            let sourceSyntax = sourceSyntaxForTestFile sourceFile
            Ok {
                SourceFile = sourceFile
                Preamble = preamble
                FunctionLineMap = funcLineMap
                AllowInternal = isInternalTestFile sourceFile
                SourceSyntax = sourceSyntax
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

// Value rendering is generated after type checking. Include the generic calls
// that a renderer introduces so prebuilt E2E contexts contain the same Dict
// specializations as a full-program compilation.
let rec private collectRendererSpecs (typ: Type) : Set<SpecKey> =
    match typ with
    | TDict (TString, valueType) ->
        Set.add ("Stdlib.Dict.toList", [valueType]) (collectRendererSpecs valueType)
    | TList elementType -> collectRendererSpecs elementType
    | TTuple elementTypes
    | TEnumFields elementTypes ->
        elementTypes
        |> List.map collectRendererSpecs
        |> List.fold Set.union Set.empty
    | TFunction (parameterTypes, returnType) ->
        parameterTypes @ [returnType]
        |> List.map collectRendererSpecs
        |> List.fold Set.union Set.empty
    | TRecord (_, typeArgs)
    | TSum (_, typeArgs) ->
        typeArgs
        |> List.map collectRendererSpecs
        |> List.fold Set.union Set.empty
    | _ -> Set.empty

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
    | PRecord (_, fields) ->
        fields
        |> List.map snd
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
    | ListCons (headElements, tailExpr) ->
        let headRefs =
            headElements
            |> List.map (collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars)
            |> combineMany
        let tailRefs =
            collectExprReferencedPreambleFuncsWithBound knownPreambleFunctions boundVars tailExpr
        Set.union headRefs tailRefs
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
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (preamble: string)
    : Result<Program, string> =
    let preambleTerminator =
        match sourceSyntax with
        | CompilerLibrary.CompilerSyntax -> "0"
        | CompilerLibrary.InterpreterSyntax -> "0L"

    let preambleSource =
        match sourceSyntax with
        | CompilerLibrary.CompilerSyntax ->
            preamble + $"\n{preambleTerminator}"
        | CompilerLibrary.InterpreterSyntax ->
            // Interpreter syntax supports `;` as a top-level separator. Add it
            // before the synthetic terminator so a trailing identifier body like
            // `= x` does not absorb the terminator as an application argument.
            preamble + $"\n;\n{preambleTerminator}"
    CompilerLibrary.parseProgram sourceSyntax allowInternal preambleSource

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
        match parsePreambleAsProgram spec.SourceSyntax spec.AllowInternal spec.Preamble with
        | Ok program -> Ok program
        | Error primaryErr ->
            let sanitizedPreamble = sanitizePreambleForReducedFallback spec.Preamble
            if sanitizedPreamble = spec.Preamble then
                Error primaryErr
            else
                parsePreambleAsProgram spec.SourceSyntax spec.AllowInternal sanitizedPreamble
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
                CompilerLibrary.parseProgram spec.SourceSyntax spec.AllowInternal test.Source
                |> Result.isError)

        let seedFunctions =
            if hasUnparsableTestSource then
                preambleFunctionNames
            else
                runnableTests
                |> List.map (fun test ->
                    CompilerLibrary.parseProgram spec.SourceSyntax spec.AllowInternal test.Source
                    |> Result.map (collectProgramReferencedPreambleFuncs preambleFunctionNames))
                |> List.choose Result.toOption
                |> List.fold Set.union Set.empty

        let requiredFunctions =
            seedFunctions
            |> Set.filter (fun name -> Set.contains name preambleFunctionNames)
            |> expandRequiredPreambleFunctions dependencyMap

        let reducedTopLevels =
            reducePreambleTopLevelsToRequiredFunctions requiredFunctions preambleTopLevels

        let reducedProgram =
            Program (reducedTopLevels @ [Expression (Int64Literal 0L)])

        TypeChecking.checkSyntheticPreambleWithBaseEnvAndSettings
            stdlib.Context.TypeCheckEnv
            (spec.SourceSyntax = CompilerLibrary.InterpreterSyntax)
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
        match CompilerLibrary.analyzePreamble spec.SourceSyntax spec.AllowInternal stdlib spec.Preamble with
        | Ok analysis ->
            Ok (Some analysis)
        | Error primaryErr when
            spec.SourceSyntax = CompilerLibrary.InterpreterSyntax
            && isUpstreamDarkTestFile spec.SourceFile ->
            analyzePreambleWithReducedFunctionSet stdlib spec tests
            |> Result.map Some
            |> Result.mapError (fun reducedErr ->
                $"Preamble parse error in {spec.SourceFile}: {primaryErr}\nReduced preamble fallback failed: {reducedErr}")
        | Error primaryErr ->
            Error $"Preamble parse error in {spec.SourceFile}: {primaryErr}"

let private collectSpecsFromTests
    (allowInternal: bool)
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (typeCheckEnv: TypeCheckEnv)
    (tests: E2ETest list)
    : Result<Set<SpecKey> * TypeRegistry * VariantLookup, string> =
    let registriesFromTypedProgram (typedAst: Program) : TypeRegistry * VariantLookup =
        match AST_to_ANF.splitTopLevels typedAst with
        | Ok (typeDefs, functions, _expr) ->
            let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
            let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
            let registries = AST_to_ANF.buildRegistries Map.empty typeDefs aliasReg resolvedFunctions
            (registries.TypeReg, registries.VariantLookup)
        | Error _ ->
            (Map.empty, Map.empty)
    let testsToAnalyze =
        tests
        |> List.filter (fun test -> not test.ExpectCompileError && Option.isNone test.SkipReason)
    let rec loop remaining accSpecs accTypeReg accVariantLookup =
        match remaining with
        | [] -> Ok (accSpecs, accTypeReg, accVariantLookup)
        | test :: rest ->
            match sourceToExecute sourceSyntax allowInternal test with
            | Error _ ->
                // Best effort: source synthesis may fail for malformed tests and
                // should not block suite-level specialization discovery.
                loop rest accSpecs accTypeReg accVariantLookup
            | Ok source ->
                let specsResult =
                    CompilerLibrary.parseProgram sourceSyntax allowInternal source
                    |> Result.bind (fun testAst ->
                        typeCheckProgramForSourceSyntax sourceSyntax typeCheckEnv testAst
                        |> Result.mapError typeErrorToString
                        |> Result.map (fun (programType, typedAst, _) ->
                            let (typeReg, variantLookup) = registriesFromTypedProgram typedAst
                            (Set.union (collectTypeAppsFromProgram typedAst) (collectRendererSpecs programType), typeReg, variantLookup)))

                match specsResult with
                | Ok (specs, typeReg, variantLookup) ->
                    let mergedTypeReg =
                        Map.fold (fun acc k v -> Map.add k v acc) accTypeReg typeReg
                    let mergedVariantLookup =
                        Map.fold (fun acc k v -> Map.add k v acc) accVariantLookup variantLookup
                    loop rest (Set.union accSpecs specs) mergedTypeReg mergedVariantLookup
                | Error _ ->
                    // Best effort: a test may intentionally fail to parse/typecheck,
                    // and that should not prevent building suite-level specializations.
                    loop rest accSpecs accTypeReg accVariantLookup
    loop testsToAnalyze Set.empty Map.empty Map.empty

let private buildPreamblePlan
    (stdlib: CompilerLibrary.StdlibResult)
    (spec: PreambleBuildSpec)
    (tests: E2ETest list)
    : Result<PreamblePlan * Set<SpecKey> * TypeRegistry * VariantLookup, string> =
    let analysisResult = analyzePreambleForPlan stdlib spec tests

    analysisResult
    |> Result.bind (fun analysisOpt ->
        let typeCheckEnv =
            match analysisOpt with
            | Some analysis -> analysis.TypeCheckEnv
            | None -> stdlib.Context.TypeCheckEnv
        collectSpecsFromTests spec.AllowInternal spec.SourceSyntax typeCheckEnv tests
        |> Result.bind (fun (testSpecs, testTypeReg, testVariantLookup) ->
            let preambleSpecs =
                match analysisOpt with
                | None -> Set.empty
                | Some analysis -> collectTypeAppsFromProgram analysis.TypedAST
            let (preambleTypeReg, preambleVariantLookup) =
                match analysisOpt with
                | None -> (Map.empty, Map.empty)
                | Some analysis ->
                    match AST_to_ANF.splitTopLevels analysis.TypedAST with
                    | Ok (typeDefs, functions, _expr) ->
                        let aliasReg = AST_to_ANF.buildAliasRegistry typeDefs
                        let resolvedFunctions = AST_to_ANF.resolveAliasesInFunctions aliasReg functions
                        let registries = AST_to_ANF.buildRegistries Map.empty typeDefs aliasReg resolvedFunctions
                        (registries.TypeReg, registries.VariantLookup)
                    | Error _ ->
                        (Map.empty, Map.empty)
            let externalTypeReg =
                Map.fold (fun acc k v -> Map.add k v acc) testTypeReg preambleTypeReg
            let externalVariantLookup =
                Map.fold (fun acc k v -> Map.add k v acc) testVariantLookup preambleVariantLookup
            let combinedSpecs = Set.union testSpecs preambleSpecs
            let preambleGenericDefs =
                match analysisOpt with
                | None -> Map.empty
                | Some analysis -> analysis.GenericFuncDefs
            let preambleSpecsForDefs = filterSpecsByDefs preambleGenericDefs combinedSpecs
            let stdlibSpecsFromCombined = filterSpecsByDefs stdlib.Context.GenericFuncDefs combinedSpecs
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
            let stdlibSpecs = Set.union stdlibSpecsFromCombined stdlibSpecsFromSpecialization
            let plan = {
                Spec = spec
                Analysis = analysisOpt
                Specialization = specialization
            }
            Ok (plan, stdlibSpecs, externalTypeReg, externalVariantLookup)))

/// Build suite stdlib specializations and per-file/per-preamble contexts
let buildSuiteContexts
    (stdlib: CompilerLibrary.StdlibResult)
    (tests: E2ETest array)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : Result<SuiteContext, string> =
    let groupedTests =
        tests
        |> Array.toList
        |> List.groupBy preambleContextKeyForTest

    let plansResult =
        groupedTests
        |> List.fold
            (fun acc (contextKey, group) ->
                acc
                |> Result.bind (fun (plans, stdlibSpecs, stdlibTypeReg, stdlibVariantLookup) ->
                    let (sourceFile, _) = contextKey
                    buildPreambleBuildSpec sourceFile group
                    |> Result.bind (fun spec ->
                        buildPreamblePlan stdlib spec group
                        |> Result.map (fun (plan, specs, typeReg, variantLookup) ->
                            let mergedTypeReg =
                                Map.fold (fun acc k v -> Map.add k v acc) stdlibTypeReg typeReg
                            let mergedVariantLookup =
                                Map.fold (fun acc k v -> Map.add k v acc) stdlibVariantLookup variantLookup
                            ((contextKey, plan) :: plans, Set.union stdlibSpecs specs, mergedTypeReg, mergedVariantLookup)))))
            (Ok ([], Set.empty, Map.empty, Map.empty))

    plansResult
    |> Result.bind (fun (plans, stdlibSpecs, externalTypeReg, externalVariantLookup) ->
        CompilerLibrary.buildStdlibSpecializations stdlib stdlibSpecs externalTypeReg externalVariantLookup passTimingRecorder
        |> Result.bind (fun stdlibWithSpecs ->
            let buildEmptyContext () : CompilerLibrary.PreambleContext =
                {
                    Context = stdlibWithSpecs.Context
                    ANFFunctions = []
                    TypeMap = stdlibWithSpecs.StdlibTypeMap
                    SymbolicFunctions = []
                }
            let contextsResult =
                plans
                |> List.fold
                    (fun acc (contextKey, plan) ->
                        acc
                        |> Result.bind (fun (currentStdlib, contexts) ->
                            let ctxResult =
                                match plan.Analysis with
                                | None -> Ok (currentStdlib, buildEmptyContext ())
                                | Some analysis ->
                                    CompilerLibrary.buildPreambleContextFromAnalysis
                                        currentStdlib
                                        analysis
                                        plan.Specialization
                                        plan.Spec.SourceFile
                                        plan.Spec.FunctionLineMap
                                        passTimingRecorder
                                    |> Result.mapError (fun err ->
                                        $"Preamble build error ({plan.Spec.SourceFile}): {err}")
                            ctxResult
                            |> Result.map (fun (updatedStdlib, ctx) ->
                                (updatedStdlib, Map.add contextKey ctx contexts))))
                    (Ok (stdlibWithSpecs, Map.empty))
            contextsResult
            |> Result.map (fun (updatedStdlib, contexts) ->
                {
                    Stdlib = updatedStdlib
                    PreambleContexts = contexts
                })))

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

let private didValueEqualityPass (run: E2ERun) : bool =
    if exitCodeFromRun run <> 0 then
        false
    else
        let lastStdoutLine =
            (stdoutFromRun run).Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryLast
        lastStdoutLine = Some "true"

let private evaluateExpectations (test: E2ETest) (run: E2ERun) : E2ETestResult =
    if test.ExpectCompileError then
        let gotError = exitCodeFromRun run <> 0
        if not gotError then
            failRun run "Expected compilation error but compilation succeeded"
        else
            match test.ExpectedErrorMessage with
            | Some expectedMsg ->
                let output = stderrFromRun run
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
            failRun run "Value mismatch"
    else
        let stdoutMatches =
            match test.ExpectedStdout with
            | None -> true
            | Some expected ->
                let actual = stdoutFromRun run
                actual.Trim() = expected.Trim()

        let stderrMatches =
            match test.ExpectedStderr with
            | None -> true
            | Some expected ->
                let actual = stderrFromRun run
                actual.Trim() = expected.Trim()

        let exitCodeMatches = exitCodeFromRun run = test.ExpectedExitCode

        if stdoutMatches && stderrMatches && exitCodeMatches then
            Ok run
        else
            failRun run "Output mismatch"

let private buildCompilerOptions
    (_sourceSyntax: CompilerLibrary.SourceSyntax)
    (test: E2ETest)
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

let private tryExecuteBinary (target: Platform.Target) (binary: byte array) : Result<CompilerLibrary.ExecutionOutput, string> =
    try Ok (CompilerLibrary.execute target 0 binary)
    with ex -> Error ex.Message

let private compileAndRun (request: CompilerLibrary.CompileRequest) : E2ERun =
    let compileReport = CompilerLibrary.compile request
    match compileReport.Result with
    | Error err ->
        CompileFailed (1, err, compileReport.CompileTime)
    | Ok binary ->
        match tryExecuteBinary compileReport.Target binary with
        | Ok execResult ->
            Ran (execResult.ExitCode, execResult.Stdout, execResult.Stderr, compileReport.CompileTime, execResult.RuntimeTime)
        | Error err ->
            Ran (-1, "", $"Execution failed: {err}", compileReport.CompileTime, TimeSpan.Zero)

let private combinePreambleWithTestSource
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (preamble: string)
    (source: string)
    : string =
    if String.IsNullOrWhiteSpace preamble then
        source
    else
        match sourceSyntax with
        | CompilerLibrary.CompilerSyntax ->
            preamble + "\n" + source
        | CompilerLibrary.InterpreterSyntax ->
            // Keep a hard top-level separator between preamble and test expression.
            preamble + "\n;\n" + source

let private tryBuildReducedPreambleForTest
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (preamble: string)
    (testSource: string)
    : string option =
    let parsePreambleResult = parsePreambleAsProgram sourceSyntax allowInternal preamble
    let parseTestResult = CompilerLibrary.parseProgram sourceSyntax allowInternal testSource

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

        let prettySyntax = prettySyntaxForSourceSyntax sourceSyntax
        let reducedPreambleSource = ASTPrettyPrinter.formatProgram prettySyntax (Program reducedTopLevels)
        Some reducedPreambleSource
    | _ ->
        None

/// Run E2E test using a prebuilt preamble context
let runE2ETestWithPreambleContext
    (stdlib: CompilerLibrary.StdlibResult)
    (preambleCtx: CompilerLibrary.PreambleContext)
    (test: E2ETest)
    (passTimingRecorder: CompilerLibrary.PassTimingRecorder option)
    : E2ETestResult =
    let allowInternal = isInternalTestFile test.SourceFile
    let sourceSyntax = sourceSyntaxForTestFile test.SourceFile
    let options = buildCompilerOptions sourceSyntax test
    match sourceToExecute sourceSyntax allowInternal test with
    | Error msg ->
        let run = CompileFailed (1, msg, TimeSpan.Zero)
        failRun run msg
    | Ok source ->
        let request : CompilerLibrary.CompileRequest = {
            Context = CompilerLibrary.StdlibWithPreamble (stdlib, preambleCtx)
            Mode = CompilerLibrary.CompileMode.TestExpression
            SourceSyntax = sourceSyntax
            Source = source
            SourceFile = test.SourceFile
            AllowInternal = allowInternal
            Verbosity = 0
            Options = options
            PassTimingRecorder = passTimingRecorder
        }
        let run = compileAndRun request
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
                tryBuildReducedPreambleForTest sourceSyntax allowInternal test.Preamble source
                |> Option.defaultValue test.Preamble
            let fallbackSource = combinePreambleWithTestSource sourceSyntax fallbackPreamble source
            let fallbackRequest : CompilerLibrary.CompileRequest = {
                Context = CompilerLibrary.StdlibOnly stdlib
                Mode = CompilerLibrary.CompileMode.FullProgram
                SourceSyntax = sourceSyntax
                Source = fallbackSource
                SourceFile = test.SourceFile
                AllowInternal = allowInternal
                Verbosity = 0
                Options = options
                PassTimingRecorder = passTimingRecorder
            }
            let fallbackRun = compileAndRun fallbackRequest
            match evaluateExpectations test fallbackRun with
            | Ok _ as success ->
                success
            | Error _ ->
                primaryResult
        else
            primaryResult
