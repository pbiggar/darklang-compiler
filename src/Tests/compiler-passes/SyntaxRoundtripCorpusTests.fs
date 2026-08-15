// SyntaxRoundtripCorpusTests.fs - Corpus roundtrip tests for parser and pretty-printer.
//
// Uses all .e2e sources as a real-world corpus and enforces:
// parse -> pretty-print -> parse -> pretty-print
// while failing fast on the first mismatch.

module SyntaxRoundtripCorpusTests

open System
open System.Text.RegularExpressions
open AST
open Parser
open InterpreterParser
open ASTPrettyPrinter
open TestDSL.E2EFormat

type TestResult = Result<unit, string>

type private SyntaxMode = {
    SourceSyntax: CompilerLibrary.SourceSyntax
}

type private Snippet = {
    Label: string
    Source: string
}

type private RoundtripPlan = {
    CheckLabel: string
    ParseOriginalSyntax: string
    PrettySyntax: ASTPrettyPrinter.Syntax
    ParsePrettySyntax: string
    ParsePretty: string -> Result<AST.Program, string>
}

let private syntaxModeForFile (sourceFile: string) : SyntaxMode =
    let normalized = sourceFile.Replace('\\', '/')
    let isUpstreamDark =
        normalized.Contains("/e2e/upstream/")
        && normalized.EndsWith(".dark", StringComparison.OrdinalIgnoreCase)

    if normalized.Contains("/interpreter/") || isUpstreamDark then
        {
            SourceSyntax = CompilerLibrary.InterpreterSyntax
        }
    else
        {
            SourceSyntax = CompilerLibrary.CompilerSyntax
        }

let private allowInternalForFile (sourceFile: string) : bool =
    let normalized = sourceFile.Replace('\\', '/')
    normalized.Contains("/stdlib-internal/")
    || normalized.EndsWith("/e2e/stdlib_internals.e2e")

let private parseBySyntax
    (sourceSyntax: CompilerLibrary.SourceSyntax)
    (allowInternal: bool)
    (source: string)
    : Result<AST.Program, string> =
    match sourceSyntax with
    | CompilerLibrary.CompilerSyntax -> Parser.parseString allowInternal source
    | CompilerLibrary.InterpreterSyntax -> InterpreterParser.parseString allowInternal source

let private sourceSyntaxName (sourceSyntax: CompilerLibrary.SourceSyntax) : string =
    match sourceSyntax with
    | CompilerLibrary.CompilerSyntax -> "CompilerSyntax"
    | CompilerLibrary.InterpreterSyntax -> "InterpreterSyntax"

let private prettySyntaxName (syntax: ASTPrettyPrinter.Syntax) : string =
    match syntax with
    | ASTPrettyPrinter.CompilerSyntax -> "CompilerSyntax"
    | ASTPrettyPrinter.InterpreterSyntax -> "InterpreterSyntax"

let private formatProgramSafe
    (syntax: ASTPrettyPrinter.Syntax)
    (ast: AST.Program)
    : Result<string, string> =
    try
        Ok (ASTPrettyPrinter.formatProgram syntax ast)
    with ex ->
        Error ex.Message

let private normalizeImplicitLambdaTypeVarSeedsInAstDebug (ast: AST.Program) : string =
    let astDebug = sprintf "%A" ast
    Regex.Replace(astDebug, "__interp_lambda_[0-9]+_", "__interp_lambda_N_")

let private roundtripPlans (mode: SyntaxMode) (allowInternal: bool) : RoundtripPlan list =
    let parseCompiler = parseBySyntax CompilerLibrary.CompilerSyntax allowInternal
    let parseInterpreter = parseBySyntax CompilerLibrary.InterpreterSyntax allowInternal
    let originalSyntaxName = sourceSyntaxName mode.SourceSyntax
    [
        {
            CheckLabel = "compiler-syntax"
            ParseOriginalSyntax = originalSyntaxName
            PrettySyntax = ASTPrettyPrinter.CompilerSyntax
            ParsePrettySyntax = sourceSyntaxName CompilerLibrary.CompilerSyntax
            ParsePretty = parseCompiler
        }
        {
            CheckLabel = "interpreter-syntax"
            ParseOriginalSyntax = originalSyntaxName
            PrettySyntax = ASTPrettyPrinter.InterpreterSyntax
            ParsePrettySyntax = sourceSyntaxName CompilerLibrary.InterpreterSyntax
            ParsePretty = parseInterpreter
        }
    ]

let private snippetsForTest (test: E2ETest) : Snippet list =
    let sourceSnippet =
        let intentionallyRejectedByParser =
            test.ExpectCompileError
            && (test.ExpectedErrorMessage
                |> Option.exists (fun message ->
                    message.StartsWith("Parse error:", StringComparison.Ordinal)))

        // A parser-rejection E2E case has no source AST to roundtrip. It still belongs
        // in the corpus as an executable negative syntax test, but is not a printer input.
        if intentionallyRejectedByParser then
            []
        else
            [ { Label = "source"; Source = test.Source } ]

    let expectedSnippet =
        match test.ExpectedValueExpr with
        | Some rhs ->
            [ { Label = "expected-value"; Source = rhs } ]
        | None -> []

    sourceSnippet @ expectedSnippet

let private failureHeader
    (kind: string)
    (plan: RoundtripPlan)
    (sourceFile: string)
    (testName: string)
    (snippetLabel: string)
    (allowInternal: bool)
    : string =
    [
        $"Roundtrip failure kind: {kind}"
        $"Roundtrip check: {plan.CheckLabel}"
        $"File: {sourceFile}"
        $"Test: {testName}"
        $"Snippet: {snippetLabel}"
        $"Parse original syntax: {plan.ParseOriginalSyntax}"
        $"Pretty syntax: {prettySyntaxName plan.PrettySyntax}"
        $"Parse pretty syntax: {plan.ParsePrettySyntax}"
        $"Allow internal: {allowInternal}"
    ]
    |> String.concat "\n"

let private roundtripWithPlan
    (sourceFile: string)
    (testName: string)
    (plan: RoundtripPlan)
    (allowInternal: bool)
    (ast0: AST.Program)
    (snippet: Snippet)
    : TestResult =
    match formatProgramSafe plan.PrettySyntax ast0 with
    | Error prettyError ->
        let header =
            failureHeader
                "PrettyPrintFailed"
                plan
                sourceFile
                testName
                snippet.Label
                allowInternal
        Error (
            $"{header}\n"
            + $"\nPretty-print error: {prettyError}\n"
            + "\nOriginal source:\n"
            + snippet.Source
        )
    | Ok printed0 ->
        match plan.ParsePretty printed0 with
        | Error prettyParseError ->
            let header =
                failureHeader
                    "ParsePrettyFailed"
                    plan
                    sourceFile
                    testName
                    snippet.Label
                    allowInternal
            Error (
                $"{header}\n"
                + $"\nPretty parse error: {prettyParseError}\n"
                + "\nOriginal source:\n"
                + snippet.Source
                + "\n\nPretty-printed source:\n"
                + printed0
            )
        | Ok ast1 ->
            match formatProgramSafe plan.PrettySyntax ast1 with
            | Error prettyError ->
                let header =
                    failureHeader
                        "PrettyPrintSecondPassFailed"
                        plan
                        sourceFile
                        testName
                        snippet.Label
                        allowInternal
                Error (
                    $"{header}\n"
                    + $"\nPretty-print second pass error: {prettyError}\n"
                    + "\nOriginal source:\n"
                    + snippet.Source
                )
            | Ok printed1 ->
                let astChanged =
                    if ast0 = ast1 then
                        false
                    else
                        let normalizedAst0Debug = normalizeImplicitLambdaTypeVarSeedsInAstDebug ast0
                        let normalizedAst1Debug = normalizeImplicitLambdaTypeVarSeedsInAstDebug ast1
                        normalizedAst0Debug <> normalizedAst1Debug

                if astChanged then
                    let ast0Repr = sprintf "%A" ast0
                    let ast1Repr = sprintf "%A" ast1
                    let header =
                        failureHeader
                            "AstChangedAfterRoundtrip"
                            plan
                            sourceFile
                            testName
                            snippet.Label
                            allowInternal
                    Error (
                        $"{header}\n"
                        + "\nOriginal source:\n"
                        + snippet.Source
                        + "\n\nPretty-printed source (first pass):\n"
                        + printed0
                        + "\n\nPretty-printed source (second pass):\n"
                        + printed1
                        + "\n\nParsed AST (first parse):\n"
                        + ast0Repr
                        + "\n\nParsed AST (second parse):\n"
                        + ast1Repr
                    )
                elif printed0 <> printed1 then
                    let header =
                        failureHeader
                            "PrettyNotIdempotent"
                            plan
                            sourceFile
                            testName
                            snippet.Label
                            allowInternal
                    Error (
                        $"{header}\n"
                        + "\nOriginal source:\n"
                        + snippet.Source
                        + "\n\nPretty-printed source (first pass):\n"
                        + printed0
                        + "\n\nPretty-printed source (second pass):\n"
                        + printed1
                    )
                else
                    Ok ()

let private roundtripSnippet
    (sourceFile: string)
    (testName: string)
    (mode: SyntaxMode)
    (allowInternal: bool)
    (snippet: Snippet)
    : TestResult =
    let parseOriginal = parseBySyntax mode.SourceSyntax allowInternal
    let parsedOriginal =
        match parseOriginal snippet.Source with
        | Ok ast ->
            Ok ast
        | Error originalParseError
            when snippet.Label = "source"
                 && snippet.Source.Contains("\n")
                 && originalParseError.Contains("Unexpected tokens after expression") ->
            // E2E source snippets may be unwrapped multiline blocks (outer parens
            // stripped by the E2E parser). Retry with grouping parens.
            match parseOriginal $"({snippet.Source})" with
            | Ok ast -> Ok ast
            | Error _ -> Error originalParseError
        | Error originalParseError ->
            Error originalParseError

    match parsedOriginal with
    | Error originalParseError ->
        let parseOriginalSyntax = sourceSyntaxName mode.SourceSyntax
        Error (
            "Roundtrip failure kind: ParseOriginalFailed\n"
            + "Roundtrip check: initial-parse\n"
            + $"File: {sourceFile}\n"
            + $"Test: {testName}\n"
            + $"Snippet: {snippet.Label}\n"
            + $"Parse original syntax: {parseOriginalSyntax}\n"
            + $"Allow internal: {allowInternal}\n"
            + $"\nOriginal parse error: {originalParseError}\n"
            + "\nOriginal source:\n"
            + snippet.Source
        )
    | Ok ast0 ->
        let rec loop (remainingPlans: RoundtripPlan list) : TestResult =
            match remainingPlans with
            | [] -> Ok ()
            | plan :: rest ->
                match roundtripWithPlan sourceFile testName plan allowInternal ast0 snippet with
                | Ok () -> loop rest
                | Error _ as err -> err
        loop (roundtripPlans mode allowInternal)

let private runRoundtripForTest
    (sourceFile: string)
    (mode: SyntaxMode)
    (allowInternal: bool)
    (test: E2ETest)
    : TestResult =
    let rec loop (remaining: Snippet list) : TestResult =
        match remaining with
        | [] -> Ok ()
        | snippet :: rest ->
            match roundtripSnippet sourceFile test.Name mode allowInternal snippet with
            | Ok () -> loop rest
            | Error _ as err -> err
    loop (snippetsForTest test)

let private runRoundtripForFile (sourceFile: string) : TestResult =
    match parseE2ETestFile sourceFile with
    | Error msg -> Error $"Failed to parse E2E file before roundtrip: {sourceFile}\n{msg}"
    | Ok tests ->
        let mode = syntaxModeForFile sourceFile
        let allowInternal = allowInternalForFile sourceFile
        let rec loop (remaining: E2ETest list) : TestResult =
            match remaining with
            | [] -> Ok ()
            | test :: rest ->
                match runRoundtripForTest sourceFile mode allowInternal test with
                | Ok () -> loop rest
                | Error _ as err -> err
        loop tests

let private runRoundtripForAllE2EFiles (testFiles: string array) : TestResult =
    let orderedFiles = testFiles |> Array.sort |> Array.toList
    let rec loop (remaining: string list) : TestResult =
        match remaining with
        | [] -> Ok ()
        | testFile :: rest ->
            match runRoundtripForFile testFile with
            | Ok () -> loop rest
            | Error _ as err -> err
    loop orderedFiles

let tests (testFiles: string array) : (string * (unit -> TestResult)) list = [
    ("parser/pretty corpus roundtrip", fun () -> runRoundtripForAllE2EFiles testFiles)
]
