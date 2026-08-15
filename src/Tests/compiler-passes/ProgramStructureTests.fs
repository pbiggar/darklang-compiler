// ProgramStructureTests.fs - Whole-program source-unit, overlay, and entry validation probes.

module ProgramStructureTests

open AST

type TestResult = Result<unit, string>

let private source name purpose text : CompilerLibrary.SourceUnit =
    { Name = name; Purpose = purpose; Source = text }

let private compile
    (stdlib: CompilerLibrary.StdlibResult)
    mode
    sources
    : CompilerLibrary.CompileReport =
    CompilerLibrary.compile {
        Context = CompilerLibrary.StdlibOnly stdlib
        Mode = mode
        SourceSyntax = CompilerLibrary.CompilerSyntax
        Sources = NonEmptyList.fromList sources
        AllowInternal = false
        Verbosity = 0
        Options = CompilerLibrary.defaultOptions
        PackageValues = CompilerLibrary.emptyPackageValueCatalog
        PassTimingRecorder = None
    }

let private expectCompileError (expected: string) (report: CompilerLibrary.CompileReport) : TestResult =
    match report.Result with
    | Error error when error.Contains expected -> Ok ()
    | Error error -> Error $"Expected compile error containing '{expected}', got: {error}"
    | Ok _ -> Error $"Expected compile error containing '{expected}', but compilation succeeded"

let testOrderedSourceComposition (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    let report =
        compile
            stdlib
            CompilerLibrary.TestExpression
            [ source "library.dark" NameSyntax.SourceUnitPurpose.Library
                "let answer(x: Int64): Int64 = x + 1"
              source "entry.dark" NameSyntax.SourceUnitPurpose.Executable "answer(41)" ]
    match report.Result with
    | Error error -> Error error
    | Ok binary ->
        let output =
            CompilerLibrary.executeCaptured report.Target 0 CompilerLibrary.Closed binary
        if output.ExitCode = 0 && output.Stdout = "42\n" then Ok ()
        else Error $"Unexpected multi-unit output: exit={output.ExitCode}; stdout={output.Stdout}; stderr={output.Stderr}"

let testLastFunctionDeclarationWins (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    let report =
        compile
            stdlib
            CompilerLibrary.TestExpression
            [ source "first.dark" NameSyntax.SourceUnitPurpose.Library
                "let overlaid(x: Int64): Int64 = x + 1"
              source "second.dark" NameSyntax.SourceUnitPurpose.Library
                "let overlaid(x: Int64): Int64 = x + 2"
              source "entry.dark" NameSyntax.SourceUnitPurpose.Executable "overlaid(40)" ]
    match report.Result with
    | Error error -> Error error
    | Ok binary ->
        let output = CompilerLibrary.executeCaptured report.Target 0 CompilerLibrary.Closed binary
        if output.ExitCode = 0 && output.Stdout = "42\n" then Ok ()
        else Error $"Unexpected overlay output: exit={output.ExitCode}; stdout={output.Stdout}; stderr={output.Stderr}"

let testDependencyEntryRejected (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    compile stdlib CompilerLibrary.FullProgram
        [source "dependency.dark" NameSyntax.SourceUnitPurpose.Package "1"]
    |> expectCompileError "must contain declarations only"

let testMissingEntryRejected (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    compile stdlib CompilerLibrary.FullProgram
        [source "library.dark" NameSyntax.SourceUnitPurpose.Executable
            "let identity(x: Int64): Int64 = x"]
    |> expectCompileError "exactly one entry expression; found 0"

let testMultipleEntriesRejected (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    compile stdlib CompilerLibrary.FullProgram
        [source "one.dark" NameSyntax.SourceUnitPurpose.Executable "1"
         source "two.dark" NameSyntax.SourceUnitPurpose.Executable "2"]
    |> expectCompileError "exactly one entry expression; found 2"

let testFileEntryTypeRejected (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    compile stdlib CompilerLibrary.FullProgram
        [source "file.dark" NameSyntax.SourceUnitPurpose.Executable "\"render only in eval mode\""]
    |> expectCompileError "File entry expression must return Unit, Int, or Int64"

let tests (stdlib: CompilerLibrary.StdlibResult) = [
    ("compose ordered named source units", testOrderedSourceComposition stdlib)
    ("last function declaration wins", testLastFunctionDeclarationWins stdlib)
    ("dependency entry is rejected", testDependencyEntryRejected stdlib)
    ("missing entry is rejected", testMissingEntryRejected stdlib)
    ("multiple entries are rejected", testMultipleEntriesRejected stdlib)
    ("file entry type is restricted", testFileEntryTypeRejected stdlib)
]
