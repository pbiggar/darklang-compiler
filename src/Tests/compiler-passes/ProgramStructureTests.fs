// ProgramStructureTests.fs - Whole-program source-unit, overlay, and entry validation probes.

module ProgramStructureTests

open AST

type TestResult = Result<unit, string>

let private source name purpose text : CompilationContexts.SourceUnit =
    { Name = name; Purpose = purpose; Source = text }

let private compile
    (stdlib: CompilationContexts.StdlibResult)
    mode
    sources
    : CompilerOptions.CompileReport =
    CompilerLibrary.compile {
        Context = CompilationContexts.StdlibOnly stdlib
        Mode = mode
        Sources = NonEmptyList.fromList sources
        AllowInternal = false
        Verbosity = 0
        Options = CompilerOptions.defaultOptions
        PackageValues = CompilationContexts.emptyPackageValueCatalog
        PassTimingRecorder = None
        Session = None
    }

let private execute (report: CompilerOptions.CompileReport) (binary: byte array) =
    TestDSL.E2ETestRunner.executeBinaryForTarget report.Target binary

let private expectCompileError (expected: string) (report: CompilerOptions.CompileReport) : TestResult =
    match report.Result with
    | Error error when error.Contains expected -> Ok ()
    | Error error -> Error $"Expected compile error containing '{expected}', got: {error}"
    | Ok _ -> Error $"Expected compile error containing '{expected}', but compilation succeeded"

let testOrderedSourceComposition (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    let report =
        compile
            stdlib
            CompilerOptions.TestExpression
            [ source "library.dark" NameSyntax.SourceUnitPurpose.Library
                "let answer (x: Int64): Int64 = x + 1L"
              source "entry.dark" NameSyntax.SourceUnitPurpose.Executable "answer 41L" ]
    match report.Result with
    | Error error -> Error error
    | Ok binary ->
        match execute report binary with
        | Error error -> Error $"Multi-unit program did not execute: {error}"
        | Ok output ->
            if output.ExitCode = 0 && output.Stdout = "42\n" then Ok ()
            else Error $"Unexpected multi-unit output: exit={output.ExitCode}; stdout={output.Stdout}; stderr={output.Stderr}"

let testLastFunctionDeclarationWins (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    let report =
        compile
            stdlib
            CompilerOptions.TestExpression
            [ source "first.dark" NameSyntax.SourceUnitPurpose.Library
                "let overlaid (x: Int64): Int64 = x + 1L"
              source "second.dark" NameSyntax.SourceUnitPurpose.Library
                "let overlaid (x: Int64): Int64 = x + 2L"
              source "entry.dark" NameSyntax.SourceUnitPurpose.Executable "overlaid 40L" ]
    match report.Result with
    | Error error -> Error error
    | Ok binary ->
        match execute report binary with
        | Error error -> Error $"Overlay program did not execute: {error}"
        | Ok output ->
            if output.ExitCode = 0 && output.Stdout = "42\n" then Ok ()
            else Error $"Unexpected overlay output: exit={output.ExitCode}; stdout={output.Stdout}; stderr={output.Stderr}"

let testDependencyEntryRejected (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    compile stdlib CompilerOptions.FullProgram
        [source "dependency.dark" NameSyntax.SourceUnitPurpose.Package "1"]
    |> expectCompileError "must contain declarations only"

let testMissingEntryRejected (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    compile stdlib CompilerOptions.FullProgram
        [source "library.dark" NameSyntax.SourceUnitPurpose.Executable
            "let identity (x: Int64): Int64 = x"]
    |> expectCompileError "exactly one entry expression; found 0"

let testMultipleEntriesRejected (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    compile stdlib CompilerOptions.FullProgram
        [source "one.dark" NameSyntax.SourceUnitPurpose.Executable "1"
         source "two.dark" NameSyntax.SourceUnitPurpose.Executable "2"]
    |> expectCompileError "exactly one entry expression; found 2"

let testFileEntryTypeRejected (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    compile stdlib CompilerOptions.FullProgram
        [source "file.dark" NameSyntax.SourceUnitPurpose.Executable "\"render only in eval mode\""]
    |> expectCompileError "File entry expression must return Unit, Int, or Int64"

/// A library unit that declares a function the stdlib already carries under a
/// non-Stdlib name (darklang/dark's LanguageTools modules are in both). The two
/// lowerings differ, and the merge used to crash on the name instead of keeping
/// the prebuilt copy as it does for Stdlib.* names.
let testLibraryRedeclaresStdlibCarriedFunction (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    let report =
        compile
            stdlib
            CompilerOptions.TestExpression
            [ source "library.dark" NameSyntax.SourceUnitPurpose.Library
                (String.concat "\n" [
                    "module Darklang.LanguageTools.PackageManager"
                    ""
                    "let countMatchingPrefix (a: List<String>) (b: List<String>) : Int ="
                    "  match (a, b) with"
                    "  | (aHead :: aTail, bHead :: bTail) ->"
                    "    if aHead == bHead then"
                    "      1 + (countMatchingPrefix aTail bTail)"
                    "    else"
                    "      0"
                    "  | _ -> 0" ])
              source "entry.dark" NameSyntax.SourceUnitPurpose.Executable
                "Darklang.LanguageTools.PackageManager.countMatchingPrefix [\"a\", \"b\", \"c\"] [\"a\", \"b\", \"x\"]" ]
    match report.Result with
    | Error error -> Error error
    | Ok binary ->
        match execute report binary with
        | Error error -> Error $"Redeclaring program did not execute: {error}"
        | Ok output ->
            if output.ExitCode = 0 && output.Stdout = "2\n" then Ok ()
            else Error $"Unexpected redeclaring output: exit={output.ExitCode}; stdout={output.Stdout}; stderr={output.Stderr}"

let tests (stdlib: CompilationContexts.StdlibResult) = [
    ("compose ordered named source units", testOrderedSourceComposition stdlib)
    ("last function declaration wins", testLastFunctionDeclarationWins stdlib)
    ("a library may redeclare a function the stdlib carries", testLibraryRedeclaresStdlibCarriedFunction stdlib)
    ("dependency entry is rejected", testDependencyEntryRejected stdlib)
    ("missing entry is rejected", testMissingEntryRejected stdlib)
    ("multiple entries are rejected", testMultipleEntriesRejected stdlib)
    ("file entry type is restricted", testFileEntryTypeRejected stdlib)
]
