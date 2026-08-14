// ValueSearchCatalogTests.fs - native integration tests for the AOT package-value catalog boundary.
//
// Catalog data is compilation input rather than Dark source, so these tests
// compile and execute complete programs through CompilerLibrary instead of the
// line-oriented E2E DSL.

module ValueSearchCatalogTests

open System.Numerics

type TestResult = Result<unit, string>

let private errorType = AST.TRecord ("Stdlib.Cli.Posix.Error", [])

let private customType hash typeArguments : CompilerLibrary.PackageCustomType = {
    Hash = hash
    TypeArguments = typeArguments
}

let private location branches owner modules name : CompilerLibrary.CatalogPackageLocation = {
    VisibleInBranches = branches
    Owner = owner
    Modules = modules
    Name = name
}

let private errorValue (errno: int) (message: string) =
    AST.RecordLiteral (
        "Stdlib.Cli.Posix.Error",
        [
            ("errno", AST.BigIntLiteral (BigInteger errno))
            ("message", AST.StringLiteral message)
        ]
    )

let private evaluator state : CompilerLibrary.TypedPackageValueEvaluator = {
    ResultType = errorType
    State = state
}

let private entry hash runtimeType locations state : CompilerLibrary.PackageValueCatalogEntry = {
    ValueHash = hash
    RuntimeType = runtimeType
    Locations = locations
    Evaluator = evaluator state
}

let private mainBranch = ["branch-main"]
let private otherBranch = ["branch-other"]

let private parityCatalog =
    let target = customType "type-error" []
    let other = customType "type-other" []
    let parameterized = customType "type-error" [customType "argument-type" []]
    CompilerLibrary.PackageValueCatalog [
        entry "value-first" target
            [location mainBranch "Owner" ["Nested"] "first"]
            (CompilerLibrary.Available (errorValue 1 "first"))
        entry "value-other-type" other
            [location mainBranch "Owner" [] "other"]
            (CompilerLibrary.Available (errorValue 20 "other"))
        entry "value-parameterized" parameterized
            [location mainBranch "Owner" [] "parameterized"]
            (CompilerLibrary.Available (errorValue 30 "parameterized"))
        entry "value-second" target
            [location mainBranch "Owner" ["Nested"; "Deep"] "second"]
            (CompilerLibrary.Available (errorValue 2 "second"))
        entry "value-multiple" target
            [ location mainBranch "Owner" ["Nested"; "Long"] "long"
              location mainBranch "Owner" ["Nested"] "short" ]
            (CompilerLibrary.Available (errorValue 3 "multiple"))
        entry "value-alternate-loses" target
            [ location mainBranch "Owner" ["Nested"] "matching"
              location mainBranch "Owner" [] "selected" ]
            (CompilerLibrary.Available (errorValue 4 "alternate"))
        entry "value-missing-location" target []
            (CompilerLibrary.Available (errorValue 5 "missing"))
        entry "value-unavailable" target
            [location mainBranch "Owner" ["Nested"] "unavailable"]
            CompilerLibrary.Unavailable
        entry "value-failure" target
            [location mainBranch "Owner" ["Nested"] "failure"]
            CompilerLibrary.EvaluationFailure
        entry "value-other-branch" target
            [location otherBranch "Other" ["Nested"] "branchValue"]
            (CompilerLibrary.Available (errorValue 8 "branch"))
    ]

let private compile
    (stdlib: CompilerLibrary.StdlibResult)
    (catalog: CompilerLibrary.PackageValueCatalog)
    (source: string)
    : CompilerLibrary.CompileReport =
    CompilerLibrary.compile {
        Context = CompilerLibrary.StdlibOnly stdlib
        Mode = CompilerLibrary.FullProgram
        SourceSyntax = CompilerLibrary.CompilerSyntax
        Source = source
        SourceFile = "ValueSearchCatalogTests.dark"
        AllowInternal = false
        Verbosity = 0
        Options = CompilerLibrary.defaultOptions
        PackageValues = catalog
        PassTimingRecorder = None
    }

let testCatalogParity (stdlib: CompilerLibrary.StdlibResult) () : TestResult =
    let source =
        $"""
        let target = Darklang.LanguageTools.ProgramTypes.Hash.Hash("type-error") in
        let all = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error>("branch-main", "", target) in
        let nested = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error>("branch-main", "Owner.Nested", target) in
        let deep = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error>("branch-main", "Owner.Nested.Deep", target) in
        let other = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error>("branch-main", "", Darklang.LanguageTools.ProgramTypes.Hash.Hash("type-other")) in
        let branch = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error>("branch-other", "", target) in
        let allMatches =
            match all with
            | [first, second, multiple, alternate] ->
                first.path == "Owner.Nested.first" && first.value.errno == 1I && first.value.message == "first" &&
                second.path == "Owner.Nested.Deep.second" && second.value.errno == 2I && second.value.message == "second" &&
                multiple.path == "Owner.Nested.short" && multiple.value.errno == 3I && multiple.value.message == "multiple" &&
                alternate.path == "Owner.selected" && alternate.value.errno == 4I && alternate.value.message == "alternate"
            | _ -> false in
        let nestedMatches =
            match nested with
            | [first, second, multiple] ->
                first.path == "Owner.Nested.first" &&
                second.path == "Owner.Nested.Deep.second" &&
                multiple.path == "Owner.Nested.short"
            | _ -> false in
        let deepMatches =
            match deep with
            | [value] -> value.path == "Owner.Nested.Deep.second" && value.value.errno == 2I
            | _ -> false in
        let otherMatches =
            match other with
            | [value] -> value.path == "Owner.other" && value.value.errno == 20I
            | _ -> false in
        let branchMatches =
            match branch with
            | [value] -> value.path == "Other.Nested.branchValue" && value.value.errno == 8I
            | _ -> false in
        allMatches && nestedMatches && deepMatches && otherMatches && branchMatches
        """
    let report = compile stdlib parityCatalog source
    match report.Result with
    | Error error -> Error $"Catalog parity program did not compile: {error}"
    | Ok binary ->
        let output =
            CompilerLibrary.executeCaptured
                report.Target
                0
                CompilerLibrary.ExecutionInput.Closed
                binary
        if output.ExitCode = 0 && output.Stdout = "true\n" && output.Stderr = "" then
            Ok ()
        else
            Error $"Unexpected catalog parity output: exit={output.ExitCode}, stdout={output.Stdout}, stderr={output.Stderr}"

let testCatalogRejectsIllTypedAvailableValue
    (stdlib: CompilerLibrary.StdlibResult)
    ()
    : TestResult =
    let catalog =
        CompilerLibrary.PackageValueCatalog [
            entry "bad-value" (customType "type-error" [])
                [location mainBranch "Owner" [] "bad"]
                (CompilerLibrary.Available (AST.StringLiteral "not an Error"))
        ]
    let source =
        "Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error>(\"branch-main\", \"\", Darklang.LanguageTools.ProgramTypes.Hash.Hash(\"type-error\"))"
    let report = compile stdlib catalog source
    match report.Result with
    | Error error when error.Contains("Package value catalog validation failed") -> Ok ()
    | Error error -> Error $"Expected catalog validation failure, got: {error}"
    | Ok _ -> Error "Expected an ill-typed available package value to fail compilation"

let tests (stdlib: CompilerLibrary.StdlibResult) = [
    ("catalog-backed ValueSearch preserves interpreter lookup order and filtering", testCatalogParity stdlib)
    ("catalog evaluator is statically validated at its concrete specialization", testCatalogRejectsIllTypedAvailableValue stdlib)
]
