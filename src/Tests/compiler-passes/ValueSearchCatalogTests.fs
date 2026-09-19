// ValueSearchCatalogTests.fs - native integration tests for the AOT package-value catalog boundary.
//
// Catalog data is compilation input rather than Dark source, so these tests
// compile and execute complete programs through CompilerLibrary instead of the
// line-oriented E2E DSL.

module ValueSearchCatalogTests

open System.Numerics

type TestResult = Result<unit, string>

let private errorType = AST.TRecord ("Darklang.Stdlib.Cli.Posix.Error", [])

let private customType hash typeArguments : CompilationContexts.PackageCustomType = {
    Hash = hash
    TypeArguments = typeArguments
}

let private location branches owner modules name : CompilationContexts.CatalogPackageLocation = {
    VisibleInBranches = branches
    Owner = owner
    Modules = modules
    Name = name
}

let private errorValue (errno: int) (message: string) =
    AST.RecordLiteral (
        AST.unresolvedRecordReference "Darklang.Stdlib.Cli.Posix.Error" [],
        [
            (AST.unresolvedRecordFieldReference "errno", AST.BigIntLiteral (BigInteger errno))
            (AST.unresolvedRecordFieldReference "message", AST.StringLiteral message)
        ]
    )

let private evaluator state : CompilationContexts.TypedPackageValueEvaluator = {
    ResultType = errorType
    State = state
}

let private entry hash runtimeType locations state : CompilationContexts.PackageValueCatalogEntry = {
    ValueHash = hash
    RuntimeType = runtimeType
    Locations = locations
    Evaluator = evaluator state
}

/// The upstream Int8 probe reads Darklang.Test.Values.int8Value. The
/// interpreter resolves that package global to 5y; AOT receives the same value
/// as an immutable catalog evaluator, never through live package lookup.
let private int8ProbeCatalog =
    CompilationContexts.PackageValueCatalog [
        { ValueHash = "darklang-test-values-int8Value"
          RuntimeType = customType "type-int8" []
          Locations = [location ["test"] "Darklang" ["Test"; "Values"] "int8Value"]
          Evaluator =
            { ResultType = AST.TInt8
              State = CompilationContexts.Available (AST.Int8Literal 5y) } }
    ]

let private mainBranch = ["branch-main"]
let private otherBranch = ["branch-other"]

let private parityCatalog =
    let target = customType "type-error" []
    let other = customType "type-other" []
    let parameterized = customType "type-error" [customType "argument-type" []]
    CompilationContexts.PackageValueCatalog [
        entry "value-first" target
            [location mainBranch "Owner" ["Nested"] "first"]
            (CompilationContexts.Available (errorValue 1 "first"))
        entry "value-other-type" other
            [location mainBranch "Owner" [] "other"]
            (CompilationContexts.Available (errorValue 20 "other"))
        entry "value-parameterized" parameterized
            [location mainBranch "Owner" [] "parameterized"]
            (CompilationContexts.Available (errorValue 30 "parameterized"))
        entry "value-second" target
            [location mainBranch "Owner" ["Nested"; "Deep"] "second"]
            (CompilationContexts.Available (errorValue 2 "second"))
        entry "value-multiple" target
            [ location mainBranch "Owner" ["Nested"; "Long"] "long"
              location mainBranch "Owner" ["Nested"] "short" ]
            (CompilationContexts.Available (errorValue 3 "multiple"))
        entry "value-alternate-loses" target
            [ location mainBranch "Owner" ["Nested"] "matching"
              location mainBranch "Owner" [] "selected" ]
            (CompilationContexts.Available (errorValue 4 "alternate"))
        entry "value-missing-location" target []
            (CompilationContexts.Available (errorValue 5 "missing"))
        entry "value-unavailable" target
            [location mainBranch "Owner" ["Nested"] "unavailable"]
            CompilationContexts.Unavailable
        entry "value-failure" target
            [location mainBranch "Owner" ["Nested"] "failure"]
            CompilationContexts.EvaluationFailure
        entry "value-other-branch" target
            [location otherBranch "Other" ["Nested"] "branchValue"]
            (CompilationContexts.Available (errorValue 8 "branch"))
    ]

let private compile
    (stdlib: CompilationContexts.StdlibResult)
    (catalog: CompilationContexts.PackageValueCatalog)
    (source: string)
    : CompilerOptions.CompileReport =
    CompilerLibrary.compile {
        Context = CompilationContexts.StdlibOnly stdlib
        Mode = CompilerOptions.FullProgram
        Sources =
            AST.NonEmptyList.singleton
                { CompilationContexts.SourceUnit.Name = "ValueSearchCatalogTests.dark"
                  Purpose = NameSyntax.SourceUnitPurpose.Executable
                  Source = source }
        AllowInternal = false
        Verbosity = 0
        Options = CompilerOptions.defaultOptions
        PackageValues = catalog
        PassTimingRecorder = None
        Session = None
    }

let private execute (report: CompilerOptions.CompileReport) (binary: byte array) =
    TestDSL.E2ETestRunner.executeBinaryForTarget report.Target binary

let testCatalogParity (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    let source =
        $"""
        let target = Darklang.LanguageTools.ProgramTypes.Hash.Hash ("type-error") in
        let all = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> ("branch-main") ("") (target) in
        let nested = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> ("branch-main") ("Owner.Nested") (target) in
        let deep = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> ("branch-main") ("Owner.Nested.Deep") (target) in
        let other = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> ("branch-main") ("") (Darklang.LanguageTools.ProgramTypes.Hash.Hash ("type-other")) in
        let branch = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> ("branch-other") ("") (target) in
        let allMatches =
            match all with
            | [first, second, multiple, alternate] ->
                first.path == "Owner.Nested.first" && first.value.errno == 1 && first.value.message == "first" &&
                second.path == "Owner.Nested.Deep.second" && second.value.errno == 2 && second.value.message == "second" &&
                multiple.path == "Owner.Nested.short" && multiple.value.errno == 3 && multiple.value.message == "multiple" &&
                alternate.path == "Owner.selected" && alternate.value.errno == 4 && alternate.value.message == "alternate"
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
            | [value] -> value.path == "Owner.Nested.Deep.second" && value.value.errno == 2
            | _ -> false in
        let otherMatches =
            match other with
            | [value] -> value.path == "Owner.other" && value.value.errno == 20
            | _ -> false in
        let branchMatches =
            match branch with
            | [value] -> value.path == "Other.Nested.branchValue" && value.value.errno == 8
            | _ -> false in
        Builtin.printLine (Stdlib.Bool.toString (allMatches && nestedMatches && deepMatches && otherMatches && branchMatches))
        """
    let report = compile stdlib parityCatalog source
    match report.Result with
    | Error error -> Error $"Catalog parity program did not compile: {error}"
    | Ok binary ->
        match execute report binary with
        | Error error -> Error $"Catalog parity program did not execute: {error}"
        | Ok output ->
            if output.ExitCode = 0 && output.Stdout = "true\n" && output.Stderr = "" then
                Ok ()
            else
                Error $"Unexpected catalog parity output: exit={output.ExitCode}, stdout={output.Stdout}, stderr={output.Stderr}"

let testCatalogRejectsIllTypedAvailableValue
    (stdlib: CompilationContexts.StdlibResult)
    ()
    : TestResult =
    let catalog =
        CompilationContexts.PackageValueCatalog [
            entry "bad-value" (customType "type-error" [])
                [location mainBranch "Owner" [] "bad"]
                (CompilationContexts.Available (AST.StringLiteral "not an Error"))
        ]
    let source =
        "let ignored = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> \"branch-main\" \"\" (Darklang.LanguageTools.ProgramTypes.Hash.Hash \"type-error\") in ()"
    let report = compile stdlib catalog source
    match report.Result with
    | Error error when error.Contains("Package value catalog validation failed") -> Ok ()
    | Error error -> Error $"Expected catalog validation failure, got: {error}"
    | Ok _ -> Error "Expected an ill-typed available package value to fail compilation"

let testInt8PackageProbeParity (stdlib: CompilationContexts.StdlibResult) () : TestResult =
    let source =
        """
        match Builtin.pmEvaluateValue<Int8> (Darklang.LanguageTools.ProgramTypes.Hash.Hash "darklang-test-values-int8Value") with
        | Some value -> Builtin.printLine (Stdlib.Bool.toString (Stdlib.Int8.add value 5y == 10y))
        | None -> Builtin.printLine "false"
        """
    let report = compile stdlib int8ProbeCatalog source
    match report.Result with
    | Error error -> Error $"Int8 package probe did not compile: {error}"
    | Ok binary ->
        match execute report binary with
        | Error error -> Error $"Int8 package probe did not execute: {error}"
        | Ok output ->
            if output.ExitCode = 0 && output.Stdout = "true\n" && output.Stderr = "" then Ok ()
            else Error $"Unexpected Int8 package probe output: exit={output.ExitCode}, stdout={output.Stdout}, stderr={output.Stderr}"

let tests (stdlib: CompilationContexts.StdlibResult) = [
    ("catalog-backed ValueSearch preserves interpreter lookup order and filtering", testCatalogParity stdlib)
    ("catalog evaluator is statically validated at its concrete specialization", testCatalogRejectsIllTypedAvailableValue stdlib)
    ("Int8 package-value probe supplies the interpreter value through the AOT catalog", testInt8PackageProbeParity stdlib)
]
