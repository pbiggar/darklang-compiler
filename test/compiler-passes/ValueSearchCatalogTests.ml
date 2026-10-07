(*
   ValueSearchCatalogTests.ml - native integration tests for the AOT package-value catalog boundary.

   Catalog data is compilation input rather than Dark source, so these tests
   compile and execute complete programs through CompilerLibrary instead of the
   line-oriented E2E DSL.
   The upstream Int8 probe reads Darklang.Test.Values.int8Value. The
   interpreter resolves that package global to 5y; AOT receives the same value
   as an immutable catalog evaluator, never through live package lookup.
*)
[@@@warning "-4-42"]
open Dark_compiler
module C=CompilationContexts
module O=CompilerOptions
type testResult=(unit,string) result
let errorType=AST.TRecord ("Darklang.Stdlib.Cli.Posix.Error",[])
let customType hash typeArguments:C.packageCustomType={C.hash;typeArguments}
let location visibleInBranches owner modules name:C.catalogPackageLocation={C.visibleInBranches;owner;modules;name}
let errorValue errno message=AST.RecordLiteral (AST.unresolvedRecordReference "Darklang.Stdlib.Cli.Posix.Error" [],[AST.unresolvedRecordFieldReference "errno",AST.BigIntLiteral (Z.of_int errno);AST.unresolvedRecordFieldReference "message",AST.StringLiteral message])
let evaluator state:C.typedPackageValueEvaluator={C.resultType=errorType;state}
let entry valueHash runtimeType locations state:C.packageValueCatalogEntry={C.valueHash;runtimeType;locations;evaluator=evaluator state}
let int8ProbeCatalog=C.PackageValueCatalog [{C.valueHash="darklang-test-values-int8Value";runtimeType=customType "type-int8" [];locations=[location ["test"] "Darklang" ["Test";"Values"] "int8Value"];evaluator={C.resultType=AST.TInt8;state=C.Available (AST.Int8Literal 5)}}]
let mainBranch=["branch-main"]
let otherBranch=["branch-other"]
let parityCatalog=
 let target=customType "type-error" [] and other=customType "type-other" [] and parameterized=customType "type-error" [customType "argument-type" []] in C.PackageValueCatalog [
 entry "value-first" target [location mainBranch "Owner" ["Nested"] "first"] (C.Available (errorValue 1 "first"));
 entry "value-other-type" other [location mainBranch "Owner" [] "other"] (C.Available (errorValue 20 "other"));
 entry "value-parameterized" parameterized [location mainBranch "Owner" [] "parameterized"] (C.Available (errorValue 30 "parameterized"));
 entry "value-second" target [location mainBranch "Owner" ["Nested";"Deep"] "second"] (C.Available (errorValue 2 "second"));
 entry "value-multiple" target [location mainBranch "Owner" ["Nested";"Long"] "long";location mainBranch "Owner" ["Nested"] "short"] (C.Available (errorValue 3 "multiple"));
 entry "value-alternate-loses" target [location mainBranch "Owner" ["Nested"] "matching";location mainBranch "Owner" [] "selected"] (C.Available (errorValue 4 "alternate"));
 entry "value-missing-location" target [] (C.Available (errorValue 5 "missing"));
 entry "value-unavailable" target [location mainBranch "Owner" ["Nested"] "unavailable"] C.Unavailable;
 entry "value-failure" target [location mainBranch "Owner" ["Nested"] "failure"] C.EvaluationFailure;
 entry "value-other-branch" target [location otherBranch "Other" ["Nested"] "branchValue"] (C.Available (errorValue 8 "branch"))]
let compile stdlib catalog source=CompilerLibrary.compile {C.context=C.StdlibOnly stdlib;mode=O.FullProgram;sources=AST.NonEmptyList.singleton {C.name="ValueSearchCatalogTests.dark";purpose=NameSyntax.SourceUnitPurpose.Executable;source};allowInternal=false;verbosity=0;options=O.defaultOptions;packageValues=catalog;packageManager=None;passTimingRecorder=None;session=None}
let execute (report:O.compileReport) binary=E2ETestRunner.executeBinaryForTarget report.O.target binary
let expectExecution label report=match report.O.result with
 |Error error->Error (label^" did not compile: "^error)
 |Ok binary->match execute report binary with Error error->Error (label^" did not execute: "^error)|Ok output->
  if output.O.exitCode=0 && output.O.stdout="true\n" && output.O.stderr="" then Ok () else Error (Printf.sprintf "Unexpected %s output: exit=%d, stdout=%s, stderr=%s" (if label="Catalog parity program" then "catalog parity" else label) output.O.exitCode output.O.stdout output.O.stderr)
let paritySource={|
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
        |}
let int8Source={|
        match Builtin.pmEvaluateValue<Int8> (Darklang.LanguageTools.ProgramTypes.Hash.Hash "darklang-test-values-int8Value") with
        | Some value -> Builtin.printLine (Stdlib.Bool.toString (Stdlib.Int8.add value 5y == 10y))
        | None -> Builtin.printLine "false"
        |}
let testCatalogParity stdlib ()=compile stdlib parityCatalog paritySource |> expectExecution "Catalog parity program"
let testCatalogRejectsIllTypedAvailableValue stdlib ()=
 let catalog=C.PackageValueCatalog [entry "bad-value" (customType "type-error" []) [location mainBranch "Owner" [] "bad"] (C.Available (AST.StringLiteral "not an Error"))] in
 let source="let ignored = Darklang.Stdlib.ValueSearch.findByType<Stdlib.Cli.Posix.Error> \"branch-main\" \"\" (Darklang.LanguageTools.ProgramTypes.Hash.Hash \"type-error\") in ()" in
 let report=compile stdlib catalog source in match report.O.result with
 |Error error when Text.contains error "Package value catalog validation failed"->Ok ()|Error error->Error ("Expected catalog validation failure, got: "^error)|Ok _->Error "Expected an ill-typed available package value to fail compilation"
let testInt8PackageProbeParity stdlib ()=compile stdlib int8ProbeCatalog int8Source |> expectExecution "Int8 package probe"
let tests stdlib=[
 "catalog-backed ValueSearch preserves interpreter lookup order and filtering",testCatalogParity stdlib;
 "catalog evaluator is statically validated at its concrete specialization",testCatalogRejectsIllTypedAvailableValue stdlib;
 "Int8 package-value probe supplies the interpreter value through the AOT catalog",testInt8PackageProbeParity stdlib]
