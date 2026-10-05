[@@@warning "-42"]
(* foundations_main.ml - Execute foundation tests before runner integration. *)
let () =
  if Array.to_list Sys.argv = [Sys.argv.(0); "--arm64-dsl-probe"] then ARMDSL_probe.run ()
  else if Array.to_list Sys.argv = [Sys.argv.(0); "--dsl-probe"] then DSL_probe.run ()
  else if Array.to_list Sys.argv = [Sys.argv.(0); "--probe"] then
    Foundation_probe.run ()
  else begin
  let typing =
    Sys.readdir "src/Tests/typecheck" |> Array.to_list |> List.sort String.compare
    |> List.filter (fun path -> Filename.check_suffix path ".typecheck")
    |> List.concat_map (fun path ->
      match TypeCheckingFormat.parseTypeCheckingTestFile (Filename.concat "src/Tests/typecheck" path) with
      | Error message -> [path, (fun () -> Error message)]
      | Ok tests -> List.map (fun (test : TypeCheckingFormat.typeCheckingTest) ->
          path ^ ": " ^ test.TypeCheckingFormat.name, (fun () ->
            let result = TypeCheckingTestRunner.runTypeCheckingTest test in
            if result.TypeCheckingTestRunner.success then Ok () else Error result.TypeCheckingTestRunner.message)) tests) in
  let armEncodingCorpus=Sys.readdir "src/Tests/passes/arm64enc" |> Array.to_list |> List.sort Dark_compiler.StringOrder.compare |> List.filter (fun path->Filename.check_suffix path ".arm64enc") |> List.map (fun path->path,(fun ()->match ARM64EncodingTestRunner.loadARM64EncodingTest (Filename.concat "src/Tests/passes/arm64enc" path) with Error error->Error error|Ok test->let result=ARM64EncodingTestRunner.runARM64EncodingTest test in if result.TestOutcome.success then Ok () else Error result.TestOutcome.message)) in
  let processChecks=[
    "Runner capture drains both large streams",(fun ()->match TestProcess.capture "/bin/sh" ["-c";"printf '%100000s' a; printf '%100000s' b >&2; exit 17"] 10000 with Ok (17,stdout,stderr) when stdout=String.make 99999 ' '^"a" && stderr=String.make 99999 ' '^"b"->Ok ()|Ok _->Error "Captured process output or exit code differs"|Error error->Error error);
    "Runner capture preserves UTF-8 and CRLF",(fun ()->match TestProcess.capture "/bin/sh" ["-c";"printf 'é😀\\r\\n'; printf 'err\\r\\n' >&2"] 10000 with Ok (0,"é😀\r\n","err\r\n")->Ok ()|Ok _->Error "Captured text differs"|Error error->Error error);
    "Runner capture times out descendants",(fun ()->match TestProcess.capture "/bin/sh" ["-c";"sleep 30 & wait"] 100 with Error "Execution timed out after 100ms"->Ok ()|Error error->Error error|Ok _->Error "Expected a process timeout")
  ] in
  let passCorpus=Yojson.Basic.from_file "scripts/ocaml/pass_runner_fixtures.json" |> Yojson.Basic.Util.to_list |> List.map (fun path->let path=Yojson.Basic.Util.to_string path in path,(fun ()->let open PassTestRunner in let result=if Filename.check_suffix path ".anf2mir" then Result.map (fun (input,expected)->runANF2MIRTest input expected) (loadANF2MIRTest path) else if Filename.check_suffix path ".mir2lir" then Result.map (fun (input,expected)->runMIR2LIRTest input expected) (loadMIR2LIRTest path) else Result.map (fun (input,expected)->runLIR2ARM64Test input expected) (loadLIR2ARM64Test path) in Result.bind result (fun outcome->if outcome.TestOutcome.success then Ok () else Error outcome.TestOutcome.message))) in
  let stdlibTests=match Result.bind (Dark_compiler.Platform.detectHostTarget ()) Dark_compiler.StdlibCompilation.buildStdlib with
    | Error message->["Compiler unit stdlib preparation",(fun ()->Error message)]
    | Ok stdlib->ProgramStructureTests.tests stdlib @ ValueSearchCatalogTests.tests stdlib @ JsonPlanningTests.tests stdlib @ StdlibOptimizationTests.tests stdlib @ CompilationSessionTests.tests Dark_compiler.Platform.LinuxX86_64 stdlib in
  let results =
    let syntax = Sys.readdir "src/Tests/syntax" |> Array.to_list
      |> List.filter (fun path -> Filename.check_suffix path ".syntax")
      |> List.map (Filename.concat "src/Tests/syntax") |> Array.of_list in
    List.map (fun (name, run) -> name, run ())
      (X86_64CodeGenTests.tests @ ["x64 nested mixed boxed-sum release dispatch",X86_64CodeGenTests.testGenericRefCountDecNestedMixedSumPayloadUsesVariantDispatch] @ ARM64CodeGenTests.tests @ RefCountInsertionTests.tests @ E2EFormatTests.tests @ OwnershipCallFactsTests.tests @ RegionContractTests.tests @ StdlibSourceTests.tests @ ScriptHelperTests.tests @ ASTToANFTests.tests @ ListHIRTests.tests @ SyntaxDSLTests.tests @ RCReleaseDSLTests.tests Dark_compiler.Platform.LinuxX86_64 @ RCReleaseTestRunner.tests Dark_compiler.Platform.LinuxX86_64 [|"src/Tests/backend/reference-release/reference-count.rcrelease"|] @ OptimizationFormatTests.tests @ EncodingDSLTests.tests @ X86_64EncodingTestRunner.tests [|"src/Tests/passes/x64enc/encoding.x64enc"|] @ processChecks @ LIRExecutionDSLTests.tests @ LIRExecutionTestRunner.tests [|"src/Tests/backend/x64/basic.lirexec"|] @ ParallelMoveDSLTests.tests @ ParallelMoveTestRunner.tests [|"src/Tests/algorithms/parallel-moves/arm64.parallelmoves"|] @ passCorpus @ PassTestRunnerTests.tests @ IRFormatSnapshotDSLTests.tests @ IRFormatSnapshotTestRunner.tests [|"src/Tests/formatting/ir/core.irformat"|] @ ChordalGraphTests.tests @ SSALivenessTests.tests @ TypeCheckingTests.tests @ IRSymbolTests.tests @ DeadCodeEliminationTests.tests @ stdlibTests @ RuntimeDataLayoutTests.tests @ X86_64ResolveTests.tests @ LambdaLiftingTests.tests @ MonomorphizationTests.tests @ IRPrinterTests.tests @ GraphColorDSLTests.tests @ GraphColorTestRunner.tests [|"src/Tests/algorithms/graph-color/coloring.graphcolor"|] @ ProgressBarTests.tests @ ProgramCliTests.tests @ ARM64BinaryTests.tests @ X86_64BinaryTests.tests @ armEncodingCorpus @ ARM64EncodingTests.tests @ ANFToMIRTests.tests @ PhiResolutionTests.tests @ LIRPeepholeTests.tests @ LIRPeepholeTests.dslTests @ LIRLayoutTests.tests @ MIROptimizeTests.tests @ SSAConstructionTests.tests @ SSAInliningTests.tests @ SSAOptimizationTests.tests @ ANFOptimizeTests.tests @ TailCallDetectionTests.tests @ HIRConstructionTests.tests @ OwnershipVariantSchedulingTests.tests @ OwnershipVariantMaterializationTests.tests @ OwnershipVariantSelectionTests.tests @ OwnedFunctionGroupInferenceTests.tests @ OwnedFunctionGroupTests.tests @ WholeFunctionOwnershipTests.tests @ RecursiveOwnershipInferenceTests.tests @ OwnershipUniquenessInferenceTests.tests @ OwnedHIRVerificationTests.tests @ HIRVerificationTests.tests @ BitsetTests.tests @ PlatformTests.tests @ TestRunnerArgsTests.tests @ ParserTests.tests
       @ typing @ TypeCheckingFormatTests.tests @ TypeCheckingTestRunnerTests.tests
       @ SyntaxTestRunner.tests syntax @ FormattingRoundtripTests.tests [|"src/Tests/formatting-roundtrip/compiler.roundtrip"|])
  in
  let failures = List.filter (fun (_, result) -> Result.is_error result) results in
  List.iter
    (fun (name, result) ->
      match result with
      | Ok () -> ()
      | Error message -> Printf.eprintf "%s: %s\n" name message)
    failures;
  Printf.printf "%d/%d translated unit tests passed\n"
    (List.length results - List.length failures) (List.length results);
  if failures <> [] then exit 1
  end
