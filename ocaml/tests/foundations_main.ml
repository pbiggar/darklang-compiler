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
  let armEncodingCorpus =
    Sys.readdir "src/Tests/passes/arm64enc" |> Array.to_list |> List.sort String.compare
    |> List.filter (fun path -> Filename.check_suffix path ".arm64enc")
    |> List.map (fun path -> path, (fun () ->
      let content=TestFileIO.readAllText (Filename.concat "src/Tests/passes/arm64enc" path) in
      match ARM64EncodingFormat.parseARM64EncodingTest content with
      | Error message -> Error message
      | Ok test ->
        let open Dark_compiler in
        match test.ARM64EncodingFormat.expectation with
        | ARM64EncodingFormat.EncodingErrorContaining expected ->
          let rec check=function [] -> Ok () | instr::rest ->
            (try ignore (ARM64_Encoding.encode instr);Error "Expected encoding error"
             with Failure message | Invalid_argument message -> if HostText.contains message expected then check rest else Error message) in
          check test.ARM64EncodingFormat.instructions
        | ARM64EncodingFormat.EncodesTo expected ->
          (try let rec encode=function [] -> Ok [] | instr::rest -> match ARM64_Encoding.encode instr with
            | [word] -> Result.map (fun words -> word::words) (encode rest)
            | words -> Error (Printf.sprintf "Expected one machine word, got %d" (List.length words)) in
            match encode test.ARM64EncodingFormat.instructions with
            | Error message -> Error message
            | Ok actual when actual<>expected -> Error "Machine word mismatch"
            | Ok actual when test.ARM64EncodingFormat.assertDifferent && List.length (List.sort_uniq Int32.compare actual)<>List.length actual -> Error "ASSERT-DIFFERENT failed"
            | Ok _ -> Ok ()
           with Failure message | Invalid_argument message -> Error message))) in
  let rcTests =
    let open Dark_compiler in
    let open ANF in
    let open RcJoinTests in [
      "Join cleanup releases locals at transfer and enclosing owners after continuation", testJoinCleanupPaths;
      "Join verifier accepts enclosing captures and nested outward transfers", (fun () -> verifyJoin (Let (TempId 2, Atom joinValue, Join (joinParameter, Return (Var (TempId 2)), Join ({id=TempId 3; typ=AST.TBool}, Jump (TempId 1, joinValue), Jump (TempId 3, BoolLiteral true))))));
      "Join verifier accepts immediate scalar block arguments", (fun () -> verifyJoin (Join ({id=TempId 3; typ=AST.TInt8}, Return joinValue, Jump (TempId 3, IntLiteral (Int8 1)))));
      "Join verifier rejects branch-local continuation capture", rejectsJoin "outside lexical scope" (Join (joinParameter, Return (Var (TempId 2)), Let (TempId 2, Atom joinValue, Jump (TempId 1, joinValue))));
      "Join verifier rejects parameter use in entry", rejectsJoin "outside lexical scope" (Join (joinParameter, Return (Var (TempId 1)), Jump (TempId 1, Var (TempId 1))));
      "Join verifier rejects recursive target", rejectsJoin "target" (Join (joinParameter, Jump (TempId 1, joinValue), Jump (TempId 1, joinValue)));
      "Join verifier rejects mismatched argument", rejectsJoin "expects" (Join (joinParameter, Return joinValue, Jump (TempId 1, BoolLiteral true)));
      "Join verifier rejects managed parameter", rejectsJoin "unsupported block argument" (Join ({joinParameter with typ=AST.TString}, Return joinValue, Jump (TempId 1, StringLiteral "value")));
      "Join verifier rejects returning entry", rejectsJoin "instead of transferring" (Join (joinParameter, Return joinValue, Return joinValue));
      "Join verifier rejects target shadowing", rejectsJoin "shadows" (Let (TempId 1, Atom joinValue, Join (joinParameter, Return joinValue, Jump (TempId 1, joinValue))));
      "inferCExprType Call returns function return type", RcTypeFactTests.testInferCallReturnsFunctionReturnType;
      "malformed __raw_get_ intrinsic does not infer Int64", RcTypeFactTests.testMalformedRawGetIntrinsicDoesNotInferInt64
    ] in
  let stdlibTests=match Result.bind (Dark_compiler.Platform.detectHostTarget ()) Dark_compiler.StdlibCompilation.buildStdlib with
    | Error message->["Compiler unit stdlib preparation",(fun ()->Error message)]
    | Ok stdlib->JsonPlanningTests.tests stdlib @ StdlibOptimizationTests.tests stdlib in
  let results =
    let syntax = Sys.readdir "src/Tests/syntax" |> Array.to_list
      |> List.filter (fun path -> Filename.check_suffix path ".syntax")
      |> List.map (Filename.concat "src/Tests/syntax") |> Array.of_list in
    List.map (fun (name, run) -> name, run ())
      (rcTests @ TypeCheckingTests.tests @ IRSymbolTests.tests @ DeadCodeEliminationTests.tests @ stdlibTests @ RuntimeDataLayoutTests.tests @ X86_64ResolveTests.tests @ LambdaLiftingTests.tests @ MonomorphizationTests.tests @ IRPrinterTests.tests @ GraphColorDSLTests.tests @ GraphColorTestRunner.tests [|"src/Tests/algorithms/graph-color/coloring.graphcolor"|] @ ProgressBarTests.tests @ ProgramCliTests.tests @ ARM64BinaryTests.tests @ X86_64BinaryTests.tests @ armEncodingCorpus @ ARM64EncodingTests.tests @ ANFToMIRTests.tests @ PhiResolutionTests.tests @ LIRPeepholeTests.tests @ LIRLayoutTests.tests @ MIROptimizeTests.tests @ SSAConstructionTests.tests @ SSAInliningTests.tests @ SSAOptimizationTests.tests @ MemoryShapeTests.tests @ ANFOptimizeTests.tests @ TailCallDetectionTests.tests @ HIRConstructionTests.tests @ OwnershipVariantSchedulingTests.tests @ OwnershipVariantMaterializationTests.tests @ OwnershipVariantSelectionTests.tests @ OwnedFunctionGroupInferenceTests.tests @ OwnedFunctionGroupTests.tests @ WholeFunctionOwnershipTests.tests @ RecursiveOwnershipInferenceTests.tests @ OwnershipUniquenessInferenceTests.tests @ OwnedHIRVerificationTests.tests @ HIRVerificationTests.tests @ BitsetTests.tests @ PlatformTests.tests @ TestRunnerArgsTests.tests @ ParserTests.tests
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
