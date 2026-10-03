(* foundations_main.ml - Execute foundation tests before runner integration. *)
let () =
  if Array.to_list Sys.argv = [Sys.argv.(0); "--dsl-probe"] then DSL_probe.run ()
  else if Array.to_list Sys.argv = [Sys.argv.(0); "--probe"] then
    Foundation_probe.run ()
  else begin
  let results =
    let syntax = Sys.readdir "src/Tests/syntax" |> Array.to_list
      |> List.filter (fun path -> Filename.check_suffix path ".syntax")
      |> List.map (Filename.concat "src/Tests/syntax") |> Array.of_list in
    List.map (fun (name, run) -> name, run ())
      (BitsetTests.tests @ PlatformTests.tests @ TestRunnerArgsTests.tests @ ParserTests.tests @ NameResolutionTests.tests
       @ TypeCheckingFormatTests.tests @ TypeCheckingTestRunnerTests.tests
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
