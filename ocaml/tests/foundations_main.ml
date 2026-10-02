(* foundations_main.ml - Execute foundation tests before runner integration. *)
let () =
  if Array.to_list Sys.argv = [Sys.argv.(0); "--probe"] then
    Foundation_probe.run ()
  else begin
  let results =
    List.map (fun (name, run) -> name, run ())
      (BitsetTests.tests @ PlatformTests.tests @ TestRunnerArgsTests.tests)
  in
  let failures = List.filter (fun (_, result) -> Result.is_error result) results in
  List.iter
    (fun (name, result) ->
      match result with
      | Ok () -> ()
      | Error message -> Printf.eprintf "%s: %s\n" name message)
    failures;
  Printf.printf "%d/%d foundation tests passed\n"
    (List.length results - List.length failures) (List.length results);
  if failures <> [] then exit 1
  end
