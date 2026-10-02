(*
   SyntaxTestRunner.fs - Runs canonical Dark syntax fixtures.
*)
(* SyntaxTestRunner.ml - Original syntax errors, formatting expectations, and roundtrip checks. *)
open Dark_compiler
open Common
let parse source = Result.map Validation.ValidatedSourceFile.toWrittenTypes (WrittenParsing.parse Validation.Script source)
let result success message expected actual = {TestOutcome.success; message; expected; actual}
let runSyntaxTest (test : SyntaxFormat.syntaxTest) =
  match parse test.SyntaxFormat.source, test.SyntaxFormat.expectedError with
  | Error error, Some expected when HostText.contains error expected -> result true "Test passed" None None
  | Error error, Some expected -> result false "Parse error did not contain expected text" (Some expected) (Some error)
  | Ok _, Some expected -> result false "Expected syntax parsing to fail" (Some expected) (Some "Parsing succeeded")
  | Error error, None -> result false ("Syntax parsing failed: " ^ error) None None
  | Ok ast, None ->
      let formatted = WrittenFormatter.format test.SyntaxFormat.source ast in
      (match test.SyntaxFormat.expectedFormat with
      | Some expected when normalizeLineEndings (HostText.trim formatted) <> normalizeLineEndings (HostText.trim expected) -> result false "Formatted syntax did not match" (Some expected) (Some formatted)
      | _ when not test.SyntaxFormat.roundtrip -> result true "Test passed" None None
      | _ -> match parse formatted with
          | Error error -> result false ("Roundtrip reparse failed: " ^ error) (Some formatted) None
          | Ok reparsed when WrittenFormatter.syntaxKey ast = WrittenFormatter.syntaxKey reparsed -> result true "Test passed" None None
          | Ok reparsed -> result false "AST changed after syntax roundtrip" (Some (WrittenFormatter.syntaxKey ast)) (Some (WrittenFormatter.syntaxKey reparsed)))
let loadSyntaxTests path =
  if not (TestFileIO.exists path) then Error ("Syntax test file not found: " ^ path)
  else try SyntaxFormat.parseSyntaxFileContent path (TestFileIO.readAllText path)
    with Sys_error message -> Error ("Failed to read syntax test file " ^ path ^ ": " ^ message)
let tests files =
  Array.to_list files |> List.sort StringOrder.compare |> List.concat_map (fun path ->
    match loadSyntaxTests path with
    | Error message -> ["parse " ^ Filename.basename path, (fun () -> Error message)]
    | Ok cases -> List.map (fun test -> test.SyntaxFormat.name, (fun () ->
        let outcome = runSyntaxTest test in
        if outcome.TestOutcome.success then Ok () else
          Error (match outcome.TestOutcome.expected, outcome.TestOutcome.actual with
            | Some expected, Some actual -> outcome.TestOutcome.message ^ "\nExpected:\n" ^ expected ^ "\nActual:\n" ^ actual
            | _ -> outcome.TestOutcome.message))) cases)
