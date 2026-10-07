(*
   FormattingRoundtripTests.ml - Focused parser/pretty roundtrip regression tests.
   Loads minimal expressions from data files so new regression cases do not
   require recompiling tests.
*)
(* FormattingRoundtripTests.ml - Preserve reparse, syntax equivalence, and idempotence assertions. *)
open Dark_compiler
module F = FormattingRoundtripFormat
let roundtripSyntax (test : F.formattingRoundtripCase) =
  let parse source = Result.map Validation.ValidatedSourceFile.toWrittenTypes (WrittenParsing.parse Validation.Script source) in
  let context = "File: " ^ test.F.sourceFile ^ "\nTest: " ^ test.F.name ^ "\nSource: " ^ test.F.source in
  match parse test.F.source with
  | Error error -> Error ("Initial parse failed.\n" ^ context ^ "\nError: " ^ error)
  | Ok original ->
      let first = WrittenFormatter.format test.F.source original in
      match parse first with
      | Error error -> Error ("Re-parse failed.\n" ^ context ^ "\nPretty: " ^ first ^ "\nError: " ^ error)
      | Ok reparsed ->
          let second = WrittenFormatter.format first reparsed in
          if WrittenFormatter.syntaxKey original <> WrittenFormatter.syntaxKey reparsed then
            Error ("AST changed after roundtrip.\n" ^ context ^ "\nPretty (pass 1): " ^ first ^ "\nPretty (pass 2): " ^ second)
          else if first <> second then
            Error ("Pretty-printer output was not idempotent.\n" ^ context ^ "\nPretty (pass 1): " ^ first ^ "\nPretty (pass 2): " ^ second)
          else Ok ()
let tests files =
  Array.to_list files |> List.sort StringOrder.compare |> List.concat_map (fun path ->
    match F.parseFormattingRoundtripFile path with
    | Error message -> ["parse " ^ Filename.basename path, (fun () -> Error message)]
    | Ok cases -> List.map (fun test -> test.F.name, (fun () -> roundtripSyntax test)) cases)
