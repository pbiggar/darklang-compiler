(* DSL_probe.ml - Observe section and syntax fixture parsing against the frozen runner. *)
open Dark_compiler
let union typ case fields = `Assoc ["type", `String typ; "case", `String case; "fields", `List fields]
let text value =
  let units = HostText.utf16Units value in
  let rec unpaired index = if index >= Array.length units then false
    else if units.(index) >= 0xd800 && units.(index) <= 0xdbff then
      if index + 1 < Array.length units && units.(index + 1) >= 0xdc00 && units.(index + 1) <= 0xdfff then unpaired (index + 2) else true
    else if units.(index) >= 0xdc00 && units.(index) <= 0xdfff then true else unpaired (index + 1) in
  if unpaired 0 then `Assoc ["utf16String", `List (Array.to_list (Array.map (fun unit -> `String (Printf.sprintf "%04x" unit)) units))] else `String value
let option encode = function None -> union "FSharpOption" "None" [] | Some value -> union "FSharpOption" "Some" [encode value]
let result encode = function Ok value -> union "FSharpResult" "Ok" [encode value] | Error error -> union "FSharpResult" "Error" [text error]
let tuple values = `Assoc ["tuple", `List values]
let record name fields = `Assoc ["record", `String name; "fields", `List (List.map (fun (name, value) -> `List [`String name; value]) fields)]
let observe source =
  let sections values = `List (List.map (fun (name, value) -> tuple [text name; text value]) values) in
  let file = Common.parseTestFile source in
  let syntax (test : SyntaxFormat.syntaxTest) = record "SyntaxTest" ["Name", text test.SyntaxFormat.name;
    "Source", text test.SyntaxFormat.source; "ExpectedError", option text test.SyntaxFormat.expectedError;
    "ExpectedFormat", option text test.SyntaxFormat.expectedFormat; "Roundtrip", `Bool test.SyntaxFormat.roundtrip; "SourceFile", text test.SyntaxFormat.sourceFile] in
  `Assoc ["sections", sections (Common.parseSections source); "file", sections (StringOrder.Map.bindings file.Common.sections);
    "required", `List (List.map (fun name -> result text (Common.getRequiredSection name file)) ["NAME"; "SOURCE"; "EXPECTED"; "BODY"]);
    "optional", `List (List.map (fun name -> option text (Common.getOptionalSection name file)) ["NAME"; "SOURCE"; "EXPECTED"; "BODY"]);
    "stripped", `List (List.map text (Common.stripCommentsAndEmpty source)); "normalized", text (Common.normalizeLineEndings source);
    "escaped", result text (Common.parseEscapedText source);
    "syntax", result (fun values -> `List (List.map syntax values)) (SyntaxFormat.parseSyntaxFileContent "probe" source)]
let run () =
  let rec loop () = match input_line stdin with
    | line ->
        let request = Yojson.Basic.from_string line in
        let source = Yojson.Basic.Util.(request |> member "source" |> to_string) in
        print_endline (Yojson.Basic.to_string (`Assoc ["schema", `Int 1; "stage", `String "dsl"; "value", observe source])); loop ()
    | exception End_of_file -> () in
  loop ()
