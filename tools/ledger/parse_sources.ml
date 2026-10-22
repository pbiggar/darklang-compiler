(* parse_sources.ml - Check every pinned source without typing or executing bodies. *)
open Dark_compiler

let disposition = function
  | CompilerAnnotations.Run -> `Assoc [ ("status", `String "run") ]
  | CompilerAnnotations.Blocked reason ->
      `Assoc [ ("status", `String "blocked"); ("reason", `String reason) ]
  | CompilerAnnotations.Excluded reason ->
      `Assoc [ ("status", `String "excluded"); ("reason", `String reason) ]
  | CompilerAnnotations.Replace { target; reason } ->
      `Assoc
        [
          ("status", `String "replace");
          ("target", `String target);
          ("reason", `String reason);
        ]
  | CompilerAnnotations.Xfail { phase; diagnostic; reason } ->
      `Assoc
        [
          ("status", `String "xfail");
          ("reason", `String reason);
          ( "phase",
            `String
              (match phase with
              | CompilerAnnotations.Compile -> "compile"
              | CompilerAnnotations.Runtime -> "runtime") );
          ("diagnostic", `String diagnostic);
        ]

let entry (item : SourceInventory.entry) =
  `Assoc
    [
      ("name", `String item.SourceInventory.name);
      ("kind", `String (SourceInventory.kindName item.SourceInventory.kind));
      ("line", `Int (item.SourceInventory.position.Tokenizer.row + 1));
      ("column", `Int item.SourceInventory.position.Tokenizer.column);
      ("compiler", disposition item.SourceInventory.disposition);
    ]

let check reasons directory path =
  let source =
    In_channel.with_open_bin
      (Filename.concat directory path)
      In_channel.input_all
  in
  let items, diagnostics =
    match SourceInventory.parse ~reasons source with
    | Ok result -> (List.map entry result.SourceInventory.entries, [])
    | Error message -> ([], [ `String message ])
  in
  `Assoc
    [
      ("source", `String path);
      ("parsed", `Bool (diagnostics = []));
      ("diagnostics", `List diagnostics);
      ("items", `List items);
    ]

let () =
  match Array.to_list Sys.argv with
  | [ _; directory; manifest; glossary; report ] ->
      let paths =
        Yojson.Basic.Util.(
          Yojson.Basic.from_file manifest |> to_list |> List.map to_string)
      in
      let reasons =
        match CompilerAnnotations.readReasons glossary with
        | Ok reasons -> reasons
        | Error message ->
            prerr_endline message;
            exit 1
      in
      let rows = List.map (check reasons directory) paths in
      Yojson.Basic.to_file report
        (`Assoc [ ("schema", `Int 1); ("sources", `List rows) ])
  | _ ->
      prerr_endline
        "Usage: parse_sources SOURCE_DIRECTORY MANIFEST GLOSSARY REPORT";
      exit 1
