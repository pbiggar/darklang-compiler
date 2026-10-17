(* Compile original package declarations through the existing native preamble pipeline.
   Tests are partitioned by the existing parser, never rewritten or executed here. *)
open Dark_compiler

let ( let* ) = Result.bind

let parseSource directory path =
  let text = In_channel.with_open_bin (Filename.concat directory path) In_channel.input_all in
  let* parsed =
    WrittenParsing.parse Validation.Package text
    |> Result.map_error (fun error -> path ^ ": " ^ error)
  in
  let* package = NamedPackageTests.partition parsed in
  Ok package.NamedPackageTests.declarations

let run directory manifest report =
  let* target = Platform.detectHostTarget () in
  let* stdlib = StdlibCompilation.buildStdlib target in
  let items = Yojson.Basic.from_file manifest |> Yojson.Basic.Util.to_list in
  let rows =
    List.map
      (fun item ->
        let source = Yojson.Basic.Util.(member "source" item |> to_string) in
        let dependencies = Yojson.Basic.Util.(member "dependencies" item |> to_list |> List.map to_string) in
        let result =
          let* sources = ResultList.traverse (parseSource directory) (dependencies @ [ source ]) in
          Result.map (fun _ -> ()) (PreambleCompilation.buildPackageContext stdlib sources)
        in
        let status, reason = match result with Ok () -> "compiled", "" | Error error -> "fail", error in
        `Assoc [ "source", `String source; "status", `String status;
                 "reason", `String reason; "dependencies", `List (List.map (fun path -> `String path) dependencies) ])
      items
  in
  Yojson.Basic.to_file report (`Assoc [ "schema", `Int 1; "stage", `String "native-declarations"; "packages", `List rows ]);
  Ok ()

let () =
  match Array.to_list Sys.argv with
  | [ _; directory; manifest; report ] -> (
      match run directory manifest report with
      | Ok () -> ()
      | Error error -> prerr_endline error; exit 1)
  | _ -> prerr_endline "Usage: compile_packages SOURCE_DIRECTORY MANIFEST REPORT"; exit 2
