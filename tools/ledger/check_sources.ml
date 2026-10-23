(* check_sources.ml - Validate selected imported bodies and replacement contracts from disk. *)
open Dark_compiler
module X = CompilationContexts

let ( let* ) = Result.bind
let read path = In_channel.with_open_bin path In_channel.input_all

let lines path =
  String.split_on_char '\n' (read path)
  |> List.filter (fun line ->
      line <> "" && not (String.starts_with ~prefix:"#" line))

let check native root =
  let* reasons =
    CompilerAnnotations.readReasons
      (Filename.concat root "porting/compiler-reasons.json")
  in
  let roots =
    lines (Filename.concat root "upstream/sources.list")
    |> List.filter (String.starts_with ~prefix:"packages/")
    |> List.map (( ^ ) "upstream/")
  in
  let supplements = lines (Filename.concat root "library-sources.list") in
  let* sources =
    ResultList.traverse
      (fun file ->
        let* source =
          SourceInventory.parse ~reasons (read (Filename.concat root file))
        in
        Ok (file, source))
      (roots @ supplements)
  in
  let validate roots label =
    let* selected =
      SourceSelection.select ~context:SourceContracts.emptyContext ~roots
        ~supplements ~sources
    in
    let units = List.map snd selected.SourceSelection.units in
    if native then (
      let* target = Platform.detectHostTarget () in
      Printf.eprintf "Native validation: %s\n%!" label;
      let recorder (timing : CompilerOptions.passTiming) =
        Printf.eprintf "%s: %.0fms\n%!" timing.CompilerOptions.pass
          (Int64.to_float timing.CompilerOptions.elapsed /. 1e6)
      in
      let* stdlib =
        StdlibCompilation.buildStdlibFromSources ~recorder target units
      in
      let request : X.compileRequest =
        {
          X.context = X.StdlibOnly stdlib;
          mode = CompilerOptions.TestExpression;
          sources =
            NonEmptyList.singleton
              {
                X.name = "native-source-reuse";
                purpose = NameSyntax.SourceUnitPurpose.Executable;
                source = "0L";
              };
          allowInternal = true;
          verbosity = 0;
          options = CompilerOptions.defaultOptions;
          packageValues = X.emptyPackageValueCatalog;
          packageManager = None;
          passTimingRecorder = None;
          session = None;
        }
      in
      let report = CompilerLibrary.compile request in
      Result.map (fun _ -> ()) report.CompilerOptions.result)
    else
      Result.map
        (fun _ -> ())
        (WrittenChecking.checkSourceUnitsWithBase None true false units)
  in
  let* () =
    if native then
      validate
        (List.filter (fun path -> not (Text.contains path "/tests/")) roots)
        "application library"
    else Ok ()
  in
  validate roots "test-helper library"

let () =
  match Array.to_list Sys.argv with
  | ([ _; root ] | [ _; "--native"; root ]) as arguments -> (
      match check (List.length arguments = 3) root with
      | Ok () ->
          print_endline
            "Selected source bodies and replacement contracts checked"
      | Error message ->
          prerr_endline message;
          exit 1)
  | _ ->
      prerr_endline "Usage: check_sources [--native] ROOT";
      exit 1
