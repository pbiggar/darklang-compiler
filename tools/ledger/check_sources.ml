(* check_sources.ml - Validate selected imported bodies and replacement contracts from disk. *)
open Dark_compiler

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
  let* selected =
    SourceSelection.select ~context:SourceContracts.emptyContext ~roots
      ~supplements ~sources
  in
  let units = List.map snd selected.SourceSelection.units in
  if native then
    let* target = Platform.detectHostTarget () in
    Result.map
      (fun _ -> ())
      (StdlibCompilation.buildStdlibFromSources target units)
  else
    Result.map
      (fun _ -> ())
      (WrittenChecking.checkSourceUnitsWithBase None true false units)

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
