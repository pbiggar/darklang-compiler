(* Locate the checked-out repository for source and tooling policy tests. *)
open Dark_compiler

let rec rootFrom path =
  if
    Sys.file_exists (Filename.concat path "library-sources.list")
    && Sys.file_exists (Filename.concat path "test/fixtures")
  then Some path
  else
    let parent = Filename.dirname path in
    if parent = path then None else rootFrom parent

let repoRoot =
  match
    rootFrom (Filename.dirname (FileIO.absolutePath Sys.executable_name))
  with
  | Some path -> path
  | None -> (
      match rootFrom (Sys.getcwd ()) with
      | Some path -> path
      | None -> FileIO.absolutePath ".")

let scriptPath relative = Filename.concat repoRoot relative

let relativePath path =
  let prefix = repoRoot ^ Filename.dir_sep in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

let readFile path =
  try Ok (FileIO.readText path)
  with exn -> Error ("Failed to read " ^ path ^ ": " ^ Printexc.to_string exn)

let filesUnder relative extension =
  let rec loop directories files =
    match directories with
    | [] -> Array.of_list (List.rev files)
    | dir :: rest ->
        let entries =
          Sys.readdir dir |> Array.to_list |> List.map (Filename.concat dir)
        in
        let children, paths = List.partition Sys.is_directory entries in
        let matching =
          List.filter (fun path -> Filename.check_suffix path extension) paths
        in
        loop (rest @ children) (List.rev_append matching files)
  in
  loop [ scriptPath relative ] []
