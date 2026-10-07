(* StdlibSourceTests.ml - Source-level invariants for maintained stdlib files.
   These checks catch stdlib definitions that are easy to shadow accidentally
   before the compiler accepts a misleading or unreachable implementation. *)
open Dark_compiler
module R = RepositoryTestFiles
module M = StringOrder.Map

type testResult = (unit, string) result

let ( let* ) = Result.bind

let stdlibFiles () =
  try
    Ok
      (Array.append
         (R.filesUnder "StdLib" ".dark")
         (R.filesUnder "packages" ".dark"))
  with exn ->
    Error ("Failed to list embedded library files: " ^ Printexc.to_string exn)

let definitionName line =
  let units = Text.scalars line in
  let count = Array.length units in
  let white c =
    Uchar.is_valid c && Uucp.White.is_white_space (Uchar.of_int c)
  in
  let start = ref 0 in
  while !start < count && white units.(!start) do
    incr start
  done;
  if !start + 4 > count || Array.sub units !start 4 <> [| 100; 101; 102; 32 |]
  then None
  else
    let from = !start + 4 in
    let finish = ref from in
    while
      !finish < count
      && units.(!finish) <> 60
      && units.(!finish) <> 40
      && not (white units.(!finish))
    do
      incr finish
    done;
    if !finish > from && !finish < count then
      Some (Text.ofScalars (Array.sub units from (!finish - from)))
    else None

let duplicateDefinitionsInFile path =
  let* text = R.readFile path in
  let names =
    String.split_on_char '\n' text |> List.filter_map definitionName
  in
  let counts =
    List.fold_left
      (fun counts name ->
        M.add name (1 + Option.value ~default:0 (M.find_opt name counts)) counts)
      M.empty names
  in
  let _, reversed =
    List.fold_left
      (fun (seen, found) name ->
        if StringOrder.Set.mem name seen then (seen, found)
        else
          ( StringOrder.Set.add name seen,
            if M.find name counts > 1 then
              (R.relativePath path ^ ":" ^ name) :: found
            else found ))
      (StringOrder.Set.empty, [])
      names
  in
  Ok (List.rev reversed)

let findDuplicateDefinitions () =
  let* files = stdlibFiles () in
  Array.fold_left
    (fun result path ->
      let* accumulated = result in
      let* duplicates = duplicateDefinitionsInFile path in
      Ok (accumulated @ duplicates))
    (Ok []) files

let testStdlibHasNoDuplicateDefinitions () =
  match findDuplicateDefinitions () with
  | Error msg -> Error msg
  | Ok [] -> Ok ()
  | Ok duplicates ->
      Error ("Duplicate stdlib definitions: " ^ String.concat ", " duplicates)

let tests =
  [
    ("stdlib has no duplicate definitions", testStdlibHasNoDuplicateDefinitions);
  ]
