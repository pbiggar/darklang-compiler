(* TestFileIO.ml - Read UTF-8 fixtures and retain .NET line splitting and BOM behavior. *)
let exists path =
  try Sys.file_exists path && not (Sys.is_directory path)
  with Sys_error _ | Invalid_argument _ -> false

let readAllText path =
  let text = In_channel.with_open_bin path In_channel.input_all in
  if String.starts_with ~prefix:"\239\187\191" text then
    String.sub text 3 (String.length text - 3)
  else text

let readAllLines path =
  let text = Common.normalizeLineEndings (readAllText path) in
  if text = "" then [||]
  else
    let lines = String.split_on_char '\n' text in
    let lines =
      if String.ends_with ~suffix:"\n" text then
        List.rev (List.tl (List.rev lines))
      else lines
    in
    Array.of_list lines
