(* FileIO.ml - Source-file paths and BOM-aware text input. *)
let absolutePath path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let exists path =
  try (Unix.stat path).Unix.st_kind <> Unix.S_DIR
  with Unix.Unix_error _ -> false

let readText path =
  In_channel.with_open_bin path (fun channel ->
      let bytes = In_channel.input_all channel in
      let encoding =
        if String.starts_with ~prefix:"\000\000\254\255" bytes then
          Some "text/plain; charset=utf-32be"
        else None
      in
      ContentEncoding.decodeContent encoding bytes)
