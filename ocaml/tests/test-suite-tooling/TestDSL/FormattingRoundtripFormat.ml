(*
   FormattingRoundtripFormat.fs - Parser for focused parser/pretty roundtrip test files.
   Format:
   <dark expression> // optional display name
   One expression per non-empty, non-comment line.
*)
(* FormattingRoundtripFormat.ml - Preserve string-aware comments and fixture line names. *)
open Dark_compiler
type formattingRoundtripCase = {name : string; source : string; sourceFile : string}
let splitLineComment line =
  let units = HostText.utf16Units line in
  let rec comment index inString escaped =
    if index >= Array.length units - 1 then None else
    match units.(index), inString, escaped with
    | _, true, true -> comment (index + 1) true false
    | 92, true, false -> comment (index + 1) true true
    | 34, true, false -> comment (index + 1) false false
    | 34, false, false -> comment (index + 1) true false
    | 47, false, false when units.(index + 1) = 47 -> Some index
    | _ -> comment (index + 1) inString false in
  match comment 0 false false with
  | Some index ->
      let source = HostText.trim (HostText.ofUtf16Units (Array.sub units 0 index)) in
      let name = HostText.trim (HostText.ofUtf16Units (Array.sub units (index + 2) (Array.length units - index - 2))) in
      source, (if name = "" then None else Some name)
  | None -> HostText.trim line, None
let parseFormattingRoundtripFile path =
  if not (TestFileIO.exists path) then Error ("Formatting roundtrip file not found: " ^ path) else
  let tests, errors = Array.fold_left (fun (tests, errors) (index, line) ->
    let number = index + 1 and trimmed = HostText.trim line in
    if trimmed <> "" && not (String.starts_with ~prefix:"//" trimmed) then
      let source, display = splitLineComment line in
      if source = "" then tests, Printf.sprintf "Line %d: missing expression before comment" number :: errors
      else let name = match display with Some value -> value | None -> source in
        {name = Printf.sprintf "L%d: %s" number name; source; sourceFile = path} :: tests, errors
    else tests, errors) ([], []) (Array.mapi (fun index line -> index, line) (TestFileIO.readAllLines path)) in
  if errors = [] then Ok (List.rev tests) else Error (String.concat "\n" (List.rev errors))
