(*
   Common.ml - Common utilities for parsing DSL-based test files
   Provides section-delimited test file parsing, comment stripping,
   and other shared utilities for test DSLs.
*)
(* Common.ml - Preserve section boundaries, repeated keys, and the explicit escape alphabet. *)
open Dark_compiler

(*
   Section name and content
*)
type section = string * string

(*
   Test file with parsed sections
*)
type testFile = { sections : string StringOrder.Map.t }

(*
   Split content into sections using ---SECTION-NAME--- delimiters
*)
let parseSections content =
  let length = String.length content in
  let rec lines start matches =
    if start > length then List.rev matches
    else
      let ending =
        match String.index_from_opt content start '\n' with
        | Some index -> index
        | None -> length
      in
      let line = String.sub content start (ending - start) in
      let nameLength = String.length line - 6 in
      let matches =
        if
          nameLength > 0
          && String.starts_with ~prefix:"---" line
          && String.ends_with ~suffix:"---" line
        then
          let name = String.sub line 3 nameLength in
          if
            String.for_all
              (fun char ->
                (char >= 'A' && char <= 'Z')
                || (char >= '0' && char <= '9')
                || char = '-')
              name
          then (name, start, ending) :: matches
          else matches
        else matches
      in
      if ending = length then List.rev matches else lines (ending + 1) matches
  in
  let rec sections = function
    | [] -> []
    | (name, _, beginning) :: rest ->
        let ending =
          match rest with (_, start, _) :: _ -> start | [] -> length
        in
        (name, String.sub content beginning (ending - beginning))
        :: sections rest
  in
  sections (lines 0 [])

(*
   Parse test file into sections
*)
let parseTestFile content =
  {
    sections =
      List.fold_left
        (fun sections (name, value) -> StringOrder.Map.add name value sections)
        StringOrder.Map.empty (parseSections content);
  }

(*
   Get required section or return error
*)
let getRequiredSection name file =
  match StringOrder.Map.find_opt name file.sections with
  | Some value -> Ok (Text.trim value)
  | None -> Error ("Missing required section: " ^ name)

(*
   Get optional section
*)
let getOptionalSection name file =
  Option.map Text.trim (StringOrder.Map.find_opt name file.sections)

(*
   Strip comments (starting with //) and empty lines from text
   Remove comments starting with //
*)
let stripCommentsAndEmpty text =
  String.split_on_char '\n' text
  |> List.concat_map (String.split_on_char '\r')
  |> List.map (fun line ->
      let rec comment index =
        if index + 1 >= String.length line then None
        else if line.[index] = '/' && line.[index + 1] = '/' then Some index
        else comment (index + 1)
      in
      Text.trim
        (match comment 0 with
        | None -> line
        | Some index -> String.sub line 0 index))
  |> List.filter (fun line -> line <> "")

(*
   Normalize line endings for comparison
*)
let normalizeLineEndings text =
  let buffer = Buffer.create (String.length text) in
  let rec loop index =
    if index < String.length text then
      if text.[index] = '\r' then begin
        Buffer.add_char buffer '\n';
        loop
          (index
          +
          if index + 1 < String.length text && text.[index + 1] = '\n' then 2
          else 1)
      end
      else begin
        Buffer.add_char buffer text.[index];
        loop (index + 1)
      end
  in
  loop 0;
  Buffer.contents buffer

(*
   Decode the small, explicit escape alphabet used by text-bearing test DSLs.
*)
let parseEscapedText text =
  let units = Text.scalars text in
  let rec loop index reversed =
    if index >= Array.length units then
      Ok (Text.ofScalars (Array.of_list (List.rev reversed)))
    else if units.(index) <> 92 then loop (index + 1) (units.(index) :: reversed)
    else if index + 1 >= Array.length units then
      Error "Escaped text cannot end with a backslash"
    else
      let decoded =
        match units.(index + 1) with
        | 92 -> Some 92
        | 34 -> Some 34
        | 110 -> Some 10
        | 114 -> Some 13
        | 116 -> Some 9
        | _ -> None
      in
      match decoded with
      | Some value -> loop (index + 2) (value :: reversed)
      | None ->
          Error
            ("Unsupported escape sequence '\\"
            ^ Text.ofScalars [| units.(index + 1) |]
            ^ "'")
  in
  loop 0 []
