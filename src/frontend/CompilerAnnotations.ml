(* CompilerAnnotations.ml - Attach compiler metadata using real lexer trivia, never text matches. *)
module T = Tokenizer

type phase = Compile | Runtime
type failureClass = AotRejection | MissingSupport | UpstreamBug

type disposition =
  | Run
  | Blocked of string
  | Excluded of string
  | Replace of { target : string; reason : string }
  | Xfail of {
      phase : phase;
      diagnostic : string;
      reason : string;
      classification : failureClass;
    }

type item = { position : T.pos; isTest : bool }
type annotation = { position : T.pos; disposition : disposition }

let ( let* ) = Result.bind

let tagged reasons tag =
  if List.mem tag reasons then Ok tag
  else Error ("Unknown compiler reason tag: " ^ tag)

let splitReason reasons text =
  match
    String.split_on_char ' ' (String.trim text) |> List.filter (( <> ) "")
  with
  | [ target; reason ] when String.starts_with ~prefix:"reason=" reason ->
      let* reason =
        tagged reasons (String.sub reason 7 (String.length reason - 7))
      in
      Ok (target, reason)
  | _ -> Error "Expected a target followed by reason=TAG"

let parse reasons text =
  let text = String.trim text in
  let after prefix =
    String.sub text (String.length prefix)
      (String.length text - String.length prefix)
    |> String.trim
  in
  if text = "run" then Ok Run
  else if String.starts_with ~prefix:"blocked:" text then
    Result.map (fun tag -> Blocked tag) (tagged reasons (after "blocked:"))
  else if String.starts_with ~prefix:"excluded:" text then
    Result.map (fun tag -> Excluded tag) (tagged reasons (after "excluded:"))
  else if String.starts_with ~prefix:"replace:" text then
    let* target, reason = splitReason reasons (after "replace:") in
    match String.split_on_char '#' target with
    | [ path; name ]
      when path <> "" && name <> "" && Filename.is_relative path
           && not (List.mem ".." (String.split_on_char '/' path)) ->
        Ok (Replace { target; reason })
    | _ ->
        Error
          "Replacement target must be a repository-relative path#declaration"
  else if String.starts_with ~prefix:"xfail " text then
    let rest = after "xfail " in
    let* phase, rest =
      if String.starts_with ~prefix:"compile:" rest then
        Ok (Compile, String.sub rest 8 (String.length rest - 8) |> String.trim)
      else if String.starts_with ~prefix:"runtime:" rest then
        Ok (Runtime, String.sub rest 8 (String.length rest - 8) |> String.trim)
      else Error "Expected xfail compile: or xfail runtime:"
    in
    (* Parse the two metadata fields from the right to preserve JSON whitespace. *)
    let splitLast text =
      match String.rindex_opt text ' ' with
      | None ->
          Error
            "Expected a quoted diagnostic followed by class=CLASS reason=TAG"
      | Some index ->
          Ok
            ( String.sub text 0 index |> String.trim,
              String.sub text (index + 1) (String.length text - index - 1) )
    in
    let* quotedAndClass, reasonField = splitLast rest in
    let* _, reason = splitReason reasons ("diagnostic " ^ reasonField) in
    let* quoted, classField = splitLast quotedAndClass in
    let* classification =
      match (classField, phase) with
      | "class=missing-support", (Compile | Runtime) -> Ok MissingSupport
      | "class=upstream-bug", (Compile | Runtime) -> Ok UpstreamBug
      | "class=aot-rejection", Compile -> Ok AotRejection
      | "class=aot-rejection", Runtime ->
          Error "An AOT rejection must require a compile failure"
      | _, (Compile | Runtime) ->
          Error
            "Expected class=aot-rejection, class=missing-support or \
             class=upstream-bug"
    in
    try
      match Yojson.Basic.from_string quoted with
      | `String diagnostic when diagnostic <> "" ->
          Ok (Xfail { phase; diagnostic; reason; classification })
      | _ -> Error "Expected a nonempty quoted diagnostic"
    with Yojson.Json_error _ -> Error "Invalid quoted diagnostic"
  else Error ("Unknown compiler annotation: " ^ text)

let read ~reasons ~items source =
  let* tokens, _ = Lexer.tokenize source in
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let rec scan annotations = function
    | [] -> Ok (List.rev annotations)
    | (token : Lexer.spannedToken) :: rest ->
        let rec comments annotations = function
          | [] -> scan annotations rest
          | (comment : Lexer.trivia) :: remaining -> (
              let text = String.trim comment.Lexer.text in
              if
                comment.Lexer.kind <> Lexer.LineComment
                || not (String.starts_with ~prefix:"// @compiler" text)
              then comments annotations remaining
              else
                let position = token.Lexer.range.T.start in
                let fail message =
                  Error
                    (Printf.sprintf "Line %d: %s"
                       (comment.Lexer.range.T.start.T.row + 1)
                       message)
                in
                if not (String.starts_with ~prefix:"// @compiler " text) then
                  fail "Expected a per-item // @compiler annotation"
                else
                  match
                    List.find_opt
                      (fun (item : item) -> item.position = position)
                      items
                  with
                  | None ->
                      fail
                        "Compiler annotation must precede a declaration or test"
                  | Some item -> (
                      let prefix =
                        String.sub
                          lines.(comment.Lexer.range.T.start.T.row)
                          0 comment.Lexer.range.T.start.T.column
                      in
                      if
                        String.trim prefix <> ""
                        || comment.Lexer.range.T.start.T.column
                           <> position.T.column
                        || comment.Lexer.range.T.end_.T.row >= position.T.row
                      then
                        fail
                          "Compiler annotation must be on its own line at the \
                           item's indentation"
                      else if
                        List.exists
                          (fun (entry : annotation) ->
                            entry.position = position)
                          annotations
                      then
                        fail "Conflicting compiler annotations on the same item"
                      else
                        let* disposition =
                          parse reasons
                            (String.sub text 13 (String.length text - 13))
                        in
                        match disposition with
                        | Xfail _ when not item.isTest ->
                            fail "Expected failures apply only to tests"
                        | Replace _ when item.isTest ->
                            fail "Tests cannot have replacement implementations"
                        | Run | Blocked _ | Excluded _ | Replace _ | Xfail _ ->
                            comments
                              ({ position; disposition } :: annotations)
                              remaining))
        in
        comments annotations token.Lexer.leadingTrivia
  in
  scan [] tokens

let at annotations position =
  match
    List.find_opt
      (fun (entry : annotation) -> entry.position = position)
      annotations
  with
  | Some entry -> entry.disposition
  | None -> Run

let parseReasons source =
  try
    match Yojson.Basic.from_string source with
    | `Assoc entries ->
        let rec validate seen = function
          | [] -> Ok (List.rev seen)
          | (tag, `String description) :: rest
            when tag <> ""
                 && String.trim description <> ""
                 && (not (List.mem tag seen))
                 && String.for_all
                      (function
                        | 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
                      tag ->
              validate (tag :: seen) rest
          | _ ->
              Error
                "Reason glossary requires unique lowercase tags and nonempty \
                 descriptions"
        in
        validate [] entries
    | _ -> Error "Reason glossary must be a JSON object"
  with Yojson.Json_error message | Sys_error message -> Error message

let readReasons path =
  try In_channel.with_open_bin path In_channel.input_all |> parseReasons
  with Sys_error message -> Error message
