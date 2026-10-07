(*
   NameSyntax.ml - Shared lexical and structural contract for source names.
   This module is the single parser-facing authority for identifier characters,
   reserved words, blank names, quoted identifiers, and qualified segments.
   Stable caller-supplied identity for an independently parsed source unit.
*)
(* NameSyntax.ml - Unicode scalar identifiers, quoted segments, and module-header extraction. *)
[@@@warning "-4"]
type identifier = OrdinaryIdentifier of string | BlankIdentifier
type qualifiedName = QualifiedName of identifier NonEmptyList.t
module Keyword = struct
  type t = Let | Val | In | If | Elif | Then | Else | Type | Of | Match | With | Fun | When | True | False | Underscore
end
(*
   Why a caller supplied a source unit. Only executable units may contribute
   an entry expression; library and package units are declarations-only.
*)
type identifierToken = IdentifierToken of identifier | KeywordToken of Keyword.t
module SourceUnitPurpose = struct type t = Executable | Library | Package end
type sourceUnitName = SourceUnitName of string
let sourceUnitName name = if Text.trim name = "" then Error "Source unit name must not be empty" else Ok (SourceUnitName name)
let sourceUnitNameText (SourceUnitName name) = name
let isStartCharacter unit = Text.isLetter unit || unit = 95
let isContinueCharacter unit = Text.isLetter unit || Text.isDigit unit || unit = 95 || unit = 39
let identifierText = function OrdinaryIdentifier text -> text | BlankIdentifier -> ""
let identifierFromText text = if text = "" || text = "___" then BlankIdentifier else OrdinaryIdentifier text
let keywords = ["let", Keyword.Let; "val", Keyword.Val; "in", Keyword.In; "if", Keyword.If; "elif", Keyword.Elif;
  "then", Keyword.Then; "else", Keyword.Else; "type", Keyword.Type; "of", Keyword.Of; "match", Keyword.Match;
  "with", Keyword.With; "fun", Keyword.Fun; "when", Keyword.When; "true", Keyword.True; "false", Keyword.False; "_", Keyword.Underscore]
let classify text = match List.assoc_opt text keywords with Some keyword -> KeywordToken keyword | None -> IdentifierToken (identifierFromText text)
module Words = Set.Make (String)
let reservedWords = Words.of_list (List.map fst keywords)
let isBareIdentifier = function
  | BlankIdentifier -> false
  | OrdinaryIdentifier text -> let units = Text.scalars text in
      Array.length units > 0 && isStartCharacter units.(0) && Array.for_all isContinueCharacter units && not (Words.mem text reservedWords)
let formatIdentifier = function BlankIdentifier -> "___" | OrdinaryIdentifier text as identifier -> if isBareIdentifier identifier then text else "``" ^ text ^ "``"
let singleton identifier = QualifiedName (NonEmptyList.singleton identifier)
let fromNonEmptySegments value = QualifiedName value
let append identifier (QualifiedName value) = QualifiedName (NonEmptyList.snoc value identifier)
let concat (QualifiedName first) (QualifiedName second) = QualifiedName (NonEmptyList.appendList first (NonEmptyList.toList second))
let segments (QualifiedName value) = NonEmptyList.toList value
let trySplitLast name = match List.rev (segments name) with
  | last :: prefix -> Option.map (fun prefix -> fromNonEmptySegments prefix, last) (NonEmptyList.tryFromList (List.rev prefix))
  | [] -> None
let formatQualifiedName name = String.concat "." (List.map formatIdentifier (segments name))
(*
   The legacy compiler AST still consumes a string at the parse/resolution
   boundary. Keep quoted segment delimiters in that string so embedded dots are
   lossless; NameResolution parses this representation back into segments.
*)
let toLegacySpelling = formatQualifiedName
let tryParseLegacySpelling spelling =
  let units = Text.scalars spelling in let length = Array.length units in
  let slice first last = Text.ofScalars (Array.sub units first (last - first)) in
  let rec quoted first index =
    if index + 1 >= length then None
    else if units.(index) = 96 && units.(index + 1) = 96 then Some (slice first index, index + 2)
    else quoted first (index + 1) in
  let rec bare first index =
    if index >= length || units.(index) = 46 then if index = first then None else Some (slice first index, index)
    else bare first (index + 1) in
  let rec loop index acc =
    if index >= length then Option.map fromNonEmptySegments (NonEmptyList.tryFromList (List.rev acc))
    else
      let segment = if index + 1 < length && units.(index) = 96 && units.(index + 1) = 96 then quoted (index + 2) (index + 2) else bare index index in
      match segment with None -> None | Some (text, next) ->
        let identifier = identifierFromText text in
        if next = length then loop next (identifier :: acc)
        else if units.(next) = 46 && next + 1 < length then loop (next + 1) (identifier :: acc)
        else None in
  loop 0 []
let scanOrdinary input start =
  let units = Text.scalars input in
  let rec ending index = if index < Array.length units && isContinueCharacter units.(index) then ending (index + 1) else index in
  let stop = ending (start + 1) in
  identifierFromText (Text.ofScalars (Array.sub units start (stop - start))), stop
let scanQuoted input start =
  let units = Text.scalars input in
  let rec ending index =
    if index >= Array.length units || units.(index) = 10 || units.(index) = 13 then Error "Unterminated backtick identifier"
    else if index + 1 < Array.length units && units.(index) = 96 && units.(index + 1) = 96 then
      Ok (identifierFromText (Text.ofScalars (Array.sub units (start + 2) (index - start - 2))), index + 2)
    else ending (index + 1) in
  ending (start + 2)
let tryExtractModuleHeader source =
  let buffer = Buffer.create (String.length source) in
  let rec normalize index = if index < String.length source then
    if source.[index] = '\r' then begin Buffer.add_char buffer '\n'; normalize (index + if index + 1 < String.length source && source.[index + 1] = '\n' then 2 else 1) end
    else begin Buffer.add_char buffer source.[index]; normalize (index + 1) end in
  normalize 0;
  let trimStart text =
    let units = Text.scalars text in
    let rec start index = if index < Array.length units && Text.trim (Text.ofScalars [|units.(index)|]) = "" then start (index + 1) else index in
    start 0 in
  let rec find prefix = function
    | [] -> None
    | line :: rest ->
        let trimmed = Text.trim line in
        if trimmed = "" || String.starts_with ~prefix:"//" trimmed then find (line :: prefix) rest
        else if String.starts_with ~prefix:"module " trimmed then
          let block = String.ends_with ~suffix:"=" trimmed in
          let moduleText = Text.trim (String.sub trimmed 7 (String.length trimmed - 7)) in
          let spelling = if block then Text.trim (String.sub moduleText 0 (String.length moduleText - 1)) else moduleText in
          let body = if block then
            let significant = List.filter (fun line -> Text.trim line <> "") rest in
            match List.sort Int.compare (List.map trimStart significant) with
            | indent :: _ when indent > trimStart line ->
                String.concat "\n" (List.map (fun line -> if Text.trim line = "" then "" else
                  let units = Text.scalars line in if Array.length units >= indent then Text.ofScalars (Array.sub units indent (Array.length units - indent)) else line) rest)
            | _ -> ""
            else String.concat "\n" (List.rev prefix @ rest) in
          Option.map (fun name -> name, body) (tryParseLegacySpelling spelling)
        else None in
  find [] (String.split_on_char '\n' (Buffer.contents buffer))
