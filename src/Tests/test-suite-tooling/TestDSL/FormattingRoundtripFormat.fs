// FormattingRoundtripFormat.fs - Parser for focused parser/pretty roundtrip test files.
//
// Format:
//   <dark expression> // optional display name
//
// One expression per non-empty, non-comment line.

module TestDSL.FormattingRoundtripFormat

open System
open System.IO

type FormattingRoundtripCase = {
    Name: string
    Source: string
    SourceFile: string
}

let private splitLineComment (line: string) : string * string option =
    let rec firstCommentIndex index inString escaped =
        if index >= line.Length - 1 then
            None
        else
            match line.[index], inString, escaped with
            | _, true, true -> firstCommentIndex (index + 1) true false
            | '\\', true, false -> firstCommentIndex (index + 1) true true
            | '"', true, false -> firstCommentIndex (index + 1) false false
            | '"', false, false -> firstCommentIndex (index + 1) true false
            | '/', false, false when line.[index + 1] = '/' -> Some index
            | _ -> firstCommentIndex (index + 1) inString false

    match firstCommentIndex 0 false false with
    | Some commentIdx ->
        let source = line.Substring(0, commentIdx).Trim()
        let comment = line.Substring(commentIdx + 2).Trim()
        let displayName =
            if String.IsNullOrWhiteSpace comment then
                None
            else
                Some comment
        (source, displayName)
    | None ->
        (line.Trim(), None)

let parseFormattingRoundtripFile (path: string) : Result<FormattingRoundtripCase list, string> =
    if not (File.Exists path) then
        Error $"Formatting roundtrip file not found: {path}"
    else
        let lines = File.ReadAllLines(path)
        let collectLine
            (tests, errors)
            (i: int)
            (line: string)
            : FormattingRoundtripCase list * string list =
            let lineNumber = i + 1
            let trimmed = line.Trim()

            if trimmed.Length > 0 && not (trimmed.StartsWith("//")) then
                let source, displayNameOpt = splitLineComment line
                if String.IsNullOrWhiteSpace source then
                    (tests, $"Line {lineNumber}: missing expression before comment" :: errors)
                else
                    let displayName = displayNameOpt |> Option.defaultValue source
                    (
                        {
                            Name = $"L{lineNumber}: {displayName}"
                            Source = source
                            SourceFile = path
                        } :: tests,
                        errors
                    )
            else
                (tests, errors)

        let (tests, errors) =
            lines
            |> Array.indexed
            |> Array.fold (fun state (i, line) -> collectLine state i line) ([], [])

        if List.isEmpty errors then
            Ok (List.rev tests)
        else
            errors
            |> List.rev
            |> String.concat "\n"
            |> Error
