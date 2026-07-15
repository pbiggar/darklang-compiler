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
        let mutable tests = []
        let mutable errors = []

        for i in 0 .. lines.Length - 1 do
            let line = lines.[i]
            let lineNumber = i + 1
            let trimmed = line.Trim()

            if trimmed.Length > 0 && not (trimmed.StartsWith("//")) then
                let source, displayNameOpt = splitLineComment line
                if String.IsNullOrWhiteSpace source then
                    errors <- $"Line {lineNumber}: missing expression before comment" :: errors
                else
                    let displayName = displayNameOpt |> Option.defaultValue source
                    tests <-
                        {
                            Name = $"L{lineNumber}: {displayName}"
                            Source = source
                            SourceFile = path
                        }
                        :: tests

        if List.isEmpty errors then
            Ok (List.rev tests)
        else
            Error (String.concat "\n" (List.rev errors))
