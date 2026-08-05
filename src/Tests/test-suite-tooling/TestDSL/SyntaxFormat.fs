// SyntaxFormat.fs - Parser for syntax acceptance, formatting, and roundtrip fixtures.
//
// Supports multiple section-delimited cases per .syntax file.

module TestDSL.SyntaxFormat

open System
open TestDSL.Common

type SyntaxKind =
    | Compiler
    | Interpreter

type SyntaxTest = {
    Name: string
    ParseAs: SyntaxKind
    Source: string
    ExpectedError: string option
    FormatAs: SyntaxKind option
    ExpectedFormat: string option
    RoundtripAs: SyntaxKind list
    SourceFile: string
}

let private parseSyntaxKind (context: string) (text: string) : Result<SyntaxKind, string> =
    match text.Trim().ToLowerInvariant() with
    | "compiler" -> Ok Compiler
    | "interpreter" -> Ok Interpreter
    | value -> Error $"Invalid {context} syntax '{value}' (expected 'compiler' or 'interpreter')"

let private parseRoundtripKinds (text: string) : Result<SyntaxKind list, string> =
    let values =
        text.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun value -> value.Trim())
        |> Array.toList

    let rec loop parsed remaining =
        match remaining with
        | [] -> Ok (List.rev parsed)
        | value :: rest ->
            match parseSyntaxKind "ROUNDTRIP-AS" value with
            | Error msg -> Error msg
            | Ok syntax -> loop (syntax :: parsed) rest

    match values with
    | [] -> Error "ROUNDTRIP-AS requires at least one syntax"
    | _ ->
        loop [] values
        |> Result.bind (fun parsed ->
            if List.distinct parsed = parsed then Ok parsed
            else Error "ROUNDTRIP-AS contains a duplicate syntax")

let private knownSections =
    Set.ofList [
        "NAME"
        "PARSE-AS"
        "SOURCE"
        "EXPECT-ERROR"
        "FORMAT-AS"
        "EXPECTED"
        "ROUNDTRIP-AS"
    ]

let private groupCases (sections: Section list) : Result<Section list list, string> =
    let rec loop completed current remaining =
        match remaining with
        | [] ->
            match current with
            | [] -> Ok (List.rev completed)
            | _ -> Ok (List.rev (List.rev current :: completed))
        | (("NAME", _) as section) :: rest ->
            match current with
            | [] -> loop completed [section] rest
            | _ -> loop (List.rev current :: completed) [section] rest
        | section :: rest ->
            match current with
            | [] -> Error $"Syntax case must start with NAME, found {fst section}"
            | _ -> loop completed (section :: current) rest

    loop [] [] sections

let private sectionMap (sections: Section list) : Result<Map<string, string>, string> =
    let unknown = sections |> List.tryFind (fun (name, _) -> not (Set.contains name knownSections))
    match unknown with
    | Some (name, _) -> Error $"Unknown syntax section: {name}"
    | None ->
        let duplicate =
            sections
            |> List.countBy fst
            |> List.tryFind (fun (_, count) -> count > 1)
        match duplicate with
        | Some (name, _) -> Error $"Duplicate syntax section: {name}"
        | None -> Ok (Map.ofList sections)

let private requiredSection (name: string) (sections: Map<string, string>) : Result<string, string> =
    match Map.tryFind name sections with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Ok (value.Trim())
    | Some _ -> Error $"Syntax section {name} cannot be empty"
    | None -> Error $"Missing required syntax section: {name}"

let private optionalSection (name: string) (sections: Map<string, string>) : string option =
    Map.tryFind name sections
    |> Option.map (fun value -> value.Trim())

let private parseCase (path: string) (sections: Section list) : Result<SyntaxTest, string> =
    sectionMap sections
    |> Result.bind (fun values ->
        match requiredSection "NAME" values, requiredSection "PARSE-AS" values, requiredSection "SOURCE" values with
        | Error msg, _, _
        | _, Error msg, _
        | _, _, Error msg -> Error msg
        | Ok name, Ok parseAsText, Ok source ->
            parseSyntaxKind "PARSE-AS" parseAsText
            |> Result.bind (fun parseAs ->
                let expectedError = optionalSection "EXPECT-ERROR" values
                let formatAsText = optionalSection "FORMAT-AS" values
                let expectedFormat = optionalSection "EXPECTED" values
                let roundtripText = optionalSection "ROUNDTRIP-AS" values

                let formatAsResult =
                    match formatAsText with
                    | Some text -> parseSyntaxKind "FORMAT-AS" text |> Result.map Some
                    | None -> Ok None
                let roundtripResult =
                    match roundtripText with
                    | Some text -> parseRoundtripKinds text
                    | None -> Ok []

                match formatAsResult, roundtripResult with
                | Error msg, _
                | _, Error msg -> Error msg
                | Ok formatAs, Ok roundtripAs ->
                    match formatAs, expectedFormat, expectedError with
                    | None, Some _, _ -> Error "EXPECTED requires FORMAT-AS"
                    | Some _, None, _ -> Error "FORMAT-AS requires EXPECTED"
                    | Some _, Some _, Some _ -> Error "EXPECT-ERROR cannot be combined with FORMAT-AS"
                    | None, None, Some _ when not (List.isEmpty roundtripAs) ->
                        Error "EXPECT-ERROR cannot be combined with ROUNDTRIP-AS"
                    | _ ->
                        Ok {
                            Name = name
                            ParseAs = parseAs
                            Source = normalizeLineEndings source
                            ExpectedError = expectedError
                            FormatAs = formatAs
                            ExpectedFormat = expectedFormat |> Option.map normalizeLineEndings
                            RoundtripAs = roundtripAs
                            SourceFile = path
                        }))

let parseSyntaxFileContent (path: string) (content: string) : Result<SyntaxTest list, string> =
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then
        Error "Syntax fixture contains no sections"
    else
        groupCases sections
        |> Result.bind (fun cases ->
            let rec loop parsed remaining =
                match remaining with
                | [] -> Ok (List.rev parsed)
                | sections :: rest ->
                    match parseCase path sections with
                    | Ok test -> loop (test :: parsed) rest
                    | Error msg -> Error msg
            loop [] cases)
