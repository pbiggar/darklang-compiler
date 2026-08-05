// X86_64EncodingFormat.fs - Parser for multi-case x64 encoding and resolution fixtures.
//
// A successful case can assert final bytes, deferred fixup labels, or both.

module TestDSL.X86_64EncodingFormat

open System
open System.Globalization
open TestDSL.Common
open TestDSL.X86_64Parser
open X86_64

type X64EncodingExpectation =
    | ResolvesTo of expectedBytes: byte array option * expectedFixupLabels: string list
    | ResolutionErrorContaining of string

type X64EncodingTest = {
    Name: string
    Instructions: Instr list
    Expectation: X64EncodingExpectation
    SourceFile: string
}

let private knownSections =
    Set.ofList [ "NAME"; "INPUT-X64"; "OUTPUT-HEX"; "EXPECT-FIXUPS"; "EXPECT-ERROR" ]

let private groupCases sections =
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
            | [] -> Error $"x64 encoding case must start with NAME, found {fst section}"
            | _ -> loop completed (section :: current) rest
    loop [] [] sections

let private toSectionMap sections =
    match sections |> List.tryFind (fun (name, _) -> not (Set.contains name knownSections)) with
    | Some (name, _) -> Error $"Unknown x64 encoding section: {name}"
    | None ->
        match sections |> List.countBy fst |> List.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> Error $"Duplicate x64 encoding section: {name}"
        | None -> Ok (Map.ofList sections)

let private required (name: string) (sections: Map<string, string>) =
    match Map.tryFind name sections with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Ok (value.Trim())
    | Some _ -> Error $"x64 encoding section {name} cannot be empty"
    | None -> Error $"Missing required x64 encoding section: {name}"

let private optional (name: string) (sections: Map<string, string>) =
    Map.tryFind name sections |> Option.map (fun value -> value.Trim())

let private parseHexBytes (text: string) : Result<byte array, string> =
    let tokens =
        text.Split([|' '; '\t'; '\n'; '\r'; ','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    let parseToken (token: string) =
        let trimmed = token.Trim()
        let digits = if trimmed.StartsWith("0x", StringComparison.OrdinalIgnoreCase) then trimmed.Substring(2) else trimmed
        match Byte.TryParse(digits, NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
        | true, value -> Ok value
        | false, _ -> Error $"Invalid x64 hex byte '{trimmed}'"
    let rec loop parsed remaining =
        match remaining with
        | [] -> Ok (List.rev parsed |> List.toArray)
        | token :: rest ->
            match parseToken token with
            | Ok value -> loop (value :: parsed) rest
            | Error msg -> Error msg
    match tokens with
    | [] -> Error "OUTPUT-HEX requires at least one byte"
    | _ -> loop [] tokens

let private parseFixups (text: string) : string list =
    text.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line <> "" && not (line.StartsWith("//")))
    |> Array.toList

let private parseCase (path: string) (sections: Section list) =
    toSectionMap sections
    |> Result.bind (fun values ->
        match required "NAME" values, required "INPUT-X64" values with
        | Error msg, _
        | _, Error msg -> Error msg
        | Ok name, Ok input ->
            parseX64 input
            |> Result.bind (fun instructions ->
                let output = optional "OUTPUT-HEX" values
                let fixups = optional "EXPECT-FIXUPS" values
                let expectedError = optional "EXPECT-ERROR" values
                match output, fixups, expectedError with
                | _, _, Some errorText when String.IsNullOrWhiteSpace errorText ->
                    Error "EXPECT-ERROR cannot be empty"
                | Some _, _, Some _
                | _, Some _, Some _ -> Error "EXPECT-ERROR cannot be combined with successful x64 expectations"
                | None, None, None -> Error "x64 encoding test requires OUTPUT-HEX, EXPECT-FIXUPS, or EXPECT-ERROR"
                | None, None, Some errorText ->
                    Ok { Name = name; Instructions = instructions; Expectation = ResolutionErrorContaining errorText; SourceFile = path }
                | output, fixups, None ->
                    let bytesResult =
                        match output with
                        | Some text -> parseHexBytes text |> Result.map Some
                        | None -> Ok None
                    match bytesResult with
                    | Error msg -> Error msg
                    | Ok expectedBytes ->
                        let expectedFixups = fixups |> Option.map parseFixups |> Option.defaultValue []
                        Ok {
                            Name = name
                            Instructions = instructions
                            Expectation = ResolvesTo (expectedBytes, expectedFixups)
                            SourceFile = path
                        }))

let parseX64EncodingFileContent (path: string) (content: string) : Result<X64EncodingTest list, string> =
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then Error "x64 encoding fixture contains no sections"
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
