// Common.fs - Common utilities for parsing DSL-based test files
//
// Provides section-delimited test file parsing, comment stripping,
// and other shared utilities for test DSLs.

module TestDSL.Common

open System
open System.Text.RegularExpressions

/// Section name and content
type Section = string * string

/// Test file with parsed sections
type TestFile = {
    Sections: Map<string, string>
}

/// Split content into sections using ---SECTION-NAME--- delimiters
let parseSections (content: string) : Section list =
    let sectionDelimiter = Regex(@"^---([A-Z0-9-]+)---$", RegexOptions.Multiline)
    let matches = sectionDelimiter.Matches(content)

    if matches.Count = 0 then
        []
    else
        let sections = ResizeArray<Section>()
        for i in 0 .. matches.Count - 1 do
            let sectionName = matches.[i].Groups.[1].Value
            let startIdx = matches.[i].Index + matches.[i].Length
            let endIdx =
                if i + 1 < matches.Count then
                    matches.[i + 1].Index
                else
                    content.Length
            let sectionContent = content.Substring(startIdx, endIdx - startIdx)
            sections.Add((sectionName, sectionContent))
        List.ofSeq sections

/// Parse test file into sections
let parseTestFile (content: string) : TestFile =
    let sections = parseSections content
    { Sections = Map.ofList sections }

/// Get required section or return error
let getRequiredSection (name: string) (file: TestFile) : Result<string, string> =
    match Map.tryFind name file.Sections with
    | Some content -> Ok (content.Trim())
    | None -> Error $"Missing required section: {name}"

/// Get optional section
let getOptionalSection (name: string) (file: TestFile) : string option =
    Map.tryFind name file.Sections
    |> Option.map (fun s -> s.Trim())

/// Strip comments (starting with //) and empty lines from text
let stripCommentsAndEmpty (text: string) : string list =
    text.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line ->
        // Remove comments starting with //
        let commentIdx = line.IndexOf("//")
        if commentIdx >= 0 then
            line.Substring(0, commentIdx)
        else
            line)
    |> Array.map (fun line -> line.Trim())
    |> Array.filter (fun line -> line.Length > 0)
    |> List.ofArray

/// Normalize line endings for comparison
let normalizeLineEndings (text: string) : string =
    text.Replace("\r\n", "\n").Replace("\r", "\n")
