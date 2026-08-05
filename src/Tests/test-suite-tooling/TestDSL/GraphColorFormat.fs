// GraphColorFormat.fs - Parser for graph-coloring algorithm fixtures.
//
// Represents graph topology, coloring preferences, and observable properties as typed data.

module TestDSL.GraphColorFormat

open System
open TestDSL.Common

type CountExpectation =
    | Exactly of int
    | AtMost of int
    | AtLeast of int

type GraphColorTest = {
    Name: string
    Vertices: int list
    Edges: (int * int) list
    AvailableColors: int
    Precolored: (int * int) list
    PreferencePairs: (int * int) list
    MovePairs: (int * int) list
    ExpectedChromatic: CountExpectation option
    ExpectedSpills: CountExpectation option
    ExpectedColored: CountExpectation option
    ExpectedColors: (int * int) list
    ExpectedSame: (int * int) list
    ExpectedDifferent: (int * int) list
    ExpectMcsCoversAll: bool
    ExpectedSelectionChecks: int option
    SourceFile: string
}

let private knownSections =
    Set.ofList [
        "NAME"
        "VERTICES"
        "EDGES"
        "AVAILABLE-COLORS"
        "PRECOLORED"
        "PREFER"
        "MOVE-PREFER"
        "EXPECT-CHROMATIC"
        "EXPECT-SPILLS"
        "EXPECT-COLORED"
        "EXPECT-COLORS"
        "EXPECT-SAME"
        "EXPECT-DIFFERENT"
        "EXPECT-MCS-ORDERING"
        "EXPECT-SELECTION-CHECKS"
    ]

let private groupCases sections =
    let rec loop completed current remaining =
        match remaining with
        | [] ->
            match current with
            | [] -> Ok (List.rev completed)
            | _ -> Ok (List.rev (List.rev current :: completed))
        | (("NAME", _) as section) :: rest ->
            match current with
            | [] -> loop completed [ section ] rest
            | _ -> loop (List.rev current :: completed) [ section ] rest
        | section :: rest ->
            match current with
            | [] -> Error $"Graph-color case must start with NAME, found {fst section}"
            | _ -> loop completed (section :: current) rest
    loop [] [] sections

let private toSectionMap sections =
    match sections |> List.tryFind (fun (name, _) -> not (Set.contains name knownSections)) with
    | Some (name, _) -> Error $"Unknown graph-color section: {name}"
    | None ->
        match sections |> List.countBy fst |> List.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> Error $"Duplicate graph-color section: {name}"
        | None -> Ok (Map.ofList sections)

let private required name (sections: Map<string, string>) =
    match Map.tryFind name sections with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Ok (value.Trim())
    | Some _ -> Error $"Graph-color section {name} cannot be empty"
    | None -> Error $"Missing required graph-color section: {name}"

let private optional name (sections: Map<string, string>) =
    Map.tryFind name sections |> Option.map (fun value -> value.Trim())

let private parseInt description (text: string) =
    match Int32.TryParse(text.Trim()) with
    | true, value when value >= 0 -> Ok value
    | _ -> Error $"Invalid {description} '{text.Trim()}' (expected non-negative integer)"

let private tokens (text: string) =
    text.Split([| ' '; '\t'; '\n'; '\r'; ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let private parseVertices (text: string) =
    if text.Trim().Equals("none", StringComparison.OrdinalIgnoreCase) then Ok []
    else
        let rec loop parsed remaining =
            match remaining with
            | [] ->
                let result = List.rev parsed
                if List.distinct result = result then Ok result else Error "VERTICES contains a duplicate vertex"
            | token :: rest ->
                match parseInt "vertex" token with
                | Ok value -> loop (value :: parsed) rest
                | Error msg -> Error msg
        loop [] (tokens text)

let private parsePair (separator: string) (description: string) (token: string) =
    let parts = token.Split(separator, StringSplitOptions.None)
    if parts.Length <> 2 then Error $"Invalid {description} '{token}' (expected A{separator}B)"
    else
        match parseInt $"{description} left value" parts.[0], parseInt $"{description} right value" parts.[1] with
        | Ok left, Ok right -> Ok (left, right)
        | Error msg, _
        | _, Error msg -> Error msg

let private parsePairs (separator: string) (description: string) (text: string) =
    if text.Trim().Equals("none", StringComparison.OrdinalIgnoreCase) then Ok []
    else
        let rec loop parsed remaining =
            match remaining with
            | [] ->
                let result = List.rev parsed
                if List.distinct result = result then Ok result else Error $"{description} contains a duplicate pair"
            | token :: rest ->
                match parsePair separator description token with
                | Ok pair -> loop (pair :: parsed) rest
                | Error msg -> Error msg
        loop [] (tokens text)

let private parseOptionalPairs separator description sectionName (sections: Map<string, string>) =
    match optional sectionName sections with
    | None -> Ok []
    | Some text -> parsePairs separator description text

let private parseCount description (text: string) =
    let trimmed = text.Trim()
    let parseWith constructor (prefix: string) =
        parseInt description (trimmed.Substring(prefix.Length).Trim()) |> Result.map constructor
    if trimmed.StartsWith("<=") then parseWith AtMost "<="
    elif trimmed.StartsWith(">=") then parseWith AtLeast ">="
    else parseInt description trimmed |> Result.map Exactly

let private parseOptionalCount description sectionName (sections: Map<string, string>) =
    match optional sectionName sections with
    | None -> Ok None
    | Some text -> parseCount description text |> Result.map Some

let private pairVertices (pairs: (int * int) list) =
    pairs |> List.collect (fun (left, right) -> [ left; right ])

let private validateKnownVertices vertices description (pairs: (int * int) list) =
    let known = Set.ofList vertices
    match pairVertices pairs |> List.tryFind (fun vertex -> not (Set.contains vertex known)) with
    | Some vertex -> Error $"{description} references unknown vertex {vertex}"
    | None -> Ok ()

let private validateKnownFirstVertices vertices description (pairs: (int * int) list) =
    let known = Set.ofList vertices
    match pairs |> List.tryFind (fun (vertex, _) -> not (Set.contains vertex known)) with
    | Some (vertex, _) -> Error $"{description} references unknown vertex {vertex}"
    | None -> Ok ()

let private parseCase path sections =
    toSectionMap sections
    |> Result.bind (fun values ->
        match required "NAME" values, required "VERTICES" values, required "AVAILABLE-COLORS" values with
        | Error msg, _, _
        | _, Error msg, _
        | _, _, Error msg -> Error msg
        | Ok name, Ok verticesText, Ok colorsText ->
            match parseVertices verticesText, parseInt "available color count" colorsText with
            | Error msg, _
            | _, Error msg -> Error msg
            | Ok vertices, Ok availableColors ->
                let edgesResult = parseOptionalPairs "-" "EDGES" "EDGES" values
                let precoloredResult = parseOptionalPairs "=" "PRECOLORED" "PRECOLORED" values
                let preferResult = parseOptionalPairs "-" "PREFER" "PREFER" values
                let moveResult = parseOptionalPairs "-" "MOVE-PREFER" "MOVE-PREFER" values
                let colorsResult = parseOptionalPairs "=" "EXPECT-COLORS" "EXPECT-COLORS" values
                let sameResult = parseOptionalPairs "-" "EXPECT-SAME" "EXPECT-SAME" values
                let differentResult = parseOptionalPairs "-" "EXPECT-DIFFERENT" "EXPECT-DIFFERENT" values
                let chromaticResult = parseOptionalCount "chromatic expectation" "EXPECT-CHROMATIC" values
                let spillsResult = parseOptionalCount "spill expectation" "EXPECT-SPILLS" values
                let coloredResult = parseOptionalCount "colored expectation" "EXPECT-COLORED" values
                let selectionResult =
                    match optional "EXPECT-SELECTION-CHECKS" values with
                    | None -> Ok None
                    | Some text -> parseInt "selection check expectation" text |> Result.map Some
                let mcsResult =
                    match optional "EXPECT-MCS-ORDERING" values with
                    | None -> Ok false
                    | Some text when text.Equals("all", StringComparison.OrdinalIgnoreCase) -> Ok true
                    | Some text -> Error $"Invalid EXPECT-MCS-ORDERING '{text}' (expected 'all')"

                match edgesResult, precoloredResult, preferResult, moveResult,
                      colorsResult, sameResult, differentResult, chromaticResult,
                      spillsResult, coloredResult, selectionResult, mcsResult with
                | Error msg, _, _, _, _, _, _, _, _, _, _, _
                | _, Error msg, _, _, _, _, _, _, _, _, _, _
                | _, _, Error msg, _, _, _, _, _, _, _, _, _
                | _, _, _, Error msg, _, _, _, _, _, _, _, _
                | _, _, _, _, Error msg, _, _, _, _, _, _, _
                | _, _, _, _, _, Error msg, _, _, _, _, _, _
                | _, _, _, _, _, _, Error msg, _, _, _, _, _
                | _, _, _, _, _, _, _, Error msg, _, _, _, _
                | _, _, _, _, _, _, _, _, Error msg, _, _, _
                | _, _, _, _, _, _, _, _, _, Error msg, _, _
                | _, _, _, _, _, _, _, _, _, _, Error msg, _
                | _, _, _, _, _, _, _, _, _, _, _, Error msg -> Error msg
                | Ok edges, Ok precolored, Ok preferences, Ok movePairs,
                  Ok expectedColors, Ok expectedSame, Ok expectedDifferent, Ok expectedChromatic,
                  Ok expectedSpills, Ok expectedColored, Ok expectedSelectionChecks, Ok expectMcsCoversAll ->
                    let vertexPairs = [
                        "EDGES", edges
                        "PREFER", preferences
                        "MOVE-PREFER", movePairs
                        "EXPECT-SAME", expectedSame
                        "EXPECT-DIFFERENT", expectedDifferent
                    ]
                    let firstVertexPairs = [
                        "PRECOLORED", precolored
                        "EXPECT-COLORS", expectedColors
                    ]
                    let rec validateBoth remaining =
                        match remaining with
                        | [] -> Ok ()
                        | (description, pairs) :: rest ->
                            match validateKnownVertices vertices description pairs with
                            | Ok () -> validateBoth rest
                            | Error msg -> Error msg
                    let rec validateFirst remaining =
                        match remaining with
                        | [] -> Ok ()
                        | (description, pairs) :: rest ->
                            match validateKnownFirstVertices vertices description pairs with
                            | Ok () -> validateFirst rest
                            | Error msg -> Error msg
                    validateBoth vertexPairs
                    |> Result.bind (fun () -> validateFirst firstVertexPairs)
                    |> Result.bind (fun () ->
                        let hasExpectation =
                            expectedChromatic.IsSome
                            || expectedSpills.IsSome
                            || expectedColored.IsSome
                            || not (List.isEmpty expectedColors)
                            || not (List.isEmpty expectedSame)
                            || not (List.isEmpty expectedDifferent)
                            || expectMcsCoversAll
                            || expectedSelectionChecks.IsSome
                        if not hasExpectation then Error "Graph-color case requires at least one EXPECT section"
                        elif availableColors = 0 && not (List.isEmpty vertices) then
                            Error "AVAILABLE-COLORS must be positive for a non-empty graph"
                        elif precolored |> List.exists (fun (_, color) -> color >= availableColors) then
                            Error "PRECOLORED contains a color outside AVAILABLE-COLORS"
                        else
                            Ok {
                                Name = name
                                Vertices = vertices
                                Edges = edges
                                AvailableColors = availableColors
                                Precolored = precolored
                                PreferencePairs = preferences
                                MovePairs = movePairs
                                ExpectedChromatic = expectedChromatic
                                ExpectedSpills = expectedSpills
                                ExpectedColored = expectedColored
                                ExpectedColors = expectedColors
                                ExpectedSame = expectedSame
                                ExpectedDifferent = expectedDifferent
                                ExpectMcsCoversAll = expectMcsCoversAll
                                ExpectedSelectionChecks = expectedSelectionChecks
                                SourceFile = path
                            })
    )

let parseGraphColorFileContent path content =
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then Error "Graph-color fixture contains no sections"
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
