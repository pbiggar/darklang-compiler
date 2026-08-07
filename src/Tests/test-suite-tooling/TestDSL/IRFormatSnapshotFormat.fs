// IRFormatSnapshotFormat.fs - Parser for exact IR formatter snapshot fixtures.
//
// Parses the existing compact ANF, MIR, and LIR test syntaxes into typed formatter inputs.

module TestDSL.IRFormatSnapshotFormat

open System
open TestDSL.Common

type IRFormatInput =
    | ANFInput of ANF.Program
    | MIRInput of MIR.Program
    | LIRInput of LIR.Program

type IRFormatSnapshotTest = {
    Name: string
    Input: IRFormatInput
    Expected: string
    SourceFile: string
}

let private knownSections = Set.ofList [ "NAME"; "IR"; "INPUT"; "EXPECTED" ]

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
            | [] -> Error $"IR-format case must start with NAME, found {fst section}"
            | _ -> loop completed (section :: current) rest
    loop [] [] sections

let private toSectionMap sections =
    match sections |> List.tryFind (fun (name, _) -> not (Set.contains name knownSections)) with
    | Some (name, _) -> Error $"Unknown IR-format section: {name}"
    | None ->
        match sections |> List.countBy fst |> List.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> Error $"Duplicate IR-format section: {name}"
        | None -> Ok (Map.ofList sections)

let private required name (sections: Map<string, string>) =
    match Map.tryFind name sections with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Ok (value.Trim())
    | Some _ -> Error $"IR-format section {name} cannot be empty"
    | None -> Error $"Missing required IR-format section: {name}"

let private parseInput (kind: string) (source: string) =
    match kind.Trim().ToLowerInvariant() with
    | "anf" -> TestDSL.ANFParser.parseANF source |> Result.map ANFInput
    | "mir" -> TestDSL.MIRParser.parseMIR source |> Result.map MIRInput
    | "lir" -> TestDSL.LIRParser.parseLIR source |> Result.map LIRInput
    | value -> Error $"Unknown IR kind '{value}' (expected anf, mir, or lir)"

let private parseCase path sections =
    toSectionMap sections
    |> Result.bind (fun values ->
        match required "NAME" values, required "IR" values, required "INPUT" values, required "EXPECTED" values with
        | Error msg, _, _, _
        | _, Error msg, _, _
        | _, _, Error msg, _
        | _, _, _, Error msg -> Error msg
        | Ok name, Ok kind, Ok source, Ok expected ->
            parseInput kind source
            |> Result.mapError (fun msg -> $"Failed to parse {kind.Trim()} INPUT: {msg}")
            |> Result.map (fun input ->
                { Name = name
                  Input = input
                  Expected = normalizeLineEndings expected
                  SourceFile = path }))

let parseIRFormatSnapshotFileContent path content =
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then Error "IR-format fixture contains no sections"
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
