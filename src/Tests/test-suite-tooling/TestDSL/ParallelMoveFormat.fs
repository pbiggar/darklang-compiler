// ParallelMoveFormat.fs - Parser for parallel-move lowering fixtures.
//
// Converts compact destination/operand pairs and expected symbolic ARM64 into typed cases.

module TestDSL.ParallelMoveFormat

open System
open System.Text.RegularExpressions
open LIR
open TestDSL.Common
open TestDSL.LIRParser
open TestDSL.ARM64SymbolicParser

type ParallelMoveTest = {
    Name: string
    Moves: (PhysReg * Operand) list
    Expected: ARM64Symbolic.Instr list
    SourceFile: string
}

let private knownSections = Set.ofList [ "NAME"; "INPUT-MOVES"; "OUTPUT-ARM64" ]

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
            | [] -> Error $"Parallel-move case must start with NAME, found {fst section}"
            | _ -> loop completed (section :: current) rest
    loop [] [] sections

let private toSectionMap sections =
    match sections |> List.tryFind (fun (name, _) -> not (Set.contains name knownSections)) with
    | Some (name, _) -> Error $"Unknown parallel-move section: {name}"
    | None ->
        match sections |> List.countBy fst |> List.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> Error $"Duplicate parallel-move section: {name}"
        | None -> Ok (Map.ofList sections)

let private required name (sections: Map<string, string>) =
    match Map.tryFind name sections with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Ok (value.Trim())
    | Some _ -> Error $"Parallel-move section {name} cannot be empty"
    | None -> Error $"Missing required parallel-move section: {name}"

let private parseMove lineNumber (line: string) =
    let matched = Regex.Match(line.Trim(), @"^(.+?)\s*<-\s*(.+)$")
    if not matched.Success then Error $"Line {lineNumber}: invalid move '{line}' (expected DEST <- OPERAND)"
    else
        match parsePhysReg matched.Groups.[1].Value, parseOperand matched.Groups.[2].Value with
        | Ok destination, Ok operand -> Ok (destination, operand)
        | Error msg, _
        | _, Error msg -> Error $"Line {lineNumber}: {msg}"

let private parseMoves text =
    let lines = stripCommentsAndEmpty text
    let rec loop parsed remaining =
        match remaining with
        | [] -> Ok (List.rev parsed)
        | (lineNumber, line) :: rest ->
            match parseMove lineNumber line with
            | Ok move -> loop (move :: parsed) rest
            | Error msg -> Error msg
    match lines with
    | [] -> Error "INPUT-MOVES requires at least one move"
    | _ -> lines |> List.mapi (fun index line -> index + 1, line) |> loop []

let private parseCase path sections =
    toSectionMap sections
    |> Result.bind (fun values ->
        match required "NAME" values, required "INPUT-MOVES" values, required "OUTPUT-ARM64" values with
        | Error msg, _, _
        | _, Error msg, _
        | _, _, Error msg -> Error msg
        | Ok name, Ok input, Ok output ->
            let expectedResult =
                if output.Equals("none", StringComparison.OrdinalIgnoreCase) then Ok []
                else parseARM64Symbolic output
            match parseMoves input, expectedResult with
            | Error msg, _ -> Error $"Failed to parse INPUT-MOVES: {msg}"
            | _, Error msg -> Error $"Failed to parse OUTPUT-ARM64: {msg}"
            | Ok moves, Ok expected ->
                Ok { Name = name; Moves = moves; Expected = expected; SourceFile = path })

let parseParallelMoveFileContent path content =
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then Error "Parallel-move fixture contains no sections"
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
