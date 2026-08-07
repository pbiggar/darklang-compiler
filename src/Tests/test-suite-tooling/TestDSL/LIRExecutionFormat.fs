// LIRExecutionFormat.fs - Parser for executable single-block LIR fixtures.
//
// Keeps successful process results and expected codegen failures typed.

module TestDSL.LIRExecutionFormat

open System
open TestDSL.Common

type LeakCheckMode =
    | LeakCheckDisabled
    | LeakCheckEnabled

type ProcessExpectation =
    | ExpectedExitCode of int
    | ExpectedStdout of string
    | ExpectedStderr of string

type LIRExecutionExpectation =
    | ExpectedProcessResult of ProcessExpectation list
    | ExpectedCodegenError of string

type LIRExecutionTest = {
    Name: string
    Program: LIR.Program
    LeakCheck: LeakCheckMode
    Expectation: LIRExecutionExpectation
    SourceFile: string
}

let private knownSections =
    Set.ofList
        [ "NAME"
          "INPUT-LIR"
          "LEAK-CHECK"
          "EXPECT-EXIT"
          "EXPECT-STDOUT"
          "EXPECT-STDERR"
          "EXPECT-CODEGEN-ERROR" ]

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
            | [] -> Error $"LIR-execution case must start with NAME, found {fst section}"
            | _ -> loop completed (section :: current) rest

    loop [] [] sections

let private toSectionMap sections =
    match sections |> List.tryFind (fun (name, _) -> not (Set.contains name knownSections)) with
    | Some (name, _) -> Error $"Unknown LIR-execution section: {name}"
    | None ->
        match sections |> List.countBy fst |> List.tryFind (fun (_, count) -> count > 1) with
        | Some (name, _) -> Error $"Duplicate LIR-execution section: {name}"
        | None -> Ok (Map.ofList sections)

let private required name (sections: Map<string, string>) =
    match Map.tryFind name sections with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Ok (value.Trim())
    | Some _ -> Error $"LIR-execution section {name} cannot be empty"
    | None -> Error $"Missing required LIR-execution section: {name}"

let private parseLeakCheck (sections: Map<string, string>) =
    match Map.tryFind "LEAK-CHECK" sections |> Option.map (fun value -> value.Trim().ToLowerInvariant()) with
    | None
    | Some "false" -> Ok LeakCheckDisabled
    | Some "true" -> Ok LeakCheckEnabled
    | Some value -> Error $"Invalid LEAK-CHECK value '{value}' (expected true or false)"

let private parseExitExpectation (sections: Map<string, string>) =
    match Map.tryFind "EXPECT-EXIT" sections with
    | None -> Ok None
    | Some value ->
        match Int32.TryParse(value.Trim()) with
        | true, exitCode -> Ok (Some (ExpectedExitCode exitCode))
        | false, _ -> Error $"Invalid EXPECT-EXIT value '{value.Trim()}' (expected 32-bit integer)"

let private outputExpectation section constructor (sections: Map<string, string>) =
    Map.tryFind section sections
    |> Option.map (fun value -> value.Trim() |> normalizeLineEndings |> constructor)

let private parseCase path sections =
    toSectionMap sections
    |> Result.bind (fun values ->
        match required "NAME" values, required "INPUT-LIR" values, parseLeakCheck values, parseExitExpectation values with
        | Error msg, _, _, _
        | _, Error msg, _, _
        | _, _, Error msg, _
        | _, _, _, Error msg -> Error msg
        | Ok name, Ok source, Ok leakCheck, Ok exitExpectation ->
            let processExpectations =
                [ exitExpectation
                  outputExpectation "EXPECT-STDOUT" ExpectedStdout values
                  outputExpectation "EXPECT-STDERR" ExpectedStderr values ]
                |> List.choose id
            let codegenError = Map.tryFind "EXPECT-CODEGEN-ERROR" values |> Option.map (fun value -> value.Trim())
            let expectationResult =
                match codegenError, processExpectations with
                | Some "", _ -> Error "EXPECT-CODEGEN-ERROR cannot be empty"
                | Some _, _ :: _ -> Error "EXPECT-CODEGEN-ERROR cannot be combined with process expectations"
                | Some expected, [] -> Ok (ExpectedCodegenError expected)
                | None, [] -> Error "LIR-execution case requires at least one expectation"
                | None, expected -> Ok (ExpectedProcessResult expected)

            expectationResult
            |> Result.bind (fun expectation ->
                TestDSL.LIRParser.parseLIR source
                |> Result.mapError (fun msg -> $"Failed to parse INPUT-LIR: {msg}")
                |> Result.map (fun program ->
                    { Name = name
                      Program = program
                      LeakCheck = leakCheck
                      Expectation = expectation
                      SourceFile = path })))

let parseLIRExecutionFileContent path content =
    let sections = parseSections (normalizeLineEndings content)
    if List.isEmpty sections then Error "LIR-execution fixture contains no sections"
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
