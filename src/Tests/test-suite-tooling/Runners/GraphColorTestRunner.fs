// GraphColorTestRunner.fs - Executes graph-coloring fixtures.
//
// Checks externally meaningful coloring, spill, preference, and MCS properties.

module TestDSL.GraphColorTestRunner

open System.IO
open RegisterAllocation
open TestDSL.GraphColorFormat
open TestDSL.PassTestRunner

let private success =
    { Success = true; Message = "Test passed"; Expected = None; Actual = None }

let private failure message expected actual =
    { Success = false; Message = message; Expected = Some expected; Actual = Some actual }

let private countMatches expectation actual =
    match expectation with
    | Exactly expected -> actual = expected
    | AtMost expected -> actual <= expected
    | AtLeast expected -> actual >= expected

let private formatCount = function
    | Exactly value -> string value
    | AtMost value -> $"<= {value}"
    | AtLeast value -> $">= {value}"

let runGraphColorTest (test: GraphColorTest) : PassTestResult =
    let graph = buildInterferenceGraphFromEdges test.Vertices test.Edges
    let result = chordalGraphColor graph test.Precolored test.AvailableColors test.PreferencePairs test.MovePairs

    let checkCount label expectation actual =
        match expectation with
        | Some expected when not (countMatches expected actual) ->
            Some (failure $"{label} did not match" (formatCount expected) (string actual))
        | _ -> None

    let checkColors pairs relation label =
        pairs
        |> List.tryPick (fun (left, right) ->
            match colorOf result left, colorOf result right with
            | Some leftColor, Some rightColor when relation leftColor rightColor -> None
            | leftColor, rightColor ->
                Some (failure label $"{left} and {right}" $"{leftColor} and {rightColor}"))

    let explicitColorFailure =
        test.ExpectedColors
        |> List.tryPick (fun (vertex, expected) ->
            match colorOf result vertex with
            | Some actual when actual = expected -> None
            | actual -> Some (failure "Vertex color did not match" $"{vertex}={expected}" $"{vertex}={actual}"))

    let mcsFailure =
        if test.ExpectMcsCoversAll then
            let ordering = maximumCardinalitySearch graph
            if List.sort ordering = List.sort test.Vertices && List.length ordering = List.length test.Vertices then None
            else Some (failure "MCS ordering did not cover every vertex exactly once" $"{List.sort test.Vertices}" $"{ordering}")
        else None

    let selectionFailure =
        match test.ExpectedSelectionChecks with
        | None -> None
        | Some expected ->
            let _, profile = maximumCardinalitySearchWithProfile graph
            if profile.SelectionChecks = expected then None
            else Some (failure "MCS selection checks did not match" (string expected) (string profile.SelectionChecks))

    [ checkCount "Chromatic number" test.ExpectedChromatic result.ChromaticNumber
      checkCount "Spill count" test.ExpectedSpills (spillCount result)
      checkCount "Colored count" test.ExpectedColored (coloredCount result)
      explicitColorFailure
      checkColors test.ExpectedSame (=) "Expected vertices to have the same color"
      checkColors test.ExpectedDifferent (<>) "Expected vertices to have different colors"
      mcsFailure
      selectionFailure ]
    |> List.tryPick id
    |> Option.defaultValue success

let loadGraphColorTests path =
    if not (File.Exists path) then Error $"Graph-color test file not found: {path}"
    else
        try File.ReadAllText path |> parseGraphColorFileContent path
        with ex -> Error $"Failed to read graph-color test file {path}: {ex.Message}"

let tests (testFiles: string array) : (string * (unit -> Result<unit, string>)) list =
    let testsForFile path =
        match loadGraphColorTests path with
        | Error msg -> [ ($"parse {Path.GetFileName path}", fun () -> Error msg) ]
        | Ok cases ->
            cases
            |> List.map (fun test ->
                (test.Name,
                 fun () ->
                    let result = runGraphColorTest test
                    if result.Success then Ok ()
                    else Error $"{result.Message}\nExpected: {result.Expected}\nActual: {result.Actual}"))
    testFiles |> Array.sort |> Array.toList |> List.collect testsForFile
