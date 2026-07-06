// OptimizationFormat.fs - Parser for optimization test files
//
// Parses test files that verify IR optimizations work correctly.
// Each test contains source code and expected IR output at a specific stage.
//
// Format:
//   ---NAME---
//   test_name
//   ---INPUT---
//   source code
//   ---EXPECTED---
//   exact IR output

module TestDSL.OptimizationFormat

open System
open TestDSL.Common

/// Stage of IR to verify
type IRStage =
    | ANF      // After ANF optimization
    | MIR      // After MIR optimization (SSA-based)
    | LIR      // After LIR peephole optimization

/// Optimization test specification
type OptimizationTest = {
    Name: string
    Source: string
    ExpectedIR: string
    Stage: IRStage
    SourceFile: string
}

type private ParseState = {
    Tests: OptimizationTest list
    Sections: Map<string, string>
    CurrentSection: string option
    CurrentContent: string list
    Errors: string list
}

/// Parse a single test from sections
let parseTest (stage: IRStage) (filePath: string) (sections: Map<string, string>) : Result<OptimizationTest, string> =
    match Map.tryFind "NAME" sections, Map.tryFind "INPUT" sections, Map.tryFind "EXPECTED" sections with
    | Some name, Some input, Some expected ->
        Ok {
            Name = name.Trim()
            Source = input.Trim()
            ExpectedIR = expected.Trim()
            Stage = stage
            SourceFile = filePath
        }
    | None, _, _ -> Error "Missing NAME section"
    | _, None, _ -> Error "Missing INPUT section"
    | _, _, None -> Error "Missing EXPECTED section"

/// Parse multiple tests from a single file
/// Tests are separated by ---NAME--- sections
let parseTestFile (stage: IRStage) (path: string) : Result<OptimizationTest list, string> =
    if not (System.IO.File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = System.IO.File.ReadAllText(path)
        let normalizedContent =
            content.Replace("\r\n", "\n").Replace("\r", "\n")

        let saveCurrentSection (state: ParseState) : ParseState =
            match state.CurrentSection with
            | None -> state
            | Some sectionName ->
                { state with
                    Sections =
                        Map.add
                            sectionName
                            (state.CurrentContent |> List.rev |> String.concat "\n")
                            state.Sections
                    CurrentContent = [] }

        let parseCompletedTest (state: ParseState) : ParseState =
            if Map.isEmpty state.Sections then
                state
            else
                match parseTest stage path state.Sections with
                | Ok test ->
                    { state with
                        Tests = test :: state.Tests
                        Sections = Map.empty }
                | Error e ->
                    { state with
                        Sections = Map.empty
                        Errors = e :: state.Errors }

        let startSection (sectionName: string) (state: ParseState) : ParseState =
            let stateWithSavedSection = saveCurrentSection state
            let stateForNewSection =
                if sectionName = "NAME" then
                    parseCompletedTest stateWithSavedSection
                else
                    stateWithSavedSection

            { stateForNewSection with
                CurrentSection = Some sectionName
                CurrentContent = [] }

        let appendContentLine (line: string) (state: ParseState) : ParseState =
            { state with CurrentContent = line :: state.CurrentContent }

        let parseLine (state: ParseState) (line: string) : ParseState =
            if line.StartsWith("---", StringComparison.Ordinal) && line.EndsWith("---", StringComparison.Ordinal) && line.Length > 6 then
                let sectionName = line.Substring(3, line.Length - 6)
                startSection sectionName state
            else
                appendContentLine line state

        let initialState = {
            Tests = []
            Sections = Map.empty
            CurrentSection = None
            CurrentContent = []
            Errors = []
        }

        let finalState =
            normalizedContent.Split([|'\n'|], StringSplitOptions.None)
            |> Array.fold parseLine initialState
            |> saveCurrentSection
            |> parseCompletedTest

        if finalState.Errors.Length > 0 then
            Error (String.concat "; " (List.rev finalState.Errors))
        else
            Ok (List.rev finalState.Tests)
