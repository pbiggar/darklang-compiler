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

        // Split into individual tests by finding ---NAME--- markers
        let lines = content.Split([|'\n'|], StringSplitOptions.None)
        let mutable tests = []
        let mutable currentSections = Map.empty<string, string>
        let mutable currentSection = ""
        let mutable currentContent = ResizeArray<string>()
        let mutable errors = []

        for line in lines do
            if line.StartsWith("---") && line.EndsWith("---") && line.Length > 6 then
                let sectionName = line.Substring(3, line.Length - 6)

                // Save previous section content
                if currentSection <> "" then
                    currentSections <- Map.add currentSection (String.concat "\n" currentContent) currentSections
                    currentContent.Clear()

                // If this is a NAME section and we already have sections, parse the previous test
                if sectionName = "NAME" && not (Map.isEmpty currentSections) then
                    match parseTest stage path currentSections with
                    | Ok test -> tests <- test :: tests
                    | Error e -> errors <- e :: errors
                    currentSections <- Map.empty

                currentSection <- sectionName
            else
                currentContent.Add(line)

        // Save last section and parse last test
        if currentSection <> "" then
            currentSections <- Map.add currentSection (String.concat "\n" currentContent) currentSections

        if not (Map.isEmpty currentSections) then
            match parseTest stage path currentSections with
            | Ok test -> tests <- test :: tests
            | Error e -> errors <- e :: errors

        if errors.Length > 0 then
            Error (String.concat "; " (List.rev errors))
        else
            Ok (List.rev tests)
