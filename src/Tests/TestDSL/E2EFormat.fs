// E2EFormat.fs - End-to-end test format parser
//
// Parses E2E test files that specify source code and expected behavior.
//
// Example test:
//   ---NAME---
//   Simple addition
//
//   ---SOURCE---
//   2 + 3
//
//   ---EXIT-CODE---
//   5

module TestDSL.E2EFormat

open TestDSL.Common

/// End-to-end test specification
type E2ETest = {
    Name: string
    Source: string
    ExpectedStdout: string option
    ExpectedStderr: string option
    ExpectedExitCode: int
}

/// Parse E2E test from file
let parseE2ETest (path: string) : Result<E2ETest, string> =
    if not (System.IO.File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = System.IO.File.ReadAllText(path)
        let testFile = parseTestFile content

        // Get test name (optional, defaults to filename)
        let name =
            match getOptionalSection "NAME" testFile with
            | Some n -> n.Trim()
            | None -> System.IO.Path.GetFileNameWithoutExtension(path)

        // Get source code (required)
        match getRequiredSection "SOURCE" testFile with
        | Error e -> Error e
        | Ok source ->
            // Get optional stdout/stderr
            let stdout = getOptionalSection "STDOUT" testFile
            let stderr = getOptionalSection "STDERR" testFile

            // Get exit code (defaults to 0)
            let exitCode =
                match getOptionalSection "EXIT-CODE" testFile with
                | Some code ->
                    match System.Int32.TryParse(code.Trim()) with
                    | true, n -> n
                    | false, _ -> 0
                | None -> 0

            Ok {
                Name = name
                Source = source
                ExpectedStdout = stdout
                ExpectedStderr = stderr
                ExpectedExitCode = exitCode
            }
