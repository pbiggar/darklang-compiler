// E2EFormat.fs - End-to-end test format parser
//
// Parses E2E test files in a simple line-based format.
//
// Format: source = expected_exit_code  // optional comment
//
// Example:
//   // Addition tests
//   2 + 3 = 5
//   1 + 2 + 3 = 10  // left associativity

module TestDSL.E2EFormat

open System

/// End-to-end test specification
type E2ETest = {
    Name: string
    Source: string
    ExpectedStdout: string option
    ExpectedStderr: string option
    ExpectedExitCode: int
}

/// Parse a single test line in format: source = expected_exit_code  // optional comment
let private parseTestLine (line: string) (lineNumber: int) : Result<E2ETest, string> =
    // Split by = to get source and rest
    let parts = line.Split([|'='|], 2)
    if parts.Length <> 2 then
        Error $"Line {lineNumber}: Expected format 'source = exit_code', got: {line}"
    else
        let source = parts.[0].Trim()
        let restOfLine = parts.[1].Trim()

        // Split rest by // to separate exit code from comment
        let exitCodeAndComment =
            let commentIdx = restOfLine.IndexOf("//")
            if commentIdx >= 0 then
                (restOfLine.Substring(0, commentIdx).Trim(), Some (restOfLine.Substring(commentIdx + 2).Trim()))
            else
                (restOfLine, None)

        let exitCodeStr, comment = exitCodeAndComment

        // Parse exit code
        match Int32.TryParse(exitCodeStr) with
        | true, exitCode ->
            Ok {
                Name = comment |> Option.defaultValue $"Line {lineNumber}: {source}"
                Source = source
                ExpectedStdout = None
                ExpectedStderr = None
                ExpectedExitCode = exitCode
            }
        | false, _ ->
            Error $"Line {lineNumber}: Invalid exit code '{exitCodeStr}'"

/// Parse all E2E tests from a single file
let parseE2ETestFile (path: string) : Result<E2ETest list, string> =
    if not (System.IO.File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let lines = System.IO.File.ReadAllLines(path)

        let mutable tests = []
        let mutable errors = []

        for i in 0 .. lines.Length - 1 do
            let line = lines.[i].Trim()
            let lineNumber = i + 1

            // Skip blank lines and comments
            if line.Length > 0 && not (line.StartsWith("//")) then
                match parseTestLine line lineNumber with
                | Ok test -> tests <- test :: tests
                | Error err -> errors <- err :: errors

        if errors.Length > 0 then
            Error (String.concat "\n" (List.rev errors))
        else
            Ok (List.rev tests)

/// Parse E2E test from old-format file (for backward compatibility during transition)
let parseE2ETest (path: string) : Result<E2ETest, string> =
    Error "Old format no longer supported - use parseE2ETestFile instead"
