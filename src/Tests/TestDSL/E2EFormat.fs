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

/// Parse string literal with escape sequences (\n, \t, \\, \")
let private parseStringLiteral (s: string) : Result<string, string> =
    if not (s.StartsWith("\"") && s.EndsWith("\"")) then
        Error $"String literal must be quoted: {s}"
    else
        let content = s.Substring(1, s.Length - 2)
        let result =
            content
                .Replace("\\n", "\n")
                .Replace("\\t", "\t")
                .Replace("\\\\", "\\")
                .Replace("\\\"", "\"")
        Ok result

/// Parse key=value attribute
let private parseAttribute (attr: string) : Result<string * string, string> =
    match attr.Split([|'='|], 2) with
    | [| key; value |] -> Ok (key.Trim(), value.Trim())
    | _ -> Error $"Invalid attribute format: {attr}"

/// Parse a single test line
/// Supports two formats:
///   Old: source = exit_code  // comment
///   New: source = [exit=N] [stdout="..."] [stderr="..."]  // comment
let private parseTestLine (line: string) (lineNumber: int) : Result<E2ETest, string> =
    // Split by = to get source and expectations
    let parts = line.Split([|'='|], 2)
    if parts.Length <> 2 then
        Error $"Line {lineNumber}: Expected format 'source = expectations', got: {line}"
    else
        let source = parts.[0].Trim()
        let restOfLine = parts.[1].Trim()

        // Split rest by // to separate expectations from comment
        let expectationsStr, comment =
            let commentIdx = restOfLine.IndexOf("//")
            if commentIdx >= 0 then
                (restOfLine.Substring(0, commentIdx).Trim(),
                 Some (restOfLine.Substring(commentIdx + 2).Trim()))
            else
                (restOfLine, None)

        // Parse expectations - either old format (bare number) or new format (attributes)
        let parseExpectations (exp: string) : Result<int * string option * string option, string> =
            let trimmed = exp.Trim()

            // Check if old format (starts with digit or negative sign followed by digit)
            let isSimpleFormat =
                if trimmed.Length > 0 && Char.IsDigit(trimmed.[0]) then
                    true
                elif trimmed.Length > 1 && trimmed.[0] = '-' && Char.IsDigit(trimmed.[1]) then
                    true
                else
                    false

            if isSimpleFormat then
                // Old format: bare exit code (possibly followed by attributes - which is an error)
                let spaceIdx = trimmed.IndexOf(' ')
                let exitCodeStr =
                    if spaceIdx >= 0 then trimmed.Substring(0, spaceIdx)
                    else trimmed

                match Int64.TryParse(exitCodeStr) with
                | true, value ->
                    // Check if there are attributes after the value
                    let remaining =
                        if spaceIdx >= 0 then trimmed.Substring(spaceIdx + 1).Trim()
                        else ""

                    if remaining.Length = 0 then
                        // Bare number format: expect stdout with exit=0
                        // e.g., "42 = 42" means stdout="42\n", exit=0
                        // The binary returns the value as exit code, but CompilerLibrary reports exit=0
                        Ok (0, Some $"{value}\n", None)
                    else
                        // Mixed format error
                        Error "Cannot mix bare number with attributes. Use explicit attributes instead."
                | false, _ ->
                    Error $"Invalid value: {exitCodeStr}"
            else
                // New format: parse attributes
                let mutable exitCode = 0  // default
                let mutable stdout = None
                let mutable stderr = None
                let mutable errors = []

                // Split by spaces, but respect quoted strings
                // Use regex to split on whitespace outside of quotes
                let attrRegex = System.Text.RegularExpressions.Regex(@"\s+(?=(?:[^""]*""[^""]*"")*[^""]*$)")
                let attrs = attrRegex.Split(trimmed)

                for attr in attrs do
                    let attrTrimmed = attr.Trim()
                    if attrTrimmed.Length > 0 then
                        match parseAttribute attrTrimmed with
                        | Ok (key, value) ->
                            match key with
                            | "exit" ->
                                match Int32.TryParse(value) with
                                | true, code -> exitCode <- code
                                | false, _ -> errors <- $"Invalid exit code: {value}" :: errors
                            | "stdout" ->
                                match parseStringLiteral value with
                                | Ok s -> stdout <- Some s
                                | Error e -> errors <- e :: errors
                            | "stderr" ->
                                match parseStringLiteral value with
                                | Ok s -> stderr <- Some s
                                | Error e -> errors <- e :: errors
                            | _ -> errors <- $"Unknown attribute: {key}" :: errors
                        | Error e -> errors <- e :: errors

                if errors.Length > 0 then
                    Error (String.concat "; " (List.rev errors))
                else
                    Ok (exitCode, stdout, stderr)

        match parseExpectations expectationsStr with
        | Ok (exitCode, stdout, stderr) ->
            Ok {
                Name = comment |> Option.defaultValue $"Line {lineNumber}: {source}"
                Source = source
                ExpectedStdout = stdout
                ExpectedStderr = stderr
                ExpectedExitCode = exitCode
            }
        | Error e ->
            Error $"Line {lineNumber}: {e}"

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
