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
    /// If true, expect the compiler to fail (type error, parse error, etc.)
    ExpectCompileError: bool
    /// Expected error message (substring match) when ExpectCompileError is true
    ExpectedErrorMessage: string option
    /// Compiler options for disabling optimizations
    DisableFreeList: bool
    DisableANFOpt: bool
    DisableTCO: bool
    DisableMIROpt: bool
    DisableLIROpt: bool
    DisableDCE: bool
    /// Source file this test came from (for grouping in output)
    SourceFile: string
}

/// Optimization flags for test parsing (internal type)
type private OptFlags = {
    DisableFreeList: bool
    DisableANFOpt: bool
    DisableTCO: bool
    DisableMIROpt: bool
    DisableLIROpt: bool
    DisableDCE: bool
}

let private defaultOptFlags = {
    DisableFreeList = false
    DisableANFOpt = false
    DisableTCO = false
    DisableMIROpt = false
    DisableLIROpt = false
    DisableDCE = false
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

/// Find the = that separates source from expectations
/// Returns Some index if this looks like a test line, None otherwise
let private findSeparatorIndex (s: string) : int option =
    let isExpectationStart (rest: string) : bool =
        let trimmed = rest.TrimStart()
        if trimmed.Length = 0 then false
        elif Char.IsDigit(trimmed.[0]) || trimmed.[0] = '-' then true
        elif trimmed.StartsWith("exit") || trimmed.StartsWith("stdout") || trimmed.StartsWith("stderr") || trimmed.StartsWith("no_free_list") || trimmed.StartsWith("error") || trimmed.StartsWith("disable_opt_") then true
        else false

    let rec findLast (i: int) (inQuotes: bool) (lastEqualsIdx: int option) : int option =
        if i >= s.Length then
            lastEqualsIdx
        elif s.[i] = '"' then
            findLast (i + 1) (not inQuotes) lastEqualsIdx
        elif s.[i] = '=' && not inQuotes then
            // Only consider this = as a separator if preceded by whitespace or )
            // This distinguishes "source = expectations" from "exit=139"
            let hasPrecedingSpace = i > 0 && (Char.IsWhiteSpace(s.[i - 1]) || s.[i - 1] = ')')
            if hasPrecedingSpace then
                // Check if this = is followed by an expectation
                let rest = s.Substring(i + 1)
                if isExpectationStart rest then
                    findLast (i + 1) inQuotes (Some i)
                else
                    findLast (i + 1) inQuotes lastEqualsIdx
            else
                findLast (i + 1) inQuotes lastEqualsIdx
        else
            findLast (i + 1) inQuotes lastEqualsIdx
    findLast 0 false None

/// Check if a line (with comment removed) looks like a test line
let private isTestLine (lineWithoutComment: string) : bool =
    findSeparatorIndex lineWithoutComment |> Option.isSome

/// Parse a single test line with optional preamble to prepend
/// Supports two formats:
///   Old: source = exit_code  // comment
///   New: source = [exit=N] [stdout="..."] [stderr="..."]  // comment
let private parseTestLineWithPreamble (line: string) (lineNumber: int) (filePath: string) (preamble: string) : Result<E2ETest, string> =
    // First, remove any comment
    let lineWithoutComment, comment =
        let commentIdx = line.IndexOf("//")
        if commentIdx >= 0 then
            (line.Substring(0, commentIdx).Trim(),
             Some (line.Substring(commentIdx + 2).Trim()))
        else
            (line, None)

    match findSeparatorIndex lineWithoutComment with
    | None ->
        Error $"Line {lineNumber}: Expected format 'source = expectations', got: {line}"
    | Some equalsIdx ->
        let testExpr = lineWithoutComment.Substring(0, equalsIdx).Trim()
        // Prepend preamble (definitions) to the test expression
        let source =
            if preamble.Length > 0 then preamble + "\n" + testExpr
            else testExpr
        let expectationsStr = lineWithoutComment.Substring(equalsIdx + 1).Trim()

        // Parse expectations - either old format (bare number), new format (attributes), or error
        // Returns: (exitCode, stdout, stderr, optFlags, expectError, errorMessage)
        let parseExpectations (exp: string) : Result<int * string option * string option * OptFlags * bool * string option, string> =
            let trimmed = exp.Trim()

            // Check for "error" keyword (compiler error expected)
            // Supports: error  or  error="message"
            if trimmed.ToLower() = "error" then
                // Expect compilation to fail with exit code 1, no specific message
                Ok (1, None, None, defaultOptFlags, true, None)
            elif trimmed.ToLower().StartsWith("error=") then
                // error="message" format
                let msgPart = trimmed.Substring(6)  // Skip "error="
                match parseStringLiteral msgPart with
                | Ok msg -> Ok (1, None, None, defaultOptFlags, true, Some msg)
                | Error e -> Error $"Invalid error message: {e}"
            // Check if old format (starts with digit or negative sign followed by digit)
            elif trimmed.Length > 0 && Char.IsDigit(trimmed.[0]) then
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
                        Ok (0, Some $"{value}\n", None, defaultOptFlags, false, None)
                    else
                        // Mixed format error
                        Error "Cannot mix bare number with attributes. Use explicit attributes instead."
                | false, _ ->
                    Error $"Invalid value: {exitCodeStr}"
            elif trimmed.Length > 1 && trimmed.[0] = '-' && Char.IsDigit(trimmed.[1]) then
                // Negative number
                let spaceIdx = trimmed.IndexOf(' ')
                let exitCodeStr =
                    if spaceIdx >= 0 then trimmed.Substring(0, spaceIdx)
                    else trimmed

                match Int64.TryParse(exitCodeStr) with
                | true, value ->
                    let remaining =
                        if spaceIdx >= 0 then trimmed.Substring(spaceIdx + 1).Trim()
                        else ""

                    if remaining.Length = 0 then
                        Ok (0, Some $"{value}\n", None, defaultOptFlags, false, None)
                    else
                        Error "Cannot mix bare number with attributes. Use explicit attributes instead."
                | false, _ ->
                    Error $"Invalid value: {exitCodeStr}"
            else
                // New format: parse attributes
                let mutable exitCode = 0  // default
                let mutable stdout = None
                let mutable stderr = None
                let mutable optFlags = defaultOptFlags
                let mutable expectError = false
                let mutable errors = []

                // Helper to parse boolean attribute value
                let parseBool (value: string) (attrName: string) : bool option =
                    match value.ToLower() with
                    | "true" | "1" -> Some true
                    | "false" | "0" -> Some false
                    | _ ->
                        errors <- $"Invalid {attrName} value: {value} (expected true/false)" :: errors
                        None

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
                            | "no_free_list" ->
                                match parseBool value "no_free_list" with
                                | Some b -> optFlags <- { optFlags with DisableFreeList = b }
                                | None -> ()
                            | "disable_opt_freelist" ->
                                match parseBool value "disable_opt_freelist" with
                                | Some b -> optFlags <- { optFlags with DisableFreeList = b }
                                | None -> ()
                            | "disable_opt_anf" ->
                                match parseBool value "disable_opt_anf" with
                                | Some b -> optFlags <- { optFlags with DisableANFOpt = b }
                                | None -> ()
                            | "disable_opt_tco" ->
                                match parseBool value "disable_opt_tco" with
                                | Some b -> optFlags <- { optFlags with DisableTCO = b }
                                | None -> ()
                            | "disable_opt_mir" ->
                                match parseBool value "disable_opt_mir" with
                                | Some b -> optFlags <- { optFlags with DisableMIROpt = b }
                                | None -> ()
                            | "disable_opt_lir" ->
                                match parseBool value "disable_opt_lir" with
                                | Some b -> optFlags <- { optFlags with DisableLIROpt = b }
                                | None -> ()
                            | "disable_opt_dce" ->
                                match parseBool value "disable_opt_dce" with
                                | Some b -> optFlags <- { optFlags with DisableDCE = b }
                                | None -> ()
                            | _ -> errors <- $"Unknown attribute: {key}" :: errors
                        | Error e -> errors <- e :: errors

                if errors.Length > 0 then
                    Error (String.concat "; " (List.rev errors))
                else
                    Ok (exitCode, stdout, stderr, optFlags, expectError, None)

        match parseExpectations expectationsStr with
        | Ok (exitCode, stdout, stderr, optFlags, expectError, errorMessage) ->
            let displayName = comment |> Option.defaultValue source
            Ok {
                Name = $"L{lineNumber}: {displayName}"
                Source = source
                ExpectedStdout = stdout
                ExpectedStderr = stderr
                ExpectedExitCode = exitCode
                ExpectCompileError = expectError
                ExpectedErrorMessage = errorMessage
                DisableFreeList = optFlags.DisableFreeList
                DisableANFOpt = optFlags.DisableANFOpt
                DisableTCO = optFlags.DisableTCO
                DisableMIROpt = optFlags.DisableMIROpt
                DisableLIROpt = optFlags.DisableLIROpt
                DisableDCE = optFlags.DisableDCE
                SourceFile = filePath
            }
        | Error e ->
            Error $"Line {lineNumber}: {e}"

/// Remove trailing comment from a line
let private stripComment (line: string) : string =
    let commentIdx = line.IndexOf("//")
    if commentIdx >= 0 then line.Substring(0, commentIdx).Trim()
    else line.Trim()

/// Parse all E2E tests from a single file
/// Supports preamble definitions that are prepended to all tests.
/// Lines that don't match the test format (expr = expectation) are treated as
/// definitions and accumulated. Test lines get the accumulated preamble prepended.
let parseE2ETestFile (path: string) : Result<E2ETest list, string> =
    if not (System.IO.File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let lines = System.IO.File.ReadAllLines(path)

        let mutable tests = []
        let mutable errors = []
        let mutable preambleLines = []

        for i in 0 .. lines.Length - 1 do
            let line = lines.[i]
            let trimmedLine = line.Trim()
            let lineNumber = i + 1

            // Skip blank lines and comment-only lines
            if trimmedLine.Length > 0 && not (trimmedLine.StartsWith("//")) then
                let lineWithoutComment = stripComment trimmedLine
                if isTestLine lineWithoutComment then
                    // This is a test line - parse it with accumulated preamble
                    let preamble = String.concat "\n" (List.rev preambleLines)
                    match parseTestLineWithPreamble trimmedLine lineNumber path preamble with
                    | Ok test -> tests <- test :: tests
                    | Error err -> errors <- err :: errors
                else
                    // This is a definition line - add to preamble
                    // Preserve original indentation for multi-line definitions
                    preambleLines <- line :: preambleLines

        if errors.Length > 0 then
            Error (String.concat "\n" (List.rev errors))
        else
            Ok (List.rev tests)

/// Parse E2E test from old-format file (for backward compatibility during transition)
let parseE2ETest (path: string) : Result<E2ETest, string> =
    Error "Old format no longer supported - use parseE2ETestFile instead"
