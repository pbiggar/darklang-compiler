// TypeCheckingFormat.fs - Type checking test format parser
//
// Parses type checking test files in a simple line-based format.
//
// Format: expression : expected_type  // optional comment
//         expression : error          // type error expected
//
// Example:
//   // Integer literals
//   42 : int
//   2 + 3 : int
//
//   // Type errors
//   1 + true : error  // cannot add int and bool

module TestDSL.TypeCheckingFormat

open System

/// Type checking test expectation
type TypeExpectation =
    | ExpectType of AST.Type
    | ExpectError

/// Type checking test specification
type TypeCheckingTest = {
    Name: string
    Source: string
    Expectation: TypeExpectation
}

/// Parse type string to AST.Type
let private parseType (typeStr: string) : Result<AST.Type, string> =
    match typeStr.Trim().ToLower() with
    | "int" | "int64" -> Ok AST.TInt64
    | "bool" | "boolean" -> Ok AST.TBool
    | "float" | "float64" -> Ok AST.TFloat64
    | "string" -> Ok AST.TString
    | "unit" -> Ok AST.TUnit
    | "error" -> Error "Use 'error' keyword, not as a type"
    | other -> Error $"Unknown type: {other}"

/// Parse a single test line
/// Format: source : expectation  // comment
let private parseTestLine (line: string) (lineNumber: int) : Result<TypeCheckingTest, string> =
    // First, remove any comment
    let lineWithoutComment, comment =
        let commentIdx = line.IndexOf("//")
        if commentIdx >= 0 then
            (line.Substring(0, commentIdx).Trim(),
             Some (line.Substring(commentIdx + 2).Trim()))
        else
            (line, None)

    // Find the : that separates source from expectation
    // It's the last : that's not inside quotes
    let findSeparatorIndex (s: string) : int option =
        let rec findLast (i: int) (inQuotes: bool) (lastColonIdx: int option) : int option =
            if i >= s.Length then
                lastColonIdx
            elif s.[i] = '"' then
                findLast (i + 1) (not inQuotes) lastColonIdx
            elif s.[i] = ':' && not inQuotes then
                findLast (i + 1) inQuotes (Some i)
            else
                findLast (i + 1) inQuotes lastColonIdx
        findLast 0 false None

    match findSeparatorIndex lineWithoutComment with
    | None ->
        Error $"Line {lineNumber}: Expected format 'source : expectation', got: {line}"
    | Some colonIdx ->
        let source = lineWithoutComment.Substring(0, colonIdx).Trim()
        let expectationStr = lineWithoutComment.Substring(colonIdx + 1).Trim()

        // Parse expectation
        let expectation =
            if expectationStr.ToLower() = "error" then
                Ok ExpectError
            else
                match parseType expectationStr with
                | Ok typ -> Ok (ExpectType typ)
                | Error e -> Error e

        match expectation with
        | Ok exp ->
            Ok {
                Name = comment |> Option.defaultValue $"Line {lineNumber}: {source}"
                Source = source
                Expectation = exp
            }
        | Error e ->
            Error $"Line {lineNumber}: {e}"

/// Parse all type checking tests from a single file
let parseTypeCheckingTestFile (path: string) : Result<TypeCheckingTest list, string> =
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
