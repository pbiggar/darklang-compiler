// StdlibSourceTests.fs - Source-level invariants for maintained stdlib files.
//
// These checks catch stdlib definitions that are easy to shadow accidentally
// before the compiler accepts a misleading or unreachable implementation.

module StdlibSourceTests

open System.IO

type TestResult = Result<unit, string>

let private repoRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", ".."))

let private readFile (path: string) : Result<string, string> =
    try Ok (File.ReadAllText(path))
    with ex -> Error $"Failed to read {path}: {ex.Message}"

let private stdlibFiles () : Result<string array, string> =
    let stdlibDir = Path.Combine(repoRoot, "src", "DarkCompiler", "stdlib")
    try
        Ok (Directory.GetFiles(stdlibDir, "*.dark", SearchOption.AllDirectories))
    with ex ->
        Error $"Failed to list stdlib files in {stdlibDir}: {ex.Message}"

let private definitionName (line: string) : string option =
    let trimmed = line.TrimStart()
    if trimmed.StartsWith("def ") then
        let afterDef = trimmed.Substring(4)
        let endIndex =
            afterDef
            |> Seq.tryFindIndex (fun ch -> ch = '<' || ch = '(' || System.Char.IsWhiteSpace(ch))
        match endIndex with
        | Some idx when idx > 0 -> Some (afterDef.Substring(0, idx))
        | _ -> None
    else
        None

let private duplicateDefinitionsInFile (path: string) : Result<string list, string> =
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        text.Split('\n')
        |> Array.choose definitionName
        |> Array.countBy id
        |> Array.choose (fun (name, count) ->
            if count > 1 then Some $"{Path.GetRelativePath(repoRoot, path)}:{name}" else None)
        |> Array.toList
        |> Ok

let private findDuplicateDefinitions () : Result<string list, string> =
    let folder (state: Result<string list, string>) (path: string) =
        match state with
        | Error _ -> state
        | Ok acc ->
            match duplicateDefinitionsInFile path with
            | Error msg -> Error msg
            | Ok duplicates -> Ok (acc @ duplicates)
    match stdlibFiles () with
    | Error msg -> Error msg
    | Ok files ->
        files
        |> Array.fold folder (Ok [])

let testStdlibHasNoDuplicateDefinitions () : TestResult =
    match findDuplicateDefinitions () with
    | Error msg -> Error msg
    | Ok [] -> Ok ()
    | Ok duplicates ->
        let details = String.concat ", " duplicates
        Error $"Duplicate stdlib definitions: {details}"

let tests = [
    ("stdlib has no duplicate definitions", testStdlibHasNoDuplicateDefinitions)
]
