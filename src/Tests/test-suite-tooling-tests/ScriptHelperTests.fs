// ScriptHelperTests.fs - Unit tests for shared test script helpers
//
// Ensures the test runner scripts rely on scripts/test-common.sh helpers.

module ScriptHelperTests

open System.IO

type TestResult = Result<unit, string>

let private repoRoot =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", ".."))

let private readFile (path: string) : Result<string, string> =
    try Ok (File.ReadAllText(path))
    with ex -> Error $"Failed to read {path}: {ex.Message}"

let private compilerSourceFiles () : string array =
    let compilerDir = Path.Combine(repoRoot, "src", "DarkCompiler")
    Directory.GetFiles(compilerDir, "*.fs", SearchOption.AllDirectories)

let private findFailwithUses () : Result<string list, string> =
    let folder (state: Result<string list, string>) (path: string) =
        match state with
        | Error _ -> state
        | Ok acc ->
            match readFile path with
            | Error msg -> Error msg
            | Ok text ->
                if text.Contains "failwith" then
                    Ok (path :: acc)
                else
                    Ok acc
    compilerSourceFiles ()
    |> Array.fold folder (Ok [])
    |> Result.map List.rev

let testCompilerAvoidsFailwith () : TestResult =
    match findFailwithUses () with
    | Error msg -> Error msg
    | Ok paths ->
        let offenders =
            paths
            |> List.filter (fun path -> Path.GetFileName(path) <> "Crash.fs")
            |> List.map (fun path -> Path.GetRelativePath(repoRoot, path))
        match offenders with
        | [] -> Ok ()
        | _ ->
            let details = String.concat ", " offenders
            Error $"Unexpected failwith usage in compiler: {details}"

let tests = [
    ("compiler avoids failwith", testCompilerAvoidsFailwith)
]
