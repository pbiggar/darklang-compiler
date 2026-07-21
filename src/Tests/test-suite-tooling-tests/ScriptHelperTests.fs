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

let private sourceFilesUnder (relativeDir: string) : string array =
    let dir = Path.Combine(repoRoot, relativeDir)
    Directory.GetFiles(dir, "*.fs", SearchOption.AllDirectories)

let private compilerSourceFiles () : string array =
    let compilerDir = Path.Combine(repoRoot, "src", "DarkCompiler")
    Directory.GetFiles(compilerDir, "*.fs", SearchOption.AllDirectories)

let private testToolingSourceFiles () : string array =
    sourceFilesUnder (Path.Combine("src", "Tests", "test-suite-tooling"))

let private scriptPath (relativePath: string) : string =
    Path.Combine(repoRoot, relativePath)

let private findFailwithUsesIn (sourceFiles: unit -> string array) : Result<string list, string> =
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
    sourceFiles ()
    |> Array.fold folder (Ok [])
    |> Result.map List.rev

let testCompilerAvoidsFailwith () : TestResult =
    match findFailwithUsesIn compilerSourceFiles with
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

let testTestToolingAvoidsFailwith () : TestResult =
    match findFailwithUsesIn testToolingSourceFiles with
    | Error msg -> Error msg
    | Ok paths ->
        let offenders =
            paths
            |> List.map (fun path -> Path.GetRelativePath(repoRoot, path))
        match offenders with
        | [] -> Ok ()
        | _ ->
            let details = String.concat ", " offenders
            Error $"Unexpected failwith usage in test tooling: {details}"

let testInstallerFormatsAssetListWithStableDelimiter () : TestResult =
    let path = scriptPath "scripts/install-darklang-interpreter.sh"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        if text.Contains "paste -sd ', ' -" then
            Error "install-darklang-interpreter.sh uses paste with multiple delimiters, which alternates comma and space instead of joining every asset with ', '"
        else
            Ok ()

let testShellcheckScansAllTrackedBashScripts () : TestResult =
    let path = scriptPath "scripts/check-shell.sh"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        if text.Contains "git ls-files -z -- run-tests scripts" then
            Error "check-shell.sh only scans run-tests and scripts/, omitting other tracked bash scripts"
        elif not (text.Contains "git ls-files -z)") then
            Error "check-shell.sh does not enumerate all tracked files before filtering bash scripts"
        elif not (text.Contains "shellcheck --severity=error") then
            Error "check-shell.sh should scan all tracked bash scripts for shellcheck errors without requiring existing warnings to be fixed in the same pass"
        else
            Ok ()

let testDumpLirFuncDoesNotSuppressCompilerFailures () : TestResult =
    let path = scriptPath "scripts/dump-lir-func.sh"
    match readFile path with
    | Error msg -> Error msg
    | Ok text ->
        if text.Contains "|| true" then
            Error "dump-lir-func.sh suppresses ./dark --dump-lir failures with `|| true`, so failed dumps can look successful"
        else
            Ok ()

let tests = [
    ("compiler avoids failwith", testCompilerAvoidsFailwith)
    ("test tooling avoids failwith", testTestToolingAvoidsFailwith)
    ("installer formats asset list with stable delimiter", testInstallerFormatsAssetListWithStableDelimiter)
    ("shellcheck scans all tracked bash scripts", testShellcheckScansAllTrackedBashScripts)
    ("dump-lir-func does not suppress compiler failures", testDumpLirFuncDoesNotSuppressCompilerFailures)
]
