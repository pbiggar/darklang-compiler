// MemoryLayoutTestRunner.fs - Execute source fixtures and observe final native value words.

module TestDSL.MemoryLayoutTestRunner

open System
open System.IO
open System.Text.RegularExpressions

type private Case = {
    Name: string
    Source: string
    Expected: string
}

let private section (heading: string) (body: string) : string option =
    let marker = $"---{heading}---"
    let start = body.IndexOf(marker, StringComparison.Ordinal)
    if start < 0 then None
    else
        let contentStart = start + marker.Length
        let next = body.IndexOf("---", contentStart, StringComparison.Ordinal)
        let content =
            if next < 0 then body.Substring(contentStart)
            else body.Substring(contentStart, next - contentStart)
        Some (content.Trim())

let private parseCase (body: string) : Result<Case, string> =
    match section "NAME" body, section "INPUT" body, section "EXPECTED" body with
    | Some name, Some source, Some expected
        when name <> "" && source <> "" && expected.StartsWith("root = word(", StringComparison.Ordinal)
             && expected.EndsWith(")", StringComparison.Ordinal) ->
        Ok { Name = name; Source = source; Expected = expected }
    | _ -> Error "Memory layout fixture needs NAME, INPUT, and EXPECTED root = word(...)"

let private parseFile (path: string) : Result<Case list, string> =
    let chunks =
        File.ReadAllText(path)
        |> fun content -> Regex.Split(content, "(?m)(?=^---NAME---$)")
        |> Array.toList
        |> List.filter (fun chunk -> chunk.Trim() <> "")
    chunks
    |> List.fold (fun result chunk ->
        result
        |> Result.bind (fun cases ->
            parseCase chunk |> Result.map (fun case -> case :: cases))) (Ok [])
    |> Result.map List.rev

let private runCase (stdlib: CompilationContexts.StdlibResult) (path: string) (test: Case) : Result<unit, string> =
    let request : CompilationContexts.CompileRequest = {
        Context = CompilationContexts.StdlibOnly stdlib
        Mode = CompilerOptions.TestExpression
        Sources = AST.NonEmptyList.singleton {
            CompilationContexts.SourceUnit.Name = path
            Purpose = NameSyntax.SourceUnitPurpose.Executable
            Source = test.Source
        }
        AllowInternal = false
        Verbosity = 0
        Options = {
            CompilerOptions.defaultOptions with
                ProbeRootWord = true
                EnableLeakCheck = false
        }
        PackageValues = CompilationContexts.emptyPackageValueCatalog
        PackageManager = None
        PassTimingRecorder = None
        Session = None
    }
    let report = CompilerLibrary.compile request
    report.Result
    |> Result.bind (TestDSL.E2ETestRunner.executeBinaryForTarget report.Target)
    |> Result.bind (fun output ->
        if output.ExitCode <> 0 then
            Error $"Native process exited {output.ExitCode}: {output.Stderr}"
        else
            let observed = Regex.Match(output.Stdout, "^-?[0-9]+")
            match observed.Success with
            | true ->
                let actual = $"root = word({observed.Value})"
                if actual = test.Expected then Ok ()
                else Error $"Expected {test.Expected}, got {actual}"
            | false -> Error "Native process did not print a root word")

let tests (stdlib: CompilationContexts.StdlibResult) (files: string array) : (string * (unit -> Result<unit, string>)) list =
    files
    |> Array.sort
    |> Array.toList
    |> List.collect (fun path ->
        match parseFile path with
        | Error error -> [$"parse {Path.GetFileName(path)}", fun () -> Error error]
        | Ok cases ->
            cases |> List.map (fun test -> test.Name, fun () -> runCase stdlib path test))
