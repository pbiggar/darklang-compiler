// CompilerLibraryTests.fs - Unit tests for CompilerLibrary public surface
//
// Ensures deprecated compilation paths are removed.

module CompilerLibraryTests

open System.Reflection

type TestResult = Result<unit, string>

let testNoCompileWithStdlibAst () : TestResult =
    match typeof<CompilerLibrary.CompilerOptions>.DeclaringType with
    | null -> Error "Could not locate CompilerLibrary module type."
    | moduleType ->
        let methods =
            moduleType.GetMethods(
                BindingFlags.Static
                ||| BindingFlags.NonPublic
                ||| BindingFlags.Public
                ||| BindingFlags.DeclaredOnly
            )
        let hasMethod =
            methods |> Array.exists (fun methodInfo -> methodInfo.Name = "compileWithStdlibAST")
        if hasMethod then
            Error "CompilerLibrary still defines compileWithStdlibAST; remove dead compile path."
        else
            Ok ()

let tests = [
    ("no compileWithStdlibAST", testNoCompileWithStdlibAst)
]
