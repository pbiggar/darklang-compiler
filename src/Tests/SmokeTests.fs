module SmokeTests

open NUnit.Framework
open FsUnit

[<Test>]
let ``Test framework smoke test`` () =
    // Verify FsUnit is working
    1 + 1 |> should equal 2

[<Test>]
let ``Verify basic F# types work`` () =
    let result = 42
    result |> should equal 42

[<Test>]
let ``Verify list operations work`` () =
    let numbers = [1; 2; 3]
    List.length numbers |> should equal 3
    List.sum numbers |> should equal 6
