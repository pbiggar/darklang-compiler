// The cases and expectations live in the text fixture, not in F# test code.
module SSAInliningTests

let tests =
    let path =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "inlining", "ssa.inline")
    TestDSL.SSAInliningFormat.testsFromFile path
