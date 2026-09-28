// SSAOptimizationTests.fs - Execute plain-text SSA optimization cases.

module SSAOptimizationTests

let tests =
    let path =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "ssa-optimization", "ssa.opt")
    TestDSL.SSAInliningFormat.testsFromFile path
