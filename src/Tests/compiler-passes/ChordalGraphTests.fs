// ChordalGraphTests.fs - Integration tests for register-allocation graph construction.
//
// Table-driven graph coloring and MCS properties live in .graphcolor fixtures.
// These tests retain CFG/liveness construction assertions that require direct compiler types.

module ChordalGraphTests

open RegisterAllocation
open LIR

type TestResult = Result<unit, string>

let colorOf (result: ColoringResult) (vregId: int) : int option =
    RegisterAllocation.colorOf result vregId

let graphNeighbors (graph: InterferenceGraph) (vregId: int) : Set<int> =
    RegisterAllocation.graphNeighbors graph vregId |> Set.ofList

let graphHasVertex (graph: InterferenceGraph) (vregId: int) : bool =
    RegisterAllocation.graphHasVertex graph vregId

/// Test 16: Build interference graph from real LIR CFG
/// Simulates a function (a, b) => a * b where parameters should interfere
let testBuildFromCFG () : TestResult =
    // Create a minimal CFG for: (a, b) => a * b
    // Virtual 0 = a (parameter)
    // Virtual 1 = b (parameter)
    // Virtual 2 = a * b (result)
    let v0 = LIR.Virtual 0
    let v1 = LIR.Virtual 1
    let v2 = LIR.Virtual 2

    let entryLabel = LIR.Label "entry"
    let entryBlock : BasicBlock = {
        Label = entryLabel
        Instrs = [
            Mul(v2, v0, v1)                          // v2 = v0 * v1
            Mov(LIR.Physical X0, Reg v2)            // X0 = v2 (return value)
        ]
        Terminator = Ret
    }

    let cfg : CFG = {
        Entry = entryLabel
        Blocks = Map.ofList [(entryLabel, entryBlock)]
    }

    // Build interference graph
    let graph = RegisterAllocation.buildInterferenceGraphBitset cfg [0; 1]

    // v0 and v1 should both be in the graph
    if not (graphHasVertex graph 0) then
        Error "v0 should be in interference graph"
    elif not (graphHasVertex graph 1) then
        Error "v1 should be in interference graph"
    elif not (graphHasVertex graph 2) then
        Error "v2 should be in interference graph"
    else
        // v0 and v1 should interfere (they're both live at Mul instruction)
        let v0Neighbors = graphNeighbors graph 0
        let v1Neighbors = graphNeighbors graph 1
        if Set.contains 1 v0Neighbors && Set.contains 0 v1Neighbors then
            Ok ()
        else
            Error $"v0 and v1 should interfere. v0 neighbors: {v0Neighbors}, v1 neighbors: {v1Neighbors}"

/// Test 16b: Bitset-based interference graph matches expected edges
let testBuildFromCFGBitsetMatches () : TestResult =
    let v0 = LIR.Virtual 0
    let v1 = LIR.Virtual 1
    let v2 = LIR.Virtual 2
    let v3 = LIR.Virtual 3
    let v4 = LIR.Virtual 4

    let labelA = LIR.Label "A"
    let labelB = LIR.Label "B"
    let labelC = LIR.Label "C"
    let labelD = LIR.Label "D"

    let blockA : BasicBlock = {
        Label = labelA
        Instrs = [Mov (v0, Imm 1L)]
        Terminator = Branch (v0, labelB, labelC)
    }

    let blockB : BasicBlock = {
        Label = labelB
        Instrs = [Mov (v1, Imm 2L)]
        Terminator = Jump labelD
    }

    let blockC : BasicBlock = {
        Label = labelC
        Instrs = [Mov (v2, Imm 3L)]
        Terminator = Jump labelD
    }

    let blockD : BasicBlock = {
        Label = labelD
        Instrs = [
            Phi (v3, [(Reg v1, labelB); (Reg v2, labelC)], None)
            Add (v4, v3, Imm 1L)
            Mov (LIR.Physical X0, Reg v4)
        ]
        Terminator = Ret
    }

    let cfg : CFG = {
        Entry = labelA
        Blocks = Map.ofList [
            (labelA, blockA)
            (labelB, blockB)
            (labelC, blockC)
            (labelD, blockD)
        ]
    }

    let bitsetGraph = RegisterAllocation.buildInterferenceGraphBitset cfg [0]
    let expectedVertices = Set.ofList [0; 1; 2; 3; 4]

    let neighbors v = graphNeighbors bitsetGraph v

    if expectedVertices |> Set.exists (fun v -> not (graphHasVertex bitsetGraph v)) then
        Error $"Bitset graph vertices differ. Expected: {expectedVertices}"
    elif Set.contains 2 (neighbors 1) || Set.contains 1 (neighbors 2) then
        Error $"v1 and v2 should not interfere across diamond branches. v1 neighbors: {neighbors 1}, v2 neighbors: {neighbors 2}"
    elif Set.contains 1 (neighbors 3) || Set.contains 2 (neighbors 3) then
        Error $"Phi dest should not interfere with its operands. v3 neighbors: {neighbors 3}"
    else
        Ok ()

/// Test 17: Full pipeline - CFG to allocation using chordal graph coloring
/// Verify that interfering parameters get different register colors
let testFullChordalPipeline () : TestResult =
    // Same CFG as testBuildFromCFG
    let v0 = LIR.Virtual 0
    let v1 = LIR.Virtual 1
    let v2 = LIR.Virtual 2

    let entryLabel = LIR.Label "entry"
    let entryBlock : BasicBlock = {
        Label = entryLabel
        Instrs = [
            Mul(v2, v0, v1)
            Mov(LIR.Physical X0, Reg v2)
        ]
        Terminator = Ret
    }

    let cfg : CFG = {
        Entry = entryLabel
        Blocks = Map.ofList [(entryLabel, entryBlock)]
    }

    // Build interference graph and run chordal coloring
    let graph = RegisterAllocation.buildInterferenceGraphBitset cfg [0; 1]
    let colorResult = chordalGraphColor graph [] 16 [] []  // 16 available colors

    // v0 and v1 must have different colors (they interfere)
    match colorOf colorResult 0, colorOf colorResult 1 with
    | Some c0, Some c1 ->
        if c0 <> c1 then
            Ok ()
        else
            Error $"v0 and v1 should have different colors, both got color {c0}"
    | _ ->
        Error "v0 or v1 not found in coloring."

/// Test 18: Simulates apply2(f, a, b) = f(a, b) pattern
/// f, a, b are all used in the ClosureCall - they should all interfere
let testApply2Pattern () : TestResult =
    // Simulate: def apply2(f, a, b) = f(a, b)
    // Virtual 0 = f (function parameter)
    // Virtual 1 = a (first int parameter)
    // Virtual 2 = b (second int parameter)
    // Virtual 3 = result of f(a, b)
    let v0 = LIR.Virtual 0  // f
    let v1 = LIR.Virtual 1  // a
    let v2 = LIR.Virtual 2  // b
    let v3 = LIR.Virtual 3  // result

    let entryLabel = LIR.Label "entry"
    let entryBlock : BasicBlock = {
        Label = entryLabel
        Instrs = [
            // ClosureCall(result, closure, args)
            ClosureCall(v3, v0, [Reg v1; Reg v2])
            Mov(LIR.Physical X0, Reg v3)
        ]
        Terminator = Ret
    }

    let cfg : CFG = {
        Entry = entryLabel
        Blocks = Map.ofList [(entryLabel, entryBlock)]
    }

    // Build interference graph
    let graph = RegisterAllocation.buildInterferenceGraphBitset cfg [0; 1; 2]

    // All of v0, v1, v2 should be in the graph and interfere with each other
    let v0Neighbors = graphNeighbors graph 0
    let v1Neighbors = graphNeighbors graph 1
    let v2Neighbors = graphNeighbors graph 2

    // Check all pairs interfere
    if not (graphHasVertex graph 0) then
        Error "v0 not in graph."
    elif not (graphHasVertex graph 1) then
        Error "v1 not in graph."
    elif not (graphHasVertex graph 2) then
        Error "v2 not in graph."
    elif not (Set.contains 1 v0Neighbors && Set.contains 2 v0Neighbors) then
        Error $"v0 should interfere with v1 and v2. v0 neighbors: {v0Neighbors}"
    elif not (Set.contains 0 v1Neighbors && Set.contains 2 v1Neighbors) then
        Error $"v1 should interfere with v0 and v2. v1 neighbors: {v1Neighbors}"
    elif not (Set.contains 0 v2Neighbors && Set.contains 1 v2Neighbors) then
        Error $"v2 should interfere with v0 and v1. v2 neighbors: {v2Neighbors}"
    else
        // Now check that coloring assigns different colors
        let colorResult = chordalGraphColor graph [] 16 [] []
        match colorOf colorResult 1, colorOf colorResult 2 with
        | Some c1, Some c2 ->
            if c1 <> c2 then
                Ok ()
            else
                Error $"v1 and v2 got same color {c1}."
        | _ ->
            Error "v1 or v2 not found in coloring."

/// Test 19: Copy move should create coalescing pairs
let testMoveCoalescingPreference () : TestResult =
    let v0 = LIR.Virtual 0
    let v1 = LIR.Virtual 1

    let entryLabel = LIR.Label "entry"
    let entryBlock : BasicBlock = {
        Label = entryLabel
        Instrs = [
            Mov(v1, Reg v0)
            Mov(LIR.Physical X0, Reg v1)
        ]
        Terminator = Ret
    }

    let cfg : CFG = {
        Entry = entryLabel
        Blocks = Map.ofList [(entryLabel, entryBlock)]
    }

    let blocks = cfg.Blocks |> Seq.toArray |> Array.map (fun kvp -> kvp.Value)
    let pairs = RegisterAllocation.collectMovePairs blocks
    let normalize (a: int, b: int) = if a < b then (a, b) else (b, a)
    let normalized = pairs |> List.map normalize
    if List.contains (0, 1) normalized then
        Ok ()
    else
        Error $"Expected move pair (0, 1), got {pairs}"

let tests = [
    ("Build from real CFG", testBuildFromCFG)
    ("Bitset graph matches", testBuildFromCFGBitsetMatches)
    ("Full chordal pipeline", testFullChordalPipeline)
    ("Apply2 pattern", testApply2Pattern)
    ("Move coalescing pairs", testMoveCoalescingPreference)
]

let runAllTests () : (string * TestResult) list =
    tests |> List.map (fun (name, test) -> (name, test ()))
