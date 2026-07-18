// ChordalGraphTests.fs - Unit tests for chordal graph coloring register allocation
//
// Tests the Maximum Cardinality Search (MCS) algorithm and greedy coloring
// used for optimal register allocation on SSA interference graphs.
//
// Key properties being tested:
// - MCS produces valid ordering containing all vertices
// - Greedy coloring on reverse PEO produces optimal coloring for chordal graphs
// - Pre-coloring constraints are respected
// - Spills occur when chromatic number exceeds available colors

module ChordalGraphTests

open RegisterAllocation
open LIR
open Crash

/// Test result type
type TestResult = Result<unit, string>

/// Helper to create an interference graph from edge list
let makeGraph (vertices: int list) (edges: (int * int) list) : InterferenceGraph =
    RegisterAllocation.buildInterferenceGraphFromEdges vertices edges

let colorOf (result: ColoringResult) (vregId: int) : int option =
    RegisterAllocation.colorOf result vregId

let isSpill (result: ColoringResult) (vregId: int) : bool =
    RegisterAllocation.isSpill result vregId

let spillCount (result: ColoringResult) : int =
    RegisterAllocation.spillCount result

let coloredCount (result: ColoringResult) : int =
    RegisterAllocation.coloredCount result

let graphNeighbors (graph: InterferenceGraph) (vregId: int) : Set<int> =
    RegisterAllocation.graphNeighbors graph vregId |> Set.ofList

let graphHasVertex (graph: InterferenceGraph) (vregId: int) : bool =
    RegisterAllocation.graphHasVertex graph vregId

let cliqueEdges (vertices: int list) : (int * int) list =
    vertices
    |> List.collect (fun i ->
        vertices
        |> List.filter (fun j -> i < j)
        |> List.map (fun j -> (i, j)))

// =============================================================================
// Test Cases
// =============================================================================

/// Test 1: Empty graph
let testEmptyGraph () : TestResult =
    let graph = makeGraph [] []
    let result = chordalGraphColor graph [] 8 [] []
    if result.ChromaticNumber = 0 && spillCount result = 0 && coloredCount result = 0 then
        Ok ()
    else
        Error $"Empty graph should have chromatic number 0, got {result.ChromaticNumber}"

/// Test 2: Single vertex
let testSingleVertex () : TestResult =
    let graph = makeGraph [0] []
    let result = chordalGraphColor graph [] 8 [] []
    match colorOf result 0 with
    | Some 0 -> Ok ()
    | _ -> Error "Single vertex should get color 0"

/// Test 3: Two non-interfering variables (no edges)
let testNoInterference () : TestResult =
    let graph = makeGraph [0; 1] []
    let result = chordalGraphColor graph [] 8 [] []
    // Both should get color 0 (same color reused since they don't interfere)
    if result.ChromaticNumber <= 1 then
        Ok ()
    else
        Error $"Non-interfering variables should use 1 color, got {result.ChromaticNumber}"

/// Test 4: Two interfering variables
let testSimpleInterference () : TestResult =
    let graph = makeGraph [0; 1] [(0, 1)]
    let result = chordalGraphColor graph [] 8 [] []
    match colorOf result 0, colorOf result 1 with
    | Some c0, Some c1 when c0 <> c1 && result.ChromaticNumber = 2 -> Ok ()
    | Some c0, Some c1 ->
        Error $"Interfering variables should have different colors, got colors {c0} and {c1}"
    | _ ->
        Error "Interfering variables should have different colors"

/// Test 5: Triangle (clique of 3) - needs exactly 3 colors
let testTriangle () : TestResult =
    let graph = makeGraph [0; 1; 2] [(0, 1); (0, 2); (1, 2)]
    let result = chordalGraphColor graph [] 8 [] []
    let colors = [0; 1; 2] |> List.choose (colorOf result)
    let distinctColors = List.distinct colors
    if List.length colors = 3 && List.length distinctColors = 3 && result.ChromaticNumber = 3 then
        Ok ()
    else
        Error $"Triangle needs exactly 3 colors, got {result.ChromaticNumber}"

/// Test 6: Chain (path graph 0--1--2--3) - needs exactly 2 colors
let testChain () : TestResult =
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (1, 2); (2, 3)]
    let result = chordalGraphColor graph [] 8 [] []
    // Chain is bipartite, so 2 colors suffice
    if result.ChromaticNumber = 2 then
        // Verify alternating colors
        match colorOf result 0, colorOf result 1, colorOf result 2, colorOf result 3 with
        | Some c0, Some c1, Some c2, Some c3 ->
            if c0 <> c1 && c1 <> c2 && c2 <> c3 then
                Ok ()
            else
                Error $"Chain should have alternating colors, got [{c0}; {c1}; {c2}; {c3}]"
        | _ ->
            Error "Chain should have alternating colors"
    else
        Error $"Chain needs 2 colors, got {result.ChromaticNumber}"

/// Test 7: Spilling required - clique of 10 with only 8 colors
let testSpillRequired () : TestResult =
    // Create a clique of 10 vertices (all pairs interfere)
    let vertices = [0..9]
    let edges = cliqueEdges vertices
    let graph = makeGraph vertices edges
    let result = chordalGraphColor graph [] 8 [] []
    // Should spill at least 2 vertices (10 - 8 = 2)
    if spillCount result >= 2 then
        // Verify colored vertices got distinct colors
        let colorValues = vertices |> List.choose (colorOf result)
        let uniqueColors = List.distinct colorValues
        if List.length uniqueColors <= 8 then
            Ok ()
        else
            Error $"Colored vertices should use at most 8 colors, used {List.length uniqueColors}"
    else
        Error $"Should spill at least 2 variables from clique of 10 with 8 colors, got {spillCount result} spills"

/// Test 8: Pre-coloring respected
let testPrecoloring () : TestResult =
    let graph = makeGraph [0; 1] [(0, 1)]
    let precolored = [(0, 3)]  // Force vertex 0 to color 3
    let result = chordalGraphColor graph precolored 8 [] []
    match colorOf result 0, colorOf result 1 with
    | Some c0, Some c1 ->
        if c0 = 3 && c1 <> 3 then
            Ok ()
        else
            Error $"Pre-coloring should be respected. Expected 0→3, got 0→{c0}, 1→{c1}"
    | _ ->
        Error "Pre-coloring should be respected"

/// Test 9: MCS produces valid ordering
let testMCSOrdering () : TestResult =
    // Four-cycle join shape: 0--1, 0--2, 1--3, 2--3
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (0, 2); (1, 3); (2, 3)]
    let ordering = maximumCardinalitySearch graph
    // Ordering should contain all vertices exactly once
    if List.length ordering = 4 && List.sort ordering = [0; 1; 2; 3] then
        Ok ()
    else
        Error $"MCS should produce valid ordering with all 4 vertices, got {ordering}"

/// Test 10: MCS profile should avoid per-vertex scans
/// For a graph with no edges, linear-time MCS should inspect only one vertex per selection.
let testMCSProfileLinear () : TestResult =
    let vertices = [0..15]
    let graph = makeGraph vertices []
    let (_ordering, profile) = maximumCardinalitySearchWithProfile graph
    if profile.SelectionChecks = vertices.Length then
        Ok ()
    else
        Error $"Expected SelectionChecks {vertices.Length}, got {profile.SelectionChecks}"

/// Test 11: Branch/join CFG pattern (common in SSA)
/// Simulates: v0 defined in A, used in B and C, phi in D
/// v1 defined in B, v2 defined in C, v3 = phi(v1, v2)
let testBranchJoinCFG () : TestResult =
    // Interferences from a typical branch/join:
    // v0 live with v1 (in block B), v0 live with v2 (in block C)
    // v1 live at phi point with v3, v2 live at phi point with v3
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (0, 2); (1, 3); (2, 3)]
    let result = chordalGraphColor graph [] 8 [] []
    // This shape should need at most 3 colors.
    if result.ChromaticNumber <= 3 then
        Ok ()
    else
        Error $"Branch/join CFG should need at most 3 colors, got {result.ChromaticNumber}"

/// Test 12: Star graph (one central vertex connected to all others)
let testStarGraph () : TestResult =
    // Center vertex 0 connected to 1, 2, 3, 4
    let graph = makeGraph [0; 1; 2; 3; 4] [(0, 1); (0, 2); (0, 3); (0, 4)]
    let result = chordalGraphColor graph [] 8 [] []
    // Star graph needs 2 colors (center + all leaves same color)
    if result.ChromaticNumber = 2 then
        // Center should have different color from all leaves
        match colorOf result 0 with
        | Some centerColor ->
            let leafColors = [1; 2; 3; 4] |> List.choose (colorOf result)
            if List.length leafColors = 4 && leafColors |> List.forall (fun c -> c <> centerColor) then
                Ok ()
            else
                Error "Center should have different color from leaves"
        | None ->
            Error "Center should have different color from leaves"
    else
        Error $"Star graph needs 2 colors, got {result.ChromaticNumber}"

/// Test 13: Multiple pre-colored vertices
let testMultiplePrecolored () : TestResult =
    // Triangle with two vertices pre-colored
    let graph = makeGraph [0; 1; 2] [(0, 1); (0, 2); (1, 2)]
    let precolored = [(0, 0); (1, 1)]  // Force specific colors
    let result = chordalGraphColor graph precolored 8 [] []
    match colorOf result 0, colorOf result 1, colorOf result 2 with
    | Some c0, Some c1, Some c2 ->
        if c0 = 0 && c1 = 1 && c2 <> 0 && c2 <> 1 then
            Ok ()
        else
            Error $"Pre-coloring not respected: 0→{c0}, 1→{c1}, 2→{c2}"
    | _ ->
        Error "Pre-coloring not respected"

/// Test 14: Large clique exactly matches register count
let testExactClique () : TestResult =
    // Clique of 8 with 8 colors - should use all colors, no spills
    let vertices = [0..7]
    let edges = cliqueEdges vertices
    let graph = makeGraph vertices edges
    let result = chordalGraphColor graph [] 8 [] []
    if result.ChromaticNumber = 8 && spillCount result = 0 then
        Ok ()
    else
        Error $"Clique of 8 with 8 colors should work with no spills, got chromatic {result.ChromaticNumber}, spills {spillCount result}"

/// Test 15: Disconnected components
let testDisconnectedComponents () : TestResult =
    // Two separate edges: 0--1, 2--3
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (2, 3)]
    let result = chordalGraphColor graph [] 8 [] []
    // Should only need 2 colors total (reuse colors across components)
    if result.ChromaticNumber = 2 then
        Ok ()
    else
        Error $"Disconnected components should reuse colors, got {result.ChromaticNumber}"

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

/// Test 20: Prefer move-related coalescing over phi preferences when only one is possible
let testMoveCoalescingPriority () : TestResult =
    let graph = makeGraph [0; 1; 2] [(1, 2)]
    let preferencePairs = [(0, 1)]
    let movePairs = [(0, 2)]
    let result = chordalGraphColor graph [] 2 preferencePairs movePairs
    match colorOf result 0, colorOf result 2 with
    | Some c0, Some c2 ->
        if c0 = c2 then
            Ok ()
        else
            Error $"Expected move-related coalescing to pick 0/2, got 0→{c0}, 2→{c2}"
    | _ ->
        Error "Missing colors for 0 or 2."

/// Run all tests and return results
let tests = [
    ("Empty graph", testEmptyGraph)
    ("Single vertex", testSingleVertex)
    ("No interference", testNoInterference)
    ("Simple interference", testSimpleInterference)
    ("Triangle (clique of 3)", testTriangle)
    ("Chain (path graph)", testChain)
    ("Spill required", testSpillRequired)
    ("Pre-coloring respected", testPrecoloring)
    ("MCS ordering", testMCSOrdering)
    ("MCS profile linear", testMCSProfileLinear)
    ("Branch/join CFG", testBranchJoinCFG)
    ("Star graph", testStarGraph)
    ("Multiple pre-colored", testMultiplePrecolored)
    ("Exact clique", testExactClique)
    ("Disconnected components", testDisconnectedComponents)
    ("Build from real CFG", testBuildFromCFG)
    ("Bitset graph matches", testBuildFromCFGBitsetMatches)
    ("Full chordal pipeline", testFullChordalPipeline)
    ("Apply2 pattern", testApply2Pattern)
    ("Move coalescing pairs", testMoveCoalescingPreference)
    ("Move coalescing priority", testMoveCoalescingPriority)
]

let runAllTests () : (string * TestResult) list =
    tests |> List.map (fun (name, test) -> (name, test ()))
