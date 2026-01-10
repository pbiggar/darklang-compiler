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
open LIRSymbolic

/// Test result type
type TestResult = Result<unit, string>

/// Helper to create an interference graph from edge list
let makeGraph (vertices: int list) (edges: (int * int) list) : InterferenceGraph =
    let vertexSet = Set.ofList vertices
    let mutable edgeMap = vertices |> List.map (fun v -> (v, Set.empty<int>)) |> Map.ofList
    for (u, v) in edges do
        if u <> v then
            edgeMap <- Map.add u (Set.add v (Map.find u edgeMap)) edgeMap
            edgeMap <- Map.add v (Set.add u (Map.find v edgeMap)) edgeMap
    { Vertices = vertexSet; Edges = edgeMap }

// =============================================================================
// Test Cases
// =============================================================================

/// Test 1: Empty graph
let testEmptyGraph () : TestResult =
    let graph = { Vertices = Set.empty; Edges = Map.empty }
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    if result.ChromaticNumber = 0 && Set.isEmpty result.Spills && Map.isEmpty result.Colors then
        Ok ()
    else
        Error $"Empty graph should have chromatic number 0, got {result.ChromaticNumber}"

/// Test 2: Single vertex
let testSingleVertex () : TestResult =
    let graph = makeGraph [0] []
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    if Map.containsKey 0 result.Colors && result.Colors.[0] = 0 then
        Ok ()
    else
        Error "Single vertex should get color 0"

/// Test 3: Two non-interfering variables (no edges)
let testNoInterference () : TestResult =
    let graph = makeGraph [0; 1] []
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    // Both should get color 0 (same color reused since they don't interfere)
    if result.ChromaticNumber <= 1 then
        Ok ()
    else
        Error $"Non-interfering variables should use 1 color, got {result.ChromaticNumber}"

/// Test 4: Two interfering variables
let testSimpleInterference () : TestResult =
    let graph = makeGraph [0; 1] [(0, 1)]
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    if result.Colors.[0] <> result.Colors.[1] && result.ChromaticNumber = 2 then
        Ok ()
    else
        Error $"Interfering variables should have different colors, got colors {result.Colors.[0]} and {result.Colors.[1]}"

/// Test 5: Triangle (clique of 3) - needs exactly 3 colors
let testTriangle () : TestResult =
    let graph = makeGraph [0; 1; 2] [(0, 1); (0, 2); (1, 2)]
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    let colors = [result.Colors.[0]; result.Colors.[1]; result.Colors.[2]]
    let distinctColors = List.distinct colors
    if List.length distinctColors = 3 && result.ChromaticNumber = 3 then
        Ok ()
    else
        Error $"Triangle needs exactly 3 colors, got {result.ChromaticNumber}"

/// Test 6: Chain (path graph 0--1--2--3) - needs exactly 2 colors
let testChain () : TestResult =
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (1, 2); (2, 3)]
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    // Chain is bipartite, so 2 colors suffice
    if result.ChromaticNumber = 2 then
        // Verify alternating colors
        let c0 = result.Colors.[0]
        let c1 = result.Colors.[1]
        let c2 = result.Colors.[2]
        let c3 = result.Colors.[3]
        if c0 <> c1 && c1 <> c2 && c2 <> c3 then
            Ok ()
        else
            Error $"Chain should have alternating colors, got [{c0}; {c1}; {c2}; {c3}]"
    else
        Error $"Chain needs 2 colors, got {result.ChromaticNumber}"

/// Test 7: Spilling required - clique of 10 with only 8 colors
let testSpillRequired () : TestResult =
    // Create a clique of 10 vertices (all pairs interfere)
    let vertices = [0..9]
    let edges = [
        for i in 0..9 do
            for j in i+1..9 do
                yield (i, j)
    ]
    let graph = makeGraph vertices edges
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    // Should spill at least 2 vertices (10 - 8 = 2)
    if Set.count result.Spills >= 2 then
        // Verify colored vertices got distinct colors
        let coloredVerts = result.Colors |> Map.toList
        let colorValues = coloredVerts |> List.map snd
        let uniqueColors = List.distinct colorValues
        if List.length uniqueColors <= 8 then
            Ok ()
        else
            Error $"Colored vertices should use at most 8 colors, used {List.length uniqueColors}"
    else
        Error $"Should spill at least 2 variables from clique of 10 with 8 colors, got {Set.count result.Spills} spills"

/// Test 8: Pre-coloring respected
let testPrecoloring () : TestResult =
    let graph = makeGraph [0; 1] [(0, 1)]
    let precolored = Map.ofList [(0, 3)]  // Force vertex 0 to color 3
    let result = chordalGraphColor graph precolored 8 Map.empty
    if result.Colors.[0] = 3 && result.Colors.[1] <> 3 then
        Ok ()
    else
        Error $"Pre-coloring should be respected. Expected 0→3, got 0→{result.Colors.[0]}, 1→{result.Colors.[1]}"

/// Test 9: MCS produces valid ordering
let testMCSOrdering () : TestResult =
    // Diamond graph: 0--1, 0--2, 1--3, 2--3
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (0, 2); (1, 3); (2, 3)]
    let ordering = maximumCardinalitySearch graph
    // Ordering should contain all vertices exactly once
    if List.length ordering = 4 && List.sort ordering = [0; 1; 2; 3] then
        Ok ()
    else
        Error $"MCS should produce valid ordering with all 4 vertices, got {ordering}"

/// Test 10: Diamond CFG pattern (common in SSA)
/// Simulates: v0 defined in A, used in B and C, phi in D
/// v1 defined in B, v2 defined in C, v3 = phi(v1, v2)
let testDiamondCFG () : TestResult =
    // Interferences from typical diamond:
    // v0 live with v1 (in block B), v0 live with v2 (in block C)
    // v1 live at phi point with v3, v2 live at phi point with v3
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (0, 2); (1, 3); (2, 3)]
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    // Diamond is chordal and should need at most 3 colors
    if result.ChromaticNumber <= 3 then
        Ok ()
    else
        Error $"Diamond CFG should need at most 3 colors, got {result.ChromaticNumber}"

/// Test 11: Star graph (one central vertex connected to all others)
let testStarGraph () : TestResult =
    // Center vertex 0 connected to 1, 2, 3, 4
    let graph = makeGraph [0; 1; 2; 3; 4] [(0, 1); (0, 2); (0, 3); (0, 4)]
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    // Star graph needs 2 colors (center + all leaves same color)
    if result.ChromaticNumber = 2 then
        // Center should have different color from all leaves
        let centerColor = result.Colors.[0]
        let leafColors = [1; 2; 3; 4] |> List.map (fun v -> result.Colors.[v])
        if leafColors |> List.forall (fun c -> c <> centerColor) then
            Ok ()
        else
            Error "Center should have different color from leaves"
    else
        Error $"Star graph needs 2 colors, got {result.ChromaticNumber}"

/// Test 12: Multiple pre-colored vertices
let testMultiplePrecolored () : TestResult =
    // Triangle with two vertices pre-colored
    let graph = makeGraph [0; 1; 2] [(0, 1); (0, 2); (1, 2)]
    let precolored = Map.ofList [(0, 0); (1, 1)]  // Force specific colors
    let result = chordalGraphColor graph precolored 8 Map.empty
    if result.Colors.[0] = 0 && result.Colors.[1] = 1 && result.Colors.[2] <> 0 && result.Colors.[2] <> 1 then
        Ok ()
    else
        Error $"Pre-coloring not respected: 0→{result.Colors.[0]}, 1→{result.Colors.[1]}, 2→{result.Colors.[2]}"

/// Test 13: Large clique exactly matches register count
let testExactClique () : TestResult =
    // Clique of 8 with 8 colors - should use all colors, no spills
    let vertices = [0..7]
    let edges = [
        for i in 0..7 do
            for j in i+1..7 do
                yield (i, j)
    ]
    let graph = makeGraph vertices edges
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    if result.ChromaticNumber = 8 && Set.isEmpty result.Spills then
        Ok ()
    else
        Error $"Clique of 8 with 8 colors should work with no spills, got chromatic {result.ChromaticNumber}, spills {Set.count result.Spills}"

/// Test 14: Disconnected components
let testDisconnectedComponents () : TestResult =
    // Two separate edges: 0--1, 2--3
    let graph = makeGraph [0; 1; 2; 3] [(0, 1); (2, 3)]
    let result = chordalGraphColor graph Map.empty 8 Map.empty
    // Should only need 2 colors total (reuse colors across components)
    if result.ChromaticNumber = 2 then
        Ok ()
    else
        Error $"Disconnected components should reuse colors, got {result.ChromaticNumber}"

/// Test 15: Build interference graph from real LIR CFG
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

    // Compute liveness and build interference graph
    let liveness = RegisterAllocation.computeLiveness cfg
    let graph = RegisterAllocation.buildInterferenceGraph cfg liveness

    // v0 and v1 should both be in the graph
    if not (Set.contains 0 graph.Vertices) then
        Error "v0 should be in interference graph"
    elif not (Set.contains 1 graph.Vertices) then
        Error "v1 should be in interference graph"
    elif not (Set.contains 2 graph.Vertices) then
        Error "v2 should be in interference graph"
    else
        // v0 and v1 should interfere (they're both live at Mul instruction)
        let v0Neighbors = Map.tryFind 0 graph.Edges |> Option.defaultValue Set.empty
        let v1Neighbors = Map.tryFind 1 graph.Edges |> Option.defaultValue Set.empty
        if Set.contains 1 v0Neighbors && Set.contains 0 v1Neighbors then
            Ok ()
        else
            Error $"v0 and v1 should interfere. v0 neighbors: {v0Neighbors}, v1 neighbors: {v1Neighbors}"

/// Test 16: Full pipeline - CFG to allocation using chordal graph coloring
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
    let liveness = RegisterAllocation.computeLiveness cfg
    let graph = RegisterAllocation.buildInterferenceGraph cfg liveness
    let colorResult = chordalGraphColor graph Map.empty 16 Map.empty  // 16 available colors

    // v0 and v1 must have different colors (they interfere)
    match Map.tryFind 0 colorResult.Colors, Map.tryFind 1 colorResult.Colors with
    | Some c0, Some c1 ->
        if c0 <> c1 then
            Ok ()
        else
            Error $"v0 and v1 should have different colors, both got color {c0}"
    | _ ->
        Error $"v0 or v1 not found in coloring. Colors: {colorResult.Colors}"

/// Test 17: Simulates apply2(f, a, b) = f(a, b) pattern
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
    let liveness = RegisterAllocation.computeLiveness cfg
    let graph = RegisterAllocation.buildInterferenceGraph cfg liveness

    // All of v0, v1, v2 should be in the graph and interfere with each other
    let v0Neighbors = Map.tryFind 0 graph.Edges |> Option.defaultValue Set.empty
    let v1Neighbors = Map.tryFind 1 graph.Edges |> Option.defaultValue Set.empty
    let v2Neighbors = Map.tryFind 2 graph.Edges |> Option.defaultValue Set.empty

    // Check all pairs interfere
    if not (Set.contains 0 graph.Vertices) then
        Error $"v0 not in graph. Vertices: {graph.Vertices}"
    elif not (Set.contains 1 graph.Vertices) then
        Error $"v1 not in graph. Vertices: {graph.Vertices}"
    elif not (Set.contains 2 graph.Vertices) then
        Error $"v2 not in graph. Vertices: {graph.Vertices}"
    elif not (Set.contains 1 v0Neighbors && Set.contains 2 v0Neighbors) then
        Error $"v0 should interfere with v1 and v2. v0 neighbors: {v0Neighbors}"
    elif not (Set.contains 0 v1Neighbors && Set.contains 2 v1Neighbors) then
        Error $"v1 should interfere with v0 and v2. v1 neighbors: {v1Neighbors}"
    elif not (Set.contains 0 v2Neighbors && Set.contains 1 v2Neighbors) then
        Error $"v2 should interfere with v0 and v1. v2 neighbors: {v2Neighbors}"
    else
        // Now check that coloring assigns different colors
        let colorResult = chordalGraphColor graph Map.empty 16 Map.empty
        match Map.tryFind 1 colorResult.Colors, Map.tryFind 2 colorResult.Colors with
        | Some c1, Some c2 ->
            if c1 <> c2 then
                Ok ()
            else
                Error $"v1 and v2 got same color {c1}. Colors: {colorResult.Colors}"
        | _ ->
            Error $"v1 or v2 not found in coloring. Colors: {colorResult.Colors}"

/// Run all tests and return results
let runAllTests () : (string * TestResult) list =
    [
        ("Empty graph", testEmptyGraph ())
        ("Single vertex", testSingleVertex ())
        ("No interference", testNoInterference ())
        ("Simple interference", testSimpleInterference ())
        ("Triangle (clique of 3)", testTriangle ())
        ("Chain (path graph)", testChain ())
        ("Spill required", testSpillRequired ())
        ("Pre-coloring respected", testPrecoloring ())
        ("MCS ordering", testMCSOrdering ())
        ("Diamond CFG", testDiamondCFG ())
        ("Star graph", testStarGraph ())
        ("Multiple pre-colored", testMultiplePrecolored ())
        ("Exact clique", testExactClique ())
        ("Disconnected components", testDisconnectedComponents ())
        ("Build from real CFG", testBuildFromCFG ())
        ("Full chordal pipeline", testFullChordalPipeline ())
        ("Apply2 pattern", testApply2Pattern ())
    ]
