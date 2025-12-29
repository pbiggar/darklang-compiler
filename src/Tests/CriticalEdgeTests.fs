// CriticalEdgeTests.fs - Unit tests for critical edge splitting
//
// Tests the critical edge detection and splitting algorithm used for SSA form.
// A critical edge is an edge from a block with multiple successors to a block
// with multiple predecessors.
//
// Key test cases:
// - Diamond CFG (no critical edges)
// - Single critical edge (branch to join with multiple preds)
// - Multiple critical edges
// - Loop with back edge

module CriticalEdgeTests

open MIR

/// Test result type
type TestResult = Result<unit, string>

/// Create a label generator starting at 0
let freshLabelGen () : LabelGen = LabelGen 0

/// Create a fresh label
let makeLabel (name: string) = Label name

/// Create a VReg from an int
let vreg (n: int) = VReg n

/// Create a simple basic block with a jump terminator
let makeJumpBlock (label: Label) (target: Label) : BasicBlock =
    { Label = label; Instrs = []; Terminator = Jump target }

/// Create a basic block with branch terminator
let makeBranchBlock (label: Label) (cond: VReg) (trueTarget: Label) (falseTarget: Label) : BasicBlock =
    { Label = label; Instrs = []; Terminator = Branch (Register cond, trueTarget, falseTarget) }

/// Create a basic block with return terminator
let makeRetBlock (label: Label) (retVal: Operand) : BasicBlock =
    { Label = label; Instrs = []; Terminator = Ret retVal }

/// Create a basic block with phi nodes
let makePhiBlock (label: Label) (phis: Instr list) (terminator: Terminator) : BasicBlock =
    { Label = label; Instrs = phis; Terminator = terminator }

/// Build a CFG from a list of blocks
let makeCFG (entry: Label) (blocks: BasicBlock list) : CFG =
    let blockMap = blocks |> List.map (fun b -> (b.Label, b)) |> Map.ofList
    { Entry = entry; Blocks = blockMap }

// =============================================================================
// Test Cases
// =============================================================================

/// Diamond CFG - no critical edges
///     A (branch)
///    / \
///   B   C
///    \ /
///     D (phi)
///
/// A->B: A has 2 succ, B has 1 pred -> NOT critical
/// A->C: A has 2 succ, C has 1 pred -> NOT critical
/// B->D: B has 1 succ, D has 2 preds -> NOT critical
/// C->D: C has 1 succ, D has 2 preds -> NOT critical
let testDiamondNoCriticalEdges () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let condReg = vreg 0

    let blockA = makeBranchBlock labelA condReg labelB labelC
    let blockB = makeJumpBlock labelB labelD
    let blockC = makeJumpBlock labelC labelD
    let blockD = makeRetBlock labelD (IntConst 0L)

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]
    let labelGen = freshLabelGen ()

    let (cfg', _) = SSA_Destruction.splitCriticalEdges cfg labelGen

    // Should have same number of blocks (no splitting needed)
    if Map.count cfg'.Blocks <> 4 then
        Error $"Expected 4 blocks, got {Map.count cfg'.Blocks}"
    else
        Ok ()

/// Single critical edge
///     A (branch)
///    /   \
///   B     \
///    \     \
///     D (phi)
///
/// A->B: A has 2 succ, B has 1 pred -> NOT critical
/// A->D: A has 2 succ, D has 2 preds -> CRITICAL
/// B->D: B has 1 succ, D has 2 preds -> NOT critical
let testSingleCriticalEdge () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelD = makeLabel "D"

    let condReg = vreg 0
    let phiDest = vreg 1

    // A branches to B (true) or D (false) - A->D is critical
    let blockA = makeBranchBlock labelA condReg labelB labelD
    let blockB = makeJumpBlock labelB labelD
    // D has phi from both A and B
    let phiInstr = Phi (phiDest, [(IntConst 1L, labelA); (IntConst 2L, labelB)])
    let blockD = makePhiBlock labelD [phiInstr] (Ret (IntConst 0L))

    let cfg = makeCFG labelA [blockA; blockB; blockD]
    let labelGen = freshLabelGen ()

    let (cfg', _) = SSA_Destruction.splitCriticalEdges cfg labelGen

    // Should have 4 blocks now (1 new block for split edge)
    if Map.count cfg'.Blocks <> 4 then
        Error $"Expected 4 blocks after split, got {Map.count cfg'.Blocks}"
    else
        // Verify A no longer points directly to D on its false branch
        let blockA' = Map.find labelA cfg'.Blocks
        match blockA'.Terminator with
        | Branch (_, _, falseTarget) when falseTarget = labelD ->
            Error "A should not point directly to D after split"
        | Branch _ -> Ok ()
        | _ -> Error "Block A should still be a branch"

/// Multiple critical edges - both branches of A go to D
///     A (branch)
///    / \
///   |   |
///    \ /
///     D (phi)
///     |
///     E
///
/// A->D (true): A has 2 succ, D has 2 preds -> CRITICAL
/// A->D (false): A has 2 succ, D has 2 preds -> CRITICAL
let testMultipleCriticalEdges () : TestResult =
    let labelA = makeLabel "A"
    let labelD = makeLabel "D"
    let labelE = makeLabel "E"

    let condReg = vreg 0
    let phiDest = vreg 1

    // A branches to D on both true and false (degenerate case)
    let blockA = makeBranchBlock labelA condReg labelD labelD
    let phiInstr = Phi (phiDest, [(IntConst 1L, labelA); (IntConst 2L, labelA)])  // Both from A
    let blockD = makePhiBlock labelD [phiInstr] (Jump labelE)
    let blockE = makeRetBlock labelE (IntConst 0L)

    let cfg = makeCFG labelA [blockA; blockD; blockE]
    let labelGen = freshLabelGen ()

    let (cfg', _) = SSA_Destruction.splitCriticalEdges cfg labelGen

    // After splitting, A should not point directly to D
    let blockA' = Map.find labelA cfg'.Blocks
    match blockA'.Terminator with
    | Branch (_, trueTarget, falseTarget) ->
        if trueTarget = labelD || falseTarget = labelD then
            Error "A should not point directly to D after split"
        else
            Ok ()
    | _ -> Error "Block A should still be a branch"

/// Loop with back edge - loop header has phi
///     A
///     |
///     v
///     B (phi) <--+
///     |         |
///     v         |
///     C (branch)
///    / \        |
///   D   +-------+
///
/// C->B: C has 2 succ, B has 2 preds -> CRITICAL
/// C->D: C has 2 succ, D has 1 pred -> NOT critical
let testLoopBackEdge () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let condReg = vreg 0
    let phiDest = vreg 1

    let blockA = makeJumpBlock labelA labelB
    let phiInstr = Phi (phiDest, [(IntConst 0L, labelA); (IntConst 1L, labelC)])
    let blockB = makePhiBlock labelB [phiInstr] (Jump labelC)
    let blockC = makeBranchBlock labelC condReg labelB labelD  // Loop back or exit
    let blockD = makeRetBlock labelD (IntConst 0L)

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]
    let labelGen = freshLabelGen ()

    let (cfg', _) = SSA_Destruction.splitCriticalEdges cfg labelGen

    // C->B is critical and should be split
    if Map.count cfg'.Blocks <= 4 then
        Error $"Expected more than 4 blocks after split, got {Map.count cfg'.Blocks}"
    else
        // Verify C no longer points directly to B on its true branch
        let blockC' = Map.find labelC cfg'.Blocks
        match blockC'.Terminator with
        | Branch (_, trueTarget, _) when trueTarget = labelB ->
            Error "C should not point directly to B after split"
        | Branch _ -> Ok ()
        | _ -> Error "Block C should still be a branch"

/// Phi nodes should be updated when edges are split
let testPhiNodesUpdated () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelD = makeLabel "D"

    let condReg = vreg 0
    let phiDest = vreg 1

    // A branches to B (true) or D (false) - A->D is critical
    let blockA = makeBranchBlock labelA condReg labelB labelD
    let blockB = makeJumpBlock labelB labelD
    // D has phi from both A and B
    let phiInstr = Phi (phiDest, [(IntConst 1L, labelA); (IntConst 2L, labelB)])
    let blockD = makePhiBlock labelD [phiInstr] (Ret (Register phiDest))

    let cfg = makeCFG labelA [blockA; blockB; blockD]
    let labelGen = freshLabelGen ()

    let (cfg', _) = SSA_Destruction.splitCriticalEdges cfg labelGen

    // Verify phi in D is updated to reference new block instead of A
    let blockD' = Map.find labelD cfg'.Blocks
    match blockD'.Instrs with
    | [Phi (_, sources)] ->
        // One source should be from B, one from the new split block (not A)
        let hasA = sources |> List.exists (fun (_, lbl) -> lbl = labelA)
        let hasB = sources |> List.exists (fun (_, lbl) -> lbl = labelB)
        if hasA then
            Error "Phi should not reference A after split (should reference new block)"
        else if not hasB then
            Error "Phi should still reference B"
        else
            Ok ()
    | _ -> Error "Block D should have exactly one phi"

/// Run all critical edge tests
let runAll () : TestResult =
    let tests = [
        ("diamond no critical edges", testDiamondNoCriticalEdges)
        ("single critical edge", testSingleCriticalEdge)
        ("multiple critical edges", testMultipleCriticalEdges)
        ("loop back edge", testLoopBackEdge)
        ("phi nodes updated", testPhiNodesUpdated)
    ]

    let rec runTests tests =
        match tests with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Error e -> Error $"FAIL: {name}: {e}"
            | Ok () -> runTests rest

    runTests tests
