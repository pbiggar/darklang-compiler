// PhiResolutionTests.fs - Unit tests for phi resolution in SSA-based register allocation
//
// Tests the conversion of phi nodes into parallel moves at predecessor block exits.
// Key test cases:
// - Single phi, two predecessors
// - Multiple phis in same block (need parallel moves)
// - Phi with swap (cycle that needs temp register)
// - Phi where source is immediate
// - Phi where some predecessors share the same source value

module PhiResolutionTests

open LIR

/// Test result type
type TestResult = Result<unit, string>

/// Create a label
let makeLabel (name: string) = Label name

/// Create a virtual register
let vr (n: int) = Virtual n

/// Create a VReg operand
let vreg (n: int) = Reg (Virtual n)

/// Create a physical register
let phys (r: PhysReg) = Physical r

/// Create a simple basic block with a jump terminator
let makeJumpBlock (label: Label) (instrs: Instr list) (target: Label) : BasicBlock =
    { Label = label; Instrs = instrs; Terminator = Jump target }

/// Create a basic block with branch terminator
let makeBranchBlock (label: Label) (instrs: Instr list) (cond: Reg) (trueTarget: Label) (falseTarget: Label) : BasicBlock =
    { Label = label; Instrs = instrs; Terminator = Branch (cond, trueTarget, falseTarget) }

/// Create a basic block with return terminator
let makeRetBlock (label: Label) (instrs: Instr list) : BasicBlock =
    { Label = label; Instrs = instrs; Terminator = Ret }

/// Build a CFG from a list of blocks
let makeCFG (entry: Label) (blocks: BasicBlock list) : CFG =
    let blockMap = blocks |> List.map (fun b -> (b.Label, b)) |> Map.ofList
    { Entry = entry; Blocks = blockMap }

/// Check if a block contains any phi instructions
let hasPhiNodes (block: BasicBlock) : bool =
    block.Instrs |> List.exists (fun instr ->
        match instr with
        | Phi _ -> true
        | _ -> false)

/// Count moves in a block (excluding phi nodes)
let countMoves (block: BasicBlock) : int =
    block.Instrs |> List.filter (fun instr ->
        match instr with
        | Mov _ -> true
        | _ -> false) |> List.length

/// Empty float allocation for tests that don't use float phis
let emptyFloatAllocation : RegisterAllocation.FAllocationResult =
    { FMapping = Map.empty; UsedCalleeSavedF = [] }

/// Check if a block has a specific move instruction
let hasMove (block: BasicBlock) (dest: Reg) (src: Operand) : bool =
    block.Instrs |> List.exists (fun instr ->
        match instr with
        | Mov (d, s) -> d = dest && s = src
        | _ -> false)

// =============================================================================
// Test Cases
// =============================================================================

/// Test: Simple phi with two predecessors
/// Diamond CFG:
///     A
///    / \
///   B   C
///    \ /
///     D (phi v3 = [v1 from B, v2 from C])
///
/// After resolution:
/// - B should have: Mov(v3, v1)
/// - C should have: Mov(v3, v2)
/// - D should have no phi nodes
let testSimplePhiResolution () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let blockA = makeBranchBlock labelA [Mov (vr 0, Imm 1L)] (vr 0) labelB labelC
    let blockB = makeJumpBlock labelB [Mov (vr 1, Imm 10L)] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 2, Imm 20L)] labelD
    let phiInstr = Phi (vr 3, [(vreg 1, labelB); (vreg 2, labelC)], None)
    let blockD = makeRetBlock labelD [phiInstr; Mov (vr 4, vreg 3)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    // Simple allocation: v1->X1, v2->X2, v3->X3, v4->X4
    let allocation : Map<int, RegisterAllocation.Allocation> =
        Map.ofList [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
            (4, RegisterAllocation.PhysReg X5)
        ]

    let resolvedCFG = RegisterAllocation.resolvePhiNodes cfg allocation emptyFloatAllocation

    // Check D has no phi nodes
    let blockD' = Map.find labelD resolvedCFG.Blocks
    if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
    // Check B has a move v3 <- v1 (X4 <- X2)
    else
        let blockB' = Map.find labelB resolvedCFG.Blocks
        if not (hasMove blockB' (phys X4) (Reg (phys X2))) then
            Error "Block B should have Mov(X4, X2) for phi resolution"
        else
            let blockC' = Map.find labelC resolvedCFG.Blocks
            if not (hasMove blockC' (phys X4) (Reg (phys X3))) then
                Error "Block C should have Mov(X4, X3) for phi resolution"
            else
                Ok ()

/// Test: Multiple phis needing parallel moves
/// Two phis in the same block that need proper sequencing:
///     A
///    / \
///   B   C
///    \ /
///     D (phi v3 = [v1 from B, v2 from C])
///       (phi v4 = [v5 from B, v6 from C])
///
/// The moves at B should be done in parallel (v3←v1, v4←v5)
let testMultiplePhisParallel () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let blockA = makeBranchBlock labelA [Mov (vr 0, Imm 1L)] (vr 0) labelB labelC
    let blockB = makeJumpBlock labelB [Mov (vr 1, Imm 10L); Mov (vr 5, Imm 50L)] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 2, Imm 20L); Mov (vr 6, Imm 60L)] labelD
    let phi1 = Phi (vr 3, [(vreg 1, labelB); (vreg 2, labelC)], None)
    let phi2 = Phi (vr 4, [(vreg 5, labelB); (vreg 6, labelC)], None)
    let blockD = makeRetBlock labelD [phi1; phi2]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    let allocation : Map<int, RegisterAllocation.Allocation> =
        Map.ofList [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
            (4, RegisterAllocation.PhysReg X5)
            (5, RegisterAllocation.PhysReg X6)
            (6, RegisterAllocation.PhysReg X7)
        ]

    let resolvedCFG = RegisterAllocation.resolvePhiNodes cfg allocation emptyFloatAllocation

    // D should have no phi nodes
    let blockD' = Map.find labelD resolvedCFG.Blocks
    if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
    // B should have 2 moves (possibly more if there are cycles)
    else
        let blockB' = Map.find labelB resolvedCFG.Blocks
        let moveCount = countMoves blockB'
        // Original had 2 movs (for v1, v5), plus 2 more for phi resolution
        if moveCount < 4 then
            Error $"Block B should have at least 4 moves, got {moveCount}"
        else
            Ok ()

/// Test: Phi with swap (creates a cycle)
/// When two phis swap values, we need temp register:
///     A
///    / \
///   B   C
///    \ /
///     D (phi v1' = [v2 from B, ...]
///       (phi v2' = [v1 from B, ...]
///
/// At B: need to do v1'←v2 and v2'←v1 simultaneously (swap)
let testPhiSwap () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let blockA = makeBranchBlock labelA [Mov (vr 0, Imm 1L)] (vr 0) labelB labelC
    // B defines v1 and v2, D swaps them into v3 and v4
    let blockB = makeJumpBlock labelB [Mov (vr 1, Imm 10L); Mov (vr 2, Imm 20L)] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 5, Imm 50L); Mov (vr 6, Imm 60L)] labelD
    // The swap: v3 gets v2 (from B), v4 gets v1 (from B)
    // If v3→X1 and v4→X2, and v1→X1 and v2→X2, then this is a direct swap
    let phi1 = Phi (vr 3, [(vreg 2, labelB); (vreg 5, labelC)], None)  // v3 ← v2 from B
    let phi2 = Phi (vr 4, [(vreg 1, labelB); (vreg 6, labelC)], None)  // v4 ← v1 from B
    let blockD = makeRetBlock labelD [phi1; phi2]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    // Allocate to create a swap: v1→X1, v2→X2, v3→X1, v4→X2
    // From B: X1←X2, X2←X1 (swap!)
    let allocation : Map<int, RegisterAllocation.Allocation> =
        Map.ofList [
            (0, RegisterAllocation.PhysReg X3)
            (1, RegisterAllocation.PhysReg X1)  // v1 in X1
            (2, RegisterAllocation.PhysReg X2)  // v2 in X2
            (3, RegisterAllocation.PhysReg X1)  // v3 wants X1 (gets v2=X2)
            (4, RegisterAllocation.PhysReg X2)  // v4 wants X2 (gets v1=X1)
            (5, RegisterAllocation.PhysReg X1)
            (6, RegisterAllocation.PhysReg X2)
        ]

    let resolvedCFG = RegisterAllocation.resolvePhiNodes cfg allocation emptyFloatAllocation

    // D should have no phi nodes
    let blockD' = Map.find labelD resolvedCFG.Blocks
    if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
    else
        // B should have moves for the swap (3 moves: save to temp, move, restore from temp)
        let blockB' = Map.find labelB resolvedCFG.Blocks
        let moveCount = countMoves blockB'
        // Original 2 movs + swap needs 3 moves (using temp)
        if moveCount < 5 then
            Error $"Block B should have at least 5 moves for swap, got {moveCount}"
        else
            Ok ()

/// Test: Phi with immediate source
/// When a phi source is an immediate, we should move the immediate directly
///     A
///    / \
///   B   C
///    \ /
///     D (phi v1 = [Imm 10 from B, v2 from C])
let testPhiWithImmediate () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let blockA = makeBranchBlock labelA [Mov (vr 0, Imm 1L)] (vr 0) labelB labelC
    let blockB = makeJumpBlock labelB [] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 2, Imm 20L)] labelD
    // Phi with immediate from B
    let phiInstr = Phi (vr 1, [(Imm 10L, labelB); (vreg 2, labelC)], None)
    let blockD = makeRetBlock labelD [phiInstr; Mov (vr 3, vreg 1)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    let allocation : Map<int, RegisterAllocation.Allocation> =
        Map.ofList [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
        ]

    let resolvedCFG = RegisterAllocation.resolvePhiNodes cfg allocation emptyFloatAllocation

    // D should have no phi nodes
    let blockD' = Map.find labelD resolvedCFG.Blocks
    if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
    else
        // B should have a move of immediate to X2
        let blockB' = Map.find labelB resolvedCFG.Blocks
        if not (hasMove blockB' (phys X2) (Imm 10L)) then
            Error "Block B should have Mov(X2, Imm 10) for phi resolution"
        else
            Ok ()

/// Test: Loop phi
/// Loop back edge needs phi resolution
///     A
///     |
///     v
///     B (phi v1 = [v0 from A, v2 from C])
///     |
///     v
///     C (v2 = v1 + 1, branch back to B or exit)
///     |
///     v
///     D
let testLoopPhi () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let blockA = makeJumpBlock labelA [Mov (vr 0, Imm 0L)] labelB
    let phiInstr = Phi (vr 1, [(vreg 0, labelA); (vreg 2, labelC)], None)
    let blockB = makeJumpBlock labelB [phiInstr] labelC
    let blockC = makeBranchBlock labelC [Add (vr 2, vr 1, Imm 1L); Mov (vr 99, Imm 1L)] (vr 99) labelB labelD
    let blockD = makeRetBlock labelD []

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    let allocation : Map<int, RegisterAllocation.Allocation> =
        Map.ofList [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (99, RegisterAllocation.PhysReg X4)
        ]

    let resolvedCFG = RegisterAllocation.resolvePhiNodes cfg allocation emptyFloatAllocation

    // B should have no phi nodes
    let blockB' = Map.find labelB resolvedCFG.Blocks
    if hasPhiNodes blockB' then
        Error "Block B should not have phi nodes after resolution"
    else
        // A should have move v1 ← v0 (X2 ← X1)
        let blockA' = Map.find labelA resolvedCFG.Blocks
        if not (hasMove blockA' (phys X2) (Reg (phys X1))) then
            Error "Block A should have Mov(X2, X1) for phi resolution"
        else
            // C should have move v1 ← v2 (X2 ← X3)
            let blockC' = Map.find labelC resolvedCFG.Blocks
            if not (hasMove blockC' (phys X2) (Reg (phys X3))) then
                Error "Block C should have Mov(X2, X3) for phi resolution"
            else
                Ok ()

/// Run all phi resolution tests
let runAll () : TestResult =
    let tests = [
        ("simple phi resolution", testSimplePhiResolution)
        ("multiple phis parallel", testMultiplePhisParallel)
        ("phi swap", testPhiSwap)
        ("phi with immediate", testPhiWithImmediate)
        ("loop phi", testLoopPhi)
    ]

    let rec runTests tests =
        match tests with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Error e -> Error $"FAIL: {name}: {e}"
            | Ok () -> runTests rest

    runTests tests
