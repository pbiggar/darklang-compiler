// SSALivenessTests.fs - Unit tests for SSA liveness analysis
//
// Tests the liveness analysis with phi nodes in SSA form.
// Key insights for SSA liveness:
// - Phi dests are defined at block entry
// - Phi sources are used at PREDECESSOR exits (not at the phi's block)
// - This affects LiveOut calculation for predecessors

module SSALivenessTests

open LIR

/// Test result type
type TestResult = Result<unit, string>

/// Create a label
let makeLabel (name: string) = Label name

/// Create a virtual register
let vr (n: int) = Virtual n

/// Create a VReg operand
let vreg (n: int) = Reg (Virtual n)

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

/// Check if a VReg is in the LiveIn set
let isLiveIn (liveness: Map<Label, RegisterAllocation.BlockLiveness>) (label: Label) (vregId: int) : bool =
    match Map.tryFind label liveness with
    | Some bl -> Set.contains vregId bl.LiveIn
    | None -> false

/// Check if a VReg is in the LiveOut set
let isLiveOut (liveness: Map<Label, RegisterAllocation.BlockLiveness>) (label: Label) (vregId: int) : bool =
    match Map.tryFind label liveness with
    | Some bl -> Set.contains vregId bl.LiveOut
    | None -> false

// =============================================================================
// Test Cases
// =============================================================================

/// Test: Phi destination is defined at block entry
/// Simple diamond CFG with phi at merge point:
///     A (def v0)
///    / \
///   B   C
///    \ /
///     D (phi v1 = [v0 from B, v0 from C])
///     |
///     E (use v1)
///
/// v0 should be live-out of B and C (used by phi in D)
/// v1 should NOT be live-in to D (it's defined there by phi)
let testPhiDefAtBlockEntry () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"
    let labelE = makeLabel "E"

    // v0 is defined in A, used by phi in D
    // v1 is defined by phi in D, used in E
    let blockA = makeBranchBlock labelA [Mov (vr 0, Imm 1L)] (vr 0) labelB labelC
    let blockB = makeJumpBlock labelB [] labelD
    let blockC = makeJumpBlock labelC [] labelD
    let phiInstr = Phi (vr 1, [(vreg 0, labelB); (vreg 0, labelC)])
    // Use v1 and then return
    let blockD = makeJumpBlock labelD [phiInstr; Mov (vr 2, vreg 1)] labelE
    let blockE = makeRetBlock labelE [Mov (vr 3, vreg 2)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD; blockE]
    let liveness = RegisterAllocation.computeLiveness cfg

    // v0 should be live-out of B (used by phi in D)
    if not (isLiveOut liveness labelB 0) then
        Error "v0 should be live-out of B (used by phi in D)"
    // v0 should be live-out of C (used by phi in D)
    else if not (isLiveOut liveness labelC 0) then
        Error "v0 should be live-out of C (used by phi in D)"
    // v1 should NOT be live-in to D (it's defined there by phi)
    else if isLiveIn liveness labelD 1 then
        Error "v1 should NOT be live-in to D (it's defined there by phi)"
    // v1 should be live-out of D (used in E via v2)
    else if not (isLiveOut liveness labelD 2) then
        Error "v2 should be live-out of D (used in E)"
    else
        Ok ()

/// Test: Phi sources are live at predecessor exits, not at phi's block
/// Diamond with different values from each branch:
///     A
///    / \
///   B   C
///  (v0) (v1)
///    \ /
///     D (phi v2 = [v0 from B, v1 from C])
///
/// v0 should be live-out of B only (not C)
/// v1 should be live-out of C only (not B)
let testPhiSourceLivenessScoped () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let condReg = vr 99

    // v0 defined and used only in B path
    // v1 defined and used only in C path
    let blockA = makeBranchBlock labelA [Mov (condReg, Imm 1L)] condReg labelB labelC
    let blockB = makeJumpBlock labelB [Mov (vr 0, Imm 10L)] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 1, Imm 20L)] labelD
    let phiInstr = Phi (vr 2, [(vreg 0, labelB); (vreg 1, labelC)])
    // Use v2 and return
    let blockD = makeRetBlock labelD [phiInstr; Mov (vr 3, vreg 2)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]
    let liveness = RegisterAllocation.computeLiveness cfg

    // v0 should be live-out of B (used by phi from B)
    if not (isLiveOut liveness labelB 0) then
        Error "v0 should be live-out of B (used by phi)"
    // v0 should NOT be live-out of C (not used from C path)
    else if isLiveOut liveness labelC 0 then
        Error "v0 should NOT be live-out of C (not in phi from C)"
    // v1 should be live-out of C (used by phi from C)
    else if not (isLiveOut liveness labelC 1) then
        Error "v1 should be live-out of C (used by phi)"
    // v1 should NOT be live-out of B (not used from B path)
    else if isLiveOut liveness labelB 1 then
        Error "v1 should NOT be live-out of B (not in phi from B)"
    else
        Ok ()

/// Test: Multiple phis in same block
/// Diamond with multiple phi nodes:
///     A
///    / \
///   B   C
///    \ /
///     D (phi v2 = [v0 from B, v1 from C])
///       (phi v3 = [v4 from B, v5 from C])
///
/// All phi sources should be live at correct predecessor exits
let testMultiplePhisSameBlock () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let condReg = vr 99

    // B defines v0, v4
    // C defines v1, v5
    let blockA = makeBranchBlock labelA [Mov (condReg, Imm 1L)] condReg labelB labelC
    let blockB = makeJumpBlock labelB [Mov (vr 0, Imm 10L); Mov (vr 4, Imm 40L)] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 1, Imm 20L); Mov (vr 5, Imm 50L)] labelD
    let phi1 = Phi (vr 2, [(vreg 0, labelB); (vreg 1, labelC)])
    let phi2 = Phi (vr 3, [(vreg 4, labelB); (vreg 5, labelC)])
    // Use both phi results with Add
    let useInstr = Add (vr 6, vr 2, vreg 3)
    let blockD = makeRetBlock labelD [phi1; phi2; useInstr; Mov (vr 7, vreg 6)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]
    let liveness = RegisterAllocation.computeLiveness cfg

    // v0 and v4 should be live-out of B
    if not (isLiveOut liveness labelB 0) then
        Error "v0 should be live-out of B"
    else if not (isLiveOut liveness labelB 4) then
        Error "v4 should be live-out of B"
    // v1 and v5 should be live-out of C
    else if not (isLiveOut liveness labelC 1) then
        Error "v1 should be live-out of C"
    else if not (isLiveOut liveness labelC 5) then
        Error "v5 should be live-out of C"
    // Neither phi dest should be live-in to D
    else if isLiveIn liveness labelD 2 then
        Error "v2 should NOT be live-in to D (defined by phi)"
    else if isLiveIn liveness labelD 3 then
        Error "v3 should NOT be live-in to D (defined by phi)"
    else
        Ok ()

/// Test: Loop with phi
///     A (def v0)
///     |
///     v
///     B (phi v1 = [v0 from A, v2 from C])
///     |
///     v
///     C (def v2, use v1)
///     |
///     v  (loop back to B or exit to D)
///     D
///
/// v2 should be live-out of C (used by phi in B)
/// v1 should be live-out of B (used in C)
let testLoopPhi () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let condReg = vr 99

    let blockA = makeJumpBlock labelA [Mov (vr 0, Imm 0L)] labelB
    let phiInstr = Phi (vr 1, [(vreg 0, labelA); (vreg 2, labelC)])
    let blockB = makeJumpBlock labelB [phiInstr] labelC
    // C uses v1 to compute v2, and might loop back
    let blockC = makeBranchBlock labelC [Add (vr 2, vr 1, Imm 1L); Mov (condReg, Imm 1L)] condReg labelB labelD
    // D uses v1 that was passed through from C (but v1 is from B's phi)
    let blockD = makeRetBlock labelD [Mov (vr 3, vreg 1)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]
    let liveness = RegisterAllocation.computeLiveness cfg

    // v0 should be live-out of A (used by phi in B)
    if not (isLiveOut liveness labelA 0) then
        Error "v0 should be live-out of A (used by phi in B)"
    // v2 should be live-out of C (used by phi in B on loop back)
    else if not (isLiveOut liveness labelC 2) then
        Error "v2 should be live-out of C (used by phi in B)"
    // v1 should be live-out of B (used in C)
    else if not (isLiveOut liveness labelB 1) then
        Error "v1 should be live-out of B (used in C)"
    else
        Ok ()

/// Run all SSA liveness tests
let runAll () : TestResult =
    let tests = [
        ("phi def at block entry", testPhiDefAtBlockEntry)
        ("phi source liveness scoped", testPhiSourceLivenessScoped)
        ("multiple phis same block", testMultiplePhisSameBlock)
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
