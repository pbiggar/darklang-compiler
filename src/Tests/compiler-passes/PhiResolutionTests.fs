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
let makeLabel (name: string) = LIR.Label name

/// Create a virtual register
let vr (n: int) = LIR.Virtual n

/// Create a VReg operand
let vreg (n: int) = Reg (LIR.Virtual n)

/// Create a physical register
let phys (r: PhysReg) = LIR.Physical r

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

/// Extract label name for error messages
let labelName (label: Label) : string =
    match label with
    | LIR.Label name -> name

/// Lookup a block in the CFG and return a test failure when missing
let withBlock (label: Label) (cfg: CFG) (f: BasicBlock -> TestResult) : TestResult =
    match Map.tryFind label cfg.Blocks with
    | Some block -> f block
    | None -> Error $"Missing block {labelName label}"

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
    { Domain = { Ids = [||]; IndexOf = [||]; IndexOffset = 0; WordCount = 0 }
      Allocations = [||]
      UsedCalleeSavedF = [] }

let buildAllocationResult
    (domain: RegisterAllocation.VRegDomain)
    (pairs: (int * RegisterAllocation.Allocation) list)
    : RegisterAllocation.AllocationResult =
    let allocations = Array.create domain.Ids.Length None
    for (vregId, alloc) in pairs do
        match domain.Ids |> Array.tryFindIndex (fun id -> id = vregId) with
        | Some idx -> allocations.[idx] <- Some alloc
        | None -> ()
    { Domain = domain; Allocations = allocations; StackSize = 0; UsedCalleeSaved = [] }

let cfgFromBlocks (entry: Label) (labels: Label array) (blocks: BasicBlock array) : CFG =
    { Entry = entry; Blocks = Array.zip labels blocks |> Map.ofArray }

let resolvePhiCFG (cfg: CFG) (allocations: (int * RegisterAllocation.Allocation) list) : CFG =
    let (domain, blockIndex, _liveness) = RegisterAllocation.computeLivenessBits cfg
    let blocks = blockIndex.Labels |> Array.map (fun label -> Map.find label cfg.Blocks)
    let allocationResult = buildAllocationResult domain allocations
    let resolvedBlocks = RegisterAllocation.resolvePhiNodes blockIndex blocks allocationResult emptyFloatAllocation
    cfgFromBlocks cfg.Entry blockIndex.Labels resolvedBlocks

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
    let allocation =
        [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
            (4, RegisterAllocation.PhysReg X5)
        ]

    let resolvedCFG = resolvePhiCFG cfg allocation

    // Check D has no phi nodes
    withBlock labelD resolvedCFG (fun blockD' ->
        if hasPhiNodes blockD' then
            Error "Block D should not have phi nodes after resolution"
        // Check B has a move v3 <- v1 (X4 <- X2)
        else
            withBlock labelB resolvedCFG (fun blockB' ->
                if not (hasMove blockB' (phys X4) (Reg (phys X2))) then
                    Error "Block B should have Mov(X4, X2) for phi resolution"
                else
                    withBlock labelC resolvedCFG (fun blockC' ->
                        if not (hasMove blockC' (phys X4) (Reg (phys X3))) then
                            Error "Block C should have Mov(X4, X3) for phi resolution"
                        else
                            Ok ())))

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
    let blockD = makeRetBlock labelD [phi1; phi2; Mov (vr 7, vreg 3); Mov (vr 8, vreg 4)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    let allocation =
        [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
            (4, RegisterAllocation.PhysReg X5)
            (5, RegisterAllocation.PhysReg X6)
            (6, RegisterAllocation.PhysReg X7)
        ]

    let resolvedCFG = resolvePhiCFG cfg allocation

    // D should have no phi nodes
    withBlock labelD resolvedCFG (fun blockD' ->
        if hasPhiNodes blockD' then
            Error "Block D should not have phi nodes after resolution"
        // B should have 2 moves (possibly more if there are cycles)
        else
            withBlock labelB resolvedCFG (fun blockB' ->
                let moveCount = countMoves blockB'
                // Original had 2 movs (for v1, v5), plus 2 more for phi resolution
                if moveCount < 4 then
                    Error $"Block B should have at least 4 moves, got {moveCount}"
                else
                    Ok ()))

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
    let blockD = makeRetBlock labelD [phi1; phi2; Mov (vr 7, vreg 3); Mov (vr 8, vreg 4)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    // Allocate to create a swap: v1→X1, v2→X2, v3→X1, v4→X2
    // From B: X1←X2, X2←X1 (swap!)
    let allocation =
        [
            (0, RegisterAllocation.PhysReg X3)
            (1, RegisterAllocation.PhysReg X1)  // v1 in X1
            (2, RegisterAllocation.PhysReg X2)  // v2 in X2
            (3, RegisterAllocation.PhysReg X1)  // v3 wants X1 (gets v2=X2)
            (4, RegisterAllocation.PhysReg X2)  // v4 wants X2 (gets v1=X1)
            (5, RegisterAllocation.PhysReg X1)
            (6, RegisterAllocation.PhysReg X2)
        ]

    let resolvedCFG = resolvePhiCFG cfg allocation

    // D should have no phi nodes
    withBlock labelD resolvedCFG (fun blockD' ->
        if hasPhiNodes blockD' then
            Error "Block D should not have phi nodes after resolution"
        else
            // B should have moves for the swap (3 moves: save to temp, move, restore from temp)
            withBlock labelB resolvedCFG (fun blockB' ->
                let moveCount = countMoves blockB'
                // Original 2 movs + swap needs 3 moves (using temp)
                if moveCount < 5 then
                    Error $"Block B should have at least 5 moves for swap, got {moveCount}"
                else
                    Ok ()))

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

    let allocation =
        [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
        ]

    let resolvedCFG = resolvePhiCFG cfg allocation

    // D should have no phi nodes
    withBlock labelD resolvedCFG (fun blockD' ->
        if hasPhiNodes blockD' then
            Error "Block D should not have phi nodes after resolution"
        else
            // B should have a move of immediate to X2
            withBlock labelB resolvedCFG (fun blockB' ->
                if not (hasMove blockB' (phys X2) (Imm 10L)) then
                    Error "Block B should have Mov(X2, Imm 10) for phi resolution"
                else
                    Ok ()))

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

    let allocation =
        [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (99, RegisterAllocation.PhysReg X4)
        ]

    let resolvedCFG = resolvePhiCFG cfg allocation

    // B should have no phi nodes
    withBlock labelB resolvedCFG (fun blockB' ->
        if hasPhiNodes blockB' then
            Error "Block B should not have phi nodes after resolution"
        else
            // A should have move v1 ← v0 (X2 ← X1)
            withBlock labelA resolvedCFG (fun blockA' ->
                if not (hasMove blockA' (phys X2) (Reg (phys X1))) then
                    Error "Block A should have Mov(X2, X1) for phi resolution"
                else
                    // C should have move v1 ← v2 (X2 ← X3)
                    withBlock labelC resolvedCFG (fun blockC' ->
                        if not (hasMove blockC' (phys X2) (Reg (phys X3))) then
                            Error "Block C should have Mov(X2, X3) for phi resolution"
                        else
                            Ok ())))

/// Test: Dead phi destinations should not emit moves
let testDeadPhiPruned () : TestResult =
    let labelA = makeLabel "A"
    let labelB = makeLabel "B"
    let labelC = makeLabel "C"
    let labelD = makeLabel "D"

    let blockA = makeBranchBlock labelA [Mov (vr 0, Imm 1L)] (vr 0) labelB labelC
    let blockB = makeJumpBlock labelB [Mov (vr 1, Imm 10L); Mov (vr 5, Imm 50L)] labelD
    let blockC = makeJumpBlock labelC [Mov (vr 2, Imm 20L); Mov (vr 6, Imm 60L)] labelD
    let livePhi = Phi (vr 3, [(vreg 1, labelB); (vreg 2, labelC)], None)
    let deadPhi = Phi (vr 4, [(vreg 5, labelB); (vreg 6, labelC)], None)
    let blockD = makeRetBlock labelD [livePhi; deadPhi; Mov (vr 7, vreg 3)]

    let cfg = makeCFG labelA [blockA; blockB; blockC; blockD]

    let allocation =
        [
            (0, RegisterAllocation.PhysReg X1)
            (1, RegisterAllocation.PhysReg X2)
            (2, RegisterAllocation.PhysReg X3)
            (3, RegisterAllocation.PhysReg X4)
            (4, RegisterAllocation.PhysReg X5)
            (5, RegisterAllocation.PhysReg X6)
            (6, RegisterAllocation.PhysReg X7)
            (7, RegisterAllocation.PhysReg X19)
        ]

    let resolvedCFG = resolvePhiCFG cfg allocation
    withBlock labelD resolvedCFG (fun blockD' ->
        if hasPhiNodes blockD' then
            Error "Block D should not have phi nodes after resolution"
        else
            withBlock labelB resolvedCFG (fun blockB' ->
                if hasMove blockB' (phys X5) (Reg (phys X6)) then
                    Error "Block B should not move dead phi destination"
                else
                    withBlock labelC resolvedCFG (fun blockC' ->
                        if hasMove blockC' (phys X5) (Reg (phys X7)) then
                            Error "Block C should not move dead phi destination"
                        else
                            Ok ())))

/// Test: Loop phi should be coalesced to avoid backedge moves
let testLoopPhiCoalesced () : TestResult =
    let labelEntry = makeLabel "loop_entry"
    let labelLoop = makeLabel "loop_body"
    let labelBack = makeLabel "loop_back"
    let labelExit = makeLabel "loop_exit"

    let blockEntry = makeJumpBlock labelEntry [Mov (vr 3, Imm 0L)] labelLoop
    let phiInstr = Phi (vr 0, [(vreg 3, labelEntry); (vreg 2, labelBack)], None)
    let loopInstrs = [
        phiInstr
        Cmp (vr 0, Imm 10L)
        Cset (vr 4, EQ)
        Add (vr 1, vr 0, Imm 1L)
        Mov (vr 2, vreg 1)
    ]
    let blockLoop = makeBranchBlock labelLoop loopInstrs (vr 4) labelExit labelBack
    let blockBack = makeJumpBlock labelBack [] labelLoop
    let blockExit = makeRetBlock labelExit []

    let cfg = makeCFG labelEntry [blockEntry; blockLoop; blockBack; blockExit]
    let func : LIR.Function = {
        Name = "phi_coalesce_loop"
        TypedParams = []
        CFG = cfg
        StackSize = 0
        UsedCalleeSaved = []
    }

    let arch = match Platform.detectArch () with Ok a -> a | Error _ -> Platform.ARM64
    let allocated = RegisterAllocation.allocateRegisters arch func
    withBlock labelBack allocated.CFG (fun backBlock ->
        if countMoves backBlock = 0 then
            Ok ()
        else
            Error "Backedge should not need moves when phi is coalesced")

let tests = [
    ("simple phi resolution", testSimplePhiResolution)
    ("multiple phis parallel", testMultiplePhisParallel)
    ("phi swap", testPhiSwap)
    ("phi with immediate", testPhiWithImmediate)
    ("loop phi", testLoopPhi)
    ("dead phi pruned", testDeadPhiPruned)
    ("loop phi coalesced", testLoopPhiCoalesced)
]

/// Run all phi resolution tests
let runAll () : TestResult =
    let rec runTests tests =
        match tests with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Error e -> Error $"FAIL: {name}: {e}"
            | Ok () -> runTests rest

    runTests tests
