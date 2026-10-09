(*
   Tests the conversion of phi nodes into parallel moves at predecessor block exits.
   Key test cases:
   - Single phi, two predecessors
   - Multiple phis in same block (need parallel moves)
   - Phi with swap (cycle that needs temp register)
   - Phi where source is immediate
   - Phi where some predecessors share the same source value
   - Floating-point loop phi coalescing through a direct feeder move
   - Floating-point phi coalescing that preserves an existing return-register allocation
   - Caller-save population around allocated call and argument-move sequences
*)
(* PhiResolutionTests.ml - Unit tests for phi resolution in SSA-based register allocation *)
[@@@warning "-4-42"]

open Dark_compiler
open LIR

(*
   Test result type
*)
type testResult = (unit, string) result

(*
   Create a label
*)
let makeLabel name = LIR.Label name

(*
   Create a virtual register
*)
let vr n = LIR.Virtual n

(*
   Create a VReg operand
*)
let vreg n = Reg (LIR.Virtual n)

(*
   Create a floating-point virtual register
*)
let fvr n = LIR.FVirtual n

(*
   Create a physical register
*)
let phys r = LIR.Physical r

(*
   Create a simple basic block with a jump terminator
*)
let makeJumpBlock label instrs target : basicBlock =
  { label; instrs; terminator = Jump target }

(*
   Create a basic block with branch terminator
*)
let makeBranchBlock label instrs cond trueTarget falseTarget : basicBlock =
  { label; instrs; terminator = Branch (cond, trueTarget, falseTarget) }

(*
   Create a basic block with return terminator
*)
let makeRetBlock label instrs : basicBlock = { label; instrs; terminator = Ret }

(*
   Build a CFG from a list of blocks
*)
let makeCFG entry blocks : cfg =
  {
    entry;
    blocks =
      LabelMap.of_list (List.map (fun (b : basicBlock) -> (b.label, b)) blocks);
  }

(*
   Extract label name for error messages
*)
let labelName (Label name) = name

(*
   Lookup a block in the CFG and return a test failure when missing
*)
let withBlock label (cfg : cfg) f =
  match LabelMap.find_opt label cfg.blocks with
  | Some block -> f block
  | None -> Error ("Missing block " ^ labelName label)

(*
   Check if a block contains any phi instructions
*)
let hasPhiNodes (block : basicBlock) =
  List.exists (function Phi _ -> true | _ -> false) block.instrs

(*
   Count moves in a block (excluding phi nodes)
*)
let countMoves (block : basicBlock) =
  List.length (List.filter (function Mov _ -> true | _ -> false) block.instrs)

(*
   Count floating-point moves in a block
*)
let countFloatMoves (block : basicBlock) =
  List.length
    (List.filter (function FMov _ -> true | _ -> false) block.instrs)

(*
   Empty float allocation for tests that don't use float phis
*)
let emptyFloatAllocation : FloatAllocation.fAllocationResult =
  {
    FloatAllocation.domain =
      {
        AllocationModel.ids = [||];
        indexOf = [||];
        indexOffset = 0;
        wordCount = 0;
      };
    allocations = [||];
    stackSize = 0;
    usedCalleeSavedF = [];
    spillScratchLeft = FVirtual 1000;
    spillScratchRight = FVirtual 1001;
    spillScratchThird = FVirtual 1002;
  }

module IntMap = Map.Make (Int)

let buildAllocationResult (domain : AllocationModel.vRegDomain) pairs :
    AllocationModel.allocationResult =
  let allocationById = IntMap.of_list pairs in
  let allocations =
    Array.map
      (fun vregId -> IntMap.find_opt vregId allocationById)
      domain.AllocationModel.ids
  in
  { AllocationModel.domain; allocations; stackSize = 0; usedCalleeSaved = [] }

let cfgFromBlocks entry labels blocks : cfg =
  {
    entry;
    blocks =
      LabelMap.of_list
        (Array.to_list
           (Array.map2 (fun label block -> (label, block)) labels blocks));
  }

let resolvePhiCFG (cfg : cfg) allocations =
  let domain, blockIndex, _liveness =
    RegisterLiveness.computeLivenessBits cfg
  in
  let blocks =
    Array.map
      (fun label -> LabelMap.find label cfg.blocks)
      blockIndex.AllocationModel.labels
  in
  let allocationResult = buildAllocationResult domain allocations in
  let resolvedBlocks =
    PhiResolution.resolvePhiNodes blockIndex blocks allocationResult
      emptyFloatAllocation
  in
  cfgFromBlocks cfg.entry blockIndex.AllocationModel.labels resolvedBlocks

(*
   Check if a block has a specific move instruction
*)
let hasMove (block : basicBlock) dest src =
  List.exists
    (function Mov (d, s) -> d = dest && s = src | _ -> false)
    block.instrs

let allocatedPairs pairs =
  List.map (fun (id, reg) -> (id, AllocationModel.PhysReg reg)) pairs

(*
   Test Cases
   Test: Simple phi with two predecessors
   Diamond CFG:
   / \
   B   C
   \ /
   D (phi v3 = [v1 from B, v2 from C])
   After resolution:
   - B should have: Mov(v3, v1)
   - C should have: Mov(v3, v2)
   - D should have no phi nodes
   Simple allocation: v1->X1, v2->X2, v3->X3, v4->X4
   Check D has no phi nodes
   Check B has a move v3 <- v1 (X4 <- X2)
*)
let testSimplePhiResolution () =
  let a = makeLabel "A" in
  let b = makeLabel "B" in
  let c = makeLabel "C" in
  let d = makeLabel "D" in
  let blockA = makeBranchBlock a [ Mov (vr 0, Imm 1L) ] (vr 0) b c in
  let blockB = makeJumpBlock b [ Mov (vr 1, Imm 10L) ] d in
  let blockC = makeJumpBlock c [ Mov (vr 2, Imm 20L) ] d in
  let phiInstr = Phi (vr 3, [ (vreg 1, b); (vreg 2, c) ], None) in
  let blockD = makeRetBlock d [ phiInstr; Mov (vr 4, vreg 3) ] in
  let cfg = makeCFG a [ blockA; blockB; blockC; blockD ] in
  let allocation =
    allocatedPairs [ (0, X1); (1, X2); (2, X3); (3, X4); (4, X5) ]
  in
  let resolvedCFG = resolvePhiCFG cfg allocation in
  withBlock d resolvedCFG (fun blockD' ->
      if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
      else
        withBlock b resolvedCFG (fun blockB' ->
            if not (hasMove blockB' (phys X4) (Reg (phys X2))) then
              Error "Block B should have Mov(X4, X2) for phi resolution"
            else
              withBlock c resolvedCFG (fun blockC' ->
                  if not (hasMove blockC' (phys X4) (Reg (phys X3))) then
                    Error "Block C should have Mov(X4, X3) for phi resolution"
                  else Ok ())))

(*
   Test: Multiple phis needing parallel moves
   Two phis in the same block that need proper sequencing:
   / \
   B   C
   \ /
   D (phi v3 = [v1 from B, v2 from C])
   (phi v4 = [v5 from B, v6 from C])
   The moves at B should be done in parallel (v3←v1, v4←v5)
   D should have no phi nodes
   B should have 2 moves (possibly more if there are cycles)
   Original had 2 movs (for v1, v5), plus 2 more for phi resolution
*)
let testMultiplePhisParallel () =
  let a = makeLabel "A" in
  let b = makeLabel "B" in
  let c = makeLabel "C" in
  let d = makeLabel "D" in
  let blockA = makeBranchBlock a [ Mov (vr 0, Imm 1L) ] (vr 0) b c in
  let blockB = makeJumpBlock b [ Mov (vr 1, Imm 10L); Mov (vr 5, Imm 50L) ] d in
  let blockC = makeJumpBlock c [ Mov (vr 2, Imm 20L); Mov (vr 6, Imm 60L) ] d in
  let phi1 = Phi (vr 3, [ (vreg 1, b); (vreg 2, c) ], None) in
  let phi2 = Phi (vr 4, [ (vreg 5, b); (vreg 6, c) ], None) in
  let blockD =
    makeRetBlock d [ phi1; phi2; Mov (vr 7, vreg 3); Mov (vr 8, vreg 4) ]
  in
  let cfg = makeCFG a [ blockA; blockB; blockC; blockD ] in
  let allocation =
    allocatedPairs
      [ (0, X1); (1, X2); (2, X3); (3, X4); (4, X5); (5, X6); (6, X7) ]
  in
  let resolvedCFG = resolvePhiCFG cfg allocation in
  withBlock d resolvedCFG (fun blockD' ->
      if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
      else
        withBlock b resolvedCFG (fun blockB' ->
            let moveCount = countMoves blockB' in
            if moveCount < 4 then
              Error
                (Printf.sprintf "Block B should have at least 4 moves, got %d"
                   moveCount)
            else Ok ()))

(*
   Test: Phi with swap (creates a cycle)
   When two phis swap values, we need temp register:
   / \
   B   C
   \ /
   D (phi v1' = [v2 from B, ...]
   (phi v2' = [v1 from B, ...]
   At B: need to do v1'←v2 and v2'←v1 simultaneously (swap)
   B defines v1 and v2, D swaps them into v3 and v4
   The swap: v3 gets v2 (from B), v4 gets v1 (from B)
   If v3→X1 and v4→X2, and v1→X1 and v2→X2, then this is a direct swap
   v3 ← v2 from B
   v4 ← v1 from B
   Allocate to create a swap: v1→X1, v2→X2, v3→X1, v4→X2
   From B: X1←X2, X2←X1 (swap!)
   v1 in X1
   v2 in X2
   v3 wants X1 (gets v2=X2)
   v4 wants X2 (gets v1=X1)
   D should have no phi nodes
   B should have moves for the swap (3 moves: save to temp, move, restore from temp)
   Original 2 movs + swap needs 3 moves (using temp)
*)
let testPhiSwap () =
  let a = makeLabel "A" in
  let b = makeLabel "B" in
  let c = makeLabel "C" in
  let d = makeLabel "D" in
  let blockA = makeBranchBlock a [ Mov (vr 0, Imm 1L) ] (vr 0) b c in
  let blockB = makeJumpBlock b [ Mov (vr 1, Imm 10L); Mov (vr 2, Imm 20L) ] d in
  let blockC = makeJumpBlock c [ Mov (vr 5, Imm 50L); Mov (vr 6, Imm 60L) ] d in
  let phi1 = Phi (vr 3, [ (vreg 2, b); (vreg 5, c) ], None) in
  let phi2 = Phi (vr 4, [ (vreg 1, b); (vreg 6, c) ], None) in
  let blockD =
    makeRetBlock d [ phi1; phi2; Mov (vr 7, vreg 3); Mov (vr 8, vreg 4) ]
  in
  let cfg = makeCFG a [ blockA; blockB; blockC; blockD ] in
  let allocation =
    allocatedPairs
      [ (0, X3); (1, X1); (2, X2); (3, X1); (4, X2); (5, X1); (6, X2) ]
  in
  let resolvedCFG = resolvePhiCFG cfg allocation in
  withBlock d resolvedCFG (fun blockD' ->
      if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
      else
        withBlock b resolvedCFG (fun blockB' ->
            let moveCount = countMoves blockB' in
            if moveCount < 5 then
              Error
                (Printf.sprintf
                   "Block B should have at least 5 moves for swap, got %d"
                   moveCount)
            else Ok ()))

(*
   Test: Phi with immediate source
   When a phi source is an immediate, we should move the immediate directly
   / \
   B   C
   \ /
   D (phi v1 = [Imm 10 from B, v2 from C])
   Phi with immediate from B
   D should have no phi nodes
   B should have a move of immediate to X2
*)
let testPhiWithImmediate () =
  let a = makeLabel "A" in
  let b = makeLabel "B" in
  let c = makeLabel "C" in
  let d = makeLabel "D" in
  let blockA = makeBranchBlock a [ Mov (vr 0, Imm 1L) ] (vr 0) b c in
  let blockB = makeJumpBlock b [] d in
  let blockC = makeJumpBlock c [ Mov (vr 2, Imm 20L) ] d in
  let phiInstr = Phi (vr 1, [ (Imm 10L, b); (vreg 2, c) ], None) in
  let blockD = makeRetBlock d [ phiInstr; Mov (vr 3, vreg 1) ] in
  let cfg = makeCFG a [ blockA; blockB; blockC; blockD ] in
  let allocation = allocatedPairs [ (0, X1); (1, X2); (2, X3); (3, X4) ] in
  let resolvedCFG = resolvePhiCFG cfg allocation in
  withBlock d resolvedCFG (fun blockD' ->
      if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
      else
        withBlock b resolvedCFG (fun blockB' ->
            if not (hasMove blockB' (phys X2) (Imm 10L)) then
              Error "Block B should have Mov(X2, Imm 10) for phi resolution"
            else Ok ()))

(*
   Test: Loop phi
   Loop back edge needs phi resolution
   |
   v
   B (phi v1 = [v0 from A, v2 from C])
   C (v2 = v1 + 1, branch back to B or exit)
   D
   B should have no phi nodes
   A should have move v1 ← v0 (X2 ← X1)
   C should have move v1 ← v2 (X2 ← X3)
*)
let testLoopPhi () =
  let a = makeLabel "A" in
  let b = makeLabel "B" in
  let c = makeLabel "C" in
  let d = makeLabel "D" in
  let blockA = makeJumpBlock a [ Mov (vr 0, Imm 0L) ] b in
  let phiInstr = Phi (vr 1, [ (vreg 0, a); (vreg 2, c) ], None) in
  let blockB = makeJumpBlock b [ phiInstr ] c in
  let blockC =
    makeBranchBlock c
      [ Add (vr 2, vr 1, Imm 1L); Mov (vr 99, Imm 1L) ]
      (vr 99) b d
  in
  let blockD = makeRetBlock d [] in
  let cfg = makeCFG a [ blockA; blockB; blockC; blockD ] in
  let allocation = allocatedPairs [ (0, X1); (1, X2); (2, X3); (99, X4) ] in
  let resolvedCFG = resolvePhiCFG cfg allocation in
  withBlock b resolvedCFG (fun blockB' ->
      if hasPhiNodes blockB' then
        Error "Block B should not have phi nodes after resolution"
      else
        withBlock a resolvedCFG (fun blockA' ->
            if not (hasMove blockA' (phys X2) (Reg (phys X1))) then
              Error "Block A should have Mov(X2, X1) for phi resolution"
            else
              withBlock c resolvedCFG (fun blockC' ->
                  if not (hasMove blockC' (phys X2) (Reg (phys X3))) then
                    Error "Block C should have Mov(X2, X3) for phi resolution"
                  else Ok ())))

(*
   Test: Dead phi destinations should not emit moves
*)
let testDeadPhiPruned () =
  let a = makeLabel "A" in
  let b = makeLabel "B" in
  let c = makeLabel "C" in
  let d = makeLabel "D" in
  let blockA = makeBranchBlock a [ Mov (vr 0, Imm 1L) ] (vr 0) b c in
  let blockB = makeJumpBlock b [ Mov (vr 1, Imm 10L); Mov (vr 5, Imm 50L) ] d in
  let blockC = makeJumpBlock c [ Mov (vr 2, Imm 20L); Mov (vr 6, Imm 60L) ] d in
  let livePhi = Phi (vr 3, [ (vreg 1, b); (vreg 2, c) ], None) in
  let deadPhi = Phi (vr 4, [ (vreg 5, b); (vreg 6, c) ], None) in
  let blockD = makeRetBlock d [ livePhi; deadPhi; Mov (vr 7, vreg 3) ] in
  let cfg = makeCFG a [ blockA; blockB; blockC; blockD ] in
  let allocation =
    allocatedPairs
      [
        (0, X1); (1, X2); (2, X3); (3, X4); (4, X5); (5, X6); (6, X7); (7, X19);
      ]
  in
  let resolvedCFG = resolvePhiCFG cfg allocation in
  withBlock d resolvedCFG (fun blockD' ->
      if hasPhiNodes blockD' then
        Error "Block D should not have phi nodes after resolution"
      else
        withBlock b resolvedCFG (fun blockB' ->
            if hasMove blockB' (phys X5) (Reg (phys X6)) then
              Error "Block B should not move dead phi destination"
            else
              withBlock c resolvedCFG (fun blockC' ->
                  if hasMove blockC' (phys X5) (Reg (phys X7)) then
                    Error "Block C should not move dead phi destination"
                  else Ok ())))

let functionFixture name typedParams cfg : functionDef =
  {
    id = TestIds.functionIdForName name;
    name;
    typedParams;
    cfg;
    stackSize = 0;
    usedCalleeSaved = [];
    codegenFacts = None;
  }

(*
   Test: Loop phi should be coalesced to avoid backedge moves
   Phi coalescing is arch-independent; hardcode ARM64 for the full
   register set in the test setup.
*)
let testLoopPhiCoalesced () =
  let entry = makeLabel "loop_entry" in
  let loop = makeLabel "loop_body" in
  let back = makeLabel "loop_back" in
  let exit = makeLabel "loop_exit" in
  let blockEntry = makeJumpBlock entry [ Mov (vr 3, Imm 0L) ] loop in
  let phiInstr = Phi (vr 0, [ (vreg 3, entry); (vreg 2, back) ], None) in
  let loopInstrs =
    [
      phiInstr;
      Cmp (vr 0, Imm 10L);
      Cset (vr 4, EQ);
      Add (vr 1, vr 0, Imm 1L);
      Mov (vr 2, vreg 1);
    ]
  in
  let blockLoop = makeBranchBlock loop loopInstrs (vr 4) exit back in
  let blockBack = makeJumpBlock back [] loop in
  let blockExit = makeRetBlock exit [] in
  let cfg = makeCFG entry [ blockEntry; blockLoop; blockBack; blockExit ] in
  let func = functionFixture "phi_coalesce_loop" [] cfg in
  let allocated = RegisterAllocation.allocateRegisters Platform.ARM64 func in
  withBlock back allocated.cfg (fun backBlock ->
      if countMoves backBlock = 0 then Ok ()
      else Error "Backedge should not need moves when phi is coalesced")

(*
   Test: A non-interfering float loop-phi destination and backedge source
   should share a physical register, so phi resolution emits no FMov.
*)
let testFloatLoopPhiCoalesced () =
  let entry = makeLabel "float_loop_entry" in
  let loop = makeLabel "float_loop_body" in
  let back = makeLabel "float_loop_back" in
  let exit = makeLabel "float_loop_exit" in
  let blockEntry =
    makeJumpBlock entry
      [ Mov (vr 5, Imm 0L); FLoad (fvr 3, 0.); FLoad (fvr 4, 1.) ]
      loop
  in
  let phiInstr = FPhi (fvr 0, [ (fvr 3, entry); (fvr 2, back) ]) in
  let blockLoop =
    makeBranchBlock loop
      [
        phiInstr;
        FLoad (fvr 7, 2.);
        FAdd (fvr 8, fvr 0, fvr 7);
        PrintFloatNoNewline (fvr 8);
        Cmp (vr 5, Imm 10L);
        Cset (vr 6, EQ);
      ]
      (vr 6) exit back
  in
  let blockBack =
    makeJumpBlock back [ FAdd (fvr 1, fvr 0, fvr 4); FMov (fvr 2, fvr 1) ] loop
  in
  let blockExit = makeRetBlock exit [ PrintFloat (fvr 0) ] in
  let cfg = makeCFG entry [ blockEntry; blockLoop; blockBack; blockExit ] in
  let func = functionFixture "float_phi_coalesce_loop" [] cfg in
  let floatAllocation = FloatAllocation.chordalFloatAllocation cfg [] in
  let phiDest =
    FloatAllocation.applyFloatAllocationToFReg floatAllocation (fvr 0)
  in
  let arithmeticResult =
    FloatAllocation.applyFloatAllocationToFReg floatAllocation (fvr 1)
  in
  let backedgeSource =
    FloatAllocation.applyFloatAllocationToFReg floatAllocation (fvr 2)
  in
  if phiDest <> backedgeSource then
    Error
      "Non-interfering FPhi destination and backedge source should share a \
       register"
  else if arithmeticResult <> backedgeSource then
    Error
      "A direct FMov feeding an FPhi source should join the coalesced register \
       chain"
  else
    let allocated = RegisterAllocation.allocateRegisters Platform.ARM64 func in
    withBlock back allocated.cfg (fun backBlock ->
        if countFloatMoves backBlock = 0 then Ok ()
        else Error "Float backedge should not need FMov when FPhi is coalesced")

(*
   Test: A loop-carried float already allocated to the ABI return register
   should not be displaced merely to coalesce its invariant backedge source.
*)
let testFloatLoopPhiPreservesReturnRegister () =
  let entry = makeLabel "float_return_entry" in
  let loop = makeLabel "float_return_body" in
  let back = makeLabel "float_return_back" in
  let exit = makeLabel "float_return_exit" in
  let blockEntry =
    makeJumpBlock entry
      [
        SaveRegs ([], []);
        FLoad (fvr 40, 0.);
        FArgMoves [ (D0, fvr 40) ];
        Call (vr 32, TestIds.functionIdForName "float_return_source", []);
        FMov (FPhysical D8, FPhysical D0);
        RestoreRegs ([], []);
        FMov (fvr 32, FPhysical D8);
      ]
      loop
  in
  let blockLoop =
    makeBranchBlock loop
      [
        FPhi (fvr 27, [ (fvr 13, entry); (fvr 38, back) ]);
        Cmp (vr 5, Imm 10L);
        Cset (vr 6, EQ);
      ]
      (vr 6) exit back
  in
  let blockBack = makeJumpBlock back [ FMov (fvr 38, fvr 32) ] loop in
  let blockExit = makeRetBlock exit [ FMov (FPhysical D0, fvr 27) ] in
  let cfg = makeCFG entry [ blockEntry; blockLoop; blockBack; blockExit ] in
  let parameters =
    [
      { reg = vr 11; typ = AST.TInt64 };
      { reg = vr 12; typ = AST.TInt64 };
      { reg = vr 13; typ = AST.TFloat64 };
    ]
  in
  let func = functionFixture "float_phi_preserve_return" parameters cfg in
  let allocated = RegisterAllocation.allocateRegisters Platform.ARM64 func in
  withBlock exit allocated.cfg (fun exitBlock ->
      if countFloatMoves exitBlock = 0 then Ok ()
      else
        Error
          "FPhi coalescing should not add a move into the ABI float return \
           register")

(*
   Test: Argument-only values die before a call and must not be preserved by
   the caller-save pair. A separate value used by the continuation remains
   live across the call and must still be saved.
*)
let testCallerSaveExcludesDeadArguments () =
  let label = makeLabel "caller_save_dead_args" in
  let block =
    makeRetBlock label
      [
        Mov (vr 0, Imm 10L);
        Mov (vr 1, Imm 20L);
        Mov (vr 2, Imm 30L);
        SaveRegs ([], []);
        ArgMoves [ (X0, Imm 0L); (X1, vreg 0); (X2, vreg 1) ];
        Call (vr 3, TestIds.functionIdForName "callee", [ vreg 0; vreg 1 ]);
        RestoreRegs ([], []);
        Mov (vr 3, Reg (phys X0));
        Add (vr 4, vr 2, vreg 3);
      ]
  in
  let cfg = makeCFG label [ block ] in
  let domain, blockIndex, liveness = RegisterLiveness.computeLivenessBits cfg in
  let _, _, floatLiveness = RegisterLiveness.computeFloatLivenessBits cfg in
  let allocation =
    buildAllocationResult domain
      (allocatedPairs [ (0, X1); (1, X2); (2, X3); (3, X19); (4, X20) ])
  in
  let allocated =
    ApplyBlockAllocation.applyToBlockWithLiveness Platform.ARM64 allocation
      emptyFloatAllocation
      liveness.(blockIndex.AllocationModel.entryIndex).AllocationModel.liveOut
      floatLiveness.(blockIndex.AllocationModel.entryIndex)
        .AllocationModel.liveOut block
  in
  match
    List.find_map
      (function SaveRegs (ints, floats) -> Some (ints, floats) | _ -> None)
      allocated.instrs
  with
  | Some ([ X3 ], []) -> Ok ()
  | Some _ -> Error "Expected only continuation register X3 to be saved"
  | None -> Error "Expected populated SaveRegs instruction"

let tests =
  [
    ("simple phi resolution", testSimplePhiResolution);
    ("multiple phis parallel", testMultiplePhisParallel);
    ("phi swap", testPhiSwap);
    ("phi with immediate", testPhiWithImmediate);
    ("loop phi", testLoopPhi);
    ("dead phi pruned", testDeadPhiPruned);
    ("loop phi coalesced", testLoopPhiCoalesced);
    ("float loop phi coalesced", testFloatLoopPhiCoalesced);
    ( "float loop phi preserves return register",
      testFloatLoopPhiPreservesReturnRegister );
    ("caller save excludes dead arguments", testCallerSaveExcludesDeadArguments);
  ]

(*
   Run all phi resolution tests
*)
let runAll () =
  let rec runTests = function
    | [] -> Ok ()
    | (name, test) :: rest -> (
        match test () with
        | Error e -> Error ("FAIL: " ^ name ^ ": " ^ e)
        | Ok () -> runTests rest)
  in
  runTests tests
