(*
   SSALivenessTests.fs - Unit tests for SSA liveness analysis
   Tests the liveness analysis with phi nodes in SSA form.
   Key insights for SSA liveness:
   - Phi dests are defined at block entry
   - Phi sources are used at PREDECESSOR exits (not at the phi's block)
   - This affects LiveOut calculation for predecessors
   Test result type
*)
(* Exercise original SSA liveness boundaries through complete LIR control-flow graphs. *)
[@@@warning "-42"]
open Dark_compiler
open LIR
type testResult=(unit,string) result
(*
   Create a label
*)
let makeLabel name=Label name
(*
   Create a virtual register
*)
let vr n=Virtual n
(*
   Create a virtual floating-point register
*)
let fvr n=FVirtual n
(*
   Create a VReg operand
*)
let vreg n=Reg (vr n)
(*
   Create a simple basic block with a jump terminator
*)
let makeJumpBlock label instrs target={label;instrs;terminator=Jump target}
(*
   Create a basic block with branch terminator
*)
let makeBranchBlock label instrs cond trueTarget falseTarget={label;instrs;terminator=Branch (cond,trueTarget,falseTarget)}
(*
   Create a basic block with return terminator
*)
let makeRetBlock label instrs={label;instrs;terminator=Ret}
(*
   Build a CFG from a list of blocks
*)
let makeCFG entry blocks={entry;blocks=LabelMap.of_list (List.map (fun (b:basicBlock)->b.label,b) blocks)}
(*
   Check if a VReg is in the LiveIn set
*)
let isLiveIn domain blockIndex liveness label id=match AllocationModel.blockLivenessForLabel blockIndex liveness label with
 |Some bl->AllocationModel.vregBitsContains domain bl.AllocationModel.liveIn id|None->false
(*
   Check if a VReg is in the LiveOut set
*)
let isLiveOut domain blockIndex liveness label id=match AllocationModel.blockLivenessForLabel blockIndex liveness label with
 |Some bl->AllocationModel.vregBitsContains domain bl.AllocationModel.liveOut id|None->false
(*
   Check if an FVirtual is in the LiveIn set
*)
let isFloatLiveIn=isLiveIn
(*
   Check if an FVirtual is in the LiveOut set
*)
let isFloatLiveOut=isLiveOut
(*
   =============================================================================
   Test Cases
   Test: Phi destination is defined at block entry
   Simple diamond CFG with phi at merge point:
   A (def v0)
   / \
   B   C
   \ /
   D (phi v1 = [v0 from B, v0 from C])
   |
   E (use v1)
   v0 should be live-out of B and C (used by phi in D)
   v1 should NOT be live-in to D (it's defined there by phi)
   v0 is defined in A, used by phi in D
   v1 is defined by phi in D, used in E
   Use v1 and then return
   v0 should be live-out of B (used by phi in D)
   v0 should be live-out of C (used by phi in D)
   v1 should be live-out of D (used in E via v2)
*)
let testPhiDefAtBlockEntry ()=
 let labelA=makeLabel "A" and labelB=makeLabel "B" and labelC=makeLabel "C" and labelD=makeLabel "D" and labelE=makeLabel "E" in
 let blockA=makeBranchBlock labelA [Mov (vr 0,Imm 1L)] (vr 0) labelB labelC in
 let blockB=makeJumpBlock labelB [] labelD and blockC=makeJumpBlock labelC [] labelD in
 let phiInstr=Phi (vr 1,[vreg 0,labelB;vreg 0,labelC],None) in
 let blockD=makeJumpBlock labelD [phiInstr;Mov (vr 2,vreg 1)] labelE in
 let blockE=makeRetBlock labelE [Mov (vr 3,vreg 2)] in
 let domain,blockIndex,liveness=RegisterLiveness.computeLivenessBits (makeCFG labelA [blockA;blockB;blockC;blockD;blockE]) in
 if not (isLiveOut domain blockIndex liveness labelB 0) then Error "v0 should be live-out of B (used by phi in D)"
 else if not (isLiveOut domain blockIndex liveness labelC 0) then Error "v0 should be live-out of C (used by phi in D)"
 else if isLiveIn domain blockIndex liveness labelD 1 then Error "v1 should NOT be live-in to D (it's defined there by phi)"
 else if not (isLiveOut domain blockIndex liveness labelD 2) then Error "v2 should be live-out of D (used in E)" else Ok ()
(*
   Test: Phi sources are live at predecessor exits, not at phi's block
   Diamond with different values from each branch:
   A
   / \
   B   C
   (v0) (v1)
   \ /
   D (phi v2 = [v0 from B, v1 from C])
   v0 should be live-out of B only (not C)
   v1 should be live-out of C only (not B)
   v0 defined and used only in B path
   v1 defined and used only in C path
   Use v2 and return
   v0 should be live-out of B (used by phi from B)
   v0 should NOT be live-out of C (not used from C path)
   v1 should be live-out of C (used by phi from C)
   v1 should NOT be live-out of B (not used from B path)
*)
let testPhiSourceLivenessScoped ()=
 let labelA=makeLabel "A" and labelB=makeLabel "B" and labelC=makeLabel "C" and labelD=makeLabel "D" in
 let condReg=vr 99 in
 let blockA=makeBranchBlock labelA [Mov (condReg,Imm 1L)] condReg labelB labelC in
 let blockB=makeJumpBlock labelB [Mov (vr 0,Imm 10L)] labelD in
 let blockC=makeJumpBlock labelC [Mov (vr 1,Imm 20L)] labelD in
 let phiInstr=Phi (vr 2,[vreg 0,labelB;vreg 1,labelC],None) in
 let blockD=makeRetBlock labelD [phiInstr;Mov (vr 3,vreg 2)] in
 let domain,blockIndex,liveness=RegisterLiveness.computeLivenessBits (makeCFG labelA [blockA;blockB;blockC;blockD]) in
 if not (isLiveOut domain blockIndex liveness labelB 0) then Error "v0 should be live-out of B (used by phi)"
 else if isLiveOut domain blockIndex liveness labelC 0 then Error "v0 should NOT be live-out of C (not in phi from C)"
 else if not (isLiveOut domain blockIndex liveness labelC 1) then Error "v1 should be live-out of C (used by phi)"
 else if isLiveOut domain blockIndex liveness labelB 1 then Error "v1 should NOT be live-out of B (not in phi from B)" else Ok ()
(*
   Test: Multiple phis in same block
   Diamond with multiple phi nodes:
   A
   / \
   B   C
   \ /
   D (phi v2 = [v0 from B, v1 from C])
   (phi v3 = [v4 from B, v5 from C])
   All phi sources should be live at correct predecessor exits
   B defines v0, v4
   C defines v1, v5
   Use both phi results with Add
   v0 and v4 should be live-out of B
   v1 and v5 should be live-out of C
   Neither phi dest should be live-in to D
*)
let testMultiplePhisSameBlock ()=
 let labelA=makeLabel "A" and labelB=makeLabel "B" and labelC=makeLabel "C" and labelD=makeLabel "D" in
 let condReg=vr 99 in
 let blockA=makeBranchBlock labelA [Mov (condReg,Imm 1L)] condReg labelB labelC in
 let blockB=makeJumpBlock labelB [Mov (vr 0,Imm 10L);Mov (vr 4,Imm 40L)] labelD in
 let blockC=makeJumpBlock labelC [Mov (vr 1,Imm 20L);Mov (vr 5,Imm 50L)] labelD in
 let phi1=Phi (vr 2,[vreg 0,labelB;vreg 1,labelC],None) in
 let phi2=Phi (vr 3,[vreg 4,labelB;vreg 5,labelC],None) in
 let useInstr=Add (vr 6,vr 2,vreg 3) in
 let blockD=makeRetBlock labelD [phi1;phi2;useInstr;Mov (vr 7,vreg 6)] in
 let domain,blockIndex,liveness=RegisterLiveness.computeLivenessBits (makeCFG labelA [blockA;blockB;blockC;blockD]) in
 if not (isLiveOut domain blockIndex liveness labelB 0) then Error "v0 should be live-out of B"
 else if not (isLiveOut domain blockIndex liveness labelB 4) then Error "v4 should be live-out of B"
 else if not (isLiveOut domain blockIndex liveness labelC 1) then Error "v1 should be live-out of C"
 else if not (isLiveOut domain blockIndex liveness labelC 5) then Error "v5 should be live-out of C"
 else if isLiveIn domain blockIndex liveness labelD 2 then Error "v2 should NOT be live-in to D (defined by phi)"
 else if isLiveIn domain blockIndex liveness labelD 3 then Error "v3 should NOT be live-in to D (defined by phi)" else Ok ()
(*
   Test: Loop with phi
   A (def v0)
   |
   v
   B (phi v1 = [v0 from A, v2 from C])
   C (def v2, use v1)
   v  (loop back to B or exit to D)
   D
   v2 should be live-out of C (used by phi in B)
   v1 should be live-out of B (used in C)
   C uses v1 to compute v2, and might loop back
   D uses v1 that was passed through from C (but v1 is from B's phi)
   v0 should be live-out of A (used by phi in B)
   v2 should be live-out of C (used by phi in B on loop back)
*)
let testLoopPhi ()=
 let labelA=makeLabel "A" and labelB=makeLabel "B" and labelC=makeLabel "C" and labelD=makeLabel "D" in
 let condReg=vr 99 in
 let blockA=makeJumpBlock labelA [Mov (vr 0,Imm 0L)] labelB in
 let phiInstr=Phi (vr 1,[vreg 0,labelA;vreg 2,labelC],None) in
 let blockB=makeJumpBlock labelB [phiInstr] labelC in
 let blockC=makeBranchBlock labelC [Add (vr 2,vr 1,Imm 1L);Mov (condReg,Imm 1L)] condReg labelB labelD in
 let blockD=makeRetBlock labelD [Mov (vr 3,vreg 1)] in
 let domain,blockIndex,liveness=RegisterLiveness.computeLivenessBits (makeCFG labelA [blockA;blockB;blockC;blockD]) in
 if not (isLiveOut domain blockIndex liveness labelA 0) then Error "v0 should be live-out of A (used by phi in B)"
 else if not (isLiveOut domain blockIndex liveness labelC 2) then Error "v2 should be live-out of C (used by phi in B on loop back)"
 else if not (isLiveOut domain blockIndex liveness labelB 1) then Error "v1 should be live-out of B (used in C)" else Ok ()
(*
   Test: Bitset-backed liveness preserves expected behavior
*)
let testBitsetLivenessBehavior ()=
 let labelA=makeLabel "A" and labelB=makeLabel "B" and labelC=makeLabel "C" and labelD=makeLabel "D" and labelE=makeLabel "E" in
 let condReg=vr 99 in
 let blockA=makeBranchBlock labelA [Mov (condReg,Imm 1L)] condReg labelB labelC in
 let blockB=makeJumpBlock labelB [Mov (vr 0,Imm 10L)] labelD in
 let blockC=makeJumpBlock labelC [Mov (vr 1,Imm 20L)] labelD in
 let phiInstr=Phi (vr 2,[vreg 0,labelB;vreg 1,labelC],None) in
 let blockD=makeJumpBlock labelD [phiInstr;Add (vr 3,vr 2,Imm 1L)] labelE in
 let blockE=makeRetBlock labelE [Mov (vr 4,vreg 3)] in
 let domain,blockIndex,bitset=RegisterLiveness.computeLivenessBits (makeCFG labelA [blockA;blockB;blockC;blockD;blockE]) in
 let liveOutB=isLiveOut domain blockIndex bitset labelB 0 and liveOutC=isLiveOut domain blockIndex bitset labelC 1 in
 if liveOutB && liveOutC then Ok () else Error (Printf.sprintf "Bitset liveness missing phi uses. LiveOut B contains v0: %s, LiveOut C contains v1: %s" (if liveOutB then "True" else "False") (if liveOutC then "True" else "False"))
(*
   Test: Float phi sources are live at predecessor exits, not at phi's block
   Mirrors integer phi liveness for the independent float liveness path.
*)
let testFloatPhiSourceLivenessScoped ()=
 let labelA=makeLabel "A" and labelB=makeLabel "B" and labelC=makeLabel "C" and labelD=makeLabel "D" in
 let condReg=vr 99 in
 let blockA=makeBranchBlock labelA [Mov (condReg,Imm 1L)] condReg labelB labelC in
 let blockB=makeJumpBlock labelB [FLoad (fvr 0,10.)] labelD in
 let blockC=makeJumpBlock labelC [FLoad (fvr 1,20.)] labelD in
 let phiInstr=FPhi (fvr 2,[fvr 0,labelB;fvr 1,labelC]) in
 let blockD=makeRetBlock labelD [phiInstr;FAdd (fvr 3,fvr 2,fvr 2)] in
 let domain,blockIndex,liveness=RegisterLiveness.computeFloatLivenessBits (makeCFG labelA [blockA;blockB;blockC;blockD]) in
 if not (isFloatLiveOut domain blockIndex liveness labelB 0) then Error "f0 should be live-out of B (used by float phi)"
 else if isFloatLiveOut domain blockIndex liveness labelC 0 then Error "f0 should NOT be live-out of C (not in float phi from C)"
 else if not (isFloatLiveOut domain blockIndex liveness labelC 1) then Error "f1 should be live-out of C (used by float phi)"
 else if isFloatLiveOut domain blockIndex liveness labelB 1 then Error "f1 should NOT be live-out of B (not in float phi from B)"
 else if isFloatLiveIn domain blockIndex liveness labelD 2 then Error "f2 should NOT be live-in to D (defined by float phi)" else Ok ()
let tests=["phi def at block entry",testPhiDefAtBlockEntry;"phi source liveness scoped",testPhiSourceLivenessScoped;"multiple phis same block",testMultiplePhisSameBlock;"loop phi",testLoopPhi;"bitset liveness behavior",testBitsetLivenessBehavior;"float phi source liveness scoped",testFloatPhiSourceLivenessScoped]
(*
   Run all SSA liveness tests
*)
let runAll ()=let rec run=function []->Ok ()|(name,test)::rest->match test () with Error error->Error ("FAIL: "^name^": "^error)|Ok ()->run rest in run tests
