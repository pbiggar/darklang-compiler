(* SpillOperands.ml - Materialize allocated operands and spilled register values. *)
[@@@warning "-4"]
open AllocationModel
open RegisterPolicy
open FloatAllocation
(*
   Linear Scan Register Allocation (kept for reference, not used)
*)
let tryAllocation (allocation:allocationResult) vregId =
 let domain=allocation.AllocationModel.domain in
 let offset=Int32.to_int (Int32.sub (Int32.of_int vregId) (Int32.of_int domain.indexOffset)) in
 if offset<0 || offset>=Array.length domain.indexOf then None else let idx=domain.indexOf.(offset) in if idx>=0 then allocation.AllocationModel.allocations.(idx) else None
(*
   Get the caller-saved physical registers that contain live values
*)
let getLiveCallerSavedRegs (allocation:allocationResult) liveVRegs =
 let used=Array.make 7 false in
 Bitset.iterIndices liveVRegs (fun idx -> match allocation.AllocationModel.allocations.(idx) with
 | Some (PhysReg LIR.X1) -> used.(0)<-true | Some (PhysReg LIR.X2) -> used.(1)<-true
 | Some (PhysReg LIR.X3) -> used.(2)<-true | Some (PhysReg LIR.X4) -> used.(3)<-true
 | Some (PhysReg LIR.X5) -> used.(4)<-true | Some (PhysReg LIR.X6) -> used.(5)<-true
 | Some (PhysReg LIR.X7) -> used.(6)<-true | _ -> ());
 List.mapi (fun idx reg -> idx,reg) callerSavedRegs |> List.filter_map (fun (idx,reg) -> if used.(idx) then Some reg else None)
(*
   Get the caller-saved physical float registers that contain live values
*)
let getLiveCallerSavedFloatRegs arch liveFVRegs (floatAllocation:fAllocationResult) =
 let callerSaved=floatCallerSavedRegsFor arch in let used=Array.make 16 false in
 Bitset.iterIndices liveFVRegs (fun idx -> match floatAllocation.FloatAllocation.allocations.(idx) with Some (FPhysReg reg) -> used.(physFPRegToInt reg)<-true | Some (FStackSlot _) | Some (FRematerialized _) | None -> ());
 List.filter (fun reg -> used.(physFPRegToInt reg)) callerSaved
(*
   Apply Allocation to LIR
   Apply allocation to a register, returning the physical register and allocation info
*)
let applyToReg allocation = function
 | LIR.Physical p -> LIR.Physical p,None
 | LIR.Virtual id -> (match tryAllocation allocation id with
  | Some (PhysReg physReg) -> LIR.Physical physReg,None
  | Some (StackSlot offset) -> LIR.Physical LIR.X11,Some (StackSlot offset)
  | None -> LIR.Physical LIR.X11,None)
(*
   Apply allocation to an operand, returning load instructions if needed
   Keep Virtual unchanged if not in integer mapping - it may be a float register
   that will be handled by float allocation later
*)
let applyToOperand allocation operand tempReg = match operand with
 | LIR.Imm n -> LIR.Imm n,[] | LIR.FloatImm f -> LIR.FloatImm f,[]
 | LIR.StringSymbol value -> LIR.StringSymbol value,[] | LIR.FloatSymbol value -> LIR.FloatSymbol value,[]
 | LIR.StackSlot s -> LIR.StackSlot s,[]
 | LIR.Reg (LIR.Physical p) -> LIR.Reg (LIR.Physical p),[]
 | LIR.Reg (LIR.Virtual id) -> (match tryAllocation allocation id with
  | Some (PhysReg physReg) -> LIR.Reg (LIR.Physical physReg),[]
  | Some (StackSlot offset) -> LIR.Reg (LIR.Physical tempReg),[LIR.Mov (LIR.Physical tempReg,LIR.StackSlot offset)]
  | None -> LIR.Reg (LIR.Virtual id),[])
 | LIR.FuncAddr name -> LIR.FuncAddr name,[]
(*
   Apply allocation to an operand WITHOUT generating load instructions for spills.
   Returns StackSlot for spilled values so CodeGen can load them at the right time.
   Used for TailArgMoves where loads must be deferred to avoid using the same temp register.
   Keep Virtual unchanged if not in integer mapping - it may be a float register
*)
let applyToOperandNoLoad allocation = function
 | LIR.Imm n -> LIR.Imm n | LIR.FloatImm f -> LIR.FloatImm f
 | LIR.StringSymbol value -> LIR.StringSymbol value | LIR.FloatSymbol value -> LIR.FloatSymbol value
 | LIR.StackSlot s -> LIR.StackSlot s | LIR.FuncAddr name -> LIR.FuncAddr name
 | LIR.Reg (LIR.Physical p) -> LIR.Reg (LIR.Physical p)
 | LIR.Reg (LIR.Virtual id) -> (match tryAllocation allocation id with Some (PhysReg physReg) -> LIR.Reg (LIR.Physical physReg) | Some (StackSlot offset) -> LIR.StackSlot offset | None -> LIR.Reg (LIR.Virtual id))
(*
   Helper to load a spilled register
*)
let loadSpilled allocation reg tempReg = match reg with
 | LIR.Physical p -> LIR.Physical p,[]
 | LIR.Virtual id -> (match tryAllocation allocation id with
  | Some (PhysReg physReg) -> LIR.Physical physReg,[]
  | Some (StackSlot offset) -> LIR.Physical tempReg,[LIR.Mov (LIR.Physical tempReg,LIR.StackSlot offset)]
  | None -> LIR.Physical tempReg,[])
(*
   On x86_64, X8-X17 all alias to R11 (scratch). Using X12 and X13 as distinct
   scratch registers for loading two spilled operands simultaneously will clobber
   the first load when the second executes. On x86_64, the second operand of binary
   ops uses applyToOperandNoLoad to keep it as a StackSlot, and the codegen handles
   loading it into R11 after the first operand has been moved to the destination.
*)
let isX86_64 = function Platform.X86_64 -> true | Platform.ARM64 -> false
let aliasesX86ScratchReg = function LIR.X8 | LIR.X9 | LIR.X10 | LIR.X11 | LIR.X12 | LIR.X13 | LIR.X14 | LIR.X15 | LIR.X16 | LIR.X17 -> true | _ -> false
let x86SpillTempExcluding excluded =
 let excludedPhysical=List.filter_map (function LIR.Physical reg -> Some reg | LIR.Virtual _ -> None) excluded in
 match List.find_opt (fun reg -> not (List.mem reg excludedPhysical)) [LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X19;LIR.X20;LIR.X21;LIR.X0;LIR.X1;LIR.X2] with
 | Some reg -> reg | None -> Crash.crash "x86_64 spill repair could not find a preserved temporary register"
(*
   On x86_64, when loading two spilled Reg-typed operands, the first must go to a
   register that won't be clobbered by the second load (into X12=R11). This function
   picks a safe register by checking what physical register the right operand uses.
   If both left and right are spilled to stack, loads left into dest register
   (unless dest conflicts with right's allocated register).
   Check if left is actually spilled (StackSlot)
   Both spilled: load left into dest, right into X12
   Check that dest doesn't also alias R11
   Only left is spilled; the right operand remains in place.
   Right spilled or neither: use X12 for left, X12 for right (OK since left isn't spilled)
*)
let loadSpilledPair arch mapping left right destReg =
 if not (isX86_64 arch) then let l=loadSpilled mapping left LIR.X12 in let r=loadSpilled mapping right LIR.X13 in l,r else
 let leftIsSpilled=match left with LIR.Virtual id -> (match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false) | _ -> false in
 let rightIsSpilled=match right with LIR.Virtual id -> (match tryAllocation mapping id with Some (StackSlot _) -> true | _ -> false) | _ -> false in
 if leftIsSpilled && rightIsSpilled then (
  let destPhys=match destReg with LIR.Physical p -> p | LIR.Virtual id -> Crash.crash ("loadSpilledPair: destination vreg "^string_of_int id^" was not allocated before x86_64 spill repair") in
  let leftTemp=if aliasesX86ScratchReg destPhys then (
   let name=List.assoc destPhys [LIR.X8,"X8";LIR.X9,"X9";LIR.X10,"X10";LIR.X11,"X11";LIR.X12,"X12";LIR.X13,"X13";LIR.X14,"X14";LIR.X15,"X15";LIR.X16,"X16";LIR.X17,"X17"] in
   Crash.crash ("loadSpilledPair: destination register "^name^" aliases x86_64 scratch register R11")) else destPhys in
  let l=loadSpilled mapping left leftTemp in let r=loadSpilled mapping right LIR.X12 in l,r)
 else if leftIsSpilled then let l=loadSpilled mapping left LIR.X12 in let r=loadSpilled mapping right LIR.X12 in l,r
 else let l=loadSpilled mapping left LIR.X12 in let r=loadSpilled mapping right LIR.X12 in l,r
