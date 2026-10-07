(* ApplyBlockAllocation.ml - Apply allocation and caller-save plans across CFG blocks. *)
[@@@warning "-4"]

open AllocationModel
open RegisterFacts
open RegisterLiveness
open SpillOperands
open ApplyRegisterAllocation

(*
   Apply allocation to terminator
   Load condition from stack before branching
   CondBranch uses condition flags, not a register - pass through unchanged
*)
let applyToTerminator mapping term =
  match term with
  | LIR.Ret -> ([], LIR.Ret)
  | LIR.Branch (cond, trueLabel, falseLabel) -> (
      match cond with
      | LIR.Virtual id -> (
          match tryAllocation mapping id with
          | Some (PhysReg physReg) ->
              ([], LIR.Branch (LIR.Physical physReg, trueLabel, falseLabel))
          | Some (StackSlot offset) ->
              let loadInstr =
                LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
              in
              ( [ loadInstr ],
                LIR.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel) )
          | None ->
              ([], LIR.Branch (LIR.Physical LIR.X11, trueLabel, falseLabel)))
      | LIR.Physical p ->
          ([], LIR.Branch (LIR.Physical p, trueLabel, falseLabel)))
  | LIR.BranchZero (cond, zeroLabel, nonZeroLabel) -> (
      match cond with
      | LIR.Virtual id -> (
          match tryAllocation mapping id with
          | Some (PhysReg physReg) ->
              ( [],
                LIR.BranchZero (LIR.Physical physReg, zeroLabel, nonZeroLabel)
              )
          | Some (StackSlot offset) ->
              let loadInstr =
                LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
              in
              ( [ loadInstr ],
                LIR.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel)
              )
          | None ->
              ( [],
                LIR.BranchZero (LIR.Physical LIR.X11, zeroLabel, nonZeroLabel)
              ))
      | LIR.Physical p ->
          ([], LIR.BranchZero (LIR.Physical p, zeroLabel, nonZeroLabel)))
  | LIR.BranchBitZero (reg, bit, zeroLabel, nonZeroLabel) -> (
      match reg with
      | LIR.Virtual id -> (
          match tryAllocation mapping id with
          | Some (PhysReg physReg) ->
              ( [],
                LIR.BranchBitZero
                  (LIR.Physical physReg, bit, zeroLabel, nonZeroLabel) )
          | Some (StackSlot offset) ->
              let loadInstr =
                LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
              in
              ( [ loadInstr ],
                LIR.BranchBitZero
                  (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel) )
          | None ->
              ( [],
                LIR.BranchBitZero
                  (LIR.Physical LIR.X11, bit, zeroLabel, nonZeroLabel) ))
      | LIR.Physical p ->
          ([], LIR.BranchBitZero (LIR.Physical p, bit, zeroLabel, nonZeroLabel))
      )
  | LIR.BranchBitNonZero (reg, bit, nonZeroLabel, zeroLabel) -> (
      match reg with
      | LIR.Virtual id -> (
          match tryAllocation mapping id with
          | Some (PhysReg physReg) ->
              ( [],
                LIR.BranchBitNonZero
                  (LIR.Physical physReg, bit, nonZeroLabel, zeroLabel) )
          | Some (StackSlot offset) ->
              let loadInstr =
                LIR.Mov (LIR.Physical LIR.X11, LIR.StackSlot offset)
              in
              ( [ loadInstr ],
                LIR.BranchBitNonZero
                  (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel) )
          | None ->
              ( [],
                LIR.BranchBitNonZero
                  (LIR.Physical LIR.X11, bit, nonZeroLabel, zeroLabel) ))
      | LIR.Physical p ->
          ( [],
            LIR.BranchBitNonZero (LIR.Physical p, bit, nonZeroLabel, zeroLabel)
          ))
  | LIR.Jump label -> ([], LIR.Jump label)
  | LIR.CondBranch (cond, trueLabel, falseLabel) ->
      ([], LIR.CondBranch (cond, trueLabel, falseLabel))

type blockAllocationPreparation = { saveRegsLiveness : (bitSet * bitSet) list }

let prepareBlockAllocation (mapping : allocationResult)
    (floatAllocation : FloatAllocation.fAllocationResult) liveOut floatLiveOut
    (block : LIR.basicBlock) instrFacts =
  let hasEmptySaveRegs = List.exists isEmptySaveRegs block.LIR.instrs in
  let saveRegsLiveness =
    if hasEmptySaveRegs then
      computeSaveRegsPreparation mapping.AllocationModel.domain
        floatAllocation.FloatAllocation.domain block instrFacts liveOut
        floatLiveOut
    else []
  in
  { saveRegsLiveness }

(*
   Apply allocation to a basic block with precomputed SaveRegs/RestoreRegs data.
   Find SaveRegs/RestoreRegs pairs and compute the registers to save while
   emitting allocated instructions directly. The old mapFold produced one
   temporary list per input instruction, concatenated all of those lists,
   and then traversed the result again for float allocation.
*)
let applyToPreparedBlock arch mapping floatAllocation preparation
    (block : LIR.basicBlock) =
  let allocatedInstrs = ref [] in
  let savedRegsStack = ref [] in
  let remainingLiveness = ref preparation.saveRegsLiveness in
  let appendOneAllocated instr =
    List.iter
      (fun allocated -> allocatedInstrs := allocated :: !allocatedInstrs)
      (FloatAllocation.applyFloatAllocationToInstrs floatAllocation instr)
  in
  let appendAllocated instrs = List.iter appendOneAllocated instrs in
  List.iter
    (function
      | LIR.SaveRegs ([], []) -> (
          match !remainingLiveness with
          | (liveAfter, floatLiveAfter) :: restLiveness ->
              let liveCallerSaved = getLiveCallerSavedRegs mapping liveAfter in
              let liveCallerSavedFloat =
                getLiveCallerSavedFloatRegs arch floatLiveAfter floatAllocation
              in
              let regs = (liveCallerSaved, liveCallerSavedFloat) in
              appendOneAllocated
                (LIR.SaveRegs (liveCallerSaved, liveCallerSavedFloat));
              savedRegsStack := regs :: !savedRegsStack;
              remainingLiveness := restLiveness
          | [] -> Crash.crash "Missing liveness snapshot for SaveRegs")
      | LIR.RestoreRegs ([], []) -> (
          match !savedRegsStack with
          | (ints, floats) :: restSavedRegs ->
              appendOneAllocated (LIR.RestoreRegs (ints, floats));
              savedRegsStack := restSavedRegs
          | [] -> Crash.crash "Unmatched RestoreRegs: SaveRegs stack is empty")
      | instr -> appendAllocated (applyToInstr arch mapping instr))
    block.LIR.instrs;
  if !remainingLiveness <> [] then
    Crash.crash "Unused liveness snapshot for SaveRegs";
  let termLoads, allocatedTerm =
    applyToTerminator mapping block.LIR.terminator
  in
  appendAllocated termLoads;
  {
    LIR.label = block.LIR.label;
    instrs = List.rev !allocatedInstrs;
    terminator = allocatedTerm;
  }

(*
   Apply allocation to a basic block with liveness-aware SaveRegs/RestoreRegs population
*)
let applyToBlockWithLiveness arch mapping floatAllocation liveOut floatLiveOut
    block =
  let instrFacts = (classifyBlocks [| block |]).(0).instrFacts in
  let preparation =
    prepareBlockAllocation mapping floatAllocation liveOut floatLiveOut block
      instrFacts
  in
  applyToPreparedBlock arch mapping floatAllocation preparation block

let prepareCFGAllocation blocks mapping
    (floatAllocation : FloatAllocation.fAllocationResult) liveness floatLiveness
    classifiedBlocks =
  let emptyFloat =
    Bitset.empty floatAllocation.FloatAllocation.domain.wordCount
  in
  Array.init (Array.length blocks) (fun idx ->
      let blockLiveness = liveness.(idx) in
      let floatBlockLiveness =
        if idx < Array.length floatLiveness then floatLiveness.(idx)
        else { liveIn = emptyFloat; liveOut = emptyFloat }
      in
      prepareBlockAllocation mapping floatAllocation blockLiveness.liveOut
        floatBlockLiveness.liveOut blocks.(idx)
        classifiedBlocks.(idx).instrFacts)

let applyPreparedCFGAllocation arch blocks mapping floatAllocation preparations
    =
  Array.init (Array.length blocks) (fun idx ->
      applyToPreparedBlock arch mapping floatAllocation preparations.(idx)
        blocks.(idx))

(*
   Apply allocation to CFG with liveness info
*)
let applyToCFGWithLiveness arch blocks mapping floatAllocation liveness
    floatLiveness =
  let classifiedBlocks = classifyBlocks blocks in
  let preparations =
    prepareCFGAllocation blocks mapping floatAllocation liveness floatLiveness
      classifiedBlocks
  in
  applyPreparedCFGAllocation arch blocks mapping floatAllocation preparations
