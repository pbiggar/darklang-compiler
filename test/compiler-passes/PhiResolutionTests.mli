(* Original phi elimination, coalescing and caller-save unit tests. *)
open Dark_compiler

type testResult = (unit, string) result

val makeLabel : string -> LIR.label
val vr : int -> LIR.reg
val vreg : int -> LIR.operand
val fvr : int -> LIR.fReg
val phys : LIR.physReg -> LIR.reg
val makeJumpBlock : LIR.label -> LIR.instr list -> LIR.label -> LIR.basicBlock

val makeBranchBlock :
  LIR.label ->
  LIR.instr list ->
  LIR.reg ->
  LIR.label ->
  LIR.label ->
  LIR.basicBlock

val makeRetBlock : LIR.label -> LIR.instr list -> LIR.basicBlock
val makeCFG : LIR.label -> LIR.basicBlock list -> LIR.cfg
val labelName : LIR.label -> string

val withBlock :
  LIR.label -> LIR.cfg -> (LIR.basicBlock -> testResult) -> testResult

val hasPhiNodes : LIR.basicBlock -> bool
val countMoves : LIR.basicBlock -> int
val countFloatMoves : LIR.basicBlock -> int
val emptyFloatAllocation : FloatAllocation.fAllocationResult

val buildAllocationResult :
  AllocationModel.vRegDomain ->
  (int * AllocationModel.allocation) list ->
  AllocationModel.allocationResult

val cfgFromBlocks :
  LIR.label -> LIR.label array -> LIR.basicBlock array -> LIR.cfg

val resolvePhiCFG :
  LIR.cfg -> (int * AllocationModel.allocation) list -> LIR.cfg

val hasMove : LIR.basicBlock -> LIR.reg -> LIR.operand -> bool
val testSimplePhiResolution : unit -> testResult
val testMultiplePhisParallel : unit -> testResult
val testPhiSwap : unit -> testResult
val testPhiWithImmediate : unit -> testResult
val testLoopPhi : unit -> testResult
val testDeadPhiPruned : unit -> testResult
val testLoopPhiCoalesced : unit -> testResult
val testFloatLoopPhiCoalesced : unit -> testResult
val testFloatLoopPhiPreservesReturnRegister : unit -> testResult
val testCallerSaveExcludesDeadArguments : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
