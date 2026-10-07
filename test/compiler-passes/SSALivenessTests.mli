(* Original SSA liveness assertions for integer and floating-point phi edges. *)
type testResult = (unit, string) result

val makeLabel : string -> Dark_compiler.LIR.label
val vr : int -> Dark_compiler.LIR.reg
val fvr : int -> Dark_compiler.LIR.fReg
val vreg : int -> Dark_compiler.LIR.operand

val makeJumpBlock :
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.instr list ->
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.basicBlock

val makeBranchBlock :
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.instr list ->
  Dark_compiler.LIR.reg ->
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.basicBlock

val makeRetBlock :
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.instr list ->
  Dark_compiler.LIR.basicBlock

val makeCFG :
  Dark_compiler.LIR.label ->
  Dark_compiler.LIR.basicBlock list ->
  Dark_compiler.LIR.cfg

val isLiveIn :
  Dark_compiler.AllocationModel.vRegDomain ->
  Dark_compiler.AllocationModel.blockIndex ->
  Dark_compiler.AllocationModel.blockLiveness array ->
  Dark_compiler.LIR.label ->
  int ->
  bool

val isLiveOut :
  Dark_compiler.AllocationModel.vRegDomain ->
  Dark_compiler.AllocationModel.blockIndex ->
  Dark_compiler.AllocationModel.blockLiveness array ->
  Dark_compiler.LIR.label ->
  int ->
  bool

val isFloatLiveIn :
  Dark_compiler.AllocationModel.vRegDomain ->
  Dark_compiler.AllocationModel.blockIndex ->
  Dark_compiler.AllocationModel.blockLiveness array ->
  Dark_compiler.LIR.label ->
  int ->
  bool

val isFloatLiveOut :
  Dark_compiler.AllocationModel.vRegDomain ->
  Dark_compiler.AllocationModel.blockIndex ->
  Dark_compiler.AllocationModel.blockLiveness array ->
  Dark_compiler.LIR.label ->
  int ->
  bool

val testPhiDefAtBlockEntry : unit -> testResult
val testPhiSourceLivenessScoped : unit -> testResult
val testMultiplePhisSameBlock : unit -> testResult
val testLoopPhi : unit -> testResult
val testBitsetLivenessBehavior : unit -> testResult
val testFloatPhiSourceLivenessScoped : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
