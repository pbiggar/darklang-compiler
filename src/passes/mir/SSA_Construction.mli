(* SSA_Construction.mli - Convert typed MIR control flow to static single assignment. *)
type predecessors = MIR.label list MIR.LabelMap.t
type dominators = MIR.label MIR.LabelMap.t
type dominanceFrontier = MIR.LabelSet.t MIR.LabelMap.t
type ssaConstructionTiming = {phase : string; elapsedMs : float}
val buildPredecessors : MIR.cfg -> predecessors
val computeDominators : MIR.cfg -> predecessors -> dominators
val computeDominanceFrontier : MIR.cfg -> predecessors -> dominators -> dominanceFrontier
val getBlockDefs : MIR.basicBlock -> MIR.VRegSet.t
val getAllDefs : MIR.cfg -> MIR.LabelSet.t MIR.VRegMap.t
val getOperandUses : MIR.operand -> MIR.VRegSet.t
val getBlockUses : MIR.basicBlock -> MIR.VRegSet.t
val getSuccessors : MIR.basicBlock -> MIR.label list
val computeLiveness : MIR.cfg -> MIR.VRegSet.t MIR.LabelMap.t * MIR.VRegSet.t MIR.LabelMap.t
val insertPhiNodes : MIR.cfg -> dominanceFrontier -> predecessors -> MIR.VRegSet.t MIR.LabelMap.t -> MIR.vReg list -> AST.semanticType list -> MIR.cfg
type renamingState = {versionStacks : (MIR.vReg, int Stack.t) Hashtbl.t; pushedVersions : MIR.vReg Stack.t; mutable nextVersion : int; originalFloatRegs : MIR.IntSet.t; floatRegs : MIR.IntSet.t ref}
val createInitialRenamingState : MIR.cfg -> MIR.IntSet.t -> MIR.vReg list -> renamingState
val newVersion : renamingState -> MIR.vReg -> int * MIR.vReg * renamingState
val getRenamedReg : renamingState -> MIR.vReg -> MIR.vReg
val renameOperand : renamingState -> MIR.operand -> MIR.operand
val renameInstr : renamingState -> MIR.instr -> MIR.instr * renamingState
val renameTerminator : renamingState -> MIR.terminator -> MIR.terminator
val renameBlock : renamingState -> MIR.basicBlock -> MIR.basicBlock * renamingState
module PhiUpdateMap : Map.S with type key = MIR.label * MIR.label * MIR.vReg
type phiSourceUpdates = MIR.operand PhiUpdateMap.t
val applyPhiSourceUpdates : phiSourceUpdates -> MIR.basicBlock -> MIR.basicBlock
val buildDomTree : dominators -> MIR.label list MIR.LabelMap.t
val renameCFG : MIR.cfg -> dominators -> MIR.IntSet.t -> MIR.vReg list -> MIR.cfg * MIR.IntSet.t
val convertFunctionToSSA : MIR.functionDef -> MIR.functionDef
val convertFunctionToSSAWithTiming : MIR.functionDef -> MIR.functionDef * ssaConstructionTiming list
val convertToSSA : MIR.program -> MIR.program
val convertToSSAWithTiming : MIR.program -> MIR.program * ssaConstructionTiming list
