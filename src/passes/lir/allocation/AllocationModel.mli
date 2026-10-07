type liveInterval = {vRegId : int; start : int; end_ : int}
type bitSet = Bitset.bitset
type vRegDomain = {ids : int array; indexOf : int array; indexOffset : int; wordCount : int}
type blockIndex = {labels : LIR.label array; entryIndex : int}
type allocation = PhysReg of LIR.physReg | StackSlot of int
type allocationResult = {domain : vRegDomain; allocations : allocation option array; stackSize : int; usedCalleeSaved : LIR.physReg list}
type blockLiveness = {liveIn : bitSet; liveOut : bitSet}
type registerAllocationTiming = {phase : string; elapsedMs : float}
type chordalColoringTiming = {coalesceMs : float; mcsMs : float; greedyMs : float; expandMs : float}
type instrRegisterFacts = {instr : LIR.instr; intUses : int list; intDef : int option; intPhiUses : (int * LIR.label) list; floatUses : int list; floatDef : int option; floatPhiUses : (int * LIR.label) list}
type classifiedBlock = {block : LIR.basicBlock; instrFacts : instrRegisterFacts array; terminatorUses : int list; hasPhiNodes : bool}
val buildVRegDomain : int list -> vRegDomain
val tryIndexOf : vRegDomain -> int -> int option
val vregBitsContains : vRegDomain -> bitSet -> int -> bool
val vregBitsAddInPlace : vRegDomain -> int -> bitSet -> unit
val vregBitsRemoveInPlace : vRegDomain -> int -> bitSet -> unit
type bitSetUnionAccumulator = NoUnionBits | BorrowedUnionBits of bitSet | OwnedUnionBits of bitSet
val bitsetAccumulateUnion : bitSetUnionAccumulator -> bitSet -> bitSetUnionAccumulator
val bitsetFinishUnion : bitSet -> bitSetUnionAccumulator -> bitSet
val vregBitsFromList : vRegDomain -> int list -> bitSet
val buildBlockIndex : LIR.cfg -> blockIndex * LIR.basicBlock array
val tryBlockIndex : blockIndex -> LIR.label -> int option
val blockIndexOfLabel : blockIndex -> LIR.label -> int option
val blockLivenessForLabel : blockIndex -> blockLiveness array -> LIR.label -> blockLiveness option
val blocksToMap : blockIndex -> LIR.basicBlock array -> LIR.basicBlock LIR.LabelMap.t
type interferenceGraph = {domain : vRegDomain; vertices : bitSet; neighbors : bitSet array}
type coloringResult = {domain : vRegDomain; colors : int option array; spills : bitSet; chromaticNumber : int}
type mcsProfile = {vertexCount : int; selectionChecks : int; weightUpdates : int; bucketSkips : int}
val buildInterferenceGraphFromEdges : int list -> (int * int) list -> interferenceGraph
val graphHasVertex : interferenceGraph -> int -> bool
val graphNeighbors : interferenceGraph -> int -> int list
val colorOf : coloringResult -> int -> int option
val isSpill : coloringResult -> int -> bool
val spillCount : coloringResult -> int
val coloredCount : coloringResult -> int
