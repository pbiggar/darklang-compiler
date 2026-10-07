(* RegisterInterference.mli - Build register-interference graphs from solved liveness. *)
val buildInterferenceGraphBitsetWithLiveness : AllocationModel.blockIndex -> AllocationModel.classifiedBlock array -> AllocationModel.vRegDomain -> AllocationModel.blockLiveness array -> AllocationModel.bitSet -> AllocationModel.interferenceGraph
val buildInterferenceGraphBitsetFast : LIR.cfg -> int list -> AllocationModel.interferenceGraph
val buildInterferenceGraphBitset : LIR.cfg -> int list -> AllocationModel.interferenceGraph
val buildFloatInterferenceGraphBitsetWithLiveness : AllocationModel.blockIndex -> AllocationModel.classifiedBlock array -> AllocationModel.vRegDomain -> AllocationModel.blockLiveness array -> AllocationModel.bitSet -> AllocationModel.interferenceGraph
