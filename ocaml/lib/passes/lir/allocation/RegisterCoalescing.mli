(* Coalescing.fs - Collect move preferences and coalesce compatible graph vertices. *)
val dedupePairs : (int * int) list -> (int * int) list
val collectMovePairs : LIR.basicBlock array -> (int * int) list
val collectPhiPairs : LIR.basicBlock array -> (int * int) list
val collectFPhiPairs : LIR.basicBlock array -> (int * int) list
val collectFPhiSourceMovePairs : LIR.basicBlock array -> (int * int) list
val collectPhiPreferences : LIR.basicBlock array -> (int * int) list
val maximumCardinalitySearchWithProfile : AllocationModel.interferenceGraph -> int list * AllocationModel.mcsProfile
val maximumCardinalitySearch : AllocationModel.interferenceGraph -> int list
type coalescedGraph = {graph : AllocationModel.interferenceGraph; repOfIndex : int array; repMembers : AllocationModel.bitSet array; preferences : AllocationModel.bitSet array; precolored : int option array}
val coalesceGraphFast : AllocationModel.interferenceGraph -> (int * int) list -> (int * int) list -> (int * int) list -> coalescedGraph
val expandColoring : AllocationModel.coloringResult -> AllocationModel.bitSet array -> AllocationModel.coloringResult
