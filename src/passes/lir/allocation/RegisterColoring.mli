(* RegisterColoring.mli - Color interference graphs and map colors to physical registers. *)
val greedyColorReverse :
  AllocationModel.interferenceGraph ->
  int list ->
  int option array ->
  int ->
  AllocationModel.bitSet array ->
  AllocationModel.coloringResult

val chordalGraphColor :
  AllocationModel.interferenceGraph ->
  (int * int) list ->
  int ->
  (int * int) list ->
  (int * int) list ->
  AllocationModel.coloringResult

val chordalGraphColorWithTiming :
  (unit -> float) ->
  AllocationModel.interferenceGraph ->
  (int * int) list ->
  int ->
  (int * int) list ->
  (int * int) list ->
  AllocationModel.coloringResult * AllocationModel.chordalColoringTiming

val coloringToAllocation :
  AllocationModel.coloringResult ->
  LIR.physReg list ->
  AllocationModel.allocationResult
