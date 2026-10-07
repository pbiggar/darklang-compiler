(* Register-allocation CFG integration tests. *)
type testResult = (unit,string) result
val colorOf : Dark_compiler.AllocationModel.coloringResult -> int -> int option
val graphNeighbors : Dark_compiler.AllocationModel.interferenceGraph -> int -> Dark_compiler.MemoryModel.IntSet.t
val graphHasVertex : Dark_compiler.AllocationModel.interferenceGraph -> int -> bool
val testBuildFromCFG : unit -> testResult
val testBuildFromCFGBitsetMatches : unit -> testResult
val testFullChordalPipeline : unit -> testResult
val testApply2Pattern : unit -> testResult
val testMoveCoalescingPreference : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAllTests : unit -> (string * testResult) list
