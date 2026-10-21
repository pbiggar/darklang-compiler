(* MIRGraphAnalysisTests.mli - Test MIR control-flow and register-use analyses. *)
type testResult = (unit, string) result

val testGetBlockUsesCoversEveryOperandPosition : unit -> testResult
val testComputeDominatorsHandlesJoinLoopAndUnreachableBlock : unit -> testResult
val tests : (string * (unit -> testResult)) list
