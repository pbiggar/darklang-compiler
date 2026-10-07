(*
   ControlFlowTests.mli - Verify target return transfers, print control flow, and RC instruction costs.
   A diamond needs one jump over the sibling branch, but neither a backward
   jump from that sibling nor a return-to-epilogue jump. Count emitted transfers
   rather than merely asserting a particular order of LIR labels.
   Count the whole RC operation, including literal protection and scratch
   preservation. Executable RC tests cover the heap/literal outcomes.
   The DFS work stack remains necessary. Only spills and copies
   protecting node state across payload destruction are redundant.
   Test: malformed ARM64 CFGs should be reported as codegen errors instead of silently dropping the entry.
*)
open Dark_compiler
val testPrintUInt64RuntimeZeroBranches : unit -> (unit,string) result
val testPrintUInt64RuntimePreservesNewline : unit -> (unit,string) result
val testBranchFalseEdgeFallsThrough : unit -> (unit,string) result
val testSharedReturnTransferCost : unit -> (unit,string) result
val testDynamicBufferRcInstructionCost : unit -> (unit,string) result
val testPrimitiveListPayloadPreservationCost : unit -> (unit,string) result
val testGeneratedEntryUsesAllocatorTransfersOnly : unit -> (unit,string) result
val testReportsMissingEntryBlock : unit -> (unit,string) result
val makeEmptyFunction : string -> LIR.typedLIRParam list -> LIR.functionDef
