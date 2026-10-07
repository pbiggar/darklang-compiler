(* Original ANF-to-MIR lowering tests. *)
type testResult = (unit, string) result

val testRawGetIntrinsicReturnTypeDoesNotDefaultToInt64 : unit -> testResult
val testBuildVariantRegistryRejectsInconsistentTypeParams : unit -> testResult
val testRecordAllocationStartsFieldsAtOffsetZero : unit -> testResult
val testNestedTerminalBranchesHaveNoInventedReturn : unit -> testResult
val testPhiEdgesPreserveDistinctPredecessors : unit -> testResult
val testPreRcSsaTypesBranchLocalDefinitions : unit -> testResult
val testSsaReturnFlowAcrossJoinAndLoop : unit -> testResult
val testSsaReturnFlowThroughSwappedParameters : unit -> testResult
val testTypeInformationComposition : unit -> testResult
val tests : (string * (unit -> testResult)) list
