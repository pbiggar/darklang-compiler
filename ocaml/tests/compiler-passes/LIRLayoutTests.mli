type testResult = (unit, string) result
val testLayoutDefersSharedReturn : unit -> testResult
val testLayoutPreservesOtherReturnShapes : unit -> testResult
val testLayoutLeavesMissingSuccessorValidationToConsumers : unit -> testResult
val testLayoutReportsMissingEntryBlock : unit -> testResult
val tests : (string * (unit -> testResult)) list
