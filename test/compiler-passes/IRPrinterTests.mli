(* Original pinned MIR formatting tests. *)
type testResult = (unit, string) result

val testFormatMIR : unit -> testResult
val testFormatMIRDumpFiltersBeforeFormatting : unit -> testResult
val testFormatMIRDumpSummary : unit -> testResult
val testFormatMIRDumpReportsNoMatches : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
