(* Original multi-kind IR formatter fixture tests. *)
type testResult = (unit, string) result

val testParsesAndRunsMultipleIRKinds : unit -> testResult
val testRejectsUnknownIRKind : unit -> testResult
val tests : (string * (unit -> testResult)) list
