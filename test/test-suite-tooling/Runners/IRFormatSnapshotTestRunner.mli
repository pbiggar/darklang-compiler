(* Execute exact IR formatting fixtures with complete failure details. *)
val runIRFormatSnapshotTest : IRFormatSnapshotFormat.irFormatSnapshotTest -> TestOutcome.t
val loadIRFormatSnapshotTests : string -> (IRFormatSnapshotFormat.irFormatSnapshotTest list,string) result
val tests : string array -> (string * (unit -> (unit,string) result)) list
