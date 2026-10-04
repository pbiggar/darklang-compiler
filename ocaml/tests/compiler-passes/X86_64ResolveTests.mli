(* Original X86_64ResolveTests declarations. *)
type testResult=(unit,string) result
val testRequireLabelPositionRejectsMissingStart : unit -> testResult
val testCallAndExecute : unit -> testResult
val tests : (string * (unit -> testResult)) list
