(* Original progress-bar tests. *)
type testResult=(unit,string) result
val testProgressBarHandlesOverCompletion : unit -> testResult
val testProgressBarClampsOverCompletionDisplay : unit -> testResult
val tests : (string * (unit -> testResult)) list
