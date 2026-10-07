(* Original JsonPlanningTests declarations. *)
type testResult=(unit,string) result
val testNonJsonProgramIsUnchanged : Dark_compiler.CompilationContexts.stdlibResult -> unit -> testResult
val tests : Dark_compiler.CompilationContexts.stdlibResult -> (string * (unit -> testResult)) list
