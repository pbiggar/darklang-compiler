(* Original StdlibOptimizationTests declarations. *)
type testResult = (unit, string) result

val tests :
  Dark_compiler.CompilationContexts.stdlibResult ->
  (string * (unit -> testResult)) list
