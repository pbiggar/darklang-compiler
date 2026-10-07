(* CompilerReachability.mli - Query standard-library reachability through the compilation pipeline. *)
val getAllStdlibFunctionNamesFromStdlib :
  CompilationContexts.stdlibResult -> StringOrder.Set.t

val getReachableStdlibFunctionsFromStdlib :
  CompilationContexts.stdlibResult ->
  string ->
  (StringOrder.Set.t, string) result
