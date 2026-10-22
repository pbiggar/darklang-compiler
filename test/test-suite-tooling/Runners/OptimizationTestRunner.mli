(* OptimizationTestRunner.mli - Check optimization fixtures with stable IR formatting.
   Source cases use production SSA optimization and ownership insertion;
   MIR/LIR cases also run tail-call detection and direct SSA MIR lowering. *)
open Dark_compiler

type optimizationTestResult = TestOutcome.t = {
  success : bool;
  message : string;
  expected : string option;
  actual : string option;
}

val normalizeIR : string -> string

val getOptimizedANF :
  CompilationContexts.stdlibResult ->
  CompilerOptions.passTimingRecorder option ->
  string ->
  (string, string) result

val getOptimizedStdlibANF :
  CompilationContexts.stdlibResult -> string -> (string, string) result

val getOptimizedMIR :
  CompilationContexts.stdlibResult ->
  CompilerOptions.passTimingRecorder option ->
  string ->
  (string, string) result

val getOptimizedLIR :
  CompilationContexts.stdlibResult ->
  CompilerOptions.passTimingRecorder option ->
  string ->
  (string, string) result

val runOptimizationTest :
  CompilationContexts.stdlibResult ->
  CompilerOptions.passTimingRecorder option ->
  OptimizationFormat.optimizationTest ->
  optimizationTestResult

val runTestFile :
  CompilationContexts.stdlibResult ->
  CompilerOptions.passTimingRecorder option ->
  (OptimizationFormat.optimizationTest -> bool) ->
  OptimizationFormat.irStage ->
  string ->
  ( (OptimizationFormat.optimizationTest * optimizationTestResult) list,
    string )
  result
