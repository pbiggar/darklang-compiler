(*
   OptimizationTestRunner.mli - Test runner for optimization verification
   Compiles source code, captures IR at specific stages, and compares
   against expected output to verify optimizations work correctly.
   Result of running an optimization test
   Normalize IR output for comparison
   - Trim whitespace
   - Normalize line endings
   - Remove trailing whitespace from each line
   - Alpha-rename temporary IDs, which are allocation-order details shared
   with unrelated functions in the compiled standard library.
   - Alpha-rename generated closure IDs for the same reason.
   Compile source and get ANF after optimization
   Type check
   Convert to ANF
   Optimize ANF
   Pretty-print the result
   Compile source and get MIR after optimization
   Type check
   Convert to ANF
   Optimize ANF
   Generated output participates in reference-count insertion.
   Convert to MIR
   SSA construction
   MIR optimization
   SSA form is now preserved (phi resolution happens in register allocation)
   Pretty-print the optimized MIR (still in SSA form)
   Compile source and get LIR after optimization
   Type check
   Convert to ANF
   Optimize ANF
   Generated output participates in reference-count insertion.
   Convert to MIR
   SSA construction and optimization
   SSA form is now preserved (phi resolution happens in register allocation)
   Convert to LIR
   LIR optimization
   Pretty-print
   Run a single optimization test
   Load and run tests from a file
*)
open Dark_compiler
type optimizationTestResult=TestOutcome.t={success:bool;message:string;expected:string option;actual:string option}
val normalizeIR : string -> string
val getOptimizedANF : CompilationContexts.stdlibResult -> CompilerOptions.passTimingRecorder option -> string -> (string,string) result
val getOptimizedStdlibANF : CompilationContexts.stdlibResult -> string -> (string,string) result
val getOptimizedMIR : CompilationContexts.stdlibResult -> CompilerOptions.passTimingRecorder option -> string -> (string,string) result
val getOptimizedLIR : CompilationContexts.stdlibResult -> CompilerOptions.passTimingRecorder option -> string -> (string,string) result
val runOptimizationTest : CompilationContexts.stdlibResult -> CompilerOptions.passTimingRecorder option -> OptimizationFormat.optimizationTest -> optimizationTestResult
val runTestFile : CompilationContexts.stdlibResult -> CompilerOptions.passTimingRecorder option -> (OptimizationFormat.optimizationTest -> bool) -> OptimizationFormat.irStage -> string -> ((OptimizationFormat.optimizationTest * optimizationTestResult) list,string) result
