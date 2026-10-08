(*
   E2ETestRunner.mli - End-to-end test runner

   Compiles source code, executes it, and validates output/exit code.
   Internal identifiers are only allowed for stdlib-internal tests.
   Retain parsed actual and expected expressions inside a checked assertion.
   Older E2E lines place an entry after a function declaration's semicolon.
   The interpreter parser treats that semicolon as part of the function body.
   Preserve evaluation order for legacy parenthesized `;` sequences that the
   copied parser cannot read directly, before synthesizing a value assertion.
   Result of running an E2E test
   A value-equality test whose synthesized checker is a single expression and
   can therefore share one compiler invocation with other compatible checks.
   One physical compile/run with one logical result for every test in it.
   Keep the bound finite so an accidental command-line value cannot synthesize
   an arbitrarily large compiler input. This is larger than the complete E2E
   corpus, allowing a requested batch to contain every compatible test.
   Each result chunk deliberately uses only the low 32 bits of an Int64. This
   keeps every printed mask non-negative and makes the final partial chunk easy
   to validate without relying on signed overflow behavior.
   Only value-equality tests with no process contract can share a process. The
   compiler path and options remain production-identical; only the synthesized
   caller contains several independent checks.
   Map of built preamble contexts and their matching stdlib specialization set,
   keyed by source file + preamble text.
   Keep only top-level definitions and their continuation lines.
   This is used by upstream reduced-preamble fallback when the raw per-test
   preamble includes non-definition noise that cannot be parsed standalone.
   Build suite stdlib specializations and per-file/per-preamble contexts
   Interpreter execution fixtures spell a returned Result.Error as `error=...`.
   Native execution renders the value instead of converting it to a process
   failure, so recognize that canonical Result presentation at this boundary.
   QEMU makes the intentional 500,000-iteration TCO stress case much
   slower than native execution while still completing reliably.
   Execute a compiler-library result on its declared target. Unit integration
   tests use the same explicit QEMU boundary as E2E tests when the target does
   not match the development host.
   Run E2E test using a prebuilt preamble context.
   Individual runs and batches compile the same comparison expression tree.
*)
open Dark_compiler
open E2EFormat

type e2eRun =
  | CompileFailed of int * string * int64
  | Ran of int * string * string * int64 * int64

type e2eFailure = { run : e2eRun; message : string }
type e2eTestResult = (e2eRun, e2eFailure) result

type preparedE2EBatchTest = {
  test : e2eTest;
  equalityProgram : WrittenTypes.sourceFile;
}

type e2eBatchExecution = {
  aggregateRun : e2eRun;
  results : (e2eTest * e2eTestResult) list;
}

val maxSupportedBatchSize : int
val tryPrepareBatchTest : e2eTest -> preparedE2EBatchTest option

type preambleContextKey = string * string

val preambleContextKeyForTest : e2eTest -> preambleContextKey

module PreambleContextMap : Map.S with type key = preambleContextKey

type suiteContext = {
  preambleContexts :
    (CompilationContexts.stdlibResult * CompilationContexts.preambleContext)
    PreambleContextMap.t;
}

val buildSuiteContexts :
  CompilationContexts.stdlibResult ->
  e2eTest array ->
  CompilerOptions.passTimingRecorder option ->
  (suiteContext, string) result

val executeBinaryForTarget :
  Platform.target -> bytes -> (CompilerOptions.executionOutput, string) result

val canBatchTogether : preparedE2EBatchTest -> preparedE2EBatchTest -> bool

val runE2ETestBatchWithPreambleContext :
  CompilationContexts.stdlibResult ->
  CompilationContexts.preambleContext ->
  CompilationSession.compilationSession option ->
  preparedE2EBatchTest list ->
  CompilerOptions.passTimingRecorder option ->
  e2eBatchExecution

val runE2ETestWithPreambleContext :
  CompilationContexts.stdlibResult ->
  CompilationContexts.preambleContext ->
  CompilationSession.compilationSession option ->
  e2eTest ->
  CompilerOptions.passTimingRecorder option ->
  e2eTestResult

val runPreparedE2ETestWithPreambleContext :
  CompilationContexts.stdlibResult ->
  CompilationContexts.preambleContext ->
  CompilationSession.compilationSession option ->
  preparedE2EBatchTest ->
  CompilerOptions.passTimingRecorder option ->
  e2eTestResult

val evaluateExpectations : e2eTest -> e2eRun -> e2eTestResult

(* Diagnostic scaffolding only; executable assertions are inserted as trees. *)
val buildBatchSource : preparedE2EBatchTest list -> string
val tryParseBatchBoolResults : int -> string -> bool list option
