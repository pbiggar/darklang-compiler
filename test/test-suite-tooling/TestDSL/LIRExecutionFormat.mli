(*
   LIRExecutionFormat.mli - Parser for executable single-block LIR fixtures.
   Keeps successful process results and expected codegen failures typed.
*)
type leakCheckMode = LeakCheckDisabled | LeakCheckEnabled

type processExpectation =
  | ExpectedExitCode of int
  | ExpectedStdout of string
  | ExpectedStderr of string

type lIRExecutionExpectation =
  | ExpectedProcessResult of processExpectation list
  | ExpectedCodegenError of string

type lIRExecutionTest = {
  name : string;
  program : Dark_compiler.LIR.program;
  leakCheck : leakCheckMode;
  expectation : lIRExecutionExpectation;
  sourceFile : string;
}

val parseLIRExecutionFileContent :
  string -> string -> (lIRExecutionTest list, string) result
