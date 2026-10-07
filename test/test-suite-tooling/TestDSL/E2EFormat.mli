(* E2EFormat.mli - Parse unchanged source, expectations, options and scoped preambles. *)
module StringMap : Map.S with type key = string
type testStdin = Closed | Bytes of string
type outputMatch = NormalizedText | ExactBytes
type errorExpectation = AnyError | CompileError
type e2eTest = {
  name: string;
  sourceLine: int;
  source: string;
  expectedValueExpr: string option;
  preamble: string;
  expectedStdout: string option;
  expectedStderr: string option;
  arguments: string list;
  environment: (string * string) list;
  stdin: testStdin;
  outputMatch: outputMatch;
  isolated: bool;
  expectedExitCode: int;
  errorExpectation: errorExpectation option;
  expectedErrorMessage: string option;
  skipReason: string option;
  disableFreeList: bool;
  disableANFOpt: bool;
  disableANFConstFolding: bool;
  disableANFConstProp: bool;
  disableANFCopyProp: bool;
  disableANFDCE: bool;
  disableANFStrengthReduction: bool;
  disableInlining: bool;
  disableTCO: bool;
  disableMIROpt: bool;
  disableMIRSCCP: bool;
  disableMIRCSE: bool;
  disableMIRDCE: bool;
  disableMIRLICM: bool;
  disableLIROpt: bool;
  disableLIRPeephole: bool;
  disableFunctionTreeShaking: bool;
  disableLeakCheck: bool;
  sourceFile: string;
  functionLineMap: int StringMap.t;
}
val parseE2ETestFile : string -> (e2eTest list, string) result
val parseE2ETest : string -> (e2eTest, string) result
