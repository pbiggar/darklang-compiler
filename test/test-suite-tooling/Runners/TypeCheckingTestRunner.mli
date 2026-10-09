(* Execute parsed type expectations through the direct source checker. *)
type typeCheckingTestResult = {
  success : bool;
  message : string;
  expectedType : Dark_compiler.AST.semanticType option;
  actualType : Dark_compiler.AST.semanticType option;
  expectedError : bool;
  actualError : string option;
}

val runTypeCheckingTest :
  TypeCheckingFormat.typeCheckingTest -> typeCheckingTestResult

val runTypeCheckingTestFile :
  string -> (typeCheckingTestResult list, string) result
