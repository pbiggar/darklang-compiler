(* Parse the original line-based type checking test format. *)
type typeExpectation = ExpectType of Dark_compiler.AST.semanticType | ExpectError
type typeCheckingTest = {name : string; source : string; expectation : typeExpectation}
val parseTypeCheckingTestFile : string -> (typeCheckingTest list, string) result
