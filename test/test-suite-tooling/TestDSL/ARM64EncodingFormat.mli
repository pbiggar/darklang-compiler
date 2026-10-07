type arm64EncodingExpectation = EncodesTo of int32 list | EncodingErrorContaining of string
type arm64EncodingTest = {name:string;instructions:Dark_compiler.ARM64.instr list;expectation:arm64EncodingExpectation;assertDifferent:bool}
val parseHexValue : string -> (int32,string) result
val parseARM64EncodingTest : string -> (arm64EncodingTest,string) result
