(* Original compiler-pass diagnostics and parser boundary assertions. *)
type testResult = (unit, string) result

val testPrettyPrintMirCfg : unit -> testResult
val testParseLIRRejectsNonFinalTerminator : unit -> testResult
val testParseLIRRejectsMissingTerminator : unit -> testResult
val testMIRParserRejectsOutOfRangeVirtualRegister : unit -> testResult
val testMIRParserAcceptsNegativeMoveLiteral : unit -> testResult
val testANFParserRejectsOutOfRangeTempId : unit -> testResult
val testANFParserRejectsTrailingLinesAfterReturn : unit -> testResult
val testLIRParserRejectsOutOfRangeNumericFields : unit -> testResult
val testARM64ParserRejectsOutOfRangeNumericFields : unit -> testResult
val testARM64ParsersAcceptAllGeneralPurposeRegisters : unit -> testResult
val testLIRParserAcceptsAllPhysicalRegisters : unit -> testResult
val testARM64SymbolicParserReportsOriginalLineNumber : unit -> testResult
val testLIRParserReportsOriginalLineNumber : unit -> testResult
val testARM64SymbolicParserAcceptsBranchInstructions : unit -> testResult
val testLIRToARM64ComparisonIgnoresExpectedLabels : unit -> testResult
val tests : (string * (unit -> testResult)) list
val runAll : unit -> testResult
