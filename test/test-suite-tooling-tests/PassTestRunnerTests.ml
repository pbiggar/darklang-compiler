(*
   PassTestRunnerTests.fs - Unit tests for pass test runner diagnostics
   Verifies MIR pretty-printing renders CFG structure for troubleshooting.
*)
(* Retain every original parser and formatter assertion at the pass-runner boundary. *)
[@@@warning "-4-42"]
open Dark_compiler
type testResult=(unit,string) result
let (let*)=Result.bind
let expectParserError description=function Error _->Ok ()|Ok _->Error ("Expected parser error for "^description)
let testPrettyPrintMirCfg ()=
 let entry=MIR.Label "entry" and exit=MIR.Label "exit" in
 let entryBlock:MIR.basicBlock={MIR.label=entry;instrs=[MIR.Mov (MIR.VReg 0,MIR.Int64Const 1L,Some AST.TInt64)];terminator=MIR.Jump exit} in
 let exitBlock:MIR.basicBlock={MIR.label=exit;instrs=[MIR.Mov (MIR.VReg 1,MIR.Register (MIR.VReg 0),Some AST.TInt64)];terminator=MIR.Ret (MIR.Register (MIR.VReg 1))} in
 let cfg:MIR.cfg={MIR.entry;blocks=MIR.LabelMap.of_list [entry,entryBlock;exit,exitBlock]} in
 let func:MIR.functionDef={MIR.id=TestIds.functionIdForName "cfg_pretty";name="cfg_pretty";typedParams=[];returnType=AST.TInt64;cfg;floatRegs=MIR.IntSet.empty} in
 let program=MIR.Program ([func],StringOrder.Map.empty,StringOrder.Map.empty) in
 let expected="Function cfg_pretty:\n  entry:\n    v0 <- 1 : TInt64\n    jump exit\n  exit:\n    v1 <- v0 : TInt64\n    ret v1" in
 let actual=PassTestRunner.prettyPrintMIR program in if actual=expected then Ok () else Error ("Pretty-printed MIR did not match.\nExpected:\n"^expected^"\nActual:\n"^actual)
let testParseLIRRejectsNonFinalTerminator ()=match LIRParser.parseLIR "Ret\nv0 <- Mov(Imm 1)" with
 |Error msg when Text.contains msg "terminator"->Ok ()|Error msg->Error ("Expected non-final terminator error, got: "^msg)|Ok _->Error "Expected parseLIR to reject a terminator before the final line"
let testParseLIRRejectsMissingTerminator ()=match LIRParser.parseLIR "v0 <- Mov(Imm 1)" with
 |Error msg when Text.contains msg "terminator"->Ok ()|Error msg->Error ("Expected missing terminator error, got: "^msg)|Ok _->Error "Expected parseLIR to reject LIR without an explicit terminator"
let testMIRParserRejectsOutOfRangeVirtualRegister ()=match MIRParser.parseVReg "v999999999999999999999999999999999999999" with
 |Error msg when Text.contains msg "Invalid register format"->Ok ()|Error msg->Error ("Expected invalid register format error, got: "^msg)|Ok (MIR.VReg id)->Error (Printf.sprintf "Expected parseVReg to reject out-of-range register, got: VReg %d" id)
let testMIRParserAcceptsNegativeMoveLiteral ()=match MIRParser.parseMIR "v0 <- -1\nret v0" with Ok _->Ok ()|Error msg->Error ("Expected parseMIR to accept a negative move literal, got: "^msg)
let testANFParserRejectsOutOfRangeTempId ()=match ANFParser.parseTempId "t999999999999999999999999999999999999999" with
 |Error msg when Text.contains msg "Invalid temp id"->Ok ()|Error msg->Error ("Expected invalid temp id error, got: "^msg)|Ok (ANF.TempId id)->Error (Printf.sprintf "Expected parseTempId to reject out-of-range temp id, got: TempId %d" id)
let testANFParserRejectsTrailingLinesAfterReturn ()=match ANFParser.parseANF "return 1\nlet t0 = 2 + 3" with
 |Error msg when Text.contains msg "after return"->Ok ()|Error msg->Error ("Expected trailing line error after return, got: "^msg)|Ok _->Error "Expected parseANF to reject trailing lines after return"
let checkErrors cases=List.fold_left (fun state (description,result)->let* ()=state in expectParserError description result) (Ok ()) cases
let testLIRParserRejectsOutOfRangeNumericFields ()=checkErrors ["virtual register",LIRParser.parseLIR "v999999999999999999999 <- Mov(Imm 1)";"immediate",LIRParser.parseLIR "v0 <- Mov(Imm 999999999999999999999)";"stack slot",LIRParser.parseLIR "Store(Stack 999999999999999999999, v0)"]
let testARM64ParserRejectsOutOfRangeNumericFields ()=
 let cases=["MOVZ immediate","MOVZ(X0, 65536, 0)";"ADD_imm immediate","ADD_imm(X0, X1, 4096)";"SUB_imm immediate","SUB_imm(X0, X1, 4096)";"STR offset","STR(X0, SP, 32768)"] in
 let rawCases=List.map (fun (name,text)->"raw "^name,ARM64Parser.parseARM64 text) cases in
 let symbolicCases=List.map (fun (name,text)->"symbolic "^name,ARM64SymbolicParser.parseARM64Symbolic text) cases in
 let* ()=checkErrors rawCases in checkErrors symbolicCases
let registerNames=(List.init 31 Fun.id |> List.filter (fun i->i<>18) |> List.map (Printf.sprintf "X%d"))@["SP"]
let testARM64ParsersAcceptAllGeneralPurposeRegisters ()=
 let rawCases=List.map (fun reg->"raw "^reg,ARM64Parser.parseARM64 ("MOV_reg("^reg^", X0)")) registerNames in
 let symbolicCases=List.map (fun reg->"symbolic "^reg,ARM64SymbolicParser.parseARM64Symbolic ("MOV_reg("^reg^", X0)")) registerNames in
 let check cases=List.fold_left (fun state (description,result)->let* ()=state in match result with Ok _->Ok ()|Error msg->Error ("Expected parser success for "^description^", got: "^msg)) (Ok ()) cases in
 let* ()=check rawCases in check symbolicCases
let testLIRParserAcceptsAllPhysicalRegisters ()=
 let names=List.filter ((<>) "X28") registerNames in
 List.fold_left (fun state reg->let* ()=state in match LIRParser.parseLIR (reg^" <- Mov(Reg X0)\nRet") with Ok _->Ok ()|Error msg->Error ("Expected LIR parser success for "^reg^", got: "^msg)) (Ok ()) names
let testARM64SymbolicParserReportsOriginalLineNumber ()=match ARM64SymbolicParser.parseARM64Symbolic "// generated setup\n\nMOV_reg(X0, X1)\nMOV_reg(NOPE, X0)" with
 |Error msg when Text.contains msg "Line 4:"->Ok ()|Error msg->Error ("Expected original source line 4 in parser error, got: "^msg)|Ok _->Error "Expected parser error for invalid symbolic ARM64 register"
let testLIRParserReportsOriginalLineNumber ()=match LIRParser.parseLIR "// generated setup\n\nv0 <- Mov(Imm 1)\nv1 <- Mov(Reg NOPE)" with
 |Error msg when Text.contains msg "Line 4:"->Ok ()|Error msg->Error ("Expected original source line 4 in parser error, got: "^msg)|Ok _->Error "Expected parser error for invalid LIR register"
let testARM64SymbolicParserAcceptsBranchInstructions ()=
 let text="CBZ(X0, zero_label)\nCBNZ(X1, nonzero_label)\nB_label(done)\nB_cond_label(EQ, equal_label)\nB(12)\nB_cond(NE, -4)" in
 match ARM64SymbolicParser.parseARM64Symbolic text with
 |Ok [Symbolic.CBZ (ARM64.X0,"zero_label");Symbolic.CBNZ (ARM64.X1,"nonzero_label");Symbolic.B_label "done";Symbolic.B_cond_label (ARM64.EQ,"equal_label");Symbolic.B 12;Symbolic.B_cond (ARM64.NE,-4)]->Ok ()
 |Ok instrs->Error ("Expected symbolic branch instructions to parse exactly, got: ["^String.concat "; " (List.map MachineDiagnostic.symbolic instrs)^"]")
 |Error msg->Error ("Expected symbolic branch instructions to parse, got: "^msg)
let testLIRToARM64ComparisonIgnoresExpectedLabels ()=
 match LIRParser.parseLIR "X1 <- Mov(Imm 10)\nX1 <- Sub(X1, Imm 3)\nX0 <- Mov(Reg X1)\nRet" with
 |Error msg->Error ("Failed to parse LIR input: "^msg)
 |Ok input->
 let renamed=PassTestRunner.renameLIRFunctions "test" input in
 let expected=[Symbolic.Label "test";Symbolic.STP_pre (ARM64.X29,ARM64.X30,ARM64.SP,-16);Symbolic.MOV_reg (ARM64.X29,ARM64.SP);Symbolic.MOVZ (ARM64.X1,10,0);Symbolic.SUB_imm (ARM64.X1,ARM64.X1,3);Symbolic.MOV_reg (ARM64.X0,ARM64.X1);Symbolic.LDP_post (ARM64.X29,ARM64.X30,ARM64.SP,16);Symbolic.RET] in
 let result=PassTestRunner.runLIR2ARM64Test renamed expected in if result.PassTestRunner.success then Ok () else Error ("Expected comparison to ignore expected labels, got: "^result.PassTestRunner.message)
let tests=["pretty print MIR CFG",testPrettyPrintMirCfg;"parse LIR rejects non-final terminator",testParseLIRRejectsNonFinalTerminator;"parse LIR rejects missing terminator",testParseLIRRejectsMissingTerminator;"MIR parser rejects out-of-range virtual register",testMIRParserRejectsOutOfRangeVirtualRegister;"MIR parser accepts negative move literal",testMIRParserAcceptsNegativeMoveLiteral;"ANF parser rejects out-of-range temp id",testANFParserRejectsOutOfRangeTempId;"ANF parser rejects trailing lines after return",testANFParserRejectsTrailingLinesAfterReturn;"LIR parser rejects out-of-range numeric fields",testLIRParserRejectsOutOfRangeNumericFields;"ARM64 parsers reject out-of-range numeric fields",testARM64ParserRejectsOutOfRangeNumericFields;"ARM64 parsers accept all general-purpose registers",testARM64ParsersAcceptAllGeneralPurposeRegisters;"LIR parser accepts all physical registers",testLIRParserAcceptsAllPhysicalRegisters;"ARM64 symbolic parser reports original line number",testARM64SymbolicParserReportsOriginalLineNumber;"LIR parser reports original line number",testLIRParserReportsOriginalLineNumber;"ARM64 symbolic parser accepts branch instructions",testARM64SymbolicParserAcceptsBranchInstructions;"LIR to ARM64 comparison ignores expected labels",testLIRToARM64ComparisonIgnoresExpectedLabels]
let runAll ()=let rec run=function []->Ok ()|(name,test)::rest->match test () with Ok ()->run rest|Error msg->Error (name^" test failed: "^msg) in run tests
