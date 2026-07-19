// PassTestRunnerTests.fs - Unit tests for pass test runner diagnostics
//
// Verifies MIR pretty-printing renders CFG structure for troubleshooting.

module PassTestRunnerTests

open MIR
open TestDSL.ANFParser
open TestDSL.MIRParser
open TestDSL.LIRParser
open TestDSL.PassTestRunner

type TestResult = Result<unit, string>

let private expectParserError (description: string) (result: Result<'a, string>) : TestResult =
    match result with
    | Error _ -> Ok ()
    | Ok _ -> Error $"Expected parser error for {description}"

let testPrettyPrintMirCfg () : TestResult =
    let entry = Label "entry"
    let exit = Label "exit"
    let entryBlock: MIR.BasicBlock = {
        Label = entry
        Instrs = [ Mov (VReg 0, Int64Const 1L, Some AST.TInt64) ]
        Terminator = Jump exit
    }
    let exitBlock: MIR.BasicBlock = {
        Label = exit
        Instrs = [ Mov (VReg 1, Register (VReg 0), Some AST.TInt64) ]
        Terminator = Ret (Register (VReg 1))
    }
    let cfg: MIR.CFG = {
        Entry = entry
        Blocks = Map.ofList [ (entry, entryBlock); (exit, exitBlock) ]
    }
    let func: MIR.Function = {
        Name = "cfg_pretty"
        TypedParams = []
        ReturnType = AST.TInt64
        CFG = cfg
        FloatRegs = Set.empty
    }
    let program = MIR.Program ([func], Map.empty, Map.empty)
    let expected =
        [
            "Function cfg_pretty:"
            "  entry:"
            "    v0 <- 1 : TInt64"
            "    jump exit"
            "  exit:"
            "    v1 <- v0 : TInt64"
            "    ret v1"
        ]
        |> String.concat "\n"
    let actual = prettyPrintMIR program
    if actual = expected then
        Ok ()
    else
        Error $"Pretty-printed MIR did not match.\nExpected:\n{expected}\nActual:\n{actual}"

let testParseLIRRejectsNonFinalTerminator () : TestResult =
    let text =
        [
            "Ret"
            "v0 <- Mov(Imm 1)"
        ]
        |> String.concat "\n"
    match parseLIR text with
    | Error msg when msg.Contains("terminator") -> Ok ()
    | Error msg -> Error $"Expected non-final terminator error, got: {msg}"
    | Ok _ -> Error "Expected parseLIR to reject a terminator before the final line"

let testParseLIRRejectsMissingTerminator () : TestResult =
    let text =
        [
            "v0 <- Mov(Imm 1)"
        ]
        |> String.concat "\n"
    match parseLIR text with
    | Error msg when msg.Contains("terminator") -> Ok ()
    | Error msg -> Error $"Expected missing terminator error, got: {msg}"
    | Ok _ -> Error "Expected parseLIR to reject LIR without an explicit terminator"

let testMIRParserRejectsOutOfRangeVirtualRegister () : TestResult =
    match parseVReg "v999999999999999999999999999999999999999" with
    | Error msg when msg.Contains("Invalid register format") -> Ok ()
    | Error msg -> Error $"Expected invalid register format error, got: {msg}"
    | Ok reg -> Error $"Expected parseVReg to reject out-of-range register, got: {reg}"

let testMIRParserAcceptsNegativeMoveLiteral () : TestResult =
    let text =
        [
            "v0 <- -1"
            "ret v0"
        ]
        |> String.concat "\n"

    match parseMIR text with
    | Ok _ -> Ok ()
    | Error msg -> Error $"Expected parseMIR to accept a negative move literal, got: {msg}"

let testANFParserRejectsOutOfRangeTempId () : TestResult =
    match parseTempId "t999999999999999999999999999999999999999" with
    | Error msg when msg.Contains("Invalid temp id") -> Ok ()
    | Error msg -> Error $"Expected invalid temp id error, got: {msg}"
    | Ok tempId -> Error $"Expected parseTempId to reject out-of-range temp id, got: {tempId}"

let testANFParserRejectsTrailingLinesAfterReturn () : TestResult =
    let text =
        [
            "return 1"
            "let t0 = 2 + 3"
        ]
        |> String.concat "\n"

    match parseANF text with
    | Error msg when msg.Contains("after return") -> Ok ()
    | Error msg -> Error $"Expected trailing line error after return, got: {msg}"
    | Ok _ -> Error "Expected parseANF to reject trailing lines after return"

let testLIRParserRejectsOutOfRangeNumericFields () : TestResult =
    let cases =
        [
            "virtual register", parseLIR "v999999999999999999999 <- Mov(Imm 1)"
            "immediate", parseLIR "v0 <- Mov(Imm 999999999999999999999)"
            "stack slot", parseLIR "Store(Stack 999999999999999999999, v0)"
        ]

    cases
    |> List.fold
        (fun state (description, result) ->
            state
            |> Result.bind (fun () -> expectParserError description result))
        (Ok ())

let testARM64ParserRejectsOutOfRangeNumericFields () : TestResult =
    let rawCases =
        [
            "raw MOVZ immediate", TestDSL.ARM64Parser.parseARM64 "MOVZ(X0, 65536, 0)"
            "raw ADD_imm immediate", TestDSL.ARM64Parser.parseARM64 "ADD_imm(X0, X1, 4096)"
            "raw SUB_imm immediate", TestDSL.ARM64Parser.parseARM64 "SUB_imm(X0, X1, 4096)"
            "raw STR offset", TestDSL.ARM64Parser.parseARM64 "STR(X0, SP, 32768)"
        ]
    let symbolicCases =
        [
            "symbolic MOVZ immediate", TestDSL.ARM64SymbolicParser.parseARM64Symbolic "MOVZ(X0, 65536, 0)"
            "symbolic ADD_imm immediate", TestDSL.ARM64SymbolicParser.parseARM64Symbolic "ADD_imm(X0, X1, 4096)"
            "symbolic SUB_imm immediate", TestDSL.ARM64SymbolicParser.parseARM64Symbolic "SUB_imm(X0, X1, 4096)"
            "symbolic STR offset", TestDSL.ARM64SymbolicParser.parseARM64Symbolic "STR(X0, SP, 32768)"
        ]
    rawCases
    |> List.fold
        (fun state (description, result) ->
            state
            |> Result.bind (fun () -> expectParserError description result))
        (Ok ())
    |> Result.bind (fun () ->
        symbolicCases
        |> List.fold
            (fun state (description, result) ->
                state
                |> Result.bind (fun () -> expectParserError description result))
            (Ok ()))

let testARM64ParsersAcceptAllGeneralPurposeRegisters () : TestResult =
    let registerNames =
        [
            "X0"; "X1"; "X2"; "X3"; "X4"; "X5"; "X6"; "X7"; "X8"; "X9"
            "X10"; "X11"; "X12"; "X13"; "X14"; "X15"; "X16"; "X17"
            "X19"; "X20"; "X21"; "X22"; "X23"; "X24"; "X25"; "X26"; "X27"; "X28"
            "X29"; "X30"; "SP"
        ]

    let rawCases =
        registerNames
        |> List.map (fun reg -> $"raw {reg}", TestDSL.ARM64Parser.parseARM64 $"MOV_reg({reg}, X0)")

    let symbolicCases =
        registerNames
        |> List.map (fun reg -> $"symbolic {reg}", TestDSL.ARM64SymbolicParser.parseARM64Symbolic $"MOV_reg({reg}, X0)")

    rawCases
    |> List.fold
        (fun state (description, result) ->
            state
            |> Result.bind (fun () ->
                match result with
                | Ok _ -> Ok ()
                | Error msg -> Error $"Expected parser success for {description}, got: {msg}"))
        (Ok ())
    |> Result.bind (fun () ->
        symbolicCases
        |> List.fold
            (fun state (description, result) ->
                state
                |> Result.bind (fun () ->
                    match result with
                    | Ok _ -> Ok ()
                    | Error msg -> Error $"Expected parser success for {description}, got: {msg}"))
            (Ok ()))

let testLIRParserAcceptsAllPhysicalRegisters () : TestResult =
    let registerNames =
        [
            "X0"; "X1"; "X2"; "X3"; "X4"; "X5"; "X6"; "X7"; "X8"; "X9"
            "X10"; "X11"; "X12"; "X13"; "X14"; "X15"; "X16"; "X17"
            "X19"; "X20"; "X21"; "X22"; "X23"; "X24"; "X25"; "X26"; "X27"
            "X29"; "X30"; "SP"
        ]

    registerNames
    |> List.fold
        (fun state reg ->
            state
            |> Result.bind (fun () ->
                match parseLIR $"{reg} <- Mov(Reg X0)\nRet" with
                | Ok _ -> Ok ()
                | Error msg -> Error $"Expected LIR parser success for {reg}, got: {msg}"))
        (Ok ())

let testARM64SymbolicParserReportsOriginalLineNumber () : TestResult =
    let text =
        [
            "// generated setup"
            ""
            "MOV_reg(X0, X1)"
            "MOV_reg(NOPE, X0)"
        ]
        |> String.concat "\n"

    match TestDSL.ARM64SymbolicParser.parseARM64Symbolic text with
    | Error msg when msg.Contains("Line 4:") -> Ok ()
    | Error msg -> Error $"Expected original source line 4 in parser error, got: {msg}"
    | Ok _ -> Error "Expected parser error for invalid symbolic ARM64 register"

let testLIRParserReportsOriginalLineNumber () : TestResult =
    let text =
        [
            "// generated setup"
            ""
            "v0 <- Mov(Imm 1)"
            "v1 <- Mov(Reg NOPE)"
        ]
        |> String.concat "\n"

    match parseLIR text with
    | Error msg when msg.Contains("Line 4:") -> Ok ()
    | Error msg -> Error $"Expected original source line 4 in parser error, got: {msg}"
    | Ok _ -> Error "Expected parser error for invalid LIR register"

let testARM64SymbolicParserAcceptsBranchInstructions () : TestResult =
    let text =
        [
            "CBZ(X0, zero_label)"
            "CBNZ(X1, nonzero_label)"
            "B_label(done)"
            "B_cond_label(EQ, equal_label)"
            "B(12)"
            "B_cond(NE, -4)"
        ]
        |> String.concat "\n"

    match TestDSL.ARM64SymbolicParser.parseARM64Symbolic text with
    | Ok [
        global.ARM64Symbolic.CBZ (ARM64.X0, "zero_label")
        global.ARM64Symbolic.CBNZ (ARM64.X1, "nonzero_label")
        global.ARM64Symbolic.B_label "done"
        global.ARM64Symbolic.B_cond_label (ARM64.EQ, "equal_label")
        global.ARM64Symbolic.B 12
        global.ARM64Symbolic.B_cond (ARM64.NE, -4)
      ] -> Ok ()
    | Ok instrs ->
        Error $"Expected symbolic branch instructions to parse exactly, got: {instrs}"
    | Error msg ->
        Error $"Expected symbolic branch instructions to parse, got: {msg}"

let tests = [
    ("pretty print MIR CFG", testPrettyPrintMirCfg)
    ("parse LIR rejects non-final terminator", testParseLIRRejectsNonFinalTerminator)
    ("parse LIR rejects missing terminator", testParseLIRRejectsMissingTerminator)
    ("MIR parser rejects out-of-range virtual register", testMIRParserRejectsOutOfRangeVirtualRegister)
    ("MIR parser accepts negative move literal", testMIRParserAcceptsNegativeMoveLiteral)
    ("ANF parser rejects out-of-range temp id", testANFParserRejectsOutOfRangeTempId)
    ("ANF parser rejects trailing lines after return", testANFParserRejectsTrailingLinesAfterReturn)
    ("LIR parser rejects out-of-range numeric fields", testLIRParserRejectsOutOfRangeNumericFields)
    ("ARM64 parsers reject out-of-range numeric fields", testARM64ParserRejectsOutOfRangeNumericFields)
    ("ARM64 parsers accept all general-purpose registers", testARM64ParsersAcceptAllGeneralPurposeRegisters)
    ("LIR parser accepts all physical registers", testLIRParserAcceptsAllPhysicalRegisters)
    ("ARM64 symbolic parser reports original line number", testARM64SymbolicParserReportsOriginalLineNumber)
    ("LIR parser reports original line number", testLIRParserReportsOriginalLineNumber)
    ("ARM64 symbolic parser accepts branch instructions", testARM64SymbolicParserAcceptsBranchInstructions)
]

let runAll () : TestResult =
    let rec run remaining =
        match remaining with
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> run rest
            | Error msg -> Error $"{name} test failed: {msg}"
    run tests
