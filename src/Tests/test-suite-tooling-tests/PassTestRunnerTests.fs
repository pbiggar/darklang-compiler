// PassTestRunnerTests.fs - Unit tests for pass test runner diagnostics
//
// Verifies MIR pretty-printing renders CFG structure for troubleshooting.

module PassTestRunnerTests

open MIR
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

let testMIRParserRejectsOutOfRangeVirtualRegister () : TestResult =
    match parseVReg "v999999999999999999999999999999999999999" with
    | Error msg when msg.Contains("Invalid register format") -> Ok ()
    | Error msg -> Error $"Expected invalid register format error, got: {msg}"
    | Ok reg -> Error $"Expected parseVReg to reject out-of-range register, got: {reg}"

let testARM64ParserRejectsOutOfRangeNumericFields () : TestResult =
    let rawCases =
        [
            "raw MOVZ immediate", TestDSL.ARM64Parser.parseARM64 "MOVZ(X0, 65536, 0)"
            "raw STR offset", TestDSL.ARM64Parser.parseARM64 "STR(X0, SP, 32768)"
        ]
    let symbolicCases =
        [
            "symbolic MOVZ immediate", TestDSL.ARM64SymbolicParser.parseARM64Symbolic "MOVZ(X0, 65536, 0)"
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

let tests = [
    ("pretty print MIR CFG", testPrettyPrintMirCfg)
    ("parse LIR rejects non-final terminator", testParseLIRRejectsNonFinalTerminator)
    ("MIR parser rejects out-of-range virtual register", testMIRParserRejectsOutOfRangeVirtualRegister)
    ("ARM64 parsers reject out-of-range numeric fields", testARM64ParserRejectsOutOfRangeNumericFields)
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
