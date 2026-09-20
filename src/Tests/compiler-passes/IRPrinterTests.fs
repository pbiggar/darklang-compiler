// IRPrinterTests.fs - Unit tests for shared IR formatting
//
// Ensures IRPrinter outputs match pinned formatting for MIR/LIR programs.

module IRPrinterTests

open MIR
open MIRPrinter
type TestResult = Result<unit, string>

let private expectFormatted (label: string) (expected: string) (actual: string) : TestResult =
    if actual = expected then
        Ok ()
    else
        Error $"{label} did not match.\nExpected:\n{expected}\nActual:\n{actual}"

let testFormatMIR () : TestResult =
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
        Id = TestIds.functionIdForName "cfg_pretty"
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
    let actual = formatMIR program
    expectFormatted "formatMIR" expected actual

let private emptyMIRFunction (name: string) : MIR.Function =
    let entry = Label $"{name}_entry"
    {
        Id = TestIds.functionIdForName name
        Name = name
        TypedParams = []
        ReturnType = AST.TUnit
        CFG = {
            Entry = entry
            Blocks =
                Map.ofList [
                    entry,
                    {
                        Label = entry
                        Instrs = []
                        Terminator = Ret (Int64Const 0L)
                    }
                ]
        }
        FloatRegs = Set.empty
    }

let testFormatMIRDumpFiltersBeforeFormatting () : TestResult =
    let program =
        MIR.Program (
            [emptyMIRFunction "Darklang.Stdlib.List.map"; emptyMIRFunction "Darklang.Stdlib.List.filter"],
            Map.empty,
            Map.empty
        )
    let actual = formatMIRDump (Some "MAP") false program
    if actual.Contains "Darklang.Stdlib.List.map" && not (actual.Contains "Darklang.Stdlib.List.filter") then
        Ok ()
    else
        Error $"Expected case-insensitive function-scoped MIR output, got:\n{actual}"

let testFormatMIRDumpSummary () : TestResult =
    let program =
        MIR.Program (
            [emptyMIRFunction "Darklang.Stdlib.List.map"; emptyMIRFunction "Darklang.Stdlib.List.filter"],
            Map.empty,
            Map.empty
        )
    let actual = formatMIRDump (Some "map") true program
    let expected = "Functions: 1\nDarklang.Stdlib.List.map: 1 blocks, 0 instructions"
    expectFormatted "formatMIRDump summary" expected actual

let testFormatMIRDumpReportsNoMatches () : TestResult =
    let program = MIR.Program ([emptyMIRFunction "Darklang.Stdlib.List.map"], Map.empty, Map.empty)
    let actual = formatMIRDump (Some "missing") false program
    expectFormatted "formatMIRDump no matches" "No functions matched 'missing'." actual

let tests = [
    ("format MIR", testFormatMIR)
    ("filter MIR dump functions before formatting", testFormatMIRDumpFiltersBeforeFormatting)
    ("summarize scoped MIR dumps", testFormatMIRDumpSummary)
    ("report empty MIR dump scopes", testFormatMIRDumpReportsNoMatches)
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
