// IRPrinterTests.fs - Unit tests for shared IR formatting
//
// Ensures IRPrinter outputs match pinned formatting for MIR/LIR programs.

module IRPrinterTests

open MIR
open IRPrinter

type TestResult = Result<unit, string>

let testFormatMIR () : TestResult =
    let entry = Label "entry"
    let exit = Label "exit"
    let entryBlock: MIR.BasicBlock = {
        Label = entry
        Instrs = [ Mov (VReg 0, IntConst 1L, Some AST.TInt64) ]
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
    let actual = formatMIR program
    if actual = expected then
        Ok ()
    else
        Error $"formatMIR did not match.\nExpected:\n{expected}\nActual:\n{actual}"

let testFormatLIR () : TestResult =
    let entry = LIR.Label "entry"
    let block: LIR.BasicBlock = {
        Label = entry
        Instrs = [
            LIR.Mov (LIR.Virtual 0, LIR.Imm 1L)
            LIR.Add (LIR.Virtual 1, LIR.Virtual 0, LIR.Imm 2L)
        ]
        Terminator = LIR.Ret
    }
    let cfg: LIR.CFG = {
        Entry = entry
        Blocks = Map.ofList [ (entry, block) ]
    }
    let func: LIR.Function = {
        Name = "lir_print"
        TypedParams = []
        CFG = cfg
        StackSize = 0
        UsedCalleeSaved = []
    }
    let program = LIR.Program ([func], MIR.emptyStringPool, MIR.emptyFloatPool)
    let expected =
        [
            "lir_print:"
            "  Label \"entry\":"
            "    v0 <- Mov(Imm 1)"
            "    v1 <- Add(v0, Imm 2)"
            "    Ret"
        ]
        |> String.concat "\n"
    let actual = formatLIR program
    if actual = expected then
        Ok ()
    else
        Error $"formatLIR did not match.\nExpected:\n{expected}\nActual:\n{actual}"

let tests = [
    ("format MIR", testFormatMIR)
    ("format LIR", testFormatLIR)
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
