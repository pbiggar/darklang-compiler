// PassTestRunner.fs - Test runner for compiler pass tests
//
// Loads pass test files (e.g., MIR→LIR tests), runs the compiler pass,
// and compares the output with expected results.

module TestDSL.PassTestRunner

open System.IO
open TestDSL.Common
open TestDSL.MIRParser
open TestDSL.LIRParser
open MIR
open LIR
open MIR_to_LIR

/// Result of running a pass test
type PassTestResult = {
    Success: bool
    Message: string
    Expected: string option
    Actual: string option
}

/// Pretty-print MIR operand
let prettyPrintMIROperand = function
    | MIR.IntConst n -> string n
    | MIR.Register (MIR.VReg n) -> $"v{n}"

/// Pretty-print MIR operator
let prettyPrintMIROp = function
    | MIR.Add -> "+"
    | MIR.Sub -> "-"
    | MIR.Mul -> "*"
    | MIR.Div -> "/"

/// Pretty-print MIR instruction
let prettyPrintMIRInstr = function
    | MIR.Mov (MIR.VReg dest, src) ->
        $"v{dest} <- {prettyPrintMIROperand src}"
    | MIR.BinOp (MIR.VReg dest, op, left, right) ->
        $"v{dest} <- {prettyPrintMIROperand left} {prettyPrintMIROp op} {prettyPrintMIROperand right}"
    | MIR.Ret operand ->
        $"ret {prettyPrintMIROperand operand}"

/// Pretty-print MIR program
let prettyPrintMIR (MIR.Program blocks) : string =
    blocks
    |> List.collect (fun (MIR.Block instrs) -> instrs)
    |> List.map prettyPrintMIRInstr
    |> String.concat "\n"

/// Pretty-print LIR physical register
let prettyPrintLIRPhysReg = function
    | LIR.X0 -> "X0" | LIR.X1 -> "X1" | LIR.X2 -> "X2" | LIR.X3 -> "X3"
    | LIR.X4 -> "X4" | LIR.X5 -> "X5" | LIR.X6 -> "X6" | LIR.X7 -> "X7"
    | LIR.X8 -> "X8" | LIR.X9 -> "X9" | LIR.X10 -> "X10" | LIR.X11 -> "X11"
    | LIR.X12 -> "X12" | LIR.X13 -> "X13" | LIR.X14 -> "X14" | LIR.X15 -> "X15"
    | LIR.X29 -> "X29" | LIR.X30 -> "X30" | LIR.SP -> "SP"

/// Pretty-print LIR register
let prettyPrintLIRReg = function
    | LIR.Physical pr -> prettyPrintLIRPhysReg pr
    | LIR.Virtual n -> $"v{n}"

/// Pretty-print LIR operand
let prettyPrintLIROperand = function
    | LIR.Imm n -> $"Imm {n}"
    | LIR.Operand.Reg reg -> $"Reg {prettyPrintLIRReg reg}"
    | LIR.StackSlot n -> $"Stack {n}"

/// Pretty-print LIR instruction
let prettyPrintLIRInstr = function
    | LIR.Mov (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mov({prettyPrintLIROperand src})"
    | LIR.Add (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Add({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Sub (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sub({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Mul (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Mul({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Sdiv (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Sdiv({prettyPrintLIRReg left}, Reg {prettyPrintLIRReg right})"
    | LIR.Ret -> "Ret"

/// Pretty-print LIR program
let prettyPrintLIR (LIR.Program funcs) : string =
    funcs
    |> List.collect (fun func -> func.Body)
    |> List.map prettyPrintLIRInstr
    |> String.concat "\n"

/// Load MIR→LIR test from file
let loadMIR2LIRTest (path: string) : Result<MIR.Program * LIR.Program, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        let testFile = parseTestFile content

        match getRequiredSection "INPUT-MIR" testFile with
        | Error e -> Error e
        | Ok inputText ->
            match parseMIR inputText with
            | Error e -> Error $"Failed to parse INPUT-MIR: {e}"
            | Ok mirProgram ->
                match getRequiredSection "OUTPUT-LIR" testFile with
                | Error e -> Error e
                | Ok outputText ->
                    match parseLIR outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-LIR: {e}"
                    | Ok lirProgram -> Ok (mirProgram, lirProgram)

/// Run MIR→LIR test
let runMIR2LIRTest (input: MIR.Program) (expected: LIR.Program) : PassTestResult =
    let actual = MIR_to_LIR.toLIR input

    if actual = expected then
        { Success = true
          Message = "Test passed"
          Expected = None
          Actual = None }
    else
        { Success = false
          Message = "Output mismatch"
          Expected = Some (prettyPrintLIR expected)
          Actual = Some (prettyPrintLIR actual) }
