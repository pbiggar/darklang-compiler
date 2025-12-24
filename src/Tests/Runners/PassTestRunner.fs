// PassTestRunner.fs - Test runner for compiler pass tests
//
// Loads pass test files (e.g., MIR→LIR tests), runs the compiler pass,
// and compares the output with expected results.
//
// NOTE: Temporarily disabled - MIR/LIR structure changed to CFG, needs rewrite
// Pass tests will be re-enabled after CFG-based test infrastructure is built

module TestDSL.PassTestRunner

open System.IO
open TestDSL.Common
open TestDSL.ANFParser
open TestDSL.MIRParser
open TestDSL.LIRParser
open TestDSL.ARM64Parser
open ANF
open MIR
open LIR
open ARM64
open ANF_to_MIR
open MIR_to_LIR
open CodeGen

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
    | MIR.BoolConst b -> if b then "true" else "false"
    | MIR.Register (MIR.VReg n) -> $"v{n}"

/// Pretty-print MIR operator
let prettyPrintMIROp = function
    | MIR.Add -> "+"
    | MIR.Sub -> "-"
    | MIR.Mul -> "*"
    | MIR.Div -> "/"
    | MIR.Eq -> "=="
    | MIR.Neq -> "!="
    | MIR.Lt -> "<"
    | MIR.Gt -> ">"
    | MIR.Lte -> "<="
    | MIR.Gte -> ">="
    | MIR.And -> "&&"
    | MIR.Or -> "||"

/// Pretty-print MIR instruction
/// NOTE: Disabled - MIR structure changed to CFG
let prettyPrintMIRInstr (instr: MIR.Instr) : string =
    "<MIR pretty-print disabled - CFG structure>"

/// Pretty-print MIR program
/// NOTE: Disabled - MIR structure changed to CFG
let prettyPrintMIR (program: MIR.Program) : string =
    "<MIR pretty-print disabled - CFG structure>"

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
let prettyPrintLIRInstr (instr: LIR.Instr) : string =
    match instr with
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
    | LIR.Cmp (left, right) ->
        $"Cmp({prettyPrintLIRReg left}, {prettyPrintLIROperand right})"
    | LIR.Cset (dest, cond) ->
        $"{prettyPrintLIRReg dest} <- Cset({cond})"
    | LIR.And (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- And({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Orr (dest, left, right) ->
        $"{prettyPrintLIRReg dest} <- Orr({prettyPrintLIRReg left}, {prettyPrintLIRReg right})"
    | LIR.Mvn (dest, src) ->
        $"{prettyPrintLIRReg dest} <- Mvn({prettyPrintLIRReg src})"
    | LIR.PrintInt reg ->
        $"PrintInt({prettyPrintLIRReg reg})"
    | LIR.PrintBool reg ->
        $"PrintBool({prettyPrintLIRReg reg})"

/// Pretty-print LIR terminator
let prettyPrintLIRTerminator (term: LIR.Terminator) : string =
    match term with
    | LIR.Ret -> "Ret"
    | LIR.Branch (cond, trueLabel, falseLabel) ->
        $"Branch({prettyPrintLIRReg cond}, {trueLabel}, {falseLabel})"
    | LIR.Jump label -> $"Jump({label})"

/// Pretty-print LIR program (flat format for single-block CFGs)
let prettyPrintLIR (program: LIR.Program) : string =
    let (LIR.Program funcs) = program
    // For simple test cases, we expect a single function with single block
    match funcs with
    | [func] ->
        let entry = func.CFG.Entry
        match Map.tryFind entry func.CFG.Blocks with
        | Some block ->
            let instrLines = block.Instrs |> List.map prettyPrintLIRInstr
            let termLine = prettyPrintLIRTerminator block.Terminator
            String.concat "\n" (instrLines @ [termLine])
        | None -> "<entry block not found>"
    | _ -> "<multiple functions not supported in pretty-print>"

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

/// Pretty-print ANF atom
let prettyPrintANFAtom = function
    | ANF.IntLiteral n -> string n
    | ANF.BoolLiteral b -> if b then "true" else "false"
    | ANF.Var (ANF.TempId n) -> $"t{n}"

/// Pretty-print ANF binary operator
let prettyPrintANFOp = function
    | ANF.Add -> "+"
    | ANF.Sub -> "-"
    | ANF.Mul -> "*"
    | ANF.Div -> "/"
    | ANF.Eq -> "=="
    | ANF.Neq -> "!="
    | ANF.Lt -> "<"
    | ANF.Gt -> ">"
    | ANF.Lte -> "<="
    | ANF.Gte -> ">="
    | ANF.And -> "&&"
    | ANF.Or -> "||"

/// Pretty-print ANF unary operator
let prettyPrintANFUnaryOp = function
    | ANF.Neg -> "-"
    | ANF.Not -> "!"

/// Pretty-print ANF complex expression
let prettyPrintANFCExpr = function
    | ANF.Atom atom -> prettyPrintANFAtom atom
    | ANF.Prim (op, left, right) ->
        $"{prettyPrintANFAtom left} {prettyPrintANFOp op} {prettyPrintANFAtom right}"
    | ANF.UnaryPrim (op, operand) ->
        $"{prettyPrintANFUnaryOp op}{prettyPrintANFAtom operand}"
    | ANF.IfValue (cond, thenAtom, elseAtom) ->
        $"if {prettyPrintANFAtom cond} then {prettyPrintANFAtom thenAtom} else {prettyPrintANFAtom elseAtom}"

/// Pretty-print ANF expression (recursive)
let rec prettyPrintANFExpr = function
    | ANF.Return atom -> $"return {prettyPrintANFAtom atom}"
    | ANF.Let (ANF.TempId n, cexpr, body) ->
        let cexprStr = prettyPrintANFCExpr cexpr
        let bodyStr = prettyPrintANFExpr body
        $"let t{n} = {cexprStr}\n{bodyStr}"
    | ANF.If (cond, thenBranch, elseBranch) ->
        let condStr = prettyPrintANFAtom cond
        let thenStr = prettyPrintANFExpr thenBranch
        let elseStr = prettyPrintANFExpr elseBranch
        $"if {condStr} then\n  {thenStr}\nelse\n  {elseStr}"

/// Pretty-print ANF program
let prettyPrintANF (ANF.Program expr) : string =
    prettyPrintANFExpr expr

/// Load ANF→MIR test from file
let loadANF2MIRTest (path: string) : Result<ANF.Program * MIR.Program, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        let testFile = parseTestFile content

        match getRequiredSection "INPUT-ANF" testFile with
        | Error e -> Error e
        | Ok inputText ->
            match parseANF inputText with
            | Error e -> Error $"Failed to parse INPUT-ANF: {e}"
            | Ok anfProgram ->
                match getRequiredSection "OUTPUT-MIR" testFile with
                | Error e -> Error e
                | Ok outputText ->
                    match parseMIR outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-MIR: {e}"
                    | Ok mirProgram -> Ok (anfProgram, mirProgram)

/// Run ANF→MIR test
let runANF2MIRTest (input: ANF.Program) (expected: MIR.Program) : PassTestResult =
    let (actual, _) = ANF_to_MIR.toMIR input MIR.initialRegGen

    if actual = expected then
        { Success = true
          Message = "Test passed"
          Expected = None
          Actual = None }
    else
        { Success = false
          Message = "Output mismatch"
          Expected = Some (prettyPrintMIR expected)
          Actual = Some (prettyPrintMIR actual) }

/// Pretty-print ARM64 register
let prettyPrintARM64Reg = function
    | ARM64.X0 -> "X0" | ARM64.X1 -> "X1" | ARM64.X2 -> "X2" | ARM64.X3 -> "X3"
    | ARM64.X4 -> "X4" | ARM64.X5 -> "X5" | ARM64.X6 -> "X6" | ARM64.X7 -> "X7"
    | ARM64.X8 -> "X8" | ARM64.X9 -> "X9" | ARM64.X10 -> "X10" | ARM64.X11 -> "X11"
    | ARM64.X12 -> "X12" | ARM64.X13 -> "X13" | ARM64.X14 -> "X14" | ARM64.X15 -> "X15"
    | ARM64.X16 -> "X16" | ARM64.X29 -> "X29" | ARM64.X30 -> "X30" | ARM64.SP -> "SP"

/// Pretty-print ARM64 instruction
let prettyPrintARM64Instr = function
    | ARM64.MOVZ (dest, imm, shift) ->
        $"MOVZ({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.MOVK (dest, imm, shift) ->
        $"MOVK({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.ADD_imm (dest, src, imm) ->
        $"ADD_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.ADD_reg (dest, src1, src2) ->
        $"ADD_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SUB_imm (dest, src, imm) ->
        $"SUB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.SUB_reg (dest, src1, src2) ->
        $"SUB_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.MUL (dest, src1, src2) ->
        $"MUL({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SDIV (dest, src1, src2) ->
        $"SDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.UDIV (dest, src1, src2) ->
        $"UDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.MSUB (dest, src1, src2, src3) ->
        $"MSUB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, {prettyPrintARM64Reg src3})"
    | ARM64.MOV_reg (dest, src) ->
        $"MOV_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.STRB (src, addr, offset) ->
        $"STRB({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.CMP_imm (src, imm) ->
        $"CMP_imm({prettyPrintARM64Reg src}, {imm})"
    | ARM64.CMP_reg (src1, src2) ->
        $"CMP_reg({prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.CSET (dest, cond) ->
        $"CSET({prettyPrintARM64Reg dest}, {cond})"
    | ARM64.AND_reg (dest, src1, src2) ->
        $"AND_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.ORR_reg (dest, src1, src2) ->
        $"ORR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.MVN (dest, src) ->
        $"MVN({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.CBZ (reg, label) ->
        $"CBZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64.CBZ_offset (reg, offset) ->
        $"CBZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64.CBNZ (reg, label) ->
        $"CBNZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64.CBNZ_offset (reg, offset) ->
        $"CBNZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64.TBNZ (reg, bit, offset) ->
        $"TBNZ({prettyPrintARM64Reg reg}, {bit}, {offset})"
    | ARM64.B offset ->
        $"B({offset})"
    | ARM64.B_label label ->
        $"B_label({label})"
    | ARM64.NEG (dest, src) ->
        $"NEG({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.RET -> "RET"
    | ARM64.SVC imm -> $"SVC({imm})"
    | ARM64.Label label -> $"Label({label})"

/// Pretty-print ARM64 program (filtering out Label pseudo-instructions)
let prettyPrintARM64 (instrs: ARM64.Instr list) : string =
    instrs
    |> List.filter (function | ARM64.Label _ -> false | _ -> true)
    |> List.map prettyPrintARM64Instr
    |> String.concat "\n"

/// Load LIR→ARM64 test from file
let loadLIR2ARM64Test (path: string) : Result<LIR.Program * ARM64.Instr list, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        let testFile = parseTestFile content

        match getRequiredSection "INPUT-LIR" testFile with
        | Error e -> Error e
        | Ok inputText ->
            match parseLIR inputText with
            | Error e -> Error $"Failed to parse INPUT-LIR: {e}"
            | Ok lirProgram ->
                match getRequiredSection "OUTPUT-ARM64" testFile with
                | Error e -> Error e
                | Ok outputText ->
                    match parseARM64 outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-ARM64: {e}"
                    | Ok arm64Instrs -> Ok (lirProgram, arm64Instrs)

/// Run LIR→ARM64 test
let runLIR2ARM64Test (input: LIR.Program) (expected: ARM64.Instr list) : PassTestResult =
    match CodeGen.generateARM64 input with
    | Error err ->
        { Success = false
          Message = $"Code generation failed: {err}"
          Expected = Some (prettyPrintARM64 expected)
          Actual = None }
    | Ok actualRaw ->
        // Filter out Label pseudo-instructions for comparison
        let actual = actualRaw |> List.filter (function | ARM64.Label _ -> false | _ -> true)
        if actual = expected then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            { Success = false
              Message = "Output mismatch"
              Expected = Some (prettyPrintARM64 expected)
              Actual = Some (prettyPrintARM64 actualRaw) }
