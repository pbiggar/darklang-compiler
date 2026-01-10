// PassTestRunner.fs - Test runner for compiler pass tests
//
// Loads pass test files (e.g., MIR→LIR tests), runs the compiler pass,
// and compares the output with expected results.
//
// Pass tests are active; MIR pretty-printing reflects CFG structure for diagnostics.

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
open IRPrinter

/// Result of running a pass test
type PassTestResult = {
    Success: bool
    Message: string
    Expected: string option
    Actual: string option
}

/// Pretty-print MIR program with shared formatter
let prettyPrintMIR (program: MIR.Program) : string =
    formatMIR program

/// Pretty-print LIR program with shared formatter
let prettyPrintLIR (program: LIR.Program) : string =
    formatLIR program

/// Pretty-print ANF program with shared formatter
let prettyPrintANF (program: ANF.Program) : string =
    formatANF program

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
    match MIR_to_LIR.toLIR input with
    | Error err ->
        { Success = false
          Message = $"LIR conversion error: {err}"
          Expected = Some (prettyPrintLIR expected)
          Actual = None }
    | Ok symbolic ->
        match LIRSymbolic.toLIR symbolic with
        | Error err ->
            { Success = false
              Message = $"LIR pool resolution error: {err}"
              Expected = Some (prettyPrintLIR expected)
              Actual = None }
        | Ok actual ->
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
    // Pass empty TypeMap and TypeReg since payload sizes are stored in instructions
    // Use TInt64 as default for pass tests (E2E tests use actual program type)
    let emptyTypeMap : ANF.TypeMap = Map.empty
    let emptyTypeReg : Map<string, (string * AST.Type) list> = Map.empty
    match ANF_to_MIR.toMIR input emptyTypeMap emptyTypeReg AST.TInt64 Map.empty Map.empty false Map.empty with
    | Error err ->
        { Success = false
          Message = $"MIR conversion error: {err}"
          Expected = Some (prettyPrintMIR expected)
          Actual = None }
    | Ok actual ->
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
    | ARM64.X16 -> "X16" | ARM64.X17 -> "X17"
    | ARM64.X19 -> "X19" | ARM64.X20 -> "X20" | ARM64.X21 -> "X21" | ARM64.X22 -> "X22"
    | ARM64.X23 -> "X23" | ARM64.X24 -> "X24" | ARM64.X25 -> "X25" | ARM64.X26 -> "X26"
    | ARM64.X27 -> "X27" | ARM64.X28 -> "X28"
    | ARM64.X29 -> "X29" | ARM64.X30 -> "X30" | ARM64.SP -> "SP"

/// Pretty-print ARM64 instruction
let prettyPrintARM64Instr = function
    | ARM64.MOVZ (dest, imm, shift) ->
        $"MOVZ({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.MOVN (dest, imm, shift) ->
        $"MOVN({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.MOVK (dest, imm, shift) ->
        $"MOVK({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64.ADD_imm (dest, src, imm) ->
        $"ADD_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.ADD_reg (dest, src1, src2) ->
        $"ADD_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SUB_imm (dest, src, imm) ->
        $"SUB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.SUB_imm12 (dest, src, imm) ->
        $"SUB_imm12({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.SUB_reg (dest, src1, src2) ->
        $"SUB_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SUBS_imm (dest, src, imm) ->
        $"SUBS_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64.MUL (dest, src1, src2) ->
        $"MUL({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.SDIV (dest, src1, src2) ->
        $"SDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.UDIV (dest, src1, src2) ->
        $"UDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.MSUB (dest, src1, src2, src3) ->
        $"MSUB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, {prettyPrintARM64Reg src3})"
    | ARM64.MADD (dest, src1, src2, src3) ->
        $"MADD({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, {prettyPrintARM64Reg src3})"
    | ARM64.MOV_reg (dest, src) ->
        $"MOV_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.STRB (src, addr, offset) ->
        $"STRB({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDRB (dest, baseAddr, index) ->
        $"LDRB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg baseAddr}, {prettyPrintARM64Reg index})"
    | ARM64.LDRB_imm (dest, baseAddr, offset) ->
        $"LDRB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg baseAddr}, {offset})"
    | ARM64.STRB_reg (src, addr) ->
        $"STRB_reg({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr})"
    | ARM64.CMP_imm (src, imm) ->
        $"CMP_imm({prettyPrintARM64Reg src}, {imm})"
    | ARM64.CMP_reg (src1, src2) ->
        $"CMP_reg({prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.CSET (dest, cond) ->
        $"CSET({prettyPrintARM64Reg dest}, {cond})"
    | ARM64.AND_reg (dest, src1, src2) ->
        $"AND_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.AND_imm (dest, src, imm) ->
        $"AND_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, #{imm})"
    | ARM64.ORR_reg (dest, src1, src2) ->
        $"ORR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.EOR_reg (dest, src1, src2) ->
        $"EOR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64.LSL_reg (dest, src, shift) ->
        $"LSL_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintARM64Reg shift})"
    | ARM64.LSR_reg (dest, src, shift) ->
        $"LSR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintARM64Reg shift})"
    | ARM64.LSL_imm (dest, src, shift) ->
        $"LSL_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, #{shift})"
    | ARM64.LSR_imm (dest, src, shift) ->
        $"LSR_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, #{shift})"
    | ARM64.MVN (dest, src) ->
        $"MVN({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.SXTB (dest, src) ->
        $"SXTB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.SXTH (dest, src) ->
        $"SXTH({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.SXTW (dest, src) ->
        $"SXTW({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.UXTB (dest, src) ->
        $"UXTB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.UXTH (dest, src) ->
        $"UXTH({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.UXTW (dest, src) ->
        $"UXTW({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.CBZ (reg, label) ->
        $"CBZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64.CBZ_offset (reg, offset) ->
        $"CBZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64.CBNZ (reg, label) ->
        $"CBNZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64.CBNZ_offset (reg, offset) ->
        $"CBNZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64.TBZ (reg, bit, offset) ->
        $"TBZ({prettyPrintARM64Reg reg}, {bit}, {offset})"
    | ARM64.TBNZ (reg, bit, offset) ->
        $"TBNZ({prettyPrintARM64Reg reg}, {bit}, {offset})"
    | ARM64.TBZ_label (reg, bit, label) ->
        $"TBZ_label({prettyPrintARM64Reg reg}, {bit}, {label})"
    | ARM64.TBNZ_label (reg, bit, label) ->
        $"TBNZ_label({prettyPrintARM64Reg reg}, {bit}, {label})"
    | ARM64.B offset ->
        $"B({offset})"
    | ARM64.B_cond (cond, offset) ->
        $"B_cond({cond}, {offset})"
    | ARM64.B_label label ->
        $"B_label({label})"
    | ARM64.B_cond_label (cond, label) ->
        $"B_cond_label({cond}, {label})"
    | ARM64.NEG (dest, src) ->
        $"NEG({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64.STP (reg1, reg2, addr, offset) ->
        $"STP({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STP_pre (reg1, reg2, addr, offset) ->
        $"STP_pre({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDP (reg1, reg2, addr, offset) ->
        $"LDP({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDP_post (reg1, reg2, addr, offset) ->
        $"LDP_post({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STR (src, addr, offset) ->
        $"STR({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDR (dest, addr, offset) ->
        $"LDR({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STUR (src, addr, offset) ->
        $"STUR({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDUR (dest, addr, offset) ->
        $"LDUR({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.BL label ->
        $"BL({label})"
    | ARM64.BLR reg ->
        $"BLR({prettyPrintARM64Reg reg})"
    | ARM64.RET -> "RET"
    | ARM64.SVC imm -> $"SVC({imm})"
    | ARM64.Label label -> $"Label({label})"
    | ARM64.ADRP (dest, label) ->
        $"ADRP({prettyPrintARM64Reg dest}, {label})"
    | ARM64.ADD_label (dest, src, label) ->
        $"ADD_label({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {label})"
    | ARM64.ADR (dest, label) ->
        $"ADR({prettyPrintARM64Reg dest}, {label})"
    // Floating-point instructions
    | ARM64.LDR_fp (dest, addr, offset) ->
        $"LDR_fp({dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STR_fp (src, addr, offset) ->
        $"STR_fp({src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.STP_fp (freg1, freg2, addr, offset) ->
        $"STP_fp({freg1}, {freg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.LDP_fp (freg1, freg2, addr, offset) ->
        $"LDP_fp({freg1}, {freg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64.FADD (dest, src1, src2) ->
        $"FADD({dest}, {src1}, {src2})"
    | ARM64.FSUB (dest, src1, src2) ->
        $"FSUB({dest}, {src1}, {src2})"
    | ARM64.FMUL (dest, src1, src2) ->
        $"FMUL({dest}, {src1}, {src2})"
    | ARM64.FDIV (dest, src1, src2) ->
        $"FDIV({dest}, {src1}, {src2})"
    | ARM64.FNEG (dest, src) ->
        $"FNEG({dest}, {src})"
    | ARM64.FABS (dest, src) ->
        $"FABS({dest}, {src})"
    | ARM64.FCMP (src1, src2) ->
        $"FCMP({src1}, {src2})"
    | ARM64.FMOV_reg (dest, src) ->
        $"FMOV_reg({dest}, {src})"
    | ARM64.FMOV_to_gp (dest, src) ->
        $"FMOV_to_gp({prettyPrintARM64Reg dest}, {src})"
    | ARM64.FMOV_from_gp (dest, src) ->
        $"FMOV_from_gp({dest}, {prettyPrintARM64Reg src})"
    | ARM64.FSQRT (dest, src) ->
        $"FSQRT({dest}, {src})"
    | ARM64.SCVTF (dest, src) ->
        $"SCVTF({dest}, {prettyPrintARM64Reg src})"
    | ARM64.FCVTZS (dest, src) ->
        $"FCVTZS({prettyPrintARM64Reg dest}, {src})"
    | ARM64.BR reg ->
        $"BR({prettyPrintARM64Reg reg})"

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
