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
open TestDSL.ARM64SymbolicParser
open ANF
open MIR
open LIR
open ARM64Symbolic
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

/// Rename all functions in an LIR program
let renameLIRFunctions (name: string) (program: LIR.Program) : LIR.Program =
    let (LIR.Program (functions, variants, records)) = program
    let renamed = functions |> List.map (fun func -> { func with Name = name })
    LIR.Program (renamed, variants, records)

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
                    match parseMIRWithEntryLabel "_start_body" outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-MIR: {e}"
                    | Ok mirProgram -> Ok (anfProgram, mirProgram)

/// Run ANF→MIR test
let runANF2MIRTest (input: ANF.Program) (expected: MIR.Program) : PassTestResult =
    // Pass-test ANF DSL is int-only, so map all TempIds to TInt64.
    let maxId = maxTempIdInProgram input
    let typeMap : ANF.TypeMap =
        if maxId < 0 then
            Map.empty
        else
            [0 .. maxId]
            |> List.map (fun id -> (ANF.TempId id, AST.TInt64))
            |> Map.ofList
    let emptyTypeReg : Map<string, (string * AST.Type) list> = Map.empty
    match ANF_to_MIR.toMIR input typeMap emptyTypeReg AST.TInt64 Map.empty Map.empty false Map.empty with
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

/// Pretty-print label references (code/data)
let private prettyPrintLabelRef (labelRef: ARM64Symbolic.LabelRef) : string =
    let escapeLabel (value: string) =
        value.Replace("\\", "\\\\").Replace("\"", "\\\"")
    match labelRef with
    | ARM64Symbolic.CodeLabel name -> name
    | ARM64Symbolic.DataLabel (ARM64Symbolic.Named name) -> $"data:{name}"
    | ARM64Symbolic.DataLabel (ARM64Symbolic.StringLiteral value) ->
        $"str:\"{escapeLabel value}\""
    | ARM64Symbolic.DataLabel (ARM64Symbolic.FloatLiteral value) ->
        $"float:{value}"

/// Pretty-print ARM64 instruction
let prettyPrintARM64Instr = function
    | ARM64Symbolic.MOVZ (dest, imm, shift) ->
        $"MOVZ({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64Symbolic.MOVN (dest, imm, shift) ->
        $"MOVN({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64Symbolic.MOVK (dest, imm, shift) ->
        $"MOVK({prettyPrintARM64Reg dest}, {imm}, {shift})"
    | ARM64Symbolic.ADD_imm (dest, src, imm) ->
        $"ADD_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64Symbolic.ADD_reg (dest, src1, src2) ->
        $"ADD_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.ADD_shifted (dest, src1, src2, shift) ->
        $"ADD_shifted({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, LSL #{shift})"
    | ARM64Symbolic.SUB_imm (dest, src, imm) ->
        $"SUB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64Symbolic.SUB_imm12 (dest, src, imm) ->
        $"SUB_imm12({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64Symbolic.SUB_reg (dest, src1, src2) ->
        $"SUB_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.SUB_shifted (dest, src1, src2, shift) ->
        $"SUB_shifted({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, LSL #{shift})"
    | ARM64Symbolic.SUBS_imm (dest, src, imm) ->
        $"SUBS_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {imm})"
    | ARM64Symbolic.MUL (dest, src1, src2) ->
        $"MUL({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.SDIV (dest, src1, src2) ->
        $"SDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.UDIV (dest, src1, src2) ->
        $"UDIV({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.MSUB (dest, src1, src2, src3) ->
        $"MSUB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, {prettyPrintARM64Reg src3})"
    | ARM64Symbolic.MADD (dest, src1, src2, src3) ->
        $"MADD({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2}, {prettyPrintARM64Reg src3})"
    | ARM64Symbolic.MOV_reg (dest, src) ->
        $"MOV_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.STRB (src, addr, offset) ->
        $"STRB({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.LDRB (dest, baseAddr, index) ->
        $"LDRB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg baseAddr}, {prettyPrintARM64Reg index})"
    | ARM64Symbolic.LDRB_imm (dest, baseAddr, offset) ->
        $"LDRB_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg baseAddr}, {offset})"
    | ARM64Symbolic.STRB_reg (src, addr) ->
        $"STRB_reg({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr})"
    | ARM64Symbolic.CMP_imm (src, imm) ->
        $"CMP_imm({prettyPrintARM64Reg src}, {imm})"
    | ARM64Symbolic.CMP_reg (src1, src2) ->
        $"CMP_reg({prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.CSET (dest, cond) ->
        $"CSET({prettyPrintARM64Reg dest}, {cond})"
    | ARM64Symbolic.AND_reg (dest, src1, src2) ->
        $"AND_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.BIC_reg (dest, src1, src2) ->
        $"BIC_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.AND_imm (dest, src, imm) ->
        $"AND_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, #{imm})"
    | ARM64Symbolic.ORR_reg (dest, src1, src2) ->
        $"ORR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.EOR_reg (dest, src1, src2) ->
        $"EOR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src1}, {prettyPrintARM64Reg src2})"
    | ARM64Symbolic.LSL_reg (dest, src, shift) ->
        $"LSL_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintARM64Reg shift})"
    | ARM64Symbolic.LSR_reg (dest, src, shift) ->
        $"LSR_reg({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintARM64Reg shift})"
    | ARM64Symbolic.LSL_imm (dest, src, shift) ->
        $"LSL_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, #{shift})"
    | ARM64Symbolic.LSR_imm (dest, src, shift) ->
        $"LSR_imm({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, #{shift})"
    | ARM64Symbolic.MVN (dest, src) ->
        $"MVN({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.SXTB (dest, src) ->
        $"SXTB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.SXTH (dest, src) ->
        $"SXTH({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.SXTW (dest, src) ->
        $"SXTW({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.UXTB (dest, src) ->
        $"UXTB({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.UXTH (dest, src) ->
        $"UXTH({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.UXTW (dest, src) ->
        $"UXTW({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.CBZ (reg, label) ->
        $"CBZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64Symbolic.CBZ_offset (reg, offset) ->
        $"CBZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64Symbolic.CBNZ (reg, label) ->
        $"CBNZ({prettyPrintARM64Reg reg}, {label})"
    | ARM64Symbolic.CBNZ_offset (reg, offset) ->
        $"CBNZ_offset({prettyPrintARM64Reg reg}, {offset})"
    | ARM64Symbolic.TBZ (reg, bit, offset) ->
        $"TBZ({prettyPrintARM64Reg reg}, {bit}, {offset})"
    | ARM64Symbolic.TBNZ (reg, bit, offset) ->
        $"TBNZ({prettyPrintARM64Reg reg}, {bit}, {offset})"
    | ARM64Symbolic.TBZ_label (reg, bit, label) ->
        $"TBZ_label({prettyPrintARM64Reg reg}, {bit}, {label})"
    | ARM64Symbolic.TBNZ_label (reg, bit, label) ->
        $"TBNZ_label({prettyPrintARM64Reg reg}, {bit}, {label})"
    | ARM64Symbolic.B offset ->
        $"B({offset})"
    | ARM64Symbolic.B_cond (cond, offset) ->
        $"B_cond({cond}, {offset})"
    | ARM64Symbolic.B_label label ->
        $"B_label({label})"
    | ARM64Symbolic.B_cond_label (cond, label) ->
        $"B_cond_label({cond}, {label})"
    | ARM64Symbolic.NEG (dest, src) ->
        $"NEG({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.STP (reg1, reg2, addr, offset) ->
        $"STP({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.STP_pre (reg1, reg2, addr, offset) ->
        $"STP_pre({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.LDP (reg1, reg2, addr, offset) ->
        $"LDP({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.LDP_post (reg1, reg2, addr, offset) ->
        $"LDP_post({prettyPrintARM64Reg reg1}, {prettyPrintARM64Reg reg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.STR (src, addr, offset) ->
        $"STR({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.LDR (dest, addr, offset) ->
        $"LDR({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.STUR (src, addr, offset) ->
        $"STUR({prettyPrintARM64Reg src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.LDUR (dest, addr, offset) ->
        $"LDUR({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.BL label ->
        $"BL({label})"
    | ARM64Symbolic.BLR reg ->
        $"BLR({prettyPrintARM64Reg reg})"
    | ARM64Symbolic.RET -> "RET"
    | ARM64Symbolic.SVC imm -> $"SVC({imm})"
    | ARM64Symbolic.Label label -> $"Label({label})"
    | ARM64Symbolic.ADRP (dest, label) ->
        $"ADRP({prettyPrintARM64Reg dest}, {prettyPrintLabelRef label})"
    | ARM64Symbolic.ADD_label (dest, src, label) ->
        $"ADD_label({prettyPrintARM64Reg dest}, {prettyPrintARM64Reg src}, {prettyPrintLabelRef label})"
    | ARM64Symbolic.ADR (dest, label) ->
        $"ADR({prettyPrintARM64Reg dest}, {prettyPrintLabelRef label})"
    // Floating-point instructions
    | ARM64Symbolic.LDR_fp (dest, addr, offset) ->
        $"LDR_fp({dest}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.STR_fp (src, addr, offset) ->
        $"STR_fp({src}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.STP_fp (freg1, freg2, addr, offset) ->
        $"STP_fp({freg1}, {freg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.LDP_fp (freg1, freg2, addr, offset) ->
        $"LDP_fp({freg1}, {freg2}, {prettyPrintARM64Reg addr}, {offset})"
    | ARM64Symbolic.FADD (dest, src1, src2) ->
        $"FADD({dest}, {src1}, {src2})"
    | ARM64Symbolic.FSUB (dest, src1, src2) ->
        $"FSUB({dest}, {src1}, {src2})"
    | ARM64Symbolic.FMUL (dest, src1, src2) ->
        $"FMUL({dest}, {src1}, {src2})"
    | ARM64Symbolic.FDIV (dest, src1, src2) ->
        $"FDIV({dest}, {src1}, {src2})"
    | ARM64Symbolic.FNEG (dest, src) ->
        $"FNEG({dest}, {src})"
    | ARM64Symbolic.FABS (dest, src) ->
        $"FABS({dest}, {src})"
    | ARM64Symbolic.FCMP (src1, src2) ->
        $"FCMP({src1}, {src2})"
    | ARM64Symbolic.FMOV_reg (dest, src) ->
        $"FMOV_reg({dest}, {src})"
    | ARM64Symbolic.FMOV_imm (dest, value) ->
        $"FMOV_imm({dest}, {value})"
    | ARM64Symbolic.FMOV_to_gp (dest, src) ->
        $"FMOV_to_gp({prettyPrintARM64Reg dest}, {src})"
    | ARM64Symbolic.FMOV_from_gp (dest, src) ->
        $"FMOV_from_gp({dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.FSQRT (dest, src) ->
        $"FSQRT({dest}, {src})"
    | ARM64Symbolic.SCVTF (dest, src) ->
        $"SCVTF({dest}, {prettyPrintARM64Reg src})"
    | ARM64Symbolic.FCVTZS (dest, src) ->
        $"FCVTZS({prettyPrintARM64Reg dest}, {src})"
    | ARM64Symbolic.BR reg ->
        $"BR({prettyPrintARM64Reg reg})"

/// Pretty-print ARM64 program (filtering out Label pseudo-instructions)
let prettyPrintARM64 (instrs: ARM64Symbolic.Instr list) : string =
    instrs
    |> List.filter (function | ARM64Symbolic.Label _ -> false | _ -> true)
    |> List.map prettyPrintARM64Instr
    |> String.concat "\n"

/// Load LIR→ARM64 test from file
let loadLIR2ARM64Test (path: string) : Result<LIR.Program * ARM64Symbolic.Instr list, string> =
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
                    match parseARM64Symbolic outputText with
                    | Error e -> Error $"Failed to parse OUTPUT-ARM64: {e}"
                    | Ok arm64Instrs ->
                        let renamedProgram = renameLIRFunctions "test" lirProgram
                        Ok (renamedProgram, arm64Instrs)

/// Run LIR→ARM64 test
let runLIR2ARM64Test (input: LIR.Program) (expected: ARM64Symbolic.Instr list) : PassTestResult =
    let target = ARM64.targetConfigFor Platform.LinuxARM64
    match CodeGen.generateARM64 target input with
    | Error err ->
        { Success = false
          Message = $"Code generation failed: {err}"
          Expected = Some (prettyPrintARM64 expected)
          Actual = None }
    | Ok actualRaw ->
        // Filter out Label pseudo-instructions for comparison
        let actual = actualRaw |> List.filter (function | ARM64Symbolic.Label _ -> false | _ -> true)
        let expectedForComparison = expected |> List.filter (function | ARM64Symbolic.Label _ -> false | _ -> true)
        if actual = expectedForComparison then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            { Success = false
              Message = "Output mismatch"
              Expected = Some (prettyPrintARM64 expected)
              Actual = Some (prettyPrintARM64 actualRaw) }
