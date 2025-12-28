// ARM64EncodingTestRunner.fs - Test runner for ARM64 encoding tests
//
// Loads ARM64 encoding test files (.arm64enc), runs the ARM64 encoder,
// and compares the output with expected machine code hex values.

module TestDSL.ARM64EncodingTestRunner

open System.IO
open TestDSL.Common
open TestDSL.PassTestRunner
open TestDSL.ARM64EncodingFormat
open ARM64
open ARM64_Encoding

/// Load ARM64 encoding test from file
let loadARM64EncodingTest (path: string) : Result<ARM64EncodingTest, string> =
    if not (File.Exists path) then
        Error $"Test file not found: {path}"
    else
        let content = File.ReadAllText(path)
        parseARM64EncodingTest content

/// Check if all values in a list are different
let hasAllDifferent (values: uint32 list) : bool =
    let unique = values |> List.distinct
    unique.Length = values.Length

/// Format encoding mismatches for display
let formatMismatches (mismatches: (int * Instr * uint32 * uint32) list) : string =
    if mismatches.IsEmpty then
        "All encodings matched"
    else
        let lines =
            mismatches
            |> List.map (fun (i, instr, expected, actual) ->
                let instrStr =
                    match instr with
                    | MOVZ (reg, imm, shift) -> $"MOVZ({reg}, {imm}, {shift})"
                    | MOVK (reg, imm, shift) -> $"MOVK({reg}, {imm}, {shift})"
                    | ADD_imm (dest, src, imm) -> $"ADD_imm({dest}, {src}, {imm})"
                    | ADD_reg (dest, src1, src2) -> $"ADD_reg({dest}, {src1}, {src2})"
                    | SUB_imm (dest, src, imm) -> $"SUB_imm({dest}, {src}, {imm})"
                    | SUB_reg (dest, src1, src2) -> $"SUB_reg({dest}, {src1}, {src2})"
                    | MUL (dest, src1, src2) -> $"MUL({dest}, {src1}, {src2})"
                    | SDIV (dest, src1, src2) -> $"SDIV({dest}, {src1}, {src2})"
                    | UDIV (dest, src1, src2) -> $"UDIV({dest}, {src1}, {src2})"
                    | MSUB (dest, src1, src2, src3) -> $"MSUB({dest}, {src1}, {src2}, {src3})"
                    | CMP_imm (src, imm) -> $"CMP_imm({src}, {imm})"
                    | CMP_reg (src1, src2) -> $"CMP_reg({src1}, {src2})"
                    | CSET (dest, cond) -> $"CSET({dest}, {cond})"
                    | AND_reg (dest, src1, src2) -> $"AND_reg({dest}, {src1}, {src2})"
                    | ORR_reg (dest, src1, src2) -> $"ORR_reg({dest}, {src1}, {src2})"
                    | EOR_reg (dest, src1, src2) -> $"EOR_reg({dest}, {src1}, {src2})"
                    | LSL_reg (dest, src, shift) -> $"LSL_reg({dest}, {src}, {shift})"
                    | LSR_reg (dest, src, shift) -> $"LSR_reg({dest}, {src}, {shift})"
                    | MVN (dest, src) -> $"MVN({dest}, {src})"
                    | MOV_reg (dest, src) -> $"MOV_reg({dest}, {src})"
                    | STRB (src, addr, offset) -> $"STRB({src}, {addr}, {offset})"
                    | LDRB (dest, baseAddr, index) -> $"LDRB({dest}, {baseAddr}, {index})"
                    | LDRB_imm (dest, baseAddr, offset) -> $"LDRB_imm({dest}, {baseAddr}, {offset})"
                    | STRB_reg (src, addr) -> $"STRB_reg({src}, {addr})"
                    | CBZ (reg, label) -> $"CBZ({reg}, {label})"
                    | CBZ_offset (reg, offset) -> $"CBZ_offset({reg}, {offset})"
                    | CBNZ (reg, label) -> $"CBNZ({reg}, {label})"
                    | CBNZ_offset (reg, offset) -> $"CBNZ_offset({reg}, {offset})"
                    | TBNZ (reg, bit, offset) -> $"TBNZ({reg}, {bit}, {offset})"
                    | B offset -> $"B({offset})"
                    | B_cond (cond, offset) -> $"B_cond({cond}, {offset})"
                    | B_label label -> $"B_label({label})"
                    | NEG (dest, src) -> $"NEG({dest}, {src})"
                    | STP (reg1, reg2, addr, offset) -> $"STP({reg1}, {reg2}, {addr}, {offset})"
                    | LDP (reg1, reg2, addr, offset) -> $"LDP({reg1}, {reg2}, {addr}, {offset})"
                    | STR (src, addr, offset) -> $"STR({src}, {addr}, {offset})"
                    | LDR (dest, addr, offset) -> $"LDR({dest}, {addr}, {offset})"
                    | STUR (src, addr, offset) -> $"STUR({src}, {addr}, {offset})"
                    | LDUR (dest, addr, offset) -> $"LDUR({dest}, {addr}, {offset})"
                    | BL label -> $"BL({label})"
                    | BLR reg -> $"BLR({reg})"
                    | RET -> "RET"
                    | SVC imm -> $"SVC({imm})"
                    | Label label -> $"Label({label})"
                    | ADRP (dest, label) -> $"ADRP({dest}, {label})"
                    | ADD_label (dest, src, label) -> $"ADD_label({dest}, {src}, {label})"
                    | ADR (dest, label) -> $"ADR({dest}, {label})"
                    // Floating-point instructions
                    | LDR_fp (dest, addr, offset) -> $"LDR_fp({dest}, {addr}, {offset})"
                    | STR_fp (src, addr, offset) -> $"STR_fp({src}, {addr}, {offset})"
                    | FADD (dest, src1, src2) -> $"FADD({dest}, {src1}, {src2})"
                    | FSUB (dest, src1, src2) -> $"FSUB({dest}, {src1}, {src2})"
                    | FMUL (dest, src1, src2) -> $"FMUL({dest}, {src1}, {src2})"
                    | FDIV (dest, src1, src2) -> $"FDIV({dest}, {src1}, {src2})"
                    | FNEG (dest, src) -> $"FNEG({dest}, {src})"
                    | FABS (dest, src) -> $"FABS({dest}, {src})"
                    | FCMP (src1, src2) -> $"FCMP({src1}, {src2})"
                    | FMOV_reg (dest, src) -> $"FMOV_reg({dest}, {src})"
                    | FMOV_to_gp (dest, src) -> $"FMOV_to_gp({dest}, {src})"
                    | FSQRT (dest, src) -> $"FSQRT({dest}, {src})"
                    | SCVTF (dest, src) -> $"SCVTF({dest}, {src})"
                    | FCVTZS (dest, src) -> $"FCVTZS({dest}, {src})"

                $"Instruction {i}: {instrStr}\n      Expected: 0x{expected:X8}, Got: 0x{actual:X8}")
        String.concat "\n    " lines

/// Run ARM64 encoding test
let runARM64EncodingTest (test: ARM64EncodingTest) : PassTestResult =
    // Encode each instruction
    let encodeResults =
        test.Instructions
        |> List.mapi (fun i instr ->
            let codes = encode instr
            if codes.Length <> 1 then
                Error $"Instruction {i}: Expected single machine code word per instruction, got {codes.Length}"
            else
                Ok codes.[0])

    // Check if any encoding failed
    let firstError =
        encodeResults
        |> List.tryFind (function | Error _ -> true | Ok _ -> false)

    match firstError with
    | Some (Error msg) ->
        { Success = false
          Message = msg
          Expected = None
          Actual = None }
    | _ ->
        // Extract all successful encodings
        let results =
            encodeResults
            |> List.map (function | Ok code -> code | Error _ -> 0u)

        // Check each encoding matches expected
        let mismatches =
            List.zip3 test.Instructions results test.ExpectedHex
            |> List.mapi (fun i (instr, actual, expected) ->
                if actual <> expected then
                    Some (i, instr, expected, actual)
                else
                    None)
            |> List.choose id

        // Check if all values are different (if required)
        let allDifferentCheck =
            if test.AssertDifferent then
                hasAllDifferent results
            else
                true

        if mismatches.IsEmpty && allDifferentCheck then
            { Success = true
              Message = "Test passed"
              Expected = None
              Actual = None }
        else
            let msg =
                if not mismatches.IsEmpty then
                    formatMismatches mismatches
                else
                    "ASSERT-DIFFERENT failed: not all hex values are different"

            { Success = false
              Message = msg
              Expected = None
              Actual = None }
