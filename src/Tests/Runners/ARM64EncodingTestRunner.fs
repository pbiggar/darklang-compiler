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
                    | MOV_reg (dest, src) -> $"MOV_reg({dest}, {src})"
                    | RET -> "RET"
                    | SVC imm -> $"SVC({imm})"

                $"Instruction {i}: {instrStr}\n      Expected: 0x{expected:X8}, Got: 0x{actual:X8}")
        String.concat "\n    " lines

/// Run ARM64 encoding test
let runARM64EncodingTest (test: ARM64EncodingTest) : PassTestResult =
    // Encode each instruction
    let results =
        test.Instructions
        |> List.map (fun instr ->
            let codes = encode instr
            if codes.Length <> 1 then
                failwith $"Expected single machine code word per instruction, got {codes.Length}"
            else
                codes.[0])

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
