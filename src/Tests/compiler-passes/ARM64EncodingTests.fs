// ARM64EncodingTests.fs - Unit tests for ARM64 encoding utilities
//
// Tests utility functions like encodeReg that are used by the
// ARM64 instruction encoder.

module ARM64EncodingTests

open ARM64
open ARM64_Encoding
open TestDSL.ARM64EncodingFormat

/// Test result type
type TestResult = Result<unit, string>

/// Test that encodeReg produces correct register numbers
let testEncodeReg () : TestResult =
    let tests = [
        (X0, 0u, "X0")
        (X1, 1u, "X1")
        (X15, 15u, "X15")
        (X30, 30u, "X30")
        (SP, 31u, "SP")
    ]

    let rec checkTests = function
        | [] -> Ok ()
        | (reg, expected, name) :: rest ->
            let actual = encodeReg reg
            if actual <> expected then
                Error $"encodeReg {name}: expected {expected}, got {actual}"
            else
                checkTests rest

    checkTests tests

/// Test MOVK encoding with various shift values
/// The shift parameter should be in bit positions (0, 16, 32, 48)
/// and gets converted to hw values (0, 1, 2, 3) by dividing by 16
let testMOVKShiftEncoding () : TestResult =
    // MOVK X0, #0xFFFF, shift
    // Expected encoding: sf=1 opc=11 100101 hw imm16 Rd
    // sf=1 (bit 31), opc=11 (bits 30-29), 100101 (bits 28-23), hw (bits 22-21), imm16 (bits 20-5), Rd (bits 4-0)
    let tests = [
        // (shift, expected_hw, test_name)
        (0, 0u, "shift=0 → hw=0")
        (16, 1u, "shift=16 → hw=1")
        (32, 2u, "shift=32 → hw=2")
        (48, 3u, "shift=48 → hw=3")
    ]

    let rec checkTests = function
        | [] -> Ok ()
        | (shift, expectedHw, name) :: rest ->
            let encoded = encode (MOVK (X0, 0xFFFFus, shift))
            match encoded with
            | [word] ->
                // Extract hw field: bits 22-21
                let actualHw = (word >>> 21) &&& 0x3u
                if actualHw <> expectedHw then
                    Error $"MOVK {name}: expected hw={expectedHw}, got hw={actualHw} (encoded=0x{word:X8})"
                else
                    checkTests rest
            | _ ->
                Error $"MOVK {name}: expected 1 word, got {List.length encoded}"

    checkTests tests

/// Test that MOVZ + MOVK sequence correctly builds 64-bit values
/// This tests the common pattern for loading large immediates
let testMOVZMOVKSequence () : TestResult =
    // Build 0x0001_0000 (65536) using MOVZ + MOVK
    // MOVZ X0, #0, 0     -> X0 = 0
    // MOVK X0, #1, 16    -> X0[31:16] = 1, so X0 = 0x10000 = 65536
    let movz = encode (MOVZ (X0, 0us, 0))
    let movk = encode (MOVK (X0, 1us, 16))

    match movz, movk with
    | [movzWord], [movkWord] ->
        // Verify MOVZ has hw=0
        let movzHw = (movzWord >>> 21) &&& 0x3u
        if movzHw <> 0u then
            Error $"MOVZ: expected hw=0, got hw={movzHw}"
        else
            // Verify MOVK has hw=1 (for shift=16)
            let movkHw = (movkWord >>> 21) &&& 0x3u
            if movkHw <> 1u then
                Error $"MOVK: expected hw=1, got hw={movkHw}"
            else
                Ok ()
    | _ ->
        Error "MOVZ/MOVK sequence: unexpected encoding length"

let private expectCrash (name: string) (f: unit -> unit) : TestResult =
    try
        f ()
        Error $"{name}: expected encoder to reject invalid immediate offset"
    with
    | _ -> Ok ()

let testUnsignedMemoryOffsetsRejectInvalidValues () : TestResult =
    let cases = [
        ("STR negative offset", fun () -> encode (STR (X0, SP, -8s)) |> ignore)
        ("LDR unaligned offset", fun () -> encode (LDR (X0, SP, 2s)) |> ignore)
        ("STR out-of-range offset", fun () -> encode (STR (X0, SP, 32761s)) |> ignore)
        ("LDR_fp negative offset", fun () -> encode (LDR_fp (D0, SP, -8s)) |> ignore)
    ]

    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (name, f) :: rest ->
            match expectCrash name f with
            | Ok () -> checkCases rest
            | Error msg -> Error msg

    checkCases cases

let testSignedPairOffsetsRejectInvalidValues () : TestResult =
    let cases = [
        ("STP unaligned offset", fun () -> encode (STP (X0, X1, SP, 7s)) |> ignore)
        ("LDP out-of-range offset", fun () -> encode (LDP (X0, X1, SP, 512s)) |> ignore)
        ("STP_pre negative out-of-range offset", fun () -> encode (STP_pre (X0, X1, SP, -520s)) |> ignore)
        ("LDP_post unaligned offset", fun () -> encode (LDP_post (X0, X1, SP, -7s)) |> ignore)
        ("STP_fp out-of-range offset", fun () -> encode (STP_fp (D0, D1, SP, 512s)) |> ignore)
        ("LDP_fp unaligned offset", fun () -> encode (LDP_fp (D0, D1, SP, 6s)) |> ignore)
    ]

    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (name, f) :: rest ->
            match expectCrash name f with
            | Ok () -> checkCases rest
            | Error msg -> Error msg

    checkCases cases

let testArithmeticImmediatesRejectOutOfRangeValues () : TestResult =
    let cases = [
        ("ADD_imm 4096", fun () -> encode (ADD_imm (X0, X1, 4096us)) |> ignore)
        ("SUB_imm 4096", fun () -> encode (SUB_imm (X0, X1, 4096us)) |> ignore)
        ("SUB_imm12 4096", fun () -> encode (SUB_imm12 (X0, X1, 4096us)) |> ignore)
        ("SUBS_imm 4096", fun () -> encode (SUBS_imm (X0, X1, 4096us)) |> ignore)
        ("CMP_imm 4096", fun () -> encode (CMP_imm (X1, 4096us)) |> ignore)
    ]

    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (name, f) :: rest ->
            match expectCrash name f with
            | Ok () -> checkCases rest
            | Error msg -> Error msg

    checkCases cases

let testMoveWideShiftsRejectInvalidValues () : TestResult =
    let cases = [
        ("MOVZ shift 8", fun () -> encode (MOVZ (X0, 1us, 8)) |> ignore)
        ("MOVN shift 24", fun () -> encode (MOVN (X0, 1us, 24)) |> ignore)
        ("MOVK shift 64", fun () -> encode (MOVK (X0, 1us, 64)) |> ignore)
    ]

    let rec checkCases remaining =
        match remaining with
        | [] -> Ok ()
        | (name, f) :: rest ->
            match expectCrash name f with
            | Ok () -> checkCases rest
            | Error msg -> Error msg

    checkCases cases

let testFMOVImmediateEncoding () : TestResult =
    let cases = [
        "1.0", FMOV_imm (D2, 1.0), 0x1E6E1002u
        "4.0", FMOV_imm (D3, 4.0), 0x1E621003u
    ]

    let rec check remaining =
        match remaining with
        | [] -> Ok ()
        | (name, instr, expected) :: rest ->
            match encode instr with
            | [word] when word = expected ->
                check rest
            | [word] ->
                Error $"FMOV_imm {name}: expected 0x{expected:X8}, got 0x{word:X8}"
            | words ->
                Error $"FMOV_imm {name}: expected 1 word, got {List.length words}"

    check cases

let testBICRegisterEncoding () : TestResult =
    match encode (BIC_reg (X3, X1, X2)) with
    | [word] when word = 0x8A220023u -> Ok ()
    | [word] -> Error $"BIC_reg: expected 0x8A220023, got 0x{word:X8}"
    | words -> Error $"BIC_reg: expected 1 word, got {List.length words}"

let testInvalidAssertDifferentValueIsRejected () : TestResult =
    let content =
        """---INPUT-ARM64---
RET

---OUTPUT-HEX---
0xD65F03C0

---ASSERT-DIFFERENT---
maybe
"""

    match parseARM64EncodingTest content with
    | Ok _ -> Error "expected invalid ASSERT-DIFFERENT value to be rejected"
    | Error msg when msg.Contains("ASSERT-DIFFERENT") -> Ok ()
    | Error msg -> Error $"expected ASSERT-DIFFERENT parse error, got: {msg}"

let tests = [
    ("encodeReg", testEncodeReg)
    ("MOVK shift encoding", testMOVKShiftEncoding)
    ("MOVZ+MOVK sequence", testMOVZMOVKSequence)
    ("unsigned memory offsets reject invalid values", testUnsignedMemoryOffsetsRejectInvalidValues)
    ("signed pair offsets reject invalid values", testSignedPairOffsetsRejectInvalidValues)
    ("arithmetic immediates reject out-of-range values", testArithmeticImmediatesRejectOutOfRangeValues)
    ("move-wide shifts reject invalid values", testMoveWideShiftsRejectInvalidValues)
    ("FMOV immediate encoding", testFMOVImmediateEncoding)
    ("BIC register encoding", testBICRegisterEncoding)
    ("invalid ASSERT-DIFFERENT value is rejected", testInvalidAssertDifferentValueIsRejected)
]

/// Run all encoding unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
