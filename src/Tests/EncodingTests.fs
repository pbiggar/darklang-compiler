// EncodingTests.fs - Unit tests for ARM64 encoding utilities
//
// Tests utility functions like encodeReg that are used by the
// ARM64 instruction encoder.
//
// NOTE: All tests now return Result<> instead of using failwith

module EncodingTests

open ARM64
open ARM64_Encoding

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

/// Run all encoding unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let tests = [
        ("encodeReg", testEncodeReg)
        ("MOVK shift encoding", testMOVKShiftEncoding)
        ("MOVZ+MOVK sequence", testMOVZMOVKSequence)
    ]

    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
