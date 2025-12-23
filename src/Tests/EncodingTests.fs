// EncodingTests.fs - Unit tests for ARM64 encoding utilities
//
// Tests utility functions like encodeReg that are used by the
// ARM64 instruction encoder.

module EncodingTests

open ARM64
open ARM64_Encoding

/// Test that encodeReg produces correct register numbers
let testEncodeReg () =
    let tests = [
        (X0, 0u, "X0")
        (X1, 1u, "X1")
        (X15, 15u, "X15")
        (X30, 30u, "X30")
        (SP, 31u, "SP")
    ]

    for (reg, expected, name) in tests do
        let actual = encodeReg reg
        if actual <> expected then
            failwith $"encodeReg {name}: expected {expected}, got {actual}"

/// Run all encoding unit tests
let runAll () =
    testEncodeReg()
    printfn "  ✓ encodeReg test passed"
