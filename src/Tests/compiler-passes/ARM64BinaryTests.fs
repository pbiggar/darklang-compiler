// ARM64BinaryTests.fs - Unit tests for ARM64 Mach-O binary generation.

module ARM64BinaryTests

open Binary
open Binary_Generation_MachO
open ARM64
open ARM64_Encoding

/// Test result type
type TestResult = Result<unit, string>

let testUint32ToBytes () : TestResult =
    let bytes = uint32ToBytes 0x12345678u
    let expected = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy |]
    if bytes <> expected then
        Error "uint32ToBytes failed"
    else
        Ok ()

let testUint64ToBytes () : TestResult =
    let bytes = uint64ToBytes 0x123456789ABCDEF0UL
    let expected = [| 0xF0uy; 0xDEuy; 0xBCuy; 0x9Auy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]
    if bytes <> expected then
        Error "uint64ToBytes failed"
    else
        Ok ()

let testPadString () : TestResult =
    let padded = padString "hello" 10
    if padded.Length <> 10 then
        Error "padString: wrong length"
    elif padded.[0] <> byte 'h' || padded.[4] <> byte 'o' then
        Error "padString: wrong content"
    elif padded.[5] <> 0uy || padded.[9] <> 0uy then
        Error "padString: wrong padding"
    else
        Ok ()

let testPadStringTruncate () : TestResult =
    let padded = padString "hello world this is long" 5
    if padded.Length <> 5 then
        Error "padString truncate: wrong length"
    elif padded.[0] <> byte 'h' || padded.[4] <> byte 'o' then
        Error "padString truncate: wrong content"
    else
        Ok ()

let testSerializeMachHeaderSize () : TestResult =
    let header = {
        Magic = MH_MAGIC_64
        CpuType = CPU_TYPE_ARM64
        CpuSubType = CPU_SUBTYPE_ARM64_ALL
        FileType = MH_EXECUTE
        NumCommands = 2u
        SizeOfCommands = 100u
        Flags = MH_NOUNDEFS
        Reserved = 0u
    }
    let bytes = serializeMachHeader header
    if bytes.Length <> 32 then
        Error $"serializeMachHeader: expected size 32, got {bytes.Length}"
    else
        Ok ()

let testSerializeMachHeaderMagic () : TestResult =
    let header = {
        Magic = MH_MAGIC_64
        CpuType = CPU_TYPE_ARM64
        CpuSubType = CPU_SUBTYPE_ARM64_ALL
        FileType = MH_EXECUTE
        NumCommands = 2u
        SizeOfCommands = 100u
        Flags = MH_NOUNDEFS
        Reserved = 0u
    }
    let bytes = serializeMachHeader header
    let expectedMagic = [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |]
    if bytes.[0..3] <> expectedMagic then
        Error "serializeMachHeader: wrong magic bytes"
    else
        Ok ()

let testSerializeSection64Size () : TestResult =
    let section = {
        SectionName = "__text"
        SegmentName = "__TEXT"
        Address = 0UL
        Size = 100UL
        Offset = 0u
        Align = 2u
        RelocationOffset = 0u
        NumRelocations = 0u
        Flags = 0u
        Reserved1 = 0u
        Reserved2 = 0u
        Reserved3 = 0u
    }
    let bytes = serializeSection64 section
    if bytes.Length <> 80 then
        Error $"serializeSection64: expected size 80, got {bytes.Length}"
    else
        Ok ()

let testCreateExecutableNonEmpty () : TestResult =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    if binary.Length = 0 then
        Error "createExecutable: binary is empty"
    else
        Ok ()

let testCreateExecutableMagic () : TestResult =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    let expectedMagic = [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |]
    if binary.[0..3] <> expectedMagic then
        Error "createExecutable: wrong magic bytes"
    else
        Ok ()

let testCreateExecutableContainsCode () : TestResult =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    let retBytes = [| 0xC0uy; 0x03uy; 0x5Fuy; 0xD6uy |]
    let found =
        binary
        |> Array.windowed retBytes.Length
        |> Array.exists (fun bytes -> bytes = retBytes)

    if not found then
        Error "createExecutable: RET instruction not found in binary"
    else
        Ok ()

let private readUInt32LE (bytes: byte array) (offset: int) : uint32 =
    uint32 bytes.[offset]
    ||| (uint32 bytes.[offset + 1] <<< 8)
    ||| (uint32 bytes.[offset + 2] <<< 16)
    ||| (uint32 bytes.[offset + 3] <<< 24)

let testMachOConstSectionOffsetPointsToAlignedData () : TestResult =
    let (_stringId, stringPool) =
        LiteralPool.addString LiteralPool.emptyStringPool "abc"

    let binary =
        createExecutableWithPools [0xD65F03C0u] stringPool LiteralPool.emptyFloatPool false

    let textSegmentOffset = 32 + 72
    let firstSectionOffset = textSegmentOffset + 72
    let secondSectionOffset = firstSectionOffset + 80
    let sectionFileOffsetField = 48
    let constFileOffset =
        readUInt32LE binary (secondSectionOffset + sectionFileOffsetField)
        |> int

    let expectedLengthPrefix = [| 3uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
    let actualLengthPrefix = binary.[constFileOffset .. constFileOffset + 7]

    if constFileOffset % 8 <> 0 then
        Error $"Expected __const section file offset to be 8-byte aligned, got {constFileOffset}"
    elif actualLengthPrefix <> expectedLengthPrefix then
        Error $"Expected __const section offset to point at string length prefix, got {actualLengthPrefix}"
    else
        Ok ()

let testCompleteEncodingPipeline () : TestResult =
    // Test the complete pipeline: instructions -> encoding -> binary
    let movInstr = MOVZ (X0, 42us, 0)
    let retInstr = RET
    let machineCode = (encode movInstr) @ (encode retInstr)

    // Verify encoding
    if machineCode.Length <> 2 then
        Error "Complete pipeline: wrong machine code count"
    elif machineCode.[0] <> 0xD2800540u then
        Error "Complete pipeline: wrong MOVZ encoding"
    elif machineCode.[1] <> 0xD65F03C0u then
        Error "Complete pipeline: wrong RET encoding"
    else
        // Verify binary generation
        let binary = createExecutable machineCode
        if binary.Length = 0 then
            Error "Complete pipeline: binary is empty"
        else
            let expectedMagic = [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |]
            if binary.[0..3] <> expectedMagic then
                Error "Complete pipeline: wrong magic bytes"
            else
                Ok ()

let testWriteToFileReturnsErrorForInvalidPath () : TestResult =
    let missingDir =
        System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            $"dark-macho-missing-{System.Guid.NewGuid()}",
            "out"
        )

    match writeToFile missingDir [| 0uy |] with
    | Ok () -> Error "Expected invalid output path to return Error"
    | Error _ -> Ok ()

let tests = [
    ("uint32ToBytes", testUint32ToBytes)
    ("uint64ToBytes", testUint64ToBytes)
    ("padString", testPadString)
    ("padString truncate", testPadStringTruncate)
    ("serializeMachHeader size", testSerializeMachHeaderSize)
    ("serializeMachHeader magic", testSerializeMachHeaderMagic)
    ("serializeSection64 size", testSerializeSection64Size)
    ("createExecutable non-empty", testCreateExecutableNonEmpty)
    ("createExecutable magic", testCreateExecutableMagic)
    ("createExecutable contains code", testCreateExecutableContainsCode)
    ("Mach-O __const section offset points to aligned data", testMachOConstSectionOffsetPointsToAlignedData)
    ("complete encoding pipeline", testCompleteEncodingPipeline)
    ("writeToFile returns Error for invalid path", testWriteToFileReturnsErrorForInvalidPath)
]

/// Run all binary generation unit tests
/// Returns Ok () if all pass, Error with first failure message if any fail
let runAll () : TestResult =
    let rec runTests = function
        | [] -> Ok ()
        | (name, test) :: rest ->
            match test () with
            | Ok () -> runTests rest
            | Error msg -> Error $"{name} test failed: {msg}"

    runTests tests
