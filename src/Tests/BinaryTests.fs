// BinaryTests.fs - Unit tests for binary generation utilities
//
// Tests utility functions and integration for Mach-O binary generation.
//
// Note: These tests are for Mach-O format specifically

module BinaryTests

open Binary
open Binary_Generation_MachO
open ARM64
open ARM64_Encoding

let testUint32ToBytes () =
    let bytes = uint32ToBytes 0x12345678u
    let expected = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy |]
    if bytes <> expected then
        failwith "uint32ToBytes failed"

let testUint64ToBytes () =
    let bytes = uint64ToBytes 0x123456789ABCDEF0UL
    let expected = [| 0xF0uy; 0xDEuy; 0xBCuy; 0x9Auy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]
    if bytes <> expected then
        failwith "uint64ToBytes failed"

let testPadString () =
    let padded = padString "hello" 10
    if padded.Length <> 10 then
        failwith "padString: wrong length"
    if padded.[0] <> byte 'h' || padded.[4] <> byte 'o' then
        failwith "padString: wrong content"
    if padded.[5] <> 0uy || padded.[9] <> 0uy then
        failwith "padString: wrong padding"

let testPadStringTruncate () =
    let padded = padString "hello world this is long" 5
    if padded.Length <> 5 then
        failwith "padString truncate: wrong length"
    if padded.[0] <> byte 'h' || padded.[4] <> byte 'o' then
        failwith "padString truncate: wrong content"

let testSerializeMachHeaderSize () =
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
        failwith $"serializeMachHeader: expected size 32, got {bytes.Length}"

let testSerializeMachHeaderMagic () =
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
        failwith "serializeMachHeader: wrong magic bytes"

let testSerializeSection64Size () =
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
        failwith $"serializeSection64: expected size 80, got {bytes.Length}"

let testCreateExecutableNonEmpty () =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    if binary.Length = 0 then
        failwith "createExecutable: binary is empty"

let testCreateExecutableMagic () =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    let expectedMagic = [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |]
    if binary.[0..3] <> expectedMagic then
        failwith "createExecutable: wrong magic bytes"

let testCreateExecutableContainsCode () =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    let retBytes = [| 0xC0uy; 0x03uy; 0x5Fuy; 0xD6uy |]

    let mutable found = false
    for i in 0 .. binary.Length - 4 do
        if binary.[i] = retBytes.[0] &&
           binary.[i+1] = retBytes.[1] &&
           binary.[i+2] = retBytes.[2] &&
           binary.[i+3] = retBytes.[3] then
            found <- true

    if not found then
        failwith "createExecutable: RET instruction not found in binary"

let testCompleteEncodingPipeline () =
    // Test the complete pipeline: instructions -> encoding -> binary
    let movInstr = MOVZ (X0, 42us, 0)
    let retInstr = RET
    let machineCode = (encode movInstr) @ (encode retInstr)

    // Verify encoding
    if machineCode.Length <> 2 then
        failwith "Complete pipeline: wrong machine code count"
    if machineCode.[0] <> 0xD2800540u then
        failwith "Complete pipeline: wrong MOVZ encoding"
    if machineCode.[1] <> 0xD65F03C0u then
        failwith "Complete pipeline: wrong RET encoding"

    // Verify binary generation
    let binary = createExecutable machineCode
    if binary.Length = 0 then
        failwith "Complete pipeline: binary is empty"
    let expectedMagic = [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |]
    if binary.[0..3] <> expectedMagic then
        failwith "Complete pipeline: wrong magic bytes"

/// Run all binary generation unit tests
let runAll () =
    testUint32ToBytes()
    testUint64ToBytes()
    testPadString()
    testPadStringTruncate()
    testSerializeMachHeaderSize()
    testSerializeMachHeaderMagic()
    testSerializeSection64Size()
    testCreateExecutableNonEmpty()
    testCreateExecutableMagic()
    testCreateExecutableContainsCode()
    testCompleteEncodingPipeline()
