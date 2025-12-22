module BinaryTests

open NUnit.Framework
open FsUnit
open Binary

[<Test>]
let ``uint32ToBytes converts correctly`` () =
    let bytes = uint32ToBytes 0x12345678u
    bytes.Length |> should equal 4
    bytes.[0] |> should equal 0x78uy
    bytes.[1] |> should equal 0x56uy
    bytes.[2] |> should equal 0x34uy
    bytes.[3] |> should equal 0x12uy

[<Test>]
let ``uint64ToBytes converts correctly`` () =
    let bytes = uint64ToBytes 0x123456789ABCDEF0UL
    bytes.Length |> should equal 8
    bytes.[0] |> should equal 0xF0uy
    bytes.[1] |> should equal 0xDEuy
    bytes.[2] |> should equal 0xBCuy
    bytes.[3] |> should equal 0x9Auy
    bytes.[4] |> should equal 0x78uy
    bytes.[5] |> should equal 0x56uy
    bytes.[6] |> should equal 0x34uy
    bytes.[7] |> should equal 0x12uy

[<Test>]
let ``padString pads to correct size`` () =
    let padded = padString "hello" 10
    padded.Length |> should equal 10
    padded.[0] |> should equal (byte 'h')
    padded.[4] |> should equal (byte 'o')
    padded.[5] |> should equal 0uy
    padded.[9] |> should equal 0uy

[<Test>]
let ``padString truncates if too long`` () =
    let padded = padString "hello world this is long" 5
    padded.Length |> should equal 5
    padded.[0] |> should equal (byte 'h')
    padded.[4] |> should equal (byte 'o')

[<Test>]
let ``serializeMachHeader produces correct size`` () =
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
    bytes.Length |> should equal 32

[<Test>]
let ``serializeMachHeader has correct magic`` () =
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
    // Check magic number (0xFEEDFACF in little-endian)
    bytes.[0] |> should equal 0xCFuy
    bytes.[1] |> should equal 0xFAuy
    bytes.[2] |> should equal 0xEDuy
    bytes.[3] |> should equal 0xFEuy

[<Test>]
let ``serializeSection64 produces correct size`` () =
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
    bytes.Length |> should equal 80

[<Test>]
let ``createExecutable produces non-empty binary`` () =
    // Simple RET instruction
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    binary.Length |> should be (greaterThan 0)

[<Test>]
let ``createExecutable binary starts with Mach-O magic`` () =
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    // Check magic number at start
    binary.[0] |> should equal 0xCFuy
    binary.[1] |> should equal 0xFAuy
    binary.[2] |> should equal 0xEDuy
    binary.[3] |> should equal 0xFEuy

[<Test>]
let ``createExecutable includes machine code`` () =
    // RET instruction: 0xD65F03C0
    let machineCode = [0xD65F03C0u]
    let binary = createExecutable machineCode
    // Find the RET instruction in the binary (should be near the end after padding)
    let retBytes = [| 0xC0uy; 0x03uy; 0x5Fuy; 0xD6uy |]
    let mutable found = false
    for i in 0 .. binary.Length - 4 do
        if binary.[i] = retBytes.[0] &&
           binary.[i+1] = retBytes.[1] &&
           binary.[i+2] = retBytes.[2] &&
           binary.[i+3] = retBytes.[3] then
            found <- true
    found |> should equal true

[<Test>]
let ``generated executable has correct encoding`` () =
    // Create simple program: mov x0, #42; ret
    // Verify the machine code is correctly encoded
    let movInstr = ARM64.MOVZ (ARM64.X0, 42us, 0)
    let retInstr = ARM64.RET
    let machineCode = (ARM64.encode movInstr) @ (ARM64.encode retInstr)

    // Verify encoding
    machineCode.Length |> should equal 2
    machineCode.[0] |> should equal 0xD2800540u  // MOVZ X0, #42
    machineCode.[1] |> should equal 0xD65F03C0u  // RET

    // Verify binary can be created
    let binary = createExecutable machineCode
    binary.Length |> should be (greaterThan 0)

    // Verify it starts with Mach-O magic
    binary.[0] |> should equal 0xCFuy
    binary.[1] |> should equal 0xFAuy
    binary.[2] |> should equal 0xEDuy
    binary.[3] |> should equal 0xFEuy
