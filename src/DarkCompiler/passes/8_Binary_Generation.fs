// 8_Binary_Generation.fs - Mach-O Binary Generation (Pass 8)
//
// Generates a complete Mach-O executable from ARM64 machine code.
//
// Binary generation algorithm:
// - Converts machine code (uint32 list) to byte array
// - Calculates file offsets and VM addresses for segments
// - Creates Mach-O header with ARM64 CPU type
// - Creates __PAGEZERO segment (4GB unmapped memory)
// - Creates __TEXT segment with __text section containing code
// - Creates LC_MAIN load command specifying entry point
// - Serializes all structures to little-endian bytes
// - Writes byte array to file with executable permissions

module Binary_Generation

/// Helper: Pad string to fixed size with null bytes
let padString (s: string) (size: int) : byte array =
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    if bytes.Length > size then
        Array.truncate size bytes
    else
        Array.append bytes (Array.create (size - bytes.Length) 0uy)

/// Helper: Convert uint32 to little-endian bytes
let uint32ToBytes (value: uint32) : byte array =
    [|
        byte (value &&& 0xFFu)
        byte ((value >>> 8) &&& 0xFFu)
        byte ((value >>> 16) &&& 0xFFu)
        byte ((value >>> 24) &&& 0xFFu)
    |]

/// Helper: Convert uint64 to little-endian bytes
let uint64ToBytes (value: uint64) : byte array =
    [|
        byte (value &&& 0xFFUL)
        byte ((value >>> 8) &&& 0xFFUL)
        byte ((value >>> 16) &&& 0xFFUL)
        byte ((value >>> 24) &&& 0xFFUL)
        byte ((value >>> 32) &&& 0xFFUL)
        byte ((value >>> 40) &&& 0xFFUL)
        byte ((value >>> 48) &&& 0xFFUL)
        byte ((value >>> 56) &&& 0xFFUL)
    |]

/// Serialize MachHeader to bytes
let serializeMachHeader (header: Binary.MachHeader) : byte array =
    [|
        yield! uint32ToBytes header.Magic
        yield! uint32ToBytes header.CpuType
        yield! uint32ToBytes header.CpuSubType
        yield! uint32ToBytes header.FileType
        yield! uint32ToBytes header.NumCommands
        yield! uint32ToBytes header.SizeOfCommands
        yield! uint32ToBytes header.Flags
        yield! uint32ToBytes header.Reserved
    |]

/// Serialize Section64 to bytes
let serializeSection64 (section: Binary.Section64) : byte array =
    [|
        yield! padString section.SectionName 16
        yield! padString section.SegmentName 16
        yield! uint64ToBytes section.Address
        yield! uint64ToBytes section.Size
        yield! uint32ToBytes section.Offset
        yield! uint32ToBytes section.Align
        yield! uint32ToBytes section.RelocationOffset
        yield! uint32ToBytes section.NumRelocations
        yield! uint32ToBytes section.Flags
        yield! uint32ToBytes section.Reserved1
        yield! uint32ToBytes section.Reserved2
        yield! uint32ToBytes section.Reserved3
    |]

/// Serialize SegmentCommand64 to bytes
let serializeSegmentCommand64 (segment: Binary.SegmentCommand64) : byte array =
    [|
        yield! uint32ToBytes segment.Command
        yield! uint32ToBytes segment.CommandSize
        yield! padString segment.SegmentName 16
        yield! uint64ToBytes segment.VmAddress
        yield! uint64ToBytes segment.VmSize
        yield! uint64ToBytes segment.FileOffset
        yield! uint64ToBytes segment.FileSize
        yield! uint32ToBytes segment.MaxProt
        yield! uint32ToBytes segment.InitProt
        yield! uint32ToBytes segment.NumSections
        yield! uint32ToBytes segment.Flags
        for section in segment.Sections do
            yield! serializeSection64 section
    |]

/// Serialize MainCommand to bytes
let serializeMainCommand (main: Binary.MainCommand) : byte array =
    [|
        yield! uint32ToBytes main.Command
        yield! uint32ToBytes main.CommandSize
        yield! uint64ToBytes main.EntryOffset
        yield! uint64ToBytes main.StackSize
    |]

/// Serialize DylinkerCommand to bytes
let serializeDylinkerCommand (dylinker: Binary.DylinkerCommand) : byte array =
    [|
        yield! uint32ToBytes dylinker.Command
        yield! uint32ToBytes dylinker.CommandSize
        yield! uint32ToBytes 12u  // Offset to name string (after command + cmdsize + offset)
        yield! padString dylinker.Name (int dylinker.CommandSize - 12)
    |]

/// Serialize UuidCommand to bytes
let serializeUuidCommand (uuid: Binary.UuidCommand) : byte array =
    [|
        yield! uint32ToBytes uuid.Command
        yield! uint32ToBytes uuid.CommandSize
        yield! uuid.Uuid
    |]

/// Serialize BuildVersionCommand to bytes
let serializeBuildVersionCommand (buildVer: Binary.BuildVersionCommand) : byte array =
    [|
        yield! uint32ToBytes buildVer.Command
        yield! uint32ToBytes buildVer.CommandSize
        yield! uint32ToBytes buildVer.Platform
        yield! uint32ToBytes buildVer.MinOS
        yield! uint32ToBytes buildVer.Sdk
        yield! uint32ToBytes buildVer.NumTools
    |]

/// Calculate the size of load commands
let calculateCommandsSize (binary: Binary.MachOBinary) : uint32 =
    binary.PageZeroCommand.CommandSize +
    binary.TextSegmentCommand.CommandSize +
    binary.DylinkerCommand.CommandSize +
    binary.UuidCommand.CommandSize +
    binary.BuildVersionCommand.CommandSize +
    binary.MainCommand.CommandSize

/// Serialize complete Mach-O binary to bytes
let serializeMachO (binary: Binary.MachOBinary) : byte array =
    let headerSize = 32  // sizeof(mach_header_64)
    let commandsSize = int (calculateCommandsSize binary)
    let dataStart = headerSize + commandsSize

    // Pad to page boundary (16KB for ARM64 macOS)
    let pageSize = 16384
    let paddingNeeded = (pageSize - (dataStart % pageSize)) % pageSize
    let padding = Array.create paddingNeeded 0uy

    [|
        yield! serializeMachHeader binary.Header
        yield! serializeSegmentCommand64 binary.PageZeroCommand
        yield! serializeSegmentCommand64 binary.TextSegmentCommand
        yield! serializeDylinkerCommand binary.DylinkerCommand
        yield! serializeUuidCommand binary.UuidCommand
        yield! serializeBuildVersionCommand binary.BuildVersionCommand
        yield! serializeMainCommand binary.MainCommand
        yield! padding
        yield! binary.MachineCode
    |]

/// Create a minimal Mach-O executable from ARM64 machine code
let createExecutable (machineCode: uint32 list) : byte array =
    let codeBytes =
        machineCode
        |> List.collect (fun word ->
            uint32ToBytes word |> Array.toList)
        |> Array.ofList

    let codeSize = uint64 codeBytes.Length

    // VM addresses - load at typical location
    let vmBase = 0x100000000UL
    let vmCodeOffset = 0x4000UL  // Start code at offset within VM

    // File layout
    let headerSize = 32
    let pageZeroCommandSize = 72  // segment_command_64 with no sections
    let textSegmentCommandSize = 72 + 80  // segment_command_64 + 1 section_64
    let dylinkerCommandSize = 32  // cmd + cmdsize + offset + padded path string
    let uuidCommandSize = 24  // cmd + cmdsize + 16-byte uuid
    let buildVersionCommandSize = 24  // cmd + cmdsize + platform + minos + sdk + numtools
    let mainCommandSize = 24
    let commandsSize = pageZeroCommandSize + textSegmentCommandSize + dylinkerCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize
    let pageSize = 16384
    let dataStart = headerSize + commandsSize
    let paddingNeeded = (pageSize - (dataStart % pageSize)) % pageSize
    let codeFileOffset = uint64 (dataStart + paddingNeeded)

    // __PAGEZERO segment (required by modern macOS)
    let pageZeroCommand : Binary.SegmentCommand64 = {
        Command = Binary.LC_SEGMENT_64
        CommandSize = uint32 pageZeroCommandSize
        SegmentName = "__PAGEZERO"
        VmAddress = 0UL
        VmSize = vmBase
        FileOffset = 0UL
        FileSize = 0UL
        MaxProt = 0u
        InitProt = 0u
        NumSections = 0u
        Flags = 0u
        Sections = []
    }

    let textSection : Binary.Section64 = {
        SectionName = "__text"
        SegmentName = "__TEXT"
        Address = vmBase + vmCodeOffset
        Size = codeSize
        Offset = uint32 codeFileOffset
        Align = 2u  // 2^2 = 4 byte alignment
        RelocationOffset = 0u
        NumRelocations = 0u
        Flags = Binary.S_REGULAR ||| Binary.S_ATTR_PURE_INSTRUCTIONS ||| Binary.S_ATTR_SOME_INSTRUCTIONS
        Reserved1 = 0u
        Reserved2 = 0u
        Reserved3 = 0u
    }

    let textSegmentCommand : Binary.SegmentCommand64 = {
        Command = Binary.LC_SEGMENT_64
        CommandSize = uint32 textSegmentCommandSize
        SegmentName = "__TEXT"
        VmAddress = vmBase
        VmSize = vmCodeOffset + codeSize
        FileOffset = 0UL
        FileSize = codeFileOffset + codeSize
        MaxProt = Binary.VM_PROT_READ ||| Binary.VM_PROT_WRITE ||| Binary.VM_PROT_EXECUTE
        InitProt = Binary.VM_PROT_READ ||| Binary.VM_PROT_EXECUTE
        NumSections = 1u
        Flags = 0u
        Sections = [textSection]
    }

    // LC_LOAD_DYLINKER command
    let dylinkerCommand : Binary.DylinkerCommand = {
        Command = Binary.LC_LOAD_DYLINKER
        CommandSize = uint32 dylinkerCommandSize
        Name = "/usr/lib/dyld"
    }

    // LC_UUID command
    let uuidCommand : Binary.UuidCommand = {
        Command = Binary.LC_UUID
        CommandSize = uint32 uuidCommandSize
        Uuid = Array.create 16 0uy  // Zero UUID for now (could generate random)
    }

    // LC_BUILD_VERSION command
    let buildVersionCommand : Binary.BuildVersionCommand = {
        Command = Binary.LC_BUILD_VERSION
        CommandSize = uint32 buildVersionCommandSize
        Platform = 1u  // 1 = macOS
        MinOS = 0xB0000u  // macOS 11.0 (Big Sur)
        Sdk = 0xF0500u  // SDK 15.5
        NumTools = 0u  // No tool entries
    }

    let mainCommand : Binary.MainCommand = {
        Command = Binary.LC_MAIN
        CommandSize = uint32 mainCommandSize
        EntryOffset = codeFileOffset
        StackSize = 0UL
    }

    let header : Binary.MachHeader = {
        Magic = Binary.MH_MAGIC_64
        CpuType = Binary.CPU_TYPE_ARM64
        CpuSubType = Binary.CPU_SUBTYPE_ARM64_ALL
        FileType = Binary.MH_EXECUTE
        NumCommands = 6u  // Updated to include new load commands
        SizeOfCommands = uint32 commandsSize
        Flags = Binary.MH_NOUNDEFS ||| Binary.MH_PIE
        Reserved = 0u
    }

    let binary : Binary.MachOBinary = {
        Header = header
        PageZeroCommand = pageZeroCommand
        TextSegmentCommand = textSegmentCommand
        DylinkerCommand = dylinkerCommand
        UuidCommand = uuidCommand
        BuildVersionCommand = buildVersionCommand
        MainCommand = mainCommand
        MachineCode = codeBytes
    }

    serializeMachO binary

/// Write bytes to file
let writeToFile (path: string) (bytes: byte array) : unit =
    System.IO.File.WriteAllBytes(path, bytes)
    // Make executable
    let permissions = System.IO.File.GetUnixFileMode(path)
    System.IO.File.SetUnixFileMode(path, permissions ||| System.IO.UnixFileMode.UserExecute)
