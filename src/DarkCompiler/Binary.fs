module Binary

/// Mach-O magic number for 64-bit
let MH_MAGIC_64 = 0xFEEDFACFu

/// CPU type constants
let CPU_TYPE_ARM64 = 0x0100000Cu
let CPU_SUBTYPE_ARM64_ALL = 0u

/// File type constants
let MH_EXECUTE = 0x2u  // Executable file

/// Flags
let MH_NOUNDEFS = 0x1u
let MH_PIE = 0x200000u

/// Load command types
let LC_SEGMENT_64 = 0x19u
let LC_MAIN = 0x80000028u

/// Virtual memory protections
let VM_PROT_READ = 0x1u
let VM_PROT_WRITE = 0x2u
let VM_PROT_EXECUTE = 0x4u

/// Section flags
let S_REGULAR = 0x0u
let S_ATTR_PURE_INSTRUCTIONS = 0x80000000u
let S_ATTR_SOME_INSTRUCTIONS = 0x00000400u

/// Mach-O header
type MachHeader = {
    Magic: uint32
    CpuType: uint32
    CpuSubType: uint32
    FileType: uint32
    NumCommands: uint32
    SizeOfCommands: uint32
    Flags: uint32
    Reserved: uint32  // For 64-bit
}

/// Section within a segment
type Section64 = {
    SectionName: string  // Max 16 bytes
    SegmentName: string  // Max 16 bytes
    Address: uint64
    Size: uint64
    Offset: uint32
    Align: uint32
    RelocationOffset: uint32
    NumRelocations: uint32
    Flags: uint32
    Reserved1: uint32
    Reserved2: uint32
    Reserved3: uint32
}

/// LC_SEGMENT_64 load command
type SegmentCommand64 = {
    Command: uint32
    CommandSize: uint32
    SegmentName: string  // Max 16 bytes
    VmAddress: uint64
    VmSize: uint64
    FileOffset: uint64
    FileSize: uint64
    MaxProt: uint32
    InitProt: uint32
    NumSections: uint32
    Flags: uint32
    Sections: Section64 list
}

/// LC_MAIN load command (entry point)
type MainCommand = {
    Command: uint32
    CommandSize: uint32
    EntryOffset: uint64
    StackSize: uint64
}

/// Complete Mach-O binary structure
type MachOBinary = {
    Header: MachHeader
    PageZeroCommand: SegmentCommand64
    TextSegmentCommand: SegmentCommand64
    MainCommand: MainCommand
    MachineCode: byte array
}

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
let serializeMachHeader (header: MachHeader) : byte array =
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
let serializeSection64 (section: Section64) : byte array =
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
let serializeSegmentCommand64 (segment: SegmentCommand64) : byte array =
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
let serializeMainCommand (main: MainCommand) : byte array =
    [|
        yield! uint32ToBytes main.Command
        yield! uint32ToBytes main.CommandSize
        yield! uint64ToBytes main.EntryOffset
        yield! uint64ToBytes main.StackSize
    |]

/// Calculate the size of load commands
let calculateCommandsSize (binary: MachOBinary) : uint32 =
    binary.PageZeroCommand.CommandSize + binary.TextSegmentCommand.CommandSize + binary.MainCommand.CommandSize

/// Serialize complete Mach-O binary to bytes
let serializeMachO (binary: MachOBinary) : byte array =
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
    let mainCommandSize = 24
    let commandsSize = pageZeroCommandSize + textSegmentCommandSize + mainCommandSize
    let pageSize = 16384
    let dataStart = headerSize + commandsSize
    let paddingNeeded = (pageSize - (dataStart % pageSize)) % pageSize
    let codeFileOffset = uint64 (dataStart + paddingNeeded)

    // __PAGEZERO segment (required by modern macOS)
    let pageZeroCommand = {
        Command = LC_SEGMENT_64
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

    let textSection = {
        SectionName = "__text"
        SegmentName = "__TEXT"
        Address = vmBase + vmCodeOffset
        Size = codeSize
        Offset = uint32 codeFileOffset
        Align = 2u  // 2^2 = 4 byte alignment
        RelocationOffset = 0u
        NumRelocations = 0u
        Flags = S_REGULAR ||| S_ATTR_PURE_INSTRUCTIONS ||| S_ATTR_SOME_INSTRUCTIONS
        Reserved1 = 0u
        Reserved2 = 0u
        Reserved3 = 0u
    }

    let textSegmentCommand = {
        Command = LC_SEGMENT_64
        CommandSize = uint32 textSegmentCommandSize
        SegmentName = "__TEXT"
        VmAddress = vmBase
        VmSize = vmCodeOffset + codeSize
        FileOffset = 0UL
        FileSize = codeFileOffset + codeSize
        MaxProt = VM_PROT_READ ||| VM_PROT_WRITE ||| VM_PROT_EXECUTE
        InitProt = VM_PROT_READ ||| VM_PROT_EXECUTE
        NumSections = 1u
        Flags = 0u
        Sections = [textSection]
    }

    let mainCommand = {
        Command = LC_MAIN
        CommandSize = uint32 mainCommandSize
        EntryOffset = codeFileOffset
        StackSize = 0UL
    }

    let header = {
        Magic = MH_MAGIC_64
        CpuType = CPU_TYPE_ARM64
        CpuSubType = CPU_SUBTYPE_ARM64_ALL
        FileType = MH_EXECUTE
        NumCommands = 3u
        SizeOfCommands = uint32 commandsSize
        Flags = MH_NOUNDEFS ||| MH_PIE
        Reserved = 0u
    }

    let binary = {
        Header = header
        PageZeroCommand = pageZeroCommand
        TextSegmentCommand = textSegmentCommand
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
