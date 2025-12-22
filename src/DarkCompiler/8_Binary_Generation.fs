// 8_Binary_Generation.fs - Mach-O Binary Generation (Pass 8)
//
// Generates a complete Mach-O executable from ARM64 machine code.
//
// This pass:
// - Converts machine code (list of uint32) to bytes
// - Creates Mach-O headers, load commands, and segments
// - Serializes all structures to bytes in the correct format
// - Writes the executable file with correct permissions
//
// Memory layout:
//   0x0000000000000000  __PAGEZERO (4GB unmapped)
//   0x0000000100000000  __TEXT segment
//   0x0000000100004000  .text section (code starts here)
//
// The generated executable:
// - Runs on ARM64 macOS
// - Returns the computed value as exit code
// - Is position-independent (PIE)

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

/// Calculate the size of load commands
let calculateCommandsSize (binary: Binary.MachOBinary) : uint32 =
    binary.PageZeroCommand.CommandSize + binary.TextSegmentCommand.CommandSize + binary.MainCommand.CommandSize

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
        Binary.Command = Binary.LC_SEGMENT_64
        Binary.CommandSize = uint32 pageZeroCommandSize
        Binary.SegmentName = "__PAGEZERO"
        Binary.VmAddress = 0UL
        Binary.VmSize = vmBase
        Binary.FileOffset = 0UL
        Binary.FileSize = 0UL
        Binary.MaxProt = 0u
        Binary.InitProt = 0u
        Binary.NumSections = 0u
        Binary.Flags = 0u
        Binary.Sections = []
    }

    let textSection = {
        Binary.SectionName = "__text"
        Binary.SegmentName = "__TEXT"
        Binary.Address = vmBase + vmCodeOffset
        Binary.Size = codeSize
        Binary.Offset = uint32 codeFileOffset
        Binary.Align = 2u  // 2^2 = 4 byte alignment
        Binary.RelocationOffset = 0u
        Binary.NumRelocations = 0u
        Binary.Flags = Binary.S_REGULAR ||| Binary.S_ATTR_PURE_INSTRUCTIONS ||| Binary.S_ATTR_SOME_INSTRUCTIONS
        Binary.Reserved1 = 0u
        Binary.Reserved2 = 0u
        Binary.Reserved3 = 0u
    }

    let textSegmentCommand = {
        Binary.Command = Binary.LC_SEGMENT_64
        Binary.CommandSize = uint32 textSegmentCommandSize
        Binary.SegmentName = "__TEXT"
        Binary.VmAddress = vmBase
        Binary.VmSize = vmCodeOffset + codeSize
        Binary.FileOffset = 0UL
        Binary.FileSize = codeFileOffset + codeSize
        Binary.MaxProt = Binary.VM_PROT_READ ||| Binary.VM_PROT_WRITE ||| Binary.VM_PROT_EXECUTE
        Binary.InitProt = Binary.VM_PROT_READ ||| Binary.VM_PROT_EXECUTE
        Binary.NumSections = 1u
        Binary.Flags = 0u
        Binary.Sections = [textSection]
    }

    let mainCommand = {
        Binary.Command = Binary.LC_MAIN
        Binary.CommandSize = uint32 mainCommandSize
        Binary.EntryOffset = codeFileOffset
        Binary.StackSize = 0UL
    }

    let header = {
        Binary.Magic = Binary.MH_MAGIC_64
        Binary.CpuType = Binary.CPU_TYPE_ARM64
        Binary.CpuSubType = Binary.CPU_SUBTYPE_ARM64_ALL
        Binary.FileType = Binary.MH_EXECUTE
        Binary.NumCommands = 3u
        Binary.SizeOfCommands = uint32 commandsSize
        Binary.Flags = Binary.MH_NOUNDEFS ||| Binary.MH_PIE
        Binary.Reserved = 0u
    }

    let binary = {
        Binary.Header = header
        Binary.PageZeroCommand = pageZeroCommand
        Binary.TextSegmentCommand = textSegmentCommand
        Binary.MainCommand = mainCommand
        Binary.MachineCode = codeBytes
    }

    serializeMachO binary

/// Write bytes to file
let writeToFile (path: string) (bytes: byte array) : unit =
    System.IO.File.WriteAllBytes(path, bytes)
    // Make executable
    let permissions = System.IO.File.GetUnixFileMode(path)
    System.IO.File.SetUnixFileMode(path, permissions ||| System.IO.UnixFileMode.UserExecute)
