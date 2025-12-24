// 8_Binary_Generation_MachO.fs - Mach-O Binary Generation (Pass 8, macOS variant)
//
// Generates a complete Mach-O executable from ARM64 machine code for macOS.
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
// - Code signs the binary (required for macOS)

module Binary_Generation_MachO

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

/// Serialize DylibCommand to bytes
let serializeDylibCommand (dylib: Binary.DylibCommand) : byte array =
    [|
        yield! uint32ToBytes dylib.Command
        yield! uint32ToBytes dylib.CommandSize
        yield! uint32ToBytes 24u  // Offset to name string (after command + cmdsize + offset + timestamp + current_version + compatibility_version)
        yield! uint32ToBytes dylib.Timestamp
        yield! uint32ToBytes dylib.CurrentVersion
        yield! uint32ToBytes dylib.CompatibilityVersion
        yield! padString dylib.Name (int dylib.CommandSize - 24)
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

/// Serialize SymtabCommand to bytes
let serializeSymtabCommand (symtab: Binary.SymtabCommand) : byte array =
    [|
        yield! uint32ToBytes symtab.Command
        yield! uint32ToBytes symtab.CommandSize
        yield! uint32ToBytes symtab.SymbolTableOffset
        yield! uint32ToBytes symtab.NumSymbols
        yield! uint32ToBytes symtab.StringTableOffset
        yield! uint32ToBytes symtab.StringTableSize
    |]

/// Serialize DysymtabCommand to bytes
let serializeDysymtabCommand (dysymtab: Binary.DysymtabCommand) : byte array =
    [|
        yield! uint32ToBytes dysymtab.Command
        yield! uint32ToBytes dysymtab.CommandSize
        yield! uint32ToBytes dysymtab.LocalSymIndex
        yield! uint32ToBytes dysymtab.NumLocalSymbols
        yield! uint32ToBytes dysymtab.ExtDefSymIndex
        yield! uint32ToBytes dysymtab.NumExtDefSymbols
        yield! uint32ToBytes dysymtab.UndefSymIndex
        yield! uint32ToBytes dysymtab.NumUndefSymbols
        yield! uint32ToBytes dysymtab.TocOffset
        yield! uint32ToBytes dysymtab.NumTocEntries
        yield! uint32ToBytes dysymtab.ModTableOffset
        yield! uint32ToBytes dysymtab.NumModTableEntries
        yield! uint32ToBytes dysymtab.ExtRefSymOffset
        yield! uint32ToBytes dysymtab.NumExtRefSyms
        yield! uint32ToBytes dysymtab.IndirectSymOffset
        yield! uint32ToBytes dysymtab.NumIndirectSyms
        yield! uint32ToBytes dysymtab.ExtRelOffset
        yield! uint32ToBytes dysymtab.NumExtRel
        yield! uint32ToBytes dysymtab.LocRelOffset
        yield! uint32ToBytes dysymtab.NumLocRel
    |]

/// Calculate the size of load commands
let calculateCommandsSize (binary: Binary.MachOBinary) : uint32 =
    binary.PageZeroCommand.CommandSize +
    binary.TextSegmentCommand.CommandSize +
    binary.LinkeditSegmentCommand.CommandSize +
    binary.DylinkerCommand.CommandSize +
    binary.DylibCommand.CommandSize +
    binary.SymtabCommand.CommandSize +
    binary.DysymtabCommand.CommandSize +
    binary.UuidCommand.CommandSize +
    binary.BuildVersionCommand.CommandSize +
    binary.MainCommand.CommandSize

/// Serialize complete Mach-O binary to bytes
let serializeMachO (binary: Binary.MachOBinary) : byte array =
    let headerSize = 32  // sizeof(mach_header_64)
    let commandsSize = int (calculateCommandsSize binary)
    let dataStart = headerSize + commandsSize

    // Pad to code offset (extract from first section's offset)
    let codeOffset =
        match binary.TextSegmentCommand.Sections with
        | section :: _ -> int section.Offset
        | [] -> 0x1000  // Default to 4KB if no sections

    let paddingBeforeCode = codeOffset - dataStart
    let paddingBefore = Array.create paddingBeforeCode 0uy

    // Pad to fill the entire __TEXT segment
    let textSegmentSize = int binary.TextSegmentCommand.FileSize
    let codeSize = binary.MachineCode.Length
    let paddingAfterCode = textSegmentSize - codeOffset - codeSize
    let paddingAfter = Array.create paddingAfterCode 0uy

    [|
        yield! serializeMachHeader binary.Header
        yield! serializeSegmentCommand64 binary.PageZeroCommand
        yield! serializeSegmentCommand64 binary.TextSegmentCommand
        yield! serializeSegmentCommand64 binary.LinkeditSegmentCommand
        yield! serializeDylinkerCommand binary.DylinkerCommand
        yield! serializeDylibCommand binary.DylibCommand
        yield! serializeSymtabCommand binary.SymtabCommand
        yield! serializeDysymtabCommand binary.DysymtabCommand
        yield! serializeUuidCommand binary.UuidCommand
        yield! serializeBuildVersionCommand binary.BuildVersionCommand
        yield! serializeMainCommand binary.MainCommand
        yield! paddingBefore
        yield! binary.MachineCode
        yield! paddingAfter
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

    // Command sizes (needed for CommandSize fields)
    let pageZeroCommandSize = 72
    let textSegmentCommandSize = 72 + 80
    let linkeditSegmentCommandSize = 72
    let dylinkerCommandSize = 32
    let dylibCommandSize = 56  // 24 bytes for fixed fields + 32 bytes for padded library path
    let symtabCommandSize = 24
    let dysymtabCommandSize = 80
    let uuidCommandSize = 24
    let buildVersionCommandSize = 24
    let mainCommandSize = 24
    let commandsSize = pageZeroCommandSize + textSegmentCommandSize + linkeditSegmentCommandSize + dylinkerCommandSize + dylibCommandSize + symtabCommandSize + dysymtabCommandSize + uuidCommandSize + buildVersionCommandSize + mainCommandSize

    // Place code right after headers and load commands
    // Add extra space (200 bytes) for codesign to add LC_CODE_SIGNATURE and other modifications
    // Round up to 8-byte alignment
    let headerSize = 32
    let codeFileOffset = uint64 ((headerSize + commandsSize + 200 + 7) &&& ~~~7)
    let vmCodeOffset = codeFileOffset

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

    // Use a page-aligned segment size (16KB like ld produces)
    let textSegmentSize = 0x4000UL

    let textSegmentCommand : Binary.SegmentCommand64 = {
        Command = Binary.LC_SEGMENT_64
        CommandSize = uint32 textSegmentCommandSize
        SegmentName = "__TEXT"
        VmAddress = vmBase
        VmSize = textSegmentSize
        FileOffset = 0UL
        FileSize = textSegmentSize
        MaxProt = Binary.VM_PROT_READ ||| Binary.VM_PROT_EXECUTE
        InitProt = Binary.VM_PROT_READ ||| Binary.VM_PROT_EXECUTE
        NumSections = 1u
        Flags = 0u
        Sections = [textSection]
    }

    // __LINKEDIT segment (required for code signing - will be populated by codesign)
    let linkeditVmAddress = vmBase + textSegmentSize
    let linkeditFileOffset = textSegmentSize
    let linkeditSegmentCommand : Binary.SegmentCommand64 = {
        Command = Binary.LC_SEGMENT_64
        CommandSize = uint32 linkeditSegmentCommandSize
        SegmentName = "__LINKEDIT"
        VmAddress = linkeditVmAddress
        VmSize = 0UL  // Will be populated by codesign
        FileOffset = linkeditFileOffset
        FileSize = 0UL  // Will be populated by codesign
        MaxProt = Binary.VM_PROT_READ
        InitProt = Binary.VM_PROT_READ
        NumSections = 0u
        Flags = 0u
        Sections = []
    }

    // LC_SYMTAB command (empty symbol table - codesign may populate)
    let symtabCommand : Binary.SymtabCommand = {
        Command = Binary.LC_SYMTAB
        CommandSize = uint32 symtabCommandSize
        SymbolTableOffset = 0u
        NumSymbols = 0u
        StringTableOffset = 0u
        StringTableSize = 0u
    }

    // LC_DYSYMTAB command (empty dynamic symbol table)
    let dysymtabCommand : Binary.DysymtabCommand = {
        Command = Binary.LC_DYSYMTAB
        CommandSize = uint32 dysymtabCommandSize
        LocalSymIndex = 0u
        NumLocalSymbols = 0u
        ExtDefSymIndex = 0u
        NumExtDefSymbols = 0u
        UndefSymIndex = 0u
        NumUndefSymbols = 0u
        TocOffset = 0u
        NumTocEntries = 0u
        ModTableOffset = 0u
        NumModTableEntries = 0u
        ExtRefSymOffset = 0u
        NumExtRefSyms = 0u
        IndirectSymOffset = 0u
        NumIndirectSyms = 0u
        ExtRelOffset = 0u
        NumExtRel = 0u
        LocRelOffset = 0u
        NumLocRel = 0u
    }

    // LC_LOAD_DYLINKER command
    let dylinkerCommand : Binary.DylinkerCommand = {
        Command = Binary.LC_LOAD_DYLINKER
        CommandSize = uint32 dylinkerCommandSize
        Name = "/usr/lib/dyld"
    }

    // LC_LOAD_DYLIB command (link to libSystem)
    let dylibCommand : Binary.DylibCommand = {
        Command = Binary.LC_LOAD_DYLIB
        CommandSize = uint32 dylibCommandSize
        Name = "/usr/lib/libSystem.B.dylib"
        Timestamp = 2u  // Standard value (ignored by dyld)
        CurrentVersion = 0x00000000u  // 0.0.0
        CompatibilityVersion = 0x00010000u  // 1.0.0
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
        NumCommands = 10u  // __PAGEZERO, __TEXT, __LINKEDIT, DYLINKER, DYLIB, SYMTAB, DYSYMTAB, UUID, BUILD_VERSION, MAIN
        SizeOfCommands = uint32 commandsSize
        Flags = Binary.MH_NOUNDEFS ||| Binary.MH_DYLDLINK ||| Binary.MH_TWOLEVEL ||| Binary.MH_PIE
        Reserved = 0u
    }

    let binary : Binary.MachOBinary = {
        Header = header
        PageZeroCommand = pageZeroCommand
        TextSegmentCommand = textSegmentCommand
        LinkeditSegmentCommand = linkeditSegmentCommand
        DylinkerCommand = dylinkerCommand
        DylibCommand = dylibCommand
        SymtabCommand = symtabCommand
        DysymtabCommand = dysymtabCommand
        UuidCommand = uuidCommand
        BuildVersionCommand = buildVersionCommand
        MainCommand = mainCommand
        MachineCode = codeBytes
    }

    serializeMachO binary

/// Write bytes to file and sign it
let writeToFile (path: string) (bytes: byte array) : unit =
    System.IO.File.WriteAllBytes(path, bytes)
    // Make executable
    let permissions = System.IO.File.GetUnixFileMode(path)
    System.IO.File.SetUnixFileMode(path, permissions ||| System.IO.UnixFileMode.UserExecute)

    // Code sign with adhoc signature (required for macOS to execute)
    let startInfo = System.Diagnostics.ProcessStartInfo()
    startInfo.FileName <- "codesign"
    startInfo.Arguments <- sprintf "-s - \"%s\"" path
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false

    let proc = System.Diagnostics.Process.Start(startInfo)
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        let stderr = proc.StandardError.ReadToEnd()
        failwith (sprintf "Code signing failed: %s" stderr)
