// 8_Binary_Generation_ELF.fs - ELF Binary Generation (Pass 8, Linux variant)
//
// Generates a complete ELF executable from ARM64 machine code for Linux.
//
// Binary generation algorithm:
// - Converts machine code (uint32 list) to byte array
// - Calculates file offsets and memory addresses for segments
// - Creates ELF64 header with ARM64 architecture
// - Creates PT_LOAD program header for executable code segment
// - Serializes all structures to little-endian bytes
// - Writes byte array to file with executable permissions
//
// No code signing needed on Linux.

module Binary_Generation_ELF

/// Helper: Convert uint16 to little-endian bytes
let uint16ToBytes (value: uint16) : byte array =
    [|
        byte (value &&& 0xFFus)
        byte ((value >>> 8) &&& 0xFFus)
    |]

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

/// Serialize ELF64 header to bytes
let serializeElf64Header (header: Binary_ELF.Elf64Header) : byte array =
    [|
        yield! header.Ident  // 16 bytes
        yield! uint16ToBytes header.Type
        yield! uint16ToBytes header.Machine
        yield! uint32ToBytes header.Version
        yield! uint64ToBytes header.Entry
        yield! uint64ToBytes header.PhOff
        yield! uint64ToBytes header.ShOff
        yield! uint32ToBytes header.Flags
        yield! uint16ToBytes header.EhSize
        yield! uint16ToBytes header.PhEntSize
        yield! uint16ToBytes header.PhNum
        yield! uint16ToBytes header.ShEntSize
        yield! uint16ToBytes header.ShNum
        yield! uint16ToBytes header.ShStrNdx
    |]

/// Serialize ELF64 program header to bytes
let serializeElf64ProgramHeader (ph: Binary_ELF.Elf64ProgramHeader) : byte array =
    [|
        yield! uint32ToBytes ph.Type
        yield! uint32ToBytes ph.Flags
        yield! uint64ToBytes ph.Offset
        yield! uint64ToBytes ph.VAddr
        yield! uint64ToBytes ph.PAddr
        yield! uint64ToBytes ph.FileSize
        yield! uint64ToBytes ph.MemSize
        yield! uint64ToBytes ph.Align
    |]

/// Serialize complete ELF binary to bytes
let serializeElf (binary: Binary_ELF.ElfBinary) : byte array =
    [|
        yield! serializeElf64Header binary.Header
        for ph in binary.ProgramHeaders do
            yield! serializeElf64ProgramHeader ph
        yield! binary.MachineCode
    |]

/// Create a minimal ELF executable from ARM64 machine code
let createExecutable (machineCode: uint32 list) : byte array =
    let codeBytes =
        machineCode
        |> List.collect (fun word ->
            uint32ToBytes word |> Array.toList)
        |> Array.ofList

    let codeSize = uint64 codeBytes.Length

    // ELF structures
    let elfHeaderSize = 64UL
    let programHeaderSize = 56UL
    let numProgramHeaders = 1us

    // Load address - typical for user-space programs
    let baseVAddr = 0x400000UL

    // Code starts right after headers
    let codeFileOffset = elfHeaderSize + (uint64 numProgramHeaders * programHeaderSize)
    let codeVAddr = baseVAddr + codeFileOffset

    // Create ELF identification bytes
    let ident = Array.create 16 0uy
    ident.[0] <- Binary_ELF.EI_MAG0  // 0x7F
    ident.[1] <- Binary_ELF.EI_MAG1  // 'E'
    ident.[2] <- Binary_ELF.EI_MAG2  // 'L'
    ident.[3] <- Binary_ELF.EI_MAG3  // 'F'
    ident.[4] <- Binary_ELF.ELFCLASS64  // 64-bit
    ident.[5] <- Binary_ELF.ELFDATA2LSB  // Little-endian
    ident.[6] <- Binary_ELF.EV_CURRENT   // Current version
    ident.[7] <- Binary_ELF.ELFOSABI_NONE  // System V ABI
    // Remaining bytes are 0 (padding)

    let header : Binary_ELF.Elf64Header = {
        Ident = ident
        Type = Binary_ELF.ET_EXEC
        Machine = Binary_ELF.EM_AARCH64
        Version = 1u  // Current version
        Entry = codeVAddr  // Entry point
        PhOff = elfHeaderSize  // Program headers start after ELF header
        ShOff = 0UL  // No section headers
        Flags = 0u  // No processor-specific flags
        EhSize = uint16 elfHeaderSize
        PhEntSize = uint16 programHeaderSize
        PhNum = numProgramHeaders
        ShEntSize = 0us  // No section headers
        ShNum = 0us
        ShStrNdx = 0us
    }

    // Create executable code segment
    // The PT_LOAD segment must include the ELF header and program headers
    // so the kernel can access them during execution
    let segmentFileSize = codeFileOffset + codeSize
    let segmentMemSize = segmentFileSize

    let codeSegment : Binary_ELF.Elf64ProgramHeader = {
        Type = Binary_ELF.PT_LOAD
        Flags = Binary_ELF.PF_R ||| Binary_ELF.PF_X  // Readable and executable
        Offset = 0UL  // Load from beginning of file (includes headers)
        VAddr = baseVAddr  // Load at base address
        PAddr = baseVAddr  // Physical = virtual for user programs
        FileSize = segmentFileSize  // Includes headers + code
        MemSize = segmentMemSize  // Same as file size
        Align = 0x1000UL  // 4KB alignment (page size)
    }

    let binary : Binary_ELF.ElfBinary = {
        Header = header
        ProgramHeaders = [codeSegment]
        MachineCode = codeBytes
    }

    serializeElf binary

/// Write bytes to file (Linux - no code signing needed)
let writeToFile (path: string) (bytes: byte array) : unit =
    System.IO.File.WriteAllBytes(path, bytes)
    // Make executable using Unix file mode
    let permissions = System.IO.File.GetUnixFileMode(path)
    System.IO.File.SetUnixFileMode(path, permissions ||| System.IO.UnixFileMode.UserExecute)
    // No code signing needed on Linux!
