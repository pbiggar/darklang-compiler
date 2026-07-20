// 8_Binary_Generation_ELF.fs - ELF Binary Generation (Pass 8, Linux variant)
//
// Generates a complete ELF executable from ARM64 machine code for Linux.
// This is a direct binary generator - no assembler or linker needed.
//
// File structure:
//   [ELF Header]         - Magic (0x7F 'ELF'), architecture, entry point
//   [Program Header]     - Describes loadable segment (PT_LOAD)
//   [Machine Code]       - ARM64 instructions
//   [Constant Data]      - Float pool + string pool (8-byte aligned)
//
// Memory layout:
//   - Base address: 0x400000 (traditional ELF user-space address)
//   - Single PT_LOAD segment: headers + code + data
//   - Flags: PF_R | PF_X (readable and executable)
//
// No code signing needed on Linux (unlike macOS).
//
// See docs/features/binary-generation.md for detailed documentation.

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

/// Convert machine code words to little-endian bytes without per-word arrays
let private machineCodeToBytes (machineCode: uint32 list) : byte array =
    let bytes = Array.zeroCreate (List.length machineCode * 4)
    let rec writeWords offset remaining =
        match remaining with
        | [] -> bytes
        | word :: rest ->
            bytes.[offset] <- byte (word &&& 0xFFu)
            bytes.[offset + 1] <- byte ((word >>> 8) &&& 0xFFu)
            bytes.[offset + 2] <- byte ((word >>> 16) &&& 0xFFu)
            bytes.[offset + 3] <- byte ((word >>> 24) &&& 0xFFu)
            writeWords (offset + 4) rest
    writeWords 0 machineCode

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
/// Adds alignment padding between code and data for 8-byte alignment
let serializeElf (binary: Binary_ELF.ElfBinary) : byte array =
    // Calculate alignment padding needed after code
    let headerSize = 64 + (56 * binary.ProgramHeaders.Length)
    let codeEnd = headerSize + binary.MachineCode.Length
    let alignedDataStart = (codeEnd + 7) &&& (~~~7)
    let alignmentPadding = Array.create (alignedDataStart - codeEnd) 0uy
    [|
        yield! serializeElf64Header binary.Header
        for ph in binary.ProgramHeaders do
            yield! serializeElf64ProgramHeader ph
        yield! binary.MachineCode
        yield! alignmentPadding  // Align to 8 bytes before float/string data
        yield! binary.StringData
    |]

/// Create float data bytes from float pool
let createFloatData (floatPool: LiteralPool.FloatPool) : byte array =
    if floatPool.Floats.IsEmpty then
        [||]
    else
        // Sort by index and collect all float bytes
        floatPool.Floats
        |> Map.toList
        |> List.sortBy fst
        |> List.map (fun (_idx, floatVal) ->
            System.BitConverter.GetBytes(floatVal))
        |> Array.ofList
        |> Array.concat

/// Create string data bytes from string pool
/// Format: [length:8 bytes][data:N bytes][padding:P][refcount:8 bytes] for each string.
/// Literal strings use INT64_MAX in the refcount slot so shared string RC code can skip them.
let createStringData (stringPool: LiteralPool.StringPool) : byte array =
    if stringPool.Strings.IsEmpty then
        [||]
    else
        // Sort by index and collect all string bytes with length prefix
        stringPool.Strings
        |> Map.toList
        |> List.sortBy fst
        |> List.map (fun (_idx, (str, len)) ->
            let lenBytes = uint64ToBytes (uint64 len)  // 8-byte length
            let strBytes = System.Text.Encoding.UTF8.GetBytes(str)
            let alignedLen = ((len + 7) / 8) * 8
            let padding = Array.zeroCreate (alignedLen - len)
            let sentinel = System.BitConverter.GetBytes(System.Int64.MaxValue)
            Array.concat [| lenBytes; strBytes; padding; sentinel |])
        |> Array.ofList
        |> Array.concat

let private elfHeaderSize = 64UL
let private programHeaderSize = 56UL
let private numProgramHeaders = 1us
let private baseVAddr = 0x400000UL

let private codeFileOffset =
    elfHeaderSize + (uint64 numProgramHeaders * programHeaderSize)

let private createElfHeader (entryVAddr: uint64) : Binary_ELF.Elf64Header =
    {
        Ident = Binary_ELF.createIdent ()
        Type = Binary_ELF.ET_EXEC
        Machine = Binary_ELF.EM_AARCH64
        Version = 1u
        Entry = entryVAddr
        PhOff = elfHeaderSize
        ShOff = 0UL
        Flags = 0u
        EhSize = uint16 elfHeaderSize
        PhEntSize = uint16 programHeaderSize
        PhNum = numProgramHeaders
        ShEntSize = 0us
        ShNum = 0us
        ShStrNdx = 0us
    }

let private createLoadSegment
    (codeSize: uint64)
    (dataSize: uint64)
    (segmentFlags: uint32)
    : Binary_ELF.Elf64ProgramHeader =
    let alignedDataOffset = (codeFileOffset + codeSize + 7UL) &&& (~~~7UL)
    let alignmentPadding = alignedDataOffset - (codeFileOffset + codeSize)
    let segmentFileSize = codeFileOffset + codeSize + alignmentPadding + dataSize

    {
        Type = Binary_ELF.PT_LOAD
        Flags = segmentFlags
        Offset = 0UL
        VAddr = baseVAddr
        PAddr = baseVAddr
        FileSize = segmentFileSize
        MemSize = segmentFileSize
        Align = 0x1000UL
    }

let private codeEntryVAddr =
    baseVAddr + codeFileOffset

let private executableSegmentFlags (enableLeakCheck: bool) : uint32 =
    if enableLeakCheck then
        Binary_ELF.PF_R ||| Binary_ELF.PF_W ||| Binary_ELF.PF_X
    else
        Binary_ELF.PF_R ||| Binary_ELF.PF_X

let private createBinary
    (codeBytes: byte array)
    (dataBytes: byte array)
    (segmentFlags: uint32)
    : Binary_ELF.ElfBinary =
    {
        Header = createElfHeader codeEntryVAddr
        ProgramHeaders =
            [createLoadSegment (uint64 codeBytes.Length) (uint64 dataBytes.Length) segmentFlags]
        MachineCode = codeBytes
        StringData = dataBytes
    }

/// Create an ELF executable with float and string data
let createExecutableWithPools
    (machineCode: uint32 list)
    (stringPool: LiteralPool.StringPool)
    (floatPool: LiteralPool.FloatPool)
    (enableLeakCheck: bool)
    : byte array =
    let codeBytes =
        machineCodeToBytes machineCode

    // Create float data (goes after code, before strings)
    let floatBytes =
        createFloatData floatPool

    // Create string data
    let stringBytes =
        createStringData stringPool

    let dataBytes =
        let floatAndStringBytes = Array.append floatBytes stringBytes
        let leakBytes = if enableLeakCheck then Array.create 8 0uy else [||]
        let leakStart = ((floatAndStringBytes.Length + 7) / 8) * 8
        let leakPadding = Array.create (leakStart - floatAndStringBytes.Length) 0uy
        if enableLeakCheck then
            Array.concat [floatAndStringBytes; leakPadding; leakBytes]
        else
            floatAndStringBytes

    createBinary codeBytes dataBytes (executableSegmentFlags enableLeakCheck)
    |> serializeElf

/// Create an ELF executable with string data (legacy wrapper for backwards compatibility)
let createExecutableWithStrings (machineCode: uint32 list) (stringPool: LiteralPool.StringPool) : byte array =
    createExecutableWithPools machineCode stringPool LiteralPool.emptyFloatPool false

/// Create a minimal ELF executable from ARM64 machine code (legacy, no data)
let createExecutable (machineCode: uint32 list) : byte array =
    createExecutableWithPools machineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false

/// Create an ELF executable with coverage data section
/// coverageExprCount: number of coverage expressions (each needs 8 bytes)
/// The coverage data is placed after strings and initialized to zero
/// Uses a single RWX segment for simplicity (code + data + coverage)
let createExecutableWithCoverage (machineCode: uint32 list) (stringPool: LiteralPool.StringPool) (floatPool: LiteralPool.FloatPool) (coverageExprCount: int) (enableLeakCheck: bool) : byte array =
    let codeBytes =
        machineCodeToBytes machineCode

    // Create float data (goes after code, before strings)
    let floatBytes = createFloatData floatPool

    // Create string data
    let stringBytes = createStringData stringPool

    // Create coverage data (zeros, 8 bytes per expression, 8-byte aligned)
    let coverageSize = ((coverageExprCount * 8 + 7) / 8) * 8
    let coverageBytes = Array.create coverageSize 0uy

    let floatAndStringBytes = Array.append floatBytes stringBytes
    let alignedCoverageStart = ((floatAndStringBytes.Length + 7) / 8) * 8
    let coveragePadding = Array.create (alignedCoverageStart - floatAndStringBytes.Length) 0uy
    let afterCoverage = alignedCoverageStart + coverageBytes.Length
    let leakBytes = if enableLeakCheck then Array.create 8 0uy else [||]
    let leakStart = ((afterCoverage + 7) / 8) * 8
    let leakPadding = Array.create (leakStart - afterCoverage) 0uy
    let dataBytes =
        if enableLeakCheck then
            Array.concat [floatAndStringBytes; coveragePadding; coverageBytes; leakPadding; leakBytes]
        else
            Array.concat [floatAndStringBytes; coveragePadding; coverageBytes]

    // Single segment: RWX (code + read-only data + coverage data)
    // Note: RWX is not ideal for security but simplifies the implementation
    createBinary codeBytes dataBytes (Binary_ELF.PF_R ||| Binary_ELF.PF_W ||| Binary_ELF.PF_X)
    |> serializeElf

/// Write bytes to file (Linux - no code signing needed)
let writeToFile (path: string) (bytes: byte array) : Result<unit, string> =
    System.IO.File.WriteAllBytes(path, bytes)
    // Make executable using Unix file mode
    let permissions = System.IO.File.GetUnixFileMode(path)
    System.IO.File.SetUnixFileMode(path, permissions ||| System.IO.UnixFileMode.UserExecute)
    // No code signing needed on Linux!
    Ok ()
