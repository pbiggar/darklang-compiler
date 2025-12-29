# Binary Generation (Mach-O/ELF)

This document describes how the Dark compiler generates native executables directly,
without using an external assembler or linker.

## Overview

The compiler generates native ARM64 binaries directly:
- **macOS**: Mach-O format (requires code signing)
- **Linux**: ELF format (no signing needed)

This approach eliminates dependencies on external tools (as, ld) and gives full
control over the binary layout.

## Why Direct Binary Generation?

1. **No toolchain dependencies** - Works on any system with just the F# runtime
2. **Faster compilation** - No subprocess spawning for as/ld
3. **Educational value** - Shows exactly how binaries work
4. **Full control** - Precise control over sections, alignment, metadata

## Mach-O Format (macOS)

Implemented in `8_Binary_Generation_MachO.fs`.

### File Structure

```
┌─────────────────────────┐
│ Mach Header (32 bytes)  │ Magic, CPU type, flags
├─────────────────────────┤
│ Load Commands           │ Describe segments/sections
│   - __PAGEZERO          │ 4GB unmapped (null ptr protection)
│   - __TEXT segment      │ Code and constants
│   - __LINKEDIT          │ Symbols (empty for us)
│   - LC_DYLINKER         │ Dynamic linker path
│   - LC_LOAD_DYLIB       │ libSystem.B.dylib
│   - LC_SYMTAB           │ Symbol table (empty)
│   - LC_DYSYMTAB         │ Dynamic symbols (empty)
│   - LC_UUID             │ Unique binary ID
│   - LC_BUILD_VERSION    │ macOS version requirement
│   - LC_MAIN             │ Entry point offset
├─────────────────────────┤
│ Padding                 │ Space for codesign
├─────────────────────────┤
│ __text section          │ Machine code
├─────────────────────────┤
│ __const section         │ Float pool + string pool
└─────────────────────────┘
```

### Key Load Commands

| Command | Purpose |
|---------|---------|
| LC_SEGMENT_64 | Define memory mapping for a segment |
| LC_MAIN | Specify entry point (offset into __TEXT) |
| LC_DYLINKER | Path to dyld (/usr/lib/dyld) |
| LC_LOAD_DYLIB | Required library (libSystem.B.dylib) |
| LC_BUILD_VERSION | Minimum macOS version |
| LC_UUID | Unique identifier for binary |

### Code Signing

macOS requires all executables to be signed. The compiler calls `codesign`:

```fsharp
let signProcess = System.Diagnostics.Process.Start(
    "codesign", $"-s - \"{path}\"")
```

The `-s -` flag performs ad-hoc signing (no certificate needed).

## ELF Format (Linux)

Implemented in `8_Binary_Generation_ELF.fs`.

### File Structure

```
┌─────────────────────────┐
│ ELF Header (64 bytes)   │ Magic, arch, entry point
├─────────────────────────┤
│ Program Headers         │ Describe loadable segments
│   - PT_LOAD             │ Code + data segment
├─────────────────────────┤
│ Machine Code            │ The actual instructions
├─────────────────────────┤
│ Constant Data           │ Float pool + string pool
└─────────────────────────┘
```

### ELF Header Fields

```fsharp
type Elf64Header = {
    Ident: byte array       // Magic: 0x7F 'E' 'L' 'F', class, endian
    Type: uint16            // ET_EXEC (executable)
    Machine: uint16         // EM_AARCH64 (ARM64)
    Entry: uint64           // Entry point virtual address
    PhOff: uint64           // Program header offset
    // ...
}
```

### Program Header

```fsharp
type Elf64ProgramHeader = {
    Type: uint32            // PT_LOAD
    Flags: uint32           // PF_R | PF_X (read + execute)
    Offset: uint64          // File offset
    VAddr: uint64           // Virtual address (0x400000 typical)
    FileSize: uint64        // Size in file
    MemSize: uint64         // Size in memory
    Align: uint64           // 4KB page alignment
}
```

## Memory Layout

### Virtual Address Space

| Platform | Base Address | Notes |
|----------|--------------|-------|
| macOS | 0x100000000 | Above 4GB (PAGEZERO protection) |
| Linux | 0x400000 | Traditional ELF base |

### Constant Data

After machine code, the binary contains:
1. **Float pool** - 8-byte IEEE 754 doubles, indexed by `FloatRef`
2. **String pool** - Null-terminated UTF-8 strings, indexed by `StringRef`

Data is 8-byte aligned for efficient float access.

## Syscall Differences

| Operation | macOS | Linux |
|-----------|-------|-------|
| Syscall number register | X16 | X8 |
| exit | 0x2000001 | 93 |
| write | 0x2000004 | 64 |
| read | 0x2000003 | 63 |

Code generation handles these differences at the MIR/CodeGen level.

## Key Git History

- `03581d8` - Add Linux ELF binary generation alongside macOS Mach-O support
- `ae3d6c0` - Add critical Mach-O load commands for macOS execution
- `18500ee` - Fix binary execution by placing code right after headers
- `d92a27f` - Fix Linux syscall register convention (use X8 instead of X16)

## Implementation Files

| File | Purpose |
|------|---------|
| `8_Binary_Generation_MachO.fs` | Mach-O generation (547 lines) |
| `8_Binary_Generation_ELF.fs` | ELF generation (227 lines) |
| `Binary.fs` | Common types for binary structures |
| `Binary_ELF.fs` | ELF-specific type definitions |

## How It Works

1. **CodeGen** produces `uint32 list` (ARM64 instructions)
2. **Binary generation** wraps instructions in appropriate format:
   - Compute segment sizes and offsets
   - Create headers and load commands
   - Serialize everything to bytes
   - Write to file with execute permission
   - (macOS only) Code sign the binary

## Example: Creating a Binary

```fsharp
// Mach-O
let bytes = createExecutableWithPools machineCode stringPool floatPool
writeToFile path bytes |> Result.bind codeSign

// ELF
let bytes = createExecutableWithPools machineCode stringPool floatPool
writeToFile path bytes  // No signing needed
```
