# Binary Generation (Mach-O/ELF)

This document describes how the Dark compiler generates native executables directly,
without using an external assembler or linker.

## Overview

The compiler generates native binaries directly:
- **macOS ARM64**: Mach-O format (requires code signing)
- **Linux ARM64**: ELF format (no signing needed)
- **Linux x86-64**: ELF format (no signing needed)

This approach eliminates dependencies on external tools (as, ld) and gives full
control over the binary layout.

## Why Direct Binary Generation?

1. **No toolchain dependencies** - Works with the native compiler executable
2. **Faster compilation** - No subprocess spawning for as/ld
3. **Educational value** - Shows exactly how binaries work
4. **Full control** - Precise control over sections, alignment, metadata

## Mach-O Format (macOS)

Implemented for ARM64 in `src/backend/arm64/Binary_Generation_MachO.ml`.

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

### Segment sizing

The emitter rounds the end of headers, machine code and constant data up to an
ARM64 16 KiB page boundary. Both `__TEXT` file size and virtual size use that
extent, and `__LINKEDIT` begins immediately afterward. Larger programs grow by
whole pages; 16 KiB is the alignment, not a capacity limit. Serializer regressions
check code preservation, section extents and load-command addresses across page
boundaries and with large UTF-8 constant pools.

### Code Signing

macOS requires all executables to be signed. The compiler calls `codesign`:

```ocaml
let process =
  Unix.create_process "codesign" [| "codesign"; "-s"; "-"; path |]
    Unix.stdin stdoutWrite stderrWrite
```

The `-s -` flag performs ad-hoc signing (no certificate needed).

## ELF Format (Linux)

Implemented for ARM64 in `src/backend/arm64/Backend_Arm64_Binary_Generation_ELF.ml`
and for x86-64 in `src/backend/x64/Binary_Generation_ELF_X86_64.ml`.

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

```ocaml
type elf64Header = {ident : bytes; typ : int; machine : int; version : int32; entry : int64; phOff : int64; shOff : int64; flags : int32; ehSize : int; phEntSize : int; phNum : int; shEntSize : int; shNum : int; shStrNdx : int}
```

### Program Header

```ocaml
type elf64ProgramHeader = {typ : int32; flags : int32; offset : int64; vAddr : int64; pAddr : int64; fileSize : int64; memSize : int64; align : int64}
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
3. **Leak counter** - Optional 8-byte `_leak_count` slot when leak checking is enabled

Data is 8-byte aligned for efficient float access.

## Syscall Differences

| Operation | macOS | Linux |
|-----------|-------|-------|
| Syscall number register | X16 | X8 |
| exit | 0x2000001 | 93 |
| write | 0x2000004 | 64 |
| read | 0x2000003 | 63 |

Code generation handles these differences at the MIR/CodeGen level.

## Implementation Files

| File | Purpose |
|------|---------|
| `src/backend/arm64/Binary_Generation_MachO.ml` | ARM64 Mach-O generation |
| `src/backend/arm64/Backend_Arm64_Binary_Generation_ELF.ml` | ARM64 ELF generation |
| `src/backend/x64/Binary_Generation_ELF_X86_64.ml` | x86-64 ELF generation |
| `src/backend/binary/Binary.ml` | Common types for binary structures |
| `src/backend/binary/ELF.ml` | ELF-specific type definitions |

## How It Works

1. **CodeGen** produces a target-specific symbolic instruction list
   (`ARM64Symbolic.Instr list` on ARM64, `X86_64.Instr list` on x64)
2. **Resolve/encoding** turns symbolic instructions into machine code:
   - ARM64 resolves literal pools and labels, then encodes fixed-width
     instructions as `uint32 list`
   - x64 resolves labels and RIP-relative data references, then encodes
     variable-width instructions as bytes
3. **Binary generation** wraps encoded machine code in the target executable
   format:
   - Compute segment sizes and offsets
   - Create headers and load commands
   - Serialize everything to bytes
   - Write to file with execute permission
   - (macOS only) Code sign the binary

## Example: Creating a Binary

```text
Mach-O: encode code and literal pools, write the binary, then sign it.
ARM64 ELF: encode code and literal pools, then write the binary.
x86-64 ELF: encode code and literal pools with the entry offset, then write the binary.
```
