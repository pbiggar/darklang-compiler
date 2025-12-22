# Darklang Compiler Documentation

## Overview

The Darklang compiler is a multi-stage compiler that transforms source code through several intermediate representations before generating native ARM64 machine code and packaging it into a Mach-O executable.

## Current Status

**Language Features:**

- Integer literals (64-bit signed)
- Arithmetic operators: `+`, `-`, `*`, `/`
- Proper operator precedence (multiplication/division before addition/subtraction)
- Left-associative operators

**Example Program:**

```
2 + 3 * 4
```

This compiles to an ARM64 executable that exits with code 14 (the result of the computation).

## Architecture

## Compilation Pipeline

The compiler transforms code through the following stages:

```
Source Code
    ↓
  Parser
    ↓
   AST (Abstract Syntax Tree)
    ↓
   ANF (A-Normal Form)
    ↓
   MIR (Mid-level IR)
    ↓
   LIR (Low-level IR)
    ↓
Register Allocation
    ↓
ARM64 Instructions
    ↓
Machine Code
    ↓
Mach-O Binary
```

### Stage Details

For detailed information about each compilation stage, see:

- [Compilation Pipeline](pipeline.md) - In-depth explanation of each stage
- [Architecture Decisions](architecture.md) - Design choices and rationale

## Project Structure

```
.
├── src/
│   ├── DarkCompiler/           # Main compiler code
│   │   ├── AST.fs              # Abstract Syntax Tree types
│   │   ├── ANF.fs              # A-Normal Form types
│   │   ├── MIR.fs              # Mid-level IR types
│   │   ├── LIR.fs              # Low-level IR types
│   │   ├── ARM64.fs            # ARM64 instruction types
│   │   ├── Binary.fs           # Mach-O binary format types
│   │   ├── passes/             # Compiler transformation passes
│   │   │   ├── 1_Parser.fs
│   │   │   ├── 2_AST_to_ANF.fs
│   │   │   ├── 3_ANF_to_MIR.fs
│   │   │   ├── 4_MIR_to_LIR.fs
│   │   │   ├── 5_RegisterAllocation.fs
│   │   │   ├── 6_CodeGen.fs
│   │   │   ├── 7_ARM64_Encoding.fs
│   │   │   └── 8_Binary_Generation.fs
│   │   └── Program.fs          # CLI entry point
│   └── Tests/                  # Test suite
├── obj/                        # Build intermediate files
├── bin/                        # Build output
└── docs/                       # Documentation
```

## Testing

Run tests with:

```bash
dotnet test
```

The test suite includes:

- Unit tests for each compiler phase
- End-to-end integration tests that compile complete programs
- Binary format validation tests

## Building

Build the compiler:

```bash
dotnet build
```

Run the compiler:

```bash
dotnet run --project src/DarkCompiler/DarkCompiler.fsproj -- <source-file> -o <output-file>
```

## Next Steps

Future enhancements planned:

- Variables and let bindings
- Functions and function calls
- Control flow (if/else)
- More data types (booleans, strings)
- Standard library
