# Darklang Compiler Documentation

This directory contains documentation for the Darklang compiler, a from-scratch compiler written in F# that targets ARM64 macOS.

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

### Design Principles

1. **Pure Functional F#**: The compiler is written in pure functional F# without mutable state or imperative features. This makes it easier to eventually port to self-hosted Darklang.

2. **Multi-Stage Pipeline**: The compiler uses multiple intermediate representations (IRs), each serving a specific purpose and making the compilation process modular and testable.

3. **Test-Driven Development**: Each phase has comprehensive unit tests. Tests are written using FsUnit with NUnit.

4. **Direct Binary Generation**: The compiler generates Mach-O binaries directly without relying on external assemblers or linkers.

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
│   ├── DarkCompiler/       # Main compiler code
│   │   ├── AST.fs          # Abstract Syntax Tree definitions
│   │   ├── Parser.fs       # Lexer and parser
│   │   ├── ANF.fs          # A-Normal Form transformation
│   │   ├── MIR.fs          # Mid-level IR
│   │   ├── LIR.fs          # Low-level IR and register allocation
│   │   ├── ARM64.fs        # ARM64 instruction encoding
│   │   ├── CodeGen.fs      # LIR to ARM64 code generation
│   │   ├── Binary.fs       # Mach-O binary generation
│   │   └── Program.fs      # CLI entry point
│   └── Tests/              # Test suite
├── obj/                    # Build intermediate files
├── bin/                    # Build output
└── docs/                   # Documentation
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
