# Darklang Compiler

A compiler for Darklang written in pure functional F# that targets ARM64 macOS.

## Quick Start

**Build:**

```bash
dotnet build
```

**Run tests:**

```bash
dotnet test
```

**Compile a program:**

```bash
dotnet run --project src/DarkCompiler/DarkCompiler.fsproj -- <source-file> -o <output-file>
```

## Current Features

**Language:**

- Integer literals (64-bit signed)
- Arithmetic operators: `+`, `-`, `*`, `/`
- Operator precedence (multiplication/division before addition/subtraction)
- Left-associative operators
- Parentheses for grouping

**Example program:**

```
2 + 3 * 4
```

Compiles to an ARM64 executable that exits with code 14.

## Compilation Pipeline

```
Source Code → Parser → AST → ANF → MIR → LIR → Register Allocation → ARM64 → Machine Code → Mach-O Binary
```

### Example Trace: `2 + 3 * 4`

```
AST:    BinOp(Add, IntLiteral(2), BinOp(Mul, IntLiteral(3), IntLiteral(4)))

ANF:    let tmp0 = 3 * 4
        let tmp1 = 2 + tmp0
        return tmp1

MIR:    v0 <- 3
        v1 <- 4
        v2 <- v0 * v1
        v3 <- 2
        v4 <- v3 + v2
        ret v4

LIR     X0 <- Mov(Imm 3)
(alloc): X1 <- Mov(Imm 4)
        X2 <- Mul(X0, X1)
        X3 <- Mov(Imm 2)
        X4 <- Add(X3, X2)
        ret X4

ARM64:  MOVZ X0, #3, LSL #0
        MOVZ X1, #4, LSL #0
        MUL X2, X0, X1
        MOVZ X3, #2, LSL #0
        ADD X4, X3, X2
        MOV X0, X4
        RET

Result: Executable exits with code 14
```

## Key Design Principles

### Pure Functional F#

- No mutable state or imperative features
- Makes future self-hosting in Darklang easier

### Multi-Stage IR Pipeline

- Each IR focuses on specific concerns
- Testable in isolation
- Easy to add new target architectures

### Direct Binary Generation

- Generates Mach-O executables directly
- No external assembler/linker required
- Complete control over output

### Test-Driven Development

- Unit tests for each phase
- End-to-end integration tests
- Tests document behavior

## Architecture Details

### ANF Transformation

ANF (A-Normal Form) flattens nested expressions by introducing temporary variables:

```
Before: (2 + 3) * 4
After:  let tmp0 = 2 + 3
        let tmp1 = tmp0 * 4
        return tmp1
```

All operands become simple (variables or literals), simplifying code generation.

### Register Allocation

Simple greedy allocator:

1. Collect all virtual registers
2. Assign physical registers (X0-X15) in order
3. Fail if more than 16 registers needed (no spilling yet)

### Mach-O Binary Format

Generated executables have:

- **Header**: Identifies ARM64 executable
- **\_\_PAGEZERO**: 4GB unmapped memory (security)
- **\_\_TEXT segment**: Contains `.text` section with code
- **LC_MAIN**: Entry point specification

Memory layout:

```
0x0000000000000000  __PAGEZERO (4GB unmapped)
0x0000000100000000  __TEXT segment
0x0000000100004000  .text section (code starts here)
```

## Testing

Test framework: FsUnit with NUnit

**Test types:**

- Unit tests for individual functions
- Phase tests for complete transformations
- Integration tests for multi-stage pipelines
- End-to-end smoke tests (compile → execute → verify)
- Binary format validation tests

**Run tests:**

```bash
dotnet test
```

## Build System

Uses .NET 9.0 with MSBuild.

**Build output structure:**

- `obj/DarkCompiler/` - Intermediate files for compiler
- `obj/Tests/` - Intermediate files for tests
- `bin/DarkCompiler/` - Compiled compiler binary
- `bin/Tests/` - Compiled test binary

Configured via `src/Directory.Build.props` to centralize build artifacts.

## Development Workflow

1. **Write tests first** (or alongside implementation)
2. **Implement functionality** to pass tests
3. **Make small, focused commits**
4. **Keep it simple** - no premature optimization or abstraction

Example commit workflow:

```bash
# Make changes
git add <files>
git commit -m "Brief description

Detailed explanation of what and why.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

## Implementation Notes

### Parser

- Recursive descent parser
- Left-associative operator handling via `parseAdditiveRest`/`parseMultiplicativeRest`
- Proper precedence: `*` and `/` bind tighter than `+` and `-`

### ARM64 Encoding

- Fixed 32-bit instruction width (RISC)
- Registers encoded in 5-bit fields
- `MOVZ` for loading small immediates (16-bit)
- `ADD_imm` supports 12-bit unsigned immediates
- `RET` defaults to X30 (link register)

### Current Limitations

- No variables or let bindings (yet)
- No functions or function calls (yet)
- No control flow (yet)
- Only integer arithmetic
- Register allocator fails if >16 registers needed (no spilling)
- Large immediates only partially supported

### Test Files

All test files follow naming convention `<Module>Tests.fs`:

- `ASTTests.fs`, `ParserTests.fs`, `ANFTests.fs`, etc.
- `SmokeTests.fs` - End-to-end integration tests

## Helpful Commands

**Clean build artifacts:**

```bash
dotnet clean
# or manually: rm -rf obj bin
```

**Rebuild everything:**

```bash
dotnet clean && dotnet build
```

**Run specific test:**

```bash
dotnet test --filter "TestName~<pattern>"
```

**Check binary structure (macOS):**

```bash
otool -l <binary>           # Show load commands
otool -tv <binary>          # Disassemble text section
file <binary>               # Check file type
```

**Execute compiled program:**

```bash
./output
echo $?                     # Show exit code
```

## Common Issues

**Build hangs:**

- Kill with Ctrl+C
- Run `dotnet clean`
- Try again

**Tests fail after changes:**

- Verify all stages are updated consistently
- Check register allocation hasn't exceeded 16 registers
- Ensure instruction encoding matches ARM64 spec

**Binary doesn't run:**

- Verify Mach-O magic number: `xxd <binary> | head -1` should show `cffa edfe`
- Check entry point is set correctly
- Verify code section has correct permissions (R-X)

## Resources

**ARM64 Architecture:**

- ARM Architecture Reference Manual (ARMv8)
- ARM64 instruction encoding tables

**Mach-O Format:**

- Apple's Mach-O file format reference
- `otool` for inspection
- `MachOView` for graphical inspection
