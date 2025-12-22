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

**Quick test an expression:**

```bash
# Run an expression (compile to temp and execute)
./dark -r -e "2 + 3"
# Output: Exit code: 5

./dark --run --expression "6 * 7"
# Output: Exit code: 42

# Quiet run (combined flags)
./dark -qr -e "10 + 32"
# Output: (just exit code: 42)
```

**Compile a file:**

```bash
# Compile file to default output (dark.out)
./dark prog.dark

# Compile file to specific output
./dark prog.dark -o output

# Run file
./dark -r prog.dark

# Run the compiled binary
./output
echo $?  # Shows exit code
```

**Compile an expression:**

```bash
# Compile expression to default file (dark.out)
./dark -e "2 + 3"

# Compile expression to file
./dark -e "6 * 7" -o output

# Quiet compile
./dark -q -e "2 + 3" -o output
```

**Other features:**

```bash
# Flags can appear in any order
./dark -o output prog.dark -q
./dark -q -o output prog.dark
# Both are equivalent

# Verbose output (shows detailed IR)
./dark -v prog.dark -o output

# Read from stdin
./dark -r -e - < input.txt

# Combined short flags
./dark -qr -e "42"    # quiet + run
./dark -re "5"        # run + expression

# Help and version
./dark -h
./dark --version
```

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

## Current Features

**Language:**

- Integer literals (64-bit signed)
- Arithmetic operators: `+`, `-`, `*`, `/`
- Operator precedence (multiplication/division before addition/subtraction)
- Left-associative operators
- Parentheses for grouping

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
