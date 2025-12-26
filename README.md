# Darklang Compiler

A compiler for Darklang written in pure functional F# that targets ARM64 (macOS and Linux).

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

## Docker Development (with Claude Code Integration)

### Initial Setup

```bash
# Build container image (includes Claude Code CLI)
./docker.sh build

# Start container
./docker.sh up

# Enter container
./docker.sh shell
```

### Using Claude Code Inside Container

**First time setup - Authenticate:**
```bash
# Enter container
./docker.sh shell

# Inside container - authenticate Claude Code
claude login
# Follow prompts to enter your API key

# Start Claude Code in /workspace
cd /workspace
claude
```

**Subsequent sessions:**
```bash
./docker.sh shell
cd /workspace
claude
```

Your Claude Code configuration, conversation history, and session memory are persisted via volume mount at `~/.claude`.

### Development Workflow

**Option A: Using Claude Code inside container (recommended for full sandboxing)**
```bash
./docker.sh shell
claude  # Start Claude Code session
# Work with Claude Code interactively in the sandboxed environment
```

**Option B: Manual development**
```bash
# Inside container
dotnet build                    # Build compiler
dotnet clean                    # Clean build artifacts

# On host (macOS)
# Edit source files with your normal editor
# Changes are immediately reflected in container via volume mount
```

### Container Commands

```bash
./docker.sh build           # Build Docker image
./docker.sh up              # Start container
./docker.sh down            # Stop container
./docker.sh shell           # Enter container shell
./docker.sh restart         # Restart container
./docker.sh build-compiler  # Build compiler in container
./docker.sh status          # Show container status
```

### What's Included

- ✅ Claude Code CLI pre-installed
- ✅ Build compiler DLL in container
- ✅ Volume mount for source code (edit on host or in container)
- ✅ Claude Code config/history persisted via volume mount
- ✅ Full filesystem isolation and sandboxing
- ✅ Run all tests in container (generates Linux ELF binaries)

## Current Features

**Types:**

- `int` - 64-bit signed integers
- `bool` - Boolean values (true/false)
- `float` - 64-bit floating-point numbers
- `string` - String literals with escape sequences
- Tuples - `(int, bool)`, `(int, int, int)`, etc.
- Records - `type Point = { x: int, y: int }`
- Algebraic Data Types - `type Option = None | Some of int`
- Lists - `[1, 2, 3]` (linked list implementation)

**Expressions:**

- Integer literals: `42`, `-17`, `0`
- Float literals: `3.14`, `-0.5`
- Boolean literals: `true`, `false`
- String literals: `"hello"`, `"with\nescape"`
- Arithmetic: `+`, `-`, `*`, `/`
- Comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical operators: `&&`, `||`, `!`
- Parentheses for grouping
- Tuple construction: `(1, 2)`, `(a, b, c)`
- Record construction: `Point { x = 3, y = 4 }` (type name required)
- List literals: `[1, 2, 3]`, `[]`
- ADT constructors: `Some(42)`, `None`

**Control Flow:**

- Let bindings: `let x = 5 in x + 1`
- Let-pattern matching: `let (a, b) = tuple in a + b`
- If expressions: `if x > 0 then x else -x`
- Pattern matching:
  ```
  match expr with
  | pattern1 -> result1
  | pattern2 -> result2
  ```

**Patterns:**

- Literal patterns: `42`, `true`, `"hello"`
- Variable patterns: `x`, `_` (wildcard)
- Tuple patterns: `(a, b)`, `(x, _, z)`
- Record patterns: `Point { x = a, y = b }` (type name required)
- ADT patterns: `Some(n)`, `None`
- List patterns: `[a, b]`, `[]`, `[_, x, _]` (exact-length matching)

**Functions:**

- Function definitions with type signatures:
  ```
  def add(x: int, y: int) : int = x + y
  ```
- Recursion support
- Up to 8 parameters (ARM64 calling convention)

**Examples:**

```
// Factorial
def factorial(n: int) : int =
  if n <= 1 then 1
  else n * factorial(n - 1)

def main() : int = factorial(5)

// Pattern matching on ADT
type Option = None | Some of int

def main() : int =
  match Some(42) with
  | Some(x) -> x * 2
  | None -> 0

// List processing (exact-length matching)
def main() : int =
  match [1, 2, 3] with
  | [a, b, c] -> a + b + c  // matches exactly 3 elements
  | _ -> 0
```

## Compiler Architecture

The compiler uses an 8-pass pipeline:

```
Source → Parser → TypeCheck → ANF → MIR → LIR → RegAlloc → CodeGen → ARM64Enc → Binary
```

| Pass | Input | Output | Purpose |
|------|-------|--------|---------|
| 1. Parser | Source text | AST | Parse syntax into abstract syntax tree |
| 1.5. TypeCheck | AST | Typed AST | Verify types and infer where needed |
| 2. AST→ANF | AST | ANF | A-Normal Form - flatten expressions |
| 3. ANF→MIR | ANF | MIR | Mid-level IR with virtual registers and CFG |
| 4. MIR→LIR | MIR | LIR | Low-level IR close to machine instructions |
| 5. RegAlloc | LIR | LIR | Allocate physical registers, handle spilling |
| 6. CodeGen | LIR | ARM64 | Generate ARM64 assembly instructions |
| 7. ARM64Enc | ARM64 | bytes | Encode instructions to machine code |
| 8. BinaryGen | bytes | executable | Generate Mach-O (macOS) or ELF (Linux) |

## Key Design Principles

### Pure Functional F#

- No mutable state or imperative features
- Result types for error handling (no exceptions)
- Makes future self-hosting in Darklang easier

### Multi-Stage IR Pipeline

- Each IR focuses on specific concerns
- Testable in isolation
- Easy to add new target architectures

### Direct Binary Generation

- Generates Mach-O (macOS) or ELF (Linux) executables directly
- No external assembler/linker required
- Complete control over output
- Cross-platform: same compiler works on macOS and Linux

### Test-Driven Development

- 711 tests covering all language features
- DSL-based E2E tests for quick iteration
- Unit tests for each compiler phase
- Tests document expected behavior

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
