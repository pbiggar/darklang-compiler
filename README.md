# Darklang Compiler

A compiler for Darklang written in pure functional F# that targets ARM64 (macOS and Linux) and x86_64 (Linux). The compiler auto-detects the host CPU and emits the correct native code.

## Quick Start

**Build:**

```bash
dotnet build
```

**Run tests:**

```bash
./run-tests
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

# Verbose output (shows pass names)
./dark -v prog.dark -o output

# Dump specific IRs
./dark --dump-anf prog.dark
./dark --dump-mir prog.dark
./dark --dump-lir prog.dark

# Dump all IRs
./dark -vvv prog.dark

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

**Run specific tests:**

```bash
# Filter by pattern (case-insensitive substring match)
./run-tests --filter=tuple       # Run tuple tests
./run-tests --filter=string      # Run string tests
./run-tests --filter=List.map    # Run List.map tests

# Combine options
./run-tests --filter=list

# Other options
./run-tests --help               # Show all options
./run-tests --build-only         # Just build, don't run
./run-tests                      # Build and run tests
```

**Check binary structure (macOS):**

```bash
otool -l <binary>           # Show load commands
otool -tv <binary>          # Disassemble text section
file <binary>               # Check file type
```

## Compiler Library API (internal)

The library surface in `src/DarkCompiler/CompilerLibrary.fs` is intentionally small:

- `buildStdlib` to prebuild stdlib for tests and tooling
- `buildStdlibSpecializations` to prebuild suite-level stdlib specializations
- `buildPreambleContext` for ad-hoc preamble build/reuse
- `analyzePreamble` + `buildPreambleContextFromAnalysis` for suite-level preamble specialization
- `compile` for in-memory compilation via `CompileRequest` (`CompileContext` + `CompileMode`)
- `execute` for running compiled binaries with timing

## Docker Development (with Codex + Claude Code Integration)

### Initial Setup

```bash
# Build container image (includes Codex CLI + Claude Code)
./docker.sh build

# Start container
./docker.sh up

# From main/
./docker.sh shell

# From wt-1/ or wt-2/, run the same command via the shared script
../main/docker.sh shell
```

The `~/projects/c4d` parent directory is bind-mounted into the container at `/workspace`, so the worktrees are available at `/workspace/main`, `/workspace/wt-1`, and `/workspace/wt-2`. The shell runs as the non-root `dark` user. Each worktree's top-level `bin/` and `obj/` directories are overlaid with Docker volumes so build artifacts stay inside the container.

### Using Codex Inside Container

**First time setup - Authenticate:**
```bash
# Enter container from the current worktree directory
./docker.sh shell

# Inside container - authenticate Codex
codex login
# Follow prompts to enter your API key

# Start Codex in the matching container directory
codex
```

**Subsequent sessions:**
```bash
./docker.sh shell
codex

# From wt-1/ or wt-2/
../main/docker.sh shell
codex
```

Your Codex configuration, conversation history, and session memory are persisted in the Docker-managed `codex-home` volume mounted at `~/.codex`.

### Using Claude Code Inside Container

**First time setup - Authenticate:**
```bash
# Enter container
./docker.sh shell

# Inside container - authenticate Claude Code
claude login
# Follow prompts to enter your API key
```

**Subsequent sessions:**
```bash
./docker.sh shell
claude

# From wt-2/
../main/docker.sh shell
claude
```

**Once-off install (if you are already inside a running container):**
```bash
npm install -g @anthropic-ai/claude-code
```

Your Claude configuration and session data are persisted in the Docker-managed `claude-home` volume mounted at `~/.claude`.

### Development Workflow

**Option A: Using Codex inside container (recommended for full sandboxing)**
```bash
./docker.sh shell              # Opens the matching /workspace/... path
codex  # Start Codex session
# Work with Codex interactively in the sandboxed environment
```

**Option B: Manual development**
```bash
# Inside container
dotnet build                    # Build compiler in the current worktree
dotnet clean                    # Clean build artifacts in the current worktree

# On host (macOS)
# Edit source files with your normal editor
# Changes are immediately reflected in container via volume mount
```

### Container Commands

```bash
./docker.sh build           # Build Docker image
./docker.sh up              # Start container
./docker.sh down            # Stop container
./docker.sh shell           # Enter the matching c4d path
./docker.sh restart         # Restart container
./docker.sh build-compiler  # Build compiler in the matching c4d path
./docker.sh status          # Show container status
```

### What's Included

- ✅ Codex CLI pre-installed
- ✅ Claude Code pre-installed
- ✅ Build compiler DLL in container
- ✅ Non-root `dark` shell by default
- ✅ Bind mount for `main`, `wt-1`, and `wt-2` through one parent mount
- ✅ Separate Docker volumes for `bin/` and `obj/` in each worktree
- ✅ Codex config/history persisted in a Docker volume
- ✅ Claude config/history persisted in a Docker volume
- ✅ NuGet packages persisted in a Docker volume
- ✅ Full filesystem isolation and sandboxing
- ✅ Run all tests in container (generates Linux ELF binaries)

## Current Features

See `docs/current-language-features.md` for the full, up-to-date language reference.

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
