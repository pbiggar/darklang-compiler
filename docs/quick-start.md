# Quick Start

See the top-level [README](../README.md) for the project overview. This
document is the short CLI reference.

## Build

```bash
dotnet build
```

## Test

```bash
./run-tests                      # Build and run the full suite
./run-tests --ai                 # AI-friendly progress output
./run-tests --quiet              # Less output
./run-tests --filter=tuple       # Filter by case-insensitive substring
./run-tests --filter=List.map    # Filter by test name fragment
./run-tests --build-only         # Just build, don't run
./run-tests --help               # All options
```

## Compile and run Dark code

```bash
# Run an expression (compile to temp, execute, print exit code)
./dark -r -e "2 + 3"

# Compile a file to dark.out
./dark prog.dark

# Compile to a specific output path
./dark prog.dark -o output

# Run a file (compile + exec)
./dark -r prog.dark

# Run the compiled binary directly
./output
echo $?   # exit code
```

## Flags

- Flags can appear in any order: `./dark -o out prog.dark -q` and
  `./dark -q prog.dark -o out` are equivalent.
- Short flags can be combined: `./dark -qr -e "42"` = quiet + run.
- `-r` / `--run`           — compile and execute
- `-e EXPR` / `--expression` — compile an inline expression
- `-o PATH` / `--output`     — output path (default `dark.out`)
- `-q` / `--quiet`           — suppress progress output
- `-v`, `-vv`, `-vvv`        — verbose (pass names, timing, all IRs)

## Dump intermediate representations

```bash
./dark --dump-anf prog.dark    # ANF stages
./dark --dump-mir prog.dark    # MIR CFG
./dark --dump-lir prog.dark    # LIR before and after register allocation
./dark -vvv prog.dark          # Dump everything
```

## Clean

```bash
dotnet clean
# Or manually: rm -rf obj bin
```

## Inspecting produced binaries

```bash
# Linux ELF
readelf -a ./output
objdump -d ./output

# macOS Mach-O
otool -l ./output
otool -tv ./output

# Either
file ./output
xxd ./output | head
```
