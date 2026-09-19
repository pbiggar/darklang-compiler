# Quick Start

See the top-level [README](../README.md) for the project overview. This
document is the short CLI reference.

## Build

```bash
./build --ai                       # Bounded output for automated work
./build                            # Human-readable minimal build output
./build --ai -- src/DarkCompiler/DarkCompiler.fsproj
```

Arguments after `--` are passed to `dotnet build`. Complete failed AI build
logs are retained under `TestResults/ai/`.

## Test

`./run-tests` never invokes a .NET/F# project build. Run `./build --ai` first,
and run it again after changing source or project files so the test binary is
current.

```bash
./run-tests --ai                 # Run the already-built full suite with bounded output
./run-tests --ai --target=linux-x86_64 # Explicit x64 suite (QEMU when cross-target)
./run-tests --quiet              # Less output
./run-tests --ai --filter=tuple    # Filter by case-insensitive substring
./run-tests --ai --filter=List.map # Filter by test name fragment
./run-tests --help               # All options
```

Tests follow the selected development target. With no `--target`, the suite
uses the host architecture and does not also run another backend's tests or
benchmarks. Use the explicit x64 target for x64 work; generated Linux x64 E2Es
run through `/opt/dcb/qemu/qemu-x86_64` when the host is ARM64.

Architecture parity is recorded at a particular revision; it does not make
cross-target testing the default for later work. Develop and verify on the
selected target unless the change explicitly includes another architecture.

## Compile and run Dark code

```bash
# Run an expression (compile to temp, execute, print exit code)
./dark -r -e "2 + 3"

# Compile a file to dark.out
./dark prog.dark

# Compile to a specific output path
./dark prog.dark -o output

# Compile independent programs while preparing the standard library once
./dark --batch -q -- first.dark first.out second.dark second.out

# Run a file (compile + exec)
./dark -r prog.dark

# Run the compiled binary directly
./output
echo $?   # exit code
```

Compiled programs can invoke commands with `Stdlib.Cli.execute`, use
argument-vector helpers under `Stdlib.Cli.Process`, discover the runtime host
under `Stdlib.Cli.Host`/`Sys`, and read terminal keys with
`Stdlib.Cli.Stdin.readKey`. These are program effects, separate from `./dark`
launching the produced executable.

## Flags

- Flags can appear in any order: `./dark -o out prog.dark -q` and
  `./dark -q prog.dark -o out` are equivalent.
- Short flags can be combined: `./dark -qr -e "42"` = quiet + run.
- `-r` / `--run`           — compile and execute
- `-e EXPR` / `--expression` — compile an inline expression
- `-o PATH` / `--output`     — output path (default `dark.out`)
- `--package-server URL`     — resolve referenced hosted packages from an HTTP server
- `-q` / `--quiet`           — suppress progress output
- `-v`, `-vv`, `-vvv`        — verbose (pass names, timing, all IRs)

Batch mode accepts one or more `SOURCE OUTPUT` pairs after `--`. Every pair is
compiled and linked independently, but the process reuses one prepared standard
library across all pairs.

Large or labeled batches can use `--manifest FILE`, where `FILE` is a JSON
array of `{ "kind", "name", "source", "output" }` objects. Add
`--keep-going` to attempt later items after a failure and `--report FILE` to
write one JSON result per line. The process prepares the standard library once
for the complete batch.

To audit hosted packages against the compiler, run the catalog-driven helper:

```bash
python3 scripts/compile-packages.py --server http://127.0.0.1:9090 --limit 10
```

The helper writes probe sources and a JSON manifest, then invokes one compiler
batch so the standard library is prepared once. Compilation continues after
individual failures. Complete JSONL results go to
`TestResults/package-compilation.jsonl`; full failure diagnostics go to
`TestResults/package-compilation.log`.

## Dump intermediate representations

```bash
./dark --dump-anf prog.dark    # ANF stages
./dark --dump-mir prog.dark    # MIR CFG
./dark --dump-lir prog.dark    # LIR before and after register allocation
./dark --dump-lir --dump-function=List.map prog.dark
./dark --dump-mir --dump-function=List.map --dump-ir-summary prog.dark
./dark --dump-anf --dump-function=List.map --dump-ir-output=/tmp/list-map.anf prog.dark
./dark -vvv prog.dark          # Dump everything
```

`--dump-function=TEXT` performs a case-insensitive function-name filter before
formatting the selected representations. `--dump-ir-summary` reports only
function, block, and instruction counts. `--dump-ir-output=FILE` writes the
compiler's requested IR output to a file instead of stdout. These modifiers
require `--dump-anf`, `--dump-mir`, `--dump-lir`, or `-vvv`.

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
