# Darklang Compiler

A compiler for Darklang written in pure functional F#. Targets native
binaries on ARM64 (macOS + Linux) and x86_64 (Linux). The compiler
detects the host CPU at runtime and emits the correct machine code.

Generates Mach-O (macOS) or ELF (Linux) executables directly — no
external assembler or linker.

## Quick start

```bash
dotnet build             # build the compiler
./run-tests              # run the full test suite
./run-tests --ai         # run the full suite with AI-friendly output
./dark -r -e "2 + 3"     # compile and execute a Dark expression
```

See [`docs/quick-start.md`](docs/quick-start.md) for the full CLI
reference and IR dump flags.

## Development environment

Build and test require .NET 10. The supported environment is the
project's Docker devcontainer — no host installs needed.

- VS Code: "Reopen in Container" uses `.devcontainer/devcontainer.json`.
- CLI: `docker compose up -d` and then
  `scripts/run-in-container ./run-tests --ai` for agent sessions.

See [`docs/docker.md`](docs/docker.md) for details on the container,
Codex/Claude integration, and git worktrees.

## Architecture

The compiler runs an 8-pass pipeline. Passes 1–5 are shared across
targets; passes 6–8 live under `src/DarkCompiler/passes/arm64/` or
`src/DarkCompiler/passes/x64/`.

```
Source → AST → ANF → MIR → LIR → RegAlloc → CodeGen → Encode → Binary
```

See [`docs/architecture.md`](docs/architecture.md) for the design
rationale and [`docs/compiler-passes.md`](docs/compiler-passes.md) for
per-pass details.

## Design principles

- **Pure functional F#** — no mutable state, no exceptions. `Result<_, _>`
  for error handling. Eases eventual self-hosting in Darklang.
- **Multi-stage IR** — each IR makes a specific class of transformations
  straightforward; each pass is testable in isolation.
- **Direct binary generation** — the compiler writes Mach-O / ELF files
  itself, so there's full control over layout and no toolchain drift.
- **TDD** — most features are driven by e2e tests in `src/Tests/e2e/`.
  See [`AGENTS.md`](AGENTS.md) for the workflow agents/contributors
  should follow.

## Current state

See [`TODOs.md`](TODOs.md) for active work and the backlog.

## For AI agents

[`AGENTS.md`](AGENTS.md) is the canonical agent/contributor guide
(conventions, testing rules, commit workflow). [`CLAUDE.md`](CLAUDE.md)
is a thin pointer to it. AI agents should run the suite with
`./run-tests --ai` for compact progress output.
