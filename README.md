# Darklang Compiler

A pure-functional F# compiler for Darklang. It emits native ARM64 (macOS and
Linux) and Linux x86_64 binaries directly, without an external assembler or
linker.

Start with the [documentation index](docs/index.md). It routes CLI use,
development environment, contributor workflow, verification, architecture,
features, compatibility, benchmarks, and agent guidance to their canonical
sources.

The compiler runs an eight-pass pipeline from source to native binary. See
[`docs/architecture.md`](docs/architecture.md) and
[`docs/compiler-passes.md`](docs/compiler-passes.md) for its design and pass
contracts.
