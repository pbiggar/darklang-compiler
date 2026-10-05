# Darklang Compiler

A native OCaml compiler for Darklang. It emits native ARM64 (macOS and
Linux) and Linux x86_64 binaries directly, without an external assembler or
linker.

Start with the [documentation index](docs/index.md). It routes CLI use,
development environment, contributor workflow, verification, architecture,
features, compatibility, benchmarks, and agent guidance to their canonical
sources.

The compiler runs an eight-pass pipeline from source to native binary. See the
[compiler overview](docs/compiler/overview.md) and
[pipeline reference](docs/compiler/pipeline.md) for its design and pass
contracts.
