# Dark Compiler - AI Agent Guidelines

Read [`docs/index.md`](docs/index.md) first. It owns navigation; this file
contains only rules specific to agents changing this repository.

## F# conventions

- Use functional constructs: no mutation, exceptions, `exit`, or throwing
  lookup helpers.
- Use `Option` only for semantic absence and `Result` for recoverable failure.
- Model invalid states out of existence; complete migrations and remove
  superseded representations rather than adding defaults or shims.
- Use `Crash.crash` for an impossible, undocumented state. Do not guess a
  default.

## Change rules

- Create a failing, focused E2E test before fixing a compiler behavior.
- Keep comments useful to a senior compiler engineer, including the required
  file-purpose comment.
- Use command-line flags rather than environment variables; use `python3` for
  scripts.
- Fix compiler warnings and errors before committing.

## Git workflow

- Rebase on local `main` before new work. Never push.
- Do not land without explicit permission. Before requesting landing, commit
  intended changes and report whether the full test and routine benchmark gates
  passed. On `land`, run `./scripts/land-on-main.sh` after committing.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
