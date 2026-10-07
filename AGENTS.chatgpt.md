# Dark Compiler — ChatGPT app instructions

Follow the shared repository rules in [`AGENTS.md`](AGENTS.md).
This file supplies the checkout, setup and handoff instructions.

## Checkout, preservation and setup

- Use the conversation's writable workspace for an isolated checkout and a
  task-specific branch.
- If no checkout is attached, clone `https://github.com/pbiggar/darklang-compiler.git`
  into the writable workspace and create the task branch from its current
  `main`. Reuse that checkout for the rest of the task. GitHub connector reads
  can inspect the repository before cloning; a checkout is needed for edits
  and verification.
- Treat the VM and all local files as disposable: they may disappear between
  turns. Always preserve repository changes on a task-specific GitHub branch.
  Creating and pushing that branch is authorized by these instructions; do not
  wait for another user request or for validation to finish. Push an initial
  checkpoint before a long setup/build/test operation, after meaningful progress,
  and before ending every turn with repository changes, including unfinished or
  failing work. Label incomplete commits as checkpoints and report remaining work.
- Push only the task branch, never `main` or another integration ref, and never
  force-push. Stage intended paths explicitly; exclude credentials, downloads,
  toolchains and generated build artifacts. Use `git push -u origin HEAD`, then
  verify that `git ls-remote origin refs/heads/<branch>` matches `git rev-parse HEAD`.
  If Git transport is unavailable, use the GitHub connector to preserve the same
  intended files and commit on the task branch, then verify the remote tree.
  If neither works, report the failure prominently; a local commit alone is not
  a durable handoff. Return the GitHub branch URL and commit in the completion report.
- After a VM reset, recover the existing task branch from GitHub and read its
  changes before continuing. Do not restart from `main` or redo completed work.
  New tasks start from current remote `main`. Branch preservation does not
  authorize merging or deployment.
- Use `Dockerfile` and `dependencies.lock` as the toolchain sources of truth.
  Bootstrap a minimal native environment with
  `bash scripts/vm/setup-native-toolchain /absolute/writable/toolchains`, then
  source that directory's `activate` file. This installs the pinned OCaml,
  Dune and native prerequisites without changing system directories. The bootstrap
  contains only the native compiler toolchain and its dependencies. Read
  [`scripts/vm/README.md`](scripts/vm/README.md) for the fresh-VM and recovery
  runbook. Keep the toolchain outside the checkout, reuse it if present, and
  rerun the bootstrap after interruption rather than inventing setup commands.
- Keep downloads, opam state, build logs and a writable `TMPDIR` under that
  toolchain directory. Build OCaml serially. Do not reuse incomplete downloads
  or accept the compiler version without checking its Unix operations.
- Build with `env -u LD_PRELOAD ./build --ai` and run the already-built suite
  with `./run-tests --ai`. Run `dune runtest` for the additional regression
  checks. The activation file supplies the VM-only temporary-path adapter.
- Commit and push the task branch even when a gate is blocked; report each
  actual validation result and limitation. Do not describe a checkpoint as
  ready or merged.

## Git workflow

- Preserve intended repository edits as pushed checkpoints while investigating.
  This does not require turning findings or raw measurements into tracked
  documentation. A pushed checkpoint preserves work; it does not demonstrate
  readiness. Merge only when the user requests it.

## Completion report

Explain the changes and their rationale, then report the GitHub branch URL,
remote commit, remote verification, actual validation commands/results, and
any remaining work or blocked gates. Include relevant tradeoffs and
unexpected decisions. Do not claim tests passed unless they actually ran.
If work is unfinished, start with `Checkpoint pushed` rather than
`Work complete`. A failed push must be called out prominently.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
