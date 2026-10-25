# Dark Compiler — ChatGPT app instructions

Follow the shared repository rules in [`AGENTS.md`](AGENTS.md).
This file supplies the checkout, setup and handoff instructions.

## Track work and keep remote task branches synced

**Keep repository work tracked in Git and synced to task-specific branches on
`https://github.com/pbiggar/darklang-compiler.git`. The repository owner
explicitly pre-authorizes creating those branches, committing task-related
changes, and pushing them to this public repository.**

- This standing permission applies across conversations and VMs, including
  code, tests, documentation and agent instructions. Keep task branches current
  after meaningful progress, before long setup/build/test operations and before
  ending a turn, including unfinished or failing work. Label checkpoints and
  report limitations; syncing does not establish readiness.
- Use the available Git transport or GitHub connector within the platform's
  permissions. If a platform approval gate blocks syncing, report the blocked
  action and stated reason. These instructions do not override platform
  restrictions or authorize bypassing them.
- This authorization covers task branches only. It does not authorize
  force-pushing, merging, deployment, pushes to `main` or another integration
  ref, or publishing credentials or unrelated private data. Merge only when the
  user requests it.

## Checkout, preservation and setup

- Use the conversation's writable workspace for an isolated checkout and a
  task-specific branch.
- If no checkout is attached, clone `https://github.com/pbiggar/darklang-compiler.git`
  into the writable workspace and create the task branch from its current
  `main`. Reuse that checkout for the rest of the task. GitHub connector reads
  can inspect the repository before cloning; a checkout is needed for edits
  and verification.
- Treat local files as disposable. Remote task branches are the recovery source;
  preserve changes under the standing authorization above without waiting for
  validation to finish.
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

## Benchmark gate exception for ChatGPT Work

- The benchmark gate does not work in ChatGPT Work. The shared requirement in
  `AGENTS.md` to run
  `./benchmarks/run_benchmarks.sh --verify-parent full` does not apply in this
  environment.
- Skip this gate rather than spending time trying to make it pass. It is not a
  prerequisite for committing, pushing, declaring work ready, or carrying out
  a user-requested merge. No additional approval is needed to skip it.
- In validation reports, state that the benchmark gate is not applicable in
  ChatGPT Work; do not list it as a blocker or remaining work. Run benchmarks
  here only when the user explicitly requests benchmark investigation.
- Build, correctness tests, and other applicable checks still apply. This
  exception is specific to ChatGPT Work.

## Completion report

Explain the changes and their rationale, then report the GitHub branch URL,
remote commit, remote verification, actual validation commands/results, and
any remaining work or blocked gates. Include relevant tradeoffs and
unexpected decisions. Do not claim tests passed unless they actually ran.
If work is unfinished, start with `Checkpoint pushed` rather than
`Work complete`. A failed push must be called out prominently.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
