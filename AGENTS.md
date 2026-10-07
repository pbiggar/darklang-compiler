# Dark Compiler - AI Agent Guidelines

Read [`docs/index.md`](docs/index.md) first. It owns navigation. Follow
[`AGENTS.mergetrain.md`](AGENTS.mergetrain.md) for the generated merge-train
contract; this file contains the additional rules specific to agents changing
this repository and takes precedence where it is stricter.

## Primary checkout boundary

### ChatGPT Work cloud VMs

This section takes precedence over the local checkout placement, task-start
and merge-train handoff rules elsewhere in these guidelines for Work VMs.

- The macOS checkout and merge-train paths below apply to the local Codex
  setup, not to ChatGPT Work's Linux VM. In Work, use the conversation's
  writable workspace for an isolated checkout and a task-specific branch.
  Do not stop solely because `/Users/paulbiggar/projects/` is unavailable.
- If no checkout is attached, clone `https://github.com/pbiggar/darklang-compiler.git`
  into the writable workspace and create the task branch from its current
  `main`. Reuse that checkout for the rest of the task. GitHub connector reads
  can inspect the repository before cloning; a checkout is needed for edits
  and verification.
- Use `Dockerfile` and `dependencies.lock` as the toolchain sources of truth.
  Bootstrap a minimal native environment with
  `bash scripts/vm/setup-native-toolchain /absolute/writable/toolchains`, then
  source that directory's `activate` file. This installs the pinned OCaml,
  Dune and native prerequisites without changing system directories. The bootstrap
  contains only the native compiler toolchain and its dependencies.
- Keep downloads, opam state, build logs and a writable `TMPDIR` under that
  toolchain directory. Build OCaml serially. Do not reuse incomplete downloads
  or accept the compiler version without checking its Unix operations.
- Build with `env -u LD_PRELOAD ./build --ai` and run the already-built suite
  with `./run-tests --ai`. Run `dune runtest` for the additional regression
  checks. The activation file supplies the VM-only temporary-path adapter.
- Commit completed changes locally. The local macOS `./land` workflow is not
  configured in Work: report verification and handoff limitations explicitly,
  and use a GitHub branch/PR only when the user authorizes that handoff.

### Local Codex checkouts

- `/Users/paulbiggar/projects/c4d-for-dcb` is the primary coordination checkout.
  Coding and task agents must treat it as read-only: do not edit or generate
  files, build, test, format, commit, switch branches, or run any other command
  that mutates its working tree. The trusted tooling-only `./land` control path
  is the sole integration exception; task agents do not make its Git update
  manually. Read-only discovery and creating a separate task worktree from it
  are allowed.
- At the start of every task, check the current worktree root with
  `git rev-parse --show-toplevel`. If it is the primary coordination checkout,
  create a dedicated task branch and worktree from the current local integration
  ref under `/Users/paulbiggar/projects/`, then change into that worktree before
  the first mutating command. Put every new task or recovery worktree directly
  under that directory, never under `.codex`, `/tmp`, or the repository itself.
  If it is an existing task worktree outside that directory, create a new task
  branch and worktree under `/Users/paulbiggar/projects/` before making changes;
  ask for direction if uncommitted changes prevent a safe move.
  Otherwise apply the Git workflow's task-start rule: refresh an eligible branch
  or create a new worktree when its completed task is awaiting merge-train
  handoff or integration. A task started
  from the primary checkout, an interactive coding-agent session, and repository
  utility work are not exceptions. If a separate worktree cannot be created,
  stop and ask for direction instead of working in the primary checkout.

## OCaml conventions

- The root `.ocamlformat` pins the standard default profile used for the
  post-port cleanup. To repeat that formatting pass, run
  `python3 scripts/format-ocaml.py`; use `--check` to inspect without editing.

- Preserve functional compiler algorithms and explicit interfaces. Localized
  parser, cache and host-I/O mutation is permitted. Do not use partial
  lookup helpers; host adapters may preserve caught host-API exceptions.
- Use `Option` only for semantic absence and `Result` for recoverable failure.
- Model invalid states out of existence; complete migrations and remove
  superseded representations rather than adding defaults or shims.
- Use `Crash.crash` for an impossible, undocumented state. Do not guess a
  default.

## Dark conventions

- Rely on type inference for module-scoped generic function and value uses.
  Do not write explicit type arguments such as `Stdlib.List.getAt<Int64>`
  unless inference is genuinely ambiguous and the explicit arguments are
  required for the program to compile.
- Standard library functions must not run shell commands or call executables
  on the user's system. The sole exception is a standard library function
  whose express purpose is to let the user make process calls.

## Change rules

- Create a failing, focused E2E test before fixing a compiler behavior.
- Preserve match validation as an ahead-of-time boundary: exhaustiveness,
  pattern and guard validity, binding consistency, and arm result types must
  fail during compilation when invalid. Never defer these failures to runtime.
  Use `compileerror=` or an upstream `#compileerror=` override when test
  correctness depends on the diagnostic phase.
- Test observable language behavior, not incidental compiler structure. Do not
  add IR or backend tests whose assertion is merely that a particular helper or
  call is present or absent. Use E2E tests for correctness and benchmarks for
  performance; add lower-level tests only when they validate independently
  meaningful backend behavior.
- Keep comments useful to a senior compiler engineer, including the required
  file-purpose comment.
- Use command-line flags rather than environment variables; use `python3` for
  scripts.
- Build the pinned native Dune graph with `./build --ai`; it keeps automated output
  bounded and retains complete failure logs under `TestResults/ai/`.
- `./run-tests` only executes an already-built test binary; it must never invoke
  a build. Run `./build --ai` first, and rebuild after changing
  source or project files before treating a test result as current.
- Before declaring a task branch ready, run the complete already-built host
  test suite with `./run-tests --ai`. Full-suite timing measurements are
  optional diagnostics and do not block integration when the host is contended.
- Do not run x64 tests on an ARM64 host unless explicitly testing x64 work.
  Likewise, do not run ARM64 tests on an x64 host unless explicitly testing
  ARM64 work.
- Run `dune runtest` for native regression and tooling checks as well; the
  complete host suite does not execute these aliases.
- Fix compiler warnings and errors before committing.
- Validate performance against the task branch's parent with
  `./benchmarks/run_benchmarks.sh --verify-parent full`. This performs the full
  benchmark measurement and fails on an aggregate regression or any individual
  benchmark regression of 0.1% or more against the snapshot stored by the
  upstream merge-base. Only attempt to land a smaller individual regression
  when it cannot be avoided; investigate and explain that loss before handoff.
  Do not run or report
  `./benchmarks/run_benchmarks.sh --verify full` as a task-readiness gate: that
  command compares with the best-known snapshot rather than the branch parent.
- During merge-conflict recovery, never hand-merge generated benchmark reports
  or choose a conflicted side. Resolve sources and measurement snapshots first,
  then run `./benchmarks/bench report` and stage its generated files.
  Regeneration runs no benchmarks and requires no new performance improvement.
  Never combine measurement rows by hand: retain a complete compatible snapshot
  under its recording rules, or remeasure when neither is valid for the merged
  sources. Applicable Darklang regression gates remain independent.
  `./benchmarks/bench report --check` verifies presentation consistency only.

## Bounded inspection

- Search with `rg` before reading source, then inspect the smallest relevant
  range. Do not dump a whole large source file when a symbol or bounded range
  answers the question.
- Keep source and documentation reads to roughly 250 lines per command unless
  the additional context is demonstrably necessary.
- Start history and review inspection with bounded summaries such as
  `git status --short`, `git diff --stat`, `git diff --numstat`, or
  `git show --no-patch`. Never run an unbounded `git show`, `git diff`, or
  `git log -p`; select explicit paths or bounded ranges before reading patches.
- Capture verbose compiler, test, benchmark, disassembly, and profiling output
  in an ignored artifact. Return only a bounded diagnostic excerpt and the
  artifact path; expand it with a targeted search rather than reading it whole.
- Prefer `--dump-function`, `--dump-ir-summary`, and `--dump-ir-output` when
  inspecting compiler IR. Use complete program dumps only when the relationship
  between multiple functions is itself under investigation.

## Git workflow

- Distinguish investigation output from a lasting repository change. For an
  investigation, profiling run, or exploratory experiment, report findings to
  the user and keep raw measurements and temporary patches in ignored artifacts.
  Do not add a report to tracked documentation, add an index link, commit, or
  run `./land` solely to preserve the investigation. A user request to
  investigate does not by itself request a durable document. Commit and hand
  off an investigation document only when the user explicitly requests that
  document as a repository artifact. If the investigation produces an accepted
  code, test, benchmark, or durable documentation change, apply the ordinary
  readiness rules to that change.
- At the start of each new task, bring its branch up to the current local value
  of the configured integration ref (`origin/main` by default) before making
  changes. Create a dedicated branch and worktree directly under
  `/Users/paulbiggar/projects/` from that ref when starting from the primary
  checkout or a worktree elsewhere. In an existing task worktree under that
  directory whose branch has not been enqueued, fast-forward the branch when
  possible; otherwise rebase its
  unpublished commits onto the ref. Preserve uncommitted changes: if they
  prevent a safe refresh, stop and ask for direction rather than discard work.
  If a rebase conflicts, abort it and ask for direction. Do not fetch or
  otherwise contact the remote first; another process owns integration-ref
  updates.
  Perform all task work in the task worktree, never in the primary checkout.
  Reuse that worktree across turns of the same task. An agent turn or an
  integration-ref advance alone does not start a new task or trigger a refresh.
  Once a completed task's commit has been handed to `./land`, keep that commit
  and worktree unchanged, whether the handoff is queued or pending. A new user
  task starts immediately on a new branch and worktree based on the current
  local integration ref, even if the previous commit has not landed. Do not
  wait for it to land, inspect its train status, or refresh or reuse its
  worktree. Authorized merge-conflict recovery under
  `AGENTS.mergetrain.md` is the exception. Task agents never push.
- When work includes an intended lasting repository change and is complete,
  commit that change automatically. An investigation whose result is only
  findings for the user is complete after the findings are reported; it does
  not need a commit or merge-train handoff.
- Run `./land --task "<brief task description>"` only when the committed branch
  is ready: the requested scope is complete, the final diff has been
  substantively reviewed,
  all relevant tests pass, relevant benchmarks show no regression, and no known
  issue or unresolved uncertainty remains. The script enqueues ordinary commits
  with bounded unattended approval and prints `queued`, or integrates an
  eligible tooling-only commit directly and prints `landed`. Once it prints
  either result, stop work on that task and report its handoff: do not inspect the
  job, poll status, wait for deployment, or report any later train outcome.
  A later user request starts a new task under the task-start rule above.
- For an intentional violation of `benchmark-sources` or `benchmarks`, report
  the evidence and reason, verify the remaining gates,
  and use `./land --exception-gate GATE --exception-reason TEXT`. This only
  requests a human review of the exact failed train candidate; it does not
  approve that gate or excuse unrelated failures.
- Judge branch readiness only from that branch's scope, review, tests,
  benchmarks, and known uncertainties. Existing queue health—including an
  unrelated job that needs attention—does not make a ready branch "not ready."
  Task agents must not inspect, diagnose, name, summarize, or prescribe
  recovery for unrelated train jobs.
- If `./land` exits with `Landing handoff is pending`, keep the ready commit
  unchanged and report only `Merge train: ⏳ handoff pending`. Do not include
  queue health, unrelated job IDs, conflicts, or recovery instructions. Treat
  other pre-enqueue errors according to their own message.
- For ordinary jobs, `./land` grants the configured merge-train runner bounded
  unattended approval for that exact destination and execution policy. For an
  eligible tooling-only commit, the trusted script validates and atomically
  updates local integration itself. Task agents do not push or integrate `main`
  directly.
- If readiness cannot be established, leave the commit on its worktree branch
  without enqueueing it and report `Merge train: ❌ not ready — <reason>`. Do
  not use a low-value mechanical check as a substitute for relevant
  validation.

## Explaining technical work

- Pair abstract claims with a small, concrete example from the task when
  explaining behavior, design, or tradeoffs. Show the relevant input and
  outcome so the reader can see what the claim means in practice.
- For correctness, include a representative case and an important boundary
  case when useful. For example, name the missing match case and show that the
  program fails at compilation, rather than only saying match validation is
  ahead of time.
- For optimization, show a representative workload and what changes for it,
  such as fewer allocations or traversals. Include measured before/after
  numbers when available; label hypothetical examples clearly and do not
  present estimates as measurements.
- Use examples in progress updates and completion reports when they clarify
  the point, while keeping them short and relevant to the user's question.

## Completion report

Use this standard format when reporting completed work. Explain what changed
and why before the commit and validation status. Describe concrete changes and
their resulting behavior or effect so the reader can understand the work
without opening the diff. Call out surprising decisions, tradeoffs, or changes
beyond the obvious request, and explain why they were necessary.

Match the explanation's detail to the complexity and importance of the work:
a small, straightforward edit may need only a sentence or two; complex or
consequential work needs enough paragraphs or bullets to explain the main
changes, their rationale, and material implications. Keep the outcome line
brief, but do not let brevity hide important details. Include exact validation
commands and omit optional lines that add no useful information, including
`Surprises` when there are none.
Use `✅` for success, `⏳` for a ready branch whose handoff is pending, `⏭️` for
a skipped or irrelevant gate, and `❌` for a failure or incomplete step.

```markdown
Work complete: <brief description of the outcome>

Changes and rationale: <what changed and why; scale the explanation to the
complexity and importance of the work>
Surprises: <unexpected decisions, tradeoffs, or scope changes and why; omit if none>

Committed: `<short hash>` — <commit subject>
Merge train: ✅ queued `<branch>` at `<short hash>`
Tests: ✅ <passed>/<total> passed — `<exact command>`
Benchmarks: ✅ no blocking regression vs task parent, ratio <ratio>
  - Parent gate: `./benchmarks/run_benchmarks.sh --verify-parent full`
Other validation: ✅ <result> — `<exact command>`
Worktree: `<absolute task worktree path>`
Working tree: ✅ clean
Notes: <residual risk, preserved pre-existing changes, or other useful context>
```

For multiple commits or verification commands, put bullet points beneath the
corresponding label. If a separately authorized runner deployed the train,
replace the merge-train line with
`Merge train: ✅ deployed <jobs/branches> to <destination> at <short hash>`.
For an eligible tooling-only commit that made `./land` print `landed`, report
`Merge train: ✅ tooling landed via ./land` instead.
A skipped gate must say why, for example:
`Tests: ⏭️ skipped — documentation-only change`.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
