# Dark Compiler - AI Agent Guidelines

Read [`docs/index.md`](docs/index.md) first. It owns navigation. Follow
[`AGENTS.mergetrain.md`](AGENTS.mergetrain.md) for the generated merge-train
contract; this file contains the additional rules specific to agents changing
this repository and takes precedence where it is stricter.

## F# conventions

- Use functional constructs: no mutation, exceptions, `exit`, or throwing
  lookup helpers.
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
- Build the .NET/F# projects with `./build --ai`; it keeps automated output
  bounded and retains complete failure logs under `TestResults/ai/`.
- `./run-tests` only executes an already-built test binary; it must never invoke
  a .NET/F# project build. Run `./build --ai` first, and rebuild after changing
  source or project files before treating a test result as current.
- Do not run x64 tests on an ARM64 host unless explicitly testing x64 work.
  Likewise, do not run ARM64 tests on an x64 host unless explicitly testing
  ARM64 work.
- Fix compiler warnings and errors before committing.
- Validate performance against the task branch's parent with
  `./benchmarks/run_benchmarks.sh --verify-parent full`. This performs the full
  benchmark measurement and fails on an aggregate regression against the
  snapshot stored by the upstream merge-base. Do not run or report
  `./benchmarks/run_benchmarks.sh --verify full` as a task-readiness gate: that
  command compares with the best-known snapshot rather than the branch parent.
- During merge-conflict recovery, never hand-merge, choose a side for, or edit
  conflict markers in generated `benchmarks/RESULTS.md`. With all source
  conflicts resolved in the rebased working tree, run
  `./benchmarks/run_benchmarks.sh full`; recording mode must prove an aggregate
  improvement, advance the canonical Dark snapshot, and regenerate
  `RESULTS.md`. Stage the regenerated files. If the run fails or does not
  replace the conflicted result, abort the recovery instead of guessing.

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

- Create a dedicated worktree once when beginning a new task and base its
  branch on the current local value of the configured integration ref
  (`origin/main` by default) before making changes. Do not fetch or otherwise
  contact the remote first; another process owns integration-ref updates.
  Perform all task work there, never in the primary checkout.
  After work begins, do not pull, merge, rebase, reset, or otherwise incorporate
  later integration-ref changes into that task worktree merely because the
  integration ref advanced or a new agent turn began. Once enqueued, keep the
  exact commit and worktree unchanged; follow-up work uses a new task branch and
  worktree based on the latest integration ref. The exceptions are an explicit
  developer instruction to update that specific existing worktree and
  authorized merge-conflict recovery under `AGENTS.mergetrain.md`. Task agents
  never push.
- When work is complete, commit the intended changes automatically.
- Run `./land --task "<brief task description>"` only when the committed branch
  is ready: the requested scope is complete, the final diff has been
  substantively reviewed,
  all relevant tests pass, relevant benchmarks show no regression, and no known
  issue or unresolved uncertainty remains. The script enqueues the exact commit
  with bounded unattended approval and prints `queued`. Once it prints
  `queued`, stop: do not inspect the job, poll status, wait for deployment, or
  report any later train outcome.
- Judge branch readiness only from that branch's scope, review, tests,
  benchmarks, and known uncertainties. Existing queue health—including an
  unrelated job that needs attention—does not make a ready branch "not ready."
  Task agents must not inspect, diagnose, name, summarize, or prescribe
  recovery for unrelated train jobs.
- If `./land` exits with `Landing handoff is pending`, keep the ready commit
  unchanged and report only `Merge train: ⏳ handoff pending`. Do not include
  queue health, unrelated job IDs, conflicts, or recovery instructions. Treat
  other pre-enqueue errors according to their own message.
- `./land` grants the configured merge-train runner bounded unattended approval
  for that exact destination and execution policy. It does not authorize the
  task agent itself to validate, deploy, push, or integrate `main` directly.
- If readiness cannot be established, leave the commit on its worktree branch
  without enqueueing it and report `Merge train: ❌ not ready — <reason>`. Do
  not use a low-value mechanical check as a substitute for relevant
  validation.

## Completion report

Use this standard format when reporting completed work. Keep the summary brief,
include exact commands, and omit optional lines that add no useful information.
Use `✅` for success, `⏳` for a ready branch whose handoff is pending, `⏭️` for
a skipped or irrelevant gate, and `❌` for a failure or incomplete step.

```markdown
Work complete: <brief description of the outcome and important details>

Committed: `<short hash>` — <commit subject>
Merge train: ✅ queued `<branch>` at `<short hash>`
Tests: ✅ <passed>/<total> passed — `<exact command>`
Benchmarks: ✅ no regression vs task parent, ratio <ratio>
  - Parent gate: `./benchmarks/run_benchmarks.sh --verify-parent full`
Other validation: ✅ <result> — `<exact command>`
Working tree: ✅ clean
Notes: <residual risk, preserved pre-existing changes, or other useful context>
```

For multiple commits or verification commands, put bullet points beneath the
corresponding label. If a separately authorized runner deployed the train,
replace the merge-train line with
`Merge train: ✅ deployed <jobs/branches> to <destination> at <short hash>`.
A skipped gate must say why, for example:
`Tests: ⏭️ skipped — documentation-only change`.

For CLI commands, development setup, architecture, feature work, and complete
verification requirements, use the canonical sources in `docs/index.md`.
