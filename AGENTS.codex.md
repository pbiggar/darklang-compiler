# Dark Compiler — Codex instructions

Follow the shared repository rules in [`AGENTS.md`](AGENTS.md) and
[`AGENTS.mergetrain.md`](AGENTS.mergetrain.md) for the generated merge-train
contract. This file contains the additional repository rules and takes
precedence where it is stricter.

## Checkout boundary

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

## Git workflow

- Fix compiler warnings and errors before committing.
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
