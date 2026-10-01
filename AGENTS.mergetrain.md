# mergetrain agent contract

Purpose: Integrate tooling changes promptly and serialize ordinary committed
task branches in arrival order.

## Existing queues and explanations

- Task agents use `./land` and stop after `queued` or `landed`, even for one branch. Runner/operator queue inspection uses status; queue counts alone do not establish health, runner ownership, or recovery needs, so read `health`, `state`, and `next_action` together.
- Branch readiness and queue availability are separate. An unrelated attention job can defer enqueueing, but it does not invalidate a branch whose own review and gates passed. Never report such a branch as `not ready`.
- Unrelated train state belongs to the runner/operator, not the task agent. Task agents do not inspect or report unrelated job IDs, conflicts, attention reasons, or recovery steps.
- For explanation-only requests, read the skill documentation when permitted and explain the procedure without Git or product commands. Distinguish hypothetical steps from observed state.

## Current command reference

- The v3 core commands are `init`, `status`, `enqueue`, `validate`, `deploy`, and `inspect`.
- Runner/operator investigations start with `mergetrain status --json`. Use `mergetrain status --diagnose --json` only for configuration, Git, runtime, or lock detail, and `mergetrain inspect JOB_ID --json` for job evidence. `doctor` is removed, not an alias for `status --diagnose`.
- Confirm uncertain syntax with the installed `mergetrain --version` and command-specific `--help` only when command execution is permitted. Otherwise use this reference and identify missing details; do not invent commands or copy older syntax from unversioned web results. Inspection and `next_action` do not authorize recovery or deployment.

## Rules

1. Work on a task-specific branch and worktree. Coding and task agents must
   treat `/Users/paulbiggar/projects/c4d-for-dcb` as a read-only coordination
   checkout, including for repository utility work and interactive sessions.
   The trusted tooling-only `./land` control path is the sole integration
   exception; task agents do not make that Git update manually.
   When started there, create and enter a separate task worktree directly under
   `/Users/paulbiggar/projects/` before any edit, build, test, or other mutating
   command. Reuse that task worktree across turns of the same task. At a new
   task boundary, refresh an eligible existing
   task branch from the local integration ref as described in `AGENTS.md`.
   Never change a completed task's commit or worktree after `./land`, whether
   its handoff is queued or pending. Start a newly requested task immediately
   in a new branch and worktree from the current local integration ref, even
   while the earlier commit has not landed. Do not inspect the earlier job or
   wait for it to land.
2. Commit a clean HEAD before handing lasting repository changes off. Findings
   from an investigation alone do not require a commit or handoff; follow the
   investigation rule in `AGENTS.md`.
3. Task agents use `./land`, not raw status inspection, for handoff. A runner/operator may use `mergetrain status --json`; its queue-level `next_action` is not a judgment about any task branch's readiness.
4. Land every named finished branch containing an intended lasting repository change with `./land --task "TASK"`. An eligible tooling-only commit is merged and validated by the script immediately; success prints `landed`. Other commits are enqueued with bounded unattended approval; success prints `queued`. After either result, stop work on that task. Do not inspect the job, poll status, or report a later outcome. A later user request starts a new task under rule 1. If an ordinary handoff exits with `Landing handoff is pending`, report only `Merge train: ⏳ handoff pending`.
5. Task agents never push configured integration refs directly. The trusted `./land` tooling path owns its atomic local integration update. The ordinary runner owns ordinary validation and deployment; recovery and destructive actions require their stated approval.

## Safety boundary

- A task agent runs `./land` and stops when it prints `queued` or `landed`. The script's internal `--auto` enqueue authorizes only the configured ordinary runner's bounded unattended validation and deployment; it does not authorize the task agent to monitor or deploy an ordinary job.
- Only a separately authorized runner uses `deploy` or a daemon.
- Recovery conflicts in generated benchmark reports are resolved with `./benchmarks/bench report` after source and snapshot conflicts are resolved. Never hand-merge reports or combine snapshot rows by hand. Regeneration requires no new improvement; Darklang performance readiness remains governed by the independent regression gates.
- The `benchmark-sources` gate rejects candidates that change
  `benchmarks/problems/*/dark/`, including workload inputs. Reference-language
  implementations may be added or changed without this exception. A lander may request a one-time exception, but only
  a human operator may approve the exact failed candidate and gate.
- Every assembled queue candidate runs `./benchmarks/bench verify --against
  deployed` once (delegating to the existing `--verify-deployed full` runner). Its exact counts are compared with the last
  deployed head, independently of the canonical best-known snapshot. A new
  regression requires an exact human `benchmarks` exception. Only confirmed
  deployment promotes the candidate's counts to the next comparison baseline.
- When benchmark recovery reproduces the failed candidate's exact tree, the
  runner stages an exact `benchmarks` review and stops replacing the job.
  This requests human review; it does not approve the regression.
- The train's `tests` gate runs the complete already-built host test suite with
  `./run-tests --ai`. Full-suite timing measurements are optional diagnostics;
  contention does not block integration.
- The `leaks` gate compiles canonical Dark benchmarks with leak accounting and
  rejects any quick or full workload that leaks or changes its output. A human
  operator may approve an exact exception for this gate.
- A task agent may request an exception with `./land --exception-gate GATE
  --exception-reason TEXT` for `benchmark-sources`, `benchmarks`, or `leaks`.
  Requesting does not approve it. The integrator stages the
  failed candidate for human review. In interactive `./mergetrain-status`,
  press `a` to review and confirm it; this retries the job. When one candidate
  fails multiple eligible gates, each gate needs its own request and review.
  An earlier approval carries to the next retry only while the exact candidate,
  integration base, destination, and policy remain the same. All other gates run.
- Deployment requires either confirmation of the human-readable exact plan or prior bounded unattended approval. Agents never select train IDs or supply plan hashes; structured evidence may include identifiers for inspection.
- Unattended approval is bound to the exact destination and execution policy. Any change blocks before push.
- Recovery and destructive cleanup require their stated approval. Follow `status.next_action`; never rewrite permanent deploy audit refs.
- Automated recovery preserves the original enqueued branch and owning
  worktree. The integrator creates a fresh recovery branch and worktree from
  the current integration commit, asks Codex to perform any semantic merge
  there, checks the new commit and clean worktree, and then replaces the
  blocked queue row. The train runs the configured gates before deployment.
  Codex never pushes or changes queue state.
- The repository integrator admits only the oldest unfinished ordinary job.
  Its shared Git directory ledger retains arrival order through native retry
  and repair rows. An attention head blocks later ordinary jobs. Tooling-only
  commits use the separate `./land` control path even while the head is blocked.

## Stable machine contract

- Every JSON payload carries `contract_version`; ignore unknown keys and fail closed on unknown safety actions.
- `deploy` means the atomic Git ref update plus configured verification. A downstream provider release is separate.
