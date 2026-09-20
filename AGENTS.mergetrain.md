# mergetrain agent contract

Purpose: Serialize committed local task branches through one merge/test/push/verify runner.

## Existing queues and explanations

- Task agents use `./land` → `queued` → stop, even for one branch. Runner/operator queue inspection uses status; queue counts alone do not establish health, runner ownership, or recovery needs, so read `health`, `state`, and `next_action` together.
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
   When started there, create and enter a separate task worktree before any
   edit, build, test, or other mutating command. Reuse that task worktree across
   turns and follow-up tasks; do not create or move to another worktree during
   that process.
2. Commit a clean HEAD before handing work off.
3. Task agents use `./land`, not raw status inspection, for handoff. A runner/operator may use `mergetrain status --json`; its queue-level `next_action` is not a judgment about any task branch's readiness.
4. Land every named finished branch in the requested order with `./land --task "TASK"`; the script resolves the branch and worktree and enqueues the exact commit with bounded unattended approval. Success is the single line `queued`. After that line, do not inspect the job, poll status, wait for deployment, or report a later outcome. If it exits with `Landing handoff is pending`, report only `Merge train: ⏳ handoff pending`; do not use `❌ not ready`, explain the train state, or volunteer to recover another job.
5. Never push configured integration refs directly. One authorized runner owns validation and deployment; recovery and destructive actions require their stated approval.

## Safety boundary

- A task agent runs `./land` and stops when it prints `queued`. The script's internal `--auto` enqueue authorizes only the configured runner's bounded unattended validation and deployment; it does not authorize the task agent to monitor, validate, deploy, or report the eventual outcome.
- Only a separately authorized runner uses `deploy` or a daemon.
- A recovery conflict in generated `benchmarks/RESULTS.md` is resolved only by a successful improving `./benchmarks/run_benchmarks.sh full` recording from the rebased source. Never hand-merge or choose one side of that file; if recording does not regenerate it from an improved canonical snapshot, recovery stops.
- The `benchmark-sources` gate rejects any candidate that changes `benchmarks/problems/`; benchmark source changes require a separately controlled integration-policy update and are never repaired or bypassed by the integrator.
- Every assembled queue candidate reruns `./benchmarks/run_benchmarks.sh --verify-fresh full`. A regression fails. An unrecorded improvement is repaired only by rebasing the owning branch, successfully recording the full suite, committing the regenerated benchmark files, and retrying that new exact commit; the retry must reproduce the recording before deployment.
- Deployment requires either confirmation of the human-readable exact plan or prior bounded unattended approval. Agents never select train IDs or supply plan hashes; structured evidence may include identifiers for inspection.
- Unattended approval is bound to the exact destination and execution policy. Any change blocks before push.
- Recovery and destructive cleanup require their stated approval. Follow `status.next_action`; never rewrite permanent deploy audit refs.

## Stable machine contract

- Every JSON payload carries `contract_version`; ignore unknown keys and fail closed on unknown safety actions.
- `deploy` means the atomic Git ref update plus configured verification. A downstream provider release is separate.
