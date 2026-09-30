# Merge-train integrator recovery

The repository integrator runs the native merge-train daemon for ordinary jobs
in arrival order. Only the oldest unfinished ordinary job is admitted to a
daemon pass. Later jobs are recorded in the shared Git directory and deferred;
their original arrival number survives native retries and repair replacements.
Blocked later rows are dismissed after their failure is recorded, because the
native queue treats them as active and would reject re-enqueueing their branch.
An attention job blocks all later ordinary jobs until it is repaired or
resolved. The daemon owns ordinary job gates and deployment.

`./land` sends a commit whose entire change is in the explicit merge-train
tooling allowlist through a separate control path. It merges that commit onto
current local `main` in an isolated worktree, checks the merged tree and runs
focused tooling tests, then atomically updates `mergetrain-local/main` under a
lease. This can complete while an ordinary attention job is blocking the
queue. The dispatch lock serializes ordinary handoffs with daemon passes;
tooling updates use an atomic push lease and can race safely with either.
Mixed tooling and product changes use the ordinary queue.

## Recovery order

For the oldest attention job, the integrator:

1. Inspects the structured job and gate evidence.
2. Retries an unchanged revision once when a gate was interrupted, timed out,
   or terminated by a signal.
3. Fetches the configured integration ref and compares patches with `git
   cherry`. A wholly patch-equivalent job is dismissed as already integrated;
   a partial recovery replays only unique commits.
4. Creates a fresh `mergetrain-repair/<job>-<sha>` branch and worktree from the
   exact integration commit. The original task branch and owning worktree stay
   clean at their enqueued commit.
5. Asks Codex to resolve a stopped cherry-pick or repair a reproducible build,
   test, or benchmark failure. Codex may edit and commit only in the fresh
   recovery worktree; it cannot change queue state or deploy.
   Recovery runs with writable shared Git metadata so staging and committing
   work in the linked worktree. The integrator checks the resulting commit and
   worktree before replacement.
6. Checks that the repair produced a new commit and left a clean worktree.
   The integration and repair identities are retained in a JSON receipt.
   Focused checks during repair are useful, but a separate full build, test,
   and benchmark run is not required before replacement. The train runs its
   configured gates on the replacement candidate before deployment.
7. Replaces the blocked job. A mergetrain implementation with the native
   `replace` command performs this atomically. Version 3 compatibility enqueues
   the verified replacement before dismissing the old blocked row, so a crash
   cannot lose the repair.

If a benchmark recovery produces the same tree as the failed candidate, the
integrator keeps the failed job at the FIFO head and stages its exact candidate
for human `benchmarks` exception review. It does not enqueue another identical
replacement or approve the gate itself.

The FIFO ledger records a deployed head in `completed` before admitting the
next task, including when the integrator discovers that deployment on its next
pass. Before retiring that head, the integrator promotes its measured 29
instruction counts to the deployed-head baseline. A missing or incompatible
measurement pauses admission. It then removes a clean recovery worktree at the
deployed commit. Native job history remains the deployment audit.

The benchmark gate carries the deployed counts across tooling-only integration
advances only when control receipts prove every intervening landing and its
actual changed paths leave the compiler and measurement inputs unchanged.
Changes to the benchmark runner require fresh measurement. A baseline error
pauses recovery before Codex or exception review: bookkeeping failures cannot
be repaired by changing compiler source or approving a benchmark regression.

For `approval_execution_policy_changed`, the integrator records the original
job's policy failure and the exact `.mergetrain.yaml` diff between its enqueue
base and current integration in an attempt artifact. It automatically recovers
only when the job did not change that file, the current control checkout matches
integration, and the integrated policy change is limited to `gates`,
`gate_parallelism`, or a standalone `state.worktree_root` relocation. The
worktree location is excluded from the native destination and execution-policy
approval hashes. It replays the job on current integration, checks the new
commit and clean worktree, then enqueues it with a fresh bounded `--auto`
approval. The daemon runs the current gates once before deployment. Changes to
reuse, verify hooks, or other execution policy settings remain operator
decisions because the configured gates cannot prove them.
If such a change pauses a deferred FIFO head, status shows its original
number and reason. An operator can stage that exact commit as a manual job with
`python3 scripts/mergetrain_fifo.py --repo . manual ORDER HEAD_SHA`, then use
interactive `mergetrain --repo . deploy` to validate and confirm the exact
plan. The job remains the FIFO head until deployment succeeds.

An unrecoverable head remains at the front. The integrator records its failure
and pauses later ordinary jobs. `./mergetrain-status` displays the FIFO head
and deferred jobs. A tooling-only `./land` still runs independently.

## One-time gate exceptions

A lander may attach a request to its exact committed branch:

```bash
./land --task "change description" --exception-gate benchmarks \
  --exception-reason "Why the measured regression is acceptable"
```

Eligible gates are `benchmark-sources`, `benchmarks`, and `leaks`.
Each request names one gate and the exact committed head. A candidate that
fails more than one eligible gate needs a separate request and human review for
each failed gate. Requests for the same head are stored separately by gate.
The `leaks` gate compiles every canonical Dark benchmark with leak accounting,
checks its quick and full output, and rejects any nonempty stderr. An exact
candidate can request human review of this gate with `./land --exception-gate
leaks --exception-reason TEXT`.
A request does not waive a gate. The candidate first
fails normally, and the integrator stages its failed gate, exact candidate
tree, integration base, destination, and gate config digest for review. It does
not automatically repair that requested failure.

Interactive `./mergetrain-status` shows the blocked job and its pending waiver.
The operator presses `a`, selects the job if several are pending, reads the
reason, failed gate log, candidate identity, and other gate outcomes, then
types the requested confirmation. The status interface retries the job.
The gate reruns on retry; only the approved gate's failure is waived if the
replacement job ID, candidate tree, original commit ancestry, integration
base, destination, and gate config still match. Mergetrain separately checks
that its own unattended approval remains valid. Other gates still block.
If a retry then fails another requested gate, its separate review uses the same
exact-candidate checks. The earlier approval follows the new retry only when
the head, branch, candidate tree, integration base, destination, and policy
still match; any changed identity requires a fresh review.
The integrator archives the approval when the replacement finishes.

The approval registry lives in the shared Git directory. The human approval
boundary is the interactive operator command and its review procedure; it is
not an OS privilege or cryptographic boundary against a process that can edit
that directory.

## Operator boundaries

Automatic recovery is limited to the cases above, non-fast-forward push races,
and attributable build, test, or benchmark gate failures. Unknown categories,
policy failures outside the gate-change case, non-fast-forward-independent push
rejection, repeated transient failures, dirty owning worktrees, and failed
independent verification remain operator decisions.

`./mergetrain-status` opens attention jobs with `a` and queued manual policy
replacements with `v`. The detail view shows the recorded failure and policy
diff, and `n`/`p` move between review pages. The selected job ID and task stay
visible at the bottom while the page scrolls. `r` offers a confirmed
`mergetrain retry` for the selected FIFO head and records its replacement under
the original arrival number. Deferred jobs are listed in status but cannot be
retried ahead of the head. Retry does not renew
an expired unattended policy approval: mergetrain creates a manual replacement
when the policy no longer matches. On a blocked policy job or its queued manual
replacement, `A` starts a human review: it displays the job's policy diff,
requires the operator to type its job ID and commit prefix, retries a blocked
job as manual when needed, validates a train containing the manual job, then
opens mergetrain's exact deploy plan for a separate interactive confirmation.
The plan can include other jobs, and declining it leaves the validated train
ready without pushing.

Generated `benchmarks/RESULTS.md` is never hand-merged. Source conflicts are
resolved first, then a successful improving full recording run must regenerate
the result and canonical snapshot.

## Artifacts

In an interactive terminal, the integrator gives each job one line with its
number, local timestamp, and latest phase. It rewrites that line as gates and
deployment progress, then leaves the final state in place. Captured output
writes a job line when it reaches a final state because a log file cannot
rewrite earlier lines. Detailed daemon and recovery output remains in the
attempt directory.

Merge-train validation worktrees use `state.worktree_root` in
`.mergetrain.yaml`; recovery worktrees live directly under
`/Users/paulbiggar/projects/` too. Codex logs, bounded final messages,
verification command logs, and receipts live
under the configured attempt directory. Their names include the original job
and commit identities. A failed recovery is preserved
for inspection instead of mutating or discarding the original task checkout.

E2E tests may read stable system locations, but they may not write through a
fixed shared `/tmp` path. Use a per-process interpolated path and keep its full
write/read/delete lifecycle inside one isolated test. The `e2e-temp-paths`
merge-train gate enforces this rule.
