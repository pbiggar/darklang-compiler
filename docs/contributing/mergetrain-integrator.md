# Merge-train integrator recovery

The repository integrator continuously runs the native merge-train daemon and
recovers auto-approved jobs when doing so is safe. The daemon exclusively owns
assembly, gate execution, and deployment. The recovery layer never pushes an
integration ref.

## Recovery order

For every attention job, the integrator:

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
6. Independently verifies the committed repair with diff and policy checks,
   `./build --ai`, `./run-tests --ai`, and
   `./benchmarks/run_benchmarks.sh --verify-parent full`. The commands and exact
   parent/head identities are retained in a JSON verification receipt.
7. Replaces the blocked job. A mergetrain implementation with the native
   `replace` command performs this atomically. Version 3 compatibility enqueues
   the verified replacement before dismissing the old blocked row, so a crash
   cannot lose the repair.

Every attention job in a status snapshot is considered. An unrecoverable job
does not prevent later blockers from being inspected or the daemon from
deploying an independently validated subset.

## Operator boundaries

Automatic recovery is limited to merge and semantic conflicts,
non-fast-forward push races, and attributable build, test, or benchmark gate
failures. Unknown categories, policy failures, non-fast-forward-independent
push rejection, repeated transient failures, dirty owning worktrees, and
failed independent verification remain operator decisions.

Generated `benchmarks/RESULTS.md` is never hand-merged. Source conflicts are
resolved first, then a successful improving full recording run must regenerate
the result and canonical snapshot.

## Artifacts

Recovery worktrees, Codex logs, bounded final messages, verification command
logs, and receipts live under the configured attempt directory. Their names
include the original job and commit identities. A failed recovery is preserved
for inspection instead of mutating or discarding the original task checkout.

E2E tests may read stable system locations, but they may not write through a
fixed shared `/tmp` path. Use a per-process interpolated path and keep its full
write/read/delete lifecycle inside one isolated test. The `e2e-temp-paths`
merge-train gate enforces this rule.
