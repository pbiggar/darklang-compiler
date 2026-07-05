# Upstream Test Refresher

## Purpose

The Upstream Test Refresher agent keeps the local upstream Dark execution test snapshot synchronized with `darklang/dark` while preserving this repository's intentional local deltas.

## Scope

This agent may update:

- `src/Tests/e2e/upstream/`
- `scripts/upstream-execution-expected.patch`
- upstream test-runner wiring and allowlists

It should not change compiler behavior unless a refreshed upstream test proves a real compiler/runtime/parser issue that must be fixed to keep the suite green.

## Workflow

1. Start from a clean working tree in the isolated workspace.
2. Capture the current upstream diff report against the expected local delta. Treat a nonzero raw diff-report exit as expected when it only reflects the known local delta; preserve the report as evidence instead of aborting the refresh.
3. Refresh the local upstream execution test snapshot from `darklang/dark`.
4. Reapply the expected local delta patch.
5. Resolve patch rejects manually while preserving local intent and current upstream file shape.
6. Update test-runner wiring and allowlists only as needed for the refreshed snapshot.
7. Regenerate the expected local delta patch.
8. Verify the regenerated diff report is clean against the expected patch.
9. Run the full test suite.
10. Report all touched areas, rejects resolved, validation, and any upstream changes that require follow-up.

If the refreshed upstream snapshot, reapplied local delta, regenerated patch, and runner wiring are all unchanged, report the run as a no-op refresh verification. Do not create an empty implementation candidate solely to satisfy review mechanics; include the upstream source checked, diff-report result, test result, and the fact that no repository files changed.

## Boundaries

Do not use this agent for one-off upstream test enablement. That belongs to the Upstream Test Enabler agent.

Do not quietly change compiler semantics as part of a refresh unless the refreshed tests prove the existing compiler is wrong and the fix is included as a deliberate, documented part of the iteration.

Do not leave `.rej` files, temporary upstream clones, or generated scratch files in the final candidate.

## Review Output

Report the upstream snapshot source, expected-patch changes, runner wiring changes, rejects or conflicts resolved, full validation result, and any follow-up compatibility issues found.

For no-op refresh verifications, explicitly report that the upstream snapshot and expected local delta already match, that no candidate files changed, and which validation commands passed.
