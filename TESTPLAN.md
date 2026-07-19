# Upstream E2E Enablement Test Plan

Date: 2026-03-13

This file has been re-evaluated against the current branch instead of preserving the
2026-03-06 snapshot.

Command examples in this plan use `./run-tests --ai` so future automated
reruns keep bounded, agent-friendly output.

Checked files:
- `language/apply/eapply.dark`
- `language/custom-data/aliases.dark`
- `language/flow-control/epipe.dark`
- `language/derror.dark`
- `language/elambda.dark`
- `stdlib/date.dark`
- `src/Tests/e2e/upstream/cloud/db.dark` (`--roundtrip-all-dark` blocker from the previous plan)

Method used on 2026-03-13:
1. Inspect the current upstream gating state in `src/Tests/test-suite-tooling/TestRunner.fs`.
2. Run targeted test slices with `./run-tests --ai` for the files above.
3. Record whether the original blocker still reproduces.
4. Record the current enablement state:
   - in default upstream E2E coverage
   - allowlisted only
   - still fully blocked

## Current enablement snapshot

- `eapply.dark`: partially enabled in the default upstream subset.
- `aliases.dark`: partially enabled behind a line allowlist only.
- `epipe.dark`: still blocked.
- `derror.dark`: still blocked, but the old parser/preamble blockers are no longer the active issue.
- `elambda.dark`: still blocked by the same `RefCountInsertion` crash.
- `date.dark`: partially enabled behind a line allowlist only.
- `cloud/db.dark` roundtrip blocker: no longer reproduces.

## `language/apply/eapply.dark`

Status: partially enabled.

Current runner state:
- Included in `defaultUpstreamDarkPaths`.
- Gated by `upstreamEnablementLineAllowlist`.
- Enabled lines: `1, 4, 7, 10, 19, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 75, 76, 78, 79, 88, 92, 93, 95, 96, 98, 100, 104, 108, 110, 124, 135, 136, 138, 139, 141, 142` (`38` lines).

What is fixed relative to the previous plan:
- The old roundtrip parser blocker at `L1` no longer reproduces in the current targeted run.
- The old suite-level preamble build/type-mismatch blocker no longer prevents targeted execution.
- Multiple `eapply` cases have been promoted into the allowlist since the previous snapshot.

Evidence:
- `./run-tests --ai --filter=eapply` passes.

Remaining work:
- Remove the line allowlist incrementally and continue promoting cases until the whole file can run ungated.

## `language/custom-data/aliases.dark`

Status: partially enabled, but not yet part of the default upstream subset.

Current runner state:
- Not included in `defaultUpstreamDarkPaths`.
- Gated by `upstreamEnablementLineAllowlist`.
- Enabled lines: `6, 9`.

What is fixed relative to the previous plan:
- The old roundtrip parser blocker for the `L48` preamble snippet no longer blocks the targeted run.
- The old suite-level preamble type error (`Cannot apply non-function type: String`) no longer blocks the targeted run.

Evidence:
- `./run-tests --ai --filter=aliases` passes.

Remaining work:
- Expand allowlisted coverage beyond `L6` and `L9`.
- Once enough of the file is green, add it to `defaultUpstreamDarkPaths`.

## `language/flow-control/epipe.dark`

Status: blocked.

Current failures from `./run-tests --ai --filter=epipe`:
- Value mismatch: `L9, L14, L16, L24, L42, L43, L45, L47, L49, L58, L62, L66, L68, L70, L73, L80, L85, L88, L99`
- Error-message mismatch: `L31`
- Total current file-local failures: `20`

Notes:
- This is still a semantic/runtime issue, not a parser gating issue.
- The current failure count is `20`, not `19` as in the previous snapshot.

## `language/derror.dark`

Status: blocked.

What is fixed relative to the previous plan:
- The old roundtrip parser blocker at `L2` no longer blocks execution.
- The old suite-level preamble parse error no longer blocks execution.

Current failures from `./run-tests --ai --filter=derror`:
- Error-message mismatch at `L2`
- Runtime error propagation / message mismatches at `L10, L13, L15, L16, L17, L18, L19, L21, L22, L23, L25, L32`
- Total current file-local failures: `13`

Notes:
- `derror.dark` has moved from syntax/preamble blockers to semantic error-behavior mismatches.
- It is still not enabled in the upstream default subset and has no line allowlist yet.

## `language/elambda.dark`

Status: blocked.

Current blocking failure:
- `./run-tests --ai --filter=elambda` still aborts before per-test reporting with:
- `payloadSize: Record type 'runtime' not found in typeReg`
- Stack root remains in `passes/2.5_RefCountInsertion.fs` via `ANF.payloadSize`.

Notes:
- This is the same blocker recorded in the previous plan.
- No partial enablement is present in `TestRunner.fs`.

## `stdlib/date.dark`

Status: partially enabled, but not yet part of the default upstream subset.

Current runner state:
- Not included in `defaultUpstreamDarkPaths`.
- Gated by `upstreamEnablementLineAllowlist`.
- Enabled line: `216`

What is fixed relative to the previous plan:
- The old roundtrip parser blocker around the preamble `let p` / `|> Stdlib.Result.map` form no longer blocks the targeted run.
- The old suite-level preamble type error (`Stdlib.Result.map expects 2 arguments, but got 3 arguments`) no longer blocks the targeted run.

Evidence:
- `./run-tests --ai --filter=date.dark` passes under the current gate.

Notes:
- The only allowlisted line is the commented `today_v0` case at `L216`, so the rest of the file remains skipped.
- This file still needs real line-by-line enablement work before it belongs in the default upstream subset.

## Additional `--roundtrip-all-dark` blocker

Status: fixed.

Previous blocker:
- File: `src/Tests/e2e/upstream/cloud/db.dark`
- Test: `L112` (`NestedRecordFieldAccess`)
- Failure mode: `ParseOriginalFailed`

Current result:
- The old failure no longer reproduces with:
- `./run-tests --ai --filter=NestedRecordFieldAccess --roundtrip-all-dark`
- `./run-tests --ai --filter=db.dark --roundtrip-all-dark`

Notes:
- The targeted all-upstream roundtrip check now completes without reproducing the previous parse failure.

## Next steps

1. Continue incremental ungating for `eapply.dark`, `aliases.dark`, and `date.dark`.
2. Fix pipe semantics in `epipe.dark`; this is still the largest remaining functional blocker in this set.
3. Fix runtime error propagation / messaging behavior in `derror.dark`.
4. Fix the `RefCountInsertion` / `payloadSize` crash to unlock `elambda.dark`.
5. After each change, re-run `./run-tests --ai` and only remove gates when the file is green without them.
