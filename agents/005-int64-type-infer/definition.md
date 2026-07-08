# Int64 type infer

Fix exactly one invalid compiler type-checking or type-propagation issue per iteration where the compiler incorrectly chooses `TInt64` as a fallback/default.

## Scope

This agent works only on plausible invalid `TInt64` default or fallback use in compiler inference, checking, or propagation logic. It does not perform broad numeric type-system cleanup, parser work, runtime work, optimization work, or unrelated refactors unless that work is directly required by the selected `TInt64` default fix.

## Iteration Workflow

Run one isolated, reviewable iteration at a time.

1. Build a candidate pool using static source search only. Include source locations where `TInt64` appears to be used as a fallback, placeholder, default, or guessed concrete type in inference or propagation logic.
2. Exclude legitimate `TInt64` uses, such as:
   - explicit integer literal representation
   - runtime tags
   - ABI-width behavior
   - representation-only pointer, tag, and function-address bookkeeping
   - monomorphized empty collection intrinsics lowered to null pointers
   - optimizer-created `Int64`-specific unary negation canonicalizations
   - backend register tracking
   - diagnostic-only legacy error formatting after a source expression has already been proven `Int64`
   - tests whose purpose is already explicit `Int64` behavior
   - any site that is not plausibly an invalid default
3. If no plausible candidates remain, report that as the iteration result instead of expanding scope.
4. Randomly select exactly one candidate from the filtered pool. Do not choose by ease, expected impact, local familiarity, or deterministic ordering.
5. Investigate only the selected candidate deeply enough to decide whether it can be exposed by a small end-to-end language test.
6. When testable, add the failing E2E regression test first. The test should demonstrate the invalid default behavior rather than merely covering the touched code.
7. Remove the bad `TInt64` default without substituting another guessed default. Prefer preserving unknown or unresolved type information, threading explicit type information, or returning `Result` where uncertainty must be represented.
8. Use `Crash.crash` only when a concrete type is genuinely impossible to avoid and the crash documents an impossible compiler state. Do not use it as a replacement default.
9. Fix downstream propagation, checking, or code generation issues exposed by the regression test without weakening the test or changing it to match current broken behavior.
10. If the selected candidate cannot be exposed by a small E2E test after investigation, stop and report that dead end as the reviewable iteration. Do not silently select another candidate in the same iteration.
11. Include documentation updates only when the work produces a durable lesson that future iterations need. Do not add documentation just to satisfy a checklist.
12. Commit only coherent sandbox work for the selected candidate, plus any durable documentation that belongs with that candidate.

## Review Output

For each iteration, report the selected candidate, why it was considered a plausible invalid `TInt64` default, the regression behavior or dead-end reason, what changed, the verification evidence required by shared policy, any benchmark result file impact if meaningful to the candidate, and residual risks or follow-up questions.
