# Upstream `.dark` Tests Enablement Playbook
## Goal
Enable all upstream `.dark` tests by default, one test at a time, while proving each enabled case is actually executed by the test runner.

## Contract (Non-Negotiable)
1. Upstream test expression semantics, expected values, and expected error text/meaning are the contract.
2. Replace `Builtin.testDerrorMessage` assertions with `error="..."` assertions carrying the same message (or equivalent wording).
3. Never edit assertions to match current compiler behavior when that changes test behavior.
4. Fix compiler/runtime/parser behavior to satisfy upstream tests.
5. A run does not count unless `./run-tests --ai --filter=<file>.dark` executes tests for the target file (never `0/0`).
6. Ensure the target file is exercised by the runner for this iteration (temporary runner inclusion is acceptable; permanent inclusion is optional).
7. The iteration is complete only when the target file's filtered run passes and full `./run-tests --ai` stays green.

## Working Rules
1. Enable exactly one skipped test in one `.dark` file per iteration.
2. Ensure the target file is exercised by the runner in this iteration; if `--filter=<file>.dark` returns `0/0`, adjust runner inclusion (temporarily if needed) or use an equivalent runner path that executes the file.
3. Run tests immediately after enabling the case.
4. Convert `Builtin.testDerrorMessage` assertions to `error="..."` with the same message intent.
5. Do not rewrite expected behavior/output/error text to match current compiler behavior.
6. If tests fail, fix compiler/runtime/parser behavior so the enabled test and target file pass.
7. Record only reusable learnings in this file (no per-iteration change log entries).
8. If blocked and guidance is needed, ask user and record both question and answer here.

## Iteration Loop (Single-Test)
1. Pick one disabled upstream case (`skip=...` or a commented assertion) from a `.dark` file.
2. Ensure the runner can execute that file for this iteration (if `./run-tests --ai --filter=<file>.dark` is `0/0`, adjust runner inclusion temporarily or use an equivalent runner entrypoint).
3. Uncomment/enable the existing upstream assertion; if it uses `Builtin.testDerrorMessage`, rewrite it to an equivalent `error="..."` assertion.
4. Run `./run-tests --ai --filter=<file>.dark` and confirm tests execute (not `0/0`).
5. Diagnose with `./dark --syntax=interpreter -r -e '<expr>'` when output/error text is unclear.
6. Resolve by fixing compiler/runtime/parser behavior until the filtered file run is passing.
7. Run full `./run-tests --ai` and keep the suite green.
8. Commit.

## Runner Execution Requirement
1. Do not accept `0/0` as success for the target file.
2. If `./run-tests --ai --filter=<file>.dark` reports `0/0`, change test execution setup so the runner executes that file (temporary inclusion is acceptable).
3. Re-run full `./run-tests --ai` after any execution setup change.
4. Record only general lessons, not per-iteration change logs.

## Order:
Go in this order for re-enabling tests
- src/Tests/e2e/upstream/language/apply/eapply.dark
- src/Tests/e2e/upstream/language/custom-data/aliases.dark
- src/Tests/e2e/upstream/language/flow-control/epipe.dark
- src/Tests/e2e/upstream/language/elambda.dark
- src/Tests/e2e/upstream/stdlib/list.dark
- src/Tests/e2e/upstream/stdlib/string.dark
- src/Tests/e2e/upstream/stdlib/date.dark
- src/Tests/e2e/upstream/stdlib/dict.dark
- src/Tests/e2e/upstream/stdlib/float.dark
- src/Tests/e2e/upstream/stdlib/http.dark
- src/Tests/e2e/upstream/stdlib/json.dark
- src/Tests/e2e/upstream/stdlib/alt-json.dark
- src/Tests/e2e/upstream/stdlib/nomodule.dark
- src/Tests/e2e/upstream/language/derror.dark
- src/Tests/e2e/upstream/language/interpreter.dark
- src/Tests/e2e/upstream/stdlib/httpclient.dark

## General Learnings
| Situation | How to Identify It | What to Do | How to Validate |
| --- | --- | --- | --- |
| Upstream assertion semantics/text are the contract | Enabled case fails because stderr/value differs from upstream expectation. | Fix compiler/runtime/parser behavior; do not rewrite assertions to fit current output. | Re-run full `./run-tests --ai`. |
| `Builtin.testDerrorMessage` migration | Assertion uses helper form. | Replace with equivalent `error="..."` message text. | Re-run full `./run-tests --ai` and confirm same failure path. |
| One-case iteration scope | Nearby commented/skipped cases tempt batch edits. | Enable one case in one file only; still fix any failures exposed in that file. | Re-run `./run-tests --ai --filter=<file>.dark`, then full `./run-tests --ai`. |
| Runner execution requirement | `--filter=<file>.dark` reports `0/0` (file not executing). | Update execution setup so the runner executes the file (temporary inclusion is acceptable). | Confirm filtered run executes tests and passes, then run full `./run-tests --ai`. |
| Probe-first debugging | Full test failure does not clearly show parser/type/runtime root cause. | Use `./dark --syntax=interpreter -r -e '<expr>'` (and compiler syntax probes when relevant). | Apply fix, then re-run full `./run-tests --ai`. |
| Parser blockers are real blockers (no test rewrites) | Upstream syntax forms fail to parse (for example `<'a>`, `{}`, backticked fields, multiline forms). | Implement parser support in the correct parser (`1_InterpreterParser.fs` for interpreter syntax); if blocked, escalate with a concrete blocker note. | Add focused parser tests, run filtered file tests, then run full `./run-tests --ai`. |
| Parser/test-format fixes can regress other files | A local parsing or E2E formatting change makes one upstream file pass but breaks others. | Validate neighboring upstream files and avoid narrow ad-hoc fixes; prefer general parser/format logic. | Run targeted filters for touched neighbors, then full `./run-tests --ai`. |
| Full-file preamble parsing/typechecking can mask per-test behavior | Many tests in a file fail before assertions execute due to preamble parse/type errors. | Improve preamble handling (for example per-test/partitioned preambles) so unrelated tests can run. | Re-run filtered file tests and confirm assertions execute, then run full `./run-tests --ai`. |
| Legacy runtime/stdlib names may require compatibility shims | `Undefined variable` for upstream names (`*_v0` or legacy module paths). | Add compatibility aliases/lookup support; do not rewrite upstream source to new names. | Reproduce targeted case, then run full `./run-tests --ai`. |
| `Builtin.testRuntimeError` contracts must survive typing/short-circuiting | Branch/boolean/arith cases fail with static type errors instead of uncaught-exception behavior. | Preserve upstream runtime-error behavior and adjust typing/short-circuit rules accordingly. | Enable one failing case and run full `./run-tests --ai`. |
| Pattern mismatch diagnostics must stay contract-compatible | Generic mismatch text appears where upstream expects `Cannot match ...` wording. | Preserve legacy-compatible mismatch messages for scalar/list/tuple/constructor cases. | Re-run full `./run-tests --ai` and confirm exact stderr. |
| Underspecified language semantics must be made explicit | Test expectations conflict because behavior is not yet clearly defined (for example value-equality or character semantics). | Define the semantics first; keep only directly affected assertions skipped with an explicit blocker note until implemented. | Add/adjust focused tests for the decided semantics, then run filtered file tests and full `./run-tests --ai`. |
| Date/time fixed-value assertions need deterministic semantics | `today`-style assertions rely on wall-clock date and/or missing legacy APIs. | Keep disabled until deterministic clock strategy and `Stdlib.DateTime.*_v0` compatibility are defined. | Document blocker and run full `./run-tests --ai`. |

## Guidance Log (Paul)
| Date | Question Asked | Why It Was Needed | Paul’s Guidance | Applied |
| --- | --- | --- | --- | --- |
| _pending_ | _none yet_ | _n/a_ | _n/a_ | _n/a_ |
