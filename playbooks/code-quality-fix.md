# Fix one small code quality issue

This playbook guides making exactly one small code quality fix in the compiler without broad refactors.

You are going to fix EXACTLY ONE code quality issue that does not require adding a new test.

1. Identify a small code quality issue that is already well tested. Pick ONE issue and ignore others. Acceptable examples:

   - Unhandled `Option`/`Result` case that crashes or hides a real error
   - Incorrect or missing compile-time error message for a specific input
   - Unsafe list access or pattern match that should be explicit
   - Missing validation for a specific AST/IR invariant
   - Redundant tests that cover identical behavior (remove or consolidate)
   - Wrapper functions that do not add value (inline or remove)

   Avoid: broad refactors, naming-only changes, formatting-only changes, re-architecture.

2. Do not add a new test. Cite the existing test(s) that already cover the case.

3. Implement the fix with the smallest possible code change.

   - Pure functional F# only: no mutation, no exceptions
   - Use `Result`/`Option` patterns from `docs/result-patterns.md`
   - Make invalid states unrepresentable with precise domain types and
     structurally related data. Use `Option` only for genuine semantic absence.
   - When adding or retaining an optional field, review whether a discriminated
     union, a required field in the relevant case, or removing the field models
     the state more precisely.
   - If a value truly cannot be known, use `Crash.crash` with a clear message
   - Do not retain defaults, shims, or special-case workarounds for unsupported
     states. Compatibility must be explicitly documented as supported behavior.
   - If the change migrates a representation, complete it: retain one
     authoritative representation and remove superseded paths.
   - Add a brief comment only if the logic is non-obvious to a senior compiler engineer

4. Run the full test suite (`./run-tests --ai`). Fix the compiler, not the tests, until all tests pass.

5. Run the routine benchmarks (`./benchmarks/run_benchmarks.sh routine`). Benchmarks should complete with no failures. If `RESULTS.md` changes, record the new "Performance ratio: X.XX" in your report.

6. Update documentation if the fix changes behavior or clarifies an invariant.

7. Write a short report to the developer. Include:

   - The issue fixed (file path + short description)
   - The existing coverage cited for the change
   - Why that coverage exercises the fixed path
   - Any benchmark impact (include the Performance ratio if `RESULTS.md` changed)

8. Follow the current repository and orchestrator review workflow for committing,
   landing, and benchmark-result files. Do not let this playbook override the
   active workflow for the environment you are using.

## Policies for code quality fixes

- One issue only. If you notice other issues, list them in the report but do not fix them.
- Prefer E2E tests; use other test types only with explicit justification.
- No behavior changes without a test demonstrating the expected behavior.
- Keep the change local to one or two files; avoid sweeping refactors.
- Maintain pure functional style and Result-based error handling.
- Before finishing a representation change, confirm optional fields describe
  genuine absence and that completed migrations leave no old representation or
  compatibility workaround behind.
