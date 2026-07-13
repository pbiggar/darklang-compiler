# Fix one type checking issue

The compiler is filled with places where a default TInt64 type is used. This leads to incorrect code, a failure to warn about real type errors, and hidden bugs.

You are going to resolve EXACTLY ONE type checking issue.

2. Find a place in the compiler where the compiler has chosen TInt64 as a default type, even though it is incorrect. Choose at random, do not pick deterministically, including by difficulty.

3. Remove the default type assumption, using Result types if possible for error handling, and the `crash` function if that isn't possible. Add an E2E test which highlights the problem.

4. In some cases, the removed assumption will lead to a problem later in the compiler, where types are not properly propagates, or later passes did not handle this type. Fix these bugs. NEVER CHANGE THE TEST TO ALLOW THE TEST TO PASS (unless the test is actually wrong, which is unlikely). NEVER CHANGE OTHER TESTS.

5. Run the test suite (`./run-tests --ai`). If tests now start to fail, attempt to fix the compiler to make the tests pass. Repeat until all tests pass. RUN ALL TESTS, not a subset. The test suite is very fast.

6. If the failing tests are incorrect and should be failing, continue and tell the developer.

7. After all tests pass, run all Dark benchmarks (`./benchmarks/run_benchmarks.sh all`). Benchmarks should complete with no failures. If RESULTS.md has changed, show the results.

8. If there is any valuable documentation to create or information to capture, create a doc or add it to an existing doc.

9. After all this, write a short report to the developer about this issue. Include what assumption was removed along with some context, what test was added (show it!). Explain what the test does before and after the change, and how the was fixed by the compiler changes, and what changes had to be made to the compiler to address the issue. If there is any change to the benchmark result in RESULTS.md, SHOW THE CHANGE!

10. DO NOT COMMIT OR MERGE UNTIL I SAY "approved". After approval, commit the code, new tests, and new benchmark results. Include in the commit message a large discussion of the issue and the choices and assumptions made. Rebase off main branch if necessary, rerun tests if so, and land using `./scripts/land-on-main.sh`.

## Policies for handling removed type defaults

- Do not reintroduce a default type for missing type arguments. Preserve unresolved type variables and let later passes surface any problems.
- If a later pass needs a concrete type (for example, monomorphized intrinsics like `__hash<k>`/`__key_eq<k>`), add explicit `*_unknown` intrinsics that crash at runtime rather than picking an arbitrary type.
- Do not change the test under any circumstances
- Exclude legitimate `TInt64` uses from the candidate pool, including:
  - explicit integer literal representation
  - runtime tags
  - ABI-width behavior
  - representation-only pointer, tag, and function-address bookkeeping
  - monomorphized empty collection intrinsics lowered to null pointers
  - backend register tracking
  - diagnostic-only legacy error formatting after a source expression has already been proven `Int64`
  - concrete source-level `Int64` container specialization, ownership, or refcount handling, unless investigation shows that type was guessed upstream
  - tests whose purpose is already explicit `Int64` behavior
  - unary negation paths that operate only after operand type checking has already selected a numeric type
  - any site that is not plausibly an invalid default
