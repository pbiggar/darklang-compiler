# Ralph Loop: Interpreter/Compiler Affinity

Only supported status:
1. `Int ✅ / Comp ❌`: implement compiler or stdlib behavior to match the interpreter. All other status combinations are skipped.

Prep (once per session)
1. Read `docs/adding-features.md`, `docs/result-patterns.md`, and `docs/verification.md`.
2. Open `interpreter-compiler-modules.md` and choose a row with `Int ✅ / Comp ❌`.

Loop (one actionable item per iteration)
1. Write a one-line hypothesis: “Compiler differs from interpreter for X when input is Y.”
2. Find tests for the function in `https://github.com/darklang/dark`.
3. Copy the smallest relevant test(s) into `src/Tests/e2e/` and adapt only what is necessary.
4. If no upstream test exists, add a minimal E2E test yourself.
5. Run `./run-tests --ai` and confirm the new test fails.
6. Implement the smallest change to match interpreter behavior.
7. Default assumption: implement in stdlib Darklang code under `src/DarkCompiler/stdlib/*.dark`.
8. Only touch compiler passes if the stdlib cannot express the behavior.
9. Follow repo rules: pure functional style, `Result`/`Option`, no exceptions, no mutation, no defaults.
10. Run `./run-tests --ai` and confirm the test passes.
11. Update `interpreter-compiler-modules.md` to flip `Comp` to `✅`.
12. Run any additional validation required by `docs/verification.md`, including benchmarks when the compiler, stdlib, tests, or benchmark-adjacent areas changed.
13. Report the chosen module/function, behavior evidence, implementation summary, validation results, and residual risk for review.

Stop criteria
1. If the change requires more than one compiler pass, stop.
2. If behavior is ambiguous or undocumented, stop.
3. If you cannot complete the loop for the chosen item, abandon it, clean up all changes from this iteration, then quit.
