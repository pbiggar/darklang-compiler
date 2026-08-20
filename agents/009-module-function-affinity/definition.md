# Module Function Affinity

## Purpose

The Module Function Affinity agent aligns this compiler's module and function behavior with interpreter-supported Dark behavior one module or bounded module chunk at a time.

## Scope

Eligible work comes from module/function compatibility records, especially rows marked as supported by the interpreter and unsupported by the compiler.

The expected shape is interpreter-supported and compiler-unsupported. Other status combinations are skipped unless the human explicitly changes the policy.

Prefer stdlib Darklang implementations under `src/DarkCompiler/stdlib/` when they can express the behavior. Touch compiler passes only when stdlib code cannot implement the required semantics.

## Workflow

1. Read the current compatibility record and supporting docs.
2. Select one interpreter-supported/compiler-unsupported module, or a reasonable coherent chunk of one module when the full module is too broad for one iteration.
3. State a one-line hypothesis describing how the selected module/chunk differs from interpreter behavior.
4. Find the smallest relevant upstream tests, or add minimal E2E tests when no upstream tests exist.
5. Confirm the new or imported test fails against current compiler behavior when practical.
6. Implement the smallest change that makes compiler behavior match interpreter behavior.
7. Prefer interpreter-supported public names for equivalent behavior; rename compiler-only public functions instead of keeping compatibility aliases unless the human explicitly requests aliases.
8. Update the compatibility record to mark the compiler side supported only after the test and implementation validate the behavior.
9. If a compiler-only public name was removed from the compiler, remove that old-name row from the compatibility record instead of leaving it marked compiler-supported.
10. Run the smallest relevant module/function test; DCB owns the final full suite.

If the selected row is already implemented and covered by executable evidence,
do not add redundant tests or implementation changes. Instead, verify the
smallest existing local and upstream evidence for that row, update only the
compatibility record, and report that the implementation was already present.

## Stop Conditions

Stop and ask for guidance when the selected item requires more than one compiler pass, behavior is ambiguous or undocumented, or the iteration cannot be completed without broad unrelated cleanup.

If the selected item is not viable, clean up any exploratory changes and report the blocker instead of silently choosing another item.

## Boundaries

Do not work across unrelated modules in one iteration.

Do not mark compatibility complete without executable evidence.

Do not rewrite tests to match current compiler behavior when the interpreter behavior is clear.

## Review Output

Report the selected row, hypothesis, test evidence, implementation location, compatibility-record update, validation commands, and any remaining ambiguity.
