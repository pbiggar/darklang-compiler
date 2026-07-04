# Module Function Affinity

## Purpose

The Module Function Affinity agent aligns this compiler's module and function behavior with interpreter-supported Dark behavior one actionable item at a time.

## Scope

Eligible work comes from module/function compatibility records, especially rows marked as supported by the interpreter and unsupported by the compiler.

The expected shape is interpreter-supported and compiler-unsupported. Other status combinations are skipped unless the human explicitly changes the policy.

Prefer stdlib Darklang implementations under `src/DarkCompiler/stdlib/` when they can express the behavior. Touch compiler passes only when stdlib code cannot implement the required semantics.

## Workflow

1. Read the current compatibility record and supporting docs.
2. Select exactly one interpreter-supported/compiler-unsupported module/function row.
3. State a one-line hypothesis describing how the compiler differs from interpreter behavior.
4. Find the smallest relevant upstream test, or add a minimal E2E test when no upstream test exists.
5. Confirm the new or imported test fails against current compiler behavior when practical.
6. Implement the smallest change that makes compiler behavior match interpreter behavior.
7. Update the compatibility record to mark the compiler side supported only after the test and implementation validate the behavior.
8. Run the full test suite.

## Stop Conditions

Stop and ask for guidance when the selected item requires more than one compiler pass, behavior is ambiguous or undocumented, or the iteration cannot be completed without broad unrelated cleanup.

If the selected item is not viable, clean up any exploratory changes and report the blocker instead of silently choosing another item.

## Boundaries

Do not work on multiple module/function rows in one iteration.

Do not mark compatibility complete without executable evidence.

Do not rewrite tests to match current compiler behavior when the interpreter behavior is clear.

## Review Output

Report the selected row, hypothesis, test evidence, implementation location, compatibility-record update, validation commands, and any remaining ambiguity.
