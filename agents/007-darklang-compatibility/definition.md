# Darklang Compatibility

## Purpose

The Darklang Compatibility agent reduces differences between this compiler and the official Darklang interpreter one confirmed difference at a time.

## Scope

Work on exactly one potential compatibility difference per iteration.

Eligible differences come from E2E tests, validation skips, documented Darklang differences, or direct comparison with `darklang-interpreter`.

The interpreter is authoritative for supported Darklang syntax and semantics. Missing interpreter features, compiler-only internal features, and tooling differences are not compiler bugs by themselves.

## Selection Workflow

1. Select one E2E test area or skipped validation case to inspect.
2. Identify the concrete expression, file, expected result, and skip or difference reason.
3. Convert the expression to official interpreter syntax when needed.
4. Run or reason from `darklang-interpreter` evidence to classify the case as:
   - equivalent behavior,
   - real compiler/interpreter difference,
   - interpreter limitation,
   - interpreter bug,
   - tooling-only difference.
5. Stop after one actionable classification.

## Fix Workflow

When the selected case is a real compiler difference:

1. Add or update the smallest E2E test that captures the authoritative interpreter behavior.
2. Confirm the test exposes the current compiler difference when practical.
3. Fix compiler, runtime, parser, or stdlib behavior in the smallest appropriate location.
4. Update validation skip rules only as narrowly as the fixed behavior allows.
5. Update `docs/darklang-differences.md` when the documented compatibility status changes.
6. Validate with the full test suite and any targeted interpreter validation used to prove the case.

## Boundaries

Do not change expected behavior to match current compiler output when the interpreter provides the supported behavior.

Do not handle multiple skipped tests in one iteration.

Do not fix interpreter bugs in this repository.

Do not broaden skip rules to hide a confirmed compiler difference.

## Review Output

Report the investigated test, classification, interpreter evidence, compiler change if any, validation commands, documentation updates, and residual risks.
