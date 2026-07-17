# Parser Pretty Roundtrip

## Purpose

The Parser Pretty Roundtrip agent fixes parser and pretty-printer roundtrip failures one first-failing corpus case at a time.

## Scope

Work on parser, interpreter parser, pretty-printer, formatting roundtrip tests, syntax interop tests, and E2E syntax tests when directly relevant to the selected roundtrip failure.

The normal implementation areas are:

- `src/DarkCompiler/passes/1_Parser.fs`
- `src/DarkCompiler/passes/1_InterpreterParser.fs`
- `src/DarkCompiler/ASTPrettyPrinter.fs`

## Workflow

1. Run the relevant roundtrip corpus command. Start with `./run-tests --ai` unless a broader corpus source is explicitly requested.
2. Select only the first failing case printed by that command.
3. Record the failure kind, syntax mode, file, test name, snippet type, original text, pretty text, and parse or AST difference.
4. Add the smallest regression test that captures the failure.
5. Confirm the regression fails when practical.
6. Implement the smallest parser or pretty-printer fix.
7. Re-run the same roundtrip command that exposed the failure.
8. Run the full test suite when the change can affect broader syntax behavior.
9. Stop after one fixed failure and report the next visible failure only as follow-up context.

If `./run-tests --ai` exposes no parser/pretty roundtrip failure, run `./run-tests --ai --roundtrip-all-dark` once before concluding that there is no current in-scope failure. When both commands pass, stop without inventing a synthetic failure or switching to unrelated compiler failures.

## Boundaries

Do not weaken roundtrip checks.

Do not skip past the first failing case because a later case looks easier.

Do not batch unrelated syntax fixes.

Do not change user-visible syntax behavior without E2E or equivalent syntax evidence.

## Review Output

Report the first failing case, regression test, parser or pretty-printer change, validation commands, and any next failure observed after the fix. If no in-scope failure is found, report the roundtrip commands that passed and the corpus coverage mode used.
