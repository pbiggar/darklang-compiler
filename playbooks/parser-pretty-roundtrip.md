# Parser/Pretty Roundtrip Playbook

Goal: fix parser/pretty-printer roundtrip failures one-by-one, always starting from the first failing corpus case.

## Run the corpus test
1. Run:
   - `./run-tests --ai`
   - `./run-tests --ai --roundtrip-all-dark` (includes all upstream `.dark` files in the roundtrip corpus)
2. The suite runs by default (the `--parser-pretty-roundtrip` flag is a legacy no-op).
3. Use `--roundtrip-all-dark` when systematically fixing upstream `.dark` roundtrip issues.
4. The suite stops at the first failure and prints:
   - failure kind (`ParseOriginalFailed`, `PrettyPrintFailed`, `ParsePrettyFailed`, `PrettyPrintSecondPassFailed`, `AstChangedAfterRoundtrip`, `PrettyNotIdempotent`)
   - roundtrip check (`source-syntax` or `interpreter-syntax`)
   - file name
   - test name
   - snippet type (`preamble`, `source`, or `expected-value`)
   - parse/pretty/reparse syntax details and `allowInternal` setting
   - original and pretty-printed code (plus parse error when relevant)

## Fix one failure at a time
1. Take only the first failing case from the command you ran.
2. Add a minimal regression test first:
   - For parser/pretty formatting regressions, add one expression to:
     - `src/Tests/formatting-roundtrip/compiler.roundtrip`
   - Prefer `src/Tests/e2e/*.e2e` when it is user-visible syntax behavior.
   - Use `src/Tests/SyntaxInteropTests.fs` only for narrowly scoped parser/printer rules.
3. Confirm the regression fails:
   - Re-run the same command (`./run-tests --ai` or `./run-tests --ai --roundtrip-all-dark`)
4. Implement the smallest fix in:
   - `src/DarkCompiler/passes/1_Parser.fs`
   - `src/DarkCompiler/passes/1_InterpreterParser.fs`
   - `src/DarkCompiler/ASTPrettyPrinter.fs`
5. Re-run:
   - Re-run the same command (`./run-tests --ai` or `./run-tests --ai --roundtrip-all-dark`)
6. Repeat from step 1 for the next failure.

## Notes
1. Keep the corpus suite strict; do not weaken checks.
2. Always let the next iteration start from the first currently failing roundtrip case.
