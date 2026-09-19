# Differential compiler fuzzing

The F# fuzzer generates valid expressions directly as the compiler's
`AST.Expr`, formats them with `ASTPrettyPrinter`, and compares the compiled
native result with `darklang-interpreter eval`. The interpreter is the semantic
oracle; the fuzzer does not implement a second evaluator or language model.

Run the continuous fuzz-to-E2E workflow from the repository root:

```bash
./fuzz
```

The fuzzer chooses and prints a seed, then generates cases without a fixed case
limit. Use optional flags when a run needs to be reproducible or tuned:

```bash
./fuzz --seed 1234 --timeout-ms 5000 --depth 8
```

Before compiling each case, the fuzzer writes its source to the campaign's
`current.dark`, so an unexpected compiler-process crash still leaves a
reproducer. When a discrepancy is found, the script preserves the original
source and diagnostic, always minimizes the source with the deterministic AST
reducer, asks the interpreter for the expected result, and appends the
deduplicated case to `src/Tests/e2e/fuzzer-found.e2e`. It then starts another
unbounded campaign. The reducer is local and deterministic; it does not use an
AI service or implement a second evaluator.

The first generator deliberately covers a small, total subset: `Int64`,
`Bool`, and `String` literals, variables, `let`, `if`, arithmetic, comparisons,
boolean operations, and string concatenation. Top-level observations are
`Int64` or `Bool`, whose interpreter and native renderings are directly
comparable; strings still participate in nested expressions and comparisons.
Add constructs directly to the existing AST generator rather than introducing
a parallel language model.

Every discrepancy should remain as a focused failing E2E test before changing
compiler behavior. The script runs until interrupted with Ctrl-C and
deliberately does not run promoted tests, which are expected to fail until the
compiler bug is fixed.
