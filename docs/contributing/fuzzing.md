# Differential compiler fuzzing

The F# fuzzer generates valid expressions directly as the compiler's
`AST.Expr`, formats them with `ASTPrettyPrinter`, and compares the compiled
native result with `darklang-interpreter eval`. The interpreter is the semantic
oracle; the fuzzer does not implement a second evaluator or language model.

Build the fuzzer, then run a bounded campaign:

```bash
./fuzz --seed 1234 --cases 1000 --max-depth 6 --timeout-ms 2000
```

The seed is always printed. Before compiling each case, the tool writes its
source to `fuzz-results/current.dark`, so an unexpected compiler-process crash
still leaves a reproducer. A normal discrepancy also produces a `.dark` source
file and a `.txt` result file named with the seed and case index. Replay an
artifact through the same oracle and compiler path with:

```bash
./fuzz --replay fuzz-results/seed-1234-case-5.dark
```

Minimize a reproducer with the deterministic structural reducer:

```bash
./fuzz --minimize fuzz-results/seed-1234-case-5.dark
```

The reducer repeatedly tries smaller compiler ASTs and retains a candidate only
when the interpreter accepts it and the compiler reproduces the same failure.
It writes `seed-1234-case-5.min.dark` beside the original. The reducer is local
and deterministic; it does not use an AI service or implement a second
evaluator.

The first generator deliberately covers a small, total subset: `Int64`,
`Bool`, and `String` literals, variables, `let`, `if`, arithmetic, comparisons,
boolean operations, and string concatenation. Top-level observations are
`Int64` or `Bool`, whose interpreter and native renderings are directly
comparable; strings still participate in nested expressions and comparisons.
Add constructs directly to the existing AST generator rather than introducing
a parallel language model.

Every discrepancy should be minimized and promoted to a focused E2E test
before changing compiler behavior.
