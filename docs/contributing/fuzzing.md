# Differential compiler fuzzing

The F# fuzzer generates valid expressions directly as the compiler's
`AST.Expr`, formats them with `ASTPrettyPrinter`, and compares the compiled
native result with `darklang-interpreter eval`. The interpreter is the semantic
oracle; the fuzzer does not implement a second evaluator or language model.

Run the continuous fuzz-to-E2E workflow from the repository root:

```bash
./fuzz
```

When launched from the primary coordination checkout, the script first creates
a dedicated campaign worktree from the configured local integration ref and
continues there before building or writing any artifacts.

The fuzzer chooses and prints a seed, then generates cases without a fixed case
limit. Use optional flags when a run needs to be reproducible or tuned:

```bash
./fuzz --seed 1234 --timeout-ms 5000 --depth 8
```

Before compiling each case, the fuzzer writes its source to the campaign's
`current.dark`, so an unexpected compiler-process crash still leaves a
reproducer. When a discrepancy is found, the script preserves the original
source and diagnostic and always minimizes the source with the deterministic
AST reducer. It displays the reduced case, the result expected by the
interpreter, and the compiler's actual result, then asks whether Codex should
start a fix. The reducer itself is local and deterministic; it does
not use an AI service or implement a second evaluator.

If accepted, the controller creates a branch and worktree from the configured
local integration ref. Non-interactive Codex adds the focused failing E2E test,
fixes and commits the compiler, then stops without landing. The controller
checks the clean commit, reruns the full build, tests, and parent-relative
benchmark gate, builds and publishes the fuzzer, and replays the minimized case.
It shows the commit, diff summary, and validation logs before asking for
separate approval to run `./land`. Declining that approval preserves the branch
and stops the loop. If the start approval is declined, the saved finding remains
in `fuzz-results/` and the loop continues without changing tracked tests.

After `./land` prints `queued`, the controller fuzzes with the published binary
from that exact fix. Before creating another fix branch, it waits for the prior
commit (or a patch-equivalent replay of it) to appear in the configured local
integration ref. It does not fetch or inspect the merge-train queue. If another
discrepancy appears first, its minimized source remains saved while the loop
waits. Each new fix then starts from the updated integration ref, so it contains
the earlier fix.

The first generator deliberately covers a small, total subset: `Int64`,
`Bool`, and `String` literals, variables, `let`, `if`, arithmetic, comparisons,
boolean operations, and string concatenation. Top-level observations are
`Int64` or `Bool`, whose interpreter and native renderings are directly
comparable; strings still participate in nested expressions and comparisons.
Add constructs directly to the existing AST generator rather than introducing
a parallel language model.

Every accepted compiler fix starts with a focused failing E2E test before
changing compiler behavior. The script runs until interrupted with Ctrl-C.
