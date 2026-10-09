# Differential compiler testing

The native OCaml differential testing tool constructs typed compiler ASTs, formats them with
`ASTPrettyPrinter`, and compares generated native programs with
`darklang-interpreter eval`. The interpreter defines the expected result;
the differential testing tool contains no evaluator.

## Campaigns

Build the pinned toolchain and put `darklang-interpreter` on PATH:

```bash
./build --ai
./scripts/install-darklang-interpreter.sh
./differential-test --seed 1234 --depth 6 --timeout-ms 2000
```

`./differential-test` runs until interrupted. It creates a campaign worktree when launched
from the primary coordination checkout. Before each compilation, it writes
`current.dark` under its ignored `differential-test-results/` directory. A finding preserves
the original program and diagnostic, then runs deterministic syntax reduction.
Every smaller candidate is parsed and executed through both the interpreter
and compiler. A reduction must preserve the result type and failure category.
Programs rejected by the interpreter are skipped; a missing or timed-out
oracle stops the campaign rather than being reported as a compiler discrepancy.

The generator covers scalar literals, tuples, lists, dictionaries, records and
updates, Option/Result patterns, guards, lambdas, generic functions, bounded
direct and mutual recursion, interpolation, Blob, DateTime, and Stream values.
The final observation is Int64 or Bool. The reducer operates on the compiler's
parsed syntax and removes expressions, declarations, match arms and guards.
Unicode source ranges are converted to byte offsets before splicing.

## Replay and bounded runs

The executable can be used without the interactive controller:

```bash
_build/default/tools/differential-testing/main.exe --seed 1234 --limit 100 --artifacts /tmp/differential-test
_build/default/tools/differential-testing/main.exe --replay /tmp/differential-test/finding.dark
_build/default/tools/differential-testing/main.exe --minimize /tmp/differential-test/finding.dark
_build/default/tools/differential-testing/main.exe --generate 100 --seed 1234 --artifacts /tmp/generated
```

`--interpreter PATH` selects the oracle. `--generate` writes programs without
running either implementation. All process timeouts are in milliseconds.
Replay exits successfully only when both implementations accept the source
and agree. Minimized sources are written next to the input as `.min.dark`.

## Approved fixes

After reduction, the controller shows the source, expected result and actual
result. It asks before launching Codex in a separate fix worktree. Codex adds a
failing E2E test, fixes the compiler and commits, without landing.

The controller verifies the clean commit, full build, complete host suite,
parent-relative benchmark gate and replay. It copies the native differential testing tool from
that exact commit into an isolated runtime directory, shows the review evidence,
and asks separately before running `./land`. Declining preserves the fix branch.

After a queued handoff, the controller uses that committed runtime for the next
campaign. Before starting another fix, it waits for the previous commit or a
patch-equivalent replay to appear in the configured local integration ref.
It neither fetches nor inspects the merge-train queue.

On ChatGPT Work VMs, direct generation, replay, reduction and campaigns use the
writable workspace. Local merge-train integration is unavailable; keep fixes
on their branches for an authorized GitHub handoff.
