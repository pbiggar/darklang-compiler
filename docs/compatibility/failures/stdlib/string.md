# stdlib/string.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/string.dark) · [File list](../../current-audit.md)

After importing the character-literal changes from `darklang/dark` PR #5899,
all **639/639 enabled assertions pass individually** on Linux x86-64.
The 13 assertions formerly blocked by `Builtin.testToChar` now use standard
single-quoted character literals and are enabled. Coverage includes family
emoji, skin-tone modifiers, combining marks, and String/Char list conversions.
The existing `String.fromList ["a"]` compile-error override is retained.

One assertion remains excluded because it requires unsupported interpreter
test instrumentation. It maps `"a string"` to `"cccccccc"` while counting eight
callback executions with `Builtin.testIncrementSideEffectCounter` and
`Builtin.testSideEffectCount`. These builtins live in the interpreter's test
utilities; this exclusion does not establish a failure in `String.map`.

| Test | Observed failure |
| --- | --- |
| [L179](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L179) — map `"a string"`, then read the callback counter | `<entry>: Unknown function or value 'Builtin.testIncrementSideEffectCounter'` (previous exclusion audit; helper remains unsupported) |
