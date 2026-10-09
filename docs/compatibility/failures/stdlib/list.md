# stdlib/list.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/list.dark) · [File list](../../current-audit.md)

Executed 220 assertions: **211 passed, 9 failed**.

## Deferred test-only builtins

`Builtin.testIncrementSideEffectCounter : a -> a` and
`Builtin.testSideEffectCount : Unit -> Int64` remain unsupported. List L61,
L65, L71, L75, and L81 use them to count callback executions and stay disabled.
A dev-only implementation is deferred: counter state, per-assertion isolation
in batched runs, and optimization safeguards are too much machinery for these
tests at present. This is missing test instrumentation, not evidence that
`List.iter` itself is broken. No counter implementation or test-gate change was made.

## Failing assertions

| Test | Observed failure |
| --- | --- |
| [L61](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L61) — Stdlib.List.iter [ 1L, 2L, 3L ] (fun x -> Builtin.testIncrementSideEffectCounter ()) Builtin.testSideEffect... | Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L65](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L65) — Stdlib.List.iter [ 1L, 2L, 3L, 4L, 5L ] (fun x -> if x % 2L == 0L then Builtin.testIncrementSideEffectCount... | Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L71](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L71) — Stdlib.List.iter [] (fun x -> Builtin.testIncrementSideEffectCounter ()) Builtin.testSideEffectCount () | Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L75](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L75) — Stdlib.List.iter [ 10L, 20L, 30L ] (fun x -> Builtin.testIncrementSideEffectCounter () Builtin.testIncremen... | Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L81](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L81) — Stdlib.List.iter [ 1L, 2L, 3L ] (fun x -> if x > 2L then Builtin.testIncrementSideEffectCounter ()) Builtin... | Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L157](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L157) — Stdlib.List.head [ Builtin.testRuntimeError "test" ] | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L184](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L184) — Stdlib.List.last [ Builtin.testRuntimeError "test" ] | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L238](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L238) — Stdlib.List.randomElement [ Builtin.testRuntimeError "test" ] | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L346](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L346) — Stdlib.List.zip [ Builtin.testRuntimeError "msg" ] [ Some "" ] | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
