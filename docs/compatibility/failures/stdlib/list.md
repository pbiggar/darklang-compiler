# stdlib/list.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/list.dark) · [File list](../../current-audit.md)

Executed 220 assertions: **202 passed, 18 failed**.

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
| [L87](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L87) — Stdlib.List.filter [ 1L, 2L, 3L ] (fun item -> match item with \| 1L -> Stdlib.Option.Option.None \| 2L -> fa... | Expected error message 'Encountered a condition that must be a Bool, but got a Darklang.Stdlib.Option.Option<_> (None)' not found in stderr. Actual stderr: <entry>: Expected TBool, got TSum ("Darklang.Stdlib.Option.Option", [TVar "t"]) |
| [L101](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L101) — Stdlib.List.filter [] (fun item -> "a") | Expected TBool, got TString |
| [L109](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L109) — Stdlib.List.filterMap [] (fun item -> 0L) | Expected TSum |
| [L129](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L129) — Stdlib.List.flatten [ 1l, 2l, 3l ] | Expected error message 'Darklang.Stdlib.List.flatten's 1st parameter 'list' expects List<List<_>>, but got List<Int32> ([1, 2, 3])' not found in stderr. Actual stderr: <entry>: Expected TList |
| [L130](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L130) — Stdlib.List.flatten [ [ 1L ], [ [ 2L, 3L ] ] ] | Expected error message 'Cannot add a List<List<Int64>> ([[2, 3]]) to a list of List<Int64>. Failed at index 1.' not found in stderr. Actual stderr: <entry>: Expected TInt64, got TList TInt64 |
| [L136](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L136) — Stdlib.List.fold [] [] (fun accum curr -> 5L) | Expected TList (TVar "t$empty"), got TInt64 |
| [L174](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L174) — Stdlib.List.interleave [ "a", "b", "c" ] [ 0L ] | Expected error message 'Darklang.Stdlib.List.interleave's 2nd parameter 'lB' expects List<String>, but got List<Int64> ([0])' not found in stderr. Actual stderr: <entry>: Expected TString, got TInt64 |
| [L180](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L180) — Stdlib.List.interpose [ "a", "b", "c" ] 0L | Expected error message 'Darklang.Stdlib.List.interpose's 2nd parameter 'sep' expects String, but got Int64 (0)' not found in stderr. Actual stderr: <entry>: Expected TString, got TInt64 |
| [L216](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L216) — Stdlib.List.partition [] (fun item -> "a") | Expected TBool, got TString |
| [L224](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L224) — Stdlib.List.partition [ 1L, 2L, 3L ] (fun item -> match item with \| 1L -> Stdlib.Option.Option.None \| 2L ->... | Expected error message 'Encountered a condition that must be a Bool, but got a Darklang.Stdlib.Option.Option<_> (None)' not found in stderr. Actual stderr: <entry>: Expected TBool, got TSum ("Darklang.Stdlib.Option.Option", [TVar "t"]) |
| [L264](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L264) — Stdlib.List.sortByComparator [ 3L, 1L, 2L ] (fun a b -> 0.1) | Expected error message 'Cannot perform equality check on Float and Int' not found in stderr. Actual stderr: <entry>: Expected TInt, got TFloat64 |
| [L269](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L269) — Stdlib.List.sortByComparator [ 1L, 2L, 3L ] (fun a b -> "㧑༷釺") | Expected error message 'Cannot perform equality check on String and Int' not found in stderr. Actual stderr: <entry>: Expected TInt, got TString |
| [L314](../../../../test/fixtures/e2e/upstream/stdlib/list.dark#L314) — Stdlib.List.uniqueBy [ 6L, 2.0 ] (fun x -> x) | Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
