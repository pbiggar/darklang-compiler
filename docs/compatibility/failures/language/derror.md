# language/derror.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/language/derror.dark) · [File list](../../current-audit.md)

Executed 15 assertions: **10 passed, 5 failed**.

| Test | Observed failure |
| --- | --- |
| [L2](../../../../test/fixtures/e2e/upstream/language/derror.dark#L2) — Stdlib.Option.map2 (Stdlib.Option.Option.Some 10L) "not an option" (fun (a, b) -> "1") | Expected error message 'Darklang.Stdlib.Option.map2's 2nd parameter 'option2' expects Darklang.Stdlib.Option.Option<_>, but got String ("not an option")' not found in stderr. Actual stderr: <entry>: Expected TSum |
| [L10](../../../../test/fixtures/e2e/upstream/language/derror.dark#L10) — (Stdlib.List.map [ 1L, 2L, 3L, 4L, 5L ] (fun x -> Builtin.testRuntimeError "X")) \|> Stdlib.List.fakeFunction | Expected error message 'Uncaught exception: X' not found in stderr. Actual stderr: <entry>: Unknown function or value 'Stdlib.List.fakeFunction' |
| [L15](../../../../test/fixtures/e2e/upstream/language/derror.dark#L15) — (Stdlib.List.head (Builtin.testRuntimeError "test")).field | Expected error message 'Uncaught exception: test' not found in stderr. Actual stderr: <entry>: Field access requires a record value |
| [L22](../../../../test/fixtures/e2e/upstream/language/derror.dark#L22) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "test") | Expected error message 'Uncaught exception: test' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: t |
| [L23](../../../../test/fixtures/e2e/upstream/language/derror.dark#L23) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "test") | Expected error message 'Uncaught exception: test' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: e |
