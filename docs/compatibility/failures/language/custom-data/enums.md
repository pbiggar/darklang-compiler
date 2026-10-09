# language/custom-data/enums.dark

[Source fixture](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) · [File list](../../../current-audit.md)

Executed 38 assertions: **33 passed, 5 failed**.

| Test | Observed failure |
| --- | --- |
| [L7](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L7) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "err") | Expected error message 'Uncaught exception: err' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: e |
| [L11](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L11) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "err") | Expected error message 'Uncaught exception: err' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: t |
| [L15](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L15) — Stdlib.Option.Option.None 5 | Expected error message 'Expected 0 fields in Darklang.Stdlib.Option.Option.'None', but got 1' not found in stderr. Actual stderr: <entry>: Type 'Darklang.Stdlib.Option.Option' expects 1 arguments, got 0 |
| [L17](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L17) — Stdlib.Option.Option.Some(5, 6) | Expected error message 'Expected 1 fields in Darklang.Stdlib.Option.Option.'Some', but got 2' not found in stderr. Actual stderr: <entry>: Type 'Darklang.Stdlib.Option.Option' expects 1 arguments, got 0 |
| [L104](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L104) — (Tuples.NotTuple(("printer broke", 7L))) | Expected TInt, got TSum ("MyEnum", []) |
