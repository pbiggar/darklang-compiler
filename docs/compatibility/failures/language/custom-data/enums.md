# language/custom-data/enums.dark

[Source fixture](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) · [File list](../../../current-audit.md)

Executed 38 assertions: **31 passed, 7 failed**.

| Test | Observed failure |
| --- | --- |
| [L7](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L7) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "err") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L9](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L9) — Stdlib.Option.Option.Some(Builtin.testRuntimeError "err") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L11](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L11) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "err") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L15](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L15) — Stdlib.Option.Option.None 5 | Expected error message 'Expected 0 fields in Darklang.Stdlib.Option.Option.'None', but got 1' not found in stderr. Actual stderr: <entry>: Type 'Darklang.Stdlib.Option.Option' expects 1 arguments, got 0 |
| [L17](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L17) — Stdlib.Option.Option.Some(5, 6) | Expected error message 'Expected 1 fields in Darklang.Stdlib.Option.Option.'Some', but got 2' not found in stderr. Actual stderr: <entry>: Type 'Darklang.Stdlib.Option.Option' expects 1 arguments, got 0 |
| [L72](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L72) — EnumOfMixedCases.Z(Builtin.testRuntimeError "1", Builtin.testRuntimeError "2") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L104](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L104) — (Tuples.NotTuple(("printer broke", 7L))) | Expected TInt, got TSum ("MyEnum", []) |
