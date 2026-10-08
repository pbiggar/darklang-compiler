# language/custom-data/enums.dark

[Source fixture](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) · [File list](../../../current-audit.md)

Executed 38 assertions: **22 passed, 16 failed**.

| Test | Observed failure |
| --- | --- |
| [L7](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L7) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "err") | Expected error message 'Uncaught exception: err' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: e |
| [L11](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L11) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "err") | Expected error message 'Uncaught exception: err' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: t |
| [L15](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L15) — Stdlib.Option.Option.None 5 | Expected error message 'Expected 0 fields in Darklang.Stdlib.Option.Option.'None', but got 1' not found in stderr. Actual stderr: <entry>: Type 'Darklang.Stdlib.Option.Option' expects 1 arguments, got 0 |
| [L17](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L17) — Stdlib.Option.Option.Some(5, 6) | Expected error message 'Expected 1 fields in Darklang.Stdlib.Option.Option.'Some', but got 2' not found in stderr. Actual stderr: <entry>: Type 'Darklang.Stdlib.Option.Option' expects 1 arguments, got 0 |
| [L22](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L22) — MyEnum.D | Expected error message 'There is no case named 'D' in Errors.User.MyEnum' not found in stderr. Actual stderr: <entry>: Unknown constructor 'MyEnum.D' |
| [L23](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L23) — MyEnum.C | Expected error message 'Expected 1 fields in Errors.User.MyEnum.'C', but got 0' not found in stderr. Actual stderr: <entry>: Constructor 'MyEnum.C' expects 1 fields |
| [L24](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L24) — MyEnum.B 5L | Expected error message 'Expected 0 fields in Errors.User.MyEnum.'B', but got 1' not found in stderr. Actual stderr: <entry>: Constructor 'MyEnum.B' expects 0 fields |
| [L26](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L26) — (match MyEnum.C "test" with \| 5 -> "unmatched because it's not an int" \| C v -> v) | Expected TInt, got TSum ("MyEnum", []) |
| [L27](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L27) — (match MyEnum.C "test" with \| C -> "unmatched because we didn't provide a space for the field") | Expected error message 'No matching case found for value Errors.User.MyEnum.C("test") in match expression' not found in stderr. Actual stderr: <entry>: Constructor pattern 'MyEnum.C' has wrong field count |
| [L28](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L28) — (match MyEnum.C "test" with \| D -> "unmatched because case name does not exist" \| C _ -> 2) | Unknown constructor 'MyEnum.D' in pattern |
| [L30](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L30) — (MyEnum.C 5L) | Expected error message 'Failed to create enum. Expected String for field 0 in 'C', but got Int64 (5)' not found in stderr. Actual stderr: <entry>: Expected TString, got TInt64 |
| [L65](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L65) — EnumOfMixedCases.X 1L | Expected error message 'Failed to create enum. Expected String for field 0 in 'X', but got Int64 (1)' not found in stderr. Actual stderr: <entry>: Expected TString, got TInt64 |
| [L67](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L67) — EnumOfMixedCases.Y "test" | Expected error message 'Failed to create enum. Expected Int64 for field 0 in 'Y', but got String ("test")' not found in stderr. Actual stderr: <entry>: Expected TInt64, got TString |
| [L69](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L69) — EnumOfMixedCases.Z 1L | Expected error message 'Expected 2 fields in MixedCases.EnumOfMixedCases.'Z', but got 1' not found in stderr. Actual stderr: <entry>: Constructor 'EnumOfMixedCases.Z' expects 2 fields |
| [L101](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L101) — match Tuples.NotTuple("printer broke", 7L) with \| NotTuple(reason, 7L) -> reason | Non-exhaustive match expression |
| [L104](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L104) — (Tuples.NotTuple(("printer broke", 7L))) | Expected TInt, got TSum ("MyEnum", []) |
