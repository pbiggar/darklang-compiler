# language/custom-data/enums.dark

[Source fixture](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) · [File list](../../../current-audit.md)

38 assertions: **34 enabled, 4 excluded**. The enabled assertions pass individually.

The invalid Option payloads now require compile-time constructor field-count diagnostics at L16 and L19; both are enabled. The invalid `NotTuple` call at L107 also requires its field-count compile error and is enabled.

| Test | Observed failure |
| --- | --- |
| [L7](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L7) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "err") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L9](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L9) — Stdlib.Option.Option.Some(Builtin.testRuntimeError "err") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L11](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L11) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "err") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L74](../../../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L74) — EnumOfMixedCases.Z(Builtin.testRuntimeError "1", Builtin.testRuntimeError "2") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
