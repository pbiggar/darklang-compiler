# language/derror.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/language/derror.dark) · [File list](../../current-audit.md)

Executed 15 assertions: **13 passed, 2 failed**.

The `Builtin.testRuntimeError` rows below are historical diagnostics. They are
now classified as [unsupported interpreter test infrastructure](../interpreter-test-runtime-error.md).

| Test | Observed failure |
| --- | --- |
| [L22](../../../../test/fixtures/e2e/upstream/language/derror.dark#L22) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "test") | Expected error message 'Uncaught exception: test' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: t |
| [L23](../../../../test/fixtures/e2e/upstream/language/derror.dark#L23) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "test") | Expected error message 'Uncaught exception: test' not found in stderr. Actual stderr: Compilation failed: Unresolved type variable in value renderer: e |
