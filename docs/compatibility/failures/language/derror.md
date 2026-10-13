# language/derror.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/language/derror.dark) · [File list](../../current-audit.md)

Executed 15 assertions: **2 passed, 13 failed**.

| Test | Observed failure |
| --- | --- |
| [L13](../../../../test/fixtures/e2e/upstream/language/derror.dark#L13) — Stdlib.List.head (Builtin.testRuntimeError "test") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L14](../../../../test/fixtures/e2e/upstream/language/derror.dark#L14) — (if Builtin.testRuntimeError "test" then 5L else 6L) | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L15](../../../../test/fixtures/e2e/upstream/language/derror.dark#L15) — (Stdlib.List.head (Builtin.testRuntimeError "test")).field | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L16](../../../../test/fixtures/e2e/upstream/language/derror.dark#L16) — [ 5L, 6L, Stdlib.List.head (Builtin.testRuntimeError "test") ] | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L17](../../../../test/fixtures/e2e/upstream/language/derror.dark#L17) — [ 5L, 6L, Builtin.testRuntimeError "test" ] | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L18](../../../../test/fixtures/e2e/upstream/language/derror.dark#L18) — 5L \|> (+) (Builtin.testRuntimeError "test") \|> (+) 3564L | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L19](../../../../test/fixtures/e2e/upstream/language/derror.dark#L19) — 5L \|> (+) (Builtin.testRuntimeError "test") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L20](../../../../test/fixtures/e2e/upstream/language/derror.dark#L20) — ("test" \|> Builtin.testRuntimeError) | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L21](../../../../test/fixtures/e2e/upstream/language/derror.dark#L21) — Stdlib.Option.Option.Some(Builtin.testRuntimeError "test") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L22](../../../../test/fixtures/e2e/upstream/language/derror.dark#L22) — Stdlib.Result.Result.Error(Builtin.testRuntimeError "test") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L23](../../../../test/fixtures/e2e/upstream/language/derror.dark#L23) — Stdlib.Result.Result.Ok(Builtin.testRuntimeError "test") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L25](../../../../test/fixtures/e2e/upstream/language/derror.dark#L25) — ("test" \|> Builtin.testRuntimeError \|> (++) "3") | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
| [L32](../../../../test/fixtures/e2e/upstream/language/derror.dark#L32) — EPRec { i = Builtin.testRuntimeError "1" m = 5L j = Stdlib.List.head (Builtin.testRuntimeError "2") n = 6L } | Unsupported interpreter test infrastructure: `Unknown function or value 'Builtin.testRuntimeError'` |
