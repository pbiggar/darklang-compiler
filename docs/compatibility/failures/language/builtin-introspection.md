# language/builtin-introspection.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/language/builtin-introspection.dark) · [File list](../../current-audit.md)

Executed 2 assertions: **0 passed, 2 failed**.
Of 2 previously disabled assertions, **0 passed and 2 failed**.

| Test | Observed failure |
| --- | --- |
| [L9](../../../../test/fixtures/e2e/upstream/language/builtin-introspection.dark#L9) — ((Builtin.getAllBuiltinFns ()) \|> Stdlib.List.length) > 100 | Unknown function or value 'Builtin.getAllBuiltinFns' |
| [L13](../../../../test/fixtures/e2e/upstream/language/builtin-introspection.dark#L13) — (Builtin.getAllBuiltinFns ()) \|> Stdlib.List.findFirst (fun f -> f.name.name == "int64Add") \|> Builtin.unwr... | Unknown function or value 'Builtin.getAllBuiltinFns' |
