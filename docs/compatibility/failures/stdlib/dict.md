# stdlib/dict.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/dict.dark) · [File list](../../current-audit.md)

Executed 140 assertions: **139 passed, 1 failed**.

| Test | Observed failure |
| --- | --- |
| [L21](../../../../test/fixtures/e2e/upstream/stdlib/dict.dark#L21) — Stdlib.Dict.set valDict "notAnInt" "c" | Expected error message 'Darklang.Stdlib.Dict.set's 2nd parameter 'key' expects Int64, but got String ("notAnInt")' not found in stderr. Actual stderr: <entry>: Expected TInt64, got TString |
