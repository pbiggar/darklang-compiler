# stdlib/sqlite.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark) · [File list](../../current-audit.md)

Executed 8 assertions: **0 passed, 8 failed**.

| Test | Observed failure |
| --- | --- |
| [L9](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L9) — let db = "/tmp/dark-test-sqlite-a.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Stdlib... | Unknown function or value 'Stdlib.Sqlite.exec' |
| [L20](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L20) — let db = "/tmp/dark-test-sqlite-blob.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Std... | Unknown function or value 'Stdlib.Sqlite.exec' |
| [L35](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L35) — Stdlib.Sqlite.asBytes (Stdlib.Sqlite.Value.Text "hi") | Unknown function or value 'Stdlib.Sqlite.asBytes' |
| [L40](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L40) — let db = "/tmp/dark-test-sqlite-b.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Stdlib... | Unknown function or value 'Stdlib.Sqlite.exec' |
| [L51](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L51) — let db = "/tmp/dark-test-sqlite-c.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Stdlib... | Unknown function or value 'Stdlib.Sqlite.exec' |
| [L62](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L62) — let db = "/tmp/dark-test-sqlite-d.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Stdlib... | Unknown function or value 'Stdlib.Sqlite.exec' |
| [L73](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L73) — let db = "/tmp/dark-test-sqlite-e.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Stdlib... | Unknown function or value 'Stdlib.Sqlite.exec' |
| [L87](../../../../test/fixtures/e2e/upstream/stdlib/sqlite.dark#L87) — let db = "/tmp/dark-test-sqlite-f.db" let _ = Stdlib.Sqlite.exec db "DROP TABLE IF EXISTS t" let _ = Stdlib... | Unknown function or value 'Stdlib.Sqlite.exec' |
