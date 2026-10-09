# stdlib/httpserver.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/httpserver.dark) · [File list](../../current-audit.md)

Executed 7 assertions: **4 passed, 3 failed**.

| Test | Observed failure |
| --- | --- |
| [L29](../../../../test/fixtures/e2e/upstream/stdlib/httpserver.dark#L29) — (Stdlib.HttpServer.get "/ping" (fun req -> Stdlib.Http.Response { statusCode = 200L; headers = []; body = S... | Expected TInt, got TInt64 |
| [L33](../../../../test/fixtures/e2e/upstream/stdlib/httpserver.dark#L33) — (Stdlib.HttpServer.get "/ping" (fun req -> Stdlib.Http.Response { statusCode = 200L; headers = []; body = S... | Expected TInt, got TInt64 |
| [L37](../../../../test/fixtures/e2e/upstream/stdlib/httpserver.dark#L37) — (Stdlib.HttpServer.post "/ops" (fun req -> Stdlib.Http.Response { statusCode = 200L; headers = []; body = S... | Expected TInt, got TInt64 |
