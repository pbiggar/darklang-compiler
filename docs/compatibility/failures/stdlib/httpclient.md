# stdlib/httpclient.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark) · [File list](../../current-audit.md)

Executed 61 assertions: **54 passed, 7 failed**.

| Test | Observed failure |
| --- | --- |
| [L71](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L71) — Stdlib.HttpClient.request "get" "http://google.com:79" [] Stdlib.Blob.empty | Value mismatch |
| [L108](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L108) — (get "localhost" []) | Value mismatch |
| [L111](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L111) — (get "http://[0:0:0:0:0:0:0:0]" []) | Value mismatch |
| [L131](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L131) — (get "http://google.com" [ ("Metadata-Flavor", "Google") ]) | Value mismatch |
| [L132](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L132) — (get "http://google.com" [ ("metadata-flavor", "Google") ]) | Value mismatch |
| [L133](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L133) — (get "http://google.com" [ ("Metadata-Flavor", " Google ") ]) | Value mismatch |
| [L134](../../../../test/fixtures/e2e/upstream/stdlib/httpclient.dark#L134) — (get "http://google.com" [ ("X-Google-Metadata-Request", " True ") ]) | Value mismatch |
