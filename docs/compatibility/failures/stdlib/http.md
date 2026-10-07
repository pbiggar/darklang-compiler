# stdlib/http.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/http.dark) · [File list](../../current-audit.md)

Executed 47 assertions: **21 passed, 26 failed**.
Of 47 previously disabled assertions, **21 passed and 26 failed**.

| Test | Observed failure |
| --- | --- |
| [L21](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L21) — Stdlib.Http.badRequest "Your request resulted in an error" | Value mismatch |
| [L27](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L27) — Stdlib.Http.response (Stdlib.String.toBlob "test") 200 | Value mismatch |
| [L33](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L33) — Stdlib.Http.responseWithHeaders (Stdlib.String.toBlob "test") [ ("Content-Type", "text/html; charset=utf-8"... | Value mismatch |
| [L48](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L48) — Stdlib.Http.success (Stdlib.String.toBlob "test") | Value mismatch |
| [L51](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L51) — Stdlib.Http.responseWithHtml "test" 200 | Value mismatch |
| [L57](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L57) — Stdlib.Http.responseWithText "test" 200 | Value mismatch |
| [L63](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L63) — Stdlib.Http.responseWithJson "test" 200 | Value mismatch |
| [L102](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L102) — Stdlib.Http.parseQueryString "https://x?q=List+map&n=A%2FB" | Value mismatch |
| [L108](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L108) — Stdlib.Http.urlDecode "List+map" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L110](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L110) — Stdlib.Http.urlDecode "A%2FB" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L111](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L111) — Stdlib.Http.urlDecode "a%3Db" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L114](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L114) — Stdlib.Http.urlDecode "caf%C3%A9" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L117](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L117) — Stdlib.Http.urlDecode "%2541" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L122](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L122) — Stdlib.Http.urlDecode "a%2Bb" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L123](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L123) — Stdlib.Http.urlDecode "C%2B%2B" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L124](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L124) — Stdlib.Http.urlDecode "a%2Bb+c" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L128](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L128) — Stdlib.Http.urlDecode "100%" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L129](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L129) — Stdlib.Http.urlDecode "%" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L130](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L130) — Stdlib.Http.urlDecode "a%2" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L131](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L131) — Stdlib.Http.urlDecode "50%zz" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L133](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L133) — Stdlib.Http.urlDecode "" | Unknown function or value 'Stdlib.Http.urlDecode' |
| [L137](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L137) — Stdlib.Http.Request.queryParams ( Stdlib.Http.Request { url = "/s?q=List+map&n=A%2FB"; headers = []; body =... | Value mismatch |
| [L144](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L144) — Stdlib.Http.Request.queryParam (Stdlib.Http.Request { url = "/s?q=a=b"; headers = []; body = Stdlib.Blob.em... | Value mismatch |
| [L149](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L149) — Stdlib.Http.Request.queryParams ( Stdlib.Http.Request { url = "/s?a=1?b=2"; headers = []; body = Stdlib.Blo... | Value mismatch |
| [L155](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L155) — Stdlib.Http.Request.queryParams ( Stdlib.Http.Request { url = "/s?owner=a%26b"; headers = []; body = Stdlib... | Value mismatch |
| [L161](../../../../test/fixtures/e2e/upstream/stdlib/http.dark#L161) — Stdlib.Http.Request.queryParam (Stdlib.Http.Request { url = "/s?q=a%3Db"; headers = []; body = Stdlib.Blob.... | Unknown function or value 'Stdlib.Http.urlDecode' |
