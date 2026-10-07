# stdlib/html.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/html.dark) · [File list](../../current-audit.md)

Executed 99 assertions: **92 passed, 7 failed**.
Of 7 previously disabled assertions, **0 passed and 7 failed**.

| Test | Observed failure |
| --- | --- |
| [L42](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L42) — (Stdlib.Html.comment "a --> b") \|> nodeToString | Value mismatch |
| [L44](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L44) — (Stdlib.Html.comment "<script>alert(1)</script> -- and more") \|> nodeToString | Value mismatch |
| [L66](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L66) — (htmlTag "a" [ ("href", Stdlib.Option.Option.Some "\" onmouseover=\"alert(1)") ] []) \|> nodeToString | Value mismatch |
| [L69](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L69) — (htmlTag "div" [ ("title", Stdlib.Option.Option.Some "a & b") ] []) \|> nodeToString | Value mismatch |
| [L72](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L72) — (htmlTag "div" [ ("title", Stdlib.Option.Option.Some "<script>") ] []) \|> nodeToString | Value mismatch |
| [L75](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L75) — (htmlTag "div" [ ("title", Stdlib.Option.Option.Some "it's") ] []) \|> nodeToString | Value mismatch |
| [L83](../../../../test/fixtures/e2e/upstream/stdlib/html.dark#L83) — (Stdlib.Html.div [ Stdlib.Html.dataAttr "note" "a \"quoted\" one" ] []) \|> nodeToString | Value mismatch |
