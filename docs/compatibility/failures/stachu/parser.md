# stachu/parser.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stachu/parser.dark) · [File list](../../current-audit.md)

Executed 56 assertions: **0 passed, 56 failed**.
Of 56 previously disabled assertions, **0 passed and 56 failed**.

| Test | Observed failure |
| --- | --- |
| [L6](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L6) — (Stachu.Parser.parse (Stachu.Parser.string "hello") "hello world") | Unknown function or value 'Stachu.Parser.parse' |
| [L8](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L8) — (Stachu.Parser.parse (Stachu.Parser.string "hello") "goodbye") | Unknown function or value 'Stachu.Parser.parse' |
| [L10](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L10) — (Stachu.Parser.parse (Stachu.Parser.string "") "test") | Unknown function or value 'Stachu.Parser.parse' |
| [L15](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L15) — (Stachu.Parser.parse (Stachu.Parser.regex "\\d+") "123abc") | Unknown function or value 'Stachu.Parser.parse' |
| [L17](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L17) — (Stachu.Parser.parse (Stachu.Parser.regex "\\d+") "abc123") | Unknown function or value 'Stachu.Parser.parse' |
| [L19](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L19) — (Stachu.Parser.parse (Stachu.Parser.regex "[a-z]+") "hello123") | Unknown function or value 'Stachu.Parser.parse' |
| [L22](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L22) — (Stachu.Parser.parse (Stachu.Parser.regex "\\w+") "hello_world123 test") | Unknown function or value 'Stachu.Parser.parse' |
| [L25](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L25) — (Stachu.Parser.parse (Stachu.Parser.regex "\\s+") " hello") | Unknown function or value 'Stachu.Parser.parse' |
| [L30](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L30) — (Stachu.Parser.parse (Stachu.Parser.digits ()) "42abc") | Unknown function or value 'Stachu.Parser.parse' |
| [L32](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L32) — (Stachu.Parser.parse (Stachu.Parser.digits ()) "abc") | Unknown function or value 'Stachu.Parser.parse' |
| [L35](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L35) — (Stachu.Parser.parse (Stachu.Parser.word ()) "hello123_world!") | Unknown function or value 'Stachu.Parser.parse' |
| [L38](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L38) — (Stachu.Parser.parse (Stachu.Parser.letters ()) "hello123") | Unknown function or value 'Stachu.Parser.parse' |
| [L41](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L41) — (Stachu.Parser.parse (Stachu.Parser.whitespace ()) " \t\ntext") | Unknown function or value 'Stachu.Parser.parse' |
| [L44](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L44) — (Stachu.Parser.parse (Stachu.Parser.optionalWhitespace ()) " text") | Unknown function or value 'Stachu.Parser.parse' |
| [L46](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L46) — (Stachu.Parser.parse (Stachu.Parser.optionalWhitespace ()) "text") | Unknown function or value 'Stachu.Parser.parse' |
| [L51](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L51) — (Stachu.Parser.parse (Stachu.Parser.int64 ()) "42 rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L53](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L53) — (Stachu.Parser.parse (Stachu.Parser.int64 ()) "-123abc") | Unknown function or value 'Stachu.Parser.parse' |
| [L55](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L55) — (Stachu.Parser.parse (Stachu.Parser.int64 ()) "0") | Unknown function or value 'Stachu.Parser.parse' |
| [L58](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L58) — (Stachu.Parser.parse (Stachu.Parser.float ()) "3.14 rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L60](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L60) — (Stachu.Parser.parse (Stachu.Parser.float ()) "-0.5abc") | Unknown function or value 'Stachu.Parser.parse' |
| [L65](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L65) — (Stachu.Parser.parse (Stachu.Parser.succeed 42L) "anything") | Unknown function or value 'Stachu.Parser.parse' |
| [L67](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L67) — (Stachu.Parser.parse (Stachu.Parser.succeed "value") "") | Unknown function or value 'Stachu.Parser.parse' |
| [L71](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L71) — (Stachu.Parser.parse (Stachu.Parser.fail "oops") "input") | Unknown function or value 'Stachu.Parser.parse' |
| [L76](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L76) — Stachu.Parser.parse (Stachu.Parser.map (fun s -> Stdlib.String.toUppercase s) (Stachu.Parser.string "hello"... | Unknown function or value 'Stachu.Parser.parse' |
| [L81](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L81) — Stachu.Parser.parse (Stachu.Parser.map (fun n -> n * 2L) (Stachu.Parser.int64 ())) "21 rest" | Unknown function or value 'Stachu.Parser.parse' |
| [L88](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L88) — Stachu.Parser.parse (Stachu.Parser.andThen (Stachu.Parser.string "hello") (Stachu.Parser.string " world")) ... | Unknown function or value 'Stachu.Parser.parse' |
| [L93](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L93) — Stachu.Parser.parse (Stachu.Parser.andThen (Stachu.Parser.string "hello") (Stachu.Parser.string " world")) ... | Unknown function or value 'Stachu.Parser.parse' |
| [L98](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L98) — Stachu.Parser.parse (Stachu.Parser.andThen (Stachu.Parser.string "hello") (Stachu.Parser.string " world")) ... | Unknown function or value 'Stachu.Parser.parse' |
| [L105](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L105) — Stachu.Parser.parse (Stachu.Parser.orElse (Stachu.Parser.string "hello") (Stachu.Parser.string "goodbye")) ... | Unknown function or value 'Stachu.Parser.parse' |
| [L110](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L110) — Stachu.Parser.parse (Stachu.Parser.orElse (Stachu.Parser.string "hello") (Stachu.Parser.string "goodbye")) ... | Unknown function or value 'Stachu.Parser.parse' |
| [L115](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L115) — Stachu.Parser.parse (Stachu.Parser.orElse (Stachu.Parser.string "hello") (Stachu.Parser.string "goodbye")) ... | Unknown function or value 'Stachu.Parser.parse' |
| [L122](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L122) — Stachu.Parser.parse (Stachu.Parser.keepLeft (Stachu.Parser.word ()) (Stachu.Parser.whitespace ())) "hello w... | Unknown function or value 'Stachu.Parser.parse' |
| [L127](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L127) — Stachu.Parser.parse (Stachu.Parser.keepRight (Stachu.Parser.whitespace ()) (Stachu.Parser.word ())) " hello... | Unknown function or value 'Stachu.Parser.parse' |
| [L135](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L135) — Stachu.Parser.parse (Stachu.Parser.many (Stachu.Parser.regex "\\d")) "123abc" | Unknown function or value 'Stachu.Parser.parse' |
| [L140](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L140) — Stachu.Parser.parse (Stachu.Parser.many (Stachu.Parser.regex "\\d")) "abc" | Unknown function or value 'Stachu.Parser.parse' |
| [L147](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L147) — Stachu.Parser.parse (Stachu.Parser.many1 (Stachu.Parser.regex "\\d")) "123abc" | Unknown function or value 'Stachu.Parser.parse' |
| [L152](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L152) — Stachu.Parser.parse (Stachu.Parser.many1 (Stachu.Parser.regex "\\d")) "abc" | Unknown function or value 'Stachu.Parser.parse' |
| [L159](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L159) — Stachu.Parser.parse (Stachu.Parser.optional (Stachu.Parser.string "-")) "-123" | Unknown function or value 'Stachu.Parser.parse' |
| [L164](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L164) — Stachu.Parser.parse (Stachu.Parser.optional (Stachu.Parser.string "-")) "123" | Unknown function or value 'Stachu.Parser.parse' |
| [L171](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L171) — Stachu.Parser.parse (Stachu.Parser.sepBy (Stachu.Parser.digits ()) (Stachu.Parser.string ",")) "1,2,3 rest" | Unknown function or value 'Stachu.Parser.parse' |
| [L176](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L176) — Stachu.Parser.parse (Stachu.Parser.sepBy (Stachu.Parser.digits ()) (Stachu.Parser.string ",")) "42 rest" | Unknown function or value 'Stachu.Parser.parse' |
| [L181](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L181) — Stachu.Parser.parse (Stachu.Parser.sepBy (Stachu.Parser.digits ()) (Stachu.Parser.string ",")) "abc" | Unknown function or value 'Stachu.Parser.parse' |
| [L188](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L188) — Stachu.Parser.parse (Stachu.Parser.between (Stachu.Parser.string "(") (Stachu.Parser.string ")") (Stachu.Pa... | Unknown function or value 'Stachu.Parser.parse' |
| [L193](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L193) — Stachu.Parser.parse (Stachu.Parser.between (Stachu.Parser.string "[") (Stachu.Parser.string "]") (Stachu.Pa... | Unknown function or value 'Stachu.Parser.parse' |
| [L200](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L200) — Stachu.Parser.parse (Stachu.Parser.choice [ Stachu.Parser.string "a", Stachu.Parser.string "b", Stachu.Pars... | Unknown function or value 'Stachu.Parser.parse' |
| [L205](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L205) — Stachu.Parser.parse (Stachu.Parser.choice [ Stachu.Parser.string "a", Stachu.Parser.string "b", Stachu.Pars... | Unknown function or value 'Stachu.Parser.parse' |
| [L210](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L210) — Stachu.Parser.parse (Stachu.Parser.choice [ Stachu.Parser.string "a", Stachu.Parser.string "b", Stachu.Pars... | Unknown function or value 'Stachu.Parser.parse' |
| [L217](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L217) — (Stachu.Parser.parse (Stachu.Parser.quotedString ()) "\"hello\" world") | Unknown function or value 'Stachu.Parser.parse' |
| [L220](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L220) — (Stachu.Parser.parse (Stachu.Parser.quotedString ()) "\"\" rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L223](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L223) — (Stachu.Parser.parse (Stachu.Parser.quotedString ()) "\"hello\\nworld\" rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L231](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L231) — Stachu.Parser.parse (Stachu.Parser.andThen (Stachu.Parser.letters ()) (Stachu.Parser.keepRight (Stachu.Pars... | Unknown function or value 'Stachu.Parser.parse' |
| [L248](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L248) — Stachu.Parser.parse (Stachu.Parser.between (Stachu.Parser.keepRight (Stachu.Parser.string "[") (Stachu.Pars... | Unknown function or value 'Stachu.Parser.parse' |
| [L262](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L262) — Stachu.Parser.parse (Stachu.Parser.regex "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}") "test@example.c... | Unknown function or value 'Stachu.Parser.parse' |
| [L266](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L266) — Stachu.Parser.parse (Stachu.Parser.regex "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}") "user.name+tag@... | Unknown function or value 'Stachu.Parser.parse' |
| [L272](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L272) — Stachu.Parser.parse (Stachu.Parser.regex "https?://[a-zA-Z0-9.-]+(?:/[a-zA-Z0-9./_-]*)?") "https://example.... | Unknown function or value 'Stachu.Parser.parse' |
| [L276](../../../../test/fixtures/e2e/upstream/stachu/parser.dark#L276) — Stachu.Parser.parse (Stachu.Parser.regex "https?://[a-zA-Z0-9.-]+(?:/[a-zA-Z0-9./_-]*)?") "http://test.org ... | Unknown function or value 'Stachu.Parser.parse' |
