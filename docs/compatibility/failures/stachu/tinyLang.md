# stachu/tinyLang.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark) · [File list](../../current-audit.md)

Executed 34 assertions: **0 passed, 34 failed**.

| Test | Observed failure |
| --- | --- |
| [L91](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L91) — (Stachu.Parser.parse (TinyLang.intLiteral ()) "42") | Unknown function or value 'Stachu.Parser.parse' |
| [L93](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L93) — (Stachu.Parser.parse (TinyLang.intLiteral ()) "0") | Unknown function or value 'Stachu.Parser.parse' |
| [L95](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L95) — (Stachu.Parser.parse (TinyLang.intLiteral ()) "-123 rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L98](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L98) — (Stachu.Parser.parse (TinyLang.boolLiteral ()) "true") | Unknown function or value 'Stachu.Parser.parse' |
| [L100](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L100) — (Stachu.Parser.parse (TinyLang.boolLiteral ()) "false") | Unknown function or value 'Stachu.Parser.parse' |
| [L103](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L103) — (Stachu.Parser.parse (TinyLang.varName ()) "foo") | Unknown function or value 'Stachu.Parser.parse' |
| [L105](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L105) — (Stachu.Parser.parse (TinyLang.varName ()) "myVar123") | Unknown function or value 'Stachu.Parser.parse' |
| [L107](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L107) — (Stachu.Parser.parse (TinyLang.varName ()) "x rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L110](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L110) — (Stachu.Parser.parse (TinyLang.varExpr ()) "count") | Unknown function or value 'Stachu.Parser.parse' |
| [L115](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L115) — (Stachu.Parser.parse (TinyLang.binOp ()) "+") | Unknown function or value 'Stachu.Parser.parse' |
| [L117](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L117) — (Stachu.Parser.parse (TinyLang.binOp ()) "-") | Unknown function or value 'Stachu.Parser.parse' |
| [L119](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L119) — (Stachu.Parser.parse (TinyLang.binOp ()) "*") | Unknown function or value 'Stachu.Parser.parse' |
| [L121](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L121) — (Stachu.Parser.parse (TinyLang.binOp ()) "/") | Unknown function or value 'Stachu.Parser.parse' |
| [L127](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L127) — (Stachu.Parser.parse (TinyLang.intLiteral ()) "1 + 2") | Unknown function or value 'Stachu.Parser.parse' |
| [L130](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L130) — Stachu.Parser.parse (Stachu.Parser.between (TinyLang.ws ()) (TinyLang.ws ()) (TinyLang.binOp ())) " + 2" | Unknown function or value 'Stachu.Parser.parse' |
| [L135](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L135) — Stachu.Parser.parse (Stachu.Parser.andThen (TinyLang.intLiteral ()) (Stachu.Parser.andThen (Stachu.Parser.b... | Unknown function or value 'Stachu.Parser.parse' |
| [L144](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L144) — Stachu.Parser.parse (Stachu.Parser.map (fun result -> let (left, (op, right)) = result TinyLang.Expr.EBinOp... | Unknown function or value 'Stachu.Parser.parse' |
| [L157](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L157) — Stachu.Parser.parse (Stachu.Parser.map (fun result -> let (left, (op, right)) = result TinyLang.Expr.EBinOp... | Unknown function or value 'Stachu.Parser.parse' |
| [L169](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L169) — Stachu.Parser.parse (Stachu.Parser.map (fun result -> let (left, (op, right)) = result TinyLang.Expr.EBinOp... | Unknown function or value 'Stachu.Parser.parse' |
| [L184](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L184) — Stachu.Parser.parse (Stachu.Parser.map (fun result -> let (left, (op, right)) = result TinyLang.Expr.EBinOp... | Unknown function or value 'Stachu.Parser.parse' |
| [L197](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L197) — Stachu.Parser.parse (Stachu.Parser.map (fun result -> let (left, (op, right)) = result TinyLang.Expr.EBinOp... | Unknown function or value 'Stachu.Parser.parse' |
| [L212](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L212) — Stachu.Parser.parse (Stachu.Parser.map (fun items -> TinyLang.Expr.EList items) (Stachu.Parser.between (Sta... | Unknown function or value 'Stachu.Parser.parse' |
| [L224](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L224) — Stachu.Parser.parse (Stachu.Parser.map (fun items -> TinyLang.Expr.EList items) (Stachu.Parser.between (Sta... | Unknown function or value 'Stachu.Parser.parse' |
| [L236](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L236) — Stachu.Parser.parse (Stachu.Parser.map (fun items -> TinyLang.Expr.EList items) (Stachu.Parser.between (Sta... | Unknown function or value 'Stachu.Parser.parse' |
| [L250](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L250) — Stachu.Parser.parse (Stachu.Parser.map (fun items -> TinyLang.Expr.EList items) (Stachu.Parser.between (Sta... | Unknown function or value 'Stachu.Parser.parse' |
| [L264](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L264) — (Stachu.Parser.parse (TinyLang.keyword "if") "if x") | Unknown function or value 'Stachu.Parser.parse' |
| [L266](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L266) — (Stachu.Parser.parse (TinyLang.keyword "then") "then 1") | Unknown function or value 'Stachu.Parser.parse' |
| [L268](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L268) — (Stachu.Parser.parse (TinyLang.keyword "else") "else 2") | Unknown function or value 'Stachu.Parser.parse' |
| [L270](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L270) — (Stachu.Parser.parse (TinyLang.keyword "let") "let x") | Unknown function or value 'Stachu.Parser.parse' |
| [L277](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L277) — (Stachu.Parser.parse (Stachu.Parser.regex "[a-z][a-zA-Z0-9_]*") "myVariable123 rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L280](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L280) — (Stachu.Parser.parse (Stachu.Parser.regex "-?[0-9]+") "-42 rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L283](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L283) — (Stachu.Parser.parse (Stachu.Parser.regex "[ \\t\\n]+") " \t\n text") | Unknown function or value 'Stachu.Parser.parse' |
| [L286](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L286) — (Stachu.Parser.parse (Stachu.Parser.regex "[^\"]*") "hello world\" rest") | Unknown function or value 'Stachu.Parser.parse' |
| [L289](../../../../test/fixtures/e2e/upstream/stachu/tinyLang.dark#L289) — (Stachu.Parser.parse (Stachu.Parser.regex "[+\\-*/]") "+ 1") | Unknown function or value 'Stachu.Parser.parse' |
