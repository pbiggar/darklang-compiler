# stdlib/language-tools/parsedFileShape.dark

[Source fixture](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark) · [File list](../../../current-audit.md)

Executed 13 assertions: **0 passed, 13 failed**.

| Test | Observed failure |
| --- | --- |
| [L19](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L19) — (shape "val v = 1") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L21](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L21) — (shape "let f (x: Int): Int = x") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L23](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L23) — (shape "type T = Unit") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L25](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L25) — (shape "module M =\n val v = 1") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L29](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L29) — (shape "1 + 1") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L32](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L32) — (shape "valoops = 1") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L34](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L34) — (shape "x = 1") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L37](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L37) — (shape "someCall () = \"Error: nope\"") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L41](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L41) — (shape "someCall () = error \"nope\"") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L48](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L48) — (shape "module Darklang.Foo\n\nvaloops = 1") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L50](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L50) — (shape "module Darklang.Foo\n\nlet f (x: Int): Int = x") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L53](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L53) — (shape "module Darklang.Foo\n\n[<DB>] type Counter = Int") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
| [L58](../../../../../test/fixtures/e2e/upstream/stdlib/language-tools/parsedFileShape.dark#L58) — (shape "val v = 1\noops = 2") | Preamble type error: shape: Unknown function or value 'Builtin.parserParseToWrittenTypes' |
