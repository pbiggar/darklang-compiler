# cli/include-parsing.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark) · [File list](../../current-audit.md)

Executed 8 assertions: **0 passed, 8 failed**.

| Test | Observed failure |
| --- | --- |
| [L12](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L12) — Include.parse [ "--include=A.b" ] | Unknown function or value 'Include.parse' |
| [L15](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L15) — Include.parse [ "--include=A.b,C.d" ] | Unknown function or value 'Include.parse' |
| [L18](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L18) — Include.parse [ "--include=A.b,C.d", "--include=E.f" ] | Unknown function or value 'Include.parse' |
| [L21](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L21) — Include.parse [ "--include=A.b, C.d" ] | Unknown function or value 'Include.parse' |
| [L25](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L25) — Include.parse [ "--include=A.b,,C.d," ] | Unknown function or value 'Include.parse' |
| [L29](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L29) — Include.parse [ "--include=A.b,A.b", "--include=A.b" ] | Unknown function or value 'Include.parse' |
| [L32](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L32) — Include.parse [ "-y", "--json", "--include=A.b", "--allow-type-errors" ] | Unknown function or value 'Include.parse' |
| [L35](../../../../test/fixtures/e2e/upstream/cli/include-parsing.dark#L35) — Include.parse [ "a message", "-y" ] | Unknown function or value 'Include.parse' |
