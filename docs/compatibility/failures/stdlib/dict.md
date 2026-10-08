# stdlib/dict.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/dict.dark) · [File list](../../current-audit.md)

Executed 140 assertions: **139 passed, 1 failed**.
Of 16 previously disabled assertions, **15 passed and 1 failed**.

| Test | Observed failure |
| --- | --- |
| [L21](../../../../test/fixtures/e2e/upstream/stdlib/dict.dark#L21) — Stdlib.Dict.set valDict "notAnInt" "c" | Expected error message 'Darklang.Stdlib.Dict.set's 2nd parameter 'key' expects Int64, but got String ("notAnInt")' not found in stderr. Actual stderr: <entry>: Expected TInt64, got TString |

Previously disabled tests that passed: L30, L32, L59, L147, L159, L251, L279, L282, L322, L334, L351, L353, L357, L396, L404.

Gate entries without an assertion at that line: L61, L74, L144, L145, L146, L227, L237, L242, L245, L273, L311, L315, L327, L332, L387, L389, L391, L394, L398, L400, L402, L406, L408.
