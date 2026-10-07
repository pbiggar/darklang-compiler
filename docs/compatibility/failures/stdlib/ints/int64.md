# stdlib/ints/int64.dark

[Source fixture](../../../../../test/fixtures/e2e/upstream/stdlib/ints/int64.dark) · [File list](../../../current-audit.md)

Executed 264 assertions: **262 passed, 2 failed**.
Of 5 previously disabled assertions, **3 passed and 2 failed**.

| Test | Observed failure |
| --- | --- |
| [L90](../../../../../test/fixtures/e2e/upstream/stdlib/ints/int64.dark#L90) — Stdlib.List.map (Stdlib.List.range -5 5) (fun v -> (Builtin.unwrap (Stdlib.Int.toInt64 v)) % 4L) | Value mismatch |
| [L368](../../../../../test/fixtures/e2e/upstream/stdlib/ints/int64.dark#L368) — Stdlib.Int64.shiftRight (-1L) 1L | Value mismatch |

Previously disabled tests that passed: L45, L60, L210.
