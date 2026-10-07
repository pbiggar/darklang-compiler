# stdlib/result.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/result.dark) · [File list](../../current-audit.md)

Executed 67 assertions: **59 passed, 8 failed**.
Of 19 previously disabled assertions, **11 passed and 8 failed**.

| Test | Observed failure |
| --- | --- |
| [L57](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L57) — Stdlib.Result.map2 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (fun a b -> ... | Operator is unavailable for this type |
| [L79](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L79) — Stdlib.Result.map3 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (Stdlib.Resu... | Operator is unavailable for this type |
| [L85](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L85) — Stdlib.Result.map3 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (Stdlib.Resu... | Operator is unavailable for this type |
| [L110](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L110) — Stdlib.Result.map4 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (Stdlib.Resu... | Operator is unavailable for this type |
| [L117](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L117) — Stdlib.Result.map4 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (Stdlib.Resu... | Operator is unavailable for this type |
| [L139](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L139) — Stdlib.Result.map5 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (Stdlib.Resu... | Operator is unavailable for this type |
| [L147](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L147) — Stdlib.Result.map5 (Stdlib.Result.Result.Error "error1") (Stdlib.Result.Result.Error "error2") (Stdlib.Resu... | Operator is unavailable for this type |
| [L178](../../../../test/fixtures/e2e/upstream/stdlib/result.dark#L178) — Stdlib.Result.mapWithDefault (Stdlib.Result.Result.Error "test1") (Stdlib.Result.Result.Error "test2") (fun... | Expected TSum ("Darklang.Stdlib.Result.Result", [TVar "t"; TString]), got TInt64 |

Previously disabled tests that passed: L19, L24, L67, L91, L97, L124, L155, L185, L188, L277, L294.
