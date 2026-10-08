# stdlib/option.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/option.dark) · [File list](../../current-audit.md)

Executed 73 assertions: **61 passed, 12 failed**.

| Test | Observed failure |
| --- | --- |
| [L44](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L44) — Stdlib.Option.andThen2 Stdlib.Option.Option.None Stdlib.Option.Option.None (fun x y -> Stdlib.Option.Option... | Operator is unavailable for this type |
| [L75](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L75) — Stdlib.Option.andThen3 Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Option.Option.None (fun x... | Operator is unavailable for this type |
| [L119](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L119) — Stdlib.Option.andThen4 Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib... | Operator is unavailable for this type |
| [L148](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L148) — Stdlib.Option.map2 Stdlib.Option.Option.None Stdlib.Option.Option.None (fun a b -> a - b) | Operator is unavailable for this type |
| [L170](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L170) — Stdlib.Option.map3 Stdlib.Option.Option.None Stdlib.Option.Option.None (Stdlib.Option.Option.Some 2L) (fun ... | Operator is unavailable for this type |
| [L176](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L176) — Stdlib.Option.map3 Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Option.Option.None (fun a b c... | Operator is unavailable for this type |
| [L204](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L204) — Stdlib.Option.map4 Stdlib.Option.Option.None Stdlib.Option.Option.None (Stdlib.Option.Option.Some 2L) (Stdl... | Operator is unavailable for this type |
| [L211](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L211) — Stdlib.Option.map4 Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Option.Option.None (Stdlib.Op... | Operator is unavailable for this type |
| [L218](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L218) — Stdlib.Option.map4 Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Opt... | Operator is unavailable for this type |
| [L242](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L242) — Stdlib.Option.map5 Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Option.Option.None Stdlib.Opt... | Operator is unavailable for this type |
| [L255](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L255) — Stdlib.Option.mapWithDefault (Stdlib.Option.Option.Some 5L) Stdlib.Option.Option.None (fun x -> x + 1L) | Expected TSum ("Darklang.Stdlib.Option.Option", [TVar "t"]), got TInt64 |
| [L260](../../../../test/fixtures/e2e/upstream/stdlib/option.dark#L260) — Stdlib.Option.mapWithDefault Stdlib.Option.Option.None Stdlib.Option.Option.None (fun x -> x + 1L) | Expected TSum ("Darklang.Stdlib.Option.Option", [TVar "t"]), got TInt64 |
