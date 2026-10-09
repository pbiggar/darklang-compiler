# language/nested-fns.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/language/nested-fns.dark) · [File list](../../current-audit.md)

Executed 12 assertions: **10 passed, 2 failed**.

| Test | Observed failure |
| --- | --- |
| [L55](../../../../test/fixtures/e2e/upstream/language/nested-fns.dark#L55) — let sumDown (n: Int64) : Int64 = if n <= 0L then 0L else n + (sumDown (n - 1L)) sumDown 3L | Expected compilation error but compilation succeeded |
| [L60](../../../../test/fixtures/e2e/upstream/language/nested-fns.dark#L60) — let isEven (n: Int64) : Bool = if n == 0L then true else isOdd (n - 1L) let isOdd (n: Int64) : Bool = if n ... | Expected compilation error but compilation succeeded |
