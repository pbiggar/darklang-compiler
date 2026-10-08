# stdlib/float.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/float.dark) · [File list](../../current-audit.md)

Executed 167 assertions: **145 passed, 22 failed**.

| Test | Observed failure |
| --- | --- |
| [L47](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L47) — Stdlib.Float.floor Builtin.testNegativeInfinity | Expected error message 'Encountered out-of-range value for type of Int' not found in stderr. Actual stderr: <entry>: Unknown function or value 'Builtin.testNegativeInfinity' |
| [L55](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L55) — Stdlib.Float.roundTowardsZero Builtin.testNegativeInfinity | Expected error message 'Encountered out-of-range value for type of Int' not found in stderr. Actual stderr: <entry>: Unknown function or value 'Builtin.testNegativeInfinity' |
| [L58](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L58) — Stdlib.Float.absoluteValue Builtin.testNegativeInfinity | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L59](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L59) — Stdlib.Float.absoluteValue Builtin.testNan | Value mismatch |
| [L64](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L64) — Stdlib.Float.negate Builtin.testNan | Value mismatch |
| [L65](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L65) — Stdlib.Float.negate Builtin.testInfinity | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L73](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L73) — Stdlib.Float.clamp Builtin.testNegativeInfinity -1.0 0.5 | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L76](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L76) — Stdlib.Float.clamp Builtin.testNan -1.0 1.0 | Value mismatch |
| [L81](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L81) — Stdlib.Float.clamp 0.5 Builtin.testNegativeInfinity 1.0 | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L89](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L89) — Stdlib.Float.clamp -1.0 0.5 Builtin.testNegativeInfinity | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L107](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L107) — Stdlib.Float.divide 9.0 -0.0 | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L110](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L110) — 17.0 / 3.3 | Value mismatch |
| [L111](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L111) — -8.74 / 5.351 | Value mismatch |
| [L125](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L125) — Stdlib.Float.max Builtin.testNegativeInfinity 1.0 | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L127](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L127) — Stdlib.Float.max 10.0 Builtin.testNan | Value mismatch |
| [L134](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L134) — Stdlib.Float.min Builtin.testNegativeInfinity 1.0 | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L136](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L136) — Stdlib.Float.min 10.0 Builtin.testNan | Value mismatch |
| [L160](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L160) — Stdlib.Float.parse "0.7999999999" | Value mismatch |
| [L173](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L173) — Stdlib.Float.parse "-5.55555555556e+28" | Value mismatch |
| [L176](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L176) — Stdlib.Float.parse "-1.8E+308" | Unknown function or value 'Builtin.testNegativeInfinity' |
| [L244](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L244) — Stdlib.Float.toInt Builtin.testNegativeInfinity | Expected error message 'Encountered out-of-range value for type of Int' not found in stderr. Actual stderr: <entry>: Unknown function or value 'Builtin.testNegativeInfinity' |
| [L255](../../../../test/fixtures/e2e/upstream/stdlib/float.dark#L255) — Stdlib.Float.toBits -0.0 | Unknown function or value 'Builtin.testNegativeInfinity' |
