# stdlib/string.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/string.dark) · [File list](../../current-audit.md)

Executed 640 assertions individually: **626 passed, 14 failed** after the Char/String type fix, L418 compile-error override, display-width padding, and portable slugify implementation.

| Test | Observed failure |
| --- | --- |
| [L182](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L182) — let v =   Stdlib.String.map "a string" (fun x ->     let _ = Builtin.testIncrementSideEffectCounter false in 'c')  (v, Builtin.testSideEffectCount ()) | <entry>: Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L191](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L191) — Stdlib.String.fromChar (c "1") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L192](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L192) — Stdlib.String.fromChar (c "👩‍👩‍👧‍👦") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L193](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L193) — Stdlib.String.fromChar (c "🏳️‍⚧️‍️") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L194](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L194) — Stdlib.String.fromChar (c "👱🏾") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L195](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L195) — Stdlib.String.fromChar (c "Z̤͔ͧ̑̓") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L413](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L413) — Stdlib.String.fromList [ c "a" ] | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L415](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L415) — Stdlib.String.fromList [ c "👩‍👩‍👧‍👦", c "🏳️‍⚧️‍️", c "👱🏾", c "Z̤͔ͧ̑̓" ] | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L422](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L422) — Stdlib.String.toList "👨‍👩‍👧‍👦" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L424](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L424) — Stdlib.String.toList "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L427](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L427) — Stdlib.String.toList "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L430](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L430) — Stdlib.String.toList "🧟‍♀️🧟‍♂️" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L433](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L433) — Stdlib.String.toList "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L436](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L436) — Stdlib.String.toList "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
