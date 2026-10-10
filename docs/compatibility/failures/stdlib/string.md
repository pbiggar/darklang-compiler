# stdlib/string.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/string.dark) · [File list](../../current-audit.md)

Executed 640 assertions individually: **593 passed, 47 failed** after the Char/String type fix and L418 compile-error override.

| Test | Observed failure |
| --- | --- |
| [L182](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L182) — let v =   Stdlib.String.map "a string" (fun x ->     let _ = Builtin.testIncrementSideEffectCounter false in 'c')  (v, Builtin.testSideEffectCount ()) | <entry>: Unknown function or value 'Builtin.testIncrementSideEffectCounter' |
| [L191](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L191) — Stdlib.String.fromChar (c "1") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L192](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L192) — Stdlib.String.fromChar (c "👩‍👩‍👧‍👦") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L193](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L193) — Stdlib.String.fromChar (c "🏳️‍⚧️‍️") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L194](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L194) — Stdlib.String.fromChar (c "👱🏾") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L195](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L195) — Stdlib.String.fromChar (c "Z̤͔ͧ̑̓") | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L382](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L382) — Builtin.stringSlugify   "  M@y  'super'  Really- exce+llent *Uber_ ama\\"zing* ~very   5x5 ~ \\"clever\\" thing: coffee😭!" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L385](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L385) — Builtin.stringSlugify   "  m@y  'super'  really- excellent *uber_ amazing* ~very  ~ \\"clever\\" thing: coffee😭!" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L388](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L388) — Builtin.stringSlugify "" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L389](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L389) — Builtin.stringSlugify "ABCD-45646sassa" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L390](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L390) — Builtin.stringSlugify "ddsd516ds125sd12sd12Ü" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L391](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L391) — Builtin.stringSlugify "q=\\u0002$\\u001a<+MC" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L392](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L392) — Builtin.stringSlugify "🎁🎄Ǣʚ231" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L393](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L393) — Builtin.stringSlugify "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L394](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L394) — Builtin.stringSlugify "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L395](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L395) — Builtin.stringSlugify "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L396](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L396) — Builtin.stringSlugify "🧟‍♀️🧟‍♂️" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L397](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L397) — Builtin.stringSlugify "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L399](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L399) — Builtin.stringSlugify   "b\\x01c\\x02d\\x03e\\x04f\\x05g\\x06h\\x07i\\x08j\\x09k\\x0Al\\x0Bm\\x0Cn\\x0Do\\x0Ep\\x0Fq" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L402](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L402) — Builtin.stringSlugify   "a\\x10b\\x11c\\x12d\\x13e\\x14f\\x15g\\x16h\\x17i\\x18j\\x19k\\x1Al\\x1Bm\\x1Cn\\x1Do\\x1Ep\\x1Fq" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L405](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L405) — Builtin.stringSlugify "!\\"#$%&'()*+,-./" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L406](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L406) — Builtin.stringSlugify ":;<=>?@" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L407](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L407) — Builtin.stringSlugify "[\\\\]^_`" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L408](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L408) — Builtin.stringSlugify "{\|}~\\x7F" | <entry>: Unknown function or value 'Builtin.stringSlugify' |
| [L413](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L413) — Stdlib.String.fromList [ c "a" ] | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L415](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L415) — Stdlib.String.fromList [ c "👩‍👩‍👧‍👦", c "🏳️‍⚧️‍️", c "👱🏾", c "Z̤͔ͧ̑̓" ] | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L422](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L422) — Stdlib.String.toList "👨‍👩‍👧‍👦" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L424](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L424) — Stdlib.String.toList "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L427](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L427) — Stdlib.String.toList "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L430](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L430) — Stdlib.String.toList "🧟‍♀️🧟‍♂️" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L433](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L433) — Stdlib.String.toList "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L436](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L436) — Stdlib.String.toList "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble parse error in test/fixtures/e2e/upstream/stdlib/string.dark: Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L849](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L849) — Stdlib.String.padEndToWidth "" 0 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L850](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L850) — Stdlib.String.padEndToWidth "abc" 3 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L851](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L851) — Stdlib.String.padEndToWidth "abc" 6 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L852](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L852) — Stdlib.String.padEndToWidth "abc" -3 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L853](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L853) — Stdlib.String.padEndToWidth "abcdef" 3 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L854](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L854) — Stdlib.String.padEndToWidth "界" 4 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L855](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L855) — Stdlib.String.padEndToWidth "界界界" 6 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L856](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L856) — Stdlib.String.padEndToWidth "🙂" 4 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L857](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L857) — Stdlib.String.padEndToWidth "e\\u0301" 3 | <entry>: Unknown function or value 'Stdlib.String.padEndToWidth' |
| [L859](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L859) — Stdlib.String.padStartToWidth "" 0 | <entry>: Unknown function or value 'Stdlib.String.padStartToWidth' |
| [L860](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L860) — Stdlib.String.padStartToWidth "abc" 6 | <entry>: Unknown function or value 'Stdlib.String.padStartToWidth' |
| [L861](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L861) — Stdlib.String.padStartToWidth "abc" 3 | <entry>: Unknown function or value 'Stdlib.String.padStartToWidth' |
| [L862](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L862) — Stdlib.String.padStartToWidth "abcdef" 3 | <entry>: Unknown function or value 'Stdlib.String.padStartToWidth' |
| [L863](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L863) — Stdlib.String.padStartToWidth "界" 4 | <entry>: Unknown function or value 'Stdlib.String.padStartToWidth' |
| [L864](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L864) — Stdlib.String.padStartToWidth "界界界" 6 | <entry>: Unknown function or value 'Stdlib.String.padStartToWidth' |
