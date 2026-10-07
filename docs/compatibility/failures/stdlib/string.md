# stdlib/string.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/stdlib/string.dark) · [File list](../../current-audit.md)

Executed 640 assertions: **0 passed, 640 failed**.
Of 640 previously disabled assertions, **0 passed and 640 failed**.

| Test | Observed failure |
| --- | --- |
| [L5](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L5) — "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L6](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L6) — "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L7](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L7) — "Είναι προικισμένοι με λογική" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L8](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L8) — "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L11](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L11) — "" ++ "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L12](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L12) — "a" ++ "̂" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L13](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L13) — "hello" ++ " world" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L14](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L14) — "ᄀ" ++ "ᅡᆨ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L15](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L15) — "" ++ "a" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L16](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L16) — "a" ++ "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L17](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L17) — "a" ++ "̂" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L20](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L20) — Stdlib.String.append "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L21](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L21) — Stdlib.String.append "hello" " world" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L22](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L22) — Stdlib.String.append works for ASCII range | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L24](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L24) — Stdlib.String.append works on non-ascii strings | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L25](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L25) — Stdlib.String.append "🧑🏼‍💻" "🧑🏻‍🍼" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L26](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L26) — Stdlib.String.append "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L27](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L27) — Stdlib.String.append "🧟‍♂️🧟‍♀️" "🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L28](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L28) — Stdlib.String.append "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "👨‍❤️‍💋‍👨" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L32](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L32) — (Stdlib.String.join [ "a", "b", "c", "d" ] "\|") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L33](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L33) — (Stdlib.String.join [ "a", "̂" ] "") \|> Stdlib.String.base64UrlEncode | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L34](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L34) — Stdlib.String.join [ "hello", " world" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L35](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L35) — Stdlib.String.join [ "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽", "🧟‍♀️" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L36](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L36) — Stdlib.String.join [ "👱👱🏻👱🏼👱🏽👱🏾👱🏿", "👨‍❤️‍💋‍👨", "﷽﷽﷽" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L37](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L37) — Stdlib.String.join [ "🧟‍♀️🧟‍♂️", "🧟‍♀️🧑🏽‍🦰" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L38](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L38) — Stdlib.String.join [ "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️", "‍⚧️‍️🇵🇷" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L39](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L39) — Stdlib.String.join [ "🧟‍♀️🧟‍♂️‍", "🧟‍♀️🧑🏽‍🦰‍‍" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L40](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L40) — Stdlib.String.join [ "🧑🏽‍🦰‍", "🧑🏼‍💻‍‍" ] "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L43](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L43) — Stdlib.List.length (Stdlib.String.toBytes "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L44](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L44) — Stdlib.List.length (Stdlib.String.toBytes "😄APPLE🍏") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L45](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L45) — Stdlib.List.length (Stdlib.String.toBytes "Είναι προικισμένοι με λογική") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L46](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L46) — Stdlib.List.length (Stdlib.String.toBytes "") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L47](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L47) — Stdlib.List.length (Stdlib.String.toBytes "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L48](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L48) — Stdlib.List.length (Stdlib.String.toBytes "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L49](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L49) — Stdlib.List.length (Stdlib.String.toBytes "👱👱🏻👱🏼👱🏽👱🏾👱🏿") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L50](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L50) — Stdlib.List.length (Stdlib.String.toBytes "🧟‍♀️🧟‍♂️") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L51](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L51) — Stdlib.List.length (Stdlib.String.toBytes "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L52](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L52) — Stdlib.List.length (Stdlib.String.toBytes "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L53](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L53) — Stdlib.List.length (Stdlib.String.toBytes "") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L62](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L62) — (Stdlib.Base64.decode "w6I") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytesWithReplacement | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L68](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L68) — (Stdlib.Base64.decode "aGVsbG8g8J+YgA==") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromByte... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L74](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L74) — (Stdlib.Base64.decode "ww==") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytesWithReplace... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L80](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L80) — (Stdlib.Base64.decode "7aCA") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytesWithReplace... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L86](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L86) — (Stdlib.Base64.decode "aMM=") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytesWithReplace... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L94](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L94) — (Stdlib.Base64.decode "w6I") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytes | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L100](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L100) — (Stdlib.Base64.decode "aGVsbG8g8J+YgA==") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytes | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L106](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L106) — (Stdlib.Base64.decode "ww==") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytes | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L112](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L112) — (Stdlib.Base64.decode "7aCA") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytes | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L118](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L118) — (Stdlib.Base64.decode "aMM=") \|> Builtin.unwrap \|> Stdlib.Blob.toList \|> Stdlib.String.fromBytes | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L126](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L126) — Stdlib.String.startsWith "a string" "a s" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L127](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L127) — Stdlib.String.startsWith "a string" " s" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L128](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L128) — Stdlib.String.startsWith "żółw" "żó" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L129](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L129) — Stdlib.String.startsWith "żółw" "r22" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L130](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L130) — Stdlib.String.startsWith "👩🏻‍🚀🍇" "🍇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L131](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L131) — Stdlib.String.startsWith "123456" "123" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L132](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L132) — Stdlib.String.startsWith "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L133](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L133) — Stdlib.String.startsWith "E" "\u0014\u0004" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L134](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L134) — Stdlib.String.startsWith "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" "🧑🏾‍🦰" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L135](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L135) — Stdlib.String.startsWith "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L136](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L136) — Stdlib.String.startsWith "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L137](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L137) — Stdlib.String.startsWith "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L138](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L138) — Stdlib.String.startsWith "🧟‍♀️🧟‍♂️" "🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L139](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L139) — Stdlib.String.startsWith "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L140](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L140) — Stdlib.String.startsWith "a string" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L143](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L143) — Stdlib.String.endsWith "a string" "in" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L144](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L144) — Stdlib.String.endsWith "a string" "ing" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L145](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L145) — Stdlib.String.endsWith "a string" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L146](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L146) — Stdlib.String.endsWith "żółw" "żó" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L147](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L147) — Stdlib.String.endsWith "żółw" "łw" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L148](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L148) — Stdlib.String.endsWith "👩🏻‍🚀🍇" "🍇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L149](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L149) — Stdlib.String.endsWith "123456" "56" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L150](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L150) — Stdlib.String.endsWith "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L151](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L151) — Stdlib.String.endsWith "E" "\u0014\u0004" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L152](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L152) — Stdlib.String.endsWith "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" "✋✋🏿✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L153](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L153) — Stdlib.String.endsWith "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "ǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L154](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L154) — Stdlib.String.endsWith "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "12xsd" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L155](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L155) — Stdlib.String.endsWith "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L156](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L156) — Stdlib.String.endsWith "🧟‍♀️🧟‍♂️" "🧟‍♀️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L157](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L157) — Stdlib.String.endsWith "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L161](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L161) — Stdlib.String.map "a string" (fun x -> x) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L162](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L162) — Stdlib.String.map "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" (fun x -> x) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L163](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L163) — Stdlib.String.map "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" (fun x -> x) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L164](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L164) — Stdlib.String.map "👱👱🏻👱🏼👱🏽👱🏾👱🏿" (fun x -> x) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L165](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L165) — Stdlib.String.map "🧟‍♀️🧟‍♂️" (fun x -> x) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L167](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L167) — Stdlib.String.map "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" (fun x -> x) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L169](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L169) — Stdlib.String.map "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" (fun x -> 'c') | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L182](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L182) — let v = Stdlib.String.map "a string" (fun x -> let _ = Builtin.testIncrementSideEffectCounter false in 'c')... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L190](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L190) — Stdlib.String.fromChar 'a' | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L191](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L191) — Stdlib.String.fromChar (c "1") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L192](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L192) — Stdlib.String.fromChar (c "👩‍👩‍👧‍👦") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L193](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L193) — Stdlib.String.fromChar (c "🏳️‍⚧️‍️") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L194](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L194) — Stdlib.String.fromChar (c "👱🏾") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L195](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L195) — Stdlib.String.fromChar (c "Z̤͔ͧ̑̓") | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L200](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L200) — empty case | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L201](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L201) — Stdlib.String.base64Decode "Kw" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L202](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L202) — Stdlib.String.base64Decode "LyotKygmQDk4NTIx" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L203](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L203) — Stdlib.String.base64Decode "yLo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L204](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L204) — Stdlib.String.base64Decode "xbzDs8WCdw" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L206](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L206) — Stdlib.String.base64Decode "random string" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L207](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L207) — Stdlib.String.base64Decode "illegal chars&@:" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L211](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L211) — Stdlib.String.base64Decode "Zg" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L212](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L212) — Stdlib.String.base64Decode "Zg==" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L213](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L213) — Stdlib.String.base64Decode "Zm8" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L214](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L214) — Stdlib.String.base64Decode "Zm8=" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L215](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L215) — Stdlib.String.base64Decode "Zm9v" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L216](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L216) — Stdlib.String.base64Decode "Zm9vYg" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L217](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L217) — Stdlib.String.base64Decode "Zm9vYg==" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L218](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L218) — Stdlib.String.base64Decode "Zm9vYmE" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L219](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L219) — Stdlib.String.base64Decode "Zm9vYmE=" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L220](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L220) — Stdlib.String.base64Decode "Zm9vYmFy" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L225](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L225) — Stdlib.String.base64Decode "ZE==" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L226](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L226) — Stdlib.String.base64Decode "ZmC=" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L227](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L227) — Stdlib.String.base64Decode "Zm9vYE==" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L228](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L228) — Stdlib.String.base64Decode "Zm9vYmC=" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L230](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L230) — Stdlib.String.base64Decode "ZnJvbT0wNi8wNy8yMDEzIHF1ZXJ5PSLOms6xzrvPjs-CIM6_z4HOr8-DzrHPhM61Ig" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L233](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L233) — Stdlib.String.base64Decode "8J-RsfCfkbHwn4-78J-RsfCfj7zwn5Gx8J-PvfCfkbHwn4--8J-RsfCfj78" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L236](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L236) — Stdlib.String.base64Decode "-p" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L237](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L237) — Stdlib.String.base64Decode "lI" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L238](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L238) — Stdlib.String.base64Decode "5Sk" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L242](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L242) — Stdlib.String.base64UrlEncode "+" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L243](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L243) — Stdlib.String.base64UrlEncode "Ⱥ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L244](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L244) — Stdlib.String.base64UrlEncode "żółw" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L245](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L245) — Stdlib.String.base64UrlEncode "/*-+(&@98521" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L246](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L246) — Stdlib.String.base64UrlEncode "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L247](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L247) — Stdlib.String.base64UrlEncode "f" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L248](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L248) — Stdlib.String.base64UrlEncode "fo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L249](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L249) — Stdlib.String.base64UrlEncode "foo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L250](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L250) — Stdlib.String.base64UrlEncode "foob" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L251](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L251) — Stdlib.String.base64UrlEncode "fooba" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L252](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L252) — Stdlib.String.base64UrlEncode "foobar" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L253](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L253) — Stdlib.String.base64UrlEncode "Hello World" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L255](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L255) — Stdlib.String.base64UrlEncode "from=06/07/2013 query=\"Καλώς ορίσατε\"" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L257](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L257) — Stdlib.String.base64UrlEncode "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L261](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L261) — Stdlib.String.base64Encode "+" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L262](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L262) — Stdlib.String.base64Encode "Ⱥ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L263](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L263) — Stdlib.String.base64Encode "żółw" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L264](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L264) — Stdlib.String.base64Encode "/*-+(&@98521" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L265](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L265) — Stdlib.String.base64Encode "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L266](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L266) — Stdlib.String.base64Encode "f" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L267](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L267) — Stdlib.String.base64Encode "fo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L268](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L268) — Stdlib.String.base64Encode "foo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L269](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L269) — Stdlib.String.base64Encode "foob" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L270](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L270) — Stdlib.String.base64Encode "fooba" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L271](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L271) — Stdlib.String.base64Encode "foobar" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L272](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L272) — Stdlib.String.base64Encode "Hello World" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L274](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L274) — Stdlib.String.base64Encode "from=06/07/2013 query=\"Καλώς ορίσατε\"" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L276](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L276) — Stdlib.String.base64Encode "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L280](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L280) — Stdlib.String.digest "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L281](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L281) — Stdlib.String.digest "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L282](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L282) — Stdlib.String.digest "ελπίδα" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L283](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L283) — Stdlib.String.digest "/*-+(&@98521" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L284](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L284) — Stdlib.String.digest "👩🏻‍🚀🍇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L285](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L285) — Stdlib.String.digest "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L286](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L286) — Stdlib.String.digest "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L287](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L287) — Stdlib.String.digest "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L288](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L288) — Stdlib.String.digest "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L289](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L289) — Stdlib.String.digest "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L293](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L293) — (Stdlib.String.random 5) == (Stdlib.String.random 5) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L295](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L295) — Stdlib.String.length ((Stdlib.String.random 10) \|> Builtin.unwrap) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L296](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L296) — Stdlib.String.length ((Stdlib.String.random 5) \|> Builtin.unwrap) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L297](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L297) — Stdlib.String.length ((Stdlib.String.random 0) \|> Builtin.unwrap) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L299](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L299) — Stdlib.String.random -1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L303](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L303) — HTML escaping works reasonably | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L305](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L305) — HTML escaping works reasonably | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L308](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L308) — Stdlib.String.htmlEscape "<html><head><!-- head definitions go here --></head><body><!-- the content goes h... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L311](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L311) — Stdlib.String.htmlEscape "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L312](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L312) — Stdlib.String.htmlEscape "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L313](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L313) — Stdlib.String.htmlEscape "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L315](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L315) — Stdlib.String.htmlEscape "<html><head></head><body><h1>﷽﷽﷽﷽﷽</h1></body></html>" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L317](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L317) — Stdlib.String.htmlEscape "<head>🧟‍♀️🧟‍♂️</head>" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L318](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L318) — Stdlib.String.htmlEscape "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L322](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L322) — Stdlib.String.isEmpty "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L323](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L323) — Stdlib.String.isEmpty "a" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L324](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L324) — Stdlib.String.isEmpty "🧑🏼‍💻🧑🏻‍🍼" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L325](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L325) — Stdlib.String.isEmpty "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L326](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L326) — Stdlib.String.isEmpty "﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L327](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L327) — Stdlib.String.isEmpty "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L328](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L328) — Stdlib.String.isEmpty "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L329](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L329) — Stdlib.String.isEmpty "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L333](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L333) — Stdlib.String.newline | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L337](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L337) — Stdlib.String.length "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L338](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L338) — Stdlib.String.length "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L339](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L339) — Stdlib.String.length "abcdef" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L340](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L340) — Stdlib.String.length "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L341](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L341) — Stdlib.String.length "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L342](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L342) — Stdlib.String.length "﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L343](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L343) — Stdlib.String.length "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L344](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L344) — Stdlib.String.length "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L345](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L345) — Stdlib.String.length "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L349](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L349) — Stdlib.String.prepend works for ASCII range | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L350](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L350) — Stdlib.String.prepend "hello" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L351](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L351) — Stdlib.String.prepend "" "hello" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L352](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L352) — Stdlib.String.prepend works on non-ascii strings | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L353](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L353) — Stdlib.String.prepend "123" "456" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L354](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L354) — Stdlib.String.prepend "óñÜá" "abc" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L355](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L355) — Stdlib.String.prepend "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "Z̤͔ͧ̑̓" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L356](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L356) — Stdlib.String.prepend "﷽﷽﷽﷽﷽" "👨‍❤️‍💋‍👨" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L357](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L357) — Stdlib.String.prepend "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L358](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L358) — Stdlib.String.prepend "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "✋🏻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L359](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L359) — Stdlib.String.prepend "🧟‍♀️🧟‍♂️" "🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L360](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L360) — Stdlib.String.prepend "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "👨‍❤️‍💋‍👨" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L361](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L361) — Stdlib.String.prepend "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L365](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L365) — Stdlib.String.replaceAll "abcABCcbaCBA" "b" "x" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L366](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L366) — Stdlib.String.replaceAll "abcABCcbaCBA" "" "x" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L367](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L367) — Stdlib.String.replaceAll "" "" "&" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L368](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L368) — Stdlib.String.replaceAll "abcABCcbaCBA" "b" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L370](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L370) — Stdlib.String.replaceAll "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "ä͖̭̈̇" "$" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L372](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L372) — Stdlib.String.replaceAll "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "﷽﷽" "$" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L373](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L373) — Stdlib.String.replaceAll "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏽" "✋🏻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L374](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L374) — Stdlib.String.replaceAll "🧟‍♀️🧟‍♂️" "🧟‍♂️" "🧑🏽‍🦰" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L376](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L376) — Stdlib.String.replaceAll "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "👨‍❤️‍💋‍👨" "👨‍❤️‍💋‍👨" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L378](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L378) — Stdlib.String.replaceAll "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "🧑🏻‍🍼" "🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L382](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L382) — Builtin.stringSlugify " M@y 'super' Really- exce+llent *Uber_ ama\"zing* ~very 5x5 ~ \"clever\" thing: coff... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L385](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L385) — Builtin.stringSlugify " m@y 'super' really- excellent *uber_ amazing* ~very ~ \"clever\" thing: coffee😭!" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L388](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L388) — Builtin.stringSlugify "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L389](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L389) — Builtin.stringSlugify "ABCD-45646sassa" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L390](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L390) — Builtin.stringSlugify "ddsd516ds125sd12sd12Ü" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L391](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L391) — Builtin.stringSlugify "q=\u0002$\u001a<+MC" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L392](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L392) — Builtin.stringSlugify "🎁🎄Ǣʚ231" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L393](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L393) — Builtin.stringSlugify "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L394](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L394) — Builtin.stringSlugify "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L395](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L395) — Builtin.stringSlugify "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L396](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L396) — Builtin.stringSlugify "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L397](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L397) — Builtin.stringSlugify "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L399](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L399) — Builtin.stringSlugify "b\x01c\x02d\x03e\x04f\x05g\x06h\x07i\x08j\x09k\x0Al\x0Bm\x0Cn\x0Do\x0Ep\x0Fq" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L402](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L402) — Builtin.stringSlugify "a\x10b\x11c\x12d\x13e\x14f\x15g\x16h\x17i\x18j\x19k\x1Al\x1Bm\x1Cn\x1Do\x1Ep\x1Fq" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L405](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L405) — Builtin.stringSlugify "!\"#$%&'()*+,-./" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L406](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L406) — Builtin.stringSlugify ":;<=>?@" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L407](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L407) — Builtin.stringSlugify "[\\]^_'" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L408](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L408) — Builtin.stringSlugify "{\|}~\x7F" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L412](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L412) — Stdlib.String.fromList [] | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L413](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L413) — Stdlib.String.fromList [ c "a" ] | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L415](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L415) — Stdlib.String.fromList [ c "👩‍👩‍👧‍👦", c "🏳️‍⚧️‍️", c "👱🏾", c "Z̤͔ͧ̑̓" ] | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L417](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L417) — Stdlib.String.fromList [ "a" ] | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L419](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L419) — Stdlib.String.toList "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L420](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L420) — Stdlib.String.toList "ab" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L421](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L421) — Stdlib.String.toList "👨‍👩‍👧‍👦" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L423](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L423) — Stdlib.String.toList "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L426](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L426) — Stdlib.String.toList "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L429](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L429) — Stdlib.String.toList "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L432](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L432) — Stdlib.String.toList "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L435](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L435) — Stdlib.String.toList "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L438](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L438) — ("ab1" \|> Stdlib.String.toList \|> Stdlib.String.fromList) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L440](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L440) — ("@Ǣá1" \|> Stdlib.String.toList \|> Stdlib.String.fromList) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L442](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L442) — "👩‍👩‍👧‍👦🏳️‍⚧️‍️👱🏾Z̤͔ͧ̑̓" \|> Stdlib.String.toList \|> Stdlib.String.fromList | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L448](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L448) — Stdlib.String.split "hello world" "notfound" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L449](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L449) — Stdlib.String.split "hello😄world" "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L450](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L450) — Stdlib.String.split "hello&&&&world" "&&&&" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L451](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L451) — Stdlib.String.split "hello34564world34564sun" "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L452](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L452) — Stdlib.String.split "hello34564world34564sun" "34564" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L453](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L453) — Stdlib.String.split "" "34564" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L454](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L454) — Stdlib.String.split "34564" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L455](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L455) — Stdlib.String.split "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" "🧑🏻‍🍼" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L456](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L456) — Stdlib.String.split "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L457](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L457) — Stdlib.String.split "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L458](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L458) — Stdlib.String.split "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏼👱🏽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L459](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L459) — Stdlib.String.split "🧟‍♀️🧟‍♂️" "👱🏽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L460](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L460) — Stdlib.String.split "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "👩‍👩‍👧‍👦" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L461](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L461) — Stdlib.String.split "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "🧑🏽‍🦰" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L462](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L462) — Stdlib.String.split "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L463](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L463) — Stdlib.String.split "666666" "6" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L464](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L464) — Stdlib.String.split "55555" "5" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L465](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L465) — Stdlib.String.split "4444" "4" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L466](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L466) — Stdlib.String.split "333" "3" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L467](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L467) — Stdlib.String.split "22" "2" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L468](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L468) — Stdlib.String.split "1" "1" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L469](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L469) — Stdlib.String.split "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L470](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L470) — Stdlib.String.split "666666x" "6" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L471](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L471) — Stdlib.String.split "55555x" "5" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L472](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L472) — Stdlib.String.split "4444x" "4" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L473](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L473) — Stdlib.String.split "333x" "3" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L474](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L474) — Stdlib.String.split "22x" "2" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L475](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L475) — Stdlib.String.split "1x" "1" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L476](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L476) — Stdlib.String.split "x666666" "6" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L477](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L477) — Stdlib.String.split "x55555" "5" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L478](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L478) — Stdlib.String.split "x4444" "4" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L479](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L479) — Stdlib.String.split "x333" "3" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L480](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L480) — Stdlib.String.split "x22" "2" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L481](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L481) — Stdlib.String.split "x1" "1" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L482](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L482) — Stdlib.String.split "x666666y" "6" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L483](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L483) — Stdlib.String.split "x55555y" "5" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L484](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L484) — Stdlib.String.split "x4444y" "4" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L485](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L485) — Stdlib.String.split "x333y" "3" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L486](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L486) — Stdlib.String.split "x22y" "2" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L487](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L487) — Stdlib.String.split "x1y" "1" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L488](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L488) — Stdlib.String.split "6a6aa6aaa6aaaa" "a" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L490](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L490) — Stdlib.String.split "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L492](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L492) — Stdlib.String.split "👨‍👩‍👧‍👦" "👩" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L496](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L496) — Stdlib.String.splitFirst "a\|b" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L497](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L497) — Stdlib.String.splitFirst "a\|b\|c\|d" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L498](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L498) — Stdlib.String.splitFirst "key=value" "=" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L499](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L499) — Stdlib.String.splitFirst "PATH=/usr/bin:/bin" "=" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L500](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L500) — Stdlib.String.splitFirst "Date: Wed, 14 May 2026 09:00:00 GMT" ": " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L502](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L502) — Stdlib.String.splitFirst "abc" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L503](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L503) — Stdlib.String.splitFirst "" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L504](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L504) — Stdlib.String.splitFirst "\|abc" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L505](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L505) — Stdlib.String.splitFirst "abc\|" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L506](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L506) — Stdlib.String.splitFirst "\|\|" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L508](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L508) — Stdlib.String.splitFirst "a==b==c" "==" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L509](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L509) — Stdlib.String.splitFirst "no-sep-here" "==" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L511](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L511) — Stdlib.String.splitFirst "abc" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L512](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L512) — Stdlib.String.splitFirst "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L514](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L514) — Stdlib.String.splitFirst "hello😄world" "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L515](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L515) — Stdlib.String.splitFirst "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼" "🧑🏼‍💻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L518](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L518) — Stdlib.String.splitFirst "🧑🏼‍💻rest" "🧑🏼‍💻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L519](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L519) — Stdlib.String.splitFirst "rest🧑🏼‍💻" "🧑🏼‍💻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L520](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L520) — Stdlib.String.splitFirst "żółw🧑🏼‍💻tail" "🧑🏼‍💻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L523](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L523) — Stdlib.String.splitFirst "ą́\|b" "\|" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L526](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L526) — Stdlib.String.splitFirst "🧑🏼‍💻" "🧑" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L530](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L530) — Stdlib.String.splitN "a\|b\|c\|d" "\|" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L531](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L531) — Stdlib.String.splitN "a\|b\|c\|d" "\|" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L532](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L532) — Stdlib.String.splitN "a\|b\|c\|d" "\|" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L533](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L533) — Stdlib.String.splitN "a\|b\|c\|d" "\|" 99 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L535](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L535) — Stdlib.String.splitN "a\|b\|c\|d" "\|" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L536](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L536) — Stdlib.String.splitN "a\|b\|c\|d" "\|" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L537](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L537) — Stdlib.String.splitN "a\|b\|c\|d" "\|" -1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L539](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L539) — Stdlib.String.splitN "abc" "\|" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L540](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L540) — Stdlib.String.splitN "" "\|" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L542](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L542) — Stdlib.String.splitN "abc" "" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L543](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L543) — Stdlib.String.splitN "a\|b" "" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L545](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L545) — Stdlib.String.splitN "hello😄world😄sun" "😄" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L546](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L546) — Stdlib.String.splitN "a==b==c==d" "==" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L549](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L549) — Stdlib.String.splitN "🧑🏽‍🦰\|🧑🏼‍💻\|🧑🏻‍🍼" "\|" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L550](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L550) — Stdlib.String.splitN "🧑🏽‍🦰\|🧑🏼‍💻\|🧑🏻‍🍼" "\|" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L553](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L553) — Stdlib.String.splitN "a🧑🏼‍💻b🧑🏼‍💻c" "🧑🏼‍💻" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L554](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L554) — Stdlib.String.splitN "a🧑🏼‍💻b🧑🏼‍💻c" "🧑🏼‍💻" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L555](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L555) — Stdlib.String.splitN "żółw🧑🏼‍💻x🧑🏼‍💻y" "🧑🏼‍💻" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L558](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L558) — Stdlib.String.splitN "🧑🏼‍💻🧑🏼‍💻x" "🧑🏼‍💻" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L562](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L562) — Stdlib.String.toLowercase "HELLO😄WORLD" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L563](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L563) — Stdlib.String.toLowercase "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L564](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L564) — Stdlib.String.toLowercase works for ASCII range | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L565](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L565) — Stdlib.String.toLowercase "AB323CDEF" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L566](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L566) — not lowercase a | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L567](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L567) — Stdlib.String.toLowercase "sánchez" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L568](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L568) — Stdlib.String.toLowercase works on non-ascii strings | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L569](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L569) — Stdlib.String.toLowercase "😄ORANGE" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L570](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L570) — Stdlib.String.toLowercase "🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L571](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L571) — Stdlib.String.toLowercase "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L572](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L572) — Stdlib.String.toLowercase "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L573](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L573) — Stdlib.String.toLowercase "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L574](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L574) — Stdlib.String.toLowercase "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L575](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L575) — Stdlib.String.toLowercase "ŻÓŁW🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L576](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L576) — Stdlib.String.toLowercase "Ჾ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L577](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L577) — Stdlib.String.toLowercase "Z̤͔ͧ̑̓Ä͖̭̈̇Lͮ̒ͫǦ̗͚̚O̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L579](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L579) — Stdlib.String.toLowercase "H̬̤̗̤͝e͜ ̜̥̝̻͍̟́w̕h̖̯͓o̝͙̖͎̱̮ ҉̺̙̞̟͈W̷̼̭a̺̪͍į͈͕̭͙̯̜t̶̼̮s̘͙͖̕ ̠̫̠B̻͍͙͉̳ͅe̵h̵̬͇̫͙i... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L585](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L585) — Stdlib.String.toUppercase "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L586](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L586) — Stdlib.String.toUppercase "hello😄world" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L587](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L587) — Stdlib.String.toUppercase "abcdef" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L588](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L588) — Stdlib.String.toUppercase "ab323cdef" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L589](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L589) — not lowercase a | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L590](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L590) — Stdlib.String.toUppercase "SÁNChEZ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L591](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L591) — Stdlib.String.toUppercase "żółw" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L592](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L592) — Stdlib.String.toUppercase "😄orange" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L593](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L593) — Stdlib.String.toUppercase "🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L594](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L594) — Stdlib.String.toUppercase "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L595](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L595) — Stdlib.String.toUppercase "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L596](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L596) — Stdlib.String.toUppercase "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L597](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L597) — Stdlib.String.toUppercase "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L598](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L598) — Stdlib.String.toUppercase "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L599](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L599) — Stdlib.String.toUppercase "ჾ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L615](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L615) — should be "FIFL" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L616](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L616) — should be "ԵՒ" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L618](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L618) — Stdlib.String.toUppercase "z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L620](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L620) — Stdlib.String.toUppercase "H̬̤̗̤͝e͜ ̜̥̝̻͍̟́w̕h̖̯͓o̝͙̖͎̱̮ ҉̺̙̞̟͈W̷̼̭a̺̪͍į͈͕̭͙̯̜t̶̼̮s̘͙͖̕ ̠̫̠B̻͍͙͉̳ͅe̵h̵̬͇̫͙i... | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L626](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L626) — Stdlib.String.trimEnd " " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L627](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L627) — Stdlib.String.trimEnd "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L628](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L628) — Stdlib.String.trimEnd " foo " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L629](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L629) — Stdlib.String.trimEnd " foo bar " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L630](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L630) — Stdlib.String.trimEnd " foo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L631](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L631) — Stdlib.String.trimEnd " 😄foobar😄 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L632](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L632) — Stdlib.String.trimEnd " foo bar " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L633](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L633) — Stdlib.String.trimEnd "foo " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L634](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L634) — Stdlib.String.trimEnd "foo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L635](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L635) — Stdlib.String.trimEnd " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L636](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L636) — Stdlib.String.trimEnd " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L637](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L637) — Stdlib.String.trimEnd " żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L638](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L638) — Stdlib.String.trimEnd " Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L639](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L639) — Stdlib.String.trimEnd " ﷽﷽ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L640](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L640) — Stdlib.String.trimEnd " 🧟‍♀️🧟‍♂️ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L641](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L641) — Stdlib.String.trimEnd " 👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L642](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L642) — Stdlib.String.trimEnd " żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L643](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L643) — Stdlib.String.trimEnd "🇺🇸🇷🇺🇸 🇦🇫🇦🇲🇸" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L647](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L647) — Stdlib.String.trimStart " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L648](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L648) — Stdlib.String.trimStart " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L649](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L649) — Stdlib.String.trimStart " " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L650](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L650) — Stdlib.String.trimStart "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L651](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L651) — Stdlib.String.trimStart " foo " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L652](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L652) — Stdlib.String.trimStart " foo bar " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L653](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L653) — Stdlib.String.trimStart " foo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L654](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L654) — Stdlib.String.trimStart " 😄foobar😄 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L655](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L655) — Stdlib.String.trimStart " foo bar " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L656](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L656) — Stdlib.String.trimStart "foo " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L657](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L657) — Stdlib.String.trimStart "foo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L658](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L658) — Stdlib.String.trimStart " żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L659](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L659) — Stdlib.String.trimStart " Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L660](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L660) — Stdlib.String.trimStart " ﷽﷽ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L661](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L661) — Stdlib.String.trimStart " 🧟‍♀️🧟‍♂️ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L662](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L662) — Stdlib.String.trimStart " 👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L663](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L663) — Stdlib.String.trimStart " żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L667](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L667) — Stdlib.String.trim " " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L668](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L668) — Stdlib.String.trim "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L669](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L669) — String trims both leading + trailing spaces | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L670](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L670) — String trims both leading + trailing spaces, leaving inner untouched | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L671](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L671) — String trims leading spaces | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L672](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L672) — String trims both leading + trailing spaces, preserving emoji | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L673](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L673) — String trims both leading + trailing spaces, leaving inner untouched w/ unicode spaces | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L674](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L674) — String trims trailing spaces | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L675](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L675) — String trim noops | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L676](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L676) — Stdlib.String.trim " żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L677](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L677) — Stdlib.String.trim " Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L678](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L678) — Stdlib.String.trim " ﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L679](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L679) — Stdlib.String.trim " 🧟‍♀️🧟‍♂️ " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L680](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L680) — Stdlib.String.trim " 👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L681](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L681) — Stdlib.String.trim " żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L682](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L682) — Stdlib.String.trim " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L683](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L683) — Stdlib.String.trim " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 " | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L684](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L684) — Stdlib.String.trim "쉆ꥨ逴皪巌䖑ⱝዓ淋" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L688](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L688) — Stdlib.String.reverse "abcde" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L689](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L689) — Stdlib.String.reverse "0abcde" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L690](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L690) — Stdlib.String.reverse "a" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L691](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L691) — Stdlib.String.reverse "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L692](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L692) — Stdlib.String.reverse "ábc" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L693](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L693) — Stdlib.String.reverse "🎁🧸Ǆʠ123" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L694](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L694) — Stdlib.String.reverse "😄foobar👽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L695](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L695) — Stdlib.String.reverse "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L696](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L696) — Stdlib.String.reverse "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L697](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L697) — Stdlib.String.reverse "👱👱🏻👱🏼👱🏽👱🏾👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L698](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L698) — Stdlib.String.reverse "🧟‍♀️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L699](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L699) — Stdlib.String.reverse "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L700](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L700) — Stdlib.String.reverse "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L704](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L704) — Stdlib.String.dropFirst "abcd" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L705](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L705) — Stdlib.String.dropFirst "abcd" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L706](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L706) — Stdlib.String.dropFirst "abcd" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L707](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L707) — Stdlib.String.dropFirst "" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L708](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L708) — Stdlib.String.dropFirst "abcd" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L709](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L709) — Stdlib.String.dropFirst "🍏🍒🍒" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L710](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L710) — Stdlib.String.dropFirst "🍏🍒🍍" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L711](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L711) — Stdlib.String.dropFirst "🍏a🍒b🍍c" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L712](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L712) — Stdlib.String.dropFirst "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" 5 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L713](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L713) — Stdlib.String.dropFirst "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L714](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L714) — Stdlib.String.dropFirst "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L715](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L715) — Stdlib.String.dropFirst "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L716](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L716) — Stdlib.String.dropFirst "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L717](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L717) — Stdlib.String.dropFirst "👱👱🏻👱🏼👱🏽👱🏾👱🏿" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L718](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L718) — Stdlib.String.dropFirst "🧟‍♀️🧟‍♂️" 20 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L719](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L719) — Stdlib.String.dropFirst "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L723](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L723) — Stdlib.String.dropLast "abcd" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L724](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L724) — Stdlib.String.dropLast "abcd" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L725](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L725) — Stdlib.String.dropLast "abcd" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L726](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L726) — Stdlib.String.dropLast "" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L727](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L727) — Stdlib.String.dropLast "🍏🍒🍒" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L728](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L728) — Stdlib.String.dropLast "🍏🍒🍍" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L729](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L729) — Stdlib.String.dropLast "🍏a🍒b🍍c" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L730](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L730) — Stdlib.String.dropLast "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L731](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L731) — Stdlib.String.dropLast "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L732](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L732) — Stdlib.String.dropLast "👱👱🏻👱🏼👱🏽👱🏾👱🏿" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L733](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L733) — Stdlib.String.dropLast "🧟‍♀️🧟‍♂️" 20 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L734](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L734) — Stdlib.String.dropLast "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L735](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L735) — Stdlib.String.dropLast "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L739](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L739) — Stdlib.String.last "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L740](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L740) — Stdlib.String.last "abcd" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L741](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L741) — Stdlib.String.last "abcd" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L742](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L742) — Stdlib.String.last "" 7 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L743](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L743) — Stdlib.String.last "abcd" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L744](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L744) — Stdlib.String.last "abcd" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L745](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L745) — Stdlib.String.last "abcd" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L746](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L746) — Stdlib.String.last "🍍🍍🍏" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L747](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L747) — Stdlib.String.last "🍊🍍🍏" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L748](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L748) — Stdlib.String.last "🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿🧑🏻‍🍼" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L749](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L749) — Stdlib.String.last "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L750](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L750) — Stdlib.String.last "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L751](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L751) — Stdlib.String.last "👱👱🏻👱🏼👱🏽👱🏾👱🏿" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L752](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L752) — Stdlib.String.last "🧟‍♀️🧟‍♂️" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L753](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L753) — Stdlib.String.last "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L757](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L757) — Stdlib.String.contains "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "2223" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L758](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L758) — Stdlib.String.contains "👱👱🏻👱🏼👱🏽👱🏾" "👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L759](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L759) — Stdlib.String.contains "🧟‍♀️🧟‍♂️" "🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L760](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L760) — Stdlib.String.contains "🧟‍♀️🧟‍♂️" "🧟‍♂️🧟‍♂️" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L761](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L761) — Stdlib.String.contains "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️🇵🇷" "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L762](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L762) — Stdlib.String.contains "اختبار" "اختبار" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L763](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L763) — Stdlib.String.contains "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L764](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L764) — Stdlib.String.contains "a" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L765](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L765) — Stdlib.String.contains "" "a" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L769](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L769) — Stdlib.String.slice "abcd" -2 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L770](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L770) — Stdlib.String.slice "abcd" -5 -6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L771](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L771) — Stdlib.String.slice "abcd" -5 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L772](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L772) — Stdlib.String.slice "abcd" 0 -1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L773](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L773) — Stdlib.String.slice "abcd" 2 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L774](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L774) — Stdlib.String.slice "abcd" 2 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L775](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L775) — Stdlib.String.slice "abcd" 3 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L776](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L776) — Stdlib.String.slice "abcd" 5 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L777](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L777) — Stdlib.String.slice "🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" 2 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L778](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L778) — Stdlib.String.slice "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 1 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L779](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L779) — Stdlib.String.slice "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" 2 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L780](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L780) — Stdlib.String.slice "👱👱🏻👱🏼👱🏽👱🏾👱🏿" 2 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L781](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L781) — Stdlib.String.slice "🧟‍♀️🧟‍♂️" 2 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L782](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L782) — Stdlib.String.slice "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" 2 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L783](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L783) — Stdlib.String.slice "abc" 0 4503599627370498 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L787](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L787) — Stdlib.String.first "abcd" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L788](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L788) — Stdlib.String.first "abcd" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L789](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L789) — Stdlib.String.first "abcd" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L790](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L790) — Stdlib.String.first "abcd" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L791](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L791) — Stdlib.String.first "abcd" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L792](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L792) — Stdlib.String.first "abcd" 3000000000000000 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L793](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L793) — Stdlib.String.first "" 7 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L794](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L794) — Stdlib.String.first "🍊🍍🍏" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L795](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L795) — Stdlib.String.first "🍊🍍🍏" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L796](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L796) — Stdlib.String.first "🍊🍍🍏" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L797](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L797) — Stdlib.String.first "🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L798](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L798) — Stdlib.String.first "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L799](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L799) — Stdlib.String.first "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L800](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L800) — Stdlib.String.first "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L801](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L801) — Stdlib.String.first "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L802](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L802) — Stdlib.String.first "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L803](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L803) — Stdlib.String.first "👱👱🏻👱🏼👱🏽👱🏾👱🏿" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L804](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L804) — Stdlib.String.first "🧟‍♀️🧟‍♂️" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L805](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L805) — Stdlib.String.first "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L809](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L809) — Stdlib.String.padStart "123" "0" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L810](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L810) — Stdlib.String.padStart "123" "0" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L811](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L811) — Stdlib.String.padStart "123" "0" 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L812](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L812) — Stdlib.String.padStart "" "0" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L813](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L813) — Stdlib.String.padStart "123🍊🍊" "0" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L814](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L814) — Stdlib.String.padStart "🍍🍍🍊🍊" "0" 7 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L815](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L815) — Stdlib.String.padStart "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L816](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L816) — Stdlib.String.padStart "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L817](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L817) — Stdlib.String.padStart "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "0" 20 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L818](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L818) — Stdlib.String.padStart "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L819](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L819) — Stdlib.String.padStart "🧟‍♀️🧟‍♂️" "0" 5 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L820](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L820) — Stdlib.String.padStart "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️🇵🇷" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L821](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L821) — Stdlib.String.padStart "鷝" "觌഻" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L823](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L823) — Stdlib.String.padStart "123" "_-" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L824](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L824) — Stdlib.String.padStart "123" "" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L828](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L828) — Stdlib.String.padEnd "" "0" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L829](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L829) — Stdlib.String.padEnd "123" "0" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L830](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L830) — Stdlib.String.padEnd "123" "0" 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L831](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L831) — Stdlib.String.padEnd "123" "0" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L832](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L832) — Stdlib.String.padEnd "123🍊🍊" "0" 8 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L833](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L833) — Stdlib.String.padEnd "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L834](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L834) — Stdlib.String.padEnd "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L835](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L835) — Stdlib.String.padEnd "﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽﷽" "0" 20 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L836](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L836) — Stdlib.String.padEnd "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L837](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L837) — Stdlib.String.padEnd "🧟‍♀️🧟‍♂️" "0" 5 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L838](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L838) — Stdlib.String.padEnd "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️🇵🇷" "0" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L839](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L839) — Stdlib.String.padEnd "鷝" "觌഻" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L841](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L841) — Stdlib.String.padEnd "123" "_-" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L842](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L842) — Stdlib.String.padEnd "123" "" 10 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L848](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L848) — Stdlib.String.padEndToWidth "" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L849](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L849) — Stdlib.String.padEndToWidth "abc" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L850](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L850) — Stdlib.String.padEndToWidth "abc" 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L851](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L851) — Stdlib.String.padEndToWidth "abc" -3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L852](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L852) — Stdlib.String.padEndToWidth "abcdef" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L853](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L853) — Stdlib.String.padEndToWidth "界" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L854](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L854) — Stdlib.String.padEndToWidth "界界界" 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L855](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L855) — Stdlib.String.padEndToWidth "🙂" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L856](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L856) — Stdlib.String.padEndToWidth "e\u0301" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L858](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L858) — Stdlib.String.padStartToWidth "" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L859](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L859) — Stdlib.String.padStartToWidth "abc" 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L860](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L860) — Stdlib.String.padStartToWidth "abc" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L861](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L861) — Stdlib.String.padStartToWidth "abcdef" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L862](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L862) — Stdlib.String.padStartToWidth "界" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L863](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L863) — Stdlib.String.padStartToWidth "界界界" 6 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L867](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L867) — Stdlib.String.indexOf "hello world" "world" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L868](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L868) — Stdlib.String.indexOf "hello world" "earth" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L869](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L869) — Stdlib.String.indexOf "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L870](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L870) — Stdlib.String.indexOf "hello" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L871](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L871) — Stdlib.String.indexOf "" "hello" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L872](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L872) — Stdlib.String.indexOf "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏼👱🏽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L873](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L873) — Stdlib.String.indexOf "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏼👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L874](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L874) — Stdlib.String.indexOf "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "👩‍👩‍👧‍👦" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L875](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L875) — Stdlib.String.indexOf "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "🧑🏽‍🦰" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L876](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L876) — Stdlib.String.indexOf "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "👱🏽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L877](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L877) — Stdlib.String.indexOf "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "🧑🏻‍🍼" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L883](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L883) — Stdlib.String.indexOfEgc "hello world" "world" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L884](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L884) — Stdlib.String.indexOfEgc "hello world" "earth" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L885](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L885) — Stdlib.String.indexOfEgc "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L886](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L886) — Stdlib.String.indexOfEgc "hello" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L887](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L887) — Stdlib.String.indexOfEgc "" "hello" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L890](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L890) — Stdlib.String.indexOfEgc "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏼👱🏽" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L891](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L891) — Stdlib.String.indexOfEgc "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏼👱🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L892](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L892) — Stdlib.String.indexOfEgc "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷" "👩‍👩‍👧‍👦" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L893](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L893) — Stdlib.String.indexOfEgc "żółw🧑🏽‍🦰🧑🏻‍🍼✋✋🏻✋🏿" "🧑🏽‍🦰" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L897](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L897) — Stdlib.String.indexOfEgc "🧑🏼‍💻" "🧑" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L899](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L899) — Stdlib.String.indexOfEgc "🧑🧑🏼‍💻" "🧑" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L904](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L904) — Stdlib.String.lastIndexOfEgc "hello world hello" "hello" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L905](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L905) — Stdlib.String.lastIndexOfEgc "hello world" "earth" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L906](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L906) — Stdlib.String.lastIndexOfEgc "" "hello" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L909](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L909) — Stdlib.String.lastIndexOfEgc "" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L910](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L910) — Stdlib.String.lastIndexOfEgc "hello" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L912](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L912) — Stdlib.String.lastIndexOfEgc "👱👱🏻" "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L915](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L915) — Stdlib.String.lastIndexOfEgc "👱👱🏻👱🏼👱🏽👱🏾👱🏿" "👱🏼" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L916](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L916) — Stdlib.String.lastIndexOfEgc "a🧑🏼‍💻b🧑🏼‍💻c" "🧑🏼‍💻" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L917](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L917) — Stdlib.String.lastIndexOfEgc "żółw🧑🏽‍🦰🧑🏻‍🍼" "🧑🏻‍🍼" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L921](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L921) — Stdlib.String.lastIndexOfEgc "🧑🏼‍💻🧑🏼‍💻" "🧑" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L923](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L923) — Stdlib.String.lastIndexOfEgc "🧑🏼‍💻🧑" "🧑" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L927](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L927) — Stdlib.String.ellipsis "hello world" 5 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L928](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L928) — Stdlib.String.ellipsis "hello world" 9 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L929](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L929) — Stdlib.String.ellipsis "hello world" 11 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L930](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L930) — Stdlib.String.ellipsis "hello world" 12 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L931](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L931) — Stdlib.String.ellipsis "👱👱🏻👱🏼👱🏽👱🏾👱🏿" 5 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L932](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L932) — Stdlib.String.ellipsis "Z̤͔ͧ̑̓ä͖̭̈̇lͮ̒ͫǧ̗͚̚o̙̔ͮ̇͐̇" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L933](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L933) — Stdlib.String.ellipsis "👩‍👩‍👧‍👦" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L935](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L935) — Stdlib.String.ellipsis "👨‍❤️‍💋‍👨👩‍👩‍👧‍👦🏳️‍⚧️‍️🇵🇷✋✋🏻✋🏿" 4 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L938](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L938) — Stdlib.String.head "hello world" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L940](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L940) — Stdlib.String.head "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L949](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L949) — Stdlib.String.articleFor "apple" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L950](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L950) — Stdlib.String.articleFor "banana" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L951](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L951) — Stdlib.String.articleFor "🍍" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L952](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L952) — Stdlib.String.articleFor "🍊" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L953](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L953) — Stdlib.String.articleFor "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L956](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L956) — Stdlib.String.repeat "ab" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L957](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L957) — Stdlib.String.repeat "x" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L958](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L958) — Stdlib.String.repeat "" 5 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L959](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L959) — Stdlib.String.repeat "ab" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L960](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L960) — Stdlib.String.repeat "ab" (0 - 1) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L962](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L962) — Stdlib.String.repeat "é" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L963](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L963) — Stdlib.String.repeat "👨‍👩‍👧‍👦" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L964](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L964) — Stdlib.String.length (Stdlib.String.repeat "─" 80) | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L968](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L968) — Stdlib.String.toCodepoints "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L969](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L969) — Stdlib.String.toCodepoints "abc" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L970](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L970) — Stdlib.String.toCodepoints "héllo" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L972](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L972) — Stdlib.String.toCodepoints "👩‍👧" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L976](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L976) — Stdlib.String.codepointLength "" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L977](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L977) — Stdlib.String.codepointLength "abcdef" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L979](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L979) — Stdlib.String.codepointLength "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L980](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L980) — Stdlib.String.length "😄" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L982](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L982) — Stdlib.String.codepointLength "👩‍👧" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L983](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L983) — Stdlib.String.length "👩‍👧" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L984](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L984) — Stdlib.String.codepointLength "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L985](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L985) — Stdlib.String.length "🧑🏽‍🦰🧑🏼‍💻🧑🏻‍🍼✋✋🏻✋🏿" | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L989](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L989) — Stdlib.String.getByteAt "abc" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L990](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L990) — Stdlib.String.getByteAt "abc" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L991](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L991) — Stdlib.String.getByteAt "abc" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L993](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L993) — Stdlib.String.getByteAt "é" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L994](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L994) — Stdlib.String.getByteAt "é" 1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L995](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L995) — Stdlib.String.getByteAt "é" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L996](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L996) — Stdlib.String.getByteAt "abc" 3 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L997](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L997) — Stdlib.String.getByteAt "" 0 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L998](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L998) — Stdlib.String.getByteAt "abc" -1 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L1000](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L1000) — Stdlib.String.getByteAt "abc" 4503599627370498 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
| [L1002](../../../../test/fixtures/e2e/upstream/stdlib/string.dark#L1002) — Stdlib.String.getByteAt "héllo" 2 | Preamble type error: c: Unknown function or value 'Builtin.testToChar' |
