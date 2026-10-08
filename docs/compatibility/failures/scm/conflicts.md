# scm/conflicts.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark) · [File list](../../current-audit.md)

Executed 40 assertions: **3 passed, 37 failed**.
Of 40 previously disabled assertions, **3 passed and 37 failed**.

| Test | Observed failure |
| --- | --- |
| [L51](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L51) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "bbb" "2... | Unknown function or value 'Helpers.detectCount' |
| [L58](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L58) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "aaa" "2... | Unknown function or value 'Helpers.detectCount' |
| [L64](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L64) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "ooo" "2... | Unknown function or value 'Helpers.detectCount' |
| [L70](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L70) — Helpers.detectCount [ Helpers.binding "f" "ooo" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "bbb" "2... | Unknown function or value 'Helpers.detectCount' |
| [L79](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L79) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "bbb" "2... | Unknown function or value 'Helpers.detectCount' |
| [L86](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L86) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ Helpers.editOf "f" "bbb" "20... | Unknown function or value 'Helpers.detectCount' |
| [L92](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L92) — Helpers.detectCount [ Helpers.editOf "f" "aaa" "2026-01-02T00:00:00.000Z" "ooo" ] [ Helpers.binding "f" "bb... | Unknown function or value 'Helpers.detectCount' |
| [L99](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L99) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "bbb" "2... | Unknown function or value 'Helpers.detectCount' |
| [L105](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L105) — Helpers.detectCount [] [ Helpers.binding "f" "bbb" "2026-01-03T00:00:00.000Z" ] [ Helpers.base' "f" "ooo" ] | Unknown function or value 'Helpers.detectCount' |
| [L111](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L111) — Helpers.detectCount [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" Helpers.binding "g" "ccc" "2026-... | Unknown function or value 'Helpers.detectCount' |
| [L129](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L129) — Helpers.detectCount [ Helpers.editOf "f" "mine" "2026-01-02T00:00:00.000Z" "parent" ] [ Helpers.editOf "f" ... | Unknown function or value 'Helpers.detectCount' |
| [L136](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L136) — Helpers.detectCount [ Helpers.editOf "f" "mine" "2026-01-02T00:00:00.000Z" "parent" ] [ Helpers.editOf "f" ... | Unknown function or value 'Helpers.detectCount' |
| [L144](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L144) — Helpers.detectCount [ Helpers.editOf "f" "mine" "2026-01-02T00:00:00.000Z" "grandparent" ] [ Helpers.editOf... | Unknown function or value 'Helpers.detectCount' |
| [L155](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L155) — Helpers.detectCount [ Helpers.editOf "f" "mine" "2026-01-02T00:00:00.000Z" "grandparent" ] [ Helpers.editOf... | Unknown function or value 'Helpers.detectCount' |
| [L161](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L161) — Helpers.detectCount [ Helpers.editOf "f" "same" "2026-01-02T00:00:00.000Z" "parent" ] [ Helpers.editOf "f" ... | Unknown function or value 'Helpers.detectCount' |
| [L169](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L169) — Helpers.detectCount [ Helpers.binding "f" "mine" "2026-01-02T00:00:00.000Z" ] [ Helpers.binding "f" "theirs... | Unknown function or value 'Helpers.detectCount' |
| [L175](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L175) — Helpers.detectCount [ Helpers.binding "f" "mine" "2026-01-02T00:00:00.000Z" ] [ Helpers.editOf "f" "theirs"... | Unknown function or value 'Helpers.detectCount' |
| [L184](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L184) — (Darklang.SCM.Conflicts.detect "yours" "theirs" [ Helpers.binding "f" "aaa" "2026-01-02T00:00:00.000Z" ] [ ... | Unknown function or value 'Darklang.SCM.Conflicts.detect' |
| [L194](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L194) — (Darklang.SCM.Conflicts.detect "yours" "theirs" [ Helpers.binding "f" "aaa" "2026-01-04T00:00:00.000Z" ] [ ... | Unknown function or value 'Darklang.SCM.Conflicts.detect' |
| [L204](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L204) — (Darklang.SCM.Conflicts.detect "yours" "theirs" [ Helpers.binding "f" "aaa" "2026-01-03T00:00:00.000Z" ] [ ... | Unknown function or value 'Darklang.SCM.Conflicts.detect' |
| [L220](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L220) — Darklang.SCM.Conflicts.conflictId "Zz" "W" "f" [ "aaa", "bbb" ] | Unknown function or value 'Darklang.SCM.Conflicts.conflictId' |
| [L224](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L224) — (Darklang.SCM.Conflicts.conflictId "Zz" "W" "f" [ "aaa", "bbb" ]) == (Darklang.SCM.Conflicts.conflictId "Zz... | Unknown function or value 'Darklang.SCM.Conflicts.conflictId' |
| [L228](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L228) — (Stdlib.String.length (Darklang.SCM.Conflicts.conflictId "Zz" "W" "f" [ "aaa", "bbb" ])) | Unknown function or value 'Darklang.SCM.Conflicts.conflictId' |
| [L232](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L232) — Darklang.SCM.Conflicts.stringGt "b" "a" | Unknown function or value 'Darklang.SCM.Conflicts.stringGt' |
| [L233](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L233) — Darklang.SCM.Conflicts.stringGt "a" "b" | Unknown function or value 'Darklang.SCM.Conflicts.stringGt' |
| [L234](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L234) — Darklang.SCM.Conflicts.stringGt "a" "a" | Unknown function or value 'Darklang.SCM.Conflicts.stringGt' |
| [L236](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L236) — Darklang.SCM.Conflicts.stringGt "2026-01-03T00:00:00.000Z" "2026-01-02T23:59:59.999Z" | Unknown function or value 'Darklang.SCM.Conflicts.stringGt' |
| [L242](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L242) — (Stdlib.List.length (Darklang.SCM.Conflicts.inChunks [])) | Unknown function or value 'Darklang.SCM.Conflicts.inChunks' |
| [L243](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L243) — (Stdlib.List.length (Darklang.SCM.Conflicts.inChunks [ "a", "b", "c" ])) | Unknown function or value 'Darklang.SCM.Conflicts.inChunks' |
| [L246](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L246) — Stdlib.List.length (Stdlib.List.flatten (Darklang.SCM.Conflicts.inChunks (Stdlib.List.map (Stdlib.List.rang... | Unknown function or value 'Darklang.SCM.Conflicts.inChunks' |
| [L251](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L251) — (Stdlib.List.length (Darklang.SCM.Conflicts.inChunks (Stdlib.List.map (Stdlib.List.range 1 2500) (fun i -> ... | Unknown function or value 'Darklang.SCM.Conflicts.inChunks' |
| [L257](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L257) — Darklang.SCM.Conflicts.inPlaceholders [] | Unknown function or value 'Darklang.SCM.Conflicts.inPlaceholders' |
| [L258](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L258) — Darklang.SCM.Conflicts.inPlaceholders [ "x" ] | Unknown function or value 'Darklang.SCM.Conflicts.inPlaceholders' |
| [L259](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L259) — Darklang.SCM.Conflicts.inPlaceholders [ "x", "y", "z" ] | Unknown function or value 'Darklang.SCM.Conflicts.inPlaceholders' |
| [L263](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L263) — Darklang.SCM.Conflicts.fqn "Zz" "W" "f" | Unknown function or value 'Darklang.SCM.Conflicts.fqn' |
| [L265](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L265) — Darklang.SCM.Conflicts.fqn "Zz" "" "f" | Unknown function or value 'Darklang.SCM.Conflicts.fqn' |
| [L266](../../../../test/fixtures/e2e/upstream/scm/conflicts.dark#L266) — Darklang.SCM.Conflicts.bindingKey "Zz" "W" "f" | Unknown function or value 'Darklang.SCM.Conflicts.bindingKey' |
