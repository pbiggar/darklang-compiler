# scm/sync-wire.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark) · [File list](../../current-audit.md)

Executed 19 assertions: **0 passed, 19 failed**.

| Test | Observed failure |
| --- | --- |
| [L31](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L31) — Darklang.SCM.Wire.currentWireVersion | Unknown function or value 'Darklang.SCM.Wire.currentWireVersion' |
| [L36](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L36) — Helpers.decode (Darklang.SCM.Wire.wireEncode []) \|> Stdlib.Result.map (fun b -> b.formatVersion) | Unknown function or value 'Helpers.decode' |
| [L41](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L41) — (Helpers.decode (Darklang.SCM.Wire.wireEncode []) \|> Stdlib.Result.map (fun b -> b.kernelHash == "")) \|> St... | Unknown function or value 'Helpers.decode' |
| [L46](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L46) — Helpers.decode (Darklang.SCM.Wire.wireEncodeAt 4200L []) \|> Stdlib.Result.map (fun b -> b.cursor) | Unknown function or value 'Helpers.decode' |
| [L52](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L52) — (Helpers.isError (Helpers.decode (Helpers.bundleAtVersion "99"))) | Unknown function or value 'Helpers.isError' |
| [L54](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L54) — (Stdlib.String.contains (Helpers.errorText (Helpers.decode (Helpers.bundleAtVersion "99"))) "v99") | Unknown function or value 'Helpers.errorText' |
| [L56](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L56) — Stdlib.String.contains (Helpers.errorText (Helpers.decode (Helpers.bundleAtVersion "99"))) "not understood ... | Unknown function or value 'Helpers.errorText' |
| [L62](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L62) — (Helpers.isError (Helpers.decode (Helpers.bundleAtVersion "0"))) | Unknown function or value 'Helpers.isError' |
| [L67](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L67) — (Helpers.isError (Helpers.decode (Helpers.bundleAtVersion "2"))) | Unknown function or value 'Helpers.isError' |
| [L72](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L72) — (Helpers.isError (Helpers.decode "this is not a bundle")) | Unknown function or value 'Helpers.isError' |
| [L76](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L76) — (Helpers.isError (Helpers.decode """{"hello":"world"}""")) | Unknown function or value 'Helpers.isError' |
| [L78](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L78) — Stdlib.String.contains (Helpers.errorText (Helpers.decode """{"hello":"world"}""")) "unparseable" | Unknown function or value 'Helpers.errorText' |
| [L83](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L83) — (Helpers.isError (Helpers.decode "")) | Unknown function or value 'Helpers.isError' |
| [L88](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L88) — Darklang.SCM.Wire.headDecode (Stdlib.String.toBlob (Darklang.SCM.Wire.headEncode (Darklang.SCM.Wire.SyncHea... | Unknown function or value 'Darklang.SCM.Wire.headDecode' |
| [L92](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L92) — Darklang.SCM.Wire.headDecode (Stdlib.String.toBlob "not json") | Unknown function or value 'Darklang.SCM.Wire.headDecode' |
| [L97](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L97) — Darklang.SCM.Wire.headStamp (Darklang.SCM.Wire.SyncHead { count = 7L; maxTs = "2026-01-02T00:00:00.000Z" }) | Unknown function or value 'Darklang.SCM.Wire.headStamp' |
| [L129](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L129) — BothWritersAgree.sameShape (BothWritersAgree.native ()) | Unknown function or value 'BothWritersAgree.sameShape' |
| [L134](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L134) — match BothWritersAgree.native () with \| Ok _ -> true \| Error _ -> false | Unknown function or value 'BothWritersAgree.native' |
| [L232](../../../../test/fixtures/e2e/upstream/scm/sync-wire.dark#L232) — (BothWritersAgreeOnOps.compare ()) | Unknown function or value 'BothWritersAgreeOnOps.compare' |
