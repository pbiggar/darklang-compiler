# scm/matter-routes.dark

[Source fixture](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark) · [File list](../../current-audit.md)

Executed 35 assertions: **0 passed, 35 failed**.
Of 35 previously disabled assertions, **0 passed and 35 failed**.

| Test | Observed failure |
| --- | --- |
| [L30](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L30) — (Helpers.status "/ping" Darklang.Matter.Relay.pingHandlerFn) | Unknown function or value 'Helpers.status' |
| [L31](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L31) — (Helpers.body "/ping" Darklang.Matter.Relay.pingHandlerFn) | Unknown function or value 'Helpers.body' |
| [L35](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L35) — (Helpers.status "/" Darklang.Matter.landingHandlerFn) | Unknown function or value 'Helpers.status' |
| [L38](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L38) — (Helpers.bodyHas "/" Darklang.Matter.landingHandlerFn "/sync/pull") | Unknown function or value 'Helpers.bodyHas' |
| [L42](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L42) — (Helpers.status "/m?name=Darklang.Stdlib.List" Darklang.Matter.moduleHandlerFn) | Unknown function or value 'Helpers.status' |
| [L46](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L46) — (Helpers.bodyHas "/m?name=Darklang.Stdlib.List" Darklang.Matter.moduleHandlerFn "List.map") | Unknown function or value 'Helpers.bodyHas' |
| [L51](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L51) — (Helpers.status "/m?name=Nope.Not.Here" Darklang.Matter.moduleHandlerFn) | Unknown function or value 'Helpers.status' |
| [L57](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L57) — (Helpers.status "/p?name=Darklang.Stdlib.List.map" Darklang.Matter.itemHandlerFn) | Unknown function or value 'Helpers.status' |
| [L62](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L62) — Helpers.bodyHas "/p?name=Darklang.Stdlib.List.map" Darklang.Matter.itemHandlerFn "<span class=\"f\">map</sp... | Unknown function or value 'Helpers.bodyHas' |
| [L68](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L68) — (Helpers.status "/p?name=Darklang.Stdlib.Option.Option" Darklang.Matter.itemHandlerFn) | Unknown function or value 'Helpers.status' |
| [L71](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L71) — (Helpers.status "/p?name=Nope.Not.Here" Darklang.Matter.itemHandlerFn) | Unknown function or value 'Helpers.status' |
| [L78](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L78) — (Helpers.status "/sync/push" Darklang.Matter.Relay.pushHandlerFn) | Unknown function or value 'Helpers.status' |
| [L80](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L80) — (Helpers.bodyHas "/sync/push" Darklang.Matter.Relay.pushHandlerFn "accepts no writes") | Unknown function or value 'Helpers.bodyHas' |
| [L83](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L83) — (Helpers.status "/branch/push" Darklang.Matter.Relay.branchPushHandlerFn) | Unknown function or value 'Helpers.status' |
| [L88](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L88) — (Helpers.status "/branch/list?owner=someone" Darklang.Matter.Relay.branchListHandlerFn) | Unknown function or value 'Helpers.status' |
| [L89](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L89) — (Helpers.status "/branch/pull?owner=someone&branch=x" Darklang.Matter.Relay.branchPullHandlerFn) | Unknown function or value 'Helpers.status' |
| [L95](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L95) — Helpers.bodyHas "/branch/list?owner=someone" Darklang.Matter.Relay.branchListHandlerFn "accepts no writes" | Unknown function or value 'Helpers.bodyHas' |
| [L101](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L101) — Helpers.bodyHas "/branch/list?owner=someone" Darklang.Matter.Relay.branchListHandlerFn "someone" | Unknown function or value 'Helpers.bodyHas' |
| [L108](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L108) — (Helpers.status "/search?q=map" Darklang.Matter.searchHandlerFn) | Unknown function or value 'Helpers.status' |
| [L110](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L110) — (Helpers.bodyHas "/search?q=map" Darklang.Matter.searchHandlerFn "List.map") | Unknown function or value 'Helpers.bodyHas' |
| [L113](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L113) — (Helpers.status "/search?q=" Darklang.Matter.searchHandlerFn) | Unknown function or value 'Helpers.status' |
| [L116](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L116) — (Helpers.status "/search?q=zzzznotathing" Darklang.Matter.searchHandlerFn) | Unknown function or value 'Helpers.status' |
| [L120](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L120) — Helpers.bodyHas "/search?q=<script>alert(1)</script>" Darklang.Matter.searchHandlerFn "<script>alert(1)</sc... | Unknown function or value 'Helpers.bodyHas' |
| [L125](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L125) — Helpers.bodyHas "/search?q=<script>alert(1)</script>" Darklang.Matter.searchHandlerFn "&lt;script&gt;" | Unknown function or value 'Helpers.bodyHas' |
| [L132](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L132) — (Helpers.status "/owners" Darklang.Matter.ownersHandlerFn) | Unknown function or value 'Helpers.status' |
| [L133](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L133) — (Helpers.bodyHas "/owners" Darklang.Matter.ownersHandlerFn "Darklang") | Unknown function or value 'Helpers.bodyHas' |
| [L137](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L137) — (Helpers.status "/stats" Darklang.Matter.statsHandlerFn) | Unknown function or value 'Helpers.status' |
| [L140](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L140) — (Helpers.bodyHas "/stats" Darklang.Matter.statsHandlerFn "ops") | Unknown function or value 'Helpers.bodyHas' |
| [L146](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L146) — (Helpers.bodyHas "/p?name=Darklang.Stdlib.List.map" Darklang.Matter.itemHandlerFn "Used by") | Unknown function or value 'Helpers.bodyHas' |
| [L149](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L149) — (Helpers.bodyHas "/p?name=Darklang.Stdlib.List.map" Darklang.Matter.itemHandlerFn "{{") | Unknown function or value 'Helpers.bodyHas' |
| [L152](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L152) — Helpers.bodyHas "/p?name=Darklang.Stdlib.List.map" Darklang.Matter.itemHandlerFn "/m?name=Darklang.Stdlib" | Unknown function or value 'Helpers.bodyHas' |
| [L159](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L159) — Helpers.bodyHas "/p?name=Darklang.Stdlib.List.map" Darklang.Matter.itemHandlerFn "<span class=\"k\">let</sp... | Unknown function or value 'Helpers.bodyHas' |
| [L166](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L166) — Stdlib.String.contains (Darklang.Matter.highlightHtml "let f (x: String) : String = \"<script>\"") "&lt;scr... | Unknown function or value 'Darklang.Matter.highlightHtml' |
| [L170](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L170) — Stdlib.String.contains (Darklang.Matter.highlightHtml "let f (x: String) : String = \"<script>\"") "<script>" | Unknown function or value 'Darklang.Matter.highlightHtml' |
| [L175](../../../../test/fixtures/e2e/upstream/scm/matter-routes.dark#L175) — (Darklang.Matter.highlightHtml "«««") | Unknown function or value 'Darklang.Matter.highlightHtml' |
