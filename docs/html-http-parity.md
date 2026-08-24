# Html and Http Parity

This document records the compatibility slice implemented for the canonical
Html and Http value modules. The comparison used compiler HEAD
`f84551b75175a2a85b4a43c8cd5dadbf6d758557` and `darklang/dark` revision
`04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. DCB1 report commit `8a402797`
and the existing parity documents were starting evidence only; every entry
below was rechecked against those exact source revisions and focused executable
probes.

The interpreter baseline is
`packages/darklang/stdlib/html.dark:47-455` and
`packages/darklang/stdlib/http.dark:4-259`; executable behavior is pinned by
`backend/testfiles/execution/stdlib/html.dark` and `http.dark` at the same
revision. The compiler implementation is in
`src/DarkCompiler/stdlib/Html.dark`, `Http.dark`, and `HttpRequest.dark`, loaded
after Blob by `src/DarkCompiler/CompilerLibrary.fs:1151-1154`.

## Compatibility matrix

| Area | Pinned interpreter behavior | Compiler status | Classification |
|---|---|---|---|
| Html structural types | `Attribute` and `Attributes` are identical list aliases; `HtmlTag` is a name/attributes/children record; `Node` is the recursive `String | HtmlTag` sum (`html.dark:47-64`) | Same public types and recursive value representation (`Html.dark:5-14`) | Parity |
| Html serialization | Fixed `&`, `<`, `>`, `"`, `'` escape order; raw String nodes, comments and attribute values; ordered and boolean attributes; case-sensitive void detection; ignored void children; explicit non-void closing tags; exact `<!DOCTYPE html>` prefix (`html.dark:67-212,438-455`) | Ported as pure Dark code (`Html.dark:16-97,175-179`) | Parity |
| Html constructors | `br` and all declared document, text, grouping, table, form, media, semantic, and metadata constructors retain their childless/child-taking arities (`html.dark:215-435`) | Complete family (`Html.dark:101-173`) | Parity |
| `Html.s` | No constructor exists in the pinned source | Added with the normal attributes-and-children shape (`Html.dark:158`) | Compiler-only extension |
| Blob bridge | Http bodies use `Blob`; `String.toBlob` and the bare `Blob.empty` value supply UTF-8 and empty bodies (`http.dark:4-7,91-208`) | `Blob` remains the existing Bytes-layout alias; `String.toBlob` delegates to Blob and bare module values are materialized by AOT lowering | Parity dependency; no duplicate runtime layout |
| Query parser | Last duplicate wins; empty segments ignored; bare keys get empty values; extra `=` and `?` are preserved; no percent/plus decoding (`http.dark:10-32`) | Direct immutable Dict accumulation preserves those results (`Http.dark:7-37`) | Parity |
| Header parser | CRLF normalized; blank/malformed lines omitted; names/values trimmed; extra colons preserved; original name casing retained; last duplicate wins (`http.dark:35-54`) | Direct immutable Dict accumulation preserves those results (`Http.dark:39-66`) | Parity |
| Request accessors | Parameters split only for exactly one `=`; malformed parameters become whole keys with empty values; order and duplicates remain; duplicate lookup values join with commas. An absent or empty query produces one empty pair under frozen `String.split` behavior (`http.dark:57-88`) | Separate `Stdlib.Http.Request` module ports the exact behavior (`HttpRequest.dark:5-34`) | Parity, intentionally distinct from `parseQueryString` |
| Response helpers | Exact body/status/header argument order, status codes, spelling and order; HTML/text UTF-8 content types; JSON without charset; arbitrary redirect strings; empty 401/403/404 bodies (`http.dark:91-208`) | All helpers ported using Blob values (`Http.dark:68-121`) | Parity |
| Cookie boundary | `Cookie` is a record; the only `cookie` implementation is commented out and dependency-incomplete (`http.dark:210-259`). The execution fixture's `setCookie` probes are also commented out | Public structural `Cookie` only; no `cookie` or `setCookie` API. Caller-provided ordered and duplicate `Set-Cookie` pairs pass unchanged through `responseWithHeaders` | Non-gap at pinned revision |
| JSON boundary | `responseWithJson` accepts a serialized `String` (`http.dark:153-160`) | Same signature; no generic JSON serializer was introduced | Non-gap at pinned revision |

There are no intentional Html or Http behavior divergences in the active
pinned surface. The only public compiler extension is `Html.s`; void-tag
detection is internal to Html serialization.

## Executable coverage and AOT boundaries

`src/Tests/e2e/html_http.e2e` covers compiler syntax, public records and sums,
qualified aliases, every constructor, rendering boundaries, parser quirks,
request accessors, Blob bodies, every response helper, ordered duplicate
`Set-Cookie` headers, and Cookie construction. The exact pinned upstream Html
and Http execution fixtures are included in normal curated discovery without
enabling other upstream suites.

Interpreter-style multiline list layout and qualified bare type aliases needed
small parser compatibility support. Recursive Html nodes also required the
existing reference-count machinery to release dynamic-buffer and recursive-list
payloads on both native backends. These are representation and syntax support,
not new Html or Http behavior.

Blob equality remains the interpreter's public handle identity. The compiler
E2E harness normally evaluates expected expressions as Dark equality; therefore
the imported Http fixture compares response fields and decoded body bytes
instead of constructing a second fresh Blob for expected values. This adapts
the test oracle without changing Blob or Http semantics: the pinned interpreter
creates a new ephemeral Blob identity for each `String.toBlob` call
(`backend/src/Builtins/Builtins.Pure/Libs/String.fs:402-411`), represents that
identity in `backend/src/LibExecution/RuntimeTypes.fs:678-700`, and compares Blob
identities in `backend/src/Builtins/Builtins.Pure/Libs/NoModule.fs:119-130`.
