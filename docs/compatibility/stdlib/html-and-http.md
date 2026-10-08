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
`stdlib/Html.dark`, `Http.dark`, and `HttpRequest.dark`, loaded
after Blob by `src/driver/StdlibCompilation.ml`.

## Compatibility matrix

| Area | Pinned interpreter behavior | Compiler status | Classification |
|---|---|---|---|
| Html structural types | `Attribute` and `Attributes` are identical list aliases; `HtmlTag` is a name/attributes/children record; `Node` is the recursive `String | HtmlTag` sum (`html.dark:47-64`) | Same public types and recursive value representation (`Html.dark:5-14`) | Parity |
| Html serialization | Fixed `&`, `<`, `>`, `"`, `'` escape order; raw String nodes, comments and attribute values; ordered and boolean attributes; case-sensitive void detection; ignored void children; explicit non-void closing tags; exact `<!DOCTYPE html>` prefix (`html.dark:67-212,438-455`) | Ported as pure Dark code (`Html.dark:16-97,175-179`) | Parity |
| Html constructors | `br` and all declared document, text, grouping, table, form, media, semantic, and metadata constructors retain their childless/child-taking arities (`html.dark:215-435`) | Complete family (`Html.dark:101-173`) | Parity |
| Blob bridge | Http bodies use `Blob`; `String.toBlob` and the bare `Blob.empty` value supply UTF-8 and empty bodies (`http.dark:4-7,91-208`) | `Blob` uses the compiler's dynamic-buffer layout; `String.toBlob` delegates to Blob and bare module values are materialized by AOT lowering | Parity dependency; no duplicate runtime layout |
| Query parser | Last duplicate wins; empty segments ignored; bare keys get empty values; extra `=` and `?` are preserved; no percent/plus decoding (`http.dark:10-32`) | Direct immutable Dict accumulation preserves those results (`Http.dark:7-37`) | Parity |
| Header parser | CRLF normalized; blank/malformed lines omitted; names/values trimmed; extra colons preserved; original name casing retained; last duplicate wins (`http.dark:35-54`) | Direct immutable Dict accumulation preserves those results (`Http.dark:39-66`) | Parity |
| Request accessors | Parameters split only for exactly one `=`; malformed parameters become whole keys with empty values; order and duplicates remain; duplicate lookup values join with commas. An absent or empty query produces one empty pair under frozen `String.split` behavior (`http.dark:57-88`) | Separate `Stdlib.Http.Request` module ports the exact behavior (`HttpRequest.dark:5-34`) | Parity, intentionally distinct from `parseQueryString` |
| Response helpers | Exact body/status/header argument order, status codes, spelling and order; HTML/text UTF-8 content types; JSON without charset; arbitrary redirect strings; empty 401/403/404 bodies (`http.dark:91-208`) | All helpers ported using Blob values (`Http.dark:68-121`) | Parity |
| Cookie boundary | `Cookie` is a record; the only `cookie` implementation is commented out and dependency-incomplete (`http.dark:210-259`). The execution fixture's `setCookie` probes are also commented out | Public structural `Cookie` only; no `cookie` or `setCookie` API. Caller-provided ordered and duplicate `Set-Cookie` pairs pass unchanged through `responseWithHeaders` | Non-gap at pinned revision |
| JSON boundary | `responseWithJson` accepts a serialized `String` (`http.dark:153-160`) | Same signature; no generic JSON serializer was introduced | Non-gap at pinned revision |

There are no intentional Html or Http behavior divergences in the active
pinned surface; void-tag detection is internal to Html serialization.

## Executable coverage and AOT boundaries

`test/fixtures/e2e/html_http.e2e` covers compiler-supported Dark syntax, public records and sums,
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

## Additional pure HTTP surfaces

The later pure-surface audit is pinned to darklang/dark release `v0.0.35`,
revision `0b3888d8e4f30d48ecd738f5cbe5cc2b8d958460`. The compiler copies the
upstream `HttpClient` response and request-error types, `basicAuth`,
`bearerToken`, and the four `ContentType` header values. The compiler now
implements buffered `HttpClient.request` and its `get`, `post`, `put`, `options`,
`delete`, and `head` wrappers for HTTP and HTTPS in Dark,
using DNS over UDP and a checked resolved address for each native TCP
connection. `requestTrusted` explicitly allows local addresses. Each request
closes its connection; redirects and cookies are not automatic. `HttpClient.stream`
returns headers after parsing them and pulls HTTP or HTTPS body bytes on demand.
Closing or draining its body stream closes the connection. `post` and `put`
accept headers and a Blob body; `options`, `delete`, and `head` accept only a URL
and send empty headers and bodies, matching the upstream signatures. Focused
`test/fixtures/e2e/http_client_wrappers.e2e` cases cover the wrapper signatures,
invalid URLs, forwarded header errors, and guest private-address restrictions.

HTTPS requests offer `h2` and `http/1.1` using TLS ALPN. An authenticated `h2`
selection uses the Dark HTTP/2 implementation; absent ALPN and `http/1.1`
retain the existing HTTP/1.1 path. Buffered and lazy streaming responses share
frame validation, HPACK static/dynamic tables and Huffman decoding, SETTINGS,
PING, flow-control windows, fragmented headers, informational responses and
trailers. Duplicate response headers remain ordered. Each exchange owns one
stream and connection; pooling, multiplexing, server push, CONNECT tunnels,
and OPTIONS asterisk-form are not implemented in this profile.

`Stdlib.Http.Request.header` performs the upstream case-insensitive lookup.
`Stdlib.HttpServer.get` and `post` construct handler records and `getMethod`
reads that header with the upstream GET default. The remaining pure routing
helpers are ported too: route parsing and matching, handler selection, path and
path-parameter lookup, and `routeRequest`. The pinned upstream matcher requires
equal segment counts even for wildcards: `/files/*path` matches `/files/a`, but
does not match `/files/a/b`.

`Stdlib.HttpServer.Config.defaults port` supplies the upstream 30 MiB request
body cap and the three enabled policy flags. `HttpServer.serve config handler
onListening` now binds a native IPv4 listener, calls `onListening` after a
successful bind, and serves HTTP/1.1 sequentially until SIGINT or SIGTERM.
Request URLs retain their raw path and query bytes; actual request methods are
prepended as `x-http-method`. Forwarded HTTPS canonicalization, optional
standard Server/HSTS headers, ordered duplicate response headers, HEAD body
suppression, and stdout request logging are implemented in Dark. An oversized
declared or chunked body receives 413 before the missing body bytes are read.
`Expect: 100-continue` is acknowledged only after framing and body-limit checks.

This initial server is developed and verified on Linux ARM64. Linux native
lowering also exists for x86_64, without a cross-target verification claim.
macOS serving returns an explicit clock-unavailable error until the monotonic
clock boundary is implemented there. The listener owns and closes accepted
connections, and each response closes its connection. Reads and writes have
10-second monotonic deadlines; headers retain the existing wire parser's line,
count, and size caps. Configured body limits range from zero through 100 MiB,
and request wire buffering is capped at the body limit plus 1 MiB for framing.
Concurrency, IPv6 listeners, persistent connections, response compression,
interpreter telemetry integration, and server-side TLS remain follow-up work.

`test/fixtures/e2e/http_server.e2e` covers routing, configuration, framing limits,
and listener ownership. `python3 scripts/test_http_server_peer.py` exercises
the compiled server against local TCP clients, including fragmented and binary
bodies, 413/400/408/417/500 responses, HEAD, duplicate headers, bind failure,
reset clients, signal shutdown during a stalled request, rebinding, and
compiled leak accounting. Its artifacts remain in `TestResults/ai/`.

The IPv4 server also detects the HTTP/2 prior-knowledge connection preface,
including fragmented arrivals, and dispatches through the same Dark handlers.
It sends GOAWAY after one exchange. This profile is verified on Linux x86_64;
server-side TLS negotiation remains follow-up work. The HTTP/2 E2E fixtures
cover wire and field boundaries. `python3 scripts/test_http2_peer.py` uses the
test-only `h2==4.3.0` Python package as an independent peer and checks TLS client
negotiation, 70-KiB uploads/responses across flow-control windows, buffering,
streaming, trailers, HEAD, early stream close, early upload rejection (including
informational replies and retained HPACK state), truncation, cleartext server
dispatch, body limits, bounded draining after early replies, shutdown during
stalled HTTP/2 reads and compiled leak accounting.

HTTPS clients now select HTTP/3 from compatible HTTPS DNS advertisements;
HTTP/3 server integration remains unfinished. The pure
wire layer provides bounded QUIC variable-length integers and HTTP/3 frame
and SETTINGS parsing. QPACK supports the RFC 9204 static table, static-name
references, literal names/values and shared Huffman decoding, advertising zero
dynamic capacity and no blocked streams. `test/fixtures/e2e/http3_qpack.e2e`
checks the RFC example, malformed references, table boundaries and list limits;
`python3 scripts/test_qpack_peer.py` verifies both directions against test-only
`pylsqpack==0.3.23`, including duplicate fields and compiled leak accounting.
`test/fixtures/e2e/http3_wire.e2e` checks encoding bounds,
fragmentation, forbidden HTTP/2 frame/settings identifiers and duplicates.
The QUIC v1 AES-128 protection layer derives initial keys, reconstructs packet
numbers, masks headers, seals/opens packets and verifies Retry integrity tags.
RFC 9001 Appendix A vectors and `python3 scripts/test_quic_crypto_peer.py`
cover both directions, all packet-number lengths, wrapping, the 62-bit limit,
tampering and leak accounting. These are pure helpers, not an authenticated
QUIC connection: initial/Retry keys are public, and connection owners must
enforce unique packet numbers, replay filtering and key/AEAD usage limits.
Raw QUIC TLS handshake processing now authenticates the certificate chain,
hostname, CertificateVerify, Finished and `h3` ALPN before returning application
keys. Transport parameters are parsed with RFC defaults and numeric/length
bounds, duplicate detection, server-only restrictions and connection-ID binding
(including Retry presence and equality). Unknown parameters are ignored.
The profile caps the extension at 4096 bytes and 128 parameters.
`test/fixtures/e2e/quic_parameters.e2e` and
`python3 scripts/test_quic_parameters_peer.py` cover these checks against
independent test-only aioquic encodings. `python3 scripts/test_quic_tls_peer.py`
also tests live TLS flights that correctly sign wrong connection IDs, invalid
parameters or duplicates, and checks client Finished and peer handshake
completion. Application-frame codecs now cover STREAM, flow control, reset,
connection-ID, path-validation and close frames, with direction and range
validation. Bounded stream state validates reordered FINs, conflicting final
sizes, receive credit and resets. Packet recovery tracks fresh packet numbers,
ACK ranges, RTT, NewReno congestion, loss, PTO probes, retransmission identity
and pacing decisions. `quic_application.e2e`, `quic_recovery.e2e` and the
independent `scripts/test_quic_application_peer.py` and
`scripts/test_quic_recovery_peer.py` exercise these pure helpers, including
reordering and duplicate data. An owned client connection now joins these
helpers with source-address and connection-ID checks, cumulative gap-preserving
ACKs, a bounded replay window, handshake/stream retransmission, shared
cross-level congestion caps, receive-credit updates, stateless-reset detection
and authenticated application key updates. The initial receive windows are
64 KiB per stream and 256 KiB per connection. The connection profile retains
at most 16 stream owners and 128 issued peer-ID history entries; it does not
implement migration, session resumption or 0-RTT.
`python3 scripts/test_quic_client_peer.py` checks live authentication, lost
Initial/server flights, duplicates, tampering, wrong source addresses and Retry.
`python3 scripts/test_quic_keys_peer.py` independently checks key generations,
unchanged header-protection keys, reordered old/new packets, expiry and invalid
updates. `python3 scripts/test_quic_stream_peer.py` drives these production
streams through an aioquic HTTP/3 request, a 70-KiB response across the initial
stream-credit window, trailers, a live application key update and
request/response loss and reordering, with
compiled cleanup accounting. The bounded HTTP/3 message layer now consumes
fragmented frame headers, streams DATA, discards unknown frame payloads without
buffering them, and validates informational/final headers, trailers and
Content-Length at FIN. The control layer requires initial SETTINGS, checks
GOAWAY identifiers and identifies unique control/QPACK streams by stream type;
closing a critical stream fails the exchange. `http3_stream.e2e`,
`http3_message.e2e`, `http3_control.e2e` and `http3_uni.e2e` cover these rules,
and the live stream peer also feeds response/control bytes through this layer.
`HttpsService` parses bounded HTTPS RDATA into aliases or endpoints advertising
`h3`, including alternate ports and IPv4/IPv6 hints. It rejects malformed
parameter ordering, lengths, target compression and mandatory-key lists;
unknown mandatory parameters make an endpoint incompatible. Missing ALPN or
an advertisement limited to HTTP/1.1 or HTTP/2 does not select QUIC.
`https_service.e2e` covers these admission and malformed-record boundaries.
`HttpsDns` binds type-65 replies to their question and random transaction ID,
checks the UDP source endpoint, follows bounded aliases with a shared deadline,
and orders compatible service records by priority. It queries up to three
configured resolvers; truncation, malformed replies and discovery failure
fall back to ordinary TCP connection establishment. `https_dns.e2e` and
`python3 scripts/test_https_dns_peer.py` cover parsing, alias cycles, forged
source/transaction replies, timeouts and owned socket cleanup.
The public buffered and streaming clients authenticate the original URL host
when an advertisement changes the target or port, and apply the same guest IP
checks to QUIC as to TCP. Connection-establishment failures permit TCP fallback;
an HTTP/3 request failure is returned without replaying the request on TCP.
The request owner uploads bounded chunks while processing flow credit and early
responses. Lazy response bodies own their connection and release it on EOF or
explicit close. The live stream peer's `--http-client`, `--request-size=71680`,
`--buffered`, `--close-early` and `--discovery` options verify these paths,
including alternate-target authentication and both directions crossing 64 KiB.
Learned Alt-Svc caching, multiplexing, pooling, graceful QUIC wire closure and
TLS/HTTP/3 server integration remain work.

`Stdlib.HttpClient.Sse.Event` and `parse` are copied from the same revision.
The parser retains upstream's `Stream.unfold` behavior: it pulls only until the
next complete event, preserves `id` across blocks, joins repeated `data`
fields, ignores comments and non-data blocks, and flushes a final unterminated
block at end of stream. The source is split and qualified where required by the
compiler's module grammar; that does not change the public API or consumption
behavior.
