# Html and Http Parity

Current compiler source review: 2026-10-07 at `7154b0ea9c1a3f53d30984ed17b9e0cc5d8f0dce`.
See the [current audit](../current-audit.md) for post-port status and validation.
Older revision pairs and executed counts below are historical evidence, not
a fresh test result for this revision.

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
`StdLib/Html.dark`, `Http.dark`, and `StdLib/Http/Request.dark`, loaded
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
| Request accessors | Parameters split only for exactly one `=`; malformed parameters become whole keys with empty values; order and duplicates remain; duplicate lookup values join with commas. An absent or empty query produces one empty pair under frozen `String.split` behavior (`http.dark:57-88`) | Separate `Stdlib.Http.Request` module ports the exact behavior (`StdLib/Http/Request.dark`) | Parity, intentionally distinct from `parseQueryString` |
| Response helpers | Exact body/status/header argument order, status codes, spelling and order; HTML/text UTF-8 content types; JSON without charset; arbitrary redirect strings; empty 401/403/404 bodies (`http.dark:91-208`) | All helpers ported using Blob values (`Http.dark:68-121`) | Parity |
| Cookie boundary | `Cookie` is a record; the only `cookie` implementation is commented out and dependency-incomplete (`http.dark:210-259`). The execution fixture's `setCookie` probes are also commented out | Public structural `Cookie` only; no `cookie` or `setCookie` API. Caller-provided ordered and duplicate `Set-Cookie` pairs pass unchanged through `responseWithHeaders` | Non-gap at pinned revision |
| JSON boundary | `responseWithJson` accepts a serialized `String` (`http.dark:153-160`) | Same signature; no generic JSON serializer was introduced | Non-gap at pinned revision |

There are no intentional Html or Http behavior divergences in the active
pinned surface; void-tag detection is internal to Html serialization.

## Executable coverage and AOT boundaries

`test/fixtures/e2e/html_http.e2e` covers compiler-supported Dark syntax, public records and sums,
qualified aliases, every constructor, rendering boundaries, parser quirks,
request accessors, Blob bodies, every response helper, ordered duplicate
`Set-Cookie` headers, and Cookie construction. The imported Html
execution fixture has local layout adaptations and is enabled with individual line gates. The imported Http
fixture is currently whole-file gated; `html_http.e2e` supplies focused coverage.

Interpreter-style multiline list layout and qualified bare type aliases needed
small parser compatibility support. Recursive Html nodes also required the
existing reference-count machinery to release dynamic-buffer and recursive-list
payloads on both native backends. These are representation and syntax support,
not new Html or Http behavior.

Blob equality remains the interpreter's public handle identity. The compiler
E2E harness normally evaluates expected expressions as Dark equality; therefore
the focused `html_http.e2e` fixture compares response fields and decoded body
bytes instead of constructing a second fresh Blob for expected values. This adapts
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
trailers. Duplicate response headers remain ordered. Stateless and lazy calls
own one stream and connection; buffered sessions additionally reuse HTTP/2
owners as described below. Server push, CONNECT tunnels and OPTIONS
asterisk-form are not implemented in this profile.

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
successful bind, and serves bounded persistent plaintext HTTP/1.1 owners until
SIGINT or SIGTERM. HTTP/1.1 requests and responses may explicitly close the
connection; HTTP/1.0 always closes. Fully framed requests can be pipelined,
with ordered responses and correct HEAD handling.
Request URLs retain their raw path and query bytes; actual request methods are
prepended as `x-http-method`. Forwarded HTTPS canonicalization, optional
standard Server/HSTS headers, ordered duplicate response headers, HEAD body
suppression, and stdout request logging are implemented in Dark. An oversized
declared or chunked body receives 413 before the missing body bytes are read.
`Expect: 100-continue` is acknowledged only after framing and body-limit checks.

This initial server is developed and verified on Linux ARM64. Linux native
lowering also exists for x86_64, without a cross-target verification claim.
macOS ARM64 lowering now reads Mach absolute ticks with their queried timebase,
sets close-on-exec through `fcntl` after socket creation, and converts listener
poll timeouts to Darwin milliseconds. Native macOS execution remains
unverified. The persistent plaintext profile is also peer-verified on Linux
x86_64. The listener owns at most sixty-four accepted connections; each serves
at most one hundred requests. Per-call nonblocking I/O advances one bounded
transition per owner per round, so a slow upload or an unread response does not
hold the HTTP/1.1 listener. Request deadlines are ten seconds; idle and response
write deadlines are thirty seconds. Headers retain the wire parser's line,
count, and size caps. Configured body limits range from zero through 100 MiB,
and request wire buffering is capped at the body limit plus 1 MiB for framing.
Total retained request/output wire data caps at 101 MiB. Native read/write
polling, persistence, pipelining, close tokens, slow-client fairness and disposal
are checked by `http1_server_owner.e2e` and the independent server peer.
Multiplexed HTTP/2–3 servers now retain independent stream owners and shared
compression/flow state. Prior-knowledge h2c uses the same active scheduler as
HTTP/1.1. Response compression and interpreter telemetry remain follow-up work.

Owned TCP/TLS transports provide nonblocking application writes and explicit
flush completion. TLS accepts at most 2 KiB plaintext into one encrypted record,
retains its ciphertext and offset across partial sends, and drains earlier
control records before new application data. Requested KeyUpdate replies are
queued without blocking a record poll; idle checkout rejects unsent output.
The ciphertext queue is capped at 32 KiB. `tls_output_queue.e2e`, existing
TLS polling fixtures, and `scripts/test_tls_poll_queue_peer.py` cover blocked
and partial sends, flush, closed ownership and independent old/new-key ordering.
`HttpServer.Tls` and the TCP side of `HttpServer.Secure` use incremental handshake owners
and persistent HTTP/1.1 owners with the same count and deadline rules as the
plaintext listener. Independent peers verify another TLS client can complete
while a client stalls in ClientHello, Finished, upload or response reading.
Client application keys remain gated by authenticated Finished. TLS HTTP/2
owners retain multiplexed stream state and queued output. QUIC owners now retain
handshake, critical-stream initialization, HTTP/3 and drain state across rounds.
The combined listener advances TCP and UDP owners before its shared readiness
wait. Client TCP/TLS connection establishment remains synchronous.

All four server modules (`HttpServer`, `.Tls`, `.Quic`, `.Secure`) also provide
`serveOn address config ...`. Four-byte addresses bind IPv4 and sixteen-byte
addresses bind IPv6; invalid byte vectors fail before announcing. Existing
`serve` calls retain their IPv4 wildcard binding. IPv6 listeners share the same
ownership, timeout and shutdown behavior. `http_server_ipv6.e2e` and
`scripts/test_http_server_ipv6_peer.py` cover IPv6 HTTP/1.1, TLS/no-ALPN, HTTP/2,
HTTP/3 large bodies, rebinding and compiled leak accounting.

`test/fixtures/e2e/stdlib-internal/http_server.e2e` covers routing, configuration, framing limits,
and listener ownership. `python3 scripts/test_http_server_peer.py` exercises
the compiled server against local TCP clients, including fragmented and binary
bodies, 413/400/408/417/500 responses, HEAD, duplicate headers, bind failure,
reset clients, signal shutdown during a stalled request, rebinding, and
compiled leak accounting. Its artifacts remain in `TestResults/ai/`.

The IPv4 server also detects the HTTP/2 prior-knowledge connection preface,
including fragmented arrivals, and dispatches through the same Dark handlers.
It sends GOAWAY after one exchange. This profile is verified on Linux x86_64;
`HttpServer.Tls.serve` adds an IPv4 TLS listener using a configured
`TlsServerIdentity`, with h2 ALPN and HTTP/1.1/no-ALPN fallback. The HTTP/2 E2E fixtures
cover wire and field boundaries. `python3 scripts/test_http2_peer.py` uses the
test-only `h2==4.3.0` Python package as an independent peer and checks TLS client
negotiation, 70-KiB uploads/responses across flow-control windows, buffering,
streaming, trailers, HEAD, early stream close, early upload rejection (including
informational replies and retained HPACK state), truncation, cleartext server
dispatch, body limits, bounded draining after early replies, shutdown during
stalled HTTP/2 reads and compiled leak accounting.
After the response, HTTP/2 listeners accept late upload data and flow-control
credits until peer EOF, shutdown, 128 KiB, or ten seconds. Short accepted-socket
timeouts only poll this deadline; they do not prematurely close a slow reader.

`HttpServer.Secure.serve config identity handler onListening` binds TCP and UDP
on the same IPv4 port, announcing only after both binds succeed. It shares the
TLS HTTP/1.1/h2 and QUIC HTTP/3 handlers and adds a same-port `Alt-Svc` advertisement
unless the application already supplied one or returned 421. Application
`Alt-Svc: clear` is preserved. Accepted connections advance independently;
stalled handshakes, uploads and flow credit do not monopolize admission. Both transports and
the shutdown owner are released on bind failure or normal shutdown.
TCP and UDP listener readiness now share one bounded kernel wait; accepted
connections and UDP sockets also expose retained native readiness watches for
the active-connection scheduler. Closed owners return immediate terminal events.
`network_poll.e2e` and `scripts/test_network_poll_peer.py` verify multi-descriptor
readiness, including data arriving only on the second descriptor and accepted
TCP socket reads. Retained QUIC owners share routed datagrams and timers with
the TCP scheduler, with at most sixty-four owners per transport.
`python3 scripts/test_http_secure_server_peer.py` checks independent TLS/QUIC
peers, repeated large requests, the owned client session, lazy body lifetime,
occupied-port cleanup, same-port rebind and stalled TCP/QUIC shutdown. It also
checks TCP and a second QUIC connection while the first waits for upload bytes,
critical-stream credit or Finished. These bounded cases pass with zero leaks;
the [HTTP stack plan](../../project/http-stack-plan.md) separately tracks the
native heap lifetime limit exposed by repeated fresh TLS handshakes.

HTTPS clients now select HTTP/3 from compatible HTTPS DNS advertisements;
HTTP/3 also has an authenticated UDP server listener described below. The pure
wire layer provides bounded QUIC variable-length integers and HTTP/3 frame
and SETTINGS parsing. QPACK supports the RFC 9204 static table, static-name
references, literal names/values and shared Huffman decoding. The static-only
codec remains available with zero dynamic capacity. `test/fixtures/e2e/http3_qpack.e2e`
checks the RFC example, malformed references, table boundaries and list limits;
`python3 scripts/test_qpack_peer.py` verifies both directions against test-only
`pylsqpack==0.3.23`, including duplicate fields and compiled leak accounting.
Outgoing HPACK and QPACK select static indexed fields and static name references,
and use RFC 7541 Huffman encoding when it is shorter than the original string.
Credential and cookie values remain never-indexed. `http_header_compression.e2e`
checks wire vectors and duplicate/Unicode fields; the independent
`scripts/test_http_header_compression_peer.py` checks all 256 Huffman symbols,
padding, a near-limit input, HPACK/QPACK round trips and sensitive-field flags.
The dynamic QPACK checkpoint adds connection-owned inbound tables with a
4096-byte advertised capacity and one blocked stream. Encoder instructions are
parsed incrementally; blocked HEADERS defer later DATA until insertions arrive.
Decoder feedback is queued under QUIC flow control, with each write bounded to
900 bytes. Focused malformed-wire and blocked-message fixtures pass. Independent
ls-qpack checks verify 560 sections, fragmentation, eviction and insert-count
wrap-around. The delayed aioquic peer verifies section acknowledgments, Unicode
headers, complete 70 KiB bodies and authenticated close; its observation window
is 30 seconds, without changing engine deadlines. Shared field-value validation
permits UTF-8 obs-text and still rejects CR, LF, NUL, DEL and other control bytes
except HTAB. Outgoing QPACK still uses only static/literal representations;
dynamic encoder/reference tracking remains open.
HTTP/2 outgoing blocks now share a separate bounded dynamic HPACK table across
connection state transitions. Peer table-capacity changes evict entries and
emit ordered size updates before the next block; credentials and cookies stay
never-indexed. Peer HTTP/2 and HTTP/3 decoded field-section limits are checked
before outgoing headers when the setting is known. Focused encoder/limit
fixtures and seven consecutive blocks decoded by the independent `hpack`
implementation cover reuse, eviction and intermediate capacity changes.
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
Authenticated QUIC application owners enforce the negotiated nonzero idle
limit with an RTT-derived three-PTO floor, independently of HTTP exchange
deadlines. Receives and the first ack-eliciting send after a receive restart
the timer. The idle fixture and an aioquic upload stalled beyond a short peer
limit check expiry and subsequent listener availability. Handshake owners
retain their separate bounded deadlines.
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
`HttpClientSession.create`, `createTrusted` and `createTrustedWithRoots` expose
owned `request`, `requestMany`, `stream`, `clear` and `close` functions. Each session learns
`h3` Alt-Svc alternatives from authenticated HTTPS response headers, binds them
to the original scheme/host/port and replaces that origin's advertisements on
each new field. Its cache holds at most 32 alternatives, caps freshness at one
day and conservatively subtracts response age and request duration. `clear`,
expired advertisements and status 421 invalidate alternatives; callers should
clear the session after a network change. Buffered HTTP/1.1 session requests
also retain at most eight idle origin connections for thirty seconds, over
TCP or TLS. Only fully consumed, self-delimited HTTP/1.1 responses without a
`Connection: close` token or surplus bytes are reusable. Checkout discards
expired, closed, failed or readable idle sockets; TLS owners also reject
buffered encrypted bytes. This conservatively discards TLS control traffic
instead of blocking checkout. A failed attempted request is never replayed.
Session clear and close release the cache and pooled connections; a returned
lazy body exclusively borrows its connection until consumed or closed. Valid
completion returns eligible HTTP/1.1–3 owners; abandonment retires them. Clear
invalidates outstanding leases, so later completion cannot repopulate the pool.
Calls after session close return `NetworkError`; a subsequent body pull releases
its borrowed owner. Existing stateless
client entry points retain HTTPS DNS discovery without persistent Alt-Svc state.
`alt_svc.e2e`, `alt_svc_cache.e2e`, `http_client_session.e2e` and
`python3 scripts/test_alt_svc_peer.py` cover parsing, bounded cache eviction,
origin separation, lifetime reduction, disposal and HTTPS-to-HTTP/3 negotiation
with both buffered and lazy responses. The live session peer also checks
misdirected responses and repeated TCP fallback after certificate rejection.
RSA-PSS and PKCS#1 certificate verification use a pure Dark Montgomery public
modular-power implementation with scratch storage bounded by the modulus width.
This removes the repeated large-integer intermediate allocations that exhausted
the native heap during that fallback check. `rsa_montgomery.e2e` and
`python3 scripts/test_rsa_montgomery.py` check malformed inputs, carry boundaries
and 48 independent Python `pow` vectors through 8192-bit moduli. This public
exponent operation is not a private-key signing implementation.
Buffered sessions also keep up to eight idle authenticated HTTP/2 origins for
thirty seconds, preserving HPACK, TLS keys, connection credit and monotonically
allocated odd stream IDs. Checkout consumes ready control frames and retires
closed or GOAWAY connections before any new request write. The internal
`__Http2Connection.requestMany` scheduler supports up to 32 requests and respects
peer concurrency limits. Upload windows and response parsing are stream-local;
HPACK and connection credit remain shared. DATA sends rotate across active
streams, out-of-order responses return in input order, resets affect only their
stream and GOAWAY drains eligible streams without replaying refused requests.
Each batch caps aggregate uploads and response buffering separately at 100 MiB.
TCP/TLS polling retains incomplete records/frames and authenticates TLS control
records without waiting for more bytes. `http2_connection.e2e`,
`http_transport_poll.e2e` and `scripts/test_http2_session_peer.py` check reuse,
reverse/interleaved 70 KiB replies, 140 KiB uploads, early responses, shared
compression, stream limits, resets, GOAWAY and cleanup. Public session batches
and HTTP/3 pooling/scheduling are described below. Lazy multiplexed bodies and
multiplexed servers remain unfinished.
Buffered and lazy HTTP/3 clients retain the latest packet-number/key state
through terminal events and send encrypted H3_NO_ERROR on completion or early
body closure. Closing keys stay available for three PTOs, capped at thirty
seconds, and repeated closes use fresh packet numbers. A failed transport
operation aborts the socket without encrypting from potentially stale state.
`python3 scripts/test_quic_stream_peer.py --http-client` and its `--buffered`,
`--close-early` and `--body-size=0` variants check peer-authenticated closure,
large bidirectional bodies, loss, reordering, trailers, key updates and cleanup.

The server identity foundation exposes `TlsServerIdentity.create` for a PEM
certificate chain and an unencrypted PKCS#1 or PKCS#8 RSA private key. The
initial signing profile accepts two-prime RSA-2048 with a public exponent of at
most 32 bits. Import checks canonical DER, component consistency, leaf signing
authorization and equality of the certificate's public key with the private
key's public components. Chains are capped at eight certificates and 64 KiB of
DER. `RsaSigning.signSha256` uses a fresh 32-byte salt and blinding factor,
performs both modular products for all 2048 private-exponent bits, wipes raw
scratch buffers and verifies the result before releasing a signature. The x64
probe's emitted loop and limb selection were inspected for branches and
addresses controlled by secret bits; this is not an independent cryptographic
audit or an ARM64 instruction-inspection claim. `rsa_private.e2e`,
`rsa_signing.e2e`, `tls_server_identity.e2e`,
`python3 scripts/test_rsa_private.py` and
`python3 scripts/test_rsa_signing.py` cover generated key formats, inconsistent
components, certificate/key mismatch, independent PSS verification, salt
diversity, fault rejection and cleanup. `Tls13ServerHandshake.start` builds a
pure TLS 1.3 AES-128-GCM/X25519 certificate flight with RSA-PSS/SHA-256;
`finish` authenticates client Finished before returning application keys.
Bounded ClientHello parsing rejects duplicate extensions, malformed algorithm
and key-share vectors, invalid compression and trailing bytes. Unknown ALPN
names remain opaque bytes; selection uses server preference. Certificate
signature offers are checked, with the trust-anchor exception. The independent
`python3 scripts/test_tls_server_hello.py` and
`python3 scripts/test_tls_server_handshake.py` exercise OpenSSL negotiation,
both application directions, invalid Finished and zero-leak cleanup. This
engine negotiates X25519 directly or through one HelloRetryRequest. It uses
the RFC 8446 message_hash transcript replacement and checks the second offer's
unchanged parameters, single requested share, early-data removal, padding and
permitted PSK age/binder or incompatible-identity changes. The independent
`python3 scripts/test_tls_server_retry.py` checks 25 offer/transcript cases and
real OpenSSL HTTP/1.1/h2 authentication and application traffic after retry.
`HttpServer.Tls.serve`
connects it to shutdown-aware TCP records and the shared HTTP/1.1 and HTTP/2
handlers. Handshake and HTTP/1.1 reads have fixed ten-second deadlines;
HTTP/2 reads use ten-second idle deadlines and a bounded record count. Record
sequence state advances before each application write, including failures,
and close sends close_notify with the next nonce before releasing the socket.
The independent `python3 scripts/test_http2_server_tls_peer.py` covers 70 KiB
uploads/responses, HEAD, early body-limit rejection, Expect, ALPN fallback and
rejection, fragmented ClientHello, forged Finished, shutdown during handshake
and application reads, and cleanup. QUIC uses the same HelloRetryRequest
transcript and offer checks with continuous Initial CRYPTO offsets and fresh
packet numbers. `python3 scripts/test_quic_server_tls_retry_peer.py` verifies
encrypted retry/ServerHello offsets, Initial retransmission numbers, independent
ECDH/HKDF, a trusted certificate and RSA-PSS signature, both Finished messages,
application-key gating and zero leaks through the real server packet spaces.
TCP TLS application transports retain both traffic secrets and process bounded
post-handshake KeyUpdate messages. Receive updates reset the receive sequence;
requested replies use the next unused old send nonce before switching the send
secret and resetting its sequence. Updates can span records, but application
data cannot interrupt a partial handshake message, and bytes after a complete
update must arrive under the new key. Clients validate and discard bounded
NewSessionTicket metadata without enabling resumption or 0-RTT; servers reject
client tickets. Connections accept at most 64 received updates and 64 tickets.
`python3 scripts/test_tls_key_update_peer.py` checks all three cipher suites
against independent AEAD/HKDF, live authenticated server updates, failed reply
writes, EOF, idempotent closure and zero leaks.
`python3 scripts/test_tls_key_update_client_peer.py` checks requested updates
after 70 KiB uploads through buffered and streaming HTTP/1.1 and h2 clients,
including the exact old-key reply sequence and zero leaks. The pure
`QuicServerTls` adapter now requires h3 ALPN and client transport parameters,
binds the client's initial source connection ID, derives QUIC handshake packet
keys, and releases application secrets and packet keys only after verifying
client Finished. `python3 scripts/test_quic_server_tls_peer.py` checks these
secrets against an independent aioquic client, verifies packet keys using a
separate HKDF implementation, and covers malformed parameters, source mismatch,
ALPN rejection, invalid Finished and cleanup. `QuicServer` now owns bounded
Initial/Handshake packet spaces after address validation, discards Initial
only on an authenticated Handshake packet, and gates application ownership
on client Finished. The client and server share packet-number reservation,
recovery and congestion budgeting. `Http3.initializeServer` owns server
critical streams and request parsing, including peers that grant no server
bidirectional streams. `python3 scripts/test_quic_server_peer.py` checks live
Retry, authenticated HTTP/3 echoes through 70 KiB, dropped flights, duplicates,
corrupted ciphertext, zero server-bidi credit and zero leaks.

`HttpServer.Quic.serve` provides multiplexed requests within each connection
and fair retained connection admission, using the same
configuration, imported identity and HTTP handler as the TLS listener. It
shares method/URL/header handling, optional standard headers and logging,
configured body limits, HEAD representation lengths and early 413 rejection.
Application state is disposed through a child socket while the listener stays
open. A successful response leaves its connection available for reuse. Up to
thirty-two request owners share compression and connection credit while
retaining independent parsing, response credit and deadlines. Completed streams
renew concurrency credit; the bounded request history drains through GOAWAY.
Graceful server termination gets a bounded ten-second delivery drain before an
encrypted H3_NO_ERROR close; closing keys are retained for three PTOs, capped
at thirty seconds, with fresh packet numbers for repeated close frames.
The listener retains only peer/CID/key/packet-number closing metadata, not
response buffers, and can admit another connection during that interval.
Closing replies are rate-limited and failed sends discard the closing keys
instead of risking a stale nonce. Shutdown interrupts handshake and request
waits and disposes the cache. The independent
`python3 scripts/test_http3_server_peer.py` checks routing, 70 KiB flow, HEAD,
early 413, explicit encrypted peer closure, token replay suppression, unsupported ALPN,
same-port rebind, incomplete-body shutdown and zero leaks. Its aioquic client
supplies HEAD method semantics because that library's raw H3 layer does not
retain the originating request method. The same peer also checks forty requests
on one connection and a completed response alongside a stalled stream. The
secure-server peer separately establishes fair UDP/TCP admission. Listener
output retains at most eight sealed datagrams per child and retries one atomic
nonblocking send per round without resealing or reserving another nonce. All
QUIC owners share a 101 MiB retained payload budget; child disposal never closes
the shared UDP listener.

`QuicServerRetry` provides listener-key HMAC tokens bound to the peer's address
and port, original destination CID, client source CID and Retry source CID.
It authenticates the token before interpreting its fields, rejects future or
older-than-30-second tokens, and emits the QUIC v1 Retry integrity tag.
`quic_server_retry.e2e` and `python3 scripts/test_quic_server_retry.py` cover
invalid local inputs, 31 independent HMAC/expiry/address/CID/tamper cases and
aioquic packet integrity, with cleanup accounting. The UDP listener retains
up to 256 consumed CIDs for at least thirty seconds, including failed handshakes
and any longer closing deadline,
and refuses admission when the live replay history is full.

`Stdlib.HttpClient.Sse.Event` and `parse` are copied from the same revision.
The parser retains upstream's `Stream.unfold` behavior: it pulls only until the
next complete event, preserves `id` across blocks, joins repeated `data`
fields, ignores comments and non-data blocks, and flushes a final unterminated
block at end of stream. The source is split and qualified where required by the
compiler's module grammar; that does not change the public API or consumption
behavior.

Buffered HTTP/3 sessions now retain QUIC packet recovery, traffic keys, QPACK
state and client stream IDs across requests, with eight idle origins and thirty
second expiry. Connection rotation after 1024 requests bounds retained final-size
history. Up to 64 active transport owners are independent of completed channels.
`Session.requestMany` accepts up to 32 `Request` records and returns ordered
`Outcome` records containing their input index and response result. It validates
all requests before any I/O, groups requests by origin and uses the negotiated
HTTP/2 or HTTP/3 scheduler; distinct origins and HTTP/1.1 exchanges run sequentially.
Uploads and responses each have a 100 MiB aggregate batch limit. A stream reset
fails that request while other eligible streams finish. Requests rejected by
GOAWAY are reported without replay. `Session.stream` now exclusively checks out
an HTTP/1.1, HTTP/2 or HTTP/3 owner. Only complete, validated body framing returns
it to the pool. HTTP/1.1 additionally requires no surplus bytes and idle readiness;
HTTP/2–3 preserve shared compression and terminal parser/transport state.
Early closure, failures, GOAWAY and session invalidation retire the borrowed owner.
Simultaneous lazy bodies use separate checked-out connections, not multiplexed
body readers. `http_session_stream.e2e`, `http_session_stream_h2.e2e` and the
session peers' `--stream` modes cover framing, invalidation, reuse and cleanup,
including independent 70 KiB TCP/TLS, HTTP/2 and HTTP/3 responses.
`http_session_batch.e2e`, `quic_flow.e2e`, and the session peers' `--public`
HTTP/2 and `--public --multiplex` HTTP/3 modes exercise these APIs.

## Upstream correctness adoption (2026-10-11)

The historical matrix above documents the earlier pinned interpreter behavior,
not the current upstream contract. The branch `adopt/upstream-http-html-correctness`
updates the pure surfaces to match darklang/dark PRs #5727 and #5735:

- `Http.parseQueryString` decodes `+` as a space and percent escapes as bytes,
  preserves invalid escapes, and keeps last-duplicate-wins dictionary semantics.
- `Http.Request.queryParams` splits at the first `=` and decodes keys and
  values while preserving ordered duplicates; `queryParam` still joins
  duplicate values with commas.
- `Html.nodeToString` escapes attribute values with `escapeHtml`, and
  `Html.comment` rewrites `--` to `- -` to prevent premature closure.

The updated `test/fixtures/e2e/html_http.e2e` pins the new behavior.
On 2026-10-12, after incorporating compiler main at
`26a2a769276e24c0d8ab6b03e025a0ead2214aaa`, native x86-64 validation passed:
`env -u LD_PRELOAD ./build --ai`; the focused `--filter=html_http` run
(37/37); `./run-tests --ai` (13,197/13,197); `dune runtest`; and
`python3 scripts/check_compiled_leaks.py` (58/58 clean workloads).
The VM used OCaml 5.5.1, Dune 3.24.2 and all 98 locked packages, with
the repository's native compilation and Unix smoke checks passing.

`./benchmarks/run_benchmarks.sh --verify-parent full` stopped before measurement:
the existing x86-64 full snapshot has an incompatible workload contract digest
(`2ead0323fc6859c952a3b8a83c56e0b0da61e1350b7ddd39ecc4a0b1da25e861`,
versus current `2a3f42ac1a2085fe75562c15a454d4ec18429c61d8f22ee22c9619d5d0ef029f`).
This branch changes no benchmark sources, contracts or snapshots. Correctness
checks passed; the performance gate is unverified and the PR remains unmerged.
