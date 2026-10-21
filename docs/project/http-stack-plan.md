# Native HTTP stack plan

The compiler should provide the interpreter's `Stdlib.HttpClient` and
`Stdlib.HttpServer` APIs without invoking another HTTP program or linking to a
host HTTP implementation. HTTP protocol and policy code belongs in Dark. The
native boundary provides only operating-system transport, time, and entropy.

The interpreter reference is `packages/darklang/stdlib/httpclient.dark`,
`httpserver.dark`, `backend/src/LibExecution/Host/HostHttp.fs`, and
`backend/src/Builtins/Builtins.Http.Server/Libs/HttpServer.fs` in `darklang/dark`.
The compiler already has the pure request/response types, response helpers,
route constructors, and SSE parser described in
[`../compatibility/stdlib/html-and-http.md`](../compatibility/stdlib/html-and-http.md).

## 1. Native transport boundary

Add private typed socket operations for macOS ARM64, Linux ARM64, and Linux
x86_64. The public Dark wrapper owns each socket handle and closes it exactly
once, including after failures and stream abandonment. A handle is never
interchangeable with an ordinary file descriptor in source code. Native
operations return structured OS errors; retryable interruption and readiness
are represented explicitly, rather than guessed from a failed read.

The primitive set is TCP and UDP socket creation, bind, listen, accept,
connect to a specified IPv4 or IPv6 address, partial receive/send, shutdown,
close, and readiness with a monotonic deadline. Add monotonic time and a
fallible secure-random-byte operation. Set close-on-exec on every created or
accepted descriptor. Bound all native buffer lengths and validate ports and
addresses before issuing syscalls. The Dark wrapper handles partial writes,
end-of-stream, cancellation, and cleanup. Expose no URL, DNS, TLS, or HTTP
operation in this boundary.

Implement and test this incrementally: start with socket creation/close,
then a loopback TCP round trip, then UDP, IPv6, deadlines, and cleanup. Each
behavior starts with a focused failing E2E test. Keep target-specific syscall
numbers and socket constants in `src/Platform.ml`; exercise each target on its
matching host architecture.

## 2. DNS and HTTP/1.1 in Dark

Read resolver configuration and hosts entries through existing file effects.
Implement bounded DNS queries over UDP, including A/AAAA records, CNAMEs,
timeouts, retries, and response validation. Connect to a checked resolved IP
address so DNS cannot change the destination between policy and connection.
Parse absolute HTTP URLs with explicit scheme, authority, port, path, query,
and percent-encoding rules.

Implement incremental HTTP/1.1 request and response framing in Dark: start
lines, headers, fixed-length and chunked bodies, connection-close bodies, and
the method/status cases without a body. Enforce header, line, body, and
connection limits. Reject ambiguous framing, conflicting content lengths,
and malformed transfer encodings during parsing. Preserve duplicate headers
and wire bytes until the public API calls for normalization.

## 3. Client and server APIs

Implement `HttpClient.request` and its method wrappers on the Dark transport.
Implement `stream` using the existing single-consumer `Stdlib.Stream` lifecycle;
closing or draining a response closes its connection. Match the interpreter's
typed errors, timeouts, no automatic redirects, no automatic cookies, and
guest private-network restrictions. Keep the connection address tied to the
one that passed the restriction check.

Implement `HttpServer.serve` over a bound listener. Preserve the request shape,
body limit, 413 behavior, `onListening` after successful bind, response header
behavior, and shutdown semantics. Begin with bounded sequential service;
introduce concurrent connections after their ownership and deadline behavior
can be tested. Keep routing in Dark.

The first Linux ARM64 server now implements bounded sequential IPv4 HTTP/1.1
service, configuration, routing, body limits, standard headers, forwarded URL
canonicalization, request logging, and graceful signal shutdown. See the
[HTTP compatibility ledger](../compatibility/stdlib/html-and-http.md#additional-pure-http-surfaces)
for the tested profile and remaining server work.

## 4. HTTPS

TLS is not an operating-system syscall. An entirely Dark HTTPS client requires
TLS record framing, handshake, authenticated encryption, key exchange, secure
randomness, certificate-chain and hostname verification, and trust-store
loading. The client implements TLS 1.3 X25519 with AES-128-GCM/SHA-256,
AES-256-GCM/SHA-384, and ChaCha20-Poly1305/SHA-256. It supports RSA-PSS and
P-256 ECDSA server authentication, RSA or P-256 ECDSA signed X.509 chains, hostname checks,
and system CA bundles. Unsupported cipher, key, certificate, and protocol
choices fail closed. Local TLS peers and protocol vectors exercise the profile;
streamed HTTPS responses use the same authenticated TLS record path and close
their connection when drained or explicitly closed. Server identities now import
bounded PEM certificate chains and unencrypted RSA-2048 private keys, verify
their binding and sign with blinded, fixed-work RSA-PSS/SHA-256. The pure server
handshake now negotiates TLS 1.3 AES-128-GCM/X25519 and RSA-PSS/SHA-256,
serializes the certificate flight, and releases application keys only after
verifying client Finished. OpenSSL checks exercise h2, HTTP/1.1 and no ALPN,
both application directions, malformed offers and invalid Finished. The
`HttpServer.Tls.serve` listener now shares routing, body limits and responses
with the existing HTTP/1.1 and HTTP/2 implementations. Accepted sockets retry
short read timeouts under shutdown-aware deadlines, and transport closure
sends close_notify and releases retained state. Independent OpenSSL/hyper-h2
checks exercise 70 KiB flow control, fallback, rejection and stalled shutdown.
The pure QUIC server TLS adapter also derives packet-protection keys, requires
h3 and binds client transport parameters before verifying client Finished.
An independent aioquic client agrees on all handshake/application traffic
secrets and completes the certificate flight. The server packet-space owner
now validates returned Retry tokens before signing, bounds ClientHello and
Finished CRYPTO, retransmits with fresh packet numbers, and releases a
role-aware HTTP/3 application owner only after Finished. Independent UDP
aioquic checks cover 70 KiB bidirectional flow, loss, duplicates, corrupted
packets, zero server bidirectional-stream credit and leak-free disposal.
`HttpServer.Quic.serve` now binds an owned sequential IPv4 UDP listener,
shares HTTP routing/headers/body limits, emits encrypted HTTP/3 closure,
and returns normally on shutdown during an incomplete body. Its bounded
Retry CID history suppresses replay of both completed and aborted handshakes.
Independent aioquic checks cover 70 KiB echo, HEAD representation lengths,
early 413, unsupported ALPN, replay suppression, rebind and zero leaks.
Minimal closing-key metadata is retained in the bounded listener history,
allowing later admissions without waiting for each connection's three-PTO
closing interval. Failed sends abandon the keys rather than reuse a nonce.
HTTP/2 delivery drains retry short socket timeouts within a shutdown-aware
ten-second/128-KiB bound, allowing a native client to consume encrypted response
frames and send late flow-control credits before TCP closes.
TCP TLS now supports one X25519 HelloRetryRequest, the message_hash transcript
replacement, compatibility CCS, immutable second ClientHello fields, permitted
padding/early-data changes and PSK age/binder updates or incompatible-identity
removal. OpenSSL verifies retried HTTP/1.1 and h2 certificate/Finished flights;
25 independent offer/transcript vectors cover positive and negative changes.
QUIC uses the same authenticated retry flow while preserving cumulative
Initial CRYPTO offsets and packet numbers across both hellos. An independent
aioquic packet-crypto peer verifies Initial retransmission numbers, handshake
encryption, ECDH/HKDF, the trusted RSA-PSS certificate flight, both Finished
messages and gated application secrets. TCP TLS clients and servers now process
bounded post-handshake KeyUpdate messages, including fragmented updates and
requested replies. Directional secrets and record sequences advance together;
reply failures close ownership before later writes can reuse a nonce.
Independent AEAD/HKDF checks cover all three client cipher suites, and live
authenticated peers exercise HTTP/1.1 and h2 updates after 70 KiB uploads.
The Retry foundation authenticates bounded address/CID-bound HMAC tokens with
a 30-second lifetime and serializes QUIC v1 Retry packets. Independent Python
and aioquic checks cover token tampering, expiry, IPv4/IPv6 binding and packet
integrity. The handshake owner enforces the first Initial datagram's minimum
size and requires a validated challenge before certificate flights. The UDP
listener consumes every authenticated Retry CID before starting TLS and
retains it for thirty seconds, refusing new admissions rather than evicting
a live entry when its 256-entry bound is full.

## 5. Compatibility and readiness

Compare observable results with the interpreter's HTTP fixtures using local
servers, plus malformed-wire and resource-lifecycle tests. Add stress and
performance measurements for buffered and streaming bodies. HTTP/1.1 is the
first interoperability target; HTTP/2 and HTTP/3 require separate protocol
work. HTTP/2 now has a single-exchange HTTPS client (authenticated ALPN with
HTTP/1.1 fallback) and a prior-knowledge cleartext server. HTTP/3 now has a
certificate-authenticated QUIC client, loss recovery, stream flow control,
key updates, static/literal QPACK, critical-stream/message validation and
buffered or lazy public client responses selected through HTTPS DNS or an
owned `HttpClientSession` Alt-Svc cache.
Buffered and lazy clients close QUIC with authenticated H3_NO_ERROR from the
latest application state, including empty final events and early cancellation.
Transport errors abort without sealing a close from stale packet-number state.
`HttpServer.Secure.serve` binds same-port TCP/UDP listeners with one identity,
handler, shutdown owner and automatic same-origin Alt-Svc advertising. It
preserves application advertisements and 421 responses, and releases both
transports when either bind fails. The separate TLS and QUIC entry points
remain available; see the compatibility ledger for exact profile boundaries. For
each implementation branch, run `./build --ai`, the already-built
`./run-tests --ai`, and
`./benchmarks/run_benchmarks.sh --verify-parent full` before merge-train
handoff.

## Full engine extension scope

The extension branch starts from `b3d13bb` and covers the complete engine gap
inventory, including optional client and transport extensions. Checked items
below have focused behavioral and independent peer validation; they do not
mean the entire branch has passed its integration or performance gates.

- [x] Shared native TCP/UDP readiness sets, retained descriptor ownership,
  immediate closed-owner events, and combined secure-listener polling.
- [x] Outgoing HPACK/QPACK static-table references and Huffman encoding,
  retaining never-indexed credentials and cookies.
- [x] IPv6 TCP listeners and explicit `serveOn` addresses for plain, TLS,
  QUIC and combined secure servers.
- [ ] Incremental nonblocking handshakes and active-connection servicing,
  including per-connection timers and fair TCP/UDP admission.

  - [x] Native per-call nonblocking TCP receive/send, with partial-write
    accounting, unready results, EOF and closed-owner checks. Existing blocking
    handshake helpers retain their descriptor mode. The independent
    `scripts/test_network_nonblocking_peer.py` verifies backpressure and cleanup.
  - [x] Plaintext HTTP/1.1 active owners share readiness waits and advance one
    bounded read, parse or write transition per round. Slow uploads and unread
    responses do not monopolize other HTTP/1.1 clients. Prior-knowledge h2c
    connections now use the shared multiplexed HTTP/2 owners. Client TCP/TLS
    connection establishment still needs the active-owner migration.
  - [x] Owned TLS transports queue KeyUpdate replies and application ciphertext
    across nonblocking partial writes. `pollWrite` reports accepted plaintext;
    `flush` confirms ciphertext completion. A queue holds one application record
    (at most 2 KiB plaintext) plus bounded control records, never reseals a
    partial record, and is drained before legacy blocking writes. Independent
    record-crypto peers verify backpressure and old/new-key wire ordering.
  - [x] `HttpServer.Tls` and the TCP side of `HttpServer.Secure` service at most sixty-four
    incremental handshake/HTTP/1–2 owners. HTTP/2 retains shared HPACK, up to
    thirty-two streams, separate stream windows, connection credit and queued
    frame output. Stalled ClientHello, Finished, uploads, response readers and
    HTTP/2 streams without credit do not monopolize other TLS TCP clients.
    Admission and owner steps use fresh clock readings.
  - [x] QUIC listeners retain at most sixty-four handshake, initialization,
    HTTP/3 and drain owners. One listener routes a datagram by CID and peer,
    then advances every owner with fresh clock readings and independent
    deadlines. HTTP/3 critical-stream initialization waits incrementally for
    credit. The combined listener advances TCP and UDP owners in the same loop.
    Per-call nonblocking UDP sends retain sealed packets in an eight-packet
    child queue; each round attempts one atomic send per owner. Unready sends
    never reserve a new nonce or reseal a packet. Closing children leaves the
    shared socket live. A 101 MiB payload budget includes HTTP bodies, queued
    transport frames and stream reassembly across all QUIC owners.
    `datagram_poll.e2e` and `quic_listener_owner.e2e` cover native polling,
    atomic backpressure and cleanup. The independent secure-server peer proves
    that stalled uploads, zero critical-stream credit and an unsent Finished
    leave TCP and a second QUIC connection live, with simultaneous-owner
    shutdown, same-port rebind and zero leaks.
- [ ] Persistent HTTP/1.1 connections and HTTP/2–3 connection reuse/pooling.

  - [x] Buffered `HttpClientSession.request` HTTP/1.1 reuse over TCP/TLS:
    eight idle origins, thirty-second expiry, complete self-delimited responses,
    idle readiness checks, explicit clear/close and no attempted-request replay.
    `http1_pool.e2e` covers expiry, eviction, origins, framing and cleanup;
    `scripts/test_http1_session_peer.py` exercises independent TCP/TLS peers.
  - [x] Persistent plaintext HTTP/1.1 server exchanges and ordered pipelining,
    with HEAD framing, explicit close tokens, one hundred requests per owner,
    sixty-four accepted owners, ten-second request deadlines, thirty-second
    idle/write deadlines and a 101 MiB retained-wire budget. HTTP/1.0 retires
    after one exchange.
  - [x] Persistent TLS HTTP/1.1 exchanges and ordered pipelining on both
    `HttpServer.Tls` and `HttpServer.Secure`, with the same HEAD/close/count/deadline
    rules and explicit ciphertext flush before exchange completion. Same-port
    peers also verify Alt-Svc policy, repeated HTTP/3 exchanges, shutdown,
    rebinding and cleanup when either bind fails.
  - [x] Lazy HTTP/1.1 session body leases over TCP/TLS. Complete self-delimited
    framing alone returns the owner; early close, surplus bytes, HTTP/1.0,
    close tokens and EOF-delimited bodies retire it. Clear invalidates outstanding
    lease generations, and pulls after session close release the borrowed owner.
  - [x] Buffered HTTP/2 session owners retain HPACK, connection credit and odd
    stream IDs across requests. Eight idle origins expire after thirty seconds;
    checkout retires stale/GOAWAY owners before writing a request.
  - [x] Buffered HTTP/3 session owners retain QUIC recovery, traffic keys,
    QPACK tables and stream IDs across requests. Eight idle origins expire
    after thirty seconds. Completed stream history remains bounded by rotation
    after 1024 requests; closed streams do not consume the 64 active owner slots.
  - [x] Lazy HTTP/3 session body leases retain the latest QUIC/QPACK owner on
    valid completion and retire it on abandonment, failure or invalidation.
    A completed early response resets only an unfinished upload before reuse.
  - [x] Lazy HTTP/2 session body leases use the connection scheduler and retain
    terminal DATA/trailer state, HPACK and connection credit. Owners remain
    exclusively checked out until the body is consumed or closed; concurrent
    lazy bodies are not yet multiplexed on one connection.

- [ ] HTTP/2 and HTTP/3 request multiplexing, stream allocation and scheduling.

  - [x] Internal HTTP/2 batch scheduler: at most 32 requests, peer concurrency
    limits, one DATA frame per active stream per round, separate upload windows,
    shared HPACK in wire order, ordered per-request results, isolated resets,
    and GOAWAY draining without replay. Aggregate upload/response bodies each
    cap at 100 MiB. Client transport polling retains partial TLS/HTTP frames and
    authenticates post-handshake control records without waiting for more bytes.
    `http2_connection.e2e`, `http_transport_poll.e2e` and the independent
    `scripts/test_http2_session_peer.py` cover these boundaries.
  - [x] Public `Session.requestMany` validates at most 32 requests before I/O,
    groups origins, multiplexes negotiated HTTP/2–3 and restores input result
    order. Different origins are serviced sequentially; HTTP/1.1 uses complete
    sequential exchanges. Network failures are per-request outcomes; invalid
    batches fail before sending any request.
  - [x] Plaintext and TLS HTTP/2 servers retain independent request/response state and shared
    compression across exchanges, rotate writable streams, isolate resets,
    and publish receive credit only after control frames are flushed. Partial
    headers, bounded bodies, early responses and per-stream deadlines are
    covered by `http2_server_flow.e2e`, `http2_server_control.e2e` and
    `http2_server_dispatch.e2e`. The independent plaintext and TLS server peers verify
    blocked-stream admission fairness, selective window updates, reset
    isolation, persistent reuse and 70 KiB upload/response bodies. Maximum DATA
    frames retain their nine-byte headers across bounded native writes without
    debiting flow credit twice.
  - [ ] Lazy-stream scheduling.
  - [x] HTTP/3 servers retain up to thirty-two independent request/response
    owners on one connection, share QPACK and connection credit, skip blocked
    response streams, isolate resets and malformed messages, and renew
    MAX_STREAMS only after both directions close. Retained bodies cap at
    100 MiB; request and response deadlines remain independent. The 1,024
    request-history bound sends one flow-controlled GOAWAY with exclusive
    boundary 4,096 and drains eligible replies. `http3_server_owners.e2e`
    covers these boundaries; the independent aioquic peer verifies a stalled
    request alongside a 70 KiB response, selective stream credit, cancellation
    and forty sequential requests on the same connection. Listener-owned
    connection routing and nonblocking queued datagram output now extend
    fairness across connections and the combined TCP/UDP listener.
  - [x] HTTP/3 request scheduling shares connection credit and QPACK while
    retaining independent message parsers. Each round sends one bounded chunk
    per stream. Resets and GOAWAY preserve eligible responses without replay;
    unfinished uploads are reset only after a complete early response. Aggregate
    uploads and responses each cap at 100 MiB. `http_session_batch.e2e`,
    `quic_flow.e2e` and independent HTTP/2–3 session peers cover these boundaries.
- [x] Connection-owned dynamic HPACK encoding, bounded eviction, sensitive
  literals, and peer table-capacity shrink/grow updates.
- [ ] Complete dynamic QPACK tables, instruction streams, blocked sections,
  feedback and eviction.

  - [x] Inbound dynamic tables, fragmented encoder instructions, blocked
    field sections, insertion increments and section acknowledgments.
    Server request owners retain QUIC receive credit while a field section
    waits for insertions, preventing a valid large upload from overflowing
    deferred DATA. Resets still cancel blocked requests. The server-owner
    fixture and independent fragmented-instruction QPACK peer cover this case.
  - [x] Outgoing dynamic encoding, peer feedback, reference tracking and
    pure stream-cancellation ownership.
  - [ ] Cancellation and table ownership across reusable multiplexed connections.
    Cancelling a wholly queued outgoing section now releases its table references;
    any written stream byte keeps them pinned until peer decoder feedback.
    Encoder insertions remain committed and cancellation does not acknowledge them.
    Server-owner and encoder fixtures cover eviction after cancellation and the
    partial-write boundary. Abandoned lazy bodies still close their leased
    connection; general cancellation and recoverable failure ownership remain.

- [ ] Streaming client uploads, server requests and server responses.
- [ ] Public received/sent trailer APIs.
- [ ] Content-encoding negotiation, decompression and server compression.
- [ ] Configurable connect/header/body deadlines and request cancellation.
- [ ] Happy Eyeballs address racing.
- [ ] QUIC NAT rebinding, migration, path validation and preferred addresses.
- [ ] QUIC path-MTU discovery, ECN congestion integration and NEW_TOKEN reuse.
- [ ] TLS session resumption and replay-controlled 0-RTT.
- [ ] Broader QUIC/server cipher and identity profiles, SNI selection and mTLS.
- [ ] CONNECT/extended CONNECT, WebSockets, HTTP/3 datagrams and WebTransport.
- [ ] Server push and priority scheduling.
- [ ] Client redirect, cookie-jar and proxy configuration.
- [x] Apply peer HTTP/3 SETTINGS and field-section limits. Connection-owned
  control state configures QPACK and checks uncompressed field budgets before
  encoding request or response headers. Peer blocked-stream allowances accept
  the complete QUIC integer range; the encoder still uses its bounded local
  table and outstanding-section limits. `http3_peer_settings.e2e` covers
  defaults, exact/over-budget fields, full-range integers and fragmented
  settings. The dynamic-QPACK peer also exercises a full-range advertisement.
- [x] Negotiated QUIC application idle timers, authenticated activity resets,
  first ack-eliciting-send restart and the RTT-derived three-PTO floor.
- [ ] Typed protocol failures with correct stream and connection wire errors,
  retaining the current packet-number/key state across recoverable failures.
- [ ] Sustained native TLS connection churn without exhausting the fixed heap.
  - [x] The parent fails the isolated thirty-two-connection TLS churn peer with
    `Out of heap memory`. Linux x86-64 now recycles canonical String/Blob/Int
    buffers and fixed blocks through 8 KiB. The same peer completes sixty-four
    fresh connections in one serving process, with exact bodies and zero leaks.
    Cumulative allocation and live-alias E2E cases cover the ownership boundary
    (see [reference counting](../compiler/runtime/reference-counting.md#leak-checking)).
  - [x] Larger canonical buffers reuse an exact-size list. Repeated 70 KB and
    128 KB allocations and held aliases exceed the arena's cumulative capacity
    without exhaustion; all three cases fail before the large-buffer change.
  - [x] Sized raw release returns TCP/UDP polling, receive and address buffers,
    entropy reads and RSA scratch allocations to reusable storage on x86-64.
    Unaligned 4 KiB and 70 KB raw-buffer regressions exceed cumulative arena
    capacity, with small-size and live-register checks at the release boundary.
    The independent TLS peer also completes sixty-four fresh connections with
    70,001-byte echo bodies in one process and zero reported leaks.
  - [x] The remaining unsized standard-library raw releases are all 8-byte
    Stream cells, shutdown masks/state, single poll descriptors and socket options.
  - [ ] ARM64 needs the corresponding canonical/large-buffer recycling migration.
    Large-block splitting
    and coalescing remain unsupported.
    Passing bounded churn cases does not establish an indefinitely reusable
    native heap. The secure-server fairness peer retains its separate scopes.

HTTP/3 control owners now retain peer parameters and enforce
`SETTINGS_MAX_FIELD_SECTION_SIZE` against decoded name/value byte lengths,
including each field's 32-byte overhead, before sending request or response
headers when the peer setting has arrived. `http3_peer_settings.e2e` covers
exact boundaries, duplicate names, absent limits and zero limits. The aioquic
server peer checks zero-limit refusal and subsequent listener availability.
Other peer SETTINGS still need their respective engine features, so the full
SETTINGS item remains unchecked.

The dynamic QPACK checkpoint adds an inbound 4096-byte table, incremental
encoder instructions, relative/post-base references and Required Insert Count
wrap-around. Message owners retain blocked sections and defer body events until
the missing insertions arrive; decoder feedback retains unsent bytes under QUIC
flow control. The single-exchange profile advertises one blocked stream.
`qpack_dynamic_decoder.e2e` and `qpack_blocked_message.e2e` cover malformed
instructions, eviction, wrap-around, deferred DATA/FIN and exactly-once
acknowledgments. The independent ls-qpack peer verifies 560 sections, including
559 dynamic sections and 280 blocked-before-insertion checks. The delayed
aioquic peer verifies Unicode headers, bytewise encoder instructions, an explicit
section acknowledgment, a 70 KiB echo and authenticated close. The live peer's
observation window and decoder probe's per-process budget are 30 seconds;
engine deadlines are unchanged. A shared field-value
validator now permits UTF-8 obs-text while retaining control-byte rejection;
`http_header_value_octets.e2e` covers that boundary. The complete host suite
passes 12,051 tests and native aliases pass in a fresh build directory. Parent
benchmark verification remains blocked by an incompatible snapshot workload
contract; no performance result is claimed. These results describe the inbound
checkpoint, not later unvalidated changes.

The outgoing QPACK checkpoint adds a connection-owned encoder with a bounded
4 KiB local table, peer SETTINGS, decoder feedback, FIFO section acknowledgments
and stream cancellations. Entries remain pinned until their insertion is known
received and every outstanding field-section reference has been released.
Potentially blocked streams are counted distinctly; zero blocked-stream credit
permits speculative insertion but forbids references until acknowledgment.
Header owners reserve complete instruction batches against both stream and
connection credit, falling back to literals when credit or eviction safety
prevents insertion. Credentials and cookies remain never-indexed.
`qpack_dynamic_encoder.e2e` exercises those boundaries, and
`scripts/test_qpack_encoder_peer.py` adds a persistent independent ls-qpack
decoder probe with 560 sections, bytewise instructions/feedback, table eviction
and Required Insert Count wrap-around. The recovered pinned toolchain and native
build pass; focused QPACK tests pass 93/93 and HTTP/3 tests pass 132/132.
The independent outgoing probe passes 2,240 dynamic sections across 64-byte,
4 KiB and 8 KiB peer maxima, including 1,120 blocked-before-insertion cases and
compiled cleanup. The delayed aioquic peer
also passes, now requiring outgoing dynamic response headers as well as inbound
dynamic request headers, with Unicode and a 70 KiB echo. The broader HTTP/3
listener peer also passes routing, body limits, SETTINGS limits, idle timeout,
encrypted close, replay suppression, shutdown, rebind and cleanup checks.
The complete host suite passes 12,078/12,078 tests and native regression aliases
pass in a fresh sibling build directory. Parent benchmark verification was
rerun and remains blocked by the incompatible snapshot workload contract;
no performance result is claimed. Reusable multiplexed connection ownership
is still missing; the full QPACK item remains unchecked.

The larger-maximum test exposed a **test-peer** compatibility issue, not an
engine encoding failure. The published `pylsqpack==0.3.23` wheel changes its
wrap modulus when receiving a smaller table capacity; with an 8 KiB advertised
maximum and our 4 KiB table, it rejects section 504 (`ff0200` prefix). RFC 9204
section 4.5.1.1 bases the modulus on the advertised maximum, not current table
capacity. Upstream reverted the conflicting behavior in ls-qpack v2.6.3
([issue 44](https://github.com/litespeedtech/ls-qpack/issues/44)). The 8 KiB
check therefore uses the unchanged pylsqpack 0.3.23 wrapper at
`e2fef71d4280aa5adbc09c18c147e3cf07cf9f3c` with ls-qpack v2.7.0 at
`91567706c41c0d97ab8dc576873ecd472d7869fa`, built only as a test dependency.
No engine workaround or production C dependency was added. Reproduce in a
fresh external test-dependency directory, using the writable workspace from
the VM setup instructions:

```bash
git clone --depth 1 --branch 0.3.23 https://github.com/aiortc/pylsqpack.git "$workspace/qpack-peer-source"
git clone --depth 1 --branch v2.7.0 --recurse-submodules https://github.com/litespeedtech/ls-qpack.git "$workspace/qpack-peer-source/vendor/ls-qpack"
env CC=gcc python3 -m pip install --no-deps --no-build-isolation --target "$workspace/http-peers-fixed" "$workspace/qpack-peer-source"
PYTHONPATH="$workspace/http-peers-fixed:$workspace/http-peers" python3 scripts/test_qpack_encoder_peer.py --maximum=8192 --sections=1120
```

The default 560-section check and `--maximum=64` also pass with the original
published wheel. `--compiler` selects an already-built compiler when using a
fresh sibling Dune build directory.

HTTP/2 now retains an outgoing HPACK table separately from its decoder. Every
state transition preserves that owner and the peer's decoded field budget;
successful header writes return the updated table. Peer table-capacity changes
evict immediately and the next block signals the smallest intervening size
before the final size. `hpack_dynamic_encoder.e2e` and
`http2_outgoing_limits.e2e` exercise reuse, eviction, never-indexed sensitive
fields, disabled tables, duplicate-field accounting and resize ordering. The
independent `hpack` decoder checks seven consecutive blocks, and the existing
TLS/hyper-h2 server peer validates routing, flow control and owned shutdown.

QUIC application owners now enforce the minimum nonzero local/peer idle limit
before sending or processing packets, independently of the HTTP deadline.
Valid authenticated receives restart the timer; unauthenticated traffic and
ACK-only sends do not. Only the first ack-eliciting send after a receive
restarts it, including retransmitted Handshake packets. Retry backoff does not
inflate the RTT-derived three-PTO floor. `quic_idle.e2e` covers negotiation,
timer boundaries and restart rules. The aioquic server peer advertises a short
limit, stalls an upload and verifies that completing it after expiry produces
no response; normal requests afterward still succeed. Handshake deadlines
remain bounded separately by the existing handshake owner.

Focused additions use `network_poll.e2e`, `http_header_compression.e2e` and
`http_server_ipv6.e2e`; independent readiness, header-compression and IPv6 peer
scripts exercise production code and compiled leak accounting. The readiness
work exposed an x64 syscall-argument capture bug when a later operand was held
in the scratch register. Capture now preserves that operand before loading
earlier arguments.

Plaintext HTTP/2 now uses the same persistent stream owners as TLS HTTP/2.
Independent hyper-h2 peers verify zero-window streams, selective response
credit, reset isolation, shared compression across later requests, and HTTP/1
admission while an HTTP/2 response is blocked. A maximum DATA frame spans
bounded native writes without duplicating its payload or flow debit. The
focused HTTP/2 server suite passes 65/65 cases.

Retained QUIC handshake steps accept listener-routed datagrams and enforce
absolute deadlines without receiving from the shared socket. Five new cases
check waiting-state retention, foreign-peer isolation, expiry and one-packet
flight progress; the focused QUIC server suite passes 8/8 cases. HTTP/3 now
multiplexes requests within an admitted connection, but still uses the
sequential UDP admission wrapper and compatibility datagram sends. Active UDP
ownership remains unfinished.

At runtime checkpoint `a65761e34c77e6d0a78614137ac5a326977dd8a2`, a fresh
build, all 12,248 host tests and native regression aliases pass. Independent
plaintext/TLS HTTP/2, HTTP/3, combined secure-listener and IPv6 peers pass.
One TLS peer run timed out during shutdown; a full rerun passed. Parent
performance verification remains blocked by the stored snapshot's incompatible
workload contract digest. No baseline reset or performance pass is claimed.

HTTP/3 servers now retain independent request and response owners on one
connection. The public serving signatures are unchanged. Independent aioquic
validation covers a 70 KiB response alongside a stalled request, selective
response credit, reset isolation and forty requests on the same connection.
Completed stream directions renew MAX_STREAMS; the 1,024-request history bound
sends one flow-controlled GOAWAY and drains eligible responses. Thirteen focused
cases cover those boundaries, message-error isolation, request timeout, response
semantics and invalid Huffman padding at connection scope. The compression
regression fails against the pre-fix build and passes against the fixed build.

Runtime checkpoint `f2056bbaddc80b1d9308cc902603209d0b87dba0` passes all
12,261 host tests and native regression aliases. The HTTP/3, combined secure
listener and IPv6 peer checks pass. Two initial full runs exposed three native
execution tests treating interrupted waits as failures; their shared test-only
helper now retries EINTR while retaining the absolute timeout and reaping the
child. Its focused native execution group passes 22/22. Parent performance
verification was rerun and remains blocked by the incompatible snapshot
workload contract. No baseline was reset. UDP connection admission and datagram
output remain sequential/blocking; active QUIC listener fairness is still
unfinished.
