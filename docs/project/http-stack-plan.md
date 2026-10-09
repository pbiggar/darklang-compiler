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
