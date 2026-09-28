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
numbers and socket constants in `Platform.fs`; exercise each target on its
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

## 4. HTTPS

TLS is not an operating-system syscall. An entirely Dark HTTPS client requires
TLS record framing, handshake, authenticated encryption, key exchange, secure
randomness, certificate-chain and hostname verification, and trust-store
loading. The client implements TLS 1.3 X25519 with AES-128-GCM/SHA-256 and
AES-256-GCM/SHA-384 profiles
with RSA-PSS server authentication, RSA-signed X.509 chains, hostname checks,
and system CA bundles. Unsupported cipher, key, certificate, and protocol
choices fail closed. Local TLS peers and protocol vectors exercise the profile;
server-side TLS can follow the client.

## 5. Compatibility and readiness

Compare observable results with the interpreter's HTTP fixtures using local
servers, plus malformed-wire and resource-lifecycle tests. Add stress and
performance measurements for buffered and streaming bodies. HTTP/1.1 is the
first interoperability target; HTTP/2 and HTTP/3 require separate protocol
work. For each implementation branch, run `./build --ai`, the already-built
`./run-tests --ai`, and
`./benchmarks/run_benchmarks.sh --verify-parent full` before merge-train
handoff.
