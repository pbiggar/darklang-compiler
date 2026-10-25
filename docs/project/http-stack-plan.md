# Remaining native HTTP work

The implemented protocols, resource limits, ownership boundaries and peer tests
are documented in [HTTP compatibility](../compatibility/stdlib/html-and-http.md).
HTTP protocol and policy belong in Dark; the native boundary supplies OS
transport, time and entropy. Preserve the public interpreter APIs and avoid
shell programs or a host HTTP implementation.

## Remaining engine work

- [ ] Finish client TCP/TLS connection establishment with incremental active
  owners, independent deadlines and fair admission. Server TCP/UDP owners
  already share readiness servicing; do not reintroduce the retired sequential
  listener design.
- [ ] Complete reuse/pooling beyond the existing bounded session profiles.
  HTTP/2 lazy bodies exclusively check out their connection until consumed or
  closed; concurrent lazy bodies still need multiplexed scheduling.
- [ ] Complete lazy-stream request multiplexing and scheduling. Buffered
  `Session.requestMany` already multiplexes HTTP/2–3; origins are serviced
  sequentially and HTTP/1.1 exchanges remain sequential.
- [ ] Complete QPACK cancellation and table ownership across reusable multiplexed
  connections. Wholly queued sections release references on cancellation;
  any written byte pins them until decoder feedback. Encoder insertions remain
  committed. Abandoned lazy bodies currently close their leased connection;
  general cancellation and recoverable failure ownership remain.
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
- [ ] Finish platform certificate trust parity with the interpreter. Linux
  currently discovers PEM bundles and certificate directories at execution
  time, including `SSL_CERT_FILE` and `SSL_CERT_DIR`. Certificate path search
  accepts unordered intermediates with a shared 128-attempt budget and an
  eight-intermediate depth limit. Remaining work includes macOS native
  `SecTrust` evaluation (and the framework-call/linker support it requires),
  .NET Linux user certificate stores in PKCS#12 format, and platform
  differential coverage. PEM discovery and the custom verifier do not provide
  full .NET trust parity. Keep HTTP policy/configuration work separate.
- [ ] Finish typed protocol failures for ACK/recovery, CRYPTO/reassembly,
  connection IDs, key validation and handshake owners. Preserve the latest
  authenticated packet/key state and triggering frame type when producing wire
  errors. HTTP/3 message/control/QPACK and QUIC stream/final-size/flow-control
  failures already carry typed scope and codes. Local parser resource limits
  remain opaque rather than guessed into peer encoding errors.
- [ ] Complete sustained TLS churn support. ARM64 still needs canonical and
  large-buffer recycling corresponding to x86-64; splitting and coalescing
  large blocks remain unsupported. Bounded churn passing does not establish an
  indefinitely reusable heap. Preserve alias safety and complete raw-buffer
  release through polling, entropy and RSA paths.

## Acceptance boundaries

A remaining item is complete only when its production owner, cleanup paths,
resource limits and independent peer coverage are in place. Preserve partial
writes without resealing encrypted records or reusing nonces, shared-socket
ownership, per-connection deadlines, stream isolation, no automatic replay of
attempted requests, and session generation invalidation. Retain independent
checks of TLS authentication, QUIC packet protection, compression, fairness,
shutdown/rebind and leak accounting.

Follow the [verification policy](../contributing/verification.md), measuring
performance against the task parent on a supported environment. A protocol
fixture passing does not establish interoperability or coverage of a different
native target.

`python3 scripts/test_x509_paths.py` generates ephemeral certificate fixtures,
checks their expected outcomes with OpenSSL, and runs the native verifier for
chain selection, certificate rejection, bundle loading and directory loading.
It requires Python `cryptography` and OpenSSL. Use `--compiler /path/to/dark`
to select a fresh build, `--paths-only` to compare a compiler without the new
source loader, or `--oracle-only` to check fixture expectations independently.
This probe does not establish macOS or .NET user-store parity.
