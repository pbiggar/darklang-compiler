#!/usr/bin/env python3
"""Verify the Dark TLS listener with OpenSSL HTTP/1.1 and independent hyper-h2 clients."""

import argparse
import datetime
import select
import signal
import socket
import ssl
import subprocess
import tempfile
import time
from pathlib import Path

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID
from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import DataReceived, ResponseReceived, StreamEnded
from h2.settings import SettingCodes

ROOT = Path(__file__).resolve().parents[1]
SERVER = '''// server.dark - TLS HTTP routing, bounded bodies and shutdown ownership.
let handler (request: Stdlib.Http.Request) : Stdlib.Http.Response =
  if Stdlib.Bool.not (Stdlib.String.startsWith request.url "https://localhost") then Stdlib.Http.responseWithText "wrong scheme" 500
  else Stdlib.Http.Response { statusCode = 200, headers = [("set-cookie", "a=1"), ("set-cookie", "b=2")], body = if Stdlib.String.endsWith request.url "/big" then Stdlib.String.toBlob (Stdlib.String.join (Stdlib.List.repeatUnsafe 70 (Stdlib.String.repeat "b" 1000)) "") else request.body }
let run () : Unit =
  let port = Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault "0" |> Stdlib.Int.parse |> Stdlib.Result.withDefault 0 in
  let imported = Stdlib.Blob.fromHex "@CERT@" |> Stdlib.Result.andThen (fun certificate ->
    Stdlib.Blob.fromHex "@KEY@" |> Stdlib.Result.andThen (fun key -> Stdlib.TlsServerIdentity.create certificate key)) in
  match imported with
  | Error message -> Stdlib.printLine message
  | Ok identity ->
    let config = Stdlib.HttpServer.Config.Config { port = port, maxBodyBytes = 80000, injectStandardHeaders = false, canonicalizeFromForwardedProto = false, logRequests = false } in
    match Stdlib.HttpServer.Tls.serve config identity handler (fun _unit -> Stdlib.printLine "LISTENING") with
    | Error message -> Stdlib.printLine message
    | Ok () -> Stdlib.printLine "STOPPED"
run ()
'''


def start(binary):
    with socket.socket() as reservation:
        reservation.bind(("127.0.0.1", 0))
        port = reservation.getsockname()[1]
    process = subprocess.Popen([str(binary), str(port)], cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    assert select.select([process.stdout], [], [], 10)[0], "No listening callback"
    assert process.stdout.readline() == b"LISTENING\n"
    return process, port


def stop(process):
    process.send_signal(signal.SIGTERM)
    stdout, stderr = process.communicate(timeout=12)
    assert process.returncode == 0 and stdout == b"STOPPED\n" and not stderr, (process.returncode, stdout, stderr)


def connect(port, certificate, protocols, host="127.0.0.1"):
    context = ssl.create_default_context(cadata=certificate.decode())
    context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
    context.set_ecdh_curve("X25519")
    context.set_alpn_protocols(protocols)
    return context.wrap_socket(socket.create_connection((host, port), timeout=15), server_hostname="localhost")


def http1_response(reader, head=False):
    status = reader.readline()
    assert status.startswith(b"HTTP/1.1 200"), status
    headers = []
    while (line := reader.readline()) != b"\r\n":
        assert line, "Truncated response headers"
        name, value = line.rstrip(b"\r\n").split(b":", 1)
        headers.append((name.lower(), value.strip()))
    length = next(int(value) for name, value in headers if name == b"content-length")
    body = reader.read(0 if head else length)
    assert head or len(body) == length, "Truncated TLS response body"
    return headers, body


def http1_owners(port, certificate):
    with connect(port, certificate, ["http/1.1"]) as peer, peer.makefile("rb") as reader:
        for method, body in (("POST", b"first"), ("HEAD", b""), ("POST", b"second")):
            peer.sendall(f"{method} /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: {len(body)}\r\n\r\n".encode() + body)
            headers, received = http1_response(reader, head=method == "HEAD")
            assert received == body and (b"connection", b"close") not in headers
        peer.sendall(b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 1\r\n\r\na"
                     b"POST /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\nContent-Length: 1\r\n\r\nb")
        assert http1_response(reader)[1] == b"a"
        assert http1_response(reader)[1] == b"b"
        assert not reader.read(1)

    def other_client():
        started = time.monotonic()
        with connect(port, certificate, ["http/1.1"]) as peer, peer.makefile("rb") as reader:
            peer.sendall(b"GET /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
            assert http1_response(reader)[1] == b""
        assert time.monotonic() - started < 2, "A stalled TLS owner monopolized the listener"

    with socket.create_connection(("127.0.0.1", port), timeout=5) as stalled:
        stalled.sendall(b"\x16\x03\x03\x00")
        other_client()
    stalled, _finished = unfinished(port, certificate)
    try:
        other_client()
    finally:
        stalled.close()
    with connect(port, certificate, ["http/1.1"]) as stalled, stalled.makefile("rb") as reader:
        stalled.sendall(b"POST /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\nContent-Length: 4\r\n\r\na")
        other_client()
        stalled.sendall(b"bcd")
        assert http1_response(reader)[1] == b"abcd"
    with connect(port, certificate, ["http/1.1"]) as stalled:
        stalled.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4096)
        stalled.sendall(b"GET /big HTTP/1.1\r\nHost: localhost\r\n\r\n" * 70)
        time.sleep(0.5)
        other_client()


def http2_owners(port, certificate):
    with connect(port, certificate, ["h2"]) as peer:
        peer.settimeout(5)
        h2 = H2Connection(H2Configuration(client_side=True, header_encoding="utf-8"))
        h2.initiate_connection()
        h2.update_settings({SettingCodes.INITIAL_WINDOW_SIZE: 0})
        for stream in (1, 3):
            h2.send_headers(stream, [(":method", "GET"), (":scheme", "https"),
                                    (":authority", f"localhost:{port}"), (":path", "/big"),
                                    ("x-shared", "shared-request-compression")], end_stream=True)
        peer.sendall(h2.data_to_send())
        headers, bodies, ended = {}, {1: bytearray(), 3: bytearray()}, set()

        def receive():
            wire = peer.recv(65536)
            assert wire, "Persistent HTTP/2 owner closed prematurely"
            events = h2.receive_data(wire)
            completed = {event.stream_id for event in events if isinstance(event, StreamEnded)}
            for event in events:
                if isinstance(event, ResponseReceived):
                    headers[event.stream_id] = event.headers
                elif isinstance(event, DataReceived):
                    bodies[event.stream_id].extend(event.data)
                    h2.increment_flow_control_window(event.flow_controlled_length)
                    if event.stream_id not in completed:
                        h2.increment_flow_control_window(event.flow_controlled_length, event.stream_id)
                elif isinstance(event, StreamEnded):
                    ended.add(event.stream_id)
            outgoing = h2.data_to_send()
            if outgoing:
                peer.sendall(outgoing)

        while set(headers) != {1, 3}:
            receive()
        assert not bodies[1] and not bodies[3], "Zero stream credit leaked DATA"
        started = time.monotonic()
        with connect(port, certificate, ["http/1.1"]) as other, other.makefile("rb") as reader:
            other.sendall(b"GET /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
            assert http1_response(reader)[1] == b""
        assert time.monotonic() - started < 2, "Blocked HTTP/2 stream monopolized TLS admission"

        # Only stream 3 receives credit: stream 1 must remain blocked.
        h2.increment_flow_control_window(65535, 3)
        peer.sendall(h2.data_to_send())
        while 3 not in ended:
            receive()
        assert bodies[3] == b"b" * 70000 and not bodies[1]
        assert [value for name, value in headers[3] if name == "set-cookie"] == ["a=1", "b=2"]

        # Cancel the blocked response and reuse the same compression/connection state.
        h2.reset_stream(1)
        bodies[5] = bytearray()
        h2.send_headers(5, [(":method", "GET"), (":scheme", "https"),
                           (":authority", f"localhost:{port}"), (":path", "/big"),
                           ("x-shared", "shared-request-compression")], end_stream=True)
        h2.increment_flow_control_window(65535, 5)
        peer.sendall(h2.data_to_send())
        while 5 not in ended:
            receive()
        assert bodies[5] == b"b" * 70000


def unfinished(port, certificate):
    context = ssl.create_default_context(cadata=certificate.decode())
    context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
    context.set_ecdh_curve("X25519")
    context.set_alpn_protocols(["h2"])
    incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
    client = context.wrap_bio(incoming, outgoing, server_side=False, server_hostname="localhost")
    peer = socket.create_connection(("127.0.0.1", port), timeout=15)
    try:
        client.do_handshake()
    except ssl.SSLWantReadError:
        pass
    hello = outgoing.read()
    # Split the record header as well as the handshake, crossing the accepted
    # socket's short receive timeout without ending the handshake deadline.
    peer.sendall(hello[:3])
    assert not select.select([peer], [], [], 0.2)[0]
    peer.sendall(hello[3:])
    while True:
        data = peer.recv(65536)
        assert data, "Server truncated handshake"
        incoming.write(data)
        try:
            client.do_handshake()
            break
        except ssl.SSLWantReadError:
            pass
    return peer, outgoing.read()


def h2_exchange(peer, port, method, total):
    assert peer.selected_alpn_protocol() == "h2"
    h2 = H2Connection(H2Configuration(client_side=True, header_encoding="utf-8"))
    h2.initiate_connection()
    h2.send_headers(1, [(":method", method), (":scheme", "https"), (":authority", f"localhost:{port}"),
                        (":path", "/echo"), ("content-length", str(total))], end_stream=total == 0)
    wire = h2.data_to_send()
    peer.sendall(wire[:8])
    peer.sendall(wire[8:])
    offset, response, ended, headers = 0, bytearray(), False, []
    while not ended:
        if offset < total:
            size = min(total - offset, h2.local_flow_control_window(1), 16384)
            if size:
                h2.send_data(1, b"x" * size, end_stream=offset + size == total)
                offset += size
        output = h2.data_to_send()
        if output:
            peer.sendall(output)
        data = peer.recv(65536)
        assert data, ("TLS HTTP/2 server truncated response", method, total, len(response), headers)
        for event in h2.receive_data(data):
            if isinstance(event, ResponseReceived):
                headers = event.headers
            elif isinstance(event, DataReceived):
                response.extend(event.data)
                h2.acknowledge_received_data(event.flow_controlled_length, 1)
            elif isinstance(event, StreamEnded):
                ended = True
    assert dict(headers)[":status"] == ("413" if total > 80000 else "200"), headers
    if total <= 80000:
        assert response == b"x" * total
        assert headers.count(("set-cookie", "a=1")) == headers.count(("set-cookie", "b=2")) == 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    parser.add_argument("--skip-owner-checks", action="store_true", help="Isolate the legacy HTTP/2 and handshake compatibility cases")
    parser.add_argument("--churn", type=int, default=0, help="Run only this many fresh TLS connections against one serving process")
    args = parser.parse_args()
    if args.churn < 0:
        parser.error("--churn must be nonnegative")
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    now = datetime.datetime.now(datetime.timezone.utc)
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    certificate = (x509.CertificateBuilder().subject_name(name).issuer_name(name)
                   .public_key(key.public_key()).serial_number(1)
                   .not_valid_before(now - datetime.timedelta(minutes=1)).not_valid_after(now + datetime.timedelta(days=1))
                   .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
                   .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), critical=False)
                   .add_extension(x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]), critical=False)
                   .sign(key, hashes.SHA256())).public_bytes(serialization.Encoding.PEM)
    pem = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    with tempfile.TemporaryDirectory(prefix="dark-server-tls-") as temporary:
        source, binary = Path(temporary) / "server.dark", Path(temporary) / "server"
        source.write_text(SERVER.replace("@CERT@", certificate.hex()).replace("@KEY@", pem.hex()))
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT,
                                  text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        process, port = start(binary)
        try:
            if args.churn:
                for index in range(args.churn):
                    body = bytes([index % 251]) * 128
                    with connect(port, certificate, ["http/1.1"]) as peer, peer.makefile("rb") as reader:
                        peer.sendall(b"POST /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\nContent-Length: 128\r\n\r\n" + body)
                        assert http1_response(reader)[1] == body, index
                        assert not reader.read(1), index
                stop(process)
                print(f"TLS server: {args.churn} fresh connections on one process, exact bodies and zero leaks", flush=True)
                return
            if not args.skip_owner_checks:
                http1_owners(port, certificate)
                http2_owners(port, certificate)
            for method, total in (("POST", 70000), ("HEAD", 0), ("POST", 80001)):
                with connect(port, certificate, ["h2", "http/1.1"]) as peer:
                    h2_exchange(peer, port, method, total)
            for protocols in (["http/1.1"], []):
                with connect(port, certificate, protocols) as peer:
                    assert peer.selected_alpn_protocol() == (protocols[0] if protocols else None)
                    peer.sendall(f"POST /echo HTTP/1.1\r\nHost: localhost:{port}\r\nConnection: close\r\nContent-Length: 8192\r\nExpect: 100-continue\r\n\r\n".encode())
                    assert peer.recv(4096) == b"HTTP/1.1 100 Continue\r\n\r\n"
                    peer.sendall(b"x" * 8192)
                    response = bytearray()
                    while chunk := peer.recv(65536):
                        response.extend(chunk)
                    head, body = response.split(b"\r\n\r\n", 1)
                    assert head.startswith(b"HTTP/1.1 200") and body == b"x" * 8192, head
            # Unsupported ALPN fails without terminating the listener.
            try:
                with connect(port, certificate, ["unsupported"]):
                    raise AssertionError("Unsupported ALPN accepted")
            except ssl.SSLError:
                pass
            peer, finished = unfinished(port, certificate)
            try:
                changed = finished[:-1] + bytes([finished[-1] ^ 1])
                peer.sendall(changed)
                try:
                    assert not peer.recv(65536), "Unauthenticated client received application data"
                except ConnectionResetError:
                    pass
            finally:
                peer.close()
            with connect(port, certificate, ["h2"]) as peer:
                h2_exchange(peer, port, "HEAD", 0)
            stop(process)
        except BaseException as error:
            running = process.poll() is None
            if running:
                process.kill()
            stdout, stderr = process.communicate()
            raise AssertionError(f"TLS server peer failed; running_before_cleanup={running}, exit={process.returncode}, stdout={stdout!r}, stderr={stderr!r}") from error
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()
        # Shutdown must interrupt both incomplete TLS and encrypted HTTP reads.
        for mode in ("hello", "finished", "http1", "http2"):
            process, port = start(binary)
            try:
                if mode == "hello":
                    peer = socket.create_connection(("127.0.0.1", port), timeout=15)
                    peer.sendall(b"\x16\x03\x03\x00")
                elif mode == "finished":
                    peer, _finished = unfinished(port, certificate)
                else:
                    peer = connect(port, certificate, ["h2"] if mode == "http2" else ["http/1.1"])
                    if mode == "http1":
                        peer.sendall(b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 1\r\n\r\n")
                    else:
                        h2 = H2Connection(H2Configuration(client_side=True))
                        h2.initiate_connection()
                        h2.send_headers(1, [(b":method", b"POST"), (b":scheme", b"https"), (b":authority", b"localhost"), (b":path", b"/echo"), (b"content-length", b"1")])
                        peer.sendall(h2.data_to_send())
                        assert peer.recv(65536)
                try:
                    stop(process)
                finally:
                    peer.close()
            finally:
                if process.poll() is None:
                    process.kill()
                    process.communicate()
    print("TLS server verified: persistent/pipelined HTTP/1, multiplexed persistent HTTP/2 with independent windows and reset isolation, stalled-client fairness, 70 KiB flow control, HEAD, early 413, no-ALPN, Expect, ALPN rejection, forged Finished, shutdown and zero leaks")


if __name__ == "__main__":
    main()
