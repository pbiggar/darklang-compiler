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
from pathlib import Path

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID
from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import DataReceived, ResponseReceived, StreamEnded

ROOT = Path(__file__).resolve().parents[1]
SERVER = '''// server.dark - TLS HTTP routing, bounded bodies and shutdown ownership.
let handler (request: Stdlib.Http.Request) : Stdlib.Http.Response =
  if Stdlib.Bool.not (Stdlib.String.startsWith request.url "https://localhost") then Stdlib.Http.responseWithText "wrong scheme" 500
  else Stdlib.Http.Response { statusCode = 200, headers = [("set-cookie", "a=1"), ("set-cookie", "b=2")], body = request.body }
let run () : Unit =
  let port = Stdlib.Cli.Args.get 0 |> Stdlib.Result.withDefault "0" |> Stdlib.Int.parse |> Stdlib.Result.withDefault 0 in
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


def connect(port, certificate, protocols):
    context = ssl.create_default_context(cadata=certificate.decode())
    context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
    context.set_ecdh_curve("X25519")
    context.set_alpn_protocols(protocols)
    return context.wrap_socket(socket.create_connection(("127.0.0.1", port), timeout=15), server_hostname="localhost")


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
    args = parser.parse_args()
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
        compiled = subprocess.run([str(args.compiler), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT,
                                  text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        process, port = start(binary)
        try:
            for method, total in (("POST", 70000), ("HEAD", 0), ("POST", 80001)):
                with connect(port, certificate, ["h2", "http/1.1"]) as peer:
                    h2_exchange(peer, port, method, total)
            for protocols in (["http/1.1"], []):
                with connect(port, certificate, protocols) as peer:
                    assert peer.selected_alpn_protocol() == (protocols[0] if protocols else None)
                    peer.sendall(f"POST /echo HTTP/1.1\r\nHost: localhost:{port}\r\nContent-Length: 8192\r\nExpect: 100-continue\r\n\r\n".encode())
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
    print("TLS HTTP/2 server verified: 70 KiB flow control, HEAD, early 413, HTTP/1.1/no-ALPN, Expect, ALPN rejection, fragmented hello, forged Finished, stalled shutdown and zero leaks")


if __name__ == "__main__":
    main()
