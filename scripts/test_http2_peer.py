#!/usr/bin/env python3
"""HTTP/2 client/server interoperability using an independent hyper-h2 peer.

Test-only dependency: h2==4.3.0. Guest protocols remain entirely Dark.
"""

import argparse
import select
import signal
import socket
import ssl
import subprocess
import tempfile
import threading
from pathlib import Path

from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import DataReceived, RequestReceived, ResponseReceived, StreamEnded

ROOT = Path(__file__).resolve().parents[1]

CLIENT = '''// client.dark - Authenticated HTTP/2 buffering, streaming and ownership probe.
match Stdlib.Cli.Args.get 0, Stdlib.Cli.Args.get 1, Stdlib.Cli.Args.get 2 with
| Ok port, Ok ca, Ok mode ->
  match Stdlib.Cli.FileSystem.readFile ca with
  | Error _ -> Stdlib.printLine "CA read failed"
  | Ok pem ->
    match Stdlib.__Tls13Client.parsePemBundle pem with
    | Error message -> Stdlib.printLine message
    | Ok roots ->
      let url = "https://localhost:" ++ port ++ "/" ++ mode in
      let method = if mode == "head" then "HEAD" else "POST" in
      let requestBody = Stdlib.String.toBlob (Stdlib.String.join (Stdlib.List.repeatUnsafe 70 (Stdlib.String.repeat "x" 1000)) "") in
      if mode == "buffered" || mode == "early-buffered" then
        match Stdlib.HttpClient.__requestTrustedWithRoots roots method url [] requestBody with
        | Error _ -> Stdlib.printLine "REQUEST ERROR"
        | Ok response -> Stdlib.printLine (Stdlib.Int.toString response.statusCode ++ "|" ++ Stdlib.Int.toString (Stdlib.Blob.length response.body) ++ "|" ++ Stdlib.Int.toString (Stdlib.List.length response.headers))
      else
        match Stdlib.HttpClient.__streamTrustedWithRoots roots method url [] requestBody with
        | Error _ -> Stdlib.printLine "REQUEST ERROR"
        | Ok response ->
          let bytes = if mode == "abandon" then Stdlib.Blob.empty else Stdlib.Stream.toBlob response.body in
          let _ = Stdlib.Stream.close response.body in
          Stdlib.printLine (Stdlib.Int.toString response.statusCode ++ "|" ++ Stdlib.Int.toString (Stdlib.Blob.length bytes) ++ "|" ++ Stdlib.Int.toString (Stdlib.List.length response.headers))
| _, _, _ -> Stdlib.printLine "Invalid arguments"
'''

SERVER = '''// server.dark - HTTP/1.1 and prior-knowledge HTTP/2 share routing and ownership.
let handler (req: Stdlib.Http.Request) : Stdlib.Http.Response =
  Stdlib.Http.responseWithHeaders req.body [("set-cookie", "a=1"), ("set-cookie", "b=2")] 200

match Stdlib.Cli.Args.int64 0 with
| Error _ -> Stdlib.printLine "Invalid port"
| Ok port ->
  let config = Stdlib.HttpServer.Config.Config { port = Stdlib.Int.fromInt64 port,
    maxBodyBytes = 80000, injectStandardHeaders = true,
    canonicalizeFromForwardedProto = true, logRequests = false } in
  match Stdlib.HttpServer.serve config handler (fun _ -> Stdlib.printLine "LISTENING") with
  | Error message -> Stdlib.printLine message
  | Ok () -> Stdlib.printLine "STOPPED"
'''


def compile_source(directory, name, source, compiler):
    path, binary = directory / f"{name}.dark", directory / name
    path.write_text(source)
    result = subprocess.run([str(compiler), str(path), "--leak-check", "-o", str(binary)],
                            cwd=ROOT, text=True, capture_output=True, timeout=120)
    assert result.returncode == 0, result.stdout + result.stderr
    return binary


def certificates(directory):
    key, cert, csr = directory / "key.pem", directory / "cert.pem", directory / "leaf.csr"
    ca_key, ca = directory / "ca.key", directory / "ca.pem"
    extensions = directory / "leaf.ext"
    extensions.write_text("subjectAltName=DNS:localhost\nextendedKeyUsage=serverAuth\nbasicConstraints=critical,CA:FALSE\n")
    commands = [
        ["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-keyout", str(ca_key), "-out", str(ca), "-days", "1", "-subj", "/CN=HTTP2 CA", "-addext", "basicConstraints=critical,CA:TRUE"],
        ["openssl", "req", "-newkey", "rsa:2048", "-nodes", "-keyout", str(key), "-out", str(csr), "-subj", "/CN=localhost"],
        ["openssl", "x509", "-req", "-in", str(csr), "-CA", str(ca), "-CAkey", str(ca_key), "-CAcreateserial", "-out", str(cert), "-days", "1", "-extfile", str(extensions)],
    ]
    for command in commands:
        subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return key, cert, ca


def tls_peer(listener, context, mode, failures):
    try:
        raw, _ = listener.accept()
        with context.wrap_socket(raw, server_side=True) as peer:
            peer.settimeout(10)
            assert peer.selected_alpn_protocol() == "h2"
            h2 = H2Connection(H2Configuration(client_side=False, header_encoding="utf-8"))
            h2.initiate_connection()
            peer.sendall(h2.data_to_send())
            request_bytes, stream_id = 0, None
            complete = False
            while not complete:
                data = peer.recv(65536)
                assert data, "Client closed before completing upload"
                for event in h2.receive_data(data):
                    if isinstance(event, RequestReceived):
                        stream_id = event.stream_id
                        assert dict(event.headers)[":path"] == "/" + mode
                        if mode in ("early-buffered", "early-stream"):
                            # Deliberately withhold upload credit. The guest
                            # must return this final response before finishing
                            # its upload, not wait for another WINDOW_UPDATE.
                            h2.send_headers(stream_id, [(":status", "103"), ("x-early-context", "preserved")])
                            h2.send_headers(stream_id, [(":status", "413"), ("content-length", "5"),
                                                        ("x-early-context", "preserved")])
                            h2.send_data(stream_id, b"early", end_stream=True)
                            peer.sendall(h2.data_to_send())
                            while peer.recv(65536):
                                pass
                            return
                    elif isinstance(event, DataReceived):
                        request_bytes += len(event.data)
                        h2.acknowledge_received_data(event.flow_controlled_length, event.stream_id)
                    elif isinstance(event, StreamEnded):
                        complete = True
                output = h2.data_to_send()
                if output:
                    peer.sendall(output)
            assert request_bytes == 70000
            h2.ping(b"12345678")
            h2.send_headers(stream_id, [(":status", "103"), ("link", "</a>")])
            h2.send_headers(stream_id, [(":status", "200"), ("content-length", "70000"),
                                       ("set-cookie", "a=1"), ("set-cookie", "b=2")],
                            end_stream=mode == "head")
            peer.sendall(h2.data_to_send())
            if mode in ("head", "abandon"):
                while peer.recv(65536):
                    pass
                return
            offset, total = 0, (7 if mode == "truncated" else 70000)
            while offset < total:
                size = min(total - offset, h2.local_flow_control_window(stream_id), 16384)
                if size == 0:
                    data = peer.recv(65536)
                    assert data, "Client closed before completing response"
                    h2.receive_data(data)
                else:
                    h2.send_data(stream_id, b"y" * size)
                    offset += size
                output = h2.data_to_send()
                if output:
                    peer.sendall(output)
            if mode != "truncated":
                h2.send_headers(stream_id, [("x-trailer", "done")], end_stream=True)
                peer.sendall(h2.data_to_send())
                while peer.recv(65536):
                    pass
    except BaseException as error:
        failures.append(error)


def client_checks(directory, modes=("buffered", "stream", "head", "abandon", "truncated", "early-buffered", "early-stream"), compiler=ROOT / "dark"):
    binary = compile_source(directory, "client", CLIENT, compiler)
    key, cert, ca = certificates(directory)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_3
    context.load_cert_chain(cert, key)
    context.set_alpn_protocols(["h2", "http/1.1"])
    for mode in modes:
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            listener.listen(1)
            failures = []
            peer = threading.Thread(target=tls_peer, args=(listener, context, mode, failures), daemon=True)
            peer.start()
            result = subprocess.run([str(binary), str(listener.getsockname()[1]), str(ca), mode],
                                    text=True, capture_output=True, cwd=ROOT, timeout=45)
            peer.join(timeout=12)
            assert not peer.is_alive(), ("TLS peer did not finish", mode, result.returncode, result.stdout, result.stderr)
            assert not failures, (mode, failures, result.returncode, result.stdout, result.stderr)
            if mode == "truncated":
                assert result.returncode != 0 and any(message in result.stderr for message in
                    ("TLS peer closed during record", "TLS application write failed", "TLS socket read failed")), result
            else:
                expected = "413|5|2\n" if mode.startswith("early-") else (
                    "200|0|3\n" if mode in ("head", "abandon") else "200|70000|3\n")
                assert result.returncode == 0 and result.stdout == expected and not result.stderr, result


def server_checks(directory, compiler=ROOT / "dark"):
    binary = compile_source(directory, "server", SERVER, compiler)
    with socket.socket() as reservation:
        reservation.bind(("127.0.0.1", 0))
        port = reservation.getsockname()[1]
    process = subprocess.Popen([str(binary), str(port)], cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        assert select.select([process.stdout], [], [], 10)[0], "No listening callback"
        assert process.stdout.readline() == b"LISTENING\n"
        for method, total in (("POST", 70000), ("HEAD", 0), ("POST", 80001)):
            with socket.create_connection(("127.0.0.1", port), timeout=15) as peer:
                h2 = H2Connection(H2Configuration(client_side=True, header_encoding="utf-8"))
                h2.initiate_connection()
                h2.send_headers(1, [(":method", method), (":scheme", "http"), (":authority", f"localhost:{port}"),
                                    (":path", "/echo"), ("content-length", str(total))], end_stream=total == 0)
                wire = h2.data_to_send()
                # Fragment the magic string so dispatch cannot rely on one TCP read.
                peer.sendall(wire[:8])
                peer.sendall(wire[8:])
                offset, response, ended, status, headers = 0, bytearray(), False, None, []
                while not ended:
                    if offset < total:
                        size = min(total - offset, h2.local_flow_control_window(1), 16384)
                        if size:
                            h2.send_data(1, b"x" * size, end_stream=offset + size == total)
                            offset += size
                    output = h2.data_to_send()
                    if output:
                        try:
                            peer.sendall(output)
                        except ConnectionResetError as error:
                            raise AssertionError((method, total, offset, status, bytes(response))) from error
                    data = peer.recv(65536)
                    assert data, "Server truncated response"
                    for event in h2.receive_data(data):
                        if isinstance(event, ResponseReceived):
                            headers = event.headers
                            status = dict(headers)[":status"]
                        elif isinstance(event, DataReceived):
                            response.extend(event.data)
                            h2.acknowledge_received_data(event.flow_controlled_length, 1)
                        elif isinstance(event, StreamEnded):
                            ended = True
                assert status == ("413" if total > 80000 else "200"), headers
                if total <= 80000:
                    assert bytes(response) == b"x" * total
                    assert headers.count(("set-cookie", "a=1")) == 1 and headers.count(("set-cookie", "b=2")) == 1
        # Signal during a stalled HTTP/2 body read, not just an idle listener.
        # Consumed shutdown signals must propagate to the serving loop.
        with socket.create_connection(("127.0.0.1", port), timeout=15) as stalled:
            h2 = H2Connection(H2Configuration(client_side=True))
            h2.initiate_connection()
            h2.send_headers(1, [(b":method", b"POST"), (b":scheme", b"http"),
                                (b":authority", b"localhost"), (b":path", b"/stalled"), (b"content-length", b"1")])
            stalled.sendall(h2.data_to_send())
            assert stalled.recv(65536), "Server did not establish HTTP/2"
            process.send_signal(signal.SIGTERM)
            stdout, stderr = process.communicate(timeout=12)
        assert process.returncode == 0 and stdout == b"STOPPED\n" and not stderr, (stdout, stderr)
    finally:
        if process.poll() is None:
            process.kill()
            process.communicate()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark", help="Compiler executable to verify")
    parser.add_argument("--client-mode", choices=("buffered", "stream", "head", "abandon", "truncated", "early-buffered", "early-stream"),
                        help="Run one client case, without the server checks")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-http2-") as temporary:
        directory = Path(temporary)
        if args.client_mode:
            client_checks(directory, (args.client_mode,), args.compiler)
            print("HTTP/2 client case verified:", args.client_mode)
        else:
            client_checks(directory, compiler=args.compiler)
            server_checks(directory, args.compiler)
            print("HTTP/2 TLS client and cleartext server verified: flow control, streaming, trailers, HEAD, early rejection, truncation and cleanup")


if __name__ == "__main__":
    main()
