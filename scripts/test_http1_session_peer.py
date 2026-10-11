#!/usr/bin/env python3
"""Verify persistent session ownership and response framing against TCP/TLS peers."""

import argparse
import socket
import ssl
import subprocess
import tempfile
import threading
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// session.dark - Buffered connection reuse, clear/close and no request replay.
let id (response: Stdlib.HttpClient.Response) : String =
  match Stdlib.List.findFirst response.headers (fun header -> let (name, _value) = header in name == "x-connection") with
  | None -> ""
  | Some header -> let (_name, value) = header in value
let run (session: Stdlib.HttpClientSession.Session) (url: String) (path: String) (same: Bool) : Stdlib.Result.Result<Unit, String> =
  let method = if path == "/head" then "HEAD" else "GET" in
  let first = session.request method (url ++ path) [] Stdlib.Blob.empty in
  let second = session.request "GET" (url ++ "/fixed") [] Stdlib.Blob.empty in
  let _ = session.clear () in
  let third = session.request "GET" (url ++ "/big") [] Stdlib.Blob.empty in
  let _ = session.close () in
  let _ = session.close () in
  match first, second, third, session.request "GET" url [] Stdlib.Blob.empty with
  | Ok first, Ok second, Ok third, Error _ ->
    if id first == "" || id second == "" || (id first == id second) != same then Error "Connection reuse failed"
    else if id third == id second || Stdlib.Blob.__byteLength third.body != 70000L then Error "Clear or body framing failed"
    else Ok ()
  | _, _, _, _ -> Error "Session request or lifecycle failed"
let url = Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault ""
let path = Stdlib.Cli.__Args.get 1 |> Stdlib.Result.withDefault "/fixed"
let same = (Stdlib.Cli.__Args.get 2 |> Stdlib.Result.withDefault "same") == "same"
let roots = Stdlib.Blob.fromHex "@CA@" |> Stdlib.Result.andThen Stdlib.__Tls13Client.parsePemBundle
match roots with
| Error message -> Stdlib.printLine message
| Ok roots ->
  let session = Stdlib.HttpClientSession.createTrustedWithRoots roots in
  match run session url path same with | Error message -> Stdlib.printLine message | Ok _unit -> Stdlib.printLine "OK"
'''


class Peer:
    def __init__(self, context):
        self.context = context
        self.listener = socket.socket()
        self.listener.bind(("127.0.0.1", 0))
        self.listener.listen(16)
        self.listener.settimeout(0.1)
        self.port = self.listener.getsockname()[1]
        self.stopping = threading.Event()
        self.workers, self.connections, self.errors = [], [], []
        self.count = 0
        self.thread = threading.Thread(target=self.accept, daemon=True)
        self.thread.start()

    def accept(self):
        while not self.stopping.is_set():
            try:
                connection, _ = self.listener.accept()
            except socket.timeout:
                continue
            except OSError:
                break
            self.count += 1
            self.connections.append(connection)
            worker = threading.Thread(target=self.serve, args=(connection, self.count), daemon=True)
            self.workers.append(worker)
            worker.start()

    def serve(self, connection, number):
        try:
            connection.settimeout(3)
            if self.context:
                connection = self.context.wrap_socket(connection, server_side=True)
                self.connections.append(connection)
            with connection:
                buffer = b""
                while not self.stopping.is_set():
                    while b"\r\n\r\n" not in buffer:
                        chunk = connection.recv(4096)
                        if not chunk:
                            return
                        buffer += chunk
                    head, buffer = buffer.split(b"\r\n\r\n", 1)
                    lines = head.split(b"\r\n")
                    method, path, _ = lines[0].split(b" ")
                    fields = dict(line.lower().split(b": ", 1) for line in lines[1:])
                    length = int(fields.get(b"content-length", b"0"))
                    while len(buffer) < length:
                        chunk = connection.recv(4096)
                        if not chunk:
                            raise AssertionError("Truncated request body")
                        buffer += chunk
                    buffer = buffer[length:]
                    body = b"b" * 70000 if path == b"/big" else str(number).encode()
                    version = b"HTTP/1.0" if path == b"/old" else b"HTTP/1.1"
                    response = version + b" 200 OK\r\nX-Connection: " + str(number).encode() + b"\r\n"
                    if path == b"/chunked":
                        response += b"Transfer-Encoding: chunked\r\n\r\n" + hex(len(body))[2:].encode() + b"\r\n" + body + b"\r\n0\r\nX-Trailer: ok\r\n\r\n"
                    elif path == b"/eof":
                        response += b"\r\n" + body
                    else:
                        response += b"Content-Length: " + str(len(body)).encode() + b"\r\n"
                        if path == b"/close":
                            response += b"Connection: keep-alive, ClOsE\r\n"
                        response += b"\r\n" + (b"" if method == b"HEAD" else body)
                        if path == b"/extra":
                            response += b"unsolicited"
                    connection.sendall(response)
                    if path in [b"/close", b"/eof", b"/old"] or b"close" in fields.get(b"connection", b""):
                        if self.context and path == b"/eof":
                            try:
                                connection.unwrap()
                            except ssl.SSLError:
                                pass
                        return
        except (OSError, ssl.SSLError) as error:
            if not self.stopping.is_set():
                self.errors.append(error)

    def close(self):
        self.stopping.set()
        self.listener.close()
        for connection in self.connections:
            try:
                connection.close()
            except OSError:
                pass
        self.thread.join(3)
        for worker in self.workers:
            worker.join(3)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    with tempfile.TemporaryDirectory(prefix="dark-http1-session-") as temporary:
        directory = Path(temporary)
        certificate, private = directory / "leaf.pem", directory / "leaf.key"
        certificate.write_bytes(leaf.public_bytes(serialization.Encoding.PEM))
        private.write_bytes(key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()))
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.minimum_version = ssl.TLSVersion.TLSv1_3
        context.num_tickets = 0
        context.set_alpn_protocols(["http/1.1"])
        context.load_cert_chain(certificate, private)
        source, binary = directory / "session.dark", directory / "session"
        source.write_text(SOURCE.replace("@CA@", ca.public_bytes(serialization.Encoding.PEM).hex()))
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                cwd=ROOT, text=True, capture_output=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        for scheme, security in [("http", None), ("https", context)]:
            peer = Peer(security)
            try:
                for path, same in [("fixed", True), ("chunked", True), ("head", True),
                                   ("close", False), ("eof", False), ("old", False), ("extra", False)]:
                    result = subprocess.run([str(binary), f"{scheme}://localhost:{peer.port}", f"/{path}",
                                             "same" if same else "different"], cwd=ROOT, text=True,
                                            capture_output=True, timeout=30)
                    assert result.returncode == 0 and result.stdout == "OK\n" and not result.stderr, (scheme, path, result)
                assert not peer.errors, peer.errors
            finally:
                peer.close()
    print("TCP/TLS session reuse, chunked/HEAD/EOF framing, close tokens, HTTP/1.0, surplus bytes, clear/close, 70 KiB bodies and compiled cleanup verified")


if __name__ == "__main__":
    main()
