#!/usr/bin/env python3
"""Verify persistent HTTP/2 sessions against independent hyper-h2/TLS owners."""

import argparse
import socket
import ssl
import subprocess
import tempfile
import threading
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import RequestReceived, StreamEnded
from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// session.dark - Persistent ALPN HTTP/2 owner and stream allocation.
let id (response: Stdlib.HttpClient.Response) : String =
  match Stdlib.List.findFirst response.headers (fun header -> let (name, _value) = header in Stdlib.String.toLowercase name == "x-connection") with
  | None -> ""
  | Some header -> let (_name, value) = header in value
let run (session: Stdlib.HttpClientSession.Session) (url: String) : Bool =
  let first = session.request "GET" (url ++ "/a") [] Stdlib.Blob.empty in
  let second = session.request "GET" (url ++ "/b") [] Stdlib.Blob.empty in
  let _ = session.clear () in
  let third = session.request "GET" (url ++ "/c") [] Stdlib.Blob.empty in
  let _ = session.close () in
  match first, second, third with
  | Ok first, Ok second, Ok third -> id first != "" && id first == id second && id third != id second && first.body == Stdlib.String.toBlob "/a" && second.body == Stdlib.String.toBlob "/b"
  | _, _, _ -> false
let roots = Stdlib.Blob.fromHex "@CA@" |> Stdlib.Result.andThen Stdlib.__Tls13Client.parsePemBundle
let url = Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault ""
match roots with
| Error message -> Stdlib.printLine message
| Ok roots ->
  let session = Stdlib.HttpClientSession.createTrustedWithRoots roots in
  Stdlib.printLine (if run session url then "OK" else "FAILED")
'''


class Peer:
    def __init__(self, context):
        self.context = context
        self.listener = socket.socket()
        self.listener.bind(("127.0.0.1", 0))
        self.listener.listen(8)
        self.listener.settimeout(0.1)
        self.port = self.listener.getsockname()[1]
        self.stop = threading.Event()
        self.count = 0
        self.errors, self.sockets, self.workers, self.streams = [], [], [], []
        self.thread = threading.Thread(target=self.accept, daemon=True)
        self.thread.start()

    def accept(self):
        while not self.stop.is_set():
            try:
                raw, _ = self.listener.accept()
            except socket.timeout:
                continue
            except OSError:
                return
            self.count += 1
            self.sockets.append(raw)
            worker = threading.Thread(target=self.serve, args=(raw, self.count), daemon=True)
            self.workers.append(worker)
            worker.start()

    def serve(self, raw, number):
        try:
            raw.settimeout(30)
            with self.context.wrap_socket(raw, server_side=True) as connection:
                self.sockets.append(connection)
                assert connection.selected_alpn_protocol() == "h2"
                protocol = H2Connection(H2Configuration(client_side=False, header_encoding="utf-8"))
                protocol.initiate_connection()
                connection.sendall(protocol.data_to_send())
                paths = {}
                while not self.stop.is_set():
                    data = connection.recv(65536)
                    if not data:
                        return
                    for event in protocol.receive_data(data):
                        if isinstance(event, RequestReceived):
                            paths[event.stream_id] = dict(event.headers)[":path"]
                            self.streams.append((number, event.stream_id))
                        elif isinstance(event, StreamEnded):
                            body = paths[event.stream_id].encode()
                            protocol.send_headers(event.stream_id, [(":status", "200"), ("x-connection", str(number)),
                                ("x-shared", "dynamic-hpack-value-" * 4), ("content-length", str(len(body)))])
                            protocol.send_data(event.stream_id, body, end_stream=True)
                    connection.sendall(protocol.data_to_send())
        except Exception as error:
            if not self.stop.is_set():
                self.errors.append(error)

    def close(self):
        self.stop.set()
        self.listener.close()
        for connection in self.sockets:
            connection.close()
        self.thread.join(3)
        for worker in self.workers:
            worker.join(3)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    with tempfile.TemporaryDirectory(prefix="dark-http2-session-") as temporary:
        directory = Path(temporary)
        certificate, private = directory / "leaf.pem", directory / "leaf.key"
        certificate.write_bytes(leaf.public_bytes(serialization.Encoding.PEM))
        private.write_bytes(key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()))
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.minimum_version = ssl.TLSVersion.TLSv1_3
        context.num_tickets = 0
        context.set_alpn_protocols(["h2"])
        context.load_cert_chain(certificate, private)
        source, binary = directory / "session.dark", directory / "session"
        source.write_text(SOURCE.replace("@CA@", ca.public_bytes(serialization.Encoding.PEM).hex()))
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        peer = Peer(context)
        try:
            result = subprocess.run([str(binary), f"https://localhost:{peer.port}"], cwd=ROOT,
                                    capture_output=True, text=True, timeout=60)
            assert result.returncode == 0 and result.stdout == "OK\n" and not result.stderr, (result, peer.streams, peer.errors)
            assert peer.streams == [(1, 1), (1, 3), (2, 1)], peer.streams
            assert not peer.errors, peer.errors
        finally:
            peer.close()
    print("Persistent HTTP/2 ALPN/TLS, odd stream allocation, shared HPACK and clear/close cleanup verified")


if __name__ == "__main__":
    main()
