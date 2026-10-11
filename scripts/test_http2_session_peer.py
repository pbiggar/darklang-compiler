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
from h2.events import DataReceived, RequestReceived, StreamEnded
from h2.settings import SettingCodes
from hyperframe.frame import GoAwayFrame
from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// session.dark - Persistent ALPN HTTP/2 owner and stream allocation.
let id (response: Stdlib.HttpClient.Response) : String =
  match Stdlib.List.findFirst response.headers (fun header -> let (name, _value) = header in Stdlib.String.toLowercase name == "x-connection") with
  | None -> ""
  | Some header -> let (_name, value) = header in value
let run (session: Stdlib.HttpClientSession.Session) (url: String) : String =
  let first = session.request "GET" (url ++ "/a") [] Stdlib.Blob.empty in
  let second = session.request "GET" (url ++ "/b") [] Stdlib.Blob.empty in
  let _ = session.clear () in
  let third = session.request "GET" (url ++ "/c") [] Stdlib.Blob.empty in
  let _ = session.close () in
  match first, second, third with
  | Ok first, Ok second, Ok third ->
    if id first != "" && id first == id second && id third != id second && Stdlib.Blob.toHex first.body == "2F61" && Stdlib.Blob.toHex second.body == "2F62" then "OK"
    else "FAILED: " ++ id first ++ "," ++ id second ++ "," ++ id third ++ "|" ++ Stdlib.Blob.toHex first.body ++ "|" ++ Stdlib.Blob.toHex second.body
  | _, _, _ -> "Request failed"
let roots = Stdlib.Blob.fromHex "@CA@" |> Stdlib.Result.andThen Stdlib.__Tls13Client.parsePemBundle
let url = Stdlib.Cli.__Args.get 0 |> Stdlib.Result.withDefault ""
match roots with
| Error message -> Stdlib.printLine message
| Ok roots ->
  let session = Stdlib.HttpClientSession.createTrustedWithRoots roots in
  Stdlib.printLine (run session url)
'''

MULTIPLEX = '''// multiplex.dark - Interleaved responses and flow credit share one TLS owner.
let request (path: String) (upload: Bool) : Stdlib.__Http2Connection.Request =
  let body = if upload then Stdlib.String.toBlob (Stdlib.String.join (Stdlib.List.repeatUnsafe 70 (Stdlib.String.repeat "u" 1000)) "") else Stdlib.Blob.empty in
  Stdlib.__Http2Connection.Request { method = if upload then "POST" else "GET", fields = [(":method", if upload then "POST" else "GET"), (":scheme", "https"), (":authority", "localhost"), (":path", path), ("content-length", Stdlib.Int64.toString (Stdlib.Blob.__byteLength body))], body = body }
let check (transport: Stdlib.__HttpTransport.Transport) (state: Stdlib.__Http2Connection.State) (mode: String) : Stdlib.Result.Result<Bool, String> =
  let upload = mode == "upload" || mode == "early" in
  Stdlib.__Http2Connection.requestMany transport state [request "/one" upload, request "/two" upload] |> Stdlib.Result.andThen (fun batch ->
    let valid = match batch.responses with
    | [first, second] ->
      match first.response, second.response with
      | Ok first, Ok second -> mode != "reset" && mode != "goaway" && Stdlib.Blob.__byteLength first.body == 70000L && Stdlib.Blob.__byteLength second.body == 70000L
      | Error _error, Ok second -> mode == "reset" && Stdlib.Blob.__byteLength second.body == 70000L
      | Ok first, Error _error -> mode == "goaway" && Stdlib.Blob.__byteLength first.body == 70000L
      | _, _ -> false
    | _ -> false in
    Stdlib.__Http2Connection.requestMany transport batch.state [request "/after" false] |> Stdlib.Result.map (fun after ->
      let followup = match after.responses with
      | [outcome] -> (match outcome.response with | Ok response -> mode != "goaway" && Stdlib.Blob.toHex response.body == "2F6166746572" | Error _ -> mode == "goaway")
      | _ -> false in
      valid && followup))
let run (port: Int64) (roots: List<Blob>) (mode: String) : Stdlib.Result.Result<Bool, String> =
  Stdlib.__Network.connectTcp4 [127L, 0L, 0L, 1L] port 10000L |> Stdlib.Result.mapError (fun _error -> "Connect failed") |> Stdlib.Result.andThen (fun connection ->
    let result = Stdlib.__Tls13Client.handshakeWithProtocols connection "localhost" roots ["h2"] |> Stdlib.Result.andThen (fun negotiated ->
      let transport = Stdlib.__HttpTransport.secure connection negotiated.keys in
      let result = Stdlib.__Http2Connection.open transport |> Stdlib.Result.andThen (fun state -> check transport state mode) in
      let _ = Stdlib.__HttpTransport.close transport in result) in
    let _ = Stdlib.__Network.closeTcp connection in result)
let roots = Stdlib.Blob.fromHex "@CA@" |> Stdlib.Result.andThen Stdlib.__Tls13Client.parsePemBundle
let mode = Stdlib.Cli.__Args.get 1 |> Stdlib.Result.withDefault "multiplex"
match Stdlib.Cli.__Args.int64 0, roots with
| Ok port, Ok roots -> match run port roots mode with | Ok true -> Stdlib.printLine "OK" | Ok false -> Stdlib.printLine "FAILED" | Error message -> Stdlib.printLine message
| _, _ -> Stdlib.printLine "Invalid arguments"
'''


class Peer:
    def __init__(self, context, mode="reuse"):
        self.context = context
        self.mode = mode
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
                if self.mode == "limit":
                    protocol.update_settings({SettingCodes.MAX_CONCURRENT_STREAMS: 1})
                connection.sendall(protocol.data_to_send())
                paths, ended, outgoing, uploaded = {}, set(), {}, {}
                replied = False
                while not self.stop.is_set():
                    data = connection.recv(65536)
                    if not data:
                        return
                    for event in protocol.receive_data(data):
                        if isinstance(event, RequestReceived):
                            paths[event.stream_id] = dict(event.headers)[":path"]
                            uploaded[event.stream_id] = 0
                            self.streams.append((number, event.stream_id))
                        elif isinstance(event, DataReceived):
                            uploaded[event.stream_id] += len(event.data)
                            if self.mode != "early":
                                protocol.acknowledge_received_data(event.flow_controlled_length, event.stream_id)
                        elif isinstance(event, StreamEnded):
                            ended.add(event.stream_id)
                    ready = [stream for stream in ended if stream not in outgoing]
                    if self.mode not in ("reuse", "limit") and not replied:
                        ready = [3, 1] if ((self.mode == "early" and len(paths) >= 2) or {1, 3}.issubset(ended)) else []
                        if ready:
                            replied = True
                    if self.mode == "goaway" and ready == [3, 1]:
                        goaway = GoAwayFrame(0)
                        goaway.last_stream_id = 1
                        goaway.error_code = 0
                        connection.sendall(goaway.serialize())
                        ready = [1]
                        outgoing[3] = (b"", 0)
                    for stream in ready:
                        if self.mode == "reset" and stream == 1:
                            protocol.reset_stream(stream, error_code=8)
                            outgoing[stream] = (b"", 0)
                            continue
                        if self.mode == "upload" and stream in (1, 3):
                            assert uploaded[stream] == 70000, uploaded
                        body = paths[stream].encode() if self.mode == "reuse" or paths[stream] == "/after" else (b"a" if stream == 1 else b"b") * 70000
                        protocol.send_headers(stream, [(":status", "200"), ("x-connection", str(number)),
                            ("x-shared", "dynamic-hpack-value-" * 4), ("content-length", str(len(body)))])
                        outgoing[stream] = (body, 0)
                    if ready:
                        protocol.ping(b"muxcheck")
                    progress = True
                    while progress:
                        progress = False
                        for stream, (body, position) in list(outgoing.items()):
                            size = min(len(body) - position, protocol.local_flow_control_window(stream), 16384) if position < len(body) else 0
                            if size:
                                position += size
                                protocol.send_data(stream, body[position-size:position], end_stream=position == len(body))
                                outgoing[stream] = (body, position)
                                progress = True
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
        source.write_text(MULTIPLEX.replace("@CA@", ca.public_bytes(serialization.Encoding.PEM).hex()))
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for mode in ("multiplex", "reset", "limit", "upload", "early", "goaway"):
            peer = Peer(context, mode)
            try:
                result = subprocess.run([str(binary), str(peer.port), mode], cwd=ROOT, capture_output=True, text=True, timeout=60)
                assert result.returncode == 0 and result.stdout == "OK\n" and not result.stderr, (mode, result, peer.streams, peer.errors)
                expected = [(1, 1), (1, 3)] + ([] if mode == "goaway" else [(1, 5)])
                assert peer.streams == expected and not peer.errors, (mode, peer.streams, peer.errors)
            finally:
                peer.close()
    print("HTTP/2 session reuse and independent multiplexing: reverse/interleaved 70 KiB replies, HPACK, PING, per-stream limits/reset, 140 KiB upload, early replies, GOAWAY and cleanup verified")


if __name__ == "__main__":
    main()
