#!/usr/bin/env python3
"""Verify connection-wide QPACK and HTTP/3 session reuse with an independent aioquic peer."""
import argparse
import socket
import subprocess
import tempfile
import threading
import time
from pathlib import Path

from aioquic.buffer import Buffer, encode_uint_var
from aioquic.h3.connection import H3Connection, encode_frame
from aioquic.h3.events import HeadersReceived
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.packet import pull_quic_header
from cryptography.hazmat.primitives import serialization
from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// h3-session.dark - Repeated requests share QUIC and dynamic QPACK ownership.
let check (session: Stdlib.HttpClientSession.Session) (index: Int64) : Bool =
  if index == @COUNT@L then true
  else match session.request "GET" ("https://localhost/" ++ Stdlib.Int64.toString index) [("x-shared", "café ☃")] Stdlib.Blob.empty with
  | Error _ -> false
  | Ok response -> Stdlib.Blob.toHex response.body == Stdlib.Blob.toHex (Stdlib.String.toBlob ("/" ++ Stdlib.Int64.toString index)) && check session (index + 1L)
let run (port: Int64) (root: Blob) : Stdlib.Result.Result<Bool, String> =
  Stdlib.__QuicClient.connect (Stdlib.__Datagram.Endpoint { address = [127L, 0L, 0L, 1L], port = port }) "localhost" [root] 10000L |> Stdlib.Result.andThen (fun ready ->
    let origin = Stdlib.__HttpWire.Url { scheme = "https", host = "localhost", port = 443L, authority = "localhost", target = "/0" } in
    match Stdlib.__Http3Pool.open ready "GET" origin [("x-shared", "café ☃")] Stdlib.Blob.empty with
    | Error _ -> Error "First request failed"
    | Ok completed ->
      match completed.entry, Stdlib.__Network.monotonicMillis () with
      | Some entry, Ok now ->
        let pool = Stdlib.__Http3Pool.put Stdlib.__Http3Pool.empty entry now in
        let session = Stdlib.HttpClientSession.__createState (Stdlib.HttpClientSession.__State { cache = Stdlib.__AltSvcCache.empty, pool = Stdlib.__Http1Pool.empty, http2 = Stdlib.__Http2Pool.empty, http3 = pool, epoch = 0L }) true (Some [root]) in
        let valid = check session 1L in
        let _ = session.clear () in
        let _ = session.close () in
        let _ = session.close () in Ok valid
      | _, _ -> Error "First connection was not reusable")
match Stdlib.Cli.__Args.int64 0, Stdlib.Blob.fromHex "@CA@" with
| Ok port, Ok root -> match run port root with | Ok true -> Stdlib.printLine "OK" | Ok false -> Stdlib.printLine "FAILED" | Error message -> Stdlib.printLine message
| _, _ -> Stdlib.printLine "Bad arguments"
'''


MULTIPLEX = '''// h3-multiplex.dark - Interleaved responses retain per-stream parsers and shared QPACK.
let request (path: String) : Stdlib.__Http3Connection.Request =
  Stdlib.__Http3Connection.Request { method = "GET", fields = [(":method", "GET"), (":scheme", "https"), (":authority", "localhost"), (":path", path), ("x-shared", "café ☃")], body = Stdlib.Blob.empty }
let check (state: Stdlib.__Http3Connection.State) (mode: String) : Stdlib.Result.Result<Bool, String> =
  Stdlib.__Http3Connection.requestMany state [request "/one", request "/two"] |> Stdlib.Result.andThen (fun batch ->
    let valid = match batch.responses with
    | [first, second] ->
      match first.response, second.response with
      | Ok first, Ok second -> mode == "interleave" && Stdlib.Blob.__byteLength first.body == 70000L && Stdlib.Blob.__byteLength second.body == 70000L
      | Error _, Ok second -> mode == "reset" && Stdlib.Blob.__byteLength second.body == 70000L
      | Ok first, Error _ -> mode == "goaway" && Stdlib.Blob.__byteLength first.body == 70000L
      | _, _ -> false
    | _ -> false in
    let result = Stdlib.__Http3Connection.requestMany batch.state [request "/after"] |> Stdlib.Result.map (fun after ->
      let _ = Stdlib.__Http3.close after.state.wire in
      let followup = match after.responses with | [outcome] -> (match outcome.response with | Ok response -> Stdlib.Blob.toHex response.body == "2F6166746572" | Error _ -> mode == "goaway") | _ -> false in
      valid && followup) in
    match result with | Ok result -> Ok result | Error message -> let _ = Stdlib.__Http3.close batch.state.wire in Error message)
match Stdlib.Cli.__Args.int64 0, Stdlib.Blob.fromHex "@CA@" with
| Ok port, Ok root ->
  match Stdlib.__QuicClient.connect (Stdlib.__Datagram.Endpoint { address = [127L, 0L, 0L, 1L], port = port }) "localhost" [root] 10000L with
  | Error message -> Stdlib.printLine message
  | Ok ready ->
    let result = Stdlib.__Http3Connection.open ready |> Stdlib.Result.andThen (fun state -> check state @MODE@) in
    let _ = Stdlib.__QuicClient.close ready in
    match result with | Ok true -> Stdlib.printLine "OK" | Ok false -> Stdlib.printLine "FAILED" | Error message -> Stdlib.printLine message
| _, _ -> Stdlib.printLine "Bad arguments"
'''


PUBLIC_MULTIPLEX = r'''// h3-session-batch.dark - Public batches use advertised QUIC without a TCP fallback.
let request (path: String) : Stdlib.HttpClientSession.Request =
  Stdlib.HttpClientSession.Request { method = "GET", url = "https://localhost" ++ path, headers = [("x-shared", "café ☃")], body = Stdlib.Blob.empty }
let check (session: Stdlib.HttpClientSession.Session) (mode: String) : Bool =
  match session.requestMany [request "/one", request "/two"] with
  | Error _ -> false
  | Ok outcomes ->
    let valid = match outcomes with
    | [first, second] -> first.index == 0L && second.index == 1L && (match first.response, second.response with
      | Ok first, Ok second -> mode == "interleave" && Stdlib.Blob.__byteLength first.body == 70000L && Stdlib.Blob.__byteLength second.body == 70000L
      | Error _, Ok second -> mode == "reset" && Stdlib.Blob.__byteLength second.body == 70000L
      | Ok first, Error _ -> mode == "goaway" && Stdlib.Blob.__byteLength first.body == 70000L
      | _, _ -> false)
    | _ -> false in
    if mode == "goaway" then valid
    else
      let after = session.request "GET" "https://localhost/after" [("x-shared", "café ☃")] Stdlib.Blob.empty in
      valid && (match after with | Ok response -> Stdlib.Blob.toHex response.body == "2F6166746572" | Error _ -> false)
match Stdlib.Cli.__Args.int64 0, Stdlib.Blob.fromHex "@CA@", Stdlib.__Network.monotonicMillis () with
| Ok port, Ok root, Ok now ->
  let origin = Stdlib.__HttpWire.Url { scheme = "https", host = "localhost", port = 443L, authority = "localhost", target = "/" } in
  let cache = Stdlib.__AltSvcCache.learn Stdlib.__AltSvcCache.empty origin 200 [("alt-svc", "h3=\"127.0.0.1:" ++ Stdlib.Int64.toString port ++ "\"; ma=60")] now now 0L in
  let session = Stdlib.HttpClientSession.__createState (Stdlib.HttpClientSession.__State { cache = cache, pool = Stdlib.__Http1Pool.empty, http2 = Stdlib.__Http2Pool.empty, http3 = Stdlib.__Http3Pool.empty, epoch = 0L }) true (Some [root]) in
  let valid = check session @MODE@ in
  let _ = session.clear () in
  let _ = session.close () in
  if valid then Stdlib.printLine "OK" else Stdlib.printLine "FAILED"
| _, _, _ -> Stdlib.printLine "Bad arguments"
'''


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    parser.add_argument("--requests", type=int, default=5)
    parser.add_argument("--multiplex", choices=["interleave", "reset", "goaway"])
    parser.add_argument("--public", action="store_true")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    configuration = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    configuration.certificate, configuration.private_key = leaf, key
    errors, streams, connections = [], [], []
    stopped = threading.Event()
    with tempfile.TemporaryDirectory(prefix="h3-session-") as temporary, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
        listener.bind(("127.0.0.1", 0))
        listener.settimeout(0.01)
        source, binary = Path(temporary) / "session.dark", Path(temporary) / "session"
        test_source = (PUBLIC_MULTIPLEX if args.public else MULTIPLEX).replace("@MODE@", '"' + args.multiplex + '"') if args.multiplex else SOURCE
        source.write_text(test_source.replace("@CA@", ca.public_bytes(serialization.Encoding.DER).hex()).replace("@COUNT@", str(args.requests)))
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr

        def peer():
            connection, http = None, None
            pending = {}
            try:
                while not stopped.is_set():
                    now = time.monotonic()
                    try:
                        packet, address = listener.recvfrom(8192)
                        if connection is None:
                            header = pull_quic_header(Buffer(data=packet), host_cid_length=8)
                            connection = QuicConnection(configuration=configuration, original_destination_connection_id=header.destination_cid)
                            connections.append(connection)
                            http = H3Connection(connection)
                        connection.receive_datagram(packet, address, now)
                    except socket.timeout:
                        pass
                    if connection is None:
                        continue
                    timer = connection.get_timer()
                    if timer is not None and timer <= now:
                        connection.handle_timer(now)
                    while (event := connection.next_event()) is not None:
                        for received in http.handle_event(event):
                            if isinstance(received, HeadersReceived):
                                streams.append(received.stream_id)
                                fields = dict(received.headers)
                                assert fields[b"x-shared"] == "café ☃".encode()
                                body = fields[b":path"]
                                if args.multiplex and received.stream_id < 8:
                                    pending[received.stream_id] = body
                                    if len(pending) == 2:
                                        if args.multiplex == "goaway":
                                            connection.send_stream_data(http._local_control_stream_id, encode_frame(7, encode_uint_var(4)))
                                        for identifier in [4, 0]:
                                            if identifier == 4 and args.multiplex == "goaway":
                                                continue
                                            if identifier == 0 and args.multiplex == "reset":
                                                connection.reset_stream(0, 268)
                                                continue
                                            http.send_headers(identifier, [(b":status", b"200"), (b"content-length", b"70000"), (b"x-shared", "café ☃".encode())])
                                            http.send_data(identifier, bytes([97 + identifier]) * 70000, end_stream=True)
                                else:
                                    http.send_headers(received.stream_id, [(b":status", b"200"), (b"content-length", str(len(body)).encode()), (b"x-shared", "café ☃".encode())])
                                    http.send_data(received.stream_id, body, end_stream=True)
                    for data, address in connection.datagrams_to_send(now):
                        listener.sendto(data, address)
            except BaseException as error:
                errors.append(error)

        thread = threading.Thread(target=peer)
        thread.start()
        try:
            result = subprocess.run([str(binary), str(listener.getsockname()[1])], cwd=ROOT, capture_output=True, text=True, timeout=60)
        finally:
            stopped.set()
            thread.join(timeout=2)
        assert not thread.is_alive() and not errors, errors
        assert result.returncode == 0 and result.stdout == "OK\n" and not result.stderr, (result, streams)
        assert len(connections) == 1 and streams == (([0, 4] if args.multiplex == "goaway" else [0, 4, 8]) if args.multiplex else list(range(0, args.requests * 4, 4))), streams
    print(f"HTTP/3 {args.multiplex or 'session'}: streams {streams}, shared Unicode QPACK and leak-free disposal passed")


if __name__ == "__main__":
    main()
