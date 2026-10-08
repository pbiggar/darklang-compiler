#!/usr/bin/env python3
"""Drive production Dark QUIC streams through a real test-only aioquic HTTP/3 exchange."""

import argparse
import socket
import subprocess
import tempfile
import threading
import time
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.h3.connection import H3Connection
from aioquic.h3.events import HeadersReceived
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.packet import QuicPacketType, pull_quic_header
from aioquic.tls import Epoch
from cryptography.hazmat.primitives import serialization
import pylsqpack

from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
BODY = bytes(range(256)) * 280
DARK = """// streams.dark - Use production QUIC streams for an HTTP/3 request and ordered response bytes.
let emit (bytes: Blob) (offset: Int64) : Unit =
  if Stdlib.Int.fromInt64 offset >= Stdlib.Blob.length bytes then ()
  else
    let part = Stdlib.Blob.slice bytes (Stdlib.Int.fromInt64 offset) 512 in
    let _ = Stdlib.printLine (Stdlib.Blob.toHex part) in
    emit bytes (offset + 512L)
let initialize (ready: Stdlib.QuicClient.Ready) : Stdlib.Result.Result<Stdlib.QuicConnection.State, String> =
  Stdlib.QuicConnection.fromClient ready |> Stdlib.Result.andThen (fun state ->
    Stdlib.QuicConnection.openStream state 2L |> Stdlib.Result.andThen (fun state ->
    Stdlib.QuicConnection.openStream state 6L |> Stdlib.Result.andThen (fun state ->
    Stdlib.QuicConnection.openStream state 10L |> Stdlib.Result.andThen (fun state ->
    Stdlib.QuicConnection.openStream state 0L |> Stdlib.Result.andThen (fun state ->
      match Stdlib.Http3Wire.serialize 4L (Stdlib.Http3Wire.settings ()),
        Stdlib.Qpack.encode [(":method", "GET"), (":scheme", "https"), (":authority", "localhost"), (":path", "/streams")] |> Stdlib.Result.andThen (Stdlib.Http3Wire.serialize 1L) with
      | Ok settings, Ok request ->
        Stdlib.QuicConnection.write state 2L (Stdlib.Blob.concat [Stdlib.String.toBlob "\\0", settings]) false |> Stdlib.Result.andThen (fun state ->
        Stdlib.Blob.fromHex "02" |> Stdlib.Result.andThen (fun marker -> Stdlib.QuicConnection.write state 6L marker false) |> Stdlib.Result.andThen (fun state ->
        Stdlib.Blob.fromHex "03" |> Stdlib.Result.andThen (fun marker -> Stdlib.QuicConnection.write state 10L marker false) |> Stdlib.Result.andThen (fun state ->
        Stdlib.QuicConnection.write state 0L request true)))
      | _, _ -> Error "HTTP/3 encoding failed")))))
let controls (state: Stdlib.QuicConnection.State) (uni: Stdlib.Http3Uni.State)
  : Stdlib.Result.Result<(Stdlib.QuicConnection.State * Stdlib.Http3Uni.State), String> =
  Stdlib.List.fold state.flow.channels (Ok ((state, uni))) (fun result channel ->
    if channel.id % 4L != 3L then result
    else result |> Stdlib.Result.andThen (fun pair ->
      let (connection, streams) = pair in
      Stdlib.QuicConnection.read connection channel.id |> Stdlib.Result.andThen (fun read ->
        Stdlib.Http3Uni.receive streams channel.id read.bytes read.finished (Stdlib.Option.isSome read.reset)
          |> Stdlib.Result.map (fun received -> (read.state, received.state)))))
let receive (state: Stdlib.QuicConnection.State) (deadline: Int64)
  (message: Stdlib.Http3Message.State) (uni: Stdlib.Http3Uni.State)
  : Stdlib.Result.Result<Unit, String> =
  controls state uni |> Stdlib.Result.andThen (fun pair ->
  let (connection, streams) = pair in
  Stdlib.QuicConnection.read connection 0L |> Stdlib.Result.andThen (fun read ->
    let _ = emit read.bytes 0L in
    if Stdlib.Option.isSome read.reset then Error "Response reset"
    else Stdlib.Http3Message.feed message read.bytes |> Stdlib.Result.andThen (fun received ->
      if read.finished then
        Stdlib.Http3Message.finish received.state |> Stdlib.Result.andThen (fun _unit ->
          Stdlib.QuicConnection.flush read.state |> Stdlib.Result.map (fun _state -> ()))
      else match Stdlib.Network.monotonicMillis () with
      | Error _ -> Error "Clock failed"
      | Ok now -> if now >= deadline then Error "Response timed out"
                  else Stdlib.QuicConnection.poll read.state 10L |> Stdlib.Result.andThen (fun next ->
                    receive next deadline received.state streams))))
let run () : Unit =
  match Stdlib.Blob.fromHex "@ROOT@", Stdlib.Cli.Args.get 0 |> Stdlib.Result.andThen (fun value -> Stdlib.Int64.parse value |> Stdlib.Result.mapError (fun _error -> "port")) with
  | Ok root, Ok port ->
    match Stdlib.QuicClient.connect (Stdlib.Datagram.Endpoint { address = [127L,0L,0L,1L], port = port }) "localhost" [root] 8000L with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok ready ->
      let result = initialize ready |> Stdlib.Result.andThen (fun state ->
        match Stdlib.Network.monotonicMillis () with | Error _ -> Error "Clock failed" | Ok now ->
          receive state (now + 10000L) (Stdlib.Http3Message.create (Response false)) (Stdlib.Http3Uni.create true)) in
      let _ = Stdlib.QuicClient.close ready in
      match result with | Error message -> Stdlib.printLine ("ERROR " ++ message) | Ok () -> Stdlib.printLine "DONE"
  | _ -> Stdlib.printLine "Bad arguments"
run ()
"""


def main():
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--body-size", type=int, default=len(BODY))
    arguments.add_argument("--mode", choices=("trusted", "drop-request", "drop-response", "reorder", "trailers", "key-update"))
    options = arguments.parse_args()
    body_bytes = (BODY * ((options.body_size + len(BODY) - 1) // len(BODY)))[:options.body_size]
    key, leaf, ca = certificates()
    config = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    config.certificate, config.private_key = leaf, key
    with tempfile.TemporaryDirectory(prefix="dark-quic-streams-") as temporary:
        source, binary = Path(temporary) / "streams.dark", Path(temporary) / "streams"
        source.write_text(DARK.replace("@ROOT@", ca.public_bytes(serialization.Encoding.DER).hex()))
        result = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT,
            text=True, capture_output=True, timeout=120)
        assert result.returncode == 0, result.stdout + result.stderr
        for mode in ([options.mode] if options.mode else ("trusted", "drop-request", "drop-response", "reorder", "trailers", "key-update")):
            failures, requested, credits = [], [], []
            stopped = threading.Event()
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
                listener.bind(("127.0.0.1", 0))
                listener.settimeout(0.01)

                def peer():
                    connection, http, dropped, pending_body = None, None, False, False
                    try:
                        while not stopped.is_set():
                            now = time.monotonic()
                            try:
                                packet, address = listener.recvfrom(8192)
                            except socket.timeout:
                                if connection is not None:
                                    timer = connection.get_timer()
                                    if timer is not None and timer <= now:
                                        connection.handle_timer(now)
                                    for response, target in connection.datagrams_to_send(now):
                                        listener.sendto(response, target)
                                continue
                            header = pull_quic_header(Buffer(data=packet), host_cid_length=8)
                            if mode == "drop-request" and header.packet_type == QuicPacketType.ONE_RTT and not dropped:
                                dropped = True
                                continue
                            if connection is None:
                                connection = QuicConnection(configuration=config, original_destination_connection_id=header.destination_cid)
                                http = H3Connection(connection)
                            connection.receive_datagram(packet, address, now)
                            while (event := connection.next_event()) is not None:
                                for message in http.handle_event(event):
                                    if isinstance(message, HeadersReceived) and message.stream_id == 0:
                                        assert message.stream_ended and (b":path", b"/streams") in message.headers, message
                                        requested.append(message)
                                        http.send_headers(0, [(b":status", b"200"), (b"content-length", str(len(body_bytes)).encode())])
                                        if mode == "key-update":
                                            pending_body = True
                                        else:
                                            http.send_data(0, body_bytes, end_stream=mode != "trailers")
                                        if mode == "trailers":
                                            http.send_headers(0, [(b"x-trailer", b"done")], end_stream=True)
                            if pending_body and len(requested) == 1 and not connection._spaces[Epoch.ONE_RTT].sent_packets:
                                connection.request_key_update()
                                http.send_data(0, body_bytes, end_stream=True)
                                pending_body = False
                            responses = connection.datagrams_to_send(now)
                            if requested and 0 in connection._streams:
                                credits.append(connection._streams[0].max_stream_data_remote)
                            if mode == "drop-response" and requested and not dropped and responses:
                                responses = responses[1:]
                                dropped = True
                            if mode == "reorder" and requested:
                                responses = list(reversed(responses))
                            for response, target in responses:
                                listener.sendto(response, target)
                    except BaseException as error:
                        failures.append(error)

                thread = threading.Thread(target=peer)
                thread.start()
                try:
                    result = subprocess.run([str(binary), str(listener.getsockname()[1])], cwd=ROOT,
                        text=True, capture_output=True, timeout=20)
                finally:
                    stopped.set()
                    thread.join(timeout=2)
                assert not thread.is_alive() and not failures, (mode, failures)
                assert result.returncode == 0 and not result.stderr, (mode, result.returncode, result.stderr)
                lines = result.stdout.splitlines()
                assert lines and lines[-1] == "DONE", (mode, lines[-3:])
                frames, body, headers = Buffer(data=bytes.fromhex("".join(lines[:-1]))), b"", []
                decoder = pylsqpack.Decoder(0, 0)
                while not frames.eof():
                    kind, size = frames.pull_uint_var(), frames.pull_uint_var()
                    data = frames.pull_bytes(size)
                    if kind == 0:
                        body += data
                    elif kind == 1:
                        _, values = decoder.feed_header(0, data)
                        headers.append(values)
                assert requested and body == body_bytes and (b":status", b"200") in headers[0], (mode, len(body), headers)
                if len(body_bytes) > 65536:
                    assert credits and max(credits) > 65536, (mode, credits)
                if mode == "trailers":
                    assert headers[-1] == [(b"x-trailer", b"done")], headers
    print(f"Production QUIC streams: live HTTP/3 request, {len(body_bytes)}-byte response, {options.mode or 'all loss/reordering/trailer modes'} and cleanup passed")


if __name__ == "__main__":
    main()
