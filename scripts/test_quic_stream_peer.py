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
from aioquic.h3.events import DataReceived, HeadersReceived
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
let initialize (ready: Stdlib.__QuicClient.Ready) : Stdlib.Result.Result<Stdlib.__QuicConnection.State, String> =
  Stdlib.__QuicConnection.fromClient ready |> Stdlib.Result.andThen (fun state ->
    Stdlib.__QuicConnection.openStream state 2L |> Stdlib.Result.andThen (fun state ->
    Stdlib.__QuicConnection.openStream state 6L |> Stdlib.Result.andThen (fun state ->
    Stdlib.__QuicConnection.openStream state 10L |> Stdlib.Result.andThen (fun state ->
    Stdlib.__QuicConnection.openStream state 0L |> Stdlib.Result.andThen (fun state ->
      match Stdlib.__Http3Wire.serialize 4L (Stdlib.__Http3Wire.settings ()),
        Stdlib.__Qpack.encode [(":method", "GET"), (":scheme", "https"), (":authority", "localhost"), (":path", "/streams")] |> Stdlib.Result.andThen (Stdlib.__Http3Wire.serialize 1L) with
      | Ok settings, Ok request ->
        Stdlib.__QuicConnection.write state 2L (Stdlib.Blob.concat [Stdlib.String.toBlob "\\0", settings]) false |> Stdlib.Result.andThen (fun state ->
        Stdlib.Blob.fromHex "02" |> Stdlib.Result.andThen (fun marker -> Stdlib.__QuicConnection.write state 6L marker false) |> Stdlib.Result.andThen (fun state ->
        Stdlib.Blob.fromHex "03" |> Stdlib.Result.andThen (fun marker -> Stdlib.__QuicConnection.write state 10L marker false) |> Stdlib.Result.andThen (fun state ->
        Stdlib.__QuicConnection.write state 0L request true)))
      | _, _ -> Error "HTTP/3 encoding failed")))))
let controls (state: Stdlib.__QuicConnection.State) (uni: Stdlib.__Http3Uni.State)
  : Stdlib.Result.Result<(Stdlib.__QuicConnection.State * Stdlib.__Http3Uni.State), String> =
  Stdlib.List.fold state.flow.channels (Ok ((state, uni))) (fun result channel ->
    if channel.id % 4L != 3L then result
    else result |> Stdlib.Result.andThen (fun pair ->
      let (connection, streams) = pair in
      Stdlib.__QuicConnection.read connection channel.id |> Stdlib.Result.andThen (fun read ->
        Stdlib.__Http3Uni.receive streams channel.id read.bytes read.finished (Stdlib.Option.isSome read.reset)
          |> Stdlib.Result.map (fun received -> (read.state, received.state)))))
let receive (state: Stdlib.__QuicConnection.State) (deadline: Int64)
  (message: Stdlib.__Http3Message.State) (uni: Stdlib.__Http3Uni.State)
  : Stdlib.Result.Result<Unit, String> =
  controls state uni |> Stdlib.Result.andThen (fun pair ->
  let (connection, streams) = pair in
  Stdlib.__QuicConnection.read connection 0L |> Stdlib.Result.andThen (fun read ->
    let _ = emit read.bytes 0L in
    if Stdlib.Option.isSome read.reset then Error "Response reset"
    else Stdlib.__Http3Message.feed message read.bytes |> Stdlib.Result.andThen (fun received ->
      if read.finished then
        Stdlib.__Http3Message.finish received.state |> Stdlib.Result.andThen (fun _unit ->
          Stdlib.__QuicConnection.flush read.state |> Stdlib.Result.map (fun _state -> ()))
      else match Stdlib.__Network.monotonicMillis () with
      | Error _ -> Error "Clock failed"
      | Ok now -> if now >= deadline then Error "Response timed out"
                  else Stdlib.__QuicConnection.poll read.state 10L |> Stdlib.Result.andThen (fun next ->
                    receive next deadline received.state streams))))
let run () : Unit =
  match Stdlib.Blob.fromHex "@ROOT@", Stdlib.Cli.__Args.get 0 |> Stdlib.Result.andThen (fun value -> Stdlib.Int64.parse value |> Stdlib.Result.mapError (fun _error -> "port")) with
  | Ok root, Ok port ->
    match Stdlib.__QuicClient.connect (Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = port }) "localhost" [root] 8000L with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok ready ->
      let result = initialize ready |> Stdlib.Result.andThen (fun state ->
        match Stdlib.__Network.monotonicMillis () with | Error _ -> Error "Clock failed" | Ok now ->
          receive state (now + 10000L) (Stdlib.__Http3Message.create (Response false)) (Stdlib.__Http3Uni.create true)) in
      let _ = Stdlib.__QuicClient.close ready in
      match result with | Error message -> Stdlib.printLine ("ERROR " ++ message) | Ok () -> Stdlib.printLine "DONE"
  | _ -> Stdlib.printLine "Bad arguments"
run ()
"""


HTTP_CLIENT_DARK = """// client.dark - Exercise lazy HTTP/3 response ownership and early close.
let output (kind: Int64) (bytes: Blob) : Unit =
  match Stdlib.__Http3Wire.serialize kind bytes with
  | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | Ok wire -> Stdlib.printLine (Stdlib.Blob.toHex wire)
let consume (body: Stream<UInt8>) (bytes: List<UInt8>) (count: Int) : Unit =
  if count == 512 then
    let _ = output 0L (Stdlib.Blob.fromList (Stdlib.List.reverse bytes)) in
    consume body [] 0
  else match Stdlib.Stream.next body with
  | Some byte -> consume body (Stdlib.List.push bytes byte) (count + 1)
  | None -> if count == 0 then () else output 0L (Stdlib.Blob.fromList (Stdlib.List.reverse bytes))
let emitBlob (body: Blob) (position: Int) : Unit =
  if position >= Stdlib.Blob.length body then ()
  else
    let _ = output 0L (Stdlib.Blob.slice body position 512) in
    emitBlob body (position + 512)
let run () : Unit =
  match Stdlib.Blob.fromHex "@ROOT@", Stdlib.__HttpWire.parseUrl "https://localhost/streams", Stdlib.Blob.fromHex "@BODY@",
    Stdlib.Cli.__Args.get 0 |> Stdlib.Result.andThen (fun value -> Stdlib.Int64.parse value |> Stdlib.Result.mapError (fun _error -> "port")) with
  | Ok root, Ok url, Ok upload, Ok port ->
    match Stdlib.__QuicClient.connect (Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = port }) "localhost" [root] 8000L with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok ready ->
      match Stdlib.__Http3Client.stream ready (if Stdlib.Blob.length upload == 0 then "GET" else "POST") url [] upload 10000L with
      | Error message -> Stdlib.printLine ("ERROR " ++ message)
      | Ok response ->
        let _ = match Stdlib.__Qpack.encode ([(":status", Stdlib.Int.toString response.statusCode)] @ response.headers) with
          | Error message -> Stdlib.printLine ("ERROR " ++ message)
          | Ok wire -> output 1L wire in
        let _ = consume response.body [] 0 in
        let _ = Stdlib.Stream.close response.body in
        Stdlib.printLine "DONE"
  | _ -> Stdlib.printLine "Bad arguments"
run ()
"""

HTTP_OWNER_DARK = """// owner.dark - Exercise the production HTTP/3 connection owner and event parser.
let emit (bytes: Blob) (offset: Int) : Unit =
  if offset >= Stdlib.Blob.length bytes then ()
  else
    let _ = Stdlib.printLine (Stdlib.Blob.toHex (Stdlib.Blob.slice bytes offset 512)) in
    emit bytes (offset + 512)
let frame (kind: Int64) (bytes: Blob) : Unit =
  match Stdlib.__Http3Wire.serialize kind bytes with
  | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | Ok bytes -> emit bytes 0
let fields (values: List<(String * String)>) : Unit =
  match Stdlib.__Qpack.encode values with
  | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | Ok bytes -> frame 1L bytes
let event (value: Stdlib.__Http3Message.Event) : Unit =
  match value with
  | BodyChunk bytes -> frame 0L bytes
  | ResponseHead head -> fields ([(":status", Stdlib.Int64.toString head.status)] @ head.headers)
  | TrailerFields trailers -> fields trailers
  | RequestHead _ -> Stdlib.printLine "ERROR request event"
let receive (state: Stdlib.__Http3.State) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Http3.receive state |> Stdlib.Result.andThen (fun received ->
    let _ = Stdlib.List.iter received.events event in
    if received.finished then Ok () else receive received.state)
let run () : Unit =
  match Stdlib.Blob.fromHex "@ROOT@", Stdlib.Cli.__Args.get 0 |> Stdlib.Result.andThen (fun value -> Stdlib.Int64.parse value |> Stdlib.Result.mapError (fun _error -> "port")) with
  | Ok root, Ok port ->
    match Stdlib.__QuicClient.connect (Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = port }) "localhost" [root] 8000L with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok ready ->
      let result = Stdlib.__Http3.initialize ready false 10000L |> Stdlib.Result.andThen (fun state ->
        Stdlib.__Qpack.encode [(":method", "GET"), (":scheme", "https"), (":authority", "localhost"), (":path", "/streams")]
          |> Stdlib.Result.andThen (Stdlib.__Http3Wire.serialize 1L)
          |> Stdlib.Result.andThen (fun request -> Stdlib.__Http3.write state request true)
          |> Stdlib.Result.andThen receive) in
      let _ = Stdlib.__QuicClient.close ready in
      match result with | Error message -> Stdlib.printLine ("ERROR " ++ message) | Ok () -> Stdlib.printLine "DONE"
  | _ -> Stdlib.printLine "Bad arguments"
run ()
"""


def main():
    arguments = argparse.ArgumentParser()
    arguments.add_argument("--body-size", type=int, default=len(BODY))
    arguments.add_argument("--request-size", type=int, default=0, help="Upload this many bytes through the HTTP client adapter")
    arguments.add_argument("--compiler", type=Path, default=ROOT / "dark", help="Compiler executable to verify")
    arguments.add_argument("--http-owner", action="store_true", help="Use the production HTTP/3 owner instead of the transport-only driver")
    arguments.add_argument("--http-client", action="store_true", help="Use the production HTTP/3 lazy response adapter")
    arguments.add_argument("--discovery", action="store_true", help="Authenticate the original host through an advertised alternate target")
    arguments.add_argument("--buffered", action="store_true", help="Use the buffered client response adapter")
    arguments.add_argument("--close-early", action="store_true", help="Close the lazy response after its headers")
    arguments.add_argument("--mode", choices=("trusted", "drop-request", "drop-response", "reorder", "trailers", "key-update", "early-response"))
    options = arguments.parse_args()
    assert options.request_size >= 0 and options.body_size >= 0
    assert not options.request_size or options.http_client, "Uploads require --http-client"
    assert not options.buffered or options.http_client and not options.close_early
    assert not options.close_early or options.http_client
    assert not options.discovery or options.http_client
    body_bytes = (BODY * ((options.body_size + len(BODY) - 1) // len(BODY)))[:options.body_size]
    upload_bytes = (BODY * ((options.request_size + len(BODY) - 1) // len(BODY)))[:options.request_size]
    key, leaf, ca = certificates()
    config = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    config.certificate, config.private_key = leaf, key
    with tempfile.TemporaryDirectory(prefix="dark-quic-streams-") as temporary:
        source, binary = Path(temporary) / "streams.dark", Path(temporary) / "streams"
        program = HTTP_CLIENT_DARK if options.http_client else HTTP_OWNER_DARK if options.http_owner else DARK
        if options.discovery:
            helper = '''let advertised (root: Blob) (port: Int64) : Stdlib.Result.Result<Stdlib.__QuicClient.Ready, String> =
  Stdlib.Blob.fromHex "000103616C74076578616D706C6507696E76616C69640000010003026833000400047F000001"
    |> Stdlib.Result.andThen (fun wire -> Stdlib.__HttpsService.parse "localhost" port wire)
    |> Stdlib.Result.andThen (fun record -> match record with
      | Some (Service service) -> Stdlib.__Http3Discovery.connect [service] "localhost" [root] true |> Stdlib.Result.fromOption "Advertised connection failed"
      | _ -> Error "Invalid advertisement")
'''
            program = program.replace("let run () : Unit =", helper + "let run () : Unit =")
            program = program.replace('Stdlib.__QuicClient.connect (Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = port }) "localhost" [root] 8000L', "advertised root port")
        if options.buffered:
            program = program.replace("Stdlib.__Http3Client.stream", "Stdlib.__Http3Client.request").replace("consume response.body [] 0", "emitBlob response.body 0").replace("Stdlib.Stream.close response.body", "()")
        elif options.close_early:
            program = program.replace("consume response.body [] 0", "()")
        source.write_text(program.replace("@ROOT@", ca.public_bytes(serialization.Encoding.DER).hex()).replace("@BODY@", upload_bytes.hex()))
        result = subprocess.run([str(options.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT,
            text=True, capture_output=True, timeout=120)
        assert result.returncode == 0, result.stdout + result.stderr
        for mode in ([options.mode] if options.mode else ("trusted", "drop-request", "drop-response", "reorder", "trailers", "key-update")):
            failures, requested, credits, uploaded, closes = [], [], [], bytearray(), []
            stopped = threading.Event()
            close_seen = threading.Event()
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
                listener.bind(("127.0.0.1", 0))
                listener.settimeout(0.01)

                def peer():
                    connection, http, dropped, pending_body = None, None, False, False
                    response_sent = False
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
                            if connection._close_event is not None and not closes:
                                closes.append(connection._close_event)
                                close_seen.set()
                            while (event := connection.next_event()) is not None:
                                for message in http.handle_event(event):
                                    if isinstance(message, HeadersReceived) and message.stream_id == 0:
                                        assert message.stream_ended == (not upload_bytes) and (b":path", b"/streams") in message.headers, message
                                        if options.http_client:
                                            assert (b"content-length", str(len(upload_bytes)).encode()) in message.headers, message
                                        requested.append(message)
                                    elif isinstance(message, DataReceived) and message.stream_id == 0:
                                        uploaded.extend(message.data)
                                    else:
                                        continue
                                    # A retransmitted empty FIN can surface another completion
                                    # event; this peer serves exactly one response per request.
                                    if not response_sent and (message.stream_ended or mode == "early-response" and isinstance(message, HeadersReceived)):
                                        response_sent = True
                                        if mode != "early-response":
                                            assert uploaded == upload_bytes, (len(uploaded), len(upload_bytes))
                                        # Early disposal must observe the new phase in HEADERS.
                                        # aioquic retires its receive phase when it initiates an
                                        # update, so updating only a later body loses a valid
                                        # close sent before the client has seen that update.
                                        if mode == "key-update" and options.close_early:
                                            connection.request_key_update()
                                        http.send_headers(0, [(b":status", b"200"), (b"content-length", str(len(body_bytes)).encode())])
                                        if mode == "key-update":
                                            pending_body = True
                                        else:
                                            http.send_data(0, body_bytes, end_stream=mode != "trailers")
                                        if mode == "trailers":
                                            http.send_headers(0, [(b"x-trailer", b"done")], end_stream=True)
                            # ACK-only packets do not elicit ACKs. Waiting for their
                            # removal deadlocks when the client flushes promptly.
                            if pending_body and len(requested) == 1 and not any(
                                packet.is_ack_eliciting for packet in connection._spaces[Epoch.ONE_RTT].sent_packets.values()
                            ):
                                if not options.close_early:
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
                        text=True, capture_output=True, timeout=60)
                finally:
                    if options.http_client:
                        close_seen.wait(timeout=1)
                    stopped.set()
                    thread.join(timeout=2)
                assert not thread.is_alive() and not failures, (mode, failures)
                if options.http_client:
                    assert closes and closes[0].error_code == 256, (mode, closes)
                assert result.returncode == 0 and not result.stderr, (mode, result.returncode, result.stderr, result.stdout[-1000:])
                lines = result.stdout.splitlines()
                assert lines and lines[-1] == "DONE", (mode, lines[-3:])
                assert all(line and all(character in "0123456789ABCDEFabcdef" for character in line) for line in lines[:-1]), (mode, [line for line in lines[:-1] if not all(character in "0123456789ABCDEFabcdef" for character in line)][:10])
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
                assert requested and body == (b"" if options.close_early else body_bytes) and (b":status", b"200") in headers[0], (mode, len(body), headers)
                if mode == "early-response":
                    assert upload_bytes and len(uploaded) < len(upload_bytes), (mode, len(uploaded))
                else:
                    assert uploaded == upload_bytes, (mode, len(uploaded), len(upload_bytes))
                if len(body_bytes) > 65536 and not options.close_early:
                    assert credits and max(credits) > 65536, (mode, credits)
                if mode == "trailers" and not options.http_client:
                    assert headers[-1] == [(b"x-trailer", b"done")], headers
    print(f"Production QUIC streams: live HTTP/3 request, {len(body_bytes)}-byte response, {options.mode or 'all loss/reordering/trailer modes'} and cleanup passed")


if __name__ == "__main__":
    main()
