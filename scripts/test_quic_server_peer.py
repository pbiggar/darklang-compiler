#!/usr/bin/env python3
"""Check Dark's address-validated QUIC server and HTTP/3 ownership against aioquic."""

import argparse
import select
import socket
import subprocess
import tempfile
import time
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.h3.connection import H3Connection
from aioquic.h3.events import DataReceived, HeadersReceived
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.events import HandshakeCompleted, ConnectionTerminated
from aioquic.quic.packet import QuicPacketType, pull_quic_header
from cryptography.hazmat.primitives import serialization

from test_quic_tls_peer import certificates

ROOT = Path(__file__).resolve().parents[1]
SERVER = '''// server.dark - Retry, authenticated QUIC, HTTP/3 body flow and owned disposal.
let listen (socket: Stdlib.__Datagram.Socket) (signals: Stdlib.__Network.ShutdownSignals) (identity: Stdlib.TlsServerIdentity.Identity) (secret: Blob) : Stdlib.Result.Result<Stdlib.__QuicServer.Ready, String> =
  match Stdlib.__Datagram.receive socket 100L with
  | Error _ -> listen socket signals identity secret
  | Ok message -> match Stdlib.__Network.monotonicMillis () with
    | Error _ -> Error "clock"
    | Ok now -> match Stdlib.__QuicPacket.parse 0L message.payload with
      | Ok (Protected packet) ->
        if packet.kind != Stdlib.__QuicPacket.Kind.Initial || Stdlib.Blob.__byteLength message.payload < 1200L then listen socket signals identity secret
        else if Stdlib.Blob.__byteLength packet.token == 0L then
          Stdlib.Crypto.__secureRandomBytes 8L |> Stdlib.Result.andThen (fun source ->
            Stdlib.__QuicServerRetry.create secret message.peer packet.destination packet.source source now |> Stdlib.Result.andThen (fun retry ->
              let _ = Stdlib.__Datagram.send socket message.peer retry 1000L in
              listen socket signals identity secret))
        else Stdlib.__QuicServer.start identity secret message now |> Stdlib.Result.andThen (fun flight -> Stdlib.__QuicServer.accept socket message.peer signals flight (now + 10000L) 4096L)
      | _ -> listen socket signals identity secret
let collect (state: Stdlib.__Http3.State) (chunks: List<Blob>) : Stdlib.Result.Result<(Stdlib.__Http3.State * Blob), String> =
  Stdlib.__Http3.receive state |> Stdlib.Result.andThen (fun received ->
    let chunks = Stdlib.List.fold received.events chunks (fun chunks event -> match event with | BodyChunk bytes -> Stdlib.List.push chunks bytes | _ -> chunks) in
    if received.finished then Ok ((received.state, Stdlib.Blob.concat (Stdlib.List.reverse chunks))) else collect received.state chunks)
let write (state: Stdlib.__Http3.State) (bytes: Blob) (offset: Int64) : Stdlib.Result.Result<Stdlib.__Http3.State, String> =
  let length = Stdlib.Blob.__byteLength bytes in
  if offset == length then Stdlib.__Http3.write state Stdlib.Blob.empty true
  else
    let credit = Stdlib.__Http3.sendCredit state in
    if credit < 8L then Stdlib.__Http3.poll state |> Stdlib.Result.andThen (fun received -> write received.state bytes offset)
    else
      let size = if length - offset < credit - 8L then length - offset else credit - 8L in
      Stdlib.__Http3Wire.serialize 0L (Stdlib.__Http2Wire.__slice bytes offset size) |> Stdlib.Result.andThen (fun frame ->
        Stdlib.__Http3.write state frame false |> Stdlib.Result.andThen (fun next -> write next bytes (offset + size)))
let drain (state: Stdlib.__QuicConnection.State) (count: Int64) : Unit =
  if count == 0L then Stdlib.__QuicConnection.close state
  else match Stdlib.__QuicConnection.poll state 10L with | Error _ -> Stdlib.__QuicConnection.close state | Ok next -> drain next (count - 1L)
let exchange (ready: Stdlib.__QuicServer.Ready) (signals: Stdlib.__Network.ShutdownSignals) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Http3.initializeServer ready signals 10000L |> Stdlib.Result.andThen (fun state ->
    collect state [] |> Stdlib.Result.andThen (fun collected ->
      let (state, body) = collected in
      Stdlib.__Qpack.encode [(":status", "200"), ("content-length", Stdlib.Int64.toString (Stdlib.Blob.__byteLength body)), ("set-cookie", "a=1"), ("set-cookie", "b=2")] |> Stdlib.Result.andThen (Stdlib.__Http3Wire.serialize 1L)
        |> Stdlib.Result.andThen (fun headers -> Stdlib.__Http3.write state headers false)
        |> Stdlib.Result.andThen (fun state -> write state body 0L)
        |> Stdlib.Result.map (fun state -> drain state.connection 100L)))
let run () : Unit =
  let port = Stdlib.Cli.Args.get 0 |> Stdlib.Result.andThen (fun value -> Stdlib.Int64.parse value |> Stdlib.Result.mapError (fun _ -> "port")) |> Stdlib.Result.withDefault 0L in
  match Stdlib.Blob.fromHex "@CERT@", Stdlib.Blob.fromHex "@KEY@", Stdlib.__Datagram.bind4 [127L,0L,0L,1L] port, Stdlib.__Network.shutdownSignals (), Stdlib.Crypto.__secureRandomBytes 32L with
  | Ok certificate, Ok key, Ok socket, Ok signals, Ok secret ->
    let result = Stdlib.TlsServerIdentity.create certificate key |> Stdlib.Result.andThen (fun identity ->
      Stdlib.printLine "LISTENING"
      listen socket signals identity secret |> Stdlib.Result.andThen (fun ready -> exchange ready signals)) in
    Stdlib.__Datagram.close socket
    Stdlib.__Network.closeShutdownSignals signals
    match result with | Ok () -> Stdlib.printLine "DONE" | Error message -> Stdlib.printLine ("ERROR " ++ message)
  | _ -> Stdlib.printLine "initialization failed"
run ()
'''


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    pem = leaf.public_bytes(serialization.Encoding.PEM)
    private = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    with tempfile.TemporaryDirectory(prefix="dark-quic-server-") as temporary:
        source, binary = Path(temporary) / "server.dark", Path(temporary) / "server"
        source.write_text(SERVER.replace("@CERT@", pem.hex()).replace("@KEY@", private.hex()))
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for mode in ("valid", "large", "drop-initial", "drop-handshake", "duplicate", "corrupt", "zero-server-bidi"):
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as reservation:
                reservation.bind(("127.0.0.1", 0))
                port = reservation.getsockname()[1]
            process = subprocess.Popen([str(binary), str(port)], cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                assert select.select([process.stdout], [], [], 5)[0], mode
                assert process.stdout.readline() == b"LISTENING\n", process.communicate(timeout=5)
                config = QuicConfiguration(is_client=True, alpn_protocols=["h3"], server_name="localhost", cadata=ca.public_bytes(serialization.Encoding.PEM))
                client = QuicConnection(configuration=config)
                if mode == "zero-server-bidi":
                    client._local_max_streams_bidi.value = 0
                http = H3Connection(client)
                body = bytes(range(251)) * 279 if mode == "large" else b"HTTP/3 server"
                response, headers, complete, sent, dropped, retry = bytearray(), [], False, False, False, False
                with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
                    peer.bind(("127.0.0.1", 0))
                    peer.settimeout(0.01)
                    client.connect(("127.0.0.1", port), time.monotonic())
                    deadline = time.monotonic() + 12
                    while not complete and time.monotonic() < deadline:
                        now = time.monotonic()
                        timer = client.get_timer()
                        if timer is not None and timer <= now:
                            client.handle_timer(now)
                        for data, address in client.datagrams_to_send(now):
                            peer.sendto(data, address)
                        try:
                            data, address = peer.recvfrom(8192)
                        except socket.timeout:
                            continue
                        header = pull_quic_header(Buffer(data=data), host_cid_length=8)
                        retry |= header.packet_type == QuicPacketType.RETRY
                        target = ((mode == "drop-initial" and header.packet_type == QuicPacketType.INITIAL) or
                                  (mode == "drop-handshake" and header.packet_type == QuicPacketType.HANDSHAKE))
                        if target and not dropped:
                            dropped = True
                            continue
                        if mode == "corrupt" and header.packet_type == QuicPacketType.HANDSHAKE and not dropped:
                            client.receive_datagram(data[:-1] + bytes([data[-1] ^ 1]), address, now)
                            dropped = True
                        if mode == "duplicate":
                            client.receive_datagram(data, address, now)
                        client.receive_datagram(data, address, now)
                        while (event := client.next_event()) is not None:
                            assert not isinstance(event, ConnectionTerminated), (mode, event)
                            if isinstance(event, HandshakeCompleted) and not sent:
                                assert event.alpn_protocol == "h3"
                                http.send_headers(0, [(b":method", b"POST"), (b":scheme", b"https"), (b":authority", b"localhost"), (b":path", b"/echo"), (b"content-length", str(len(body)).encode())])
                                http.send_data(0, body, end_stream=True)
                                sent = True
                            for received in http.handle_event(event):
                                if isinstance(received, HeadersReceived):
                                    headers.extend(received.headers)
                                    complete |= received.stream_ended
                                elif isinstance(received, DataReceived):
                                    response.extend(received.data)
                                    complete |= received.stream_ended
                    assert complete and sent and retry and response == body, (mode, complete, sent, retry, len(response))
                    assert (b":status", b"200") in headers and headers.count((b"set-cookie", b"a=1")) == 1 and headers.count((b"set-cookie", b"b=2")) == 1, headers
                    for data, address in client.datagrams_to_send(time.monotonic()):
                        peer.sendto(data, address)
                stdout, stderr = process.communicate(timeout=5)
                assert process.returncode == 0 and stdout == b"DONE\n" and not stderr, (mode, process.returncode, stdout, stderr)
                print(f"{mode}: Retry, authenticated handshake and {len(body)} byte HTTP/3 echo; zero leaks", flush=True)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.communicate()


if __name__ == "__main__":
    main()
