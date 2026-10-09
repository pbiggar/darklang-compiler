#!/usr/bin/env python3
"""Authenticate a live QUIC TLS flight with compiled Dark and test-only aioquic."""

import datetime
import socket
import subprocess
import tempfile
import threading
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.events import HandshakeCompleted
from aioquic.quic.packet import QuicPacketType, QuicTransportParameters, pull_ack_frame, pull_quic_header, pull_quic_transport_parameters, push_quic_transport_parameters
from aioquic.tls import Epoch
from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID

ROOT = Path(__file__).resolve().parents[1]
DESTINATION, SOURCE = bytes(range(8)), bytes(range(8, 16))
PARAMETER_ERRORS = {
    "parameters": "QUIC original destination connection ID mismatch",
    "source": "QUIC initial source connection ID mismatch",
    "retry": "Unexpected QUIC Retry source connection ID",
    "missing": "Missing QUIC original destination connection ID",
    "duplicate": "Duplicate QUIC transport parameter",
    "ack-delay": "QUIC ACK delay exponent exceeds 20",
}
SUCCESS_MODES = ("trusted", "reordered", "unknown")

DARK = """// handshake.dark - Exercise authenticated Initial/Handshake CRYPTO streams over UDP.
type Exchange = {
  initial: Stdlib.__QuicReassembly.State, crypto: Stdlib.__QuicReassembly.State,
  tls: Stdlib.Option.Option<Stdlib.__QuicTls.Handshake>, destination: Blob,
  privateKey: Blob, hello: Blob, keys: Stdlib.__QuicCrypto.Initial,
  largestInitial: Int64, largestHandshake: Int64
}
let update (state: Exchange) (initial: Stdlib.__QuicReassembly.State) (crypto: Stdlib.__QuicReassembly.State)
  (tls: Stdlib.Option.Option<Stdlib.__QuicTls.Handshake>) (destination: Blob) (largestInitial: Int64) (largestHandshake: Int64) : Exchange =
  Exchange { initial = initial, crypto = crypto, tls = tls, destination = destination,
    privateKey = state.privateKey, hello = state.hello, keys = state.keys,
    largestInitial = largestInitial, largestHandshake = largestHandshake }
let collect (state: Stdlib.__QuicReassembly.State) (frames: List<Stdlib.__QuicFrames.Frame>)
  : Stdlib.Result.Result<Stdlib.__QuicReassembly.State, String> =
  match frames with
  | [] -> Ok state
  | Crypto frame :: tail ->
    Stdlib.__QuicReassembly.insert state frame.offset frame.bytes |> Stdlib.Result.andThen (fun next -> collect next tail)
  | Close _ :: _ -> Error "Peer closed QUIC"
  | _ :: tail -> collect state tail
let packets (state: Exchange) (bytes: Blob) : Stdlib.Result.Result<Exchange, String> =
  if Stdlib.Blob.length bytes == 0 then Ok state
  else
    match Stdlib.__QuicPacket.parse 8L bytes with
    | Error message -> Error message
    | Ok (Protected packet) ->
      if Stdlib.Blob.toHex packet.destination != "08090A0B0C0D0E0F" then Error "Wrong destination"
      else
        let result =
          match packet.kind with
          | Initial ->
            match Stdlib.__QuicCrypto.openPacket state.keys.server packet.bytes packet.numberOffset state.largestInitial with
            | Error message -> Error message
            | Ok opened ->
              if state.largestInitial >= 0L && Stdlib.Blob.toHex packet.source != Stdlib.Blob.toHex state.destination then
                Error "QUIC Initial source connection ID changed"
              else
              match Stdlib.__QuicFrames.parseHandshake opened.payload |> Stdlib.Result.andThen (collect state.initial) with
              | Error message -> Error message
              | Ok initial ->
                let available = Stdlib.__QuicReassembly.available initial in
                let largest = if opened.number > state.largestInitial then opened.number else state.largestInitial in
                match state.tls with
                | Some _ ->
                  if Stdlib.Blob.length available != 0 then Error "Extra Initial CRYPTO bytes"
                  else Ok (update state initial state.crypto state.tls state.destination largest state.largestHandshake)
                | None ->
                  match Stdlib.__Tls13.parseServerHello available with
                  | Error Incomplete -> Ok (update state initial state.crypto None packet.source largest state.largestHandshake)
                  | Error _ -> Error "Invalid server hello"
                  | Ok _ ->
                    Stdlib.__QuicTls.start state.privateKey state.hello available |> Stdlib.Result.map (fun tls -> update state (Stdlib.__QuicReassembly.drain initial) state.crypto (Some tls) packet.source largest state.largestHandshake)
          | Handshake ->
            match state.tls with
            | None -> Error "No handshake keys"
            | Some tls ->
              match Stdlib.__QuicCrypto.openPacket tls.receive packet.bytes packet.numberOffset state.largestHandshake with
              | Error message -> Error message
              | Ok opened ->
                match Stdlib.__QuicFrames.parseHandshake opened.payload |> Stdlib.Result.andThen (collect state.crypto) with
                | Error message -> Error message
                | Ok crypto ->
                  let available = Stdlib.__QuicReassembly.available crypto in
                  let largest = if opened.number > state.largestHandshake then opened.number else state.largestHandshake in
                  if Stdlib.Blob.length available == 0 then Ok (update state state.initial crypto state.tls state.destination state.largestInitial largest)
                  else Stdlib.__QuicTls.feed tls available |> Stdlib.Result.map (fun next -> update state state.initial (Stdlib.__QuicReassembly.drain crypto) (Some next) state.destination state.largestInitial largest)
          | _ -> Error "Unexpected QUIC packet level" in
        result |> Stdlib.Result.andThen (fun next -> packets next packet.remaining)
    | Ok _ -> Error "Unexpected QUIC control packet"
let receive (connection: Stdlib.__Datagram.Socket) (state: Exchange) (remaining: Int64)
  : Stdlib.Result.Result<Exchange, String> =
  let complete = match state.tls with | Some tls -> tls.flight.phase == Stdlib.__Tls13Handshake.FlightPhase.ServerFinished | None -> false in
  if complete then Ok state
  else if remaining == 0L then Error "Too many handshake datagrams"
  else
    match Stdlib.__Datagram.receive connection 1000L with
    | Error _ -> Error "QUIC handshake receive failed"
    | Ok datagram -> packets state datagram.payload |> Stdlib.Result.andThen (fun next -> receive connection next (remaining - 1L))
let exchange (connection: Stdlib.__Datagram.Socket) (peer: Stdlib.__Datagram.Endpoint) (state: Exchange)
  (source: Blob) (roots: List<Blob>) (host: String) : Stdlib.Result.Result<Unit, String> =
  match Stdlib.__QuicFrames.crypto 0L state.hello with
  | Error message -> Error message
  | Ok frame ->
    match Stdlib.__QuicPacket.sealLong Stdlib.__QuicPacket.Kind.Initial state.keys.client state.destination source Stdlib.Blob.empty frame 0L with
    | Error message -> Error message
    | Ok packet ->
      match Stdlib.__Datagram.send connection peer packet 1000L with
      | Error _ -> Error "QUIC Initial send failed"
      | Ok () ->
        match receive connection state 16L with
        | Error message -> Error message
        | Ok received ->
          match received.tls with
          | None -> Error "Missing TLS flight"
          | Some tls ->
            let ids = Stdlib.__QuicParameters.ConnectionIds {
              originalDestination = state.destination, initialSource = received.destination, retrySource = None } in
            match Stdlib.__QuicTls.authenticate tls host roots ids with
            | Error message -> Error message
            | Ok authenticated ->
              match Stdlib.__QuicFrames.crypto 0L authenticated.finished, Stdlib.__QuicFrames.acknowledge received.largestHandshake with
              | Ok finished, Ok ack ->
                match Stdlib.__QuicPacket.sealLong Stdlib.__QuicPacket.Kind.Handshake tls.send received.destination source Stdlib.Blob.empty (Stdlib.Blob.concat [ack, finished]) 0L with
                | Error message -> Error message
                | Ok packet ->
                  match Stdlib.__Datagram.send connection peer packet 1000L with
                  | Error _ -> Error "QUIC Finished send failed"
                  | Ok () -> Ok ()
              | _, _ -> Error "QUIC Finished framing failed"
let runExchange () : Unit =
  match Stdlib.Blob.fromHex "@PARAMETERS@", Stdlib.Blob.fromHex "@ROOT@",
        Stdlib.Blob.fromHex "0001020304050607", Stdlib.Blob.fromHex "08090A0B0C0D0E0F",
        Stdlib.__X25519.generateKeyPair (), Stdlib.Crypto.__secureRandomBytes 32L,
        Stdlib.Cli.Args.get 0, Stdlib.Cli.Args.get 1 |> Stdlib.Result.andThen (fun value -> Stdlib.Int64.parse value |> Stdlib.Result.mapError (fun _error -> "Invalid port")) with
  | Ok parameters, Ok root, Ok destination, Ok source, Ok pair, Ok random, Ok mode, Ok port ->
    match Stdlib.__Tls13.quicClientHello "localhost" random pair.publicKey parameters, Stdlib.__QuicCrypto.initial destination, Stdlib.__QuicReassembly.create 1048576L, Stdlib.__Datagram.bind4 [127L, 0L, 0L, 1L] 0L with
    | Ok hello, Ok keys, Ok initial, Ok connection ->
      let state = Exchange { initial = initial, crypto = initial, tls = None, destination = destination, privateKey = pair.privateKey, hello = hello, keys = keys, largestInitial = -1L, largestHandshake = -1L } in
      let peer = Stdlib.__Datagram.Endpoint { address = [127L, 0L, 0L, 1L], port = port } in
      let result = exchange connection peer state source (if mode == "untrusted" then [] else [root]) (if mode == "hostname" then "wrong.example.com" else "localhost") in
      let _ = Stdlib.__Datagram.close connection in
      match result with | Ok () -> Stdlib.printLine "AUTHENTICATED" | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | _, _, _, Ok connection -> let _ = Stdlib.__Datagram.close connection in Stdlib.printLine "Setup failed"
    | _, _, _, _ -> Stdlib.printLine "Setup failed"
  | _ -> Stdlib.printLine "Bad arguments"
runExchange ()
"""


def certificates():
    now = datetime.datetime.now(datetime.timezone.utc)
    ca_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    ca_name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "QUIC TLS test CA")])
    ca = (x509.CertificateBuilder().subject_name(ca_name).issuer_name(ca_name).public_key(ca_key.public_key())
          .serial_number(x509.random_serial_number()).not_valid_before(now - datetime.timedelta(minutes=1))
          .not_valid_after(now + datetime.timedelta(days=1)).add_extension(x509.BasicConstraints(ca=True, path_length=1), critical=True)
          .add_extension(x509.KeyUsage(False, False, False, False, False, True, True, False, False), critical=True)
          .sign(ca_key, hashes.SHA256()))
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    leaf = (x509.CertificateBuilder().subject_name(name).issuer_name(ca_name).public_key(key.public_key())
            .serial_number(x509.random_serial_number()).not_valid_before(now - datetime.timedelta(minutes=1))
            .not_valid_after(now + datetime.timedelta(days=1)).add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
            .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), critical=False)
            .add_extension(x509.ExtendedKeyUsage([ExtendedKeyUsageOID.SERVER_AUTH]), critical=False).sign(ca_key, hashes.SHA256()))
    return key, leaf, ca


def main():
    parameters = Buffer(capacity=1024)
    push_quic_transport_parameters(parameters, QuicTransportParameters(
        initial_source_connection_id=SOURCE, max_udp_payload_size=4096, initial_max_data=104857600,
        initial_max_stream_data_bidi_local=104857600, initial_max_stream_data_bidi_remote=104857600,
        initial_max_stream_data_uni=65536, initial_max_streams_bidi=1, initial_max_streams_uni=3,
        disable_active_migration=True))
    key, leaf, ca = certificates()
    configuration = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    configuration.certificate, configuration.private_key = leaf, key
    with tempfile.TemporaryDirectory(prefix="dark-quic-tls-") as temporary:
        source, binary = Path(temporary) / "handshake.dark", Path(temporary) / "handshake"
        source.write_text(DARK.replace("@PARAMETERS@", parameters.data.hex()).replace("@ROOT@", ca.public_bytes(serialization.Encoding.DER).hex()))
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for mode in (*SUCCESS_MODES, *PARAMETER_ERRORS, "untrusted", "hostname", "finished"):
            failures = []
            with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
                listener.bind(("127.0.0.1", 0))
                listener.settimeout(5)

                def peer():
                    try:
                        packet, address = listener.recvfrom(8192)
                        connection = QuicConnection(configuration=configuration, original_destination_connection_id=DESTINATION)
                        if mode in PARAMETER_ERRORS or mode == "unknown":
                            serialize = connection._serialize_transport_parameters

                            def malformed_parameters():
                                values = pull_quic_transport_parameters(Buffer(data=serialize()))
                                if mode == "parameters":
                                    values.original_destination_connection_id = bytes(reversed(DESTINATION))
                                elif mode == "source":
                                    cid = values.initial_source_connection_id
                                    values.initial_source_connection_id = bytes([cid[0] ^ 1]) + cid[1:]
                                elif mode == "retry":
                                    values.retry_source_connection_id = b"\x01"
                                elif mode == "missing":
                                    values.original_destination_connection_id = None
                                elif mode == "ack-delay":
                                    values.ack_delay_exponent = 21
                                encoded = Buffer(capacity=4096)
                                push_quic_transport_parameters(encoded, values)
                                if mode == "duplicate":
                                    return encoded.data + b"\x0f\x00"
                                if mode == "unknown":
                                    return encoded.data + b"\x40\x63\x01\x00"
                                return encoded.data

                            # Sign a wrong connection ID in the real TLS flight.
                            connection._serialize_transport_parameters = malformed_parameters
                        connection.receive_datagram(packet, address, now=0)
                        responses = connection.datagrams_to_send(now=0)
                        if mode == "finished":
                            # The peer owns the real handshake traffic secret.
                            # Change the last CRYPTO byte and re-authenticate the
                            # packet, so TLS (rather than packet AEAD) must reject it.
                            candidates = []
                            context = connection._cryptos[Epoch.HANDSHAKE].send
                            for index, (response, target) in enumerate(responses):
                                buf = Buffer(data=response)
                                while not buf.eof():
                                    start = buf.tell()
                                    header = pull_quic_header(buf, host_cid_length=8)
                                    offset, end = buf.tell() - start, start + header.packet_length
                                    if header.packet_type == QuicPacketType.HANDSHAKE:
                                        raw = response[start:end]
                                        plain, payload, number, _ = context.decrypt_packet(raw, offset, 0)
                                        frames = Buffer(data=payload)
                                        while not frames.eof():
                                            kind = frames.pull_uint_var()
                                            if kind in (0, 1):
                                                continue
                                            if kind in (2, 3):
                                                pull_ack_frame(frames)
                                                if kind == 3:
                                                    for _ in range(3):
                                                        frames.pull_uint_var()
                                            elif kind == 6:
                                                stream_offset, size = frames.pull_uint_var(), frames.pull_uint_var()
                                                data_start = frames.tell()
                                                if size:
                                                    candidates.append((stream_offset + size, index, start, end, plain, payload, number, data_start + size - 1))
                                                frames.seek(data_start + size)
                                            else:
                                                raise AssertionError(kind)
                                    buf.seek(end)
                            _, index, start, end, plain, payload, number, position = max(candidates, key=lambda item: item[0])
                            changed = bytearray(payload)
                            changed[position] ^= 1
                            sealed = context.encrypt_packet(plain, bytes(changed), number)
                            response, target = responses[index]
                            responses[index] = (response[:start] + sealed + response[end:], target)
                        if mode == "reordered":
                            # Keep the Initial first so handshake keys exist,
                            # then duplicate it and reverse later CRYPTO chunks.
                            responses = [responses[0], responses[0], *reversed(responses[1:])]
                        for response, target in responses:
                            listener.sendto(response, target)
                        if mode in SUCCESS_MODES:
                            finished, _ = listener.recvfrom(8192)
                            connection.receive_datagram(finished, address, now=0.1)
                            events = []
                            while (event := connection.next_event()) is not None:
                                events.append(event)
                            assert any(isinstance(event, HandshakeCompleted) for event in events), events
                    except BaseException as error:
                        failures.append(error)

                thread = threading.Thread(target=peer)
                thread.start()
                result = subprocess.run([str(binary), mode, str(listener.getsockname()[1])], cwd=ROOT,
                                        text=True, capture_output=True, timeout=20)
                thread.join(timeout=6)
                assert not thread.is_alive() and not failures, failures
                assert result.returncode == 0 and not result.stderr, result
                if mode in SUCCESS_MODES:
                    assert result.stdout == "AUTHENTICATED\n", result.stdout
                else:
                    expected = {**PARAMETER_ERRORS, "untrusted": "X.509 certificate chain is not trusted", "hostname": "X.509 certificate does not match hostname", "finished": "TLS Finished verification failed"}[mode]
                    assert result.stdout == "ERROR " + expected + "\n", result.stdout
    print("Live QUIC TLS: certificates, Finished, connection-ID binding, invalid transport parameters, unknown extensions, handshake completion, and cleanup verified")


if __name__ == "__main__":
    main()
