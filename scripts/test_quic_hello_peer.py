#!/usr/bin/env python3
"""Verify a compiled Dark QUIC Initial against a real aioquic server flight.

This validates packet/TLS integration, not a completed or authenticated handshake.
The native guest boundary is UDP, entropy and time only.
"""

import datetime
import socket
import subprocess
import tempfile
import threading
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.configuration import QuicConfiguration
from aioquic.quic.connection import QuicConnection
from aioquic.quic.crypto import CryptoPair
from aioquic.quic.packet import QuicTransportParameters, pull_quic_header, push_quic_transport_parameters
from aioquic.tls import Epoch, ExtensionType, pull_client_hello
from cryptography import x509
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.x509.oid import NameOID

ROOT = Path(__file__).resolve().parents[1]
DESTINATION, SOURCE = bytes(range(8)), bytes(range(8, 16))


def dark_source(port, parameters):
    return f"""// initial.dark - Fresh X25519 ClientHello and live UDP server Initial, with owned cleanup.
match Stdlib.Blob.fromHex "{parameters.hex()}", Stdlib.Blob.fromHex "{DESTINATION.hex()}", Stdlib.Blob.fromHex "{SOURCE.hex()}",
      Stdlib.X25519.generateKeyPair (), Stdlib.Crypto.secureRandomBytes 32L with
| Ok parameters, Ok destination, Ok source, Ok pair, Ok random ->
  match Stdlib.Tls13.quicClientHello "localhost" random pair.publicKey parameters, Stdlib.QuicCrypto.initial destination with
  | Ok hello, Ok keys ->
    let _ = Stdlib.printLine ("HELLO " ++ Stdlib.Blob.toHex hello) in
    match Stdlib.QuicWire.integer (match Stdlib.Int.toInt64 (Stdlib.Blob.length hello) with | Some size -> size | None -> 0L), Stdlib.Blob.fromHex "0600" with
    | Error message, _ -> Stdlib.printLine message
    | _, Error message -> Stdlib.printLine message
    | Ok length, Ok prefix ->
      let frame = Stdlib.Blob.concat [prefix, length, hello] in
      match Stdlib.QuicPacket.sealLong Stdlib.QuicPacket.Kind.Initial keys.client destination source Stdlib.Blob.empty frame 0L,
            Stdlib.Datagram.bind4 [127L, 0L, 0L, 1L] 0L with
      | Ok packet, Ok connection ->
        let result =
          match Stdlib.Datagram.send connection (Stdlib.Datagram.Endpoint {{ address = [127L, 0L, 0L, 1L], port = {port}L }}) packet 1000L with
          | Error _ -> Error "UDP send failed"
          | Ok () ->
            match Stdlib.Datagram.receive connection 1000L with
            | Error _ -> Error "UDP receive failed"
            | Ok datagram ->
              if datagram.peer.address != [127L, 0L, 0L, 1L] || datagram.peer.port != {port}L then Error "Wrong QUIC peer"
              else
                match Stdlib.QuicPacket.parse 8L datagram.payload with
                | Error message -> Error message
                | Ok (Protected received) ->
                  if received.kind != Stdlib.QuicPacket.Kind.Initial || Stdlib.Blob.toHex received.destination != Stdlib.Blob.toHex source then Error "Wrong Initial connection"
                  else Stdlib.QuicCrypto.openPacket keys.server received.bytes received.numberOffset -1L |> Stdlib.Result.map (fun opened -> opened.payload)
                | Ok _ -> Error "Expected server Initial" in
        let _ = Stdlib.Datagram.close connection in
        match result with
        | Error message -> Stdlib.printLine ("ERROR " ++ message)
        | Ok payload -> Stdlib.printLine ("SERVER " ++ Stdlib.Blob.toHex payload)
      | Error message, Ok connection -> let _ = Stdlib.Datagram.close connection in Stdlib.printLine message
      | _, _ -> Stdlib.printLine "Initial setup failed"
  | _, _ -> Stdlib.printLine "TLS hello failed"
| _ -> Stdlib.printLine "Entropy failed"
"""


def main():
    parameters = Buffer(capacity=1024)
    push_quic_transport_parameters(parameters, QuicTransportParameters(
        initial_source_connection_id=SOURCE, max_udp_payload_size=4096,
        initial_max_data=104857600, initial_max_stream_data_bidi_local=104857600,
        initial_max_stream_data_bidi_remote=104857600, initial_max_stream_data_uni=65536,
        initial_max_streams_bidi=1, initial_max_streams_uni=3, disable_active_migration=True))
    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    name = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "localhost")])
    now = datetime.datetime.now(datetime.timezone.utc)
    certificate = (x509.CertificateBuilder().subject_name(name).issuer_name(name)
                   .public_key(key.public_key()).serial_number(x509.random_serial_number())
                   .not_valid_before(now - datetime.timedelta(minutes=1))
                   .not_valid_after(now + datetime.timedelta(days=1))
                   .add_extension(x509.SubjectAlternativeName([x509.DNSName("localhost")]), critical=False)
                   .sign(key, hashes.SHA256()))
    configuration = QuicConfiguration(is_client=False, alpn_protocols=["h3"])
    configuration.certificate, configuration.private_key = certificate, key
    failures, observed = [], []
    with tempfile.TemporaryDirectory(prefix="dark-quic-hello-") as temporary, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as listener:
        listener.bind(("127.0.0.1", 0))
        listener.settimeout(10)
        source, binary = Path(temporary) / "initial.dark", Path(temporary) / "initial"
        source.write_text(dark_source(listener.getsockname()[1], parameters.data))
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr

        def peer():
            try:
                packet, address = listener.recvfrom(8192)
                assert len(packet) >= 1200
                buffer = Buffer(data=packet)
                header = pull_quic_header(buffer, host_cid_length=8)
                assert header.destination_cid == DESTINATION and header.source_cid == SOURCE
                crypto = CryptoPair()
                crypto.setup_initial(cid=DESTINATION, is_client=False, version=1)
                _, decoded, number = crypto.decrypt_packet(packet, buffer.tell(), 0)
                assert number == 0
                frames = Buffer(data=decoded)
                assert frames.pull_uint_var() == 6 and frames.pull_uint_var() == 0
                hello = frames.pull_bytes(frames.pull_uint_var())
                parsed = pull_client_hello(Buffer(data=hello))
                assert parsed.server_name == "localhost" and parsed.alpn_protocols == ["h3"]
                assert parsed.cipher_suites == [0x1301]
                assert parsed.supported_versions == [0x0304]
                assert parsed.supported_groups == [29]
                assert (ExtensionType.QUIC_TRANSPORT_PARAMETERS, parameters.data) in parsed.other_extensions
                observed.append(hello)
                connection = QuicConnection(configuration=configuration, original_destination_connection_id=DESTINATION)
                connection.receive_datagram(packet, address, now=0)
                replies = connection.datagrams_to_send(now=0)
                assert replies, "QUIC server rejected the Initial"
                server_packet = replies[0][0]
                server_buf = Buffer(data=server_packet)
                server_header = pull_quic_header(server_buf, host_cid_length=8)
                # The inverse initial context independently authenticates the server packet.
                inverse = CryptoPair()
                inverse.setup_initial(cid=DESTINATION, is_client=True, version=1)
                _, server_payload, server_number = inverse.decrypt_packet(
                    server_packet[:server_header.packet_length], server_buf.tell(), 0)
                assert server_number == 0
                observed.append(server_payload)
                for reply, target in replies:
                    listener.sendto(reply, target)
                assert connection._cryptos[Epoch.HANDSHAKE].send.is_valid(), "No handshake traffic keys"
            except BaseException as error:
                failures.append(error)

        thread = threading.Thread(target=peer)
        thread.start()
        result = subprocess.run([str(binary)], cwd=ROOT, text=True, capture_output=True, timeout=15)
        thread.join(timeout=12)
        assert not thread.is_alive() and not failures, failures
        assert result.returncode == 0 and not result.stderr, result
        assert result.stdout.splitlines() == ["HELLO " + observed[0].hex().upper(), "SERVER " + observed[1].hex().upper()], result.stdout
    print("Live QUIC Initial: fresh X25519, h3 ALPN, AES-128, transport parameters, aioquic server flight, and leak accounting verified")


if __name__ == "__main__":
    main()
