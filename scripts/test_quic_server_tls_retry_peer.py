#!/usr/bin/env python3
"""Check encrypted server QUIC HRR offsets, certificate flight and Finished with aioquic crypto."""

import argparse
import hashlib
import hmac
import select
import subprocess
import tempfile
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.crypto import CryptoPair
from aioquic.quic.packet import QuicPacketType, pull_quic_header
from aioquic.quic.packet_builder import QuicPacketBuilder
from aioquic.tls import CipherSuite
from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import padding, x25519
from cryptography.hazmat.primitives.kdf.hkdf import HKDFExpand

from test_quic_server_retry import CLIENT, RETRY, SECRET, token
from test_quic_tls_peer import certificates
from test_tls_server_hello import extension, hello, u16

ROOT = Path(__file__).resolve().parents[1]
PROBE = '''// server.dark - Exercise real server packet spaces through one TLS retry and Finished.
let bytes (text: String) : Blob = Stdlib.Blob.fromHex text |> Stdlib.Result.withDefault Stdlib.Blob.empty
let input () : Blob = bytes (Builtin.stdinReadLine (()))
let flush (socket: Stdlib.__Datagram.Socket) (peer: Stdlib.__Datagram.Endpoint) (state: Stdlib.__QuicServer.Flight) (now: Int64) (count: Int64) : Stdlib.Result.Result<Stdlib.__QuicServer.Flight, String> =
  if count == 0L then Ok state else Stdlib.__QuicServer.__flush socket peer state now |> Stdlib.Result.andThen (fun next -> flush socket peer next (now + 10L) (count - 1L))
let exchange (socket: Stdlib.__Datagram.Socket) : Stdlib.Result.Result<Unit, String> =
  let peer = Stdlib.__Datagram.Endpoint { address = [127L,0L,0L,1L], port = 443L } in
  Stdlib.TlsServerIdentity.create (bytes "@CERT@") (bytes "@KEY@") |> Stdlib.Result.andThen (fun identity ->
    Stdlib.__QuicServer.start identity (bytes "@SECRET@") (Stdlib.__Datagram.Message { peer = peer, payload = input () }) 1000L |> Stdlib.Result.andThen (fun state ->
      if Stdlib.Option.isNone state.retry || Stdlib.Option.isSome state.tls then Error "Expected initial TLS retry"
      else flush socket peer state 1000L 1L |> Stdlib.Result.andThen (fun state ->
        Stdlib.printLine "SECOND"
        Stdlib.__QuicServer.packets state (input ()) 1010L |> Stdlib.Result.andThen (fun state ->
          flush socket peer state 1010L 10L |> Stdlib.Result.andThen (fun state ->
            Stdlib.printLine "FINISHED"
            if Stdlib.Option.isSome state.authenticated then Error "Premature application keys"
            else Stdlib.__QuicServer.packets state (input ()) 1200L |> Stdlib.Result.andThen (fun state ->
              match state.authenticated with
              | None -> Error "Missing authenticated keys"
              | Some ready ->
                if Stdlib.Bool.not state.initialDiscarded then Error "Initial keys retained after client Handshake"
                else
                  Stdlib.printLine (Stdlib.Blob.toHex ready.sendSecret)
                  Stdlib.printLine (Stdlib.Blob.toHex ready.receiveSecret)
                  Stdlib.printLine "DONE"
                  Ok ()))))))
let emit (_peer: Stdlib.__Datagram.Endpoint) (packet: Blob) (_timeout: Int64) : Stdlib.Result.Result<Unit, Int64> =
  Stdlib.printLine ("PACKET " ++ Stdlib.Blob.toHex packet)
  Ok ()
let lifecycle = Stdlib.Stream.__new (fun _unit -> Some ()) (fun _unit -> ())
let socket = Stdlib.__Datagram.Socket { lifecycle = lifecycle, watch = None, receive = fun _timeout -> Error 11L, send = emit }
let result = exchange socket
Stdlib.__Datagram.close socket
match result with | Ok () -> () | Error message -> Stdlib.printLine ("ERROR " ++ message)
'''


def expand(secret, label, context=b"", length=32):
    label = b"tls13 " + label
    info = length.to_bytes(2, "big") + bytes([len(label)]) + label + bytes([len(context)]) + context
    return HKDFExpand(algorithm=hashes.SHA256(), length=length, info=info).derive(secret)


def extract(salt, material):
    return hmac.new(salt, material, hashlib.sha256).digest()


def packet(crypto, kind, data, offset, number, ack=None):
    builder = QuicPacketBuilder(host_cid=CLIENT, peer_cid=RETRY, version=1, is_client=True,
                                max_datagram_size=1200, packet_number=number, peer_token=token(issued=1000))
    builder.start_packet(kind, crypto)
    if ack is not None:
        buffer = builder.start_frame(2, capacity=32)
        for value in (ack, 0, 0, ack):
            buffer.push_uint_var(value)
    buffer = builder.start_frame(6, capacity=len(data) + 16)
    buffer.push_uint_var(offset)
    buffer.push_uint_var(len(data))
    buffer.push_bytes(data)
    datagrams, _ = builder.flush()
    assert len(datagrams) == 1
    return datagrams[0]


def crypto_frames(payload):
    buffer, result = Buffer(data=payload), []
    while not buffer.eof():
        kind = buffer.pull_uint_var()
        if kind in (0, 1):
            continue
        if kind in (2, 3):
            buffer.pull_uint_var()
            buffer.pull_uint_var()
            count = buffer.pull_uint_var()
            buffer.pull_uint_var()
            for _ in range(count):
                buffer.pull_uint_var()
                buffer.pull_uint_var()
            if kind == 3:
                for _ in range(3):
                    buffer.pull_uint_var()
        else:
            assert kind == 6, kind
            offset, size = buffer.pull_uint_var(), buffer.pull_uint_var()
            result.append((offset, buffer.pull_bytes(size)))
    return result


def decrypt(crypto, data, expected):
    buffer = Buffer(data=data)
    header = pull_quic_header(buffer, host_cid_length=8)
    assert header.destination_cid == CLIENT and header.source_cid == RETRY
    _, payload, number = crypto.decrypt_packet(data, buffer.tell(), expected)
    return header, crypto_frames(payload), number


def message(kind, body):
    return bytes([kind]) + len(body).to_bytes(3, "big") + body


def messages(data):
    result = []
    while data:
        size = 4 + int.from_bytes(data[1:4], "big")
        assert len(data) >= size
        result.append(data[:size])
        data = data[size:]
    return result


def read_until(process, marker):
    packets = []
    while True:
        assert select.select([process.stdout], [], [], 10)[0], "server output timed out"
        line = process.stdout.readline().strip()
        if line == marker:
            return packets
        assert line.startswith(b"PACKET "), line
        packets.append(bytes.fromhex(line[7:].decode()))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    private = x25519.X25519PrivateKey.generate()
    public = private.public_key().public_bytes(serialization.Encoding.Raw, serialization.PublicFormat.Raw)
    # The original QUIC client source CID is identical in both TLS offers.
    parameters = b"\x0f\x08" + CLIENT
    fixed = extension(43, b"\x02\x03\x04") + extension(10, b"\x00\x02\x00\x1d") + extension(13, b"\x00\x04\x08\x04\x04\x01") + extension(16, b"\x00\x03\x02h3") + extension(57, parameters)
    first = hello(fixed + extension(51, b"\x00\x00"))
    second = hello(fixed + extension(51, b"\x00\x24\x00\x1d\x00\x20" + public))
    initial = CryptoPair()
    initial.setup_initial(RETRY, is_client=True, version=1)
    with tempfile.TemporaryDirectory(prefix="dark-quic-tls-retry-") as temporary:
        source, binary = Path(temporary) / "server.dark", Path(temporary) / "server"
        source.write_text(PROBE.replace("@CERT@", leaf.public_bytes(serialization.Encoding.PEM).hex()).replace("@KEY@", key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()).hex()).replace("@SECRET@", SECRET.hex()))
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process = subprocess.Popen([str(binary)], cwd=ROOT, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
        try:
            process.stdin.write(packet(initial, QuicPacketType.INITIAL, first, 0, 0).hex().encode() + b"\n")
            process.stdin.flush()
            packets = read_until(process, b"SECOND")
            assert len(packets) == 1
            header, frames, number = decrypt(initial, packets[0], 0)
            assert header.packet_type == QuicPacketType.INITIAL and number == 0 and frames[0][0] == 0
            hrr = frames[0][1]
            assert hrr[6:38] == bytes.fromhex("CF21AD74E59A6111BE1D8C021E65B891C2A211167ABB8C5E079E09E2C8A8339C")
            process.stdin.write(packet(initial, QuicPacketType.INITIAL, second, len(first), 1, ack=0).hex().encode() + b"\n")
            process.stdin.flush()
            packets = read_until(process, b"FINISHED")
            header, frames, number = decrypt(initial, packets[0], 1)
            assert header.packet_type == QuicPacketType.INITIAL and number == 1 and frames[0][0] == len(hrr)
            server_hello = frames[0][1]
            session_size = server_hello[38]
            extensions = Buffer(data=server_hello[44 + session_size:])
            server_share = None
            while not extensions.eof():
                kind, length = extensions.pull_uint16(), extensions.pull_uint16()
                data = extensions.pull_bytes(length)
                if kind == 51:
                    assert data[:4] == b"\x00\x1d\x00\x20"
                    server_share = data[4:]
            assert server_share is not None
            shared = private.exchange(x25519.X25519PublicKey.from_public_bytes(server_share))
            early = extract(bytes(32), bytes(32))
            handshake_secret = extract(expand(early, b"derived", hashlib.sha256(b"").digest()), shared)
            transcript = message(254, hashlib.sha256(first).digest()) + hrr + second + server_hello
            client_secret = expand(handshake_secret, b"c hs traffic", hashlib.sha256(transcript).digest())
            server_secret = expand(handshake_secret, b"s hs traffic", hashlib.sha256(transcript).digest())
            handshake = CryptoPair()
            handshake.send.setup(cipher_suite=CipherSuite.AES_128_GCM_SHA256, secret=client_secret, version=1)
            handshake.recv.setup(cipher_suite=CipherSuite.AES_128_GCM_SHA256, secret=server_secret, version=1)
            crypto, largest, initial_largest = {}, -1, 1
            for data in packets[1:]:
                kind = pull_quic_header(Buffer(data=data), host_cid_length=8).packet_type
                if kind == QuicPacketType.INITIAL:
                    _, repeats, number = decrypt(initial, data, initial_largest + 1)
                    assert number > initial_largest
                    initial_largest = number
                    assert repeats == [(len(hrr), server_hello)]
                    continue
                header, frames, number = decrypt(handshake, data, largest + 1)
                assert header.packet_type == QuicPacketType.HANDSHAKE
                largest = max(largest, number)
                for offset, data in frames:
                    if offset in crypto:
                        assert crypto[offset] == data
                    crypto[offset] = data
            flight, offset = b"", 0
            for start, data in sorted(crypto.items()):
                assert start == offset
                flight += data
                offset += len(data)
            frames = messages(flight)
            assert [frame[0] for frame in frames] == [8, 11, 15, 20]
            certificate = frames[1]
            assert certificate[4] == 0
            size = int.from_bytes(certificate[8:11], "big")
            der = certificate[11:11 + size]
            assert der == leaf.public_bytes(serialization.Encoding.DER)
            loaded = x509.load_der_x509_certificate(der)
            ca.public_key().verify(loaded.signature, loaded.tbs_certificate_bytes, padding.PKCS1v15(), hashes.SHA256())
            transcript += frames[0] + frames[1]
            verify = frames[2]
            assert verify[4:6] == b"\x08\x04"
            loaded.public_key().verify(verify[8:], bytes([32]) * 64 + b"TLS 1.3, server CertificateVerify\x00" + hashlib.sha256(transcript).digest(), padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=32), hashes.SHA256())
            transcript += verify
            expected = hmac.new(expand(server_secret, b"finished"), hashlib.sha256(transcript).digest(), hashlib.sha256).digest()
            assert frames[3] == message(20, expected)
            transcript += frames[3]
            finished = message(20, hmac.new(expand(client_secret, b"finished"), hashlib.sha256(transcript).digest(), hashlib.sha256).digest())
            process.stdin.write(packet(handshake, QuicPacketType.HANDSHAKE, finished, 0, 0, ack=largest).hex().encode() + b"\n")
            process.stdin.flush()
            stdout, stderr = process.communicate(timeout=10)
            assert process.returncode == 0 and not stderr, (process.returncode, stdout, stderr)
            lines = stdout.splitlines()
            master = extract(expand(handshake_secret, b"derived", hashlib.sha256(b"").digest()), bytes(32))
            assert lines == [expand(master, b"s ap traffic", hashlib.sha256(transcript).digest()).hex().upper().encode(), expand(master, b"c ap traffic", hashlib.sha256(transcript).digest()).hex().upper().encode(), b"DONE"], lines
            print("QUIC TLS retry: encrypted Initial offsets/packet numbers, independent ECDH/HKDF, trusted certificate/PSS/Finished, client Finished gating and zero leaks", flush=True)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()


if __name__ == "__main__":
    main()
