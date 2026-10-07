#!/usr/bin/env python3
"""QUIC packet framing/protection interoperability with test-only aioquic==1.3.0."""

import subprocess
import tempfile
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.crypto import CryptoContext, derive_key_iv_hp
from aioquic.quic.packet import (QuicPacketType, encode_long_header_first_byte,
                                 encode_quic_retry, encode_quic_version_negotiation,
                                 pull_quic_header)
from aioquic.tls import CipherSuite
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM

ROOT = Path(__file__).resolve().parents[1]
SECRET = bytes(range(32))
DESTINATION, SOURCE = bytes(range(8)), bytes(range(8, 11))
NUMBER = 1073741825
KINDS = [QuicPacketType.INITIAL, QuicPacketType.ZERO_RTT, QuicPacketType.HANDSHAKE, QuicPacketType.ONE_RTT]

DARK = """// quic_packets.dark - Packet framing and authenticated peer payloads with leak checks.
let inspect (keys: Stdlib.QuicCrypto.Keys) (data: Blob) : Unit =
  if Stdlib.Blob.length data == 0 then ()
  else
    match Stdlib.QuicPacket.parse 8L data with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok packet ->
      match packet with
      | Protected value ->
        let kind = match value.kind with | Initial -> "0" | ZeroRtt -> "1" | Handshake -> "2" | OneRtt -> "3" in
        let _ = Stdlib.printLine ("P " ++ kind ++ " " ++ Stdlib.Blob.toHex value.destination ++ " " ++ Stdlib.Blob.toHex value.source ++ " " ++ Stdlib.Blob.toHex value.token ++ " " ++ Stdlib.Int64.toString value.numberOffset ++ " " ++ Stdlib.Int.toString (Stdlib.Blob.length value.bytes)) in
        let _ =
          match Stdlib.QuicCrypto.openPacket keys value.bytes value.numberOffset 1073741824L with
          | Error message -> Stdlib.printLine ("OPEN ERROR " ++ message)
          | Ok opened -> Stdlib.printLine ("OPEN " ++ Stdlib.Int64.toString opened.number ++ " " ++ Stdlib.Blob.toHex opened.payload) in
        inspect keys value.remaining
      | Retry value -> Stdlib.printLine ("R " ++ Stdlib.Blob.toHex value.destination ++ " " ++ Stdlib.Blob.toHex value.source ++ " " ++ Stdlib.Blob.toHex value.token)
      | VersionNegotiation value -> Stdlib.printLine ("V " ++ Stdlib.Blob.toHex value.destination ++ " " ++ Stdlib.Blob.toHex value.source ++ " " ++ Stdlib.String.join (Stdlib.List.map value.versions Stdlib.Int64.toString) ",")
      | UnsupportedVersion value -> Stdlib.printLine ("U " ++ Stdlib.Int64.toString value.version)
let checkPackets () : Unit =
  match Stdlib.Blob.fromHex "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F" |> Stdlib.Result.andThen Stdlib.QuicCrypto.derive,
        Stdlib.Blob.fromHex "0001020304050607", Stdlib.Blob.fromHex "08090A",
        Stdlib.Cli.Args.get 0, Stdlib.Cli.Args.get 1, Stdlib.Cli.Args.get 2 with
  | Ok keys, Ok destination, Ok source, Ok mode, Ok code, Ok hex ->
    match Stdlib.Blob.fromHex hex with
    | Error message -> Stdlib.printLine message
    | Ok data ->
      if mode == "inspect" then inspect keys data
      else
        let kind = if code == "0" then Stdlib.QuicPacket.Kind.Initial else if code == "1" then Stdlib.QuicPacket.Kind.ZeroRtt else Stdlib.QuicPacket.Kind.Handshake in
        let result =
          if mode == "token" then Stdlib.QuicPacket.sealLong Stdlib.QuicPacket.Kind.Initial keys destination source data Stdlib.Blob.empty 1073741825L
          else if code == "3" then Stdlib.QuicPacket.sealShort keys destination false data 1073741825L
          else Stdlib.QuicPacket.sealLong kind keys destination source (if code == "0" then Stdlib.String.toBlob "token" else Stdlib.Blob.empty) data 1073741825L in
        match result with
        | Error message -> Stdlib.printLine ("ERROR " ++ message)
        | Ok packet -> Stdlib.printLine (Stdlib.Blob.toHex packet)
  | _ -> Stdlib.printLine "Bad arguments"
checkPackets ()
"""


def main():
    crypto = CryptoContext()
    crypto.setup(cipher_suite=CipherSuite.AES_128_GCM_SHA256, secret=SECRET, version=1)
    with tempfile.TemporaryDirectory(prefix="dark-quic-packet-") as temporary:
        source, binary = Path(temporary) / "packets.dark", Path(temporary) / "packets"
        source.write_text(DARK)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr

        def run(mode, code, data):
            result = subprocess.run([str(binary), mode, str(code), data.hex()], cwd=ROOT,
                                    text=True, capture_output=True, timeout=20)
            assert result.returncode == 0 and not result.stderr, result
            return result.stdout.strip()

        packets, expected = [], []
        for index, kind in enumerate(KINDS):
            for payload in (b"", b"\x01", bytes(range(256)), b"\x06" * 1000):
                packet = bytes.fromhex(run("seal", index, payload))
                buffer = Buffer(data=packet)
                header = pull_quic_header(buffer, host_cid_length=8)
                assert header.packet_type == kind and header.destination_cid == DESTINATION
                assert header.packet_length == len(packet)
                offset = buffer.tell()
                plain_header, decoded, number, _ = crypto.decrypt_packet(packet, offset, NUMBER)
                assert number == NUMBER
                if kind == QuicPacketType.INITIAL:
                    assert len(packet) >= 1200 and header.token == b"token"
                    assert decoded.startswith(payload) and not any(decoded[len(payload):])
                else:
                    assert decoded == payload
                # Re-protect the same independent header with aioquic, then let
                # Dark decode and authenticate it, including multiple PN lengths.
                for pn_length in (1, 2, 3, 4):
                    head = bytes([(plain_header[0] & ~3) | (pn_length - 1)]) + plain_header[1:offset]
                    body = b"\x01" * max(4 - pn_length, 1)
                    if index != 3:
                        # Rebuild the unprotected length field using a fixed
                        # two-byte varint; it includes PN and AEAD tag.
                        token_prefix = head[:offset - 2]
                        length = len(body) + pn_length + 16
                        head = token_prefix + (0x4000 | length).to_bytes(2, "big")
                    head += (NUMBER % (1 << (8 * pn_length))).to_bytes(pn_length, "big")
                    peer_packet = crypto.encrypt_packet(head, body, NUMBER)
                    if index != 3:
                        packets.append(peer_packet)
                    peer_buf = Buffer(data=peer_packet)
                    peer_header = pull_quic_header(peer_buf, host_cid_length=8)
                    prefix = f"P {index} {DESTINATION.hex().upper()} {SOURCE.hex().upper() if index != 3 else ''} {('token'.encode().hex().upper() if index == 0 else '')} {peer_buf.tell()} {len(peer_packet)}"
                    expected_pair = [prefix, f"OPEN {NUMBER} {body.hex().upper()}"]
                    assert run("inspect", 0, peer_packet).splitlines() == expected_pair
                    if index != 3:
                        expected.extend(expected_pair)
        # Initial/0-RTT/Handshake may be coalesced; the final 1-RTT owns the rest.
        assert run("inspect", 0, b"".join(packets[:12])).splitlines() == expected[:24]
        for size in (0, 63, 64, 1100, 1136, 1150, 1170, 1200, 2048):
            token = b"t" * size
            packet = bytes.fromhex(run("token", 0, token))
            buffer = Buffer(data=packet)
            header = pull_quic_header(buffer, host_cid_length=8)
            assert header.token == token and 1200 <= len(packet) <= 4096
            # Use the independent primitives for oversized headers: aioquic's
            # native HP helper crashed on the >2KiB header in this boundary case.
            if size > 1200:
                key, iv, hp = derive_key_iv_hp(cipher_suite=CipherSuite.AES_128_GCM_SHA256, secret=SECRET, version=1)
                offset = buffer.tell()
                encryptor = Cipher(algorithms.AES(hp), modes.ECB()).encryptor()
                mask = encryptor.update(packet[offset + 4:offset + 20]) + encryptor.finalize()
                plain = bytearray(packet[:offset + 4])
                plain[0] ^= mask[0] & 15
                for index in range(4):
                    plain[offset + index] ^= mask[index + 1]
                number = int.from_bytes(plain[offset:], "big")
                nonce = bytes(a ^ b for a, b in zip(iv, number.to_bytes(12, "big")))
                body = AESGCM(key).decrypt(nonce, packet[offset + 4:], bytes(plain))
            else:
                _, body, number, _ = crypto.decrypt_packet(packet, buffer.tell(), NUMBER)
            assert number == NUMBER and not any(body)
        for size in (2049, 4096):
            assert run("token", 0, b"t" * size) == "ERROR QUIC packet exceeds datagram limit"
        for packet in packets[:4]:
            for end in range(min(32, len(packet))):
                assert run("inspect", 0, packet[:end]).startswith("ERROR ") or end == 0
        retry = encode_quic_retry(1, SOURCE, DESTINATION, DESTINATION, b"retry-token")
        assert run("inspect", 0, retry) == f"R {DESTINATION.hex().upper()} {SOURCE.hex().upper()} {b'retry-token'.hex().upper()}"
        versions = encode_quic_version_negotiation(SOURCE, DESTINATION, [1, 0x6B3343CF])
        assert run("inspect", 0, versions) == f"V {DESTINATION.hex().upper()} {SOURCE.hex().upper()} 1,1798521807"
    print("QUIC packet interoperability: long/short protection, all PN lengths, coalescing, Initial padding, Retry and Version Negotiation verified")


if __name__ == "__main__":
    main()
