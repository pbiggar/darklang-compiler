#!/usr/bin/env python3
"""QUIC packet protection against independent HMAC/HKDF and AES implementations.

cryptography is test-only; all guest cryptography remains in Dark.
"""

import hashlib
import hmac
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM

ROOT = Path(__file__).resolve().parents[1]
CID = bytes.fromhex("8394c8f03e515708")

SOURCE = '''// quicCrypto.dark - Pure packet protection with native ownership accounting.
match Stdlib.Cli.Args.get 0, Stdlib.Cli.Args.int64 1, Stdlib.Cli.Args.int64 2,
      Stdlib.Cli.Args.get 3, Stdlib.Cli.Args.get 4 with
| Ok mode, Ok number, Ok offset, Ok a, Ok b ->
  let cid = Stdlib.Blob.fromHex "8394C8F03E515708" |> Stdlib.Result.withDefault Stdlib.Blob.empty in
  match Stdlib.__QuicCrypto.initial cid, Stdlib.Blob.fromHex a, Stdlib.Blob.fromHex b with
  | Ok initial, Ok data, Ok payload ->
    let keys = if Stdlib.String.startsWith mode "client" then initial.client else initial.server in
    if Stdlib.String.endsWith mode "seal" then
      match Stdlib.__QuicCrypto.seal keys data payload number offset with
      | Error message -> Stdlib.printLine ("ERROR: " ++ message)
      | Ok packet -> Stdlib.printLine (Stdlib.Blob.toHex packet)
    else
      match Stdlib.__QuicCrypto.openPacket keys data offset number with
      | Error message -> Stdlib.printLine ("ERROR: " ++ message)
      | Ok opened -> Stdlib.printLine (Stdlib.Int64.toString opened.number ++ "|" ++ Stdlib.Blob.toHex opened.header ++ "|" ++ Stdlib.Blob.toHex opened.payload)
  | _, _, _ -> Stdlib.printLine "Invalid inputs"
| _, _, _, _, _ -> Stdlib.printLine "Invalid arguments"
'''


def expand_label(secret, label, size):
    label = b"tls13 " + label
    info = size.to_bytes(2, "big") + bytes([len(label)]) + label + b"\0"
    return hmac.new(secret, info + b"\1", hashlib.sha256).digest()[:size]


def initial_keys(role):
    secret = hmac.new(bytes.fromhex("38762cf7f55934b34d179ae6a4c80cadccbb7f0a"), CID, hashlib.sha256).digest()
    secret = expand_label(secret, role.encode() + b" in", 32)
    return tuple(expand_label(secret, b"quic " + label, size)
                 for label, size in ((b"key", 16), (b"iv", 12), (b"hp", 16)))


def protect(keys, header, payload, number, offset):
    key, iv, hp = keys
    nonce = (int.from_bytes(iv, "big") ^ number).to_bytes(12, "big")
    packet = bytearray(header + AESGCM(key).encrypt(nonce, payload, header))
    sample = bytes(packet[offset + 4:offset + 20])
    mask = Cipher(algorithms.AES(hp), modes.ECB()).encryptor().update(sample)
    packet[0] ^= mask[0] & (15 if header[0] & 128 else 31)
    for index in range((header[0] & 3) + 1):
        packet[offset + index] ^= mask[index + 1]
    return bytes(packet)


def main():
    with tempfile.TemporaryDirectory(prefix="dark-quic-crypto-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "quicCrypto.dark", directory / "quicCrypto"
        source.write_text(SOURCE)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr

        def guest(mode, number, offset, a, b=b""):
            result = subprocess.run([str(binary), mode, str(number), str(offset), a.hex(), b.hex()],
                                    cwd=ROOT, text=True, capture_output=True, timeout=15)
            assert result.returncode == 0 and not result.stderr, result
            return result.stdout.strip()

        for role in ("client", "server"):
            keys = initial_keys(role)
            for length in range(1, 5):
                for number in (0, 255, 256, 2**32 + 7, 2**62 - 1):
                    header = bytes([0x40 + length - 1]) + b"cidbytes" + (number % 2**(8 * length)).to_bytes(length, "big")
                    payload = bytes(range(32))
                    packet = protect(keys, header, payload, number, 9)
                    assert guest(role + "seal", number, 9, header, payload) == packet.hex().upper()
                    expected = f"{number}|{header.hex().upper()}|{payload.hex().upper()}"
                    assert guest(role + "open", number - 1, 9, packet) == expected
            # Reserved-bit violations must be considered only after successful
            # authentication. A tampered packet does not reach that validation.
            header, payload = b"\x48cidbytes\0", b"\1\0\0"
            packet = protect(keys, header, payload, 0, 9)
            assert guest(role + "open", -1, 9, packet) == "ERROR: Invalid QUIC reserved bits"
            damaged = packet[:-1] + bytes([packet[-1] ^ 1])
            assert guest(role + "open", -1, 9, damaged) == "ERROR: AES-GCM authentication failed"
            assert guest(role + "open", -1, 9, packet[:20]) == "ERROR: Invalid QUIC packet bounds"
    print("QUIC AES-128 protection verified independently: packet-number lengths, wrap, 62-bit limit, tampering and cleanup")


if __name__ == "__main__":
    main()
