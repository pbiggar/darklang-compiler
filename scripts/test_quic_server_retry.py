#!/usr/bin/env python3
"""Check Dark address-bound Retry tokens with Python HMAC and aioquic integrity tags."""

import argparse
import hashlib
import hmac
import ipaddress
import subprocess
import tempfile
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.packet import get_retry_integrity_tag, pull_quic_header

ROOT = Path(__file__).resolve().parents[1]
SECRET = bytes(range(32))
ORIGINAL, CLIENT, RETRY = b"original", b"clientID", b"retryCID"
NOW = 123456789


def token(address="127.0.0.1", port=443, original=ORIGINAL, client=CLIENT, retry=RETRY, issued=NOW, version=1):
    address = ipaddress.ip_address(address).packed
    material = (bytes([version]) + issued.to_bytes(8, "big") + bytes([len(original)]) + original
                + bytes([len(client)]) + client + bytes([len(retry)]) + retry
                + bytes([len(address)]) + address + port.to_bytes(2, "big"))
    return material + hmac.new(SECRET, material, hashlib.sha256).digest()[:16]


def blob(data):
    return f'(bytes "{data.hex()}")'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    normal = token()
    vectors = []

    def add(data=normal, now=NOW, address="127.0.0.1", port=443, destination=RETRY, source=CLIENT,
            initial=True, valid=False, original=ORIGINAL):
        vectors.append((data, now, address, port, destination, source, initial, valid, original))

    add(valid=True)
    add(now=NOW + 30000, valid=True)
    add(now=NOW + 30001)
    add(now=NOW - 1)
    add(address="127.0.0.2")
    add(port=444)
    add(port=0)
    add(destination=b"wrongCID")
    add(source=b"wrongCID")
    add(initial=False)
    add(data=normal[:-1] + bytes([normal[-1] ^ 1]))
    changed = bytearray(normal)
    changed[15] ^= 1
    add(data=bytes(changed))
    for length in (0, 1, 16, 25, 41, len(normal) - 1):
        add(data=normal[:length])
    add(data=normal + b"x")
    add(data=token(version=2))
    add(data=token(issued=1 << 63))
    add(data=token(original=b"short"))
    add(data=token(original=bytes(21)))
    add(data=token(client=bytes(21)))
    add(data=token(retry=b""), destination=b"")
    add(data=token(retry=bytes(21)), destination=bytes(21))
    add(data=token(address="::1"), address="::1", valid=True)
    add(data=token(address="::1"), address="::2")
    add(data=token(address="::1"), address="127.0.0.1")
    longest = bytes(range(20))
    add(data=token(address="::1", original=longest, client=longest, retry=longest), address="::1",
        destination=longest, source=longest, original=longest, valid=True)
    add(data=token(client=b"", retry=b"x"), source=b"", destination=b"x", valid=True)
    checks = []
    for data, now, address, port, destination, source, initial, valid, original in vectors:
        packed = ipaddress.ip_address(address).packed
        endpoint = "[" + ",".join(f"{byte}L" for byte in packed) + "]"
        kind = "Initial" if initial else "Handshake"
        expected = f'Some "{original.hex().upper()}"' if valid else "None"
        checks.append(f'check "{data.hex()}" {now}L {endpoint} {port}L {blob(destination)} {blob(source)} Stdlib.QuicPacket.Kind.{kind} == {expected}')
    source = '''// retry.dark - Expiring Retry token oracle and malformed authenticated-field rejection.
let bytes (text: String) : Blob = Stdlib.Blob.fromHex text |> Stdlib.Result.withDefault Stdlib.Blob.empty
let check (text: String) (now: Int64) (address: List<Int64>) (port: Int64) (destination: Blob) (source: Blob) (kind: Stdlib.QuicPacket.Kind) : Stdlib.Option.Option<String> =
  let peer = Stdlib.Datagram.Endpoint { address = address, port = port } in
  let packet = Stdlib.QuicPacket.Protected { kind = kind, destination = destination, source = source, token = bytes text, bytes = Stdlib.Blob.empty, numberOffset = 0L, remaining = Stdlib.Blob.empty } in
  match Stdlib.QuicServerRetry.validate (bytes "@SECRET@") peer packet now with
  | Error _ -> None
  | Ok original -> Some (Stdlib.Blob.toHex original)
let run () : Unit =
  if (Stdlib.Cli.Args.get 0 |> Stdlib.Result.withDefault "") == "create" then
    match Stdlib.QuicServerRetry.create (bytes "@SECRET@") (Stdlib.Datagram.Endpoint { address = [127L,0L,0L,1L], port = 443L }) (bytes "@ORIGINAL@") (bytes "@CLIENT@") (bytes "@RETRY@") 123456789L with
    | Error message -> Stdlib.printLine message
    | Ok wire -> Stdlib.printLine (Stdlib.Blob.toHex wire)
  else Stdlib.printLine (if @CHECKS@ then "DONE" else "FAILED")
run ()
'''
    source = (source.replace("@SECRET@", SECRET.hex()).replace("@ORIGINAL@", ORIGINAL.hex())
              .replace("@CLIENT@", CLIENT.hex()).replace("@RETRY@", RETRY.hex()).replace("@CHECKS@", " &&\n    ".join(f"({check})" for check in checks)))
    with tempfile.TemporaryDirectory(prefix="dark-server-retry-") as temporary:
        path, binary = Path(temporary) / "retry.dark", Path(temporary) / "retry"
        path.write_text(source)
        compiled = subprocess.run([str(args.compiler), str(path), "--leak-check", "-o", str(binary)], cwd=ROOT,
                                  text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        result = subprocess.run([str(binary), "validate"], cwd=ROOT, text=True, capture_output=True, timeout=30)
        assert result.returncode == 0 and not result.stderr and result.stdout.strip() == "DONE", result
        created = subprocess.run([str(binary), "create"], cwd=ROOT, text=True, capture_output=True, timeout=30)
        assert created.returncode == 0 and not created.stderr, created
        wire = bytes.fromhex(created.stdout.strip())
        header = pull_quic_header(Buffer(data=wire))
        assert header.version == 1 and header.destination_cid == CLIENT and header.source_cid == RETRY
        assert header.token == normal and len(wire) < 1200
        assert wire[-16:] == get_retry_integrity_tag(wire[:-16], ORIGINAL, 1)
    print(f"QUIC server Retry verified: {len(vectors)} HMAC, expiry, IPv4/IPv6, CID, tamper and shape cases; aioquic packet integrity; zero leaks")


if __name__ == "__main__":
    main()
