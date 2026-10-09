#!/usr/bin/env python3
"""Verify server HRR transcripts and immutable offers, then real OpenSSL HTTP/1 and h2."""

import argparse
import hashlib
import re
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import DataReceived, ResponseReceived, StreamEnded

from test_http2_server_tls_peer import SERVER, start, stop
from test_quic_tls_peer import certificates
from test_tls_server_hello import extension, hello, u16

ROOT = Path(__file__).resolve().parents[1]
PROBE = '''// probe.dark - Retry validation and transcript bytes without signing.
let arg (index: Int) : Blob = Stdlib.Cli.Args.get index |> Stdlib.Result.andThen Stdlib.Blob.fromHex |> Stdlib.Result.withDefault Stdlib.Blob.empty
match Stdlib.__Tls13ServerHello.parse (arg 0) |> Stdlib.Result.andThen Stdlib.__Tls13ServerRetry.create |> Stdlib.Result.andThen (fun retry ->
  Stdlib.__Tls13ServerRetry.validate retry (arg 1) |> Stdlib.Result.map (fun checked ->
    let (_hello, transcript) = checked in
    Stdlib.printLine (Stdlib.Blob.toHex retry.hello)
    Stdlib.printLine (Stdlib.Blob.toHex transcript))) with
| Ok () -> ()
| Error _ -> Stdlib.printLine "ERROR"
'''
RANDOM = bytes.fromhex("CF21AD74E59A6111BE1D8C021E65B891C2A211167ABB8C5E079E09E2C8A8339C")


def psk(items, age=0, fill=0):
    identities = b"".join(u16(len(identity)) + identity + age.to_bytes(4, "big") for identity, _ in items)
    binders = b"".join(bytes([size]) + bytes([fill]) * size for _, size in items)
    return extension(41, u16(len(identities)) + identities + u16(len(binders)) + binders)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    fixed = extension(43, b"\x02\x03\x04") + extension(10, b"\x00\x04\x00\x17\x00\x1d") + extension(13, b"\x00\x02\x08\x04") + extension(16, b"\x00\x03\x02h2")
    empty, share = extension(51, b"\x00\x00"), extension(51, b"\x00\x24\x00\x1d\x00\x20" + bytes(range(32)))
    first, second = hello(fixed + empty), hello(fixed + share)
    cases = [(first, second, True, "basic"), (hello(fixed + empty, session=b"session"), hello(fixed + share, session=b"session"), True, "session echo"),
             (first, hello(fixed + share, session=b"new"), False, "session changed"),
             (first, second[:6] + b"x" + second[7:], False, "random changed"),
             (first, hello(fixed + share, ciphers=b"\x13\x01\x13\x02"), False, "ciphers changed"),
             (first, hello(fixed + empty), False, "second retry"),
             (first, hello(fixed + extension(51, b"\x00\x49\x00\x1d\x00\x20" + bytes(32) + b"\x00\x17\x00\x21" + bytes(33))), False, "multiple second shares"),
             (first, hello(fixed + share + extension(42, b"")), False, "second early data"),
             (first, hello(fixed + share + extension(44, b"\x00\x01x")), False, "unsolicited cookie"),
             (hello(fixed + empty + extension(42, b"")), second, True, "remove early data"),
             (first, hello(fixed + share + extension(21, bytes(31))), True, "add padding"),
             (hello(fixed + empty + extension(21, bytes(7))), second, True, "remove padding"),
             (hello(fixed + empty + extension(21, bytes(7))), hello(fixed + share + extension(21, bytes(11))), True, "resize padding"),
             (first, hello(fixed + share + extension(21, b"x")), False, "nonzero padding"),
             (hello(fixed + empty + extension(65000, b"opaque")), hello(fixed + share + extension(65000, b"opaque")), True, "opaque extension"),
             (hello(fixed + empty + extension(65000, b"opaque")), hello(fixed + share + extension(65000, b"changed")), False, "opaque extension changed"),
             (first, hello(fixed + share + extension(65000, b"new")), False, "extension added")]
    modes = extension(45, b"\x01\x01")
    for before, after, valid, name in (([(b"a", 32)], [(b"a", 32)], True, "PSK age and binder update"),
                                      ([(b"a", 48)], [], True, "remove incompatible PSK"),
                                      ([(b"a", 48), (b"b", 32)], [(b"b", 32)], True, "PSK compatible subset"),
                                      ([(b"a", 32)], [], False, "remove compatible PSK"),
                                      ([(b"a", 32)], [(b"b", 32)], False, "PSK identity changed"),
                                      ([(b"a", 32)], [(b"a", 48)], False, "PSK hash changed"),
                                      ([], [(b"a", 32)], False, "PSK added")):
        a = psk(before) if before else b""
        b = psk(after, age=100, fill=17) if after else b""
        cases.append((hello(fixed + empty + modes + a), hello(fixed + share + modes + b), valid, name))
    cases.append((hello(fixed + empty + extension(41, b"bad")), hello(fixed + share + extension(41, b"bad")), False, "malformed PSK"))
    key, leaf, ca = certificates()
    with tempfile.TemporaryDirectory(prefix="dark-tls-retry-") as temporary:
        directory = Path(temporary)
        probe, probe_binary = directory / "probe.dark", directory / "probe"
        probe.write_text(PROBE)
        result = subprocess.run([str(args.compiler), str(probe), "--allow-internal", "--leak-check", "-o", str(probe_binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        for a, b, valid, name in cases:
            result = subprocess.run([str(probe_binary), a.hex(), b.hex()], cwd=ROOT, capture_output=True, text=True, timeout=5)
            assert result.returncode == 0 and not result.stderr, (name, result.returncode, result.stderr)
            lines = result.stdout.splitlines()
            assert (lines != ["ERROR"]) == valid, (name, lines)
            if valid:
                hrr, transcript = map(bytes.fromhex, lines)
                assert hrr[6:38] == RANDOM
                assert transcript == b"\xfe\x00\x00\x20" + hashlib.sha256(a).digest() + hrr + b, name
        print(f"Retry offer/transcript oracle: {len(cases)} cases, zero leaks", flush=True)
        source, binary, trust = directory / "server.dark", directory / "server", directory / "ca.pem"
        source.write_text(SERVER.replace("@CERT@", leaf.public_bytes(serialization.Encoding.PEM).hex()).replace("@KEY@", key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()).hex()))
        trust.write_bytes(ca.public_bytes(serialization.Encoding.PEM))
        result = subprocess.run([str(args.compiler), str(source), "--leak-check", "-o", str(binary)], cwd=ROOT, capture_output=True, text=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process, port = start(binary)
        try:
            for protocol in ("http/1.1", "h2"):
                trace = directory / "trace.txt"
                connection = H2Connection(config=H2Configuration(client_side=True))
                if protocol == "h2":
                    connection.initiate_connection()
                    connection.send_headers(1, [(":method", "POST"), (":scheme", "https"), (":authority", f"localhost:{port}"), (":path", "/echo"), ("content-length", "5")])
                    connection.send_data(1, b"retry", end_stream=True)
                    request = connection.data_to_send()
                else:
                    request = f"POST /echo HTTP/1.1\r\nHost: localhost:{port}\r\nContent-Length: 5\r\n\r\nretry".encode()
                command = ["openssl", "s_client", "-connect", f"127.0.0.1:{port}", "-servername", "localhost", "-groups", "P-256:X25519", "-tls1_3", "-alpn", protocol, "-CAfile", str(trust), "-verify_return_error", "-quiet", "-verify_quiet", "-msg", "-msgfile", str(trace)]
                result = subprocess.run(command, input=request, cwd=ROOT, capture_output=True, timeout=15)
                assert result.returncode == 0, (protocol, result.stdout, result.stderr)
                trace_bytes = bytes.fromhex("".join(line.strip().replace(" ", "") for line in trace.read_text().splitlines() if re.fullmatch(r"(?:\s*[0-9a-f]{2})+\s*", line)))
                assert RANDOM in trace_bytes, "OpenSSL did not exercise HRR"
                if protocol == "h2":
                    events = connection.receive_data(result.stdout)
                    assert any(isinstance(event, ResponseReceived) and (b":status", b"200") in event.headers for event in events), events
                    assert b"".join(event.data for event in events if isinstance(event, DataReceived)) == b"retry"
                    assert any(isinstance(event, StreamEnded) for event in events)
                else:
                    assert result.stdout.startswith(b"HTTP/1.1 200") and result.stdout.endswith(b"retry"), result.stdout
                print(f"OpenSSL {protocol}: HRR transcript, certificate/Finished authentication and application exchange", flush=True)
            stop(process)
            print("TLS HRR listener shutdown: zero leaks", flush=True)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()


if __name__ == "__main__":
    main()
