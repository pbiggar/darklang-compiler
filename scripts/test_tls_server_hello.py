#!/usr/bin/env python3
"""Check Dark TLS server negotiation against OpenSSL and independent malformed offers."""

import argparse
import ssl
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def u16(value):
    return value.to_bytes(2, "big")


def extension(kind, data):
    return u16(kind) + u16(len(data)) + data


def hello(extensions, *, session=b"", ciphers=b"\x13\x01", compression=b"\x01\x00"):
    body = (b"\x03\x03" + bytes(range(32)) + bytes([len(session)]) + session
            + u16(len(ciphers)) + ciphers + compression + u16(len(extensions)) + extensions)
    return b"\x01" + len(body).to_bytes(3, "big") + body


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    share = bytes(range(32))
    required = [extension(43, b"\x02\x03\x04"), extension(10, b"\x00\x02\x00\x1d"),
                extension(13, b"\x00\x02\x08\x04"),
                extension(51, b"\x00\x24\x00\x1d\x00\x20" + share)]
    alpn = extension(16, b"\x00\x07\x02\xff\xfe\x03foo")
    h2 = extension(16, b"\x00\x07\x02h2\x03foo")
    vectors = [(hello(b"".join(required) + h2), True, "h2", True),
               (hello(b"".join(required)), True, "", True),
               (hello(b"".join(required) + alpn), True, "error", True),
               (hello(b"".join(required) + extension(65000, b"opaque") + h2,
                      session=bytes(range(32)), ciphers=b"\xfa\xfa\x13\x01"), True, "h2", True),
               (hello(b"".join(required[:3]) + extension(51, b"\x00\x00")), True, "", False)]
    for index in range(4):
        vectors.append((hello(b"".join(required[:index] + required[index + 1:])), False, "", False))
    vectors += [(hello(b"".join(required) + required[0]), False, "", False),
                (hello(b"".join(required) + extension(16, b"\x00\x01\x00")), False, "", False),
                (hello(b"".join(required), session=bytes(33)), False, "", False),
                (hello(b"".join(required), compression=b"\x01\x01"), False, "", False),
                (hello(b"".join(required), ciphers=b"\x13\x02"), False, "", False),
                (hello(b"".join(required), ciphers=b"\x13"), False, "", False)]
    for data in (b"\x00\x23\x00\x1d\x00\x1f" + share[:-1],
                 b"\x00\x48" + (b"\x00\x1d\x00\x20" + share) * 2,
                 b"\x00\x05\x00\x17\x00\x01x", b"\x00\x01x"):
        vectors.append((hello(b"".join(required[:3]) + extension(51, data)), False, "", False))
    for data in (b"\x01\x03", b"\x04\x03\x04", b"\x02\x03\x03"):
        vectors.append((hello(extension(43, data) + b"".join(required[1:])), False, "", False))
    vectors += [(hello(b"".join(required) + extension(41, b"") + h2), False, "", False),
                (hello(b"".join(required) + extension(64000, b"x") * 65), False, "", False)]
    valid = vectors[0][0]
    vectors += [(valid[:-1], False, "", False), (valid + b"x", False, "", False),
                (bytes([2]) + valid[1:], False, "", False)]
    # OpenSSL's actual client serialization exercises the compatibility session,
    # real key share and extra extensions independently of our constructors.
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    context.minimum_version = context.maximum_version = ssl.TLSVersion.TLSv1_3
    context.set_ecdh_curve("X25519")
    context.set_alpn_protocols(["h2", "http/1.1"])
    incoming, outgoing = ssl.MemoryBIO(), ssl.MemoryBIO()
    client = context.wrap_bio(incoming, outgoing, server_side=False, server_hostname="localhost")
    try:
        client.do_handshake()
    except ssl.SSLWantReadError:
        pass
    wire = outgoing.read()
    assert wire[0] == 22 and int.from_bytes(wire[3:5], "big") == len(wire) - 5
    vectors.append((wire[5:], True, "h2", True))
    checks = [f'check "{data.hex()}" {str(valid).lower()} "{selected}" {str(has_share).lower()}'
              for data, valid, selected, has_share in vectors]
    source = '''// hello.dark - Independent client negotiation and bounded malformed-input cleanup.
let check (text: String) (valid: Bool) (selected: String) (hasShare: Bool) : Bool =
  match Stdlib.Blob.fromHex text |> Stdlib.Result.andThen Stdlib.Tls13ServerHello.parse with
  | Error _ -> Stdlib.Bool.not valid
  | Ok hello ->
    let share = match hello.keyShare with | None -> false | Some _ -> true in
    let protocol = match Stdlib.Tls13ServerHello.selectProtocol hello ["h2", "http/1.1"] with | Error _ -> "error" | Ok name -> name in
    valid && share == hasShare && protocol == selected
''' + '\nStdlib.printLine (if ' + ' &&\n  '.join(checks) + ' then "DONE" else "FAILED")\n'
    with tempfile.TemporaryDirectory(prefix="dark-server-hello-") as temporary:
        path, binary = Path(temporary) / "hello.dark", Path(temporary) / "hello"
        path.write_text(source)
        compiled = subprocess.run([str(args.compiler), str(path), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        result = subprocess.run([str(binary)], cwd=ROOT, text=True, capture_output=True, timeout=30)
        assert result.returncode == 0 and not result.stderr and result.stdout.strip() == "DONE", result
    print(f"TLS server ClientHello verified: {len(vectors)} OpenSSL, negotiation and rejection cases")


if __name__ == "__main__":
    main()
