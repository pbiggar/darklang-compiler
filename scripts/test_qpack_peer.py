#!/usr/bin/env python3
"""QPACK interoperability against test-only pylsqpack==0.3.23 (ls-qpack).

No native QPACK implementation is linked into compiled Dark programs.
"""

import subprocess
import tempfile
from pathlib import Path

import pylsqpack

ROOT = Path(__file__).resolve().parents[1]

SOURCE = '''// qpack.dark - Decode independent field sections and re-encode with ownership checks.
match Stdlib.Cli.Args.get 0 with
| Error _ -> Stdlib.printLine "Missing block"
| Ok hex ->
  match Stdlib.Blob.fromHex hex |> Stdlib.Result.andThen Stdlib.Qpack.decode |> Stdlib.Result.andThen Stdlib.Qpack.encode with
  | Error message -> Stdlib.printLine message
  | Ok encoded -> Stdlib.printLine (Stdlib.Blob.toHex encoded)
'''


def main():
    # Includes static indexed fields, name references, Huffman literals,
    # long wrapped static-table values, duplicates and non-ASCII UTF-8.
    cases = [
        [(b":method", b"GET"), (b":scheme", b"https"), (b":authority", b"www.example.com"), (b":path", b"/index.html")],
        [(b":status", b"200"), (b"set-cookie", b"a=1"), (b"set-cookie", b"b=2"), (b"custom-key", b"custom-value")],
        [(b"accept", b"application/dns-message"), (b"cache-control", b"public, max-age=31536000"),
         (b"content-type", b"application/x-www-form-urlencoded"), (b"content-type", b"text/plain;charset=utf-8"),
         (b"strict-transport-security", b"max-age=31536000; includesubdomains; preload"),
         (b"content-security-policy", b"script-src 'none'; object-src 'none'; base-uri 'none'"),
         (b"x-frame-options", b"sameorigin")],
        [(b"x-message", "café ☃".encode()), (b"x-empty", b""), (b"x-long", b"x" * 300)],
    ]
    with tempfile.TemporaryDirectory(prefix="dark-qpack-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "qpack.dark", directory / "qpack"
        source.write_text(SOURCE)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for fields in cases:
            control, block = pylsqpack.Encoder().encode(0, fields)
            assert not control, "Peer unexpectedly used its dynamic table"
            result = subprocess.run([str(binary), block.hex()], cwd=ROOT, text=True,
                                    capture_output=True, timeout=15)
            assert result.returncode == 0 and not result.stderr, result
            output = bytes.fromhex(result.stdout.strip())
            try:
                feedback, decoded = pylsqpack.Decoder(0, 0).feed_header(0, output)
            except pylsqpack.DecompressionFailed as error:
                raise AssertionError((fields, block.hex(), output.hex())) from error
            assert not feedback and decoded == fields, (fields, decoded)
    print("QPACK static/literal interoperability and compiled leak accounting verified")


if __name__ == "__main__":
    main()
