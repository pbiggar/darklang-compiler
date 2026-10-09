#!/usr/bin/env python3
"""Check compiled HPACK/QPACK encoders against pinned independent codecs."""

import argparse
import subprocess
import tempfile
from pathlib import Path

import hpack
import pylsqpack
from hpack.huffman import HuffmanEncoder
from hpack.huffman_constants import REQUEST_CODES, REQUEST_CODES_LENGTH

ROOT = Path(__file__).resolve().parents[1]

HUFFMAN = '''// huffman.dark - Encode arbitrary octets without UTF-8 interpretation.
let emit (bytes: Blob) (offset: Int64) : Unit =
  let remaining = Stdlib.Blob.__byteLength bytes - offset in
  if remaining == 0L then ()
  else
    let size = if remaining > 256L then 256L else remaining in
    Stdlib.__Http2Wire.__slice bytes offset size |> Stdlib.Blob.toHex |> Stdlib.printLine
    emit bytes (offset + size)
match Stdlib.Cli.__Args.get 0 with
| Error message -> Stdlib.printLine message
| Ok hex -> match Stdlib.Blob.fromHex hex with
  | Error message -> Stdlib.printLine message
  | Ok bytes -> emit (Stdlib.__Hpack.__encodeHuffman bytes 0L 0L 0L []) 0L
'''

HEADERS = '''// headers.dark - Static references, Huffman names/values and never-indexed credentials.
let fields = [(":method", "GET"), (":scheme", "https"), (":path", "/"), (":authority", "www.example.com"),
  ("content-type", "text/plain"), ("authorization", ""), ("authorization", "Bearer secret"),
  ("proxy-authorization", "Basic secret"), ("cookie", ""), ("set-cookie", "a=1"), ("set-cookie", "b=2"),
  ("www-authenticate", "Basic realm=example"), ("x-custom-name", "café ☃"), ("x-empty", "")] in
match Stdlib.__Hpack.encode fields, Stdlib.__Qpack.encode fields with
| Ok hp, Ok qp ->
  Stdlib.printLine (Stdlib.Blob.toHex hp)
  Stdlib.printLine (Stdlib.Blob.toHex qp)
| _, _ -> Stdlib.printLine "ERROR"
'''


def compile_probe(directory, name, source, compiler):
    path, binary = directory / (name + ".dark"), directory / name
    path.write_text(source)
    result = subprocess.run([str(compiler), str(path), "--allow-internal", "--leak-check", "-o", str(binary)],
                            cwd=ROOT, capture_output=True, text=True, timeout=180)
    assert result.returncode == 0, result.stdout + result.stderr
    return binary


def execute(binary, *arguments):
    result = subprocess.run([str(binary), *arguments], cwd=ROOT, capture_output=True, text=True, timeout=30)
    assert result.returncode == 0 and not result.stderr, (result.stdout, result.stderr)
    return result.stdout.strip().splitlines()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-header-compression-") as temporary:
        directory = Path(temporary)
        huffman = compile_probe(directory, "huffman", HUFFMAN, args.compiler)
        encoder = HuffmanEncoder(REQUEST_CODES, REQUEST_CODES_LENGTH)
        vectors = [bytes([value]) for value in range(256)]
        vectors += [b"", b"www.example.com", bytes(range(256)), bytes(reversed(range(256))),
                    "café ☃".encode(), b"x" * 65504]
        for vector in vectors:
            actual = execute(huffman, vector.hex())
            # Empty output is the valid encoding of an empty input.
            encoded = bytes.fromhex("".join(actual)) if actual else b""
            assert encoded == encoder.encode(vector), vector[:32]
        headers = compile_probe(directory, "headers", HEADERS, args.compiler)
        hp, qp = [bytes.fromhex(line) for line in execute(headers)]
        expected = [(b":method", b"GET"), (b":scheme", b"https"), (b":path", b"/"),
                    (b":authority", b"www.example.com"), (b"content-type", b"text/plain"),
                    (b"authorization", b""), (b"authorization", b"Bearer secret"),
                    (b"proxy-authorization", b"Basic secret"), (b"cookie", b""),
                    (b"set-cookie", b"a=1"), (b"set-cookie", b"b=2"),
                    (b"www-authenticate", b"Basic realm=example"),
                    (b"x-custom-name", "café ☃".encode()), (b"x-empty", b"")]
        decoded = hpack.Decoder().decode(hp, raw=True)
        assert decoded == expected, decoded
        for field in decoded:
            if field[0] in (b"authorization", b"proxy-authorization", b"cookie", b"set-cookie"):
                assert not field.indexable, field
        feedback, decoded = pylsqpack.Decoder(0, 0).feed_header(0, qp)
        assert not feedback and decoded == expected, decoded
    print("262 independent Huffman vectors, HPACK/QPACK field round trips and sensitive-field indexing verified")


if __name__ == "__main__":
    main()
