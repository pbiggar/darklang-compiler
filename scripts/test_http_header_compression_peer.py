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

DYNAMIC = '''// dynamic.dark - Consecutive blocks share one connection-owned table.
let step (state: Stdlib.__Hpack.Encoder) (updates: List<Int64>) (fields: List<(String * String)>) : Stdlib.__Hpack.Encoder =
  let resized = Stdlib.List.fold updates (Ok state) (fun result maximum -> result |> Stdlib.Result.andThen (fun current -> Stdlib.__Hpack.resizeEncoder current maximum)) in
  match resized |> Stdlib.Result.andThen (fun current -> Stdlib.__Hpack.encodeDynamic current fields) with
  | Error message -> Builtin.crash message
  | Ok encoded ->
    Stdlib.printLine (Stdlib.Blob.toHex encoded.bytes)
    encoded.state
let state = step (Stdlib.__Hpack.encoder ()) [] [(":method", "GET"), ("x-name", "café ☃"), ("x-name", "café ☃")] in
let state = step state [] [("x-name", "café ☃"), ("x-name", "changed"), ("authorization", "secret"), ("cookie", "a=1")] in
let state = step state [34L] [("x", "y"), ("a", "b"), ("x", "y")] in
let state = step state [0L, 4096L] [("x-name", "restored"), ("set-cookie", "a=1")] in
let state = step state [] [("x-name", "restored"), ("set-cookie", "a=1")] in
let state = step state [0L] [("x", "y")] in
let _ = step state [128L] [("x", "y"), ("x", "z"), ("x", "y")] in
()
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
        dynamic = compile_probe(directory, "dynamic", DYNAMIC, args.compiler)
        blocks = [bytes.fromhex(line) for line in execute(dynamic)]
        expected_blocks = [[(b":method", b"GET"), (b"x-name", "café ☃".encode())] + [(b"x-name", "café ☃".encode())],
                           [(b"x-name", "café ☃".encode()), (b"x-name", b"changed"), (b"authorization", b"secret"), (b"cookie", b"a=1")],
                           [(b"x", b"y"), (b"a", b"b"), (b"x", b"y")],
                           [(b"x-name", b"restored"), (b"set-cookie", b"a=1")],
                           [(b"x-name", b"restored"), (b"set-cookie", b"a=1")],
                           [(b"x", b"y")], [(b"x", b"y"), (b"x", b"z"), (b"x", b"y")]]
        decoder = hpack.Decoder()
        assert len(blocks) == len(expected_blocks)
        for block, fields in zip(blocks, expected_blocks):
            decoded = decoder.decode(block, raw=True)
            assert decoded == fields, (block.hex(), decoded, fields)
            for field in decoded:
                if field[0] in (b"authorization", b"cookie", b"set-cookie"):
                    assert not field.indexable, field
        assert blocks[4][0] == 0xBE and blocks[3].startswith(bytes.fromhex("203fe11f")), [b.hex() for b in blocks]
    print("262 independent Huffman vectors, HPACK/QPACK fields, seven stateful HPACK blocks, eviction, resize ordering and sensitive literals verified")


if __name__ == "__main__":
    main()
