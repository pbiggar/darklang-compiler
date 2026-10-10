#!/usr/bin/env python3
"""Check compiled dynamic QPACK decoding against pinned independent ls-qpack."""

import argparse
import subprocess
import tempfile
from pathlib import Path

import pylsqpack

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// decoder.dark - Fragmented encoder instructions and dynamic header sections.
let feed (state: Stdlib.__QpackDecoder.State) (bytes: Blob) (offset: Int64) : Stdlib.Result.Result<Stdlib.__QpackDecoder.State, String> =
  if offset == Stdlib.Blob.__byteLength bytes then Ok state
  else Stdlib.__QpackDecoder.feed state (Stdlib.__Http2Wire.__slice bytes offset 1L) |> Stdlib.Result.andThen (fun received -> feed received.state bytes (offset + 1L))
let run () : Stdlib.Result.Result<Blob, String> =
  Stdlib.Cli.__Args.get 0 |> Stdlib.Result.andThen Stdlib.Blob.fromHex |> Stdlib.Result.andThen (fun instructions ->
    Stdlib.Cli.__Args.get 1 |> Stdlib.Result.andThen Stdlib.Blob.fromHex |> Stdlib.Result.andThen (fun header ->
      Stdlib.__QpackDecoder.create 4096L |> Stdlib.Result.andThen (fun state -> feed state instructions 0L) |> Stdlib.Result.andThen (fun state ->
        Stdlib.__QpackDecoder.decode state header |> Stdlib.Result.andThen (fun section ->
          match section with | Blocked _ -> Error "Blocked" | Decoded fields -> Stdlib.__Qpack.encode fields.fields))))
match run () with | Error message -> Stdlib.printLine message | Ok bytes -> Stdlib.printLine (Stdlib.Blob.toHex bytes)
'''


def execute(binary, instructions, block):
    result = subprocess.run([str(binary), instructions.hex(), block.hex()], cwd=ROOT,
                            text=True, capture_output=True, timeout=15)
    assert result.returncode == 0 and not result.stderr, result
    return result.stdout.strip()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-qpack-dynamic-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "decoder.dark", directory / "decoder"
        source.write_text(SOURCE)
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                cwd=ROOT, text=True, capture_output=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        encoder = pylsqpack.Encoder()
        instructions = bytearray(encoder.apply_settings(4096, 1))
        dynamic, blocked, checks = 0, 0, 0
        # More than 256 insertions exercise Required Insert Count wrap-around,
        # while a 4 KiB table also forces repeated eviction.
        for index in range(280):
            fields = [(b":status", b"200"), (b"x-name", str(index).encode()),
                      (b"x-utf8", "café ☃".encode()), (b"set-cookie", b"a=1"), (b"set-cookie", b"b=2")]
            for repetition in range(2):
                stream = (index * 2 + repetition) * 4
                updates, block = encoder.encode(stream, fields)
                previous = bytes(instructions)
                instructions.extend(updates)
                if updates and block[0]:
                    assert execute(binary, previous, block) == "Blocked", (index, updates.hex(), block.hex())
                    blocked += 1
                actual = execute(binary, bytes(instructions), block)
                decoded = pylsqpack.Decoder(0, 0).feed_header(stream, bytes.fromhex(actual))[1]
                assert decoded == fields, (index, repetition, decoded, fields)
                checks += 1
                if block[0]:
                    # Section acknowledgments release references and advance
                    # the peer's known received count for subsequent blocks.
                    encoder.feed_decoder(bytes([0x80 | stream]) if stream < 127 else prefixed_integer(stream))
                    dynamic += 1
        assert dynamic >= 280 and blocked >= 256, (dynamic, blocked)
    print(f"{checks} independent QPACK blocks, {dynamic} dynamic sections, {blocked} blocked-before-insert checks; fragmentation, eviction, wrap-around and compiled cleanup verified")


def prefixed_integer(value):
    output = bytearray([0xFF])
    value -= 127
    while value >= 128:
        output.append(0x80 | (value % 128))
        value //= 128
    output.append(value)
    return bytes(output)


if __name__ == "__main__":
    main()
