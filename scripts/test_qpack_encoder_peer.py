#!/usr/bin/env python3
"""Check a persistent compiled QPACK encoder against pinned independent ls-qpack."""

import argparse
import selectors
import subprocess
import tempfile
from pathlib import Path

import pylsqpack

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// encoder.dark - Outgoing tables survive sections and fragmented decoder feedback.
let feed (state: Stdlib.__QpackEncoder.State) (bytes: Blob) (offset: Int64) : Stdlib.Result.Result<Stdlib.__QpackEncoder.State, String> =
  if offset == Stdlib.Blob.__byteLength bytes then Ok state
  else Stdlib.__QpackEncoder.feed state (Stdlib.__Http2Wire.__slice bytes offset 1L) |> Stdlib.Result.andThen (fun next -> feed next bytes (offset + 1L))
let run (state: Stdlib.__QpackEncoder.State) (index: Int64) : Stdlib.Result.Result<Unit, String> =
  if index == 560L then Ok ()
  else
    let fields = [(":status", "200"), ("x-name", Stdlib.Int64.toString (index / 2L)), ("x-utf8", "café ☃"), ("set-cookie", "a=1"), ("set-cookie", "b=2")] in
    Stdlib.__QpackEncoder.encode state (index * 4L) fields 65536L |> Stdlib.Result.andThen (fun encoded ->
      let _ = Stdlib.printLine ((Stdlib.Blob.toHex encoded.instructions) ++ "|" ++ (Stdlib.Blob.toHex encoded.bytes)) in
      Stdlib.Blob.fromHex (Stdlib.Cli.Stdin.readLine ()) |> Stdlib.Result.andThen (fun bytes -> feed encoded.state bytes 0L) |> Stdlib.Result.andThen (fun next -> run next (index + 1L)))
match Stdlib.__QpackEncoder.create 4096L 1L |> Stdlib.Result.andThen (fun state -> run state 0L) with
| Error message -> Stdlib.printLine message
| Ok _unit -> Stdlib.printLine "done"
'''


def line(process):
    with selectors.DefaultSelector() as selector:
        selector.register(process.stdout, selectors.EVENT_READ)
        assert selector.select(30), "Compiled QPACK encoder exceeded observation budget"
    result = process.stdout.readline()
    assert result, "Compiled QPACK encoder exited early"
    return result.strip()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-qpack-encoder-") as temporary:
        directory = Path(temporary)
        source, binary = directory / "encoder.dark", directory / "encoder"
        source.write_text(SOURCE)
        result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                cwd=ROOT, text=True, capture_output=True, timeout=180)
        assert result.returncode == 0, result.stdout + result.stderr
        process = subprocess.Popen([str(binary)], cwd=ROOT, text=True, stdin=subprocess.PIPE,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=1)
        dynamic, blocked = 0, 0
        decoder = pylsqpack.Decoder(4096, 1)
        try:
            for index in range(560):
                updates, block = (bytes.fromhex(part) for part in line(process).split("|"))
                stream = index * 4
                expected = [(b":status", b"200"), (b"x-name", str(index // 2).encode()),
                            (b"x-utf8", "café ☃".encode()), (b"set-cookie", b"a=1"), (b"set-cookie", b"b=2")]
                if updates and block[0]:
                    try:
                        decoder.feed_header(stream, block)
                    except pylsqpack.StreamBlocked:
                        blocked += 1
                    else:
                        raise AssertionError("New insertion did not block before encoder instructions")
                    unblocked = []
                    for byte in updates:
                        unblocked.extend(decoder.feed_encoder(bytes([byte])))
                    assert unblocked == [stream], unblocked
                    feedback, fields = decoder.resume_header(stream)
                else:
                    assert decoder.feed_encoder(updates) == []
                    feedback, fields = decoder.feed_header(stream, block)
                assert fields == expected, (index, fields)
                dynamic += bool(block[0])
                process.stdin.write(feedback.hex() + "\n")
                process.stdin.flush()
            assert line(process) == "done"
            stdout, stderr = process.communicate(timeout=30)
            assert process.returncode == 0 and not stdout and not stderr, (stdout, stderr)
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate()
        assert dynamic == 560 and blocked >= 280, (dynamic, blocked)
    print(f"560 outgoing QPACK sections, {dynamic} dynamic and {blocked} blocked-before-insertion; independent decoder, fragmented feedback, eviction, wrap-around and compiled cleanup verified")


if __name__ == "__main__":
    main()
