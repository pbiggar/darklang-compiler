#!/usr/bin/env python3
"""Cross-check compiled 1-RTT STREAM codecs and reassembly with aioquic buffers/streams."""
from pathlib import Path
import random
import subprocess
import tempfile

from aioquic.buffer import Buffer
from aioquic.quic.packet import QuicStreamFrame
from aioquic.quic.stream import QuicStreamReceiver

ROOT = Path(__file__).resolve().parents[1]
DARK = '''// application.dark - Independent application frame fields and stream delivery.
let number (value: Int64) : String = Stdlib.Int64.toString value
let boolean (value: Bool) : String = if value then "true" else "false"
let printFrame (frame: Stdlib.QuicApplication.Frame) : Unit =
  match frame with
  | Stream value -> Stdlib.printLine ("STREAM " ++ number value.id ++ " " ++ number value.offset ++ " " ++ boolean value.fin ++ " " ++ Stdlib.Blob.toHex value.bytes)
  | ResetStream value -> Stdlib.printLine ("RESET " ++ number value.id ++ " " ++ number value.code ++ " " ++ number value.finalSize)
  | StopSending value -> Stdlib.printLine ("STOP " ++ number value.id ++ " " ++ number value.value)
  | MaxStreamData value -> Stdlib.printLine ("STREAMLIMIT " ++ number value.id ++ " " ++ number value.value)
  | StreamDataBlocked value -> Stdlib.printLine ("STREAMBLOCKED " ++ number value.id ++ " " ++ number value.value)
  | MaxData value -> Stdlib.printLine ("MAXDATA " ++ number value)
  | MaxStreamsBidi value -> Stdlib.printLine ("BIDI " ++ number value)
  | MaxStreamsUni value -> Stdlib.printLine ("UNI " ++ number value)
  | DataBlocked value -> Stdlib.printLine ("DATABLOCKED " ++ number value)
  | StreamsBlockedBidi value -> Stdlib.printLine ("BIDIBLOCKED " ++ number value)
  | StreamsBlockedUni value -> Stdlib.printLine ("UNIBLOCKED " ++ number value)
  | NewConnectionId value -> Stdlib.printLine ("CID " ++ number value.sequence ++ " " ++ number value.retirePriorTo ++ " " ++ Stdlib.Blob.toHex value.id ++ " " ++ Stdlib.Blob.toHex value.resetToken)
  | RetireConnectionId value -> Stdlib.printLine ("RETIRE " ++ number value)
  | PathChallenge value -> Stdlib.printLine ("CHALLENGE " ++ Stdlib.Blob.toHex value)
  | PathResponse value -> Stdlib.printLine ("RESPONSE " ++ Stdlib.Blob.toHex value)
  | NewToken value -> Stdlib.printLine ("TOKEN " ++ Stdlib.Blob.toHex value)
  | HandshakeDone -> Stdlib.printLine "DONE"
  | ApplicationClose value -> Stdlib.printLine ("CLOSE " ++ number value.code ++ " " ++ Stdlib.Blob.toHex value.reason)
  | Handshake _ -> Stdlib.printLine "HANDSHAKE"
let printFrames (frames: List<Stdlib.QuicApplication.Frame>) : Unit =
  match frames with | [] -> () | frame :: rest -> let _ = printFrame frame in printFrames rest
let assemble (state: Stdlib.QuicStreams.Receive) (frames: List<Stdlib.QuicApplication.Frame>) (chunks: List<Blob>)
  : Stdlib.Result.Result<String, String> =
  match frames with
  | [] -> Ok (Stdlib.Blob.toHex (Stdlib.Blob.concat (Stdlib.List.reverse chunks)) ++ " " ++ boolean (Stdlib.QuicStreams.complete state) ++ " " ++ number state.highest)
  | Stream value :: rest ->
    match Stdlib.QuicStreams.insert state value.offset value.bytes value.fin with
    | Error message -> Error message
    | Ok next -> assemble (Stdlib.QuicStreams.drain next) rest (Stdlib.List.push chunks (Stdlib.QuicStreams.available next))
  | _ :: rest -> assemble state rest chunks
let runCheck () : Unit =
  match Stdlib.Cli.Args.get 0, Stdlib.Cli.Args.get 1 |> Stdlib.Result.andThen Stdlib.Blob.fromHex with
  | Ok mode, Ok bytes ->
    match Stdlib.QuicApplication.parse (mode != "client") bytes with
    | Error message -> Stdlib.printLine ("ERROR " ++ message)
    | Ok frames ->
      if mode == "assemble" then
        match Stdlib.QuicStreams.receiver 4096L |> Stdlib.Result.andThen (fun state -> assemble state frames []) with
        | Error message -> Stdlib.printLine ("ERROR " ++ message)
        | Ok value -> Stdlib.printLine value
      else if mode == "encode" then
        match frames with
        | Stream value :: _ ->
          match Stdlib.QuicApplication.stream value.id value.offset value.bytes value.fin with
          | Error message -> Stdlib.printLine ("ERROR " ++ message)
          | Ok wire -> Stdlib.printLine (Stdlib.Blob.toHex wire)
        | _ -> Stdlib.printLine "ERROR No stream frame"
      else printFrames frames
  | _ -> Stdlib.printLine "ERROR Bad arguments"
runCheck ()
'''


def integer(value):
    buffer = Buffer(capacity=8)
    buffer.push_uint_var(value)
    return buffer.data


def stream(identifier, offset, data, fin, flags=6):
    kind = 8 | flags | int(fin)
    return bytes([kind]) + integer(identifier) + (integer(offset) if flags & 4 else b"") + (integer(len(data)) if flags & 2 else b"") + data


def main():
    with tempfile.TemporaryDirectory(prefix="dark-quic-application-") as temporary:
        source, binary = Path(temporary) / "application.dark", Path(temporary) / "application"
        source.write_text(DARK)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr

        def run(mode, data):
            result = subprocess.run([str(binary), mode, data.hex()], cwd=ROOT, text=True,
                                    capture_output=True, timeout=20)
            assert result.returncode == 0 and not result.stderr, result
            return result.stdout.rstrip("\n")

        for flags in (0, 2, 4, 6):
            for fin in (False, True):
                for identifier in (0, 3, 63, 64, 16384, 2**62 - 1):
                    offset = 2**32 if flags & 4 else 0
                    data = bytes(range(16))
                    wire = stream(identifier, offset, data, fin, flags)
                    assert run("decode", wire) == f"STREAM {identifier} {offset} {str(fin).lower()} {data.hex().upper()}"
                    encoded = Buffer(data=bytes.fromhex(run("encode", wire)))
                    assert encoded.pull_uint8() == 14 + int(fin)
                    assert encoded.pull_uint_var() == identifier
                    assert encoded.pull_uint_var() == offset
                    assert encoded.pull_uint_var() == len(data)
                    assert encoded.pull_bytes(len(data)) == data and encoded.eof()

        data = bytes(range(256)) * 2
        for seed in range(40):
            rng = random.Random(seed)
            pieces = [(n, data[n:n + 16], False) for n in range(0, len(data), 16)]
            pieces += rng.sample(pieces, 8) + [(len(data), b"", True)]
            rng.shuffle(pieces)
            receiver = QuicStreamReceiver(stream_id=0, readable=True)
            chunks = bytearray()
            for offset, piece, fin in pieces:
                event = receiver.handle_frame(QuicStreamFrame(offset=offset, data=piece, fin=fin))
                if event is not None:
                    chunks.extend(event.data)
            wire = b"".join(stream(0, offset, piece, fin) for offset, piece, fin in pieces)
            assert run("assemble", wire) == f"{chunks.hex().upper()} true {len(data)}"

        values = [(4, [4, 17, 12345], "RESET 4 17 12345"), (5, [4, 17], "STOP 4 17"),
                  (16, [2**62 - 1], f"MAXDATA {2**62 - 1}"), (17, [4, 8192], "STREAMLIMIT 4 8192"),
                  (18, [2**60], f"BIDI {2**60}"), (19, [3], "UNI 3"), (20, [20], "DATABLOCKED 20"),
                  (21, [4, 100], "STREAMBLOCKED 4 100"), (22, [4], "BIDIBLOCKED 4"),
                  (23, [3], "UNIBLOCKED 3"), (25, [19], "RETIRE 19")]
        for kind, fields, expected in values:
            assert run("decode", bytes([kind]) + b"".join(integer(v) for v in fields)) == expected
        for size in (1, 8, 20):
            cid, token = bytes(range(size)), bytes(range(16))
            wire = b"\x18" + integer(3) + integer(1) + bytes([size]) + cid + token
            assert run("decode", wire) == f"CID 3 1 {cid.hex().upper()} {token.hex().upper()}"
        assert run("decode", b"\x1d" + integer(256) + integer(3) + b"bye") == "CLOSE 256 627965"
        assert run("decode", b"\x07\x01\xaa") == "TOKEN AA"
        for kind, prefix in [(26, "CHALLENGE"), (27, "RESPONSE")]:
            assert run("decode", bytes([kind]) + bytes(range(8))) == prefix + " 0001020304050607"
        invalid = [(b"\x0f\x00\x00\x02\xaa", "Truncated QUIC STREAM frame"),
                   (stream(0, 2**62 - 1, b"x", False), "Invalid QUIC STREAM range"),
                   (b"\x18\x00\x01", "Invalid QUIC retire-prior-to"),
                   (b"\x18\x00\x00\x15" + bytes(37), "Invalid QUIC new connection ID length"),
                   (b"\x18\x00\x00\x01\xaa", "Truncated QUIC connection ID"),
                   (b"\x1d\x00\x02\xaa", "Truncated QUIC close reason"),
                   (b"\x12" + integer(2**60 + 1), "QUIC stream count exceeds 2^60"),
                   (b"\x01" * 1025, "Too many QUIC application frames")]
        for wire, message in invalid:
            assert run("decode", wire) == "ERROR " + message
        assert run("client", b"\x1e") == "ERROR Client sent QUIC HANDSHAKE_DONE"
        assert run("client", b"\x07\x01\xaa") == "ERROR Client sent QUIC NEW_TOKEN"
    print("QUIC application frames: 48 flag/ID combinations, control fields, 40 reordered streams, bounds and cleanup passed")


if __name__ == "__main__":
    main()
