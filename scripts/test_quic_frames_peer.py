#!/usr/bin/env python3
"""Independent ACK/frame checks and reordered CRYPTO reassembly against aioquic."""

import random
import subprocess
import tempfile
from pathlib import Path

from aioquic.buffer import Buffer
from aioquic.quic.packet import QuicStreamFrame, pull_ack_frame, push_ack_frame
from aioquic.quic.rangeset import RangeSet
from aioquic.quic.stream import QuicStreamReceiver

ROOT = Path(__file__).resolve().parents[1]
DARK = """// frames.dark - Frame fields, owned ordered bytes, and independent codecs.
let printFrame (frame: Stdlib.__QuicFrames.Frame) : Unit =
  match frame with
  | Padding count -> Stdlib.printLine ("PAD " ++ Stdlib.Int64.toString count)
  | Ping -> Stdlib.printLine "PING"
  | Crypto value -> Stdlib.printLine ("CRYPTO " ++ Stdlib.Int64.toString value.offset ++ " " ++ Stdlib.Blob.toHex value.bytes)
  | Close value -> Stdlib.printLine ("CLOSE " ++ Stdlib.Int64.toString value.code ++ " " ++ Stdlib.Int64.toString value.kind ++ " " ++ Stdlib.Blob.toHex value.reason)
  | Ack value ->
    let ranges = Stdlib.List.map value.ranges (fun range -> Stdlib.Int64.toString range.smallest ++ ":" ++ Stdlib.Int64.toString range.largest) in
    let ecn = match value.ecn with | None -> "-" | Some count -> Stdlib.Int64.toString count.ect0 ++ ":" ++ Stdlib.Int64.toString count.ect1 ++ ":" ++ Stdlib.Int64.toString count.ce in
    Stdlib.printLine ("ACK " ++ Stdlib.Int64.toString value.delay ++ " " ++ Stdlib.String.join ranges "," ++ " " ++ ecn)
let printFrames (frames: List<Stdlib.__QuicFrames.Frame>) : Unit =
  match frames with | [] -> () | frame :: rest -> let _ = printFrame frame in printFrames rest
let assemble (state: Stdlib.__QuicReassembly.State) (frames: List<Stdlib.__QuicFrames.Frame>) (chunks: List<Blob>)
  : Stdlib.Result.Result<Blob, String> =
  match frames with
  | [] -> Ok (Stdlib.Blob.concat (Stdlib.List.reverse chunks))
  | Crypto frame :: rest ->
    match Stdlib.__QuicReassembly.insert state frame.offset frame.bytes with
    | Error message -> Error message
    | Ok next ->
      let bytes = Stdlib.__QuicReassembly.available next in
      assemble (Stdlib.__QuicReassembly.drain next) rest (Stdlib.List.push chunks bytes)
  | _ :: rest -> assemble state rest chunks
let checkFrames () : Unit =
  match Stdlib.Cli.__Args.get 0, Stdlib.Cli.__Args.get 1 |> Stdlib.Result.andThen Stdlib.Blob.fromHex with
  | Ok mode, Ok data ->
    if mode == "ack" then
      let result =
        match Stdlib.__QuicWire.parseInteger data 0L with
        | Error _ -> Error "Invalid number"
        | Ok number -> Stdlib.__QuicFrames.acknowledge number.value in
      match result with | Error message -> Stdlib.printLine ("ERROR " ++ message) | Ok ack -> Stdlib.printLine (Stdlib.Blob.toHex ack)
    else
      let result = Stdlib.__QuicFrames.parseHandshake data in
      match result with
      | Error message -> Stdlib.printLine ("ERROR " ++ message)
      | Ok frames ->
        if mode == "decode" then printFrames frames
        else
          let assembled = Stdlib.__QuicReassembly.create 4096L |> Stdlib.Result.andThen (fun state -> assemble state frames []) in
          match assembled with | Error message -> Stdlib.printLine ("ERROR " ++ message) | Ok bytes -> Stdlib.printLine (Stdlib.Blob.toHex bytes)
  | _ -> Stdlib.printLine "Bad arguments"
checkFrames ()
"""


def integer(value):
    buf = Buffer(capacity=8)
    buf.push_uint_var(value)
    return buf.data


def crypto(offset, data):
    return b"\x06" + integer(offset) + integer(len(data)) + data


def main():
    with tempfile.TemporaryDirectory(prefix="dark-quic-frames-") as temporary:
        source, binary = Path(temporary) / "frames.dark", Path(temporary) / "frames"
        source.write_text(DARK)
        compiled = subprocess.run([str(ROOT / "dark"), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                  cwd=ROOT, text=True, capture_output=True, timeout=120)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr

        def run(mode, data):
            result = subprocess.run([str(binary), mode, data.hex()], cwd=ROOT,
                                    text=True, capture_output=True, timeout=20)
            assert result.returncode == 0 and not result.stderr, result
            return result.stdout.strip()

        for ranges in ([range(0, 1)], [range(0, 3), range(5, 10)], [range(1000, 1024), range(4000, 4010)], [range(2**62 - 8, 2**62)]):
            rangeset = RangeSet(ranges)
            buf = Buffer(capacity=4096)
            buf.push_uint8(2)
            push_ack_frame(buf, rangeset, delay=12345)
            decoded, delay = pull_ack_frame(Buffer(data=buf.data[1:]))
            expected = ",".join(f"{item.start}:{item.stop - 1}" for item in reversed(list(decoded)))
            assert run("decode", buf.data) == f"ACK {delay} {expected} -"
            with_ecn = b"\x03" + buf.data[1:] + integer(100) + integer(200) + integer(300)
            assert run("decode", with_ecn) == f"ACK {delay} {expected} 100:200:300"
        for number in (0, 63, 64, 16384, 2**32, 2**62 - 1):
            ack = bytes.fromhex(run("ack", integer(number)))
            ranges, delay = pull_ack_frame(Buffer(data=ack[1:]))
            assert ack[0] == 2 and list(ranges) == [range(number, number + 1)] and delay == 0
        data = bytes(range(256)) * 2
        for seed in range(30):
            rng = random.Random(seed)
            pieces = [(offset, data[offset:offset + 16]) for offset in range(0, len(data), 16)]
            pieces += rng.sample(pieces, 10)
            pieces += [(offset, data[offset:offset + 31]) for offset in rng.sample(range(len(data) - 31), 10)]
            rng.shuffle(pieces)
            receiver = QuicStreamReceiver(stream_id=None, readable=True)
            ordered = bytearray()
            for offset, piece in pieces:
                event = receiver.handle_frame(QuicStreamFrame(data=piece, offset=offset, fin=False))
                if event is not None:
                    ordered.extend(event.data)
            assert ordered == data
            assert run("assemble", b"".join(crypto(offset, piece) for offset, piece in pieces)) == data.hex().upper()
        assert run("decode", b"\x00" * 1200) == "PAD 1200"
        assert run("decode", b"\x1c" + integer(256) + integer(6) + integer(4) + b"oops") == "CLOSE 256 6 6F6F7073"
        assert run("assemble", crypto(1, b"abc") + crypto(2, b"XX")) == "ERROR Conflicting QUIC CRYPTO bytes"
        assert run("assemble", b"".join(crypto(2 * index + 1, b"x") for index in range(257))) == "ERROR Too many QUIC CRYPTO fragments"
        assert run("decode", b"\x02\x00\x00" + integer(256) + b"\x00") == "ERROR Too many QUIC ACK ranges"
    print("QUIC ACK/ECN codecs and 30 reordered/overlapping/retransmitted CRYPTO streams verified against aioquic")


if __name__ == "__main__":
    main()
