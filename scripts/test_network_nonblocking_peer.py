#!/usr/bin/env python3
"""Verify per-call nonblocking reads, partial writes, backpressure and EOF."""

import argparse
import selectors
import socket
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE = '''// nonblocking.dark - Socket polling does not inherit the ten-second read timeout.
let readAll (connection: Stdlib.__Network.TcpConnection) (deadline: Int64) (pieces: List<Blob>) : Stdlib.Result.Result<Blob, String> =
  match Stdlib.__Network.monotonicMillis () with
  | Error _ -> Error "Clock failed"
  | Ok now ->
    if now >= deadline then Error "Read timed out"
    else Stdlib.__Network.pollReadTcp connection |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun available ->
      match available with
      | Some bytes ->
        if Stdlib.Blob.__byteLength bytes == 0L then Ok (Stdlib.Blob.concat (Stdlib.List.reverse pieces))
        else readAll connection deadline (Stdlib.List.push pieces bytes)
      | None -> match connection.watch with
        | None -> Error "Missing watch"
        | Some watch -> Stdlib.__Network.waitSockets [Stdlib.__Network.Interest { watch = watch, readable = true, writable = false }] 100L
          |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun _events -> readAll connection deadline pieces))
let fill (connection: Stdlib.__Network.TcpConnection) (bytes: Blob) (started: Int64) (total: Int64) (attempts: Int64) : Stdlib.Result.Result<Int64, String> =
  match Stdlib.__Network.monotonicMillis () with
  | Error _ -> Error "Clock failed"
  | Ok now ->
    if now - started > 2000L || attempts >= 4096L then Error "Write blocked instead of returning backpressure"
    else Stdlib.__Network.pollWriteTcp connection bytes |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun sent ->
      match sent with | None -> Ok total | Some size -> fill connection bytes started (total + size) (attempts + 1L))
let run (port: Int64) (mode: String) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Network.connectTcp4 [127L, 0L, 0L, 1L] port 10000L |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun connection ->
    let result = Stdlib.__Network.monotonicMillis () |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun started ->
      Stdlib.__Network.pollReadTcp connection |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun empty ->
        match Stdlib.__Network.monotonicMillis () with
        | Error _ -> Error "Clock failed"
        | Ok now ->
          if empty != None || now - started > 250L then Error "Unready socket read blocked"
          else if mode == "fill" then
            let bytes = Stdlib.String.toBlob (Stdlib.String.join (Stdlib.List.repeatUnsafe 16 (Stdlib.String.repeat "y" 1000)) "") in
            fill connection bytes now 0L 0L |> Stdlib.Result.andThen (fun total ->
              if total == 0L then Error "Write made no progress" else let _ = Stdlib.printLine ("BLOCKED " + Stdlib.Int64.toString total) in Ok ())
          else Stdlib.__Network.pollWriteTcp connection (Stdlib.String.toBlob "HELLO") |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun sent ->
            if sent != Some 5L then Error "Small write failed"
            else readAll connection (now + 5000L) [] |> Stdlib.Result.andThen (fun bytes -> if Stdlib.Blob.toString bytes == Ok "packet" then Ok () else Error "Payload changed")))) in
    let _ = Stdlib.__Network.closeTcp connection in
    if Stdlib.__Network.pollReadTcp connection != Error 9L || Stdlib.__Network.pollWriteTcp connection Stdlib.Blob.empty != Error 9L then Error "Closed descriptor accessed" else result)
match Stdlib.Cli.__Args.int64 0, Stdlib.Cli.__Args.get 1 with
| Ok port, Ok mode -> match run port mode with | Ok () -> Stdlib.printLine "OK" | Error message -> Stdlib.printLine message
| _, _ -> Stdlib.printLine "Bad arguments"
'''


def line(process):
    with selectors.DefaultSelector() as selector:
        selector.register(process.stdout, selectors.EVENT_READ)
        assert selector.select(5), "Nonblocking operation stalled"
    return process.stdout.readline().decode().strip()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-nonblocking-") as temporary:
        source, binary = Path(temporary) / "poll.dark", Path(temporary) / "poll"
        source.write_text(SOURCE)
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, text=True, capture_output=True, timeout=180)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for mode in ("read", "fill"):
            with socket.socket() as listener:
                listener.bind(("127.0.0.1", 0))
                listener.listen(1)
                listener.settimeout(5)
                process = subprocess.Popen([str(binary), str(listener.getsockname()[1]), mode], cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
                try:
                    with listener.accept()[0] as peer:
                        peer.settimeout(5)
                        if mode == "read":
                            assert peer.recv(5) == b"HELLO"
                            peer.sendall(b"packet")
                            peer.shutdown(socket.SHUT_WR)
                        else:
                            blocked = line(process)
                            assert blocked.startswith("BLOCKED "), blocked
                            expected, received = int(blocked.split()[1]), 0
                            while chunk := peer.recv(65536):
                                assert chunk == b"y" * len(chunk), "Partial write corrupted payload"
                                received += len(chunk)
                            assert received == expected, (received, expected)
                        output, errors = process.communicate(timeout=5)
                        assert process.returncode == 0 and output == b"OK\n" and not errors, (output, errors)
                finally:
                    if process.poll() is None:
                        process.kill()
                    process.communicate()
    print("Unready read, available bytes, EOF, send backpressure, partial-write accounting, closed owners and cleanup passed")


if __name__ == "__main__":
    main()
