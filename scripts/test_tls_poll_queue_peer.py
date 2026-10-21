#!/usr/bin/env python3
"""Verify TLS polling under TCP send backpressure with independent record crypto."""

import argparse
import socket
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.hkdf import HKDFExpand

from test_network_nonblocking_peer import ROOT, line

SOURCE = '''// tls-poll.dark - KeyUpdate replies never block a record poll or overtake application data.
let fill (connection: Stdlib.__Network.TcpConnection) (bytes: Blob) (total: Int64) (attempts: Int64) : Stdlib.Result.Result<Int64, String> =
  if attempts >= 4096L then Error "Send queue did not fill"
  else Stdlib.__Network.pollWriteTcp connection bytes |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun sent ->
    match sent with | None -> Ok total | Some size -> fill connection bytes (total + size) (attempts + 1L))
let ready (connection: Stdlib.__Network.TcpConnection) (transport: Stdlib.__HttpTransport.Transport) (deadline: Int64) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Network.monotonicMillis () |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun started ->
    if started >= deadline then Error "TLS poll timed out"
    else
      let polled = transport.poll () in
      Stdlib.__Network.monotonicMillis () |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun finished ->
        if finished - started >= 250L then Error "TLS control reply blocked the poll"
        else polled |> Stdlib.Result.andThen (fun available ->
          match available with
          | Some bytes -> if Stdlib.Blob.toString bytes == Ok "ready" then Ok () else Error "TLS payload changed"
          | None -> match connection.watch with
            | None -> Error "Missing watch"
            | Some watch -> Stdlib.__Network.waitSockets [Stdlib.__Network.Interest { watch = watch, readable = true, writable = false }] 10L
              |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun _events -> ready connection transport deadline))))
let output (connection: Stdlib.__Network.TcpConnection) (transport: Stdlib.__HttpTransport.Transport) (bytes: Blob) (position: Int64) (deadline: Int64) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Network.monotonicMillis () |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun started ->
    if started >= deadline then Error "TLS output timed out"
    else
      let result = if position == Stdlib.Blob.__byteLength bytes then transport.flush () |> Stdlib.Result.map (fun done -> (done, position))
                   else transport.pollWrite (Stdlib.__Http2Wire.__slice bytes position (Stdlib.Blob.__byteLength bytes - position)) |> Stdlib.Result.map (fun sent -> (false, position + Stdlib.Option.withDefault sent 0L)) in
      Stdlib.__Network.monotonicMillis () |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun finished ->
        if finished - started >= 250L then Error "TLS output poll blocked"
        else result |> Stdlib.Result.andThen (fun pair ->
          let (done, next) = pair in
          if done then Ok () else match connection.watch with
          | None -> Error "Missing watch"
          | Some watch -> Stdlib.__Network.waitSockets [Stdlib.__Network.Interest { watch = watch, readable = false, writable = true }] 10L
            |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun _events -> output connection transport bytes next deadline))))
let run (port: Int64) (mode: String) : Stdlib.Result.Result<Unit, String> =
  Stdlib.__Network.connectTcp4 [127L, 0L, 0L, 1L] port 10000L |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun connection ->
    let result =
      let secret = Stdlib.Blob.__fromInt64List (Stdlib.List.repeatUnsafe 32 0L) in
      Stdlib.__Tls13.trafficKeys secret |> Stdlib.Result.andThen (fun keys ->
        let bytes = Stdlib.String.toBlob (Stdlib.String.join (Stdlib.List.repeatUnsafe 16 (Stdlib.String.repeat "y" 1000)) "") in
        fill connection bytes 0L 0L |> Stdlib.Result.andThen (fun total ->
          let transport = Stdlib.__HttpTransport.secure connection (Stdlib.__Tls13Client.SecureKeys { client = keys, server = keys, clientSecret = secret, serverSecret = secret, pending = Stdlib.Blob.empty }) in
          let _ = Stdlib.printLine ("BLOCKED " + Stdlib.Int64.toString total) in
          let result = Stdlib.__Network.monotonicMillis () |> Stdlib.Result.mapError Stdlib.Int64.toString |> Stdlib.Result.andThen (fun started ->
            ready connection transport (started + 5000L) |> Stdlib.Result.andThen (fun _unit ->
              let _ = Stdlib.printLine "POLLED" in
              if mode == "poll" then output connection transport (Stdlib.String.toBlob "after") 0L (started + 10000L)
              else transport.write (Stdlib.String.toBlob "after"))) in
          let _ = Stdlib.__HttpTransport.close transport in result)) in
    let _ = Stdlib.__Network.closeTcp connection in result)
match Stdlib.Cli.__Args.int64 0, Stdlib.Cli.__Args.get 1 with
| Ok port, Ok mode -> match run port mode with | Ok () -> Stdlib.printLine "OK" | Error message -> Stdlib.printLine message
| _, _ -> Stdlib.printLine "Bad arguments"
'''


def expand(secret, label, length):
    label = b"tls13 " + label
    info = length.to_bytes(2, "big") + bytes([len(label)]) + label + b"\0"
    return HKDFExpand(algorithm=hashes.SHA256(), length=length, info=info).derive(secret)


def record(secret, content, kind):
    inner = content + bytes([kind])
    header = b"\x17\x03\x03" + (len(inner) + 16).to_bytes(2, "big")
    return header + AESGCM(expand(secret, b"key", 16)).encrypt(expand(secret, b"iv", 12), inner, header)


def receive(peer, length):
    pieces = bytearray()
    while len(pieces) < length:
        chunk = peer.recv(length - len(pieces))
        assert chunk, "Unexpected socket EOF"
        pieces.extend(chunk)
    return bytes(pieces)


def opened(peer, secret):
    header = receive(peer, 5)
    assert header[:3] == b"\x17\x03\x03", header
    payload = receive(peer, int.from_bytes(header[3:], "big"))
    return AESGCM(expand(secret, b"key", 16)).decrypt(expand(secret, b"iv", 12), payload, header)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="dark-tls-poll-") as temporary:
        source, binary = Path(temporary) / "poll.dark", Path(temporary) / "poll"
        source.write_text(SOURCE)
        compiled = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)], cwd=ROOT, text=True, capture_output=True, timeout=180)
        assert compiled.returncode == 0, compiled.stdout + compiled.stderr
        for mode in ("blocking", "poll"):
          with socket.socket() as listener:
            listener.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 32768)
            listener.bind(("127.0.0.1", 0))
            listener.listen(1)
            listener.settimeout(5)
            process = subprocess.Popen([str(binary), str(listener.getsockname()[1]), mode], cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
            try:
                with listener.accept()[0] as peer:
                    peer.settimeout(5)
                    blocked = line(process)
                    assert blocked.startswith("BLOCKED "), blocked
                    remaining = int(blocked.split()[1])
                    secret = bytes(32)
                    next_secret = expand(secret, b"traffic upd", 32)
                    peer.sendall(record(secret, b"\x18\x00\x00\x01\x01", 22) + record(next_secret, b"ready", 23))
                    assert line(process) == "POLLED", "TLS poll blocked before the peer drained its send queue"
                    while remaining:
                        chunk = receive(peer, min(65536, remaining))
                        assert chunk == b"y" * len(chunk)
                        remaining -= len(chunk)
                    assert opened(peer, secret) == b"\x18\x00\x00\x01\x00\x16", "KeyUpdate reply must precede new-key traffic"
                    assert opened(peer, next_secret) == b"after\x17", "Application record used the wrong keys or sequence"
                    output, errors = process.communicate(timeout=5)
                    assert process.returncode == 0 and output == b"OK\n" and not errors, (output, errors)
            finally:
                if process.poll() is None:
                    process.kill()
                process.communicate()
    print("TLS send backpressure, nonblocking KeyUpdate/application polling, flush, old/new-key order and compiled cleanup passed")


if __name__ == "__main__":
    main()
