#!/usr/bin/env python3
"""Exercise eager native socket operations against peers imposing backpressure."""
import pathlib
import platform
import socket
import subprocess
import sys
import tempfile
import threading


def main():
    if platform.system() != "Linux" or platform.machine() not in ("x86_64", "amd64"):
        print("Native async socket regression requires Linux x64")
        return
    compiler = str(pathlib.Path(sys.argv[1]).resolve())
    for transparent in (False, True):
        run_reads(compiler, transparent)
    for mode in ("explicit", "transparent", "abandoned"):
        run_writes(compiler, mode)
    print("Eager socket reads and writes overlap, survive backpressure and retain abandoned producers")


def run_reads(compiler, transparent):
    with tempfile.TemporaryDirectory(prefix="dark-async-socket-") as directory:
        root = pathlib.Path(directory)
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            listener.listen(2)
            listener.settimeout(10)
            port = listener.getsockname()[1]
            source = root / "reads.dark"
            binary = root / "reads"
            program = '''let connect () : Stdlib.__Network.TcpConnection =
  match Stdlib.__Network.connectTcp4 [127L, 0L, 0L, 1L] PORTL 1000L with
  | Ok connection -> connection
  | Error code -> Builtin.crash (Stdlib.Int64.toString code)
let result = Stdlib.__Task.__runMain (fun _unit ->
  let first = connect () in
  let second = connect () in
  let pendingFirst = Stdlib.__Task.__readTcpCurrent first in
  let pendingSecond = Stdlib.__Task.__readTcpCurrent second in
  let secondBytes = Builtin.unwrap (Stdlib.__Task.__forceCurrent pendingSecond) in
  let _ = Builtin.unwrap (Stdlib.__Network.writeTcp second (Stdlib.String.toBlob "ack")) in
  let firstBytes = Builtin.unwrap (Stdlib.__Task.__forceCurrent pendingFirst) in
  let _ = Stdlib.__Network.closeTcp first in
  let _ = Stdlib.__Network.closeTcp second in
  (Builtin.unwrap (Stdlib.Blob.toString firstBytes)) + "|" + (Builtin.unwrap (Stdlib.Blob.toString secondBytes))) in
let _ = Builtin.print result in
0L
'''.replace("PORTL", f"{port}L")
            if transparent:
                program = (
                    "let sourceRead (connection: Stdlib.__Network.TcpConnection) : Stdlib.Result.Result<Blob, Int64> =\n"
                    "  Stdlib.__Task.__transport (Stdlib.__Task.__readTcpCurrent connection)\n"
                    "let passRead (value: Stdlib.Result.Result<Blob, Int64>) : Stdlib.Result.Result<Blob, Int64> = value\n"
                    + program.replace("Stdlib.__Task.__readTcpCurrent first", "passRead (sourceRead first)")
                    .replace("Stdlib.__Task.__readTcpCurrent second", "passRead (sourceRead second)")
                    .replace("(Stdlib.__Task.__forceCurrent pendingSecond)", "pendingSecond")
                    .replace("(Stdlib.__Task.__forceCurrent pendingFirst)", "pendingFirst")
                )
            source.write_text(program, encoding="utf-8")
            subprocess.run([compiler, "-q", "--allow-internal", "--leak-check",
                            str(source), "-o", str(binary)], check=True, timeout=120)
            failures = []

            def serve():
                try:
                    with listener.accept()[0] as first, listener.accept()[0] as second:
                        first.settimeout(5)
                        second.settimeout(5)
                        second.sendall(b"B")
                        acknowledgment = b""
                        while len(acknowledgment) < 3:
                            chunk = second.recv(3 - len(acknowledgment))
                            if not chunk:
                                raise AssertionError("Second read closed before acknowledging")
                            acknowledgment += chunk
                        assert acknowledgment == b"ack", acknowledgment
                        first.sendall(b"A")
                except Exception as error:
                    failures.append(error)

            peer = threading.Thread(target=serve, daemon=True)
            peer.start()
            result = subprocess.run([str(binary)], capture_output=True, timeout=15)
            peer.join(timeout=10)
            assert not peer.is_alive(), "Async peer did not finish"
            assert not failures, failures
            assert (result.returncode, result.stdout, result.stderr) == (0, b"A|B", b""), result


def run_writes(compiler, mode):
    size = 8 * 1024 * 1024
    with tempfile.TemporaryDirectory(prefix="dark-async-write-") as directory:
        root = pathlib.Path(directory)
        with socket.socket() as listener:
            listener.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 65536)
            listener.bind(("127.0.0.1", 0))
            listener.listen(2)
            listener.settimeout(15)
            port = listener.getsockname()[1]
            source = root / "writes.dark"
            binary = root / "writes"
            program = '''let connect () : Stdlib.__Network.TcpConnection =
  Builtin.unwrap (Stdlib.__Network.connectTcp4 [127L, 0L, 0L, 1L] PORTL 1000L)
let result = Stdlib.__Task.__runMain (fun _unit ->
  let first = connect () in
  let second = connect () in
  let pendingFirst = Stdlib.__Task.__writeTcpCurrent first (Stdlib.Blob.__createZeroed SIZEL) in
  let pendingSecond = Stdlib.__Task.__writeTcpCurrent second (Stdlib.String.toBlob "go") in
  let _ = Builtin.unwrap (Stdlib.__Task.__forceCurrent pendingSecond) in
  let acknowledgment = Builtin.unwrap (Stdlib.__Task.__forceCurrent (Stdlib.__Task.__readTcpCurrent second)) in
  let _ = Builtin.unwrap (Stdlib.__Task.__forceCurrent pendingFirst) in
  let _ = Stdlib.__Network.closeTcp first in
  let _ = Stdlib.__Network.closeTcp second in
  Builtin.unwrap (Stdlib.Blob.toString acknowledgment)) in
let _ = Builtin.print result in
0L
'''
            if mode == "transparent":
                program = (
                    "let sourceWrite (connection: Stdlib.__Network.TcpConnection) (bytes: Blob) : Stdlib.Result.Result<Unit, Int64> =\n"
                    "  Stdlib.__Task.__transport (Stdlib.__Task.__writeTcpCurrent connection bytes)\n"
                    "let passWrite (result: Stdlib.Result.Result<Unit, Int64>) : Stdlib.Result.Result<Unit, Int64> = result\n"
                    + program.replace("Stdlib.__Task.__writeTcpCurrent first (Stdlib.Blob.__createZeroed SIZEL)",
                                      "passWrite (sourceWrite first (Stdlib.Blob.__createZeroed SIZEL))")
                    .replace("Stdlib.__Task.__writeTcpCurrent second (Stdlib.String.toBlob \"go\")",
                             "passWrite (sourceWrite second (Stdlib.String.toBlob \"go\"))")
                    .replace("(Stdlib.__Task.__forceCurrent pendingFirst)", "pendingFirst")
                    .replace("(Stdlib.__Task.__forceCurrent pendingSecond)", "pendingSecond")
                )
            elif mode == "abandoned":
                program = program.replace(
                    "  let _ = Builtin.unwrap (Stdlib.__Task.__forceCurrent pendingFirst) in\n"
                    "  let _ = Stdlib.__Network.closeTcp first in\n", "")
            source.write_text(program.replace("PORTL", f"{port}L").replace("SIZEL", f"{size}L"), encoding="utf-8")
            subprocess.run([compiler, "-q", "--allow-internal", "--leak-check",
                            str(source), "-o", str(binary)], check=True, timeout=120)
            failures = []

            def serve():
                try:
                    with listener.accept()[0] as first, listener.accept()[0] as second:
                        first.settimeout(15)
                        second.settimeout(15)
                        # The first write cannot finish within the socket buffers.
                        # No first-connection reads occur until the second operation starts.
                        command = b""
                        while len(command) < 2:
                            chunk = second.recv(2 - len(command))
                            assert chunk, "Second write ended before its command"
                            command += chunk
                        assert command == b"go"
                        second.sendall(b"K")
                        received = 0
                        while received < size:
                            chunk = first.recv(65536)
                            assert chunk, f"Write ended after {received}/{size} bytes"
                            assert not any(chunk), "Write payload was corrupted"
                            received += len(chunk)
                        assert first.recv(1) == b"", "Write sent extra bytes or retained its socket"
                except Exception as error:
                    failures.append(error)

            peer = threading.Thread(target=serve, daemon=True)
            peer.start()
            result = subprocess.run([str(binary)], capture_output=True, timeout=30)
            peer.join(timeout=20)
            assert not peer.is_alive(), "Async write peer did not finish"
            assert not failures, failures
            assert (result.returncode, result.stdout, result.stderr) == (0, b"K", b""), result


if __name__ == "__main__":
    main()
