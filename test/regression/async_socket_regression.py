#!/usr/bin/env python3
"""Exercise eager native reads against a peer requiring the second read first."""
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
    print("Eager explicit and source-typed socket reads overlap and preserve owned results")


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
  (Builtin.unwrap (Stdlib.Blob.toString firstBytes)) ++ "|" ++ (Builtin.unwrap (Stdlib.Blob.toString secondBytes))) in
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


if __name__ == "__main__":
    main()
