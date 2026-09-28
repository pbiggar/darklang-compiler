#!/usr/bin/env python3
"""Exercise Dark HTTPS response streaming and truncation against a local TLS peer."""

import socket
import ssl
import subprocess
import tempfile
import threading
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def run(*args: str) -> None:
    subprocess.run(args, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def serve(listener: socket.socket, cert: Path, key: Path) -> None:
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_3
    context.load_cert_chain(str(cert), str(key))
    responses = [
        [b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhe", b"llo world"],
        [b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhe",
         b"llo\r\n6\r\n wo", b"rld\r\n0\r\n\r\n"],
        [b"HTTP/1.1 200 OK\r\nContent-Length: 11\r\n\r\nhello"],
    ]
    for pieces in responses:
        connection, _ = listener.accept()
        with context.wrap_socket(connection, server_side=True) as secure:
            request = b""
            while b"\r\n\r\n" not in request:
                chunk = secure.recv(4096)
                if not chunk:
                    break
                request += chunk
            for piece in pieces:
                secure.sendall(piece)
                time.sleep(0.01)


def main() -> None:
    with tempfile.TemporaryDirectory(prefix="dark-https-stream-") as temporary:
        directory = Path(temporary)
        ca_key, ca_cert = directory / "ca.key", directory / "ca.pem"
        leaf_key, leaf_csr, leaf_cert = (directory / "leaf.key", directory / "leaf.csr",
                                         directory / "leaf.pem")
        extensions = directory / "leaf.ext"
        extensions.write_text("subjectAltName=DNS:localhost\nextendedKeyUsage=serverAuth\n")
        run("openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
            "-keyout", str(ca_key), "-out", str(ca_cert), "-days", "1",
            "-subj", "/CN=Dark stream test CA",
            "-addext", "basicConstraints=critical,CA:TRUE")
        run("openssl", "req", "-newkey", "rsa:2048", "-nodes", "-keyout",
            str(leaf_key), "-out", str(leaf_csr), "-subj", "/CN=localhost")
        run("openssl", "x509", "-req", "-in", str(leaf_csr), "-CA", str(ca_cert),
            "-CAkey", str(ca_key), "-CAcreateserial", "-out", str(leaf_cert),
            "-days", "1", "-extfile", str(extensions))
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            listener.listen(3)
            port = listener.getsockname()[1]
            source = directory / "client.dark"
            source.write_text(f'''// client.dark - Local HTTPS streaming interoperability probe.
match Stdlib.Cli.FileSystem.readFile "{ca_cert}" with
| Error _ -> Stdlib.printLine "CA read failed"
| Ok pem ->
  match Stdlib.Tls13Client.parsePemBundle pem with
  | Error message -> Stdlib.printLine message
  | Ok roots ->
    match Stdlib.HttpClient.streamTrustedWithRoots roots "GET" "https://localhost:{port}/" [] Stdlib.Blob.empty with
    | Error _ -> Stdlib.printLine "Stream request failed"
    | Ok response ->
      let bytes = Stdlib.Stream.toBlob response.body in
      let _ = Stdlib.Stream.close response.body in
      match Stdlib.Blob.toString bytes with
      | Ok body ->
        if response.statusCode == 200 && body == "hello world" then Stdlib.printLine "STREAM OK"
        else Stdlib.printLine "Unexpected stream body"
      | Error _ -> Stdlib.printLine "Stream body is not text"
''')
            binary = directory / "client"
            subprocess.run([str(ROOT / "dark"), str(source), "-o", str(binary)],
                           check=True, cwd=ROOT, stdout=subprocess.DEVNULL)
            peer = threading.Thread(target=serve, args=(listener, leaf_cert, leaf_key),
                                    daemon=True)
            peer.start()
            for name in ("fixed", "chunked"):
                result = subprocess.run([str(binary)], text=True, capture_output=True,
                                        timeout=30, cwd=ROOT)
                if result.returncode != 0 or "STREAM OK" not in result.stdout:
                    raise RuntimeError(f"{name} HTTPS stream failed: {result.stdout} {result.stderr}")
            truncated = subprocess.run([str(binary)], text=True, capture_output=True,
                                       timeout=30, cwd=ROOT)
            if truncated.returncode == 0 or "HTTP fixed-length response was truncated" not in truncated.stderr:
                raise RuntimeError(f"Truncated HTTPS stream was accepted: {truncated.stdout} {truncated.stderr}")
            peer.join(timeout=5)
            if peer.is_alive():
                raise RuntimeError("Local TLS peer did not finish")
            print("HTTPS fixed, chunked, and truncated streaming responses verified")


if __name__ == "__main__":
    main()
