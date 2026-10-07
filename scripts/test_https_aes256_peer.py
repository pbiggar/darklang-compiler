#!/usr/bin/env python3
"""Exercise the Dark TLS client against a forced local TLS 1.3 cipher suite."""

import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def run(*args: str) -> None:
    subprocess.run(args, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def wait_for_port(port: int, peer: subprocess.Popen[bytes]) -> None:
    for _ in range(100):
        if peer.poll() is not None:
            raise RuntimeError("OpenSSL TLS peer exited before accepting connections")
        try:
            with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                return
        except OSError:
            time.sleep(0.05)
    raise RuntimeError("OpenSSL TLS peer did not start")


def main() -> None:
    suite = sys.argv[1] if len(sys.argv) > 1 else "TLS_AES_256_GCM_SHA384"
    authentication = sys.argv[2] if len(sys.argv) > 2 else "rsa"
    ca_authentication = sys.argv[3] if len(sys.argv) > 3 else "rsa"
    response_mode = sys.argv[4] if len(sys.argv) > 4 else "buffered"
    if suite not in ("TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"):
        raise ValueError(f"Unsupported TLS test suite: {suite}")
    if authentication not in ("rsa", "ecdsa"):
        raise ValueError(f"Unsupported TLS test authentication: {authentication}")
    if ca_authentication not in ("rsa", "ecdsa"):
        raise ValueError(f"Unsupported TLS test CA: {ca_authentication}")
    if response_mode not in ("buffered", "stream"):
        raise ValueError(f"Unsupported TLS response mode: {response_mode}")
    with tempfile.TemporaryDirectory(prefix="dark-https-aes256-") as temporary:
        directory = Path(temporary)
        ca_key = directory / "ca.key"
        ca_cert = directory / "ca.pem"
        leaf_key = directory / "leaf.key"
        leaf_csr = directory / "leaf.csr"
        leaf_cert = directory / "leaf.pem"
        extensions = directory / "leaf.ext"
        extensions.write_text("subjectAltName=DNS:localhost\nextendedKeyUsage=serverAuth\n")
        if ca_authentication == "ecdsa":
            run("openssl", "ecparam", "-name", "prime256v1", "-genkey", "-noout",
                "-out", str(ca_key))
            run("openssl", "req", "-x509", "-new", "-key", str(ca_key),
                "-out", str(ca_cert), "-days", "1", "-subj", "/CN=Dark test CA",
                "-addext", "basicConstraints=critical,CA:TRUE")
        else:
            run("openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
                "-keyout", str(ca_key), "-out", str(ca_cert), "-days", "1",
                "-subj", "/CN=Dark test CA", "-addext", "basicConstraints=critical,CA:TRUE")
        if authentication == "ecdsa":
            run("openssl", "ecparam", "-name", "prime256v1", "-genkey", "-noout",
                "-out", str(leaf_key))
            run("openssl", "req", "-new", "-key", str(leaf_key), "-out",
                str(leaf_csr), "-subj", "/CN=localhost")
        else:
            run("openssl", "req", "-newkey", "rsa:2048", "-nodes", "-keyout",
                str(leaf_key), "-out", str(leaf_csr), "-subj", "/CN=localhost")
        run("openssl", "x509", "-req", "-in", str(leaf_csr), "-CA", str(ca_cert),
            "-CAkey", str(ca_key), "-CAcreateserial", "-out", str(leaf_cert),
            "-days", "1", "-extfile", str(extensions))
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
        source = directory / "client.dark"
        if response_mode == "stream":
            operation = f'''    let authorities = if host == "badroot" then [] else roots in
    match Stdlib.HttpClient.__streamTrustedWithRoots authorities "GET" "https://localhost:{port}/" [] Stdlib.Blob.empty with
    | Error _ -> Stdlib.printLine "TLS failed: stream request"
    | Ok response ->
      let bytes = Stdlib.Stream.toBlob response.body in
      let _ = Stdlib.Stream.close response.body in
      if response.statusCode == 200 && Stdlib.Blob.length bytes > 0 then
        Stdlib.printLine "TLS OK"
      else Stdlib.printLine "Unexpected HTTP response"
'''
        else:
            operation = f'''    match Stdlib.__Network.connectTcp4 [127L, 0L, 0L, 1L] {port}L 2000L with
    | Error _ -> Stdlib.printLine "TCP connect failed"
    | Ok connection ->
      let request = Stdlib.String.toBlob "GET / HTTP/1.1\\r\\nHost: localhost\\r\\nConnection: close\\r\\n\\r\\n" in
      let response = Stdlib.__Tls13Client.exchange connection host "GET" roots request in
      let _ = Stdlib.__Network.closeTcp connection in
      match response with
      | Error message -> Stdlib.printLine ("TLS failed: " ++ message)
      | Ok bytes ->
        match Stdlib.Blob.toString bytes with
        | Error _ -> Stdlib.printLine "Response is not text"
        | Ok text ->
          if Stdlib.String.contains text "HTTP/1.0 200" ||
             Stdlib.String.contains text "HTTP/1.1 200" then
            Stdlib.printLine "TLS OK"
          else Stdlib.printLine "Unexpected HTTP response"
'''
        source.write_text(f'''// client.dark - Local TLS cipher interoperability probe.
let host = match Stdlib.Cli.__Args.get 0 with
           | Ok value -> value
           | Error _ -> "localhost" in
match Stdlib.Cli.FileSystem.readFile "{ca_cert}" with
| Error _ -> Stdlib.printLine "CA read failed"
| Ok pem ->
  match Stdlib.__Tls13Client.parsePemBundle pem with
  | Error message -> Stdlib.printLine message
  | Ok roots ->
{operation}
''')
        binary = directory / "client"
        subprocess.run([str(ROOT / "dark"), str(source), "-o", str(binary)],
                       check=True, cwd=ROOT, stdout=subprocess.DEVNULL)
        with (directory / "peer.log").open("wb") as log:
            peer = subprocess.Popen(["openssl", "s_server", "-accept", str(port),
                                     "-cert", str(leaf_cert), "-key", str(leaf_key),
                                     "-tls1_3", "-ciphersuites",
                                     suite, "-www"],
                                    stdout=log, stderr=subprocess.STDOUT)
            try:
                wait_for_port(port, peer)
                result = subprocess.run([str(binary)], cwd=ROOT, text=True,
                                        capture_output=True, timeout=30)
                if result.returncode != 0 or "TLS OK" not in result.stdout:
                    raise RuntimeError(f"Dark TLS probe failed: {result.stdout} {result.stderr}")
                rejection_case = "badroot" if response_mode == "stream" else "wrong.local"
                rejected = subprocess.run([str(binary), rejection_case], cwd=ROOT,
                                          text=True, capture_output=True, timeout=30)
                if rejected.returncode != 0 or "TLS failed:" not in rejected.stdout:
                    raise RuntimeError(f"Invalid trust or hostname was accepted: {rejected.stdout} {rejected.stderr}")
                rejection = "untrusted root" if response_mode == "stream" else "invalid hostname"
                print(f"{suite} with {authentication} leaf and {ca_authentication} CA, {response_mode} OK; {rejection} rejected")
            finally:
                peer.terminate()
                peer.wait(timeout=5)


if __name__ == "__main__":
    main()
