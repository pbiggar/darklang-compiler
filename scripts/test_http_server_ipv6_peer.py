#!/usr/bin/env python3
"""Verify native IPv6 HTTP/1.1, TLS HTTP/2 and QUIC HTTP/3 serving and cleanup."""

import argparse
import socket
import subprocess
import tempfile
from pathlib import Path

from cryptography.hazmat.primitives import serialization

from test_http3_server_peer import ROOT, SERVER, exchange, start, stop
from test_http2_server_tls_peer import connect, h2_exchange
from test_quic_tls_peer import certificates

LOOPBACK = "[0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L]"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiler", type=Path, default=ROOT / "dark")
    args = parser.parse_args()
    key, leaf, ca = certificates()
    pem = leaf.public_bytes(serialization.Encoding.PEM)
    trust = ca.public_bytes(serialization.Encoding.PEM)
    private = key.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption())
    with tempfile.TemporaryDirectory(prefix="dark-http-ipv6-") as temporary:
        directory = Path(temporary)
        for secure in (False, True):
            source, binary = directory / "server.dark", directory / "server"
            program = SERVER.replace("@CERT@", pem.hex()).replace("@KEY@", private.hex())
            if secure:
                program = program.replace("Stdlib.HttpServer.Quic.serve config identity", f"Stdlib.HttpServer.Secure.serveOn {LOOPBACK} config identity")
            else:
                program = program.replace("Stdlib.HttpServer.Quic.serve config identity", f"Stdlib.HttpServer.serveOn {LOOPBACK} config")
                program = program.replace('"https://localhost"', '"http://localhost"')
            source.write_text(program)
            result = subprocess.run([str(args.compiler), str(source), "--allow-internal", "--leak-check", "-o", str(binary)],
                                    cwd=ROOT, capture_output=True, text=True, timeout=180)
            assert result.returncode == 0, result.stdout + result.stderr
            process, port = start(binary, host="::1")
            try:
                if secure:
                    for protocols in (["http/1.1"], []):
                        with connect(port, trust, protocols, host="::1") as peer:
                            peer.sendall(b"GET /echo HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
                            response = bytearray()
                            while chunk := peer.recv(65536):
                                response.extend(chunk)
                            assert response.startswith(b"HTTP/1.1 200"), response
                    with connect(port, trust, ["h2"], host="::1") as peer:
                        h2_exchange(peer, port, "POST", 70000)
                    with socket.socket(socket.AF_INET6, socket.SOCK_DGRAM) as peer:
                        peer.bind(("::1", 0))
                        peer.settimeout(0.01)
                        body = bytes(range(251)) * 279
                        headers, response, _replay = exchange(peer, port, ca, body=body, host="::1")
                        assert response == body and headers.count((b"set-cookie", b"a=1")) == 1, headers
                else:
                    with socket.create_connection(("::1", port), timeout=5) as peer:
                        peer.sendall(b"POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 4\r\nConnection: close\r\n\r\necho")
                        response = bytearray()
                        while chunk := peer.recv(65536):
                            response.extend(chunk)
                        assert response.startswith(b"HTTP/1.1 200") and response.endswith(b"echo"), response
                stop(process)
                process, _ = start(binary, port, host="::1")
                stop(process)
            finally:
                if process.poll() is None:
                    process.kill()
                process.communicate()
    print("IPv6 HTTP/1.1, TLS/no-ALPN, HTTP/2 and HTTP/3 large bodies, rebind and compiled cleanup verified")


if __name__ == "__main__":
    main()
