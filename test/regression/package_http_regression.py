#!/usr/bin/env python3
"""Exercise OCaml HTTP/TLS against local servers with a generated test CA."""
import http.server
import os
from pathlib import Path
import ssl
import subprocess
import sys
import tempfile
import threading


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *_):
        pass

    def do_GET(self):
        if self.path == "/redirect":
            self.send_response(302)
            self.send_header("Location", "/text")
            self.end_headers()
        elif self.path == "/chunked":
            self.send_response(200)
            self.send_header("Transfer-Encoding", "chunked")
            self.end_headers()
            for _ in range(10):
                self.wfile.write(b"2710\r\n" + b"x" * 10000 + b"\r\n")
            self.wfile.write(b"0\r\n\r\n")
        else:
            status, body, charset = {
                "/text": (200, "A😀".encode("utf-16"), "utf-16le"),
                "/missing": (404, b"missing", "utf-8"),
                "/secure": (200, b"secure", "utf-8"),
            }[self.path]
            self.send_response(status)
            self.send_header("Content-Type", "text/plain; charset=" + charset)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)


def main():
    executable = str(Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="dark-package-tls-") as temporary:
        cert, key = Path(temporary) / "cert.pem", Path(temporary) / "key.pem"
        subprocess.run([executable, "--certificate", temporary], check=True, timeout=30)
        plain = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        https = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain(cert, key)
        https.socket = context.wrap_socket(https.socket, server_side=True)
        servers = [plain, https]
        for server in servers:
            threading.Thread(target=server.serve_forever, daemon=True).start()
        try:
            for mode in ["trusted", "untrusted"]:
                environment = os.environ.copy()
                environment.pop("SSL_CERT_FILE", None)
                environment.pop("NIX_SSL_CERT_FILE", None)
                if mode == "trusted":
                    environment["SSL_CERT_FILE"] = str(cert)
                subprocess.run([executable, f"http://127.0.0.1:{plain.server_port}",
                                f"https://localhost:{https.server_port}/secure", mode],
                               env=environment, check=True, timeout=30)
        finally:
            for server in servers:
                server.shutdown()
                server.server_close()


if __name__ == "__main__":
    main()
